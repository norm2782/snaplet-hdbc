{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}


module Snap.Snaplet.Auth.Backends.Hdbc where

import           Control.Monad.State
import           Database.HDBC
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as DM
import           Data.Lens.Lazy
import           Web.ClientSession

import           Snap.Snaplet.Auth.Types
import           Snap.Snaplet
import           Snap.Snaplet.Session




initHdbcAuthManager
  :: IConnection conn
  => AuthSettings
  -> Lens b (Snaplet SessionManager)
  -> conn
  -> AuthTable
  -> SnapletInit b (AuthManager b)
initHdbcAuthManager s l conn tbl =
  makeSnaplet "HdbcAuthManager" "A snaplet providing user authentication using an HDBC backend"
    Nothing $ liftIO $ do
  key <- getKey (asSiteKey s)
  return $ AuthManager {
      backend = HdbcAuthManager conn tbl
    , session = l
    , activeUser = Nothing
    , minPasswdLen = asMinPasswdLen s
    , rememberCookieName = asRememberCookieName s
    , rememberPeriod = asRememberPeriod s
    , siteKey = key
    , lockout = asLockout s
  }

data AuthTable = AuthTable {
    tblName     :: String
  , idCol       :: String
  , loginCol    :: String
  , passCol     :: String
  , rememberCol :: String
  }

data HdbcAuthManager = forall conn. IConnection conn => HdbcAuthManager {
    dbconn :: conn
  , table  :: AuthTable
}

defAuthTable :: AuthTable
defAuthTable = AuthTable "users" "uid" "email" "password" "remember_token"

instance IAuthBackend HdbcAuthManager where
  destroy = undefined
  save mgr usr = return usr -- TODO: Implement
  lookupByUserId mgr uid = query mgr idCol [toSql $ unUid uid]
  lookupByLogin mgr lgn = query mgr loginCol [toSql lgn]
  lookupByRememberToken mgr rmb = query mgr tblName [toSql rmb]

mkSelect :: AuthTable -> String -> String
mkSelect at whr = "SELECT * FROM " ++ tblName at ++ " WHERE " ++ whr ++ " = ? "

query :: HdbcAuthManager -> (AuthTable -> String) -> [SqlValue] -> IO (Maybe AuthUser)
query (HdbcAuthManager conn tbl) col vals = withTransaction conn $ \conn' -> do
  stmt  <- prepare conn (mkSelect tbl $ col tbl)
  _     <- execute stmt vals
  res   <- fetchRowMap stmt
  case res of
    Nothing -> return Nothing
    Just mp -> return $ Just mkUser
                 where rdSql mp k = fromSql $ mp DM.! k
                 -- TODO: This is all very incomplete. We need to persist more and update various counters
                       mkUser = AuthUser {
                                  userId = Just . UserId $ rdSql mp $ idCol tbl
                                , userLogin = rdSql mp $ loginCol tbl
                                , userPassword = Just . Encrypted $ rdSql mp $ passCol tbl
                                , userActivatedAt = Nothing -- :: Maybe UTCTime
                                , userSuspendedAt = Nothing -- :: Maybe UTCTime
                                , userRememberToken = Nothing -- :: Maybe Text
                                , userLoginCount = 0 -- :: Int
                                , userFailedLoginCount = 0 -- :: Int
                                , userLockedOutAt = Nothing -- :: Maybe UTCTime
                                , userCurrentLoginAt = Nothing -- :: Maybe UTCTime
                                , userLastLoginAt = Nothing -- :: Maybe UTCTime
                                , userCurrentLoginIp = Nothing -- :: Maybe ByteString
                                , userLastLoginIp = Nothing -- :: Maybe ByteString
                                , userCreatedAt = Nothing -- :: Maybe UTCTime
                                , userUpdatedAt = Nothing -- :: Maybe UTCTime
                                , userRoles = [] -- :: [Role]
                                , userMeta = HM.empty -- :: HashMap Text Value
                                }

{- class IAuthBackend r where-}
{-   save :: r -> AuthUser -> IO AuthUser-}
{-   lookupByUserId :: r -> UserId -> IO (Maybe AuthUser)-}
{-   lookupByLogin :: r -> Text -> IO (Maybe AuthUser)-}
{-   lookupByRememberToken :: r -> Text -> IO (Maybe AuthUser)-}
{-   destroy :: r -> AuthUser -> IO ()-}
