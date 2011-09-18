{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}


module Snap.Snaplet.Auth.Backends.Hdbc where

import            Control.Monad.State
import            Data.Convertible.Base
import qualified  Data.HashMap.Strict as HM
import            Data.Lens.Lazy
import            Data.List
import qualified  Data.Map as DM
import            Database.HDBC
import            Web.ClientSession

import            Snap.Snaplet
import            Snap.Snaplet.Auth.Types
import            Snap.Snaplet.Session




initHdbcAuthManager
  :: IConnection conn
  => AuthSettings
  -> Lens b (Snaplet SessionManager)
  -> conn
  -> AuthTable
  -> SnapletInit b (AuthManager b)
initHdbcAuthManager s l conn tbl =
  makeSnaplet  "HdbcAuthManager"
               "A snaplet providing user authentication using an HDBC backend"
               Nothing $ liftIO $ do
  key <- getKey (asSiteKey s)
  return AuthManager {
      backend = HdbcAuthManager conn tbl
    , session = l
    , activeUser = Nothing
    , minPasswdLen = asMinPasswdLen s
    , rememberCookieName = asRememberCookieName s
    , rememberPeriod = asRememberPeriod s
    , siteKey = key
    , lockout = asLockout s
  }


data HdbcAuthManager = forall conn. IConnection conn => HdbcAuthManager {
    dbconn :: conn
  , table  :: AuthTable
}

data AuthTable = AuthTable {
     tblName :: String
  ,  colId :: String
  ,  colLogin :: String
  ,  colPassword :: String
  ,  colActivatedAt :: String
  ,  colSuspendedAt :: String
  ,  colRememberToken :: String
  ,  colLoginCount :: String
  ,  colFailedLoginCount :: String
  ,  colLockedOutAt :: String
  ,  colCurrentLoginAt :: String
  ,  colLastLoginAt :: String
  ,  colCurrentLoginIp :: String
  ,  colLastLoginIp :: String
  ,  colCreatedAt :: String
  ,  colUpdatedAt :: String
  ,  colRoles :: String
  ,  colMeta :: String }

defAuthTable :: AuthTable
defAuthTable = AuthTable  {
     tblName = "users"
  ,  colId = "uid"
  ,  colLogin = "email"
  ,  colPassword = "password"
  ,  colActivatedAt = "activated_at"
  ,  colSuspendedAt = "suspended_at"
  ,  colRememberToken = "remember_token"
  ,  colLoginCount = "login_count"
  ,  colFailedLoginCount = "failed_login_count"
  ,  colLockedOutAt = "locked_out_at"
  ,  colCurrentLoginAt = "current_login_at"
  ,  colLastLoginAt = "last_login_at"
  ,  colCurrentLoginIp = "current_login_ip"
  ,  colLastLoginIp = "last_login_ip"
  ,  colCreatedAt = "created_at"
  ,  colUpdatedAt = "updated_at"
  ,  colRoles = "roles"
  ,  colMeta = "meta" }


instance Convertible Password SqlValue where
  safeConvert (ClearText bs) = Right $ toSql bs
  safeConvert (Encrypted bs) = Right $ toSql bs

instance Convertible UserId SqlValue where
  safeConvert (UserId uid) = Right $ toSql uid

instance IAuthBackend HdbcAuthManager where
  destroy (HdbcAuthManager conn tbl) au = destroy' (userId au)
    where  destroy' Nothing     = error "Need an auth user"
           destroy' (Just uid)  = withTransaction conn $ \conn' -> do
             stmt  <- prepare conn' delQuery
             _     <- execute stmt [toSql uid]
             return ()
           delQuery =  "DELETE FROM " ++ tblName tbl ++ " WHERE " ++
                       colId tbl ++ " = ? "

  save (HdbcAuthManager conn tbl) au = withTransaction conn $ \conn' -> do
    stmt  <- mkStmt (userId au) conn'
    _     <- execute stmt $ mkVals (userId au)
    return au
    where  mkStmt Nothing conn'' = prepare conn'' $
             "INSERT INTO " ++ tblName tbl ++ " (" ++
             intercalate "," (map (\f -> f tbl)  colLst)
             ++ ") VALUES (" ++ intercalate "," (map (const "?") colLst) ++ ")"
           mkStmt (Just _) conn'' = prepare conn'' $
             "UPDATE " ++ tblName tbl ++ " SET (" ++
             intercalate "," (map (\f -> f tbl ++ " = ?")  colLst)
             ++ ") WHERE " ++ colId tbl ++ " = ?"
           mkVals Nothing     = mkVals'
           mkVals (Just uid)  = mkVals' ++ [toSql uid]
           mkVals' =  [  toSql $ userLogin au
                      ,  toSql $ userPassword au
                      ,  toSql $ userActivatedAt au
                      ,  toSql $ userSuspendedAt au
                      ,  toSql $ userRememberToken au
                      ,  toSql $ userLoginCount au
                      ,  toSql $ userFailedLoginCount au
                      ,  toSql $ userLockedOutAt au
                      ,  toSql $ userCurrentLoginAt au
                      ,  toSql $ userLastLoginAt au
                      ,  toSql $ userCurrentLoginIp au
                      ,  toSql $ userLastLoginIp au
                      ,  toSql $ userCreatedAt au
                      ,  toSql $ userUpdatedAt au
                      ,  SqlNull -- userRoles au TODO: Implement when ACL system is live
                      ,  SqlNull -- userMeta au TODO: What should we store here?
                      ]
           colLst  =  [  colLogin
                      ,  colPassword
                      ,  colActivatedAt
                      ,  colSuspendedAt
                      ,  colRememberToken
                      ,  colLoginCount
                      ,  colFailedLoginCount
                      ,  colLockedOutAt
                      ,  colCurrentLoginAt
                      ,  colLastLoginAt
                      ,  colCurrentLoginIp
                      ,  colLastLoginIp
                      ,  colCreatedAt
                      ,  colUpdatedAt
                      ,  colRoles
                      ,  colMeta ]

  lookupByUserId mgr uid = query mgr colId [toSql uid]
  lookupByLogin mgr lgn = query mgr colLogin [toSql lgn]
  lookupByRememberToken mgr rmb = query mgr tblName [toSql rmb]

mkSelect :: AuthTable -> String -> String
mkSelect at whr = "SELECT * FROM " ++ tblName at ++ " WHERE " ++ whr ++ " = ? "

query  ::  HdbcAuthManager -> (AuthTable -> String) -> [SqlValue]
       ->  IO (Maybe AuthUser)
query (HdbcAuthManager conn tbl) col vals = withTransaction conn $ \conn' -> do
  stmt  <- prepare conn' (mkSelect tbl $ col tbl)
  _     <- execute stmt vals
  res   <- fetchRowMap stmt
  case res of
    Nothing  ->  return Nothing
    Just mp  ->  return $ Just mkUser
                 where  colLU col' = mp DM.! col' tbl
                        rdSql' = rdSql'
                        rdSql con col' =
                          case colLU col' of
                            SqlNull  -> Nothing
                            x        -> Just . con $ fromSql x
                        mkUser = AuthUser {
                                      userId = rdSql UserId colId
                                   ,  userLogin = fromSql $ colLU colLogin
                                   ,  userPassword = rdSql Encrypted colPassword
                                   ,  userActivatedAt = rdSql' colActivatedAt
                                   ,  userSuspendedAt = rdSql' colSuspendedAt
                                   ,  userRememberToken = rdSql' colRememberToken
                                   ,  userLoginCount = fromSql $ colLU colLoginCount
                                   ,  userFailedLoginCount = fromSql $ colLU colFailedLoginCount
                                   ,  userLockedOutAt = rdSql' colLockedOutAt
                                   ,  userCurrentLoginAt = rdSql' colCurrentLoginAt
                                   ,  userLastLoginAt = rdSql' colLastLoginAt
                                   ,  userCurrentLoginIp = rdSql' colCurrentLoginIp
                                   ,  userLastLoginIp = rdSql' colLastLoginIp
                                   ,  userCreatedAt = rdSql' colCreatedAt
                                   ,  userUpdatedAt = rdSql' colUpdatedAt
                                   ,  userRoles = [] -- :: [Role] TODO
                                   ,  userMeta = HM.empty } -- :: HashMap Text Value TODO

{- class IAuthBackend r where-}
{-   save :: r -> AuthUser -> IO AuthUser-}
{-   lookupByUserId :: r -> UserId -> IO (Maybe AuthUser)-}
{-   lookupByLogin :: r -> Text -> IO (Maybe AuthUser)-}
{-   lookupByRememberToken :: r -> Text -> IO (Maybe AuthUser)-}
{-   destroy :: r -> AuthUser -> IO ()-}
