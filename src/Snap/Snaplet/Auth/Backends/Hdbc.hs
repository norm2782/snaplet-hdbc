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
import            Data.Pool
import            Database.HDBC
import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Session
import            Web.ClientSession

initHdbcAuthManager
  :: IConnection conn
  => AuthSettings
  -> Lens b (Snaplet SessionManager)
  -> IO conn
  -> AuthTable
  -> Queries
  -> SnapletInit b (AuthManager b)
initHdbcAuthManager s l conn tbl qs = initHdbcAuthManager' s l pool tbl qs
  where pool = createPool conn disconnect 1 5 1

initHdbcAuthManager'
  :: IConnection conn
  => AuthSettings
  -> Lens b (Snaplet SessionManager)
  -> IO (Pool conn)
  -> AuthTable
  -> Queries
  -> SnapletInit b (AuthManager b)
initHdbcAuthManager' s l pool tbl qs =
  makeSnaplet  "HdbcAuthManager"
               "A snaplet providing user authentication using an HDBC backend"
               Nothing $ liftIO $ do
  key  <- getKey (asSiteKey s)
  pl   <- pool
  return AuthManager {
      backend = HdbcAuthManager pl tbl qs
    , session = l
    , activeUser = Nothing
    , minPasswdLen = asMinPasswdLen s
    , rememberCookieName = asRememberCookieName s
    , rememberPeriod = asRememberPeriod s
    , siteKey = key
    , lockout = asLockout s
  }

data HdbcAuthManager = forall conn. IConnection conn => HdbcAuthManager {
     dbpool  :: Pool conn
  ,  table   :: AuthTable
  ,  qries   :: Queries
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
  ,  colLockedOutUntil :: String
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
  ,  colLockedOutUntil = "locked_out_until"
  ,  colCurrentLoginAt = "current_login_at"
  ,  colLastLoginAt = "last_login_at"
  ,  colCurrentLoginIp = "current_login_ip"
  ,  colLastLoginIp = "last_login_ip"
  ,  colCreatedAt = "created_at"
  ,  colUpdatedAt = "updated_at"
  ,  colRoles = "roles"
  ,  colMeta = "meta" }

colLst :: [AuthTable -> String]
colLst =  [  colLogin
          ,  colPassword
          ,  colActivatedAt
          ,  colSuspendedAt
          ,  colRememberToken
          ,  colLoginCount
          ,  colFailedLoginCount
          ,  colLockedOutUntil
          ,  colCurrentLoginAt
          ,  colLastLoginAt
          ,  colCurrentLoginIp
          ,  colLastLoginIp
          ,  colCreatedAt
          ,  colUpdatedAt
          ,  colRoles
          ,  colMeta ]

data LookupQuery = ByUserId | ByLogin | ByRememberToken

type QueryAndVals = (String, [SqlValue])
type SelectQuery = AuthTable -> LookupQuery -> [SqlValue] -> QueryAndVals
type ModifyQuery = AuthTable -> AuthUser -> QueryAndVals

data Queries = Queries {
     selectQuery  :: SelectQuery
  ,  saveQuery    :: ModifyQuery
  ,  deleteQuery  :: ModifyQuery
}

defQueries :: Queries
defQueries = Queries {
     selectQuery  = defSelectQuery
  ,  saveQuery    = defSaveQuery
  ,  deleteQuery  = defDeleteQuery }

defSelectQuery :: SelectQuery
defSelectQuery tbl luq sqlVals = case luq of
            ByUserId         -> (mkSelect colId, sqlVals)
            ByLogin          -> (mkSelect colLogin, sqlVals)
            ByRememberToken  -> (mkSelect colRememberToken, sqlVals)
  where mkSelect whr  =  "SELECT * FROM " ++ tblName tbl ++ " WHERE " ++
                          whr tbl ++ " = ? "

defSaveQuery :: ModifyQuery
defSaveQuery tbl au = (mkQry uid, mkVals uid)
  where  uid             =  userId au
         mkQry Nothing   =  "INSERT INTO " ++ tblName tbl ++ " (" ++
                            intercalate "," (map (\f -> f tbl)  colLst)
                            ++ ") VALUES (" ++
                            intercalate "," (map (const "?") colLst)
                            ++ ")"
         mkQry (Just _)  =  "UPDATE " ++ tblName tbl ++ " SET " ++
                            intercalate "," (map (\f -> f tbl ++ " = ?")  colLst)
                            ++ " WHERE " ++ colId tbl ++ " = ?"
         mkVals Nothing   = mkVals'
         mkVals (Just i)  = mkVals' ++ [toSql i]
         mkVals' =  [  toSql $ userLogin au
                    ,  toSql $ userPassword au
                    ,  toSql $ userActivatedAt au
                    ,  toSql $ userSuspendedAt au
                    ,  toSql $ userRememberToken au
                    ,  toSql $ userLoginCount au
                    ,  toSql $ userFailedLoginCount au
                    ,  toSql $ userLockedOutUntil au
                    ,  toSql $ userCurrentLoginAt au
                    ,  toSql $ userLastLoginAt au
                    ,  toSql $ userCurrentLoginIp au
                    ,  toSql $ userLastLoginIp au
                    ,  toSql $ userCreatedAt au
                    ,  toSql $ userUpdatedAt au
                    ,  SqlNull -- userRoles au TODO: Implement when ACL system is live
                    ,  SqlNull -- userMeta au TODO: What should we store here?
                    ]

defDeleteQuery :: ModifyQuery
defDeleteQuery tbl ausr =
  case userId ausr of
    Nothing   ->  error "Cannot delete user without unique ID"
    Just uid  ->  (  "DELETE FROM " ++ tblName tbl ++ " WHERE " ++
                     colId tbl ++ " = ? "
                  ,  [toSql uid])

instance Convertible Password SqlValue where
  safeConvert (ClearText bs) = Right $ toSql bs
  safeConvert (Encrypted bs) = Right $ toSql bs

instance Convertible UserId SqlValue where
  safeConvert (UserId uid) = Right $ toSql uid

instance IAuthBackend HdbcAuthManager where
  destroy (HdbcAuthManager pool tbl qs) au = withResource pool $
    \conn -> withTransaction conn $ \conn' -> do
      let (qry, vals) = deleteQuery qs tbl au
      stmt  <- prepare conn' qry
      _     <- execute stmt vals
      return ()

  save (HdbcAuthManager pool tbl qs) au = withResource pool $
    \conn -> withTransaction conn $ \conn' -> do
      let (qry, vals) = saveQuery qs tbl au
      stmt  <- prepare conn' qry
      _     <- execute stmt vals
      -- TODO: Retrieve row to populate ID field after an INSERT... by username? By all fields
      return au

  lookupByUserId mgr@(HdbcAuthManager _ tbl qs) uid = authQuery mgr $
    selectQuery qs tbl ByUserId [toSql uid]
  lookupByLogin mgr@(HdbcAuthManager _ tbl qs) lgn = authQuery mgr $
    selectQuery qs tbl ByLogin [toSql lgn]
  lookupByRememberToken mgr@(HdbcAuthManager _ tbl qs) rmb = authQuery mgr $
    selectQuery qs tbl ByRememberToken [toSql rmb]

authQuery :: HdbcAuthManager -> QueryAndVals -> IO (Maybe AuthUser)
authQuery (HdbcAuthManager pool tbl _) (qry, vals) = withResource pool $ \conn -> withTransaction conn $
  \conn' -> do
    stmt  <- prepare conn' qry
    _     <- execute stmt vals
    res   <- fetchRowMap stmt
    case res of
      Nothing  ->  return Nothing
      Just mp  ->  return $ Just mkUser
                   where  colLU col' = mp DM.! col' tbl
                          rdSql con col' =  case colLU col' of
                                              SqlNull  -> Nothing
                                              x        -> Just . con $ fromSql x
                          rdInt col =  case colLU col of
                                         SqlNull  -> 0
                                         x        -> fromSql x
                          mkUser =  AuthUser {
                                       userId = rdSql UserId colId
                                    ,  userLogin = fromSql $ colLU colLogin
                                    ,  userPassword = rdSql Encrypted colPassword
                                    ,  userActivatedAt = rdSql id colActivatedAt
                                    ,  userSuspendedAt = rdSql id colSuspendedAt
                                    ,  userRememberToken = rdSql id colRememberToken
                                    ,  userLoginCount = rdInt colLoginCount
                                    ,  userFailedLoginCount = rdInt colFailedLoginCount
                                    ,  userLockedOutUntil = rdSql id colLockedOutUntil
                                    ,  userCurrentLoginAt = rdSql id colCurrentLoginAt
                                    ,  userLastLoginAt = rdSql id colLastLoginAt
                                    ,  userCurrentLoginIp = rdSql id colCurrentLoginIp
                                    ,  userLastLoginIp = rdSql id colLastLoginIp
                                    ,  userCreatedAt = rdSql id colCreatedAt
                                    ,  userUpdatedAt = rdSql id colUpdatedAt
                                    ,  userRoles = [] -- :: [Role] TODO
                                    ,  userMeta = HM.empty } -- :: HashMap Text Value TODO
