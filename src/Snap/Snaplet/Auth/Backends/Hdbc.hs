{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Authentication backend using HDBC
module Snap.Snaplet.Auth.Backends.Hdbc where

import            Control.Monad.State
import            Data.Convertible.Base
import qualified  Data.HashMap.Strict as HM
import            Data.Lens.Lazy
import            Data.List
import            Data.Map (Map)
import qualified  Data.Map as DM
import            Data.Maybe
import            Data.Pool
import            Data.Text (Text)
import            Database.HDBC
import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Session
import            Web.ClientSession

-- | Initialises this HDBC snaplet. It automatically configures a resource
-- pool with commonly acceptable default settings. Use `initHdbcAuthManager'`
-- to initialise with a custom resource pool.
initHdbcAuthManager
  :: IConnection conn
  => AuthSettings  -- ^ Auth settings
  -> Lens b (Snaplet SessionManager)  -- ^ Lens to the session manager
  -> IO conn       -- ^ Raw HDBC connection
  -> AuthTable     -- ^ Authentication table configuration
  -> Queries       -- ^ Queries to be used for authentication
  -> SnapletInit b (AuthManager b)
initHdbcAuthManager s l conn tbl qs = initHdbcAuthManager' s l pool tbl qs
  where pool = createPool conn disconnect 1 300 1

-- | Initialises this HDBC snaplet with a custom resource pool.
initHdbcAuthManager'
  :: IConnection conn
  => AuthSettings    -- ^ Auth settings
  -> Lens b (Snaplet SessionManager)  -- ^ Lens to the session manager
  -> IO (Pool conn)  -- ^ A pre-configured resource pool which dispenses
                     --   HDBC connections
  -> AuthTable       -- ^ Authentication table configuration
  -> Queries         -- ^ Queries to be used for authentication
  -> SnapletInit b (AuthManager b)
initHdbcAuthManager' s l pool tbl qs =
  makeSnaplet  "HdbcAuthManager"
               "A snaplet providing user authentication using an HDBC backend"
               Nothing $ liftIO $ do
  key  <- getKey (asSiteKey s)
  pl   <- pool
  return AuthManager
    {  backend = HdbcAuthManager pl tbl qs
    ,  session = l
    ,  activeUser = Nothing
    ,  minPasswdLen = asMinPasswdLen s
    ,  rememberCookieName = asRememberCookieName s
    ,  rememberPeriod = asRememberPeriod s
    ,  siteKey = key
    ,  lockout = asLockout s }

-- | Authmanager state containing the resource pool and the table/query
-- configuration.
data HdbcAuthManager
  =   forall conn. IConnection conn
  =>  HdbcAuthManager
  {   authDBPool :: Pool conn
  ,   table  :: AuthTable
  ,   qries  :: Queries }

-- | Datatype containing the names of the columns for the authentication table.
data AuthTable
  =  AuthTable
  {  tblName :: String
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

-- | Default authentication table layout
defAuthTable :: AuthTable
defAuthTable
  =  AuthTable
  {  tblName = "users"
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

-- | List of deconstructors so it's easier to extract column names form an
-- 'AuthTable'.
colLst :: [AuthTable -> String]
colLst =
  [  colLogin
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

data Queries
  =  Queries
  {  selectQuery  :: AuthTable -> LookupQuery -> [SqlValue] -> (String, [SqlValue])
  ,  saveQuery    :: AuthTable -> AuthUser -> (String, String, [SqlValue])
  ,  deleteQuery  :: AuthTable -> AuthUser -> (String, [SqlValue]) }

defQueries :: Queries
defQueries = Queries {
     selectQuery  = defSelectQuery
  ,  saveQuery    = defSaveQuery
  ,  deleteQuery  = defDeleteQuery }

defSelectQuery :: AuthTable -> LookupQuery -> [SqlValue] -> (String, [SqlValue])
defSelectQuery tbl luq sqlVals = case luq of
            ByUserId         -> (mkSelect colId, sqlVals)
            ByLogin          -> (mkSelect colLogin, sqlVals)
            ByRememberToken  -> (mkSelect colRememberToken, sqlVals)
  where mkSelect whr  =  "SELECT * FROM " ++ tblName tbl ++ " WHERE " ++
                          whr tbl ++ " = ? "

defSaveQuery :: AuthTable -> AuthUser -> (String, String, [SqlValue])
defSaveQuery tbl au = (mkQry uid, mkIdQry, mkVals uid)
  where  uid     = userId au
         qval f  = f tbl ++ " = ?"
         mkQry Nothing   =  "INSERT INTO " ++ tblName tbl ++ " (" ++
                            intercalate "," (map (\f -> f tbl)  colLst)
                            ++ ") VALUES (" ++
                            intercalate "," (map (const "?") colLst)
                            ++ ")"
         mkQry (Just _)  =  "UPDATE " ++ tblName tbl ++ " SET " ++
                            intercalate "," (map qval colLst)
                            ++ " WHERE " ++ colId tbl ++ " = ?"
         mkIdQry =  "SELECT " ++ colId tbl ++ " FROM " ++ tblName tbl ++
                    " WHERE " ++ intercalate " AND " (map qval
                    [colLogin, colPassword])
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

defDeleteQuery :: AuthTable -> AuthUser -> (String, [SqlValue])
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
    \conn -> do
      let (qry, vals) = deleteQuery qs tbl au
      _ <- prepExec conn qry vals
      return ()

  save (HdbcAuthManager pool tbl qs) au = withResource pool $
    \conn -> do
      let (qry, idQry, vals) = saveQuery qs tbl au
      _ <- prepExec conn qry vals
      if isJust $ userId au
        then  return au
        else  do
          rw   <- withTransaction conn $ \conn' -> do
            stmt'  <- prepare conn' idQry
            _      <- execute stmt'  [  toSql $ userLogin au
                                     ,  toSql $ userPassword au]
            fetchRow stmt'
          nid  <- case rw of
                  Nothing     -> fail $  "Failed to fetch the newly inserted row. " ++
                                         "It might not have been inserted at all."
                  Just []     -> fail "Something went wrong"
                  Just (x:_)  -> return (fromSql x :: Text)
          return $ au { userId = Just (UserId nid) }

  lookupByUserId mgr@(HdbcAuthManager _ tbl qs) uid = authQuery mgr $
    selectQuery qs tbl ByUserId [toSql uid]
  lookupByLogin mgr@(HdbcAuthManager _ tbl qs) lgn = authQuery mgr $
    selectQuery qs tbl ByLogin [toSql lgn]
  lookupByRememberToken mgr@(HdbcAuthManager _ tbl qs) rmb = authQuery mgr $
    selectQuery qs tbl ByRememberToken [toSql rmb]

prepExec :: IConnection conn => conn -> String -> [SqlValue] -> IO ()
prepExec conn qry vals = withTransaction conn $ \conn' -> do
  stmt  <- prepare conn' qry
  _     <- execute stmt vals
  return ()

authQuery :: HdbcAuthManager -> (String, [SqlValue]) -> IO (Maybe AuthUser)
authQuery (HdbcAuthManager pool tbl _) (qry, vals) = withResource pool $
  \conn -> do
    res <- withTransaction conn $ \conn' -> do
      stmt  <- prepare conn' qry
      _     <- execute stmt vals
      fetchRowMap stmt
    case res of
      Nothing  -> return Nothing
      Just mp  -> return $ Just $ mkUser tbl mp

mkUser :: AuthTable -> Map String SqlValue -> AuthUser
mkUser tbl mp =
  let  colLU col' = mp DM.! col' tbl
       rdSql con col' =  case colLU col' of
                           SqlNull  -> Nothing
                           x        -> Just . con $ fromSql x
       rdInt col =  case colLU col of
                      SqlNull  -> 0
                      x        -> fromSql x
  in   AuthUser
       {  userId = rdSql UserId colId
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
