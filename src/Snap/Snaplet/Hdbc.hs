{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | This module provides a very thin wrapper around HDBC. It wraps some of the
-- HDBC functions in more convenient functions and re-exports the rest of the
-- HDBC functions.
module Snap.Snaplet.Hdbc (
  -- Snaplet functions
     HdbcSnaplet(..)
  ,  HasHdbc(..)
  ,  hdbcInit
  ,  query
  ,  query'

  -- Snapletified HDBC functions
  ,  clone
  ,  commit
  ,  dbServerVer
  ,  dbTransactionSupport
  ,  describeTable
  ,  disconnect
  ,  getTables
  ,  hdbcClientVer
  ,  hdbcDriverName
  ,  prepare
  ,  proxiedClientName
  ,  proxiedClientVer
  ,  quickQuery
  ,  quickQuery'
  ,  rollback
  ,  run
  ,  runRaw
  ,  sRun
  ,  withHdbc
  ,  withHdbc'
  ,  withTransaction
  ,  withTransaction'

  -- HDBC functions
  ,  SqlValue(..)
  ,  HDBC.toSql
  ,  HDBC.fromSql
  ,  HDBC.safeFromSql
  ,  HDBC.nToSql
  ,  HDBC.iToSql
  ,  HDBC.posixToSql
  ,  HDBC.withWConn
  ,  Statement(..)
  ,  HDBC.sExecute
  ,  HDBC.sExecuteMany
  ,  HDBC.fetchRowAL
  ,  HDBC.fetchRowMap
  ,  HDBC.sFetchRow
  ,  HDBC.fetchAllRows
  ,  HDBC.fetchAllRows'
  ,  HDBC.fetchAllRowsAL
  ,  HDBC.fetchAllRowsAL'
  ,  HDBC.fetchAllRowsMap
  ,  HDBC.fetchAllRowsMap'
  ,  HDBC.sFetchAllRows
  ,  HDBC.sFetchAllRows'
  ,  SqlError(..)
  ,  HDBC.throwSqlError
  ,  HDBC.catchSql
  ,  HDBC.handleSql
  ,  HDBC.sqlExceptions
  ,  HDBC.handleSqlError
  ,  module Database.HDBC.ColTypes
  ) where

import            Prelude hiding (catch)

import            Control.Exception.Control hiding (Handler)
import            Control.Monad.IO.Control
import            Control.Monad.State
import            Data.Map (Map)
import            Data.Pool
import qualified  Database.HDBC as HDBC
import            Database.HDBC (IConnection(), SqlValue, SqlError, Statement)
import            Database.HDBC.ColTypes
import            Snap.Snaplet

-- | A map with the column name as key and the value from the database as value
type Row = Map String SqlValue

-- | Instantiate this typeclass on 'Handler b YourSnapletState' so this snaplet
-- can find the resource pool. Typically you would instantiate it for Snap's
-- Handler type and use your snaplet's lens to this snaplet to access this
-- snaplet's state, which contains the pool. Suppose your snaplet state type is
-- defined as follows, where 'Connection' is the connection type from the HDBC
-- database adapter of your choosing:
--
-- > data App = App
-- >  { _dbLens :: Snaplet (HdbcSnaplet Connection) }
--
-- Then a typical instance you will want to define in your own snaplet is the
-- following:
--
-- > instance HasHdbc (Handler b App) Connection where
-- >   getPool = with dbLens $ gets hdbcPool
--
class (IConnection c, MonadControlIO m) => HasHdbc m c | m -> c where
  getPool :: m (Pool c)

-- | This is (hopefully) a temporary instance, which will disppear once the
-- entire snap framework is switched to 'MonadControlIO'.
instance MonadControlIO (Handler b v) where
  liftControlIO f = liftIO (f return)

-- | The snaplet state type containing a resource pool, parameterised by a raw
-- HDBC connection.
data HdbcSnaplet c
  =   IConnection c
  =>  HdbcSnaplet
  {   hdbcPool :: Pool c }

-- | Initialise the snaplet by providing it with a raw HDBC connection. A
-- resource pool is created with some default parameters that should be fine
-- for most common usecases. If a custom resource pool configuration is
-- desired, use the `hdbcInit'` initialiser instead. When the snaplet is
-- unloaded, the 'disconnect' function is called to close any remaining
-- connections.
hdbcInit :: IConnection c => IO c -> SnapletInit b (HdbcSnaplet c)
hdbcInit conn = hdbcInit' $ createPool conn HDBC.disconnect 1 300 1

-- | Instead of a raw HDBC connection, this initialiser expects a
-- pre-configured resource pool that dispenses HDBC connections. When the
-- snaplet is unloaded, the 'disconnect' function is called to close any
-- remaining connections.
hdbcInit' :: IConnection c => IO (Pool c) -> SnapletInit b (HdbcSnaplet c)
hdbcInit' pl = makeSnaplet "hdbc" "HDBC abstraction" Nothing $ do
  pl' <- liftIO pl
  onUnload $ withResource pl' HDBC.disconnect
  return $ HdbcSnaplet pl'

-- | Get a new connection from the resource pool, apply the provided function
-- to it and return the result in of the 'IO' compution in monad @m@.
withHdbc :: HasHdbc m c => (c -> IO a) -> m a
withHdbc f = do
  pl <- getPool
  withResource pl (\conn -> liftIO $ f conn)

-- | Get a new connection from the resource pool, apply the provided function
-- to it and return the result in of the compution in monad 'm'.
withHdbc' :: HasHdbc m c => (c -> a) -> m a
withHdbc' f = do
  pl <- getPool
  withResource pl (\conn -> return $ f conn)

-- | Execute a @SELECT@ query on the database by passing the query as 'String',
-- together with a list of values to bind to it. A list of 'Row's is returned.
query
  :: HasHdbc m c
  => String      -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
  -> [SqlValue]  -- ^ Values for each placeholder according to its position in
                 --   the SQL statement.
  -> m [Row]     -- ^ A 'Map' of attribute name to attribute value for each
                 --   row. Can be the empty list.
query sql bind = do
  stmt <- prepare sql
  liftIO $ HDBC.execute stmt bind
  liftIO $ HDBC.fetchAllRowsMap stmt

-- | Similar to 'query', but instead of returning a list of 'Row's, it returns
-- an 'Integer' indicating the numbers of affected rows. This is typically used
-- for @INSERT@, @UPDATE@ and @DELETE@ queries.
query' :: HasHdbc m conn => String -> [SqlValue] -> m Integer
query' sql bind = withTransaction' $ do
  stmt <- prepare sql
  liftIO $ HDBC.execute stmt bind

-- | Run an action inside a transaction. If the action throws an exception, the
-- transaction will be rolled back, and the exception rethrown.
--
-- > withTransaction' $ \conn -> do ...
--
withTransaction :: HasHdbc m c => (c -> IO a) -> m a
withTransaction f = withHdbc (`HDBC.withTransaction` f)

-- | Run an action inside a transaction. If the action throws an exception, the
-- transaction will be rolled back, and the exception rethrown.
--
-- > withTransaction' $ do
-- >   query "INSERT INTO ..." []
-- >   query "DELETE FROM ..." []
--
withTransaction' :: HasHdbc m c => m a -> m a
withTransaction' action = do
  r <- onException action doRollback
  commit
  return r
  where doRollback = catch rollback doRollbackHandler
        doRollbackHandler :: MonadControlIO m => SomeException -> m ()
        doRollbackHandler _ = return ()

-- | The functions provided below are wrappers around the original HDBC
-- functions. Please refer to the HDBC documentation to see what they do and
-- how they work.

disconnect :: HasHdbc m c => m ()
disconnect = withHdbc HDBC.disconnect

commit :: HasHdbc m c => m ()
commit = withHdbc HDBC.commit

rollback :: HasHdbc m c => m ()
rollback = withHdbc HDBC.rollback

runRaw :: HasHdbc m c => String -> m ()
runRaw str = withHdbc (`HDBC.runRaw` str)

run :: HasHdbc m c => String -> [SqlValue] -> m Integer
run str vs = withHdbc (\conn -> HDBC.run conn str vs)

prepare :: HasHdbc m c => String -> m Statement
prepare str = withHdbc (`HDBC.prepare` str)

clone :: HasHdbc m c => m c
clone = withHdbc HDBC.clone

hdbcDriverName :: HasHdbc m c => m String
hdbcDriverName = withHdbc' HDBC.hdbcDriverName

hdbcClientVer :: HasHdbc m c => m String
hdbcClientVer = withHdbc' HDBC.hdbcClientVer

proxiedClientName :: HasHdbc m c => m String
proxiedClientName = withHdbc' HDBC.proxiedClientName

proxiedClientVer :: HasHdbc m c => m String
proxiedClientVer = withHdbc' HDBC.proxiedClientVer

dbServerVer :: HasHdbc m c => m String
dbServerVer = withHdbc' HDBC.dbServerVer

dbTransactionSupport :: HasHdbc m c => m Bool
dbTransactionSupport = withHdbc' HDBC.dbTransactionSupport

getTables :: HasHdbc m c => m [String]
getTables = withHdbc HDBC.getTables

describeTable :: HasHdbc m c => String -> m [(String, SqlColDesc)]
describeTable str = withHdbc (`HDBC.describeTable` str)

quickQuery' :: HasHdbc m c => String -> [SqlValue] -> m [[SqlValue]]
quickQuery' str vs = withHdbc (\conn -> HDBC.quickQuery' conn str vs)

quickQuery :: HasHdbc m c => String -> [SqlValue] -> m [[SqlValue]]
quickQuery str vs = withHdbc (\conn -> HDBC.quickQuery conn str vs)

sRun :: HasHdbc m c => String -> [Maybe String] -> m Integer
sRun str mstrs = withHdbc (\conn -> HDBC.sRun conn str mstrs)
