{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|

  This module provides a very thin wrapper around HDBC
-}
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

type Row = Map String SqlValue

class  (IConnection c, MonadControlIO m) => HasHdbc m c | m -> c where
  getPool :: m (Pool c)

data HdbcSnaplet c = IConnection c => HdbcSnaplet {
  hdbcPool :: Pool c }

hdbcInit :: IConnection c => IO c -> SnapletInit b (HdbcSnaplet c)
hdbcInit conn = hdbcInit' $ createPool conn HDBC.disconnect 1 300 1

hdbcInit' :: IConnection c => IO (Pool c) -> SnapletInit b (HdbcSnaplet c)
hdbcInit' pl = makeSnaplet "hdbc" "HDBC abstraction" Nothing $ do
  pl' <- liftIO pl
  onUnload $ withResource pl' HDBC.disconnect
  return $ HdbcSnaplet pl'

withHdbc :: HasHdbc m c => (c -> IO a) -> m a
withHdbc f = do
  pl <- getPool
  withResource pl (\conn -> liftIO $ f conn)

withHdbc' :: HasHdbc m c => (c -> a) -> m a
withHdbc' f = do
  pl <- getPool
  withResource pl (\conn -> return $ f conn)

query :: HasHdbc m c
      => String      {-^ The raw SQL to execute. Use @?@ to indicate
                         placeholders. -}
      -> [SqlValue]  {-^ Values for each placeholder according to its position
                         in the SQL statement. -}
      -> m [Row]     {-^ A 'Map' of attribute name to attribute value for each
                         row. Can be the empty list. -}
query sql bind = do
  stmt <- prepare sql
  liftIO $ HDBC.execute stmt bind
  liftIO $ HDBC.fetchAllRowsMap stmt

query' :: HasHdbc m conn => String -> [SqlValue] -> m Integer
query' sql bind = withTransaction' $ do
  stmt <- prepare sql
  liftIO $ HDBC.execute stmt bind

withTransaction :: HasHdbc m c => (c -> IO a) -> m a
withTransaction f = withHdbc (`HDBC.withTransaction` f)

{-| Run an action inside a transaction. If the action throws an exception, the
transaction will be rolled back, and the exception rethrown.

> withTransaction' $ do
>   query "INSERT INTO ..." []
>   query "DELETE FROM ..." []

-}
withTransaction' :: HasHdbc m c => m a -> m a
withTransaction' action = do
  r <- onException action doRollback
  commit
  return r
  where doRollback = catch rollback doRollbackHandler
        doRollbackHandler :: MonadControlIO m => SomeException -> m ()
        doRollbackHandler _ = return ()

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
