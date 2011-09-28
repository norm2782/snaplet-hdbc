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

  -- Snapletified HDBC functions
  ,  disconnect
  ,  commit
  ,  rollback
  ,  runRaw
  ,  run
  ,  prepare
  ,  clone
  ,  hdbcDriverName
  ,  hdbcClientVer
  ,  proxiedClientVer
  ,  proxiedClientName
  ,  dbServerVer
  ,  dbTransactionSupport
  ,  getTables
  ,  describeTable
  ,  quickQuery'
  ,  quickQuery
  ,  sRun
  ,  withHdbc
  ,  withHdbc'
  ,  withTransaction

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

import            Control.Monad.State
import            Data.Map (Map)
import qualified  Database.HDBC as HDBC
import            Database.HDBC (IConnection(), SqlValue, SqlError, Statement)
import            Database.HDBC.ColTypes
import            Snap.Snaplet

type Row = Map String SqlValue

class (IConnection c, MonadIO m) => HasHdbc m c | m -> c where
  getHdbc :: m c

data HdbcSnaplet c = IConnection c => HdbcSnaplet {
  hdbcConn :: c }


hdbcInit :: IConnection a => a -> SnapletInit b (HdbcSnaplet a)
hdbcInit conn = makeSnaplet "hdbc" "HDBC abstraction" Nothing $ do
  onUnload $ HDBC.disconnect conn
  return $ HdbcSnaplet conn

withHdbc :: HasHdbc m c => (c -> IO a) -> m a
withHdbc f = do
  conn <- getHdbc
  liftIO $ f conn

withHdbc' :: HasHdbc m c => (c -> a) -> m a
withHdbc' f = do
  conn <- getHdbc
  return $ f conn

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

withTransaction :: HasHdbc m c => (c -> IO a) -> m a
withTransaction f = withHdbc (`HDBC.withTransaction` f)

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
