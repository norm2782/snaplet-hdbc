{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|

  This module provides a very thin wrapper around HDBC
-}
module Snap.Snaplet.Hdbc (
  -- Snaplet functions
     HdbcSnaplet(..)
  ,  hdbcInit
  ,  disconnect
  ,  commit
  ,  getConn
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
  ,  withTransaction
  ,  withConn
  ,  withConn'

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
import qualified  Database.HDBC as HDBC
import            Database.HDBC (IConnection(), SqlValue, SqlError, Statement)
import            Database.HDBC.ColTypes
import            Snap.Snaplet

data HdbcSnaplet conn = IConnection conn => HdbcSnaplet {
  hdbcConn :: conn }

hdbcInit :: IConnection conn => conn -> SnapletInit b (HdbcSnaplet conn)
hdbcInit conn = makeSnaplet "hdbc" "HDBC abstraction" Nothing $ do
  onUnload $ HDBC.disconnect conn
  return $ HdbcSnaplet conn

getConn :: (MonadIO m, MonadState (HdbcSnaplet conn) m, IConnection conn) => m conn
getConn = gets hdbcConn

withConn :: (MonadIO m, MonadState (HdbcSnaplet t) m) => (t -> IO b) -> m b
withConn f = do
  conn <- gets hdbcConn
  liftIO $ f conn

withConn' :: MonadState (HdbcSnaplet t) m => (t -> b) -> m b
withConn' f = do
  conn <- gets hdbcConn
  return $ f conn

disconnect :: IConnection conn => Handler b (HdbcSnaplet conn) ()
disconnect = withConn HDBC.disconnect

commit :: IConnection conn => Handler b (HdbcSnaplet conn) ()
commit = withConn HDBC.commit

rollback :: IConnection conn => Handler b (HdbcSnaplet conn) ()
rollback = withConn HDBC.rollback

runRaw :: IConnection conn => String -> Handler b (HdbcSnaplet conn) ()
runRaw str = withConn (`HDBC.runRaw` str)

run :: IConnection conn => String -> [SqlValue]
    -> Handler b (HdbcSnaplet conn) Integer
run str vs = withConn (\conn -> HDBC.run conn str vs)

prepare :: IConnection conn => String -> Handler b (HdbcSnaplet conn) Statement
prepare str = withConn (`HDBC.prepare` str)

clone :: IConnection conn => Handler b (HdbcSnaplet conn) conn
clone = withConn HDBC.clone

hdbcDriverName :: IConnection conn => Handler b (HdbcSnaplet conn) String
hdbcDriverName = withConn' HDBC.hdbcDriverName

hdbcClientVer :: IConnection conn => Handler b (HdbcSnaplet conn) String
hdbcClientVer = withConn' HDBC.hdbcClientVer

proxiedClientName :: IConnection conn => Handler b (HdbcSnaplet conn) String
proxiedClientName = withConn' HDBC.proxiedClientName

proxiedClientVer :: IConnection conn => Handler b (HdbcSnaplet conn) String
proxiedClientVer = withConn' HDBC.proxiedClientVer

dbServerVer :: IConnection conn => Handler b (HdbcSnaplet conn) String
dbServerVer = withConn' HDBC.dbServerVer

dbTransactionSupport :: IConnection conn => Handler b (HdbcSnaplet conn) Bool
dbTransactionSupport = withConn' HDBC.dbTransactionSupport

getTables :: IConnection conn => Handler b (HdbcSnaplet conn) [String]
getTables = withConn HDBC.getTables

describeTable :: IConnection conn => String
              -> Handler b (HdbcSnaplet conn) [(String, SqlColDesc)]
describeTable str = withConn (`HDBC.describeTable` str)

quickQuery' :: IConnection conn => String -> [SqlValue]
            -> Handler b (HdbcSnaplet conn) [[SqlValue]]
quickQuery' str vs = withConn (\conn -> HDBC.quickQuery' conn str vs)

quickQuery :: IConnection conn => String -> [SqlValue]
           -> Handler b (HdbcSnaplet conn) [[SqlValue]]
quickQuery str vs = withConn (\conn -> HDBC.quickQuery conn str vs)

sRun :: IConnection conn => String -> [Maybe String]
     -> Handler b (HdbcSnaplet conn) Integer
sRun str mstrs = withConn (\conn -> HDBC.sRun conn str mstrs)

withTransaction :: IConnection conn => (conn -> IO a)
                -> Handler b (HdbcSnaplet conn) a
withTransaction f = withConn (`HDBC.withTransaction` f)
