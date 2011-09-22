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

getConn :: (MonadState (HdbcSnaplet a) m) => m a
getConn = gets hdbcConn

withConn :: (MonadIO m, MonadState (HdbcSnaplet t) m) => (t -> IO b) -> m b
withConn f = do
  conn <- gets hdbcConn
  liftIO $ f conn

withConn' :: MonadState (HdbcSnaplet t) m => (t -> b) -> m b
withConn' f = do
  conn <- gets hdbcConn
  return $ f conn

disconnect :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m) => m ()
disconnect = withConn HDBC.disconnect

commit :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m) => m ()
commit = withConn HDBC.commit

rollback :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m) => m ()
rollback = withConn HDBC.rollback

runRaw :: (IConnection t, MonadState (HdbcSnaplet t) m, MonadIO m) => String 
       -> m ()
runRaw str = withConn (`HDBC.runRaw` str)

run :: (IConnection conn, MonadState (HdbcSnaplet conn) m, MonadIO m) => String
    -> [SqlValue] -> m Integer
run str vs = withConn (\conn -> HDBC.run conn str vs)

prepare :: (IConnection t, MonadState (HdbcSnaplet t) m, MonadIO m) => String
        -> m Statement
prepare str = withConn (`HDBC.prepare` str)

clone :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m) => m a
clone = withConn HDBC.clone

hdbcDriverName :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
               => m String
hdbcDriverName = withConn' HDBC.hdbcDriverName

hdbcClientVer :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
              => m String
hdbcClientVer = withConn' HDBC.hdbcClientVer

proxiedClientName :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
                  => m String
proxiedClientName = withConn' HDBC.proxiedClientName

proxiedClientVer :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
                 => m String
proxiedClientVer = withConn' HDBC.proxiedClientVer

dbServerVer :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
            => m String
dbServerVer = withConn' HDBC.dbServerVer

dbTransactionSupport :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
                     => m Bool
dbTransactionSupport = withConn' HDBC.dbTransactionSupport

getTables :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
          => m [String]
getTables = withConn HDBC.getTables

describeTable :: (IConnection t, MonadState (HdbcSnaplet t) m, MonadIO m)
              => String -> m [(String, SqlColDesc)]
describeTable str = withConn (`HDBC.describeTable` str)

quickQuery' :: (IConnection conn, MonadState (HdbcSnaplet conn) m, MonadIO m)
            => String -> [SqlValue] -> m [[SqlValue]]
quickQuery' str vs = withConn (\conn -> HDBC.quickQuery' conn str vs)

quickQuery :: (IConnection conn, MonadState (HdbcSnaplet conn) m, MonadIO m)
           => String -> [SqlValue] -> m [[SqlValue]]
quickQuery str vs = withConn (\conn -> HDBC.quickQuery conn str vs)

sRun :: (IConnection conn, MonadState (HdbcSnaplet conn) m, MonadIO m)
     => String -> [Maybe String] -> m Integer
sRun str mstrs = withConn (\conn -> HDBC.sRun conn str mstrs)

withTransaction :: (IConnection t, MonadState (HdbcSnaplet t) m, MonadIO m)
                => (t -> IO b) -> m b
withTransaction f = withConn (`HDBC.withTransaction` f)
