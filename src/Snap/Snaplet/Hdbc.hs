{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

  This module provides a very thin wrapper around HDBC
-}
module Snap.Snaplet.Hdbc (
  -- Snaplet functions
     HdbcSnaplet(..)
  ,  hdbcInit
  {- ,  disconnect-}
  {- ,  commit-}
  {- ,  getConn-}
  {- ,  rollback-}
  {- ,  runRaw-}
  {- ,  run-}
  {- ,  prepare-}
  {- ,  clone-}
  {- ,  hdbcDriverName-}
  {- ,  hdbcClientVer-}
  {- ,  proxiedClientVer-}
  {- ,  proxiedClientName-}
  {- ,  dbServerVer-}
  {- ,  dbTransactionSupport-}
  {- ,  getTables-}
  {- ,  describeTable-}
  {- ,  quickQuery'-}
  {- ,  quickQuery-}
  {- ,  sRun-}
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
import qualified  Database.HDBC as HDBC
import            Database.HDBC (IConnection(), SqlValue, SqlError, Statement)
import            Database.HDBC.ColTypes
import            Snap.Snaplet

class (IConnection c, MonadIO m) => HasHdbc m c where
  getHdbc :: m c

data HdbcSnaplet a = IConnection a => HdbcSnaplet {
  hdbcConn :: a }

instance IConnection a => HasHdbc (Handler b (HdbcSnaplet a)) a where
  getHdbc = getsSnapletState hdbcConn

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

withTransaction :: HasHdbc m c => (c -> IO a) -> m a
withTransaction f = withHdbc (`HDBC.withTransaction` f)

{- disconnect :: HasHdbc m c => m ()-}
{- disconnect = withHdbc HDBC.disconnect-}

{- commit :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m) => m ()-}
{- commit :: (IConnection c, HasHdbc m c) => m ()-}
{- commit = withHdbc HDBC.commit-}
{-
rollback :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m) => m ()
rollback = withHdbc HDBC.rollback

runRaw :: (IConnection a, MonadState (HdbcSnaplet a) m, MonadIO m) => String
       -> m ()
runRaw str = withHdbc (`HDBC.runRaw` str)

run :: (IConnection a, MonadState (HdbcSnaplet a) m, MonadIO m) => String
    -> [SqlValue] -> m Integer
run str vs = withHdbc (\conn -> HDBC.run conn str vs)

prepare :: (IConnection a, MonadState (HdbcSnaplet a) m, MonadIO m) => String
        -> m Statement
prepare str = withHdbc (`HDBC.prepare` str)

clone :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m) => m a
clone = withHdbc HDBC.clone

hdbcDriverName :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
               => m String
hdbcDriverName = withHdbc' HDBC.hdbcDriverName

hdbcClientVer :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
              => m String
hdbcClientVer = withHdbc' HDBC.hdbcClientVer

proxiedClientName :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
                  => m String
proxiedClientName = withHdbc' HDBC.proxiedClientName

proxiedClientVer :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
                 => m String
proxiedClientVer = withHdbc' HDBC.proxiedClientVer

dbServerVer :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
            => m String
dbServerVer = withHdbc' HDBC.dbServerVer

dbTransactionSupport :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
                     => m Bool
dbTransactionSupport = withHdbc' HDBC.dbTransactionSupport

getTables :: (IConnection a, MonadIO m, MonadState (HdbcSnaplet a) m)
          => m [String]
getTables = withHdbc HDBC.getTables

describeTable :: (IConnection a, MonadState (HdbcSnaplet a) m, MonadIO m)
              => String -> m [(String, SqlColDesc)]
describeTable str = withHdbc (`HDBC.describeTable` str)

quickQuery' :: (IConnection a, MonadState (HdbcSnaplet a) m, MonadIO m)
            => String -> [SqlValue] -> m [[SqlValue]]
quickQuery' str vs = withHdbc (\conn -> HDBC.quickQuery' conn str vs)

quickQuery :: (IConnection a, MonadState (HdbcSnaplet a) m, MonadIO m)
           => String -> [SqlValue] -> m [[SqlValue]]
quickQuery str vs = withHdbc (\conn -> HDBC.quickQuery conn str vs)

sRun :: (IConnection a, MonadState (HdbcSnaplet a) m, MonadIO m)
     => String -> [Maybe String] -> m Integer
sRun str mstrs = withHdbc (\conn -> HDBC.sRun conn str mstrs)
-}

{- withTransaction :: (IConnection a, MonadState (HdbcSnaplet a) m, MonadIO m)-}
                {- => (a -> IO b) -> m b-}

