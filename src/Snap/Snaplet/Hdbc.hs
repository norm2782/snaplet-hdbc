{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|

  This module provides a very thin wrapper around HDBC
-}
module Snap.Snaplet.Hdbc where

import           Database.HDBC (IConnection(), SqlValue, Statement, SqlColDesc)
import qualified Database.HDBC as HDBC
import           Snap.Snaplet
import           Control.Monad.State

data HdbcSnaplet conn = IConnection conn => HdbcSnaplet {
    hdbcConn :: conn
  }

hdbcInit :: IConnection conn => conn -> SnapletInit b (HdbcSnaplet conn)
hdbcInit conn = makeSnaplet "hdbc" "HDBC abstraction" Nothing $ do
  onUnload $ HDBC.disconnect conn
  return $ HdbcSnaplet conn

getConn :: IConnection conn => Handler b (HdbcSnaplet conn) conn
getConn = gets hdbcConn

runUnit :: IConnection conn => (conn -> IO ())
        -> Handler b (HdbcSnaplet conn) ()
runUnit f = do
  conn <- gets hdbcConn
  liftIO $ f conn

disconnect :: IConnection conn => Handler b (HdbcSnaplet conn) ()
disconnect = runUnit HDBC.disconnect

commit :: IConnection conn => Handler b (HdbcSnaplet conn) ()
commit = runUnit HDBC.commit

rollback :: IConnection conn => Handler b (HdbcSnaplet conn) ()
rollback = runUnit HDBC.rollback

runRaw :: IConnection conn => String -> Handler b (HdbcSnaplet conn) ()
runRaw str = do
  conn <- gets hdbcConn
  liftIO $ HDBC.runRaw conn str

run :: IConnection conn => String -> [SqlValue]
    -> Handler b (HdbcSnaplet conn) Integer
run str vs = do
  conn <- gets hdbcConn
  liftIO $ HDBC.run conn str vs

prepare :: IConnection conn => String -> Handler b (HdbcSnaplet conn) Statement
prepare str = do
  conn <- gets hdbcConn
  liftIO $ HDBC.prepare conn str

clone :: IConnection conn => Handler b (HdbcSnaplet conn) conn
clone = do
  conn <- gets hdbcConn
  liftIO $ HDBC.clone conn

pureStr :: IConnection conn => (conn -> String)
        -> Handler b (HdbcSnaplet conn) String
pureStr f =  do
  conn <- gets hdbcConn
  return $ f conn

hdbcDriverName :: IConnection conn => Handler b (HdbcSnaplet conn) String
hdbcDriverName = pureStr HDBC.hdbcDriverName

hdbcClientVer :: IConnection conn => Handler b (HdbcSnaplet conn) String
hdbcClientVer = pureStr HDBC.hdbcClientVer

proxiedClientName :: IConnection conn => Handler b (HdbcSnaplet conn) String
proxiedClientName = pureStr HDBC.proxiedClientName

proxiedClientVer :: IConnection conn => Handler b (HdbcSnaplet conn) String
proxiedClientVer = pureStr HDBC.proxiedClientVer

dbServerVer :: IConnection conn => Handler b (HdbcSnaplet conn) String
dbServerVer = pureStr HDBC.dbServerVer

dbTransactionSupport :: IConnection conn => Handler b (HdbcSnaplet conn) Bool
dbTransactionSupport = do
  conn <- gets hdbcConn
  return $ HDBC.dbTransactionSupport conn

getTables :: IConnection conn => Handler b (HdbcSnaplet conn) [String]
getTables = do
  conn <- gets hdbcConn
  liftIO $ HDBC.getTables conn

describeTable :: IConnection conn => String
              -> Handler b (HdbcSnaplet conn) [(String, SqlColDesc)]
describeTable str = do
  conn <- gets hdbcConn
  liftIO $ HDBC.describeTable conn str

quickQuery' :: IConnection conn => String -> [SqlValue]
            -> Handler b (HdbcSnaplet conn) [[SqlValue]]
quickQuery' str vs = do
  conn <- gets hdbcConn
  liftIO $ HDBC.quickQuery' conn str vs

quickQuery :: IConnection conn => String -> [SqlValue]
           -> Handler b (HdbcSnaplet conn) [[SqlValue]]
quickQuery str vs = do
  conn <- gets hdbcConn
  liftIO $ HDBC.quickQuery conn str vs

sRun :: IConnection conn => String -> [Maybe String]
     -> Handler b (HdbcSnaplet conn) Integer
sRun str mstrs = do
  conn <- gets hdbcConn
  liftIO $ HDBC.sRun conn str mstrs

withTransaction :: IConnection conn => (conn -> IO a) -> Handler b (HdbcSnaplet conn) a
withTransaction f = do
  conn <- gets hdbcConn
  liftIO $ HDBC.withTransaction conn f

