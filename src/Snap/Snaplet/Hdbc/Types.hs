{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.Hdbc.Types where

import            Control.Concurrent.MVar
import            Control.Monad.CatchIO
import            Control.Monad.State
import            Database.HDBC (IConnection())
import qualified  Database.HDBC as HDBC
import            Data.Pool

-- | The snaplet state type containing a resource pool, parameterised by a raw
-- HDBC connection.
data HdbcSnaplet c s
  =   (IConnection c, ConnSrc s)
  =>  HdbcSnaplet
  {   connSrc  :: s c
  ,   connVar  :: MVar c }

class ConnSrc s where
  withConn   :: (MonadCatchIO m, IConnection c) => HdbcSnaplet c s -> (c -> m b) -> m b
  closeConn  :: (MonadCatchIO m, IConnection c) => HdbcSnaplet c s -> c -> m ()

instance ConnSrc Pool where
  withConn       = withResource . connSrc
  closeConn _ _  = return ()

instance ConnSrc IO where
  withConn st fn = do
    let cv = connVar st
    emp   <-  liftIO $ isEmptyMVar cv
    conn  <-  if emp
                then do
                  conn <- liftIO $ connSrc st
                  liftIO $ putMVar cv conn
                  return conn
                else liftIO $ readMVar cv
    fn conn
  closeConn _  = liftIO . HDBC.disconnect
