{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.Hdbc.Types where

import            Control.Concurrent.MVar
import            Control.Monad.State
import            Database.HDBC (IConnection())
import qualified  Database.HDBC as HDBC
import            Data.Pool

#if MIN_VERSION_monad_control(0,3,0)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (liftBase)
#else
import Control.Monad.IO.Control (MonadControlIO)
#define control controlIO
#define liftBase liftIO
#endif

-- | The snaplet state type containing a resource pool, parameterised by a raw
-- HDBC connection.
data HdbcSnaplet c s
  =   (IConnection c, ConnSrc s)
  =>  HdbcSnaplet
  {   connSrc  :: s c
  ,   connVar  :: MVar c }

#if MIN_VERSION_monad_control(0,3,0)
class ConnSrc s where
  withConn   :: (MonadBaseControl IO m, IConnection c) => HdbcSnaplet c s -> (c -> m b) -> m b
  closeConn  :: (MonadBaseControl IO m, IConnection c) => HdbcSnaplet c s -> c -> m ()
#else
class ConnSrc s where
  withConn   :: (MonadControlIO m, IConnection c) => HdbcSnaplet c s -> (c -> m b) -> m b
  closeConn  :: (MonadControlIO m, IConnection c) => HdbcSnaplet c s -> c -> m ()
#endif

instance ConnSrc Pool where
  withConn       = withResource . connSrc
  closeConn _ _  = return ()

instance ConnSrc IO where
  withConn st fn = do
    let cv = connVar st
    emp   <-  liftBase $ isEmptyMVar cv
    conn  <-  if emp
                then do
                  conn <- liftBase $ connSrc st
                  liftBase $ putMVar cv conn
                  return conn
                else liftBase $ readMVar cv
    fn conn
  closeConn _  = liftBase . HDBC.disconnect
