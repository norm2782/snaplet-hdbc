module Snap.Snaplet.Hdbc.Types where

import            Control.Monad.IO.Control
import            Control.Monad.State
import            Database.HDBC (IConnection())
import qualified  Database.HDBC as HDBC
import            Data.Pool

class ConnSrc s where
  withConn   :: (MonadControlIO m, IConnection c) => s c -> (c -> m b) -> m b
  closeConn  :: (MonadControlIO m, IConnection c) => s c -> c -> m ()

instance ConnSrc Pool where
  withConn       = withResource
  closeConn _ _  = return ()

instance ConnSrc IO where
  withConn conn fn = do
    conn' <- liftIO conn
    fn conn'
  closeConn _ = liftIO . HDBC.disconnect
