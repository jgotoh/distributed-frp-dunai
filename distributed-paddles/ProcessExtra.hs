module ProcessExtra
    (
    initializeNode
    ) where

import qualified Network.Socket                as N
import Network.Common
import Network.Transport.TCP as NT
import qualified Control.Distributed.Process.Node
                                               as Node
import           Control.Exception.Base         ( IOException )
import qualified Network.Transport             as T

-- | Create a 'Node.LocalNode' and a TCP 'T.Transport'.
initializeNode
  :: N.HostName -> Port -> IO (Either IOException (Node.LocalNode, T.Transport))
initializeNode ip port = do
  t <- NT.createTransport (NT.defaultTCPAddr ip (show port))
                          NT.defaultTCPParameters
  case t of
    Left  l -> return $ Left l
    Right r -> do
      n <- createLocalNode r
      return $ Right (n, r)
