module Site.Types (
  Channels,
  Streams,
  Abbreviations
) where

-- Channels
import qualified Data.Map as Map
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

-- Streams
import qualified System.IO.Streams as S
import qualified Data.ByteString.Char8 as C

-- Abbreviations
import Text.Regex.TDFA (Regex)

-- the websocket server channels
type Channels = TVar (Map.Map String (TChan String, Integer))

-- the pigments server streams
type Streams = (S.OutputStream C.ByteString, S.InputStream C.ByteString)

-- the abbreviations map
type Abbreviations = Map.Map String (Regex, String)
