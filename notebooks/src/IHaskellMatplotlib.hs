module IHaskellMatplotlib where

import           IHaskell.Display
import           Graphics.Matplotlib
import qualified Data.ByteString.Char8         as Char
import qualified System.IO.Temp                as Sys
import qualified System.Directory              as Sys
import qualified Control.Exception             as E

instance IHaskellDisplay Matplotlib where
  display = displaySVG

basename = ".ihaskell-matplotlib."

displayPNG :: Width -> Height -> Matplotlib -> IO Display
displayPNG w h p = do
  fname <- Sys.emptySystemTempFile $ basename <> "png"
  file fname p
  imgData <- Char.readFile fname
  removeFile fname
  return $ Display [png w h $ base64 imgData]

displaySVG :: Matplotlib -> IO Display
displaySVG p = do
  fname <- Sys.emptySystemTempFile $ basename <> "svg"
  file fname p
  imgData <- Char.readFile fname
  removeFile fname
  return $ Display [svg $ Char.unpack imgData]

removeFile file = Sys.removeFile file `E.catch` handleFileError
 where
  handleFileError :: IOError -> IO ()
  handleFileError _ = pure ()
