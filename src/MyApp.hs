module MyApp where

import Control.Monad (when)
import System.Random (getStdGen)
import System.Exit (exitSuccess)
import Data.IORef (IORef)
import FRP.Yampa (reactimate)
import SDL.Event (pollEvents,eventPayload,EventPayload(..))
import SDL.Input.Keyboard (getKeyboardState)

import WithSDL (withSDL)
import WithSDLAudio (withSDLAudio)
import WithSDLVideo (withSDLVideo)
import WithIORef (withIORefInit,getTimeDifference)
import IOAction (ioact)
import Inputs (initInput,readInputs,Inputs(inpQ))
import MainSF (mainSF)
import MyData (loadMyData,MyData)

appMain :: IO ()
appMain = withSDL $ do 
    withSDLAudio $ do
      withSDLVideo $ \renderer -> do
        md <- loadMyData renderer
        withIORefInit $ \tRef -> do
          rgen <- getStdGen
          reactimate (return initInput)
                     (input tRef)
                     output
                     (mainSF rgen md)

input :: IORef Double -> Bool -> IO (Double,Maybe Inputs) 
input tRef _ = do
  es <- pollEvents
  keyDown <- getKeyboardState
  let inputs = readInputs keyDown 
  dt <- getTimeDifference tRef 
  when (any (isQuit . eventPayload) es || inpQ inputs) exitSuccess
  return (dt,Just inputs) 

isQuit :: EventPayload -> Bool
isQuit QuitEvent = True
isQuit (WindowClosedEvent _) = True
isQuit _ = False
  
output ::  Bool -> MyData -> IO Bool
output _ md = ioact md >> return False
