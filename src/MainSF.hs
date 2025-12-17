{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,arr,SF,(^<<),(>>>),(^+^))
import System.Random (StdGen)

import Inputs (Inputs(..))
import IOAction (ioact)
import MyData (MyData(..))

mainSF :: StdGen -> MyData -> SF Inputs (IO ()) 
mainSF rgen wd = update rgen wd >>> arr ioact 

update :: StdGen -> MyData -> SF Inputs MyData
update rgen wd = proc i -> do
    rec
       let nwd = wd{mdDouble=p}
       v1 <- arr setVel -< (i,0)
       v <- (1 ^+^) ^<< integral -< v1 
       p <- (10 ^+^) ^<< integral -< v
    returnA -< nwd 

setVel :: (Inputs,Double) -> Double
setVel (i,v)
  | inpUp i = v - 10 
  | inpDown i = v + 10
  | inpLeft i = v - 50 
  | inpRight i = v + 50 
  | otherwise = v
