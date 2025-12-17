{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,arr,SF,(^<<),(>>>),(^+^))
import System.Random (StdGen)

import Inputs (Inputs(..))
import MyData (MyData(..))

mainSF :: StdGen -> MyData -> SF Inputs MyData
mainSF rgen md = proc i -> do
    rec
       let nmd = md{mdDouble=p}
       v1 <- arr setVel -< (i,0)
       v <- (1 ^+^) ^<< integral -< v1 
       p <- (10 ^+^) ^<< integral -< v
    returnA -< nmd 

setVel :: (Inputs,Double) -> Double
setVel (i,v)
  | inpUp i = v - 10 
  | inpDown i = v + 10
  | inpLeft i = v - 50 
  | inpRight i = v + 50 
  | otherwise = v
