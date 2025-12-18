{-# LANGUAGE Arrows #-}

module MainSF(mainSF) where

import FRP.Yampa (integral,returnA,arr,SF,(^<<),(>>>),(^+^))
import System.Random (StdGen)
import Foreign.C.Types (CFloat)
import Data.Point2 (Point2(..))
import Data.Vector2 (vector2,vector2Y,Vector2)
import Data.AffineSpace ((.+^))

import Inputs (Inputs(..))
import MyData (MyData(..))

mainSF :: StdGen -> MyData -> SF Inputs MyData
mainSF rgen md = proc i -> do
    rec
       let nmd = md{mdDouble=d,mdPlayerPos=p}
       v1 <- arr setVel -< (i,vector2 0 0)
       v <- (vector2 0 0 ^+^) ^<< integral -< v1 
       p <- (mdPlayerPos md .+^) ^<< integral -< v
       d <- (+ mdDouble md) ^<< integral -< realToFrac (vector2Y v)
    returnA -< nmd 

setVel :: (Inputs,Vector2 CFloat) -> Vector2 CFloat 
setVel (i,v)
  | inpUp i = v ^+^ vector2 0 (-50) 
  | inpDown i = v ^+^ vector2 0 50 
  | inpLeft i = v ^+^ vector2 (-50) 0 
  | inpRight i = v ^+^ vector2 50 0 
  | otherwise = v
