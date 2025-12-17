module SDLDraw (draw) where

import SDL (V4(V4),V2(V2))
import SDL.Primitive (fillCircle)

import MyData (MyData(mdRenderer,mdDouble))

draw :: MyData -> IO ()
draw md = do 
  let renderer = mdRenderer md
  let i = mdDouble md
  fillCircle renderer (V2 60 (10+fromIntegral(floor i))) 10 (V4 102 178 255 255)
  putStrLn ("Hello " ++ show i)
