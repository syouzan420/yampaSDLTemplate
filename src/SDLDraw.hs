module SDLDraw (draw) where

import SDL (V4(V4),V2(V2))
import SDL.Primitive (fillCircle)

import MyData (MyData(mdRenderer,mdDouble))

draw :: MyData -> IO ()
draw wd = do 
  let renderer = mdRenderer wd
  let i = mdDouble wd
  fillCircle renderer (V2 60 (10+fromIntegral(floor i))) 10 (V4 102 178 255 255)
  putStrLn ("Hello " ++ show i)
