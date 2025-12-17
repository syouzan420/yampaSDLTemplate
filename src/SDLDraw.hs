module SDLDraw (draw) where

import SDL (V4(V4),V2(V2),Point(P))
import SDL.Video.Renderer (copy,Rectangle(..))
import SDL.Primitive (fillCircle)

import MyData (MyData(mdRenderer,mdGetSprite,mdDouble))
import SpriteName (SpriteName(PlayerFrontLeft))

draw :: MyData -> IO ()
draw md = do 
  let renderer = mdRenderer md
  let getSprite = mdGetSprite md
  let playerImage = getSprite PlayerFrontLeft
  let i = mdDouble md
  fillCircle renderer (V2 60 (10+fromIntegral(floor i))) 10 (V4 102 178 255 255)
  copy renderer playerImage Nothing (Just (Rectangle (P (V2 10 10)) (V2 16 16)))
  putStrLn ("Hello " ++ show i)
