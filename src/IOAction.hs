module IOAction(ioact) where

import WithSDLVideo (withRenderer)
import SDLDraw (draw)
import MyData (MyData(mdRenderer))

ioact :: MyData -> IO ()
ioact md = do
  let renderer = mdRenderer md
  withRenderer renderer $ draw md
