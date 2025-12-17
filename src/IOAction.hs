module IOAction(ioact) where

import WithSDLVideo (withRenderer)
import SDLDraw (draw)
import MyData (MyData(mdRenderer))

ioact :: MyData -> IO ()
ioact wd = do
  let renderer = mdRenderer wd
  withRenderer renderer $ draw wd
