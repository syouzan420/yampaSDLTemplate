{-# LANGUAGE OverloadedStrings #-}
module MyData where

import SDL (V2(V2))
import SDL.Video.Renderer (Renderer,Texture)
import qualified SDL.Image as I
import qualified Data.Map as M
import Foreign.C.Types (CFloat)
import Data.Maybe (fromMaybe)
import Control.Exception (handle,SomeException)

import Data.Text (Text)

import SpriteName (SpriteName)

data MyData = MyData {
    mdRenderer :: !Renderer
   ,mdGetSprite :: !(SpriteName -> Texture)
   ,mdDouble :: !Double
}

title :: Text
title = "MyApp"

windowSize :: V2 CFloat
windowSize = V2 144 160 

defaultImagePath :: String
defaultImagePath = "resources/images/default.png"

spritePaths :: [(SpriteName, String)]
spritePaths = [(name, "resources/images/sprites/" ++ show name ++ ".png")
               | name <- [toEnum 0 ..]
              ]

loadMyData :: Renderer -> IO MyData
loadMyData renderer = do
  defaultTexture <- I.loadTexture renderer defaultImagePath
  
  let textureFail :: SomeException -> IO Texture
      textureFail e = return defaultTexture

  sprites <- mapM (handle textureFail . I.loadTexture renderer)
                                               (M.fromList spritePaths)
  return MyData {
    mdRenderer = renderer
   ,mdGetSprite = fromMaybe defaultTexture . (`M.lookup` sprites)
   ,mdDouble = 0
  }

