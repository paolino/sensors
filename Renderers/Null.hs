{-# LANGUAGE TemplateHaskell #-}

module Renderers.Null (renderer) where

import Renderer
import Graphics.Gloss
import Gloss (CColor (..))
import Library
import Control.Lens
import Control.Lens.TH
import Data.List (zipWith7)
import Data.Machine.Moore
import Data.List
import Debug.Trace




picture _ = Blank 

renderer :: Renderer Float

renderer = Moore (Blank,0) step

step (Sensor qs) = Moore (Blank,sum qs) step
step _ = renderer
