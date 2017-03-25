{-# LANGUAGE TemplateHaskell #-}
module State where

import Data.List.PointedList
import Control.Lens.TH
import Data.Machine.Mealy (Mealy)

import Renderer (Renderer)
import Filter (SensorFilter)

data State a = State 
	{ _renderers :: PointedList (Renderer a)
	, _filters :: [SensorFilter]
	, _halting :: Bool
        , _pausing :: Bool
	}

$(makeLenses ''State)


