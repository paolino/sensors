-- | interface types for renderers

module Renderer where

import Data.Machine.Moore (Moore)

-- control index
newtype ControlI = ControlI Int

-- | renderer can receive data from sensors or controls
data Input = Sensor [Float] | Control Int Float

-- | renderers should output a picture a serialization and a deserializer
-- data Output = Output Picture -- String (String -> Renderer)

type Renderer a = Moore Input (IO (),a)


