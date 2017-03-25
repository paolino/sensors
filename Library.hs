-- | generic stuff / missings

module Library  where

import Data.Machine.Moore

modulus2D x y = sqrt (x ^ 2 + y ^ 2)

-- | approximate to given number of decimals
roundn 	:: RealFrac a 
	=> Int -- ^ number of decimals
	-> a -- ^ fractional to round
	-> a -- ^ rounded fractional
roundn n x = fromIntegral (round (x * (10 ^ n))) / (10 ^ n)

-- | linear interpolation, 
linear 	:: Fractional a
	=> Int  -- ^ steps
	-> a -- ^ start
	-> a -- ^ end
	-> [a] -- ^ interpolated values, excluding start
linear n x y =  tail $ scanl (+) x (replicate n $ (y - x)/fromIntegral n) 

-- | quantization
quant 	:: RealFrac a
	=> Int -- ^ quantization factor
	-> a -- ^ unquantized
	-> a -- ^ quantized   
quant n x = fromIntegral (floor (x * fromIntegral n))/fromIntegral n

quantiz  :: RealFrac a
	=> a -- ^ quantum
	-> a -- ^ unquantized
	-> a -- ^ quantized 
quantiz q x = fromIntegral (floor (q / x)) * q

stepMoore :: a -> Moore a b -> Moore a b
stepMoore x (Moore _ f) = f x


