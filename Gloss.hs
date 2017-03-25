module Gloss where
import Graphics.Gloss

--  gloss colors are abstract , wtf
data CColor = CColor Float Float Float Float


-- a segment with a color
data Seg = Seg CColor Point Point

seg0 = Seg (CColor 0 0 0 1) (0,0) (0,0) -- a black collapsed line in (0,0)

-- a segment in Gloss
segToPicture ::  Seg -> Picture
segToPicture (Seg (CColor r g b a) p1 p2) = Color (makeColor r g b a) (Line [p1,p2])

