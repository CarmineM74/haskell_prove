module UsePictures where
import Pictures

whiteHorse :: Picture
whiteHorse = invertColour horse

rotateHorse :: Picture -> Picture
rotateHorse = rotate

fourSquares :: Picture
fourSquares = let row = beside white black in above row $ invertColour row

fourSquares' :: Picture
fourSquares' = let row = beside white black in above row $ flipV row
