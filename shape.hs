--module Shape(...) where

data Shape = Rectangle Side Side
            | Ellipse Radius Radius
            | RtTriangle Side Side
            | Polygon [Vertex]
        deriving Show
        
type Radius = Float
type Side = Float
type Vertex = (Float, Float)

square :: Side -> Shape
square s = Rectangle s s

circle :: Radius -> Shape
circle r = Ellipse r r

rectangle :: Side -> Side -> Shape
rectangle s1 s2 = Polygon [(0,0),(s1,0),(s1,s2),(0,s2)]

rtTrignale :: Side -> Side -> Shape
rtTriangle s1 s2 = Polygon [(0,0),(0,s1),(s1,s2)]

regularPolygon :: Int -> Side -> Shape
regularPolygon n s = undefined