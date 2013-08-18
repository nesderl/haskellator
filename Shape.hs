module Shape (
    Shape(..),
    square,
    circle,
    rectangle,
    regularPolygon,
    shapeArea
) where

type Unit = Float 
type Vertex = (Unit, Unit)

data Shape = Rectangle Unit Unit
           | Ellipse Unit Unit
           | RightTriangle Unit Unit
           | Polygon [Vertex]
    deriving (Show)

square :: Unit -> Shape
square s = Rectangle s s

circle :: Unit -> Shape
circle c = Ellipse c c

rectangle :: Unit -> Unit -> Shape
rectangle w h = 
                let x = w/2
                    y = h/2
                in Polygon [(-x,-y), (x,-y), (x,y), (-x,y)]

regularPolygon :: Int -> Unit -> Shape
regularPolygon 0 _ = Polygon []
regularPolygon _ 0 = Polygon [(0, 0)]
regularPolygon points edge = 
                let angle_inc = -(3.141592 * 2) / fromIntegral points
                    angleToVertex angle = (edge * cos angle, edge * sin angle)
                    list_points 0 = []
                    list_points i = angleToVertex (i*angle_inc) : list_points (i-1)
                in Polygon $ list_points $ fromIntegral points

shapeArea :: Shape -> Unit
shapeArea (Rectangle w h) = w * h
shapeArea (Ellipse w h) = w * h * 3.141592
shapeArea (RightTriangle w h) = w * h * 0.5
shapeArea (Polygon (pivot:vertices)) = polygonArea vertices
                where polygonArea (v:v':[]) = triangleArea pivot v v'
                      polygonArea (v:v':vs) = triangleArea pivot v v' + polygonArea (v':vs)
                      polygonArea _         = 0


triangleArea :: Vertex -> Vertex -> Vertex -> Unit
triangleArea v1 v2 v3 = 
            let a = distance v1 v2
                b = distance v1 v3
                c = distance v2 v3
                s = (a + b + c) / 2
            in sqrt(s*(s-a)*(s-b)*(s-c))

distance :: Vertex -> Vertex -> Unit
distance (x, y) (x', y') = sqrt ((x-x')^2 + (y-y')^2)
