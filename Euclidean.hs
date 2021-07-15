data Vertex = Vertex { x :: Float, y :: Float, z :: Float } deriving (Eq, Read, Show)
data PointCloud = PointCloud { vertices :: [Vertex] } deriving (Eq, Read, Show)

distance :: Vertex -> Vertex -> Float
distance (Vertex x y z) (Vertex x' y' z') = sqrt $ dx + dy + dz
    where dx = (x-x') ** 2
          dy = (y-y') ** 2
          dz = (z-z') ** 2

addition :: Vertex -> Vertex -> Vertex
addition (Vertex x y z) (Vertex x' y' z') = Vertex (x + x') (y + y') (z + z)

vertex1 = Vertex 1 2 3 
vertex2 = Vertex 1 2 3 
vertex3 = Vertex 1 2 3 
vertexCloud = PointCloud [vertex1,vertex2,vertex3] 