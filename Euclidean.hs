data Vertex = Vertex { x :: Float, y :: Float, z :: Float } deriving (Eq, Read, Show)
data PointCloud = Pointcloud { vertices :: [Vertex] }

distance :: Vertex -> Vertex -> Float
distance (Vertex x y z) (Vertex x' y' z') = sqrt $ dx + dy + dz
    where dx = (x-x') ** 2
          dy = (y-y') ** 2
          dz = (z-z') ** 2
