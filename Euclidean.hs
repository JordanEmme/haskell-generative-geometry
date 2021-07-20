-- Vertices
data Vertex = Vertex { x :: Float, y :: Float, z :: Float } deriving (Eq, Read, Show)

instance Monoid Vertex where
    (Vertex x y z) <> (Vertex x' y' z') = Vertex (x + x') (y + y') (z + z)
    mempty = Vertex 0 0 0

distance :: Vertex -> Vertex -> Float
distance (Vertex x y z) (Vertex x' y' z') = sqrt $ dx + dy + dz
    where dx = (x-x') ** 2
          dy = (y-y') ** 2
          dz = (z-z') ** 2

scaleVertex :: Vertex -> Float -> Vertex
scaleVertex (Vertex x y z) s = Vertex (s * x) (s * y) (s * z)


-- Edges
type HalfEdge = (Vertex, Vertex)

-- Tim: rewrite this in a Haskell-y-er way
edgeLength :: HalfEdge -> Float
edgeLength (u, v) = distance u v

-- Tim: rewrite this in a Haskell-y-er way
mirrorEdge :: HalfEdge -> HalfEdge
mirrorEdge (u, v) = (v, u)

splitEdge :: HalfEdge -> (HalfEdge, HalfEdge)
splitEdge (u, v) = (firstHalf, secondHalf)
    where midpoint   = scaleVertex (u <> v) 0.5
          firstHalf  = HalfEdge u midpoint
          secondHalf = HalfEdge midpoint v


-- Triangles
type Triangle = (HalfEdge, HalfEdge, HalfEdge)

splitTriangle :: Triangle -> HalfEdge -> Maybe (Triangle, Triangle)
splitTriangle (a, b, c) a = Just ((secondHalf, b, newEdge), (firstHalf, (mirrorEdge newEdge), c))
     where (firstHalf, secondHalf) = splitEdge a
           newEdge                 = (snd b, fst secondHalf)
splitTriangle (a, b, c) b = splitTriangle (b, c, a) b
splitTriangle (a, b, c) c = splitTriangle (c, a, b) c
splitTriangle _           = Nothing


-- Point clouds
type PointCloud = [Vertex] --this is what's called a *type synonym*, i.e. "just new notation"


-- Testing

vertex1 = Vertex 1 2 3 
vertex2 = Vertex 1 2 3 
vertex3 = Vertex 1 2 3 
vertexCloud = PointCloud [vertex1,vertex2,vertex3]
