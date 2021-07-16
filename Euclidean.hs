data Vertex = Vertex { x :: Float, y :: Float, z :: Float } deriving (Eq, Read, Show)
type PointCloud = [Vertex] --this is what's called a *type synonym*, i.e. "just new notation"

distance :: Vertex -> Vertex -> Float
distance (Vertex x y z) (Vertex x' y' z') = sqrt $ dx + dy + dz
    where dx = (x-x') ** 2
          dy = (y-y') ** 2
          dz = (z-z') ** 2

-- Let's rewrite addition using *monoids* (not monads!) (https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Monoid.html)
-- To make Vertex an instance of the Monoid type class, we must define two things:
--     mempty :: Vertex
--     <> :: Vertex -> Vertex -> Vertex
-- These should satisfy (although this is *not* enforced by the compiler (because Haskell doesn't have dependent types))
--     mempty <> v = v
--     v <> mempty = v
--     (u <> v) <> w = u <> (v <> w)
instance Monoid Vertex where
    (Vertex x y z) <> (Vertex x' y' z') = Vertex (x + x') (y + y') (z + z)
    mempty = Vertex 0 0 0

vertex1 = Vertex 1 2 3 
vertex2 = Vertex 1 2 3 
vertex3 = Vertex 1 2 3 
vertexCloud = PointCloud [vertex1,vertex2,vertex3]
