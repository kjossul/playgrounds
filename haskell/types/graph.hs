data Node a = Node {
  id :: Int,
  val :: a,
  next :: [Int]
} deriving (Show, Eq)

data Graph a = Graph [Node a] deriving (Show, Eq)

graph_lookup :: Graph a -> Int -> Maybe a
graph_lookup (Graph []) _ = Nothing
graph_lookup (Graph ((Node id val _):rest)) n 
  | id == n   = Just val
  | otherwise = graph_lookup (Graph rest) n

adjacents :: Node a -> Node a -> Bool
adjacents (Node n _ ns1) (Node m _ ns2) = elem n ns2 || elem m ns1

instance Functor Node where
  fmap f (Node id val next) = Node id (f val) next

instance Functor Graph where
  fmap f (Graph ns) = Graph $ map (fmap f) ns
