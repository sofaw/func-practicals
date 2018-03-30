module GraphSpec where

class GraphSpec g where
  empty    :: g a
  addVert  :: Ord a => a -> g a -> g a
  addEdge  :: Ord a => a -> a -> g a -> g a
  delVert  :: Ord a => a -> g a -> g a
  delEdge  :: Ord a => a -> a -> g a -> g a
  vertices :: Ord a => g a -> [a]
  adj      :: Ord a => a -> g a -> [a]
  deg      :: Ord a => a -> g a -> Int
  deg v    =  length . adj v

graph    :: (GraphSpec g, Ord a) => [a] -> [(a,a)] -> g a
graph vs es  =  foldr (uncurry addEdge) gvs es
  where gvs  =  foldr addVert empty vs

showGraph :: (Ord a, Show a, GraphSpec g) => g a -> String
--showGraph gr = showGraph' (vertices gr)
--                where
--                showGraph' [] = ""
--                showGraph' (x:xs) = show x ++ "-" ++ show (adj x gr) ++ " " ++ showGraph' xs
showGraph gr = unwords (map (\x -> show x ++ "-" ++ show (adj x gr)) (vertices gr))
