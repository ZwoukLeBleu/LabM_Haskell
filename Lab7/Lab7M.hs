import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import System.IO

type Node = Int
type Edge = (Node, Float)
type Graph = Map.Map Node [Edge]

dijkstra :: Graph -> Node -> Map.Map Node (Float, [Node])
dijkstra graph start = dijkstra' (Map.singleton start (0, [start])) (Set.singleton start)
  where
    dijkstra' distSet visited
      | Map.null distSet = Map.empty
      | otherwise = let (nextNode, (nextDist, path)) = Map.findMin distSet
                        neighbors = graph Map.! nextNode
                        updateDistances = foldl' (\map (node, dist) -> 
                          if node `Set.member` visited then map
                          else Map.insertWith min node (nextDist + dist, path ++ [node]) map) 
                          (Map.delete nextNode distSet) neighbors
                    in Map.insert nextNode (nextDist, path) (dijkstra' updateDistances (Set.insert nextNode visited))

printChemin :: (Float, [Node]) -> IO ()
printChemin (dist, path) = do
  putStrLn $ "Distance: " ++ show dist
  putStrLn "Path:"
  mapM_ print path

parseGraph :: [String] -> Graph
parseGraph = foldl' (\graph edge -> 
  let (node1:node2:weight:_) = words edge
  in Map.insertWith (++) (read node1) [(read node2, read weight)] graph) Map.empty

main :: IO ()
main = do
  putStrLn "Entrez les arcs du graphe, un arc par ligne," 
  putStrLn "Chaque arc doit etre de la forme 'sommetInitial sommetFinal longueur' (ex: '1 2 15')"
  putStrLn "Appuiyer sur [ENTER] sur une ligne vide pour terminer la saisie."
  inputLines <- userDijkstra
  let graph = parseGraph inputLines
  let shortestPaths = dijkstra graph 1
  mapM_ (\(node, path) -> do
    putStrLn $ "Sommet: " ++ show node
    printChemin path) (Map.toList shortestPaths)

userDijkstra :: IO [String]
userDijkstra = do
  line <- getLine
  if line == "" then return [] else do
    rest <- userDijkstra
    return (line : rest)


  
