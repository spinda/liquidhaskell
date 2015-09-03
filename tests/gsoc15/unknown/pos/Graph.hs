
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Data.Set


type Size = Int
data Map k v = Tip | Bin Size k v (Map k v) (Map k v)
             deriving (Eq, Ord)


data Edges edge 


[lq| measure getEdgesIncoming :: (Edges edge) -> (Set edge) |]

[lq| measure getEdgesOutgoing :: (Edges edge) -> (Set edge) |]

data Graph node edge = 
  Graph {
    graphMap :: 
      Map node (Edges edge, Edges edge)
  }


[lq| invariant {v: (Graph node edge) | (getGraphIncoming v) = (getGraphOutgoing v) } |]

[lq| measure getGraphIncoming :: (Graph node edge) -> (Set edge)
    getGraphIncoming (Graph m) = (getMapIncoming m)
  |]

[lq| measure getGraphOutgoing :: (Graph node edge) -> (Set edge)
    getGraphOutgoing (Graph m) = (getMapOutgoing m)
  |]


[lq| measure getMapIncoming :: (Map node (Edges edge, Edges e)) -> (Set edge)
    getMapIncoming (Tip) = {v | Set_emp v }
    getMapIncoming (Bin size k v lm rm) = (Set_cup (getPairIncoming v) (Set_cup (getMapIncoming lm) (getMapIncoming rm)))
  |]


[lq| measure getMapOutgoing :: (Map node (Edges edge, Edges edge)) -> (Set edge)
    getMapOutgoing (Tip) = {v | Set_emp v }
    getMapOutgoing (Bin size k v lm rm) = Set_cup (getPairOutgoing v) (Set_cup (getMapOutgoing lm) (getMapOutgoing rm))
  |]

[lq| measure getPairIncoming :: (Edges edge, Edges e) -> (Set edge)
    getPairIncoming (x, y) = (getEdgesIncoming x)
  |]

[lq| measure getPairOutgoing :: (Edges e, Edges edge) -> (Set edge)
    getPairOutgoing (x, y) = (getEdgesOutgoing y)
  |]

