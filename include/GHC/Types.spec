module spec GHC.Types where

-- TODO: Drop prefix below
GHC.Types.EQ :: {v:GHC.Types.Ordering | v = (cmp v) }
GHC.Types.LT :: {v:GHC.Types.Ordering | v = (cmp v) }
GHC.Types.GT :: {v:GHC.Types.Ordering | v = (cmp v) }

measure cmp :: GHC.Types.Ordering -> GHC.Types.Ordering
cmp (GHC.Types.EQ) = { v | v = GHC.Types.EQ }
cmp (GHC.Types.LT) = { v | v = GHC.Types.LT }
cmp (GHC.Types.GT) = { v | v = GHC.Types.GT }


GHC.Types.True  :: {v:GHC.Types.Bool | (Prop(v))}
GHC.Types.False :: {v:GHC.Types.Bool | (~ (Prop(v)))}


GHC.Types.isTrue#  :: n:_ -> {v:GHC.Types.Bool | ((n = 1) <=> (Prop(v)))}


type GeInt N = {v: GHC.Types.Int  | v >= N }
type LeInt N = {v: GHC.Types.Int  | v <= N }
type Nat     = {v: GHC.Types.Int  | v >= 0 }
type Even    = {v: GHC.Types.Int  | (v mod 2) = 0 }
type Odd     = {v: GHC.Types.Int  | (v mod 2) = 1 }
type BNat N  = {v: Nat            | v <= N }
type TT      = {v: GHC.Types.Bool | Prop v}
type FF      = {v: GHC.Types.Bool | not (Prop v)}

predicate Max V X Y = if X > Y then V = X else V = Y
predicate Min V X Y = if X < Y then V = X else V = Y



