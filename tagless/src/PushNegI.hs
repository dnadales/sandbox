module PushNegI where

import           Exp

pushNeg :: Exp -> Exp
pushNeg e@Lit{}           = e
pushNeg e@(Neg Lit{})     = e
pushNeg (Neg (Neg e))     = e
pushNeg (Neg (Add e0 e1)) = Add (pushNeg (Neg e0)) (pushNeg (Neg e1))
pushNeg (Add e0 e1)       = Add (pushNeg e0) (pushNeg e1)

ti1Norm = pushNeg ti1
