module FlataI where

import           Exp
import           PushNegI

-- | Reassociate additions to the right.
flata :: Exp -> Exp
flata e@Lit{}              = e
-- At this point we assume all the negations are flatted down!:
flata e@Neg{}              = e
flata (Add (Add e0 e1) e2) = flata (Add e0 (Add e1 e2))
flata (Add e0 e1)          = Add e0 (flata e1)

norm :: Exp -> Exp
norm = flata . pushNeg

tfi3 = Add ti1 (Neg (Neg ti1))
