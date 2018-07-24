-- | A higher-order typed language using a tagless encoding.
module LambdaCalc.HOTLTE where

vz :: (a, env) -> a
vz (vc, _) = vc

-- | Successor function.
--
-- Now note that (vs vz) is a lookup function!
--
-- >>> :t vs vz
-- vs vz :: (b, (a, env)) -> a
--
-- The function above will lookup the second element on a nested tuple.
vs :: (env -> a) -> (b, env) -> a
vs vp (_, envr) = vp envr

b :: t -> env -> t
b bv _ = bv

l :: ((a, env) -> b) -> env -> (a -> b)
l e env = \x -> e (x, env)

a :: (env -> a -> b) -> (env -> a) -> env -> b
a e0 e1 env = (e0 env) (e1 env)

-- | In this case you don't need an evaluation function!
--
-- >>> tf1 ()
-- True
--
-- >>> tf1 (20)
-- True
--
tf1 :: env -> Bool
tf1 = a (l vz) (b True)

-- | The inferred type of the expression tells you what is needed in the
-- environment!
--
--
tf2 = a (l (vs vz)) (b True)

-- This will also result in an error! Quite cool!
--
-- > tf2a = a (b True) (b False)
-- >     • Couldn't match type ‘Bool’ with ‘Bool -> b’
--
