{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ResetIds where

import           GHC.Generics

type Name = String
newtype Id = Id { id :: Int } deriving (Eq, Show, Num)

data VarId = VarId
    { vName :: Name
    , vId   :: Id
    , vSort :: SortId
    } deriving (Eq, Show, Generic)

data FuncId = FuncId
    { fName    :: Name
    , fId      :: Id
    , argsSort :: [SortId]
    , retSort  :: SortId
    } deriving (Eq, Show, Generic)

data SortId = SortId
    { sName :: Name
    , sId   :: Id
    } deriving (Eq, Show, Generic)


data Exp = Var VarId
         | FuncCall FuncId [Exp]
         | Plus Exp Exp
         | Times Exp Exp
         deriving (Eq, Show, Generic)

-- * The problem:
--
-- Reset the unique identifiers in an expression, by setting them to zero.
--
int = SortId "Int" 10
bool = SortId "Bool" 11

var0 = Var (VarId "v0" 0 int)
var1 = Var (VarId "v1" 1 int)
var2 = Var (VarId "v2" 2 int)

var3 = Var (VarId "v3" 3 bool)
var4 = Var (VarId "v4" 4 bool)

fcall0 = FuncCall (FuncId "foo" 20 [int, bool] int) [var2, var3]
fcall1 = FuncCall (FuncId "bar" 21 [bool, bool] int) [var3, var4]

exp0 = Plus (Times var0 var1 ) (Plus fcall0 var0)

exp1 = Times (Times (Plus var0 fcall0) fcall1) (Times var1 var2)

var0' = Var (VarId "v0" 90 int)
var1' = Var (VarId "v1" 91 int)
var2' = Var (VarId "v2" 92 int)

exp0' = Plus (Times var0' var1' ) (Plus fcall0 var0')

-- We would like to compare @exp0@ and @exp0'@ modulo the unique id's.

-- We can do this with some boilerplate:
(~~) :: Exp -> Exp -> Bool
e0 ~~ e1 = resetExp e0 == resetExp e1

resetExp :: Exp -> Exp
resetExp (Var vid)           = Var (resetVarId vid)
resetExp (FuncCall fid exps) = FuncCall (resetFuncId fid) (resetExp <$> exps)
resetExp (Plus e0 e1)        = Plus (resetExp e0) (resetExp e1)
resetExp (Times e0 e1)       = Times (resetExp e0) (resetExp e1)

resetVarId :: VarId -> VarId
resetVarId (VarId n _ s) = VarId n 0 (resetSortId s)

resetSortId :: SortId -> SortId
resetSortId (SortId n _) = SortId n 0

resetFuncId :: FuncId -> FuncId
resetFuncId (FuncId n _ as r) = FuncId n 0 (resetSortId <$> as) (resetSortId r)

-- Whatalotaboilerplate! Let's try to solve this...
