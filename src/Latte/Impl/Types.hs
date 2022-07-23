module Latte.Impl.Types where

import qualified Control.Monad.Except as CME
import qualified Control.Monad.Reader as CMR
import qualified Data.Map as DM
import qualified Latte.Abs as LA
import Prelude (Bool, IO, Int, Eq, Ord (compare), Show, String, Bool(False), error)

type Loc = Int

type Id = LA.Ident

type LnCol = (Int, Int)

data Store
  = IntVal Int
  | BoolVal Bool
  | StrVal String
  | None LA.Type
  | Ret
  deriving (Eq, Show)

instance Ord Store where
  (IntVal x1) `compare` (IntVal x2) = x1 `compare` x2
  (BoolVal x1) `compare` (BoolVal x2) = x1 `compare` x2
  (StrVal x1) `compare` (StrVal x2) = x1 `compare` x2
  _ `compare` _ = error "Comparing different types!"


type Mem = DM.Map Loc Store

type Vars = DM.Map Id Loc

type Funs = DM.Map Id ContF

data Env = MakeEnv
  { vars :: Vars,
    funs :: Funs,
    retType :: LA.Type
  }

data State = MakeState
  { mem :: Mem,
    last :: Loc
  }

data RuntimeException
  = ArithmeticException LnCol
  | ArgumentException LnCol
  | TypeException LnCol
  | MulitpleDeclarationException LnCol
  | ConstantAssignmentException LnCol
  | NameException LnCol
  | NoReturn LnCol
  | NoMain
  deriving (Show)

type Eval resT = CMR.ReaderT Env (CME.ExceptT RuntimeException IO) resT

type Res = State

type Cont = State -> Eval Res

type ContE = Store -> State -> Eval Res

type ContF = [LA.Expr] -> Env -> State -> Eval Res
