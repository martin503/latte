module Latte.Impl.Helpers where

import qualified Control.Monad.Except as CME
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Reader as CMR
import Data.Map (insert, keys, lookup, member, singleton, (!))
import Data.Maybe (fromMaybe)
import qualified Latte.Abs as LA
import qualified Latte.Impl.Types as T
import Prelude
  ( Bool (False, True),
    Either (Left, Right),
    IO,
    Int,
    Maybe (Just, Nothing),
    Monad (return),
    Show,
    String,
    error,
    not,
    otherwise,
    print,
    show,
    ($),
    (&&),
    (*),
    (+),
    (++),
    (<),
    (||),
  )

runEval :: T.Env -> T.Eval resT -> IO (Either T.RuntimeException resT)
runEval env ev = CME.runExceptT $ CMR.runReaderT ev env

getLnCol :: LA.HasPosition a => a -> T.LnCol
getLnCol pos = fromMaybe (0, 0) $ LA.hasPosition pos

funId :: LA.TopDef -> LA.Ident
funId topDef = case topDef of
  LA.FnDef _ _ id _ _ -> id

int :: T.Store -> Int
int st = case st of
  T.IntVal x -> x
  _ -> error "Expected Int"

bool :: T.Store -> Bool
bool st = case st of
  T.BoolVal x -> x
  _ -> error "Expected Bool"

str :: T.Store -> String
str st = case st of
  T.StrVal x -> x
  _ -> error "Expected Str"

class IsType a where
  isInt :: a -> Bool
  isBool :: a -> Bool
  isStr :: a -> Bool
  isEmpty :: a -> Bool

instance IsType (LA.Type' a) where
  isInt (LA.Int _) = True
  isInt _ = False
  isBool (LA.Bool _) = True
  isBool _ = False
  isStr (LA.Str _) = True
  isStr _ = False
  isEmpty _ = False

instance IsType T.Store where
  isInt (T.IntVal _) = True
  isInt (T.None t) = isInt t
  isInt T.Ret = True
  isInt _ = False
  isBool (T.BoolVal _) = True
  isBool (T.None t) = isBool t
  isBool T.Ret = True
  isBool _ = False
  isStr (T.StrVal _) = True
  isStr (T.None t) = isStr t
  isStr T.Ret = True
  isStr _ = False
  isEmpty (T.None _) = True
  isEmpty T.Ret = True
  isEmpty _ = False

isTypeMatched :: IsType a => IsType b => a -> b -> Bool
isTypeMatched t st = isInt st && isInt t || isBool st && isBool t || isStr st && isStr t

new :: LA.Ident -> Int -> T.Store -> LA.Type -> T.Env -> T.State -> Either T.RuntimeException (T.Env, T.State)
new id sign st t env s
  | not $ isTypeMatched t st = Left $ T.TypeException $ getLnCol t
  | otherwise = Right (newEnv, newS)
  where
    newLoc = sign * T.last s
    newS =
      s
        { T.mem = insert newLoc st $ T.mem s,
          T.last = T.last s + 1
        }
    newEnv =
      env
        { T.vars = insert id newLoc $ T.vars env
        }

ass :: LA.Ident -> T.Store -> T.LnCol -> T.Env -> T.State -> T.Eval T.State
ass id st lnCol env s
  | not $ member id $ T.vars env = CME.throwError $ T.NameException lnCol
  | T.vars env ! id < 0 = CME.throwError $ T.ConstantAssignmentException lnCol
  | not $ isTypeMatched (T.mem s ! (T.vars env ! id)) st = CME.throwError $ T.TypeException lnCol
  | otherwise = return $ s {T.mem = insert (T.vars env ! id) st $ T.mem s}

get :: LA.Ident -> T.Env -> T.State -> Maybe T.Store
get id env s =
  if not $ member id $ T.vars env
    then Nothing
    else Just $ T.mem s ! (T.vars env ! id)

myPrint :: Show a => a -> T.State -> T.Eval T.State
myPrint x s = do
  liftIO $ print x
  CMR.return $ s {T.mem = insert 0 (T.IntVal 0) $ T.mem s}
