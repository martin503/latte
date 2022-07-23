{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Latte.Interpreter
  ( runInterpret,
  )
where

import qualified Control.Monad.Except as CME
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Reader as CMR
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Map (insert, keys, lookup, member, singleton, (!))
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Latte.Abs as LA
import qualified Latte.Impl.Helpers as H
import qualified Latte.Impl.Types as T
import Prelude
  ( Bool (False, True),
    Either (..),
    IO,
    Integer,
    Maybe (Just, Nothing),
    Monad (return),
    Show,
    String,
    const,
    div,
    fromIntegral,
    head,
    id,
    length,
    mod,
    not,
    print,
    show,
    undefined,
    (!!),
    ($),
    (&&),
    (*),
    (+),
    (-),
    (/=),
    (<),
    (<=),
    (==),
    (>),
    (>=),
    (>>),
    (||),
  )

runInterpret :: LA.Program -> IO (Either T.RuntimeException T.Res)
runInterpret prog = H.runEval makeEnv $ interpretProgram prog makeState

printFun :: T.ContF
printFun args env s =
  if length args /= 1
    then CME.throwError $ T.ArgumentException $ H.getLnCol (args !! 1)
    else
      interpretExpr
        (head args)
        ( \q s' -> case q of
            T.IntVal x -> H.myPrint x s'
            T.BoolVal x -> H.myPrint x s'
            T.StrVal x -> H.myPrint x s'
            _ -> CME.throwError $ T.NameException $ H.getLnCol (head args)
        )
        s

makeEnv :: T.Env
makeEnv =
  T.MakeEnv
    { T.vars = singleton (LA.Ident "retVal") 0,
      T.funs = singleton (LA.Ident "print") printFun,
      T.retType = LA.Int Nothing -- just for init
    }

makeState :: T.State
makeState =
  T.MakeState
    { T.mem = singleton 0 T.Ret,
      T.last = 1 -- on 0 are results of funcs
    }

-- INTERPRETATION

interpretProgram :: LA.Program -> T.Cont
interpretProgram x s = case x of
  LA.Program _ topDefs -> eval topDefs s
  where
    eval topDefs s =
      case topDefs of
        hd : tl -> do
          env <- CMR.ask
          let newEnv = env {T.funs = insert (H.funId hd) (interpretTopDef hd newEnv) $ T.funs env}
          let newS = s {T.last = T.last s + 1}
          CMR.local (const newEnv) (eval tl newS)
        [] -> do
          env <- CMR.ask
          let main = lookup (LA.Ident "main") $ T.funs env
          case main of
            Nothing -> CME.throwError T.NoMain
            Just contF -> do
              s' <- contF [] env s
              if H.isEmpty $ T.mem s' ! 0 then CME.throwError $ T.NoReturn $ H.getLnCol x
              else return s'


interpretTopDef :: LA.TopDef -> T.Env -> T.ContF
interpretTopDef x env exprs exprEnv s = do
  case x of
    LA.FnDef _ t id args block -> CMR.local (const $ env {T.retType = t}) $ eval args exprs exprEnv s
      where
        eval :: [LA.Arg] -> T.ContF
        eval args exprs exprEnv s = case (args, exprs) of
          (ha : ta, he : te) -> interpretArg ha he exprEnv (eval ta te exprEnv) s
          ([], []) -> interpretBlock block return s
          _ -> CME.throwError $ T.ArgumentException $ H.getLnCol x

interpretArg :: LA.Arg -> LA.Expr -> T.Env -> T.Cont -> T.Cont
interpretArg x exp expEnv cont s = do
  env <- CMR.ask
  case x of
    LA.ValArg _ t id ->
      CMR.local
        (const expEnv)
        ( interpretExpr
            exp
            ( \q s' -> do
                let new = H.new id 1 q t env s
                case new of
                  Left err -> CME.throwError err
                  Right (env', s') -> CMR.local (const env') $ cont s'
            )
            s
        )
    LA.VarArg _ t id -> case exp of
      LA.EVar _ idArg -> do
        let st = H.get idArg expEnv s
        if isNothing st
          then CME.throwError $ T.NameException $ H.getLnCol exp
          else
            if not $ H.isTypeMatched t $ fromJust st
              then CME.throwError $ T.TypeException $ H.getLnCol exp
              else CMR.local (const $ env {T.vars = insert id (T.vars expEnv ! idArg) $ T.vars env}) $ cont s
      _ -> CME.throwError $ T.ArgumentException $ H.getLnCol x

interpretDecl :: LA.Decl -> T.Cont -> T.Cont
interpretDecl x cont s =
  case x of
    LA.Mutable _ t items -> eval t 1 items s
    LA.Immutable _ t items -> eval t (-1) items s
  where
    eval t sign items s = case items of
      hd : tl -> case hd of
        LA.NoInit _ id -> contE id hd tl (T.None t) s
        LA.Init _ id expr -> interpretExpr expr (contE id hd tl) s
      [] -> cont s
      where
        contE :: LA.Ident -> LA.Item -> [LA.Item] -> T.ContE
        contE id hd tl = \q s -> do
          env <- CMR.ask
          if member id $ T.vars env
            then CME.throwError $ T.MulitpleDeclarationException (fromJust $ LA.hasPosition hd)
            else do
              let new = H.new id sign q t env s
              case new of
                Right (env', s') -> CMR.local (const env') $ eval t sign tl s'
                Left exc -> CME.throwError exc

interpretBlock :: LA.Block -> T.Cont -> T.Cont
interpretBlock x cont s = case x of
  LA.Block _ stmts -> eval stmts s
  where
    eval stmts s' = case stmts of
      hd : tl -> interpretStmt hd (eval tl) s'
      [] -> cont s'

interpretStmt :: LA.Stmt -> T.Cont -> T.Cont
interpretStmt x cont = case x of
  LA.Empty _ -> cont
  LA.DStmt _ decl -> interpretDecl decl cont
  LA.Ret _ expr ->
    interpretExpr
      expr
      ( \q s -> do
          env <- CMR.ask
          if not $ H.isTypeMatched q $ T.retType env
            then CME.throwError $ T.TypeException $ H.getLnCol expr
            else return $ s {T.mem = insert 0 q $ T.mem s}
      )
  LA.SExp _ expr -> interpretExpr expr (\q s -> cont s)
  LA.Cond _ expr block ->
    interpretExpr
      expr
      ( \q s ->
          if not $ H.isBool q
            then CME.throwError $ T.TypeException $ H.getLnCol expr
            else
              if H.bool q
                then interpretBlock block cont s
                else cont s
      )
  LA.CondElse _ expr block1 block2 ->
    interpretExpr
      expr
      ( \q s ->
          if not $ H.isBool q
            then CME.throwError $ T.TypeException $ H.getLnCol expr
            else
              if H.bool q
                then interpretBlock block1 cont s
                else interpretBlock block2 cont s
      )
  LA.Incr _ id -> modify id (1 +)
  LA.Decr _ id -> modify id (-1 +)
  LA.Ass _ id expr ->
    interpretExpr
      expr
      ( \q s -> do
          env <- CMR.ask
          newS <- H.ass id q (H.getLnCol expr) env s
          cont newS
      )
  LA.While _ expr block ->
    interpretExpr
      expr
      ( \q s ->
          if not $ H.isBool q
            then CME.throwError $ T.TypeException $ H.getLnCol expr
            else
              if not $ H.bool q
                then cont s
                else interpretBlock block (interpretStmt x cont) s
      )
  where
    modify id op =
      \s -> do
        env <- CMR.ask
        let q = H.get id env s
        case q of
          Nothing -> CME.throwError $ T.NameException $ H.getLnCol x
          Just val -> do
            newS <- H.ass id (T.IntVal $ op $ H.int val) (H.getLnCol x) env s
            cont newS

interpretExpr :: LA.Expr -> T.ContE -> T.Cont
interpretExpr expr contE s = do
  env <- CMR.ask
  case expr of
    LA.EVar _ id -> do
      let var = H.get id env s
      if isNothing var
        then CME.throwError $ T.NameException $ H.getLnCol expr
        else contE (fromJust var) s
    LA.ELitInt _ int -> contE (T.IntVal $ fromIntegral int) s
    LA.ELitTrue _ -> contE (T.BoolVal True) s
    LA.ELitFalse _ -> contE (T.BoolVal False) s
    LA.EString _ str -> contE (T.StrVal str) s
    LA.Neg _ expr -> do
      let exprLnCol = H.getLnCol expr
      interpretExpr
        expr
        ( \q s' ->
            if not (H.isInt q)
              then CME.throwError $ T.TypeException exprLnCol
              else do
                let qInt = H.int q
                contE (T.IntVal $ - qInt) s'
        )
        s
    LA.Not _ expr -> do
      let exprLnCol = H.getLnCol expr
      interpretExpr
        expr
        ( \b s' ->
            if not (H.isBool b)
              then CME.throwError $ T.TypeException exprLnCol
              else do
                let bBool = H.bool b
                contE (T.BoolVal $ not bBool) s'
        )
        s
    LA.ERel _ expr1 relop expr2 -> interpretRelOp expr1 relop expr2 contE s
    LA.EApp _ id exprs ->
      if isNothing $ lookup id $ T.funs env
        then CME.throwError $ T.NameException $ H.getLnCol expr
        else do
          s' <- (T.funs env ! id) exprs env s
          if H.isEmpty $ T.mem s' ! 0 then CME.throwError $ T.NoReturn $ H.getLnCol expr
          else do
            let fRet = T.mem s' ! 0
            contE fRet $ s' {T.mem = insert 0 T.Ret $ T.mem s'}
    LA.EMul _ expr1 mulop expr2 -> interpretMulOp expr1 mulop expr2 contE s
    LA.EAdd _ expr1 addop expr2 -> interpretAddOp expr1 addop expr2 contE s
    LA.EAnd _ expr1 expr2 -> interpretLogicalOp expr1 (&&) expr2 contE s
    LA.EOr _ expr1 expr2 -> interpretLogicalOp expr1 (||) expr2 contE s

interpretLogicalOp :: LA.Expr -> (Bool -> Bool -> Bool) -> LA.Expr -> T.ContE -> T.Cont
interpretLogicalOp expr1 op expr2 contB = do
  let expr1LnCol = H.getLnCol expr1
  let expr2LnCol = H.getLnCol expr2
  interpretExpr
    expr1
    ( \q1 s1 ->
        interpretExpr
          expr2
          ( \q2 s2 ->
              if not (H.isBool q1)
                then CME.throwError $ T.TypeException expr1LnCol
                else
                  if not (H.isBool q2)
                    then CME.throwError $ T.TypeException expr2LnCol
                    else do
                      let q1Bool = H.bool q1
                      let q2Bool = H.bool q2
                      contB (T.BoolVal (op q1Bool q2Bool)) s2
          )
          s1
    )

interpretAddOp :: LA.Expr -> LA.AddOp -> LA.Expr -> T.ContE -> T.Cont
interpretAddOp expr1 op expr2 contE = do
  let expr1LnCol = H.getLnCol expr1
  let expr2LnCol = H.getLnCol expr2
  interpretExpr
    expr1
    ( \q1 s1 ->
        interpretExpr
          expr2
          ( \q2 s2 -> do
              if not (H.isInt q1)
                then CME.throwError $ T.TypeException expr1LnCol
                else
                  if not (H.isInt q2)
                    then CME.throwError $ T.TypeException expr2LnCol
                    else do
                      let q1Int = H.int q1
                      let q2Int = H.int q2
                      case op of
                        LA.Plus _ -> contE (T.IntVal (q1Int + q2Int)) s2
                        LA.Minus _ -> contE (T.IntVal (q1Int - q2Int)) s2
          )
          s1
    )

interpretMulOp :: LA.Expr -> LA.MulOp -> LA.Expr -> T.ContE -> T.Cont
interpretMulOp expr1 op expr2 contE = do
  let expr1LnCol = H.getLnCol expr1
  let expr2LnCol = H.getLnCol expr2
  interpretExpr
    expr1
    ( \q1 s1 ->
        interpretExpr
          expr2
          ( \q2 s2 ->
              if not (H.isInt q1)
                then CME.throwError $ T.TypeException expr1LnCol
                else
                  if not (H.isInt q2)
                    then CME.throwError $ T.TypeException expr2LnCol
                    else do
                      let q1Int = H.int q1
                      let q2Int = H.int q2
                      case op of
                        LA.Times _ -> contE (T.IntVal (q1Int * q2Int)) s2
                        LA.Div _ ->
                          if q2Int == 0
                            then CME.throwError $ T.ArithmeticException $ H.getLnCol expr2
                            else contE (T.IntVal (q1Int `div` q2Int)) s2
                        LA.Mod _ ->
                          if q2Int == 0
                            then CME.throwError $ T.ArithmeticException $ H.getLnCol expr2
                            else contE (T.IntVal (q1Int `mod` q2Int)) s2
          )
          s1
    )

interpretRelOp :: LA.Expr -> LA.RelOp -> LA.Expr -> T.ContE -> T.Cont
interpretRelOp expr1 op expr2 contB = do
  let expr1LnCol = H.getLnCol expr1
  let expr2LnCol = H.getLnCol expr2
  interpretExpr
    expr1
    ( \q1 s1 ->
        interpretExpr
          expr2
          ( \q2 s2 ->
              if not (H.isTypeMatched q1 q2)
                then CME.throwError $ T.TypeException expr2LnCol
                else do
                  case op of
                    LA.LTH _ -> contB (T.BoolVal (q1 < q2)) s2
                    LA.LE _ -> contB (T.BoolVal (q1 <= q2)) s2
                    LA.GTH _ -> contB (T.BoolVal (q1 > q2)) s2
                    LA.GE _ -> contB (T.BoolVal (q1 >= q2)) s2
                    LA.EQU _ -> contB (T.BoolVal (q1 == q2)) s2
                    LA.NE _ -> contB (T.BoolVal (q1 /= q2)) s2
          )
          s1
    )
