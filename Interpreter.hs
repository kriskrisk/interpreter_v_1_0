module Interpreter where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

import LexCH
import ParCH
import AbsCH

import ErrM

import Evaluator

declare :: Item -> MyMonad () -> MyMonad ()
declare (Init (Ident n) e) m = do
  e' <- evalExpr e
  local (Map.insert n e') m

incIntVal :: Value -> Value
incIntVal (IntVal i) = IntVal (i + 1)

insertPair :: Env -> (Arg, Value) -> Env
insertPair m (Arg _ (Ident name), value) = Map.insert name value m

createFun :: Name -> Env -> [Arg] -> [Stmt] -> Value
createFun name env args body = fun
  where
    fun = FunVal $ \argVals -> local (const (Map.insert name fun (foldl insertPair env (zip args argVals)))) $ do
      interpretStmts body
      pure (IntVal 4) --TODO
      {- get return value -}

interpretStmts :: [Stmt] -> MyMonad ()
interpretStmts [] = pure ()
interpretStmts ((FnDef _ (Ident name) args (Block body)) : rest) = do
  env <- ask
  local (Map.insert name (createFun name env args body)) $ interpretStmts rest
interpretStmts ((Decl _ decls) : rest) = foldr declare (interpretStmts rest) decls
interpretStmts (stmt : stmts) = interpretStmt stmt >> interpretStmts stmts

interpretStmt :: Stmt -> MyMonad ()
interpretStmt (BStmt (Block stmts)) = interpretStmts stmts

interpretStmt (Incr (Ident n)) = do
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError ("variable " ++ n ++ " not defined")
    Just val -> case val of
      (IntVal i) -> local (Map.adjust incIntVal n) (return ())
      _ -> throwError "type error in incrementation"

{-
interpretProg :: Program -> MyMonad Value
interpretProg (Program xs) = 
-}

runInterpret :: Env -> MyMonad a -> IO (Either String a, [String])
runInterpret env ev = runWriterT (runExceptT (runReaderT ev env))

exampleProg = Program [Decl Int [Init (Ident "x") (ELitInt 5)],Decl Int [Init (Ident "y") (ELitInt 7)],Decl Int [Init (Ident "z") (EAdd (EVar (Ident "x")) Plus (EVar (Ident "y")))]]
exampleExpr1 = ELitInt 5
exampleExpr2 = EAdd (ELitInt 7) Plus (ELitInt 5)
exampleExpr3 = EAdd (ELitInt 7) Minus (ELitInt 5)
exampleExpr4 = Neg ELitTrue
exampleExpr5 = EMul (ELitInt 7) Div (ELitInt 5)
exampleExpr6 = EMul (ELitInt 7) Times (ELitInt 5)

exampleStmts1 = [Decl Int [Init (Ident "testVar") (ELitInt 7)],Incr (Ident "testVar")]

exampleStmt1 = SExp (EAdd (ELitInt 7) Plus (ELitInt 5))

-- To run: runInterpret Map.empty (interpretStmts exampleStmts1)
