module Interpreter where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

import LexCH
import ParCH
import AbsCH

import ErrM

type MyMonad a = ReaderT Env (ErrorT String (WriterT [String ] (StateT Integer IO))) a

type Name = String
data Value = IntVal Integer | FunVal Env Name Expr deriving (Show)
type Env = Map.Map Name Value

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

evalExpr :: Expr -> MyMonad Value
evalExpr (ELitInt i) = do
  tick
  liftIO $ print i
  return $ IntVal i

evalExpr (EVar (Ident n)) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val

evalExpr (EAdd e1 Plus e2) = do
  tick
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type error in addition"
{-
eval6 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e

eval6 (App e1 e2 ) = do
  tick
  val1 <- eval6 e1
  val2 <- eval6 e2
  case val1 of
    FunVal env0 n body -> local (const (Map.insert n val2 env0)) (eval6 body)
    _ -> throwError "type error in application"
-}

interpret :: Stmt -> MyMonad Value
interpret (SExp e) = evalExpr e

interpret (Decl ty items) = do  -- Maybe error when variable was already declared
  local (const (Map.insert n val2 env0)) (eval6 body)

interpret ()
{-
interpretProg :: Program -> MyMonad Value
interpretProg (Program xs) = 
-}
runInterpret :: Env -> Integer -> MyMonad a -> IO ((Either String a, [String]), Integer)
runInterpret env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st

exampleStmt = SExp (EAdd (ELitInt 7) Plus (ELitInt 5))
exampleProg = Program [Decl Int [Init (Ident "x") (ELitInt 5)],Decl Int [Init (Ident "y") (ELitInt 7)],Decl Int [Init (Ident "z") (EAdd (EVar (Ident "x")) Plus (EVar (Ident "y")))]]
exampleExpr1 = ELitInt 5
exampleExpr2 = EAdd (ELitInt 7) Plus (ELitInt 5)

--runInterp Map.empty 0 (interp exampleExp)
{-
main = do
  interact calc
  putStrLn ""

calc s =
  let Ok e = pExp (myLexer s) in show (interpret e)
-}