module Evaluator where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Maybe
import qualified Data.Map as Map
import Data.IORef

import LexCH
import ParCH
import AbsCH

import ErrM

type Env = Map.Map Ident (IORef Value)
type MyMonad a = ReaderT Env (ExceptT String (ContT Value IO)) a

data FunArg = Val Value | RefVal (IORef Value)
data Value = IntVal Integer
           | BoolVal Bool
           | StrVal String
           | VoidVal
           | FunVal [Bool] ([FunArg] -> MyMonad Value)  -- List of Bool values is used to determin positions of references
           | RetFun (Value -> MyMonad ())
           | Undefined

instance Show Value where
  show (IntVal x) = show x
  show (BoolVal x) = show x
  show (StrVal x) = show x
  show VoidVal = "<void value>"
  show (FunVal _ _) = "<some function>"
  show (RetFun _) = "<return function>"

evalArg :: (Bool, Expr) -> MyMonad FunArg
evalArg (True, (EVar ident)) = do
  Just ref <- asks $ Map.lookup ident
  return $ RefVal ref
evalArg (_, expr) = do
  val <- evalExpr expr
  return $ Val val

evalExpr :: Expr -> MyMonad Value
evalExpr (ELitInt i) = do
  return $ IntVal i

evalExpr (EVar ident) = do
  Just val <- asks (Map.lookup ident)
  liftIO (readIORef val)

evalExpr ELitTrue = return (BoolVal True)

evalExpr ELitFalse = return (BoolVal False)

evalExpr (EApp ident args) = do
  Just ioref <- asks (Map.lookup ident)
  FunVal refPos fun <- (liftIO . readIORef) ioref
  argVals <- mapM evalArg (zip refPos args)
  fun argVals

evalExpr (EString s) = return (StrVal s)

evalExpr (Neg e) = do
  val <- evalExpr e
  case val of
    BoolVal b -> return $ BoolVal (not b)
    _ -> throwError "not a boolean expression"

evalExpr (EMul e1 op e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> 
      case op of
        Times -> return $ IntVal (i1 * i2)
        Div -> case i2 of
          0 -> throwError "division by zero error"
          _ -> return $ IntVal (i1 `quot` i2)
        Mod -> return $ IntVal (i1 `mod` i2)
    _ -> throwError "type error in multiplication"

evalExpr (EAdd e1 op e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> 
      case op of
        Plus -> return $ IntVal (i1 + i2)
        Minus -> return $ IntVal (i1 - i2)
    _ -> throwError "type error in addition"

evalExpr (ERel e1 op e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> 
      case op of
        LTH -> return $ BoolVal (i1 < i2)
        LE -> return $ BoolVal (i1 <= i2)
        GTH -> return $ BoolVal (i1 > i2)
        GE -> return $ BoolVal (i1 >= i2)
        EQU -> return $ BoolVal (i1 == i2)
        NE -> return $ BoolVal (i1 /= i2)
    _ -> throwError "type error in relation operation"

evalExpr (EAnd e1 e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 && b2)
    _ -> throwError "type error in AND operator"

evalExpr (EOr e1 e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 || b2)
    _ -> throwError "type error in AND operator"
