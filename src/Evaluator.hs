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

import TypesDef


instance Show Value where
  show (IntVal x) = show x
  show (BoolVal x) = show x
  show (StrVal x) = show $ tail $ init x
  show VoidVal = "<void value>"
  show (FunVal _ _) = "<some function>"
  show (RetFun _) = "<return function>"

evalArg :: (Bool, Expr Pos) -> MyMonad FunArg
evalArg (True, (EVar pos ident)) = do
  Just ref <- asks $ Map.lookup ident
  return $ RefVal ref
evalArg (_, expr) = do
  val <- evalExpr expr
  return $ Val val

evalExpr :: Expr Pos -> MyMonad Value
evalExpr (ELitInt pos i) = return $ IntVal i

evalExpr (EVar _ ident) = do
  Just val <- asks (Map.lookup ident)
  liftIO (readIORef val)

evalExpr (ELitTrue _) = return (BoolVal True)

evalExpr (ELitFalse _) = return (BoolVal False)

evalExpr (EApp _ ident args) = do
  Just ioref <- asks (Map.lookup ident)
  FunVal refPos fun <- (liftIO . readIORef) ioref
  argVals <- mapM evalArg (zip refPos args)
  fun argVals

evalExpr (EString _ s) = return (StrVal s)

evalExpr (Neg _ e) = do
  BoolVal val <- evalExpr e
  return $ BoolVal (not val)

evalExpr (EMul _ e1 op e2) = do
  IntVal i1 <- evalExpr e1
  IntVal i2 <- evalExpr e2
  case op of
    Times _ -> return $ IntVal (i1 * i2)
    Div pos -> case i2 of
      0 -> throwError ("[RUNTIME ERROR]: " ++ show (fromJust pos) ++ " division by zero")
      _ -> return $ IntVal (i1 `quot` i2)
    Mod _ -> return $ IntVal (i1 `mod` i2)

evalExpr (EAdd _ e1 op e2) = do
  IntVal i1 <- evalExpr e1
  IntVal i2 <- evalExpr e2
  case op of
    Plus _ -> return $ IntVal (i1 + i2)
    Minus _ -> return $ IntVal (i1 - i2)

evalExpr (ERel _ e1 op e2) = do
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> 
      case op of
        LTH _ -> return $ BoolVal (i1 < i2)
        LE _ -> return $ BoolVal (i1 <= i2)
        GTH _ -> return $ BoolVal (i1 > i2)
        GE _ -> return $ BoolVal (i1 >= i2)
        EQU _ -> return $ BoolVal (i1 == i2)
        NE _ -> return $ BoolVal (i1 /= i2)
    (BoolVal i1, BoolVal i2) -> 
      case op of
        EQU _ -> return $ BoolVal (i1 == i2)
        NE _ -> return $ BoolVal (i1 /= i2)
    (StrVal i1, StrVal i2) -> 
      case op of
        EQU _ -> return $ BoolVal (i1 == i2)
        NE _ -> return $ BoolVal (i1 /= i2)

evalExpr (EAnd _ e1 e2) = do
  BoolVal b1 <- evalExpr e1
  BoolVal b2 <- evalExpr e2
  return $ BoolVal (b1 && b2)

evalExpr (EOr _ e1 e2) = do
  BoolVal b1 <- evalExpr e1
  BoolVal b2 <- evalExpr e2
  return $ BoolVal (b1 || b2)
