module Evaluator where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

import LexCH
import ParCH
import AbsCH

import ErrM

type MyMonad a = ReaderT Env (ExceptT String (WriterT [String] IO)) a

type Name = String
data Value = IntVal Integer | BoolVal Bool | StrVal String | FunVal ([Value] -> MyMonad Value)
type Env = Map.Map Name Value

instance Show Value where
  show (IntVal x) = "IntVal " ++ show x
  show (BoolVal x) = "BoolVal " ++ show x
  show (StrVal x) = "StrVal " ++ show x
  show (FunVal x) = "FunVal <some function>"

evalExpr :: Expr -> MyMonad Value
evalExpr (ELitInt i) = do
  liftIO $ print i
  return $ IntVal i

evalExpr (EVar (Ident n)) = do
  tell [n]
  var <- asks (Map.lookup n)
  case var of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val

evalExpr ELitTrue = return (BoolVal True)

evalExpr ELitFalse = return (BoolVal False)

evalExpr (EApp (Ident name) args) = do
  Just (FunVal fun) <- asks (Map.lookup name)
  argVals <- mapM evalExpr args
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
        Div -> return $ IntVal (i1 `quot` i2)
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
