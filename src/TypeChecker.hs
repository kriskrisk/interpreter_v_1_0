module TypeChecker where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Maybe
import qualified Data.Map as Map

import LexCH
import ParCH
import AbsCH

import ErrM


type TypeEnv = Map.Map Ident Type
type TypeMonad a = ReaderT TypeEnv (ExceptT String Type) a

data Type = IntVal
           | BoolVal
           | StrVal
           | VoidVal
           | FunVal [Bool]  -- List of Bool values is used to determin positions of references

checkExpr :: Expr -> TypeMonad Type
checkExpr (ELitInt i) = return IntVal

checkExpr (EVar ident) = do
  Just t <- asks (Map.lookup ident)
  return t
