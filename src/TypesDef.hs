module TypesDef where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Maybe
import qualified Data.Map as Map
import Data.IORef

import AbsCH

type Env = Map.Map Ident (IORef Value)
type MyMonad a = ReaderT Env (ExceptT String (ContT () IO)) a

data FunArg = Val Value | RefVal (IORef Value)
data Value = IntVal Integer
           | BoolVal Bool
           | StrVal String
           | VoidVal
           | FunVal [Bool] ([FunArg] -> MyMonad Value)  -- List of Bool values is used to determin positions of references
           | RetFun (Value -> MyMonad ())
           | Undefined

type Pos = Maybe (Int, Int)
type TypeEnv = Map.Map Ident (Type Pos)
type TypeMonad a = ReaderT TypeEnv (ExceptT String IO) a