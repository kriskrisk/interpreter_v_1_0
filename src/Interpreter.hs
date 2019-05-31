module Interpreter where

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

import Evaluator
import TypesDef


declare :: Item Pos -> MyMonad () -> MyMonad ()
declare (Init _ ident e) m = do
  e' <- evalExpr e
  ioref <- liftIO (newIORef e')
  local (Map.insert ident ioref) m
declare (NoInit _ ident) m = do
  ioref <- liftIO (newIORef Undefined)
  local (Map.insert ident ioref) m

incIntVal :: Value -> Value
incIntVal (IntVal i) = IntVal (i + 1)

applyToInt :: (Integer -> Integer) -> Value -> Value
applyToInt fun (IntVal i) = IntVal (fun i)

insertPair :: Env -> (Arg Pos, (IORef Value)) -> Env
insertPair m (Arg _ _ ident, val) = Map.insert ident val m
insertPair m (RefArg _ _ ident, val) = Map.insert ident val m

isRef :: Arg Pos -> Bool
isRef (Arg _ _ _) = False
isRef (RefArg _ _ _) = True

addIORef :: FunArg -> MyMonad (IORef Value)
addIORef (Val val) = liftIO $ newIORef val
addIORef (RefVal ioref) = return ioref

createFun :: Ident -> Env -> [Arg Pos] -> [Stmt Pos] -> Value
createFun ident env args body = fun
  where
    fun = FunVal isRefList $ \argVals -> do
      argVars <- mapM addIORef argVals
      local (const $ foldl insertPair env $ zip args argVars) $ callCC $ \ret -> do
        retFun <- liftIO $ newIORef (RetFun ret)
        defFun <- liftIO $ newIORef fun
        local (Map.insert (Ident "$ret$") retFun) $ do
          interpretStmts body
          return VoidVal
    isRefList = map isRef args

loop :: Expr Pos -> Stmt Pos -> MyMonad ()
loop expr stmt = do
  BoolVal val <- evalExpr expr
  if val
    then do 
      interpretStmt stmt
      loop expr stmt
  else return ()

interpretStmts :: [Stmt Pos] -> MyMonad ()
interpretStmts [] = pure ()
interpretStmts ((FnDef _ _ ident args (Block _ body)) : rest) = do
  defFun <- liftIO $ newIORef Undefined
  local (Map.insert ident defFun) $ do
    env <- ask
    liftIO $ writeIORef defFun $ createFun ident env args body
    interpretStmts rest
interpretStmts ((Decl _ _ decls) : rest) = foldr declare (interpretStmts rest) decls
interpretStmts (stmt : stmts) = interpretStmt stmt >> interpretStmts stmts

interpretStmt :: Stmt Pos -> MyMonad ()
interpretStmt (Empty _) = return ()

interpretStmt (BStmt _ (Block _ stmts)) = interpretStmts stmts

interpretStmt (Ass _ ident expr) = do
  Just ioref <- asks (Map.lookup ident)
  val <- evalExpr expr
  liftIO . modifyIORef' ioref $ \_ -> val

interpretStmt (Incr _ ident) = do
  Just ioref <- asks (Map.lookup ident)
  liftIO . modifyIORef' ioref $ \(IntVal i) -> IntVal (i + 1)

interpretStmt (Decr _ ident) = do
  Just ioref <- asks (Map.lookup ident)
  liftIO . modifyIORef' ioref $ \(IntVal i) -> IntVal (i - 1)

interpretStmt (Ret _ expr) = do
  Just ioref <- asks (Map.lookup (Ident "$ret$"))
  RetFun fun <- liftIO $ readIORef ioref
  val <- evalExpr expr
  fun val

interpretStmt (VRet _) = do
  Just ioref <- asks (Map.lookup (Ident "$ret$"))
  RetFun fun <- liftIO $ readIORef ioref
  fun VoidVal

interpretStmt (Cond _ expr stmt) = do
  cond <- evalExpr expr
  case cond of
    BoolVal True -> interpretStmt stmt
    BoolVal False -> return ()

interpretStmt (CondElse _ expr stmtTrue stmtFalse) = do
  cond <- evalExpr expr
  case cond of
    BoolVal True -> interpretStmt stmtTrue
    BoolVal False -> interpretStmt stmtFalse

interpretStmt (While _ expr stmt) = loop expr stmt

interpretStmt (Print _ expr) = do
  val <- evalExpr expr
  liftIO $ print $ val

interpretStmt (SExp _ expr) = do
  val <- evalExpr expr
  return ()


printResult :: Either String () -> IO ()
printResult res = case res of
  Left errMsg -> do
    print errMsg
  Right _ -> pure ()

interpretProg :: Program Pos -> IO ()
interpretProg (Program _ stmts) = runContT (runExceptT (runReaderT (interpretStmts stmts) Map.empty)) printResult
