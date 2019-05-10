module Interpreter where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Maybe
import qualified Data.Map as Map
import Data.IORef

import System.IO
import System.Environment

import LexCH
import ParCH
import AbsCH

import ErrM

import Evaluator


declare :: Item -> MyMonad () -> MyMonad ()
declare (Init ident e) m = do
  e' <- evalExpr e
  ioref <- liftIO (newIORef e')
  local (Map.insert ident ioref) m

incIntVal :: Value -> Value
incIntVal (IntVal i) = IntVal (i + 1)

applyToInt :: (Integer -> Integer) -> Value -> Value
applyToInt fun (IntVal i) = IntVal (fun i)

insertPair :: Env -> (Arg, (IORef Value)) -> Env
insertPair m (Arg _ ident, val) = Map.insert ident val m
insertPair m (RefArg _ ident, val) = Map.insert ident val m

isRef :: Arg -> Bool
isRef (Arg _ _) = False
isRef (RefArg _ _) = True

addIORef :: FunArg -> MyMonad (IORef Value)
addIORef (Val val) = liftIO $ newIORef val
addIORef (RefVal ioref) = return ioref

createFun :: Ident -> Env -> [Arg] -> [Stmt] -> Value
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

loop :: Expr -> Stmt -> MyMonad ()
loop expr stmt = do
  BoolVal val <- evalExpr expr
  if val
    then do 
      interpretStmt stmt
      loop expr stmt
  else return ()

interpretStmts :: [Stmt] -> MyMonad ()
interpretStmts [] = pure ()
interpretStmts ((FnDef _ ident args (Block body)) : rest) = do
  defFun <- liftIO $ newIORef Undefined
  local (Map.insert ident defFun) $ do
    env <- ask
    liftIO $ writeIORef defFun $ createFun ident env args body
    interpretStmts rest
interpretStmts ((Decl _ decls) : rest) = foldr declare (interpretStmts rest) decls
interpretStmts (stmt : stmts) = interpretStmt stmt >> interpretStmts stmts

interpretStmt :: Stmt -> MyMonad ()
interpretStmt Empty = return ()

interpretStmt (BStmt (Block stmts)) = interpretStmts stmts

interpretStmt (Ass ident expr) = do
  Just ioref <- asks (Map.lookup ident)
  val <- evalExpr expr
  liftIO . modifyIORef' ioref $ \_ -> val

interpretStmt (Incr ident) = do
  Just ioref <- asks (Map.lookup ident)
  liftIO . modifyIORef' ioref $ \(IntVal i) -> IntVal (i + 1)

interpretStmt (Decr ident) = do
  Just ioref <- asks (Map.lookup ident)
  liftIO . modifyIORef' ioref $ \(IntVal i) -> IntVal (i - 1)

interpretStmt (Ret expr) = do
  Just ioref <- asks (Map.lookup (Ident "$ret$"))
  RetFun fun <- liftIO $ readIORef ioref
  val <- evalExpr expr
  fun val

interpretStmt VRet = do
  Just ioref <- asks (Map.lookup (Ident "$ret$"))
  RetFun fun <- liftIO $ readIORef ioref
  fun VoidVal

interpretStmt (Cond expr stmt) = do
  cond <- evalExpr expr
  case cond of
    BoolVal True -> interpretStmt stmt
    BoolVal False -> return ()

interpretStmt (CondElse expr stmtTrue stmtFalse) = do
  cond <- evalExpr expr
  case cond of
    BoolVal True -> interpretStmt stmtTrue
    BoolVal False -> interpretStmt stmtFalse

interpretStmt (While expr stmt) = loop expr stmt

interpretStmt (Print expr) = do
  val <- evalExpr expr
  liftIO $ print $ val

interpretStmt (SExp expr) = do
  val <- evalExpr expr
  return ()


printResult :: Either String () -> IO Value
printResult res = do
  case res of
    Left errMsg -> do
      print errMsg
      return (IntVal 1)
    Right _ -> return (IntVal 0)

interpretProg :: Program -> IO Value
interpretProg (Program stmts) = runContT (runExceptT (runReaderT (interpretStmts stmts) Map.empty)) printResult


main = do
  args <- getArgs   -- TODO: Add case of missing file to interpret
  fd <- openFile (head args) ReadMode
  program <- hGetContents fd
  let Ok prog = pProgram (myLexer program) in interpretProg prog
