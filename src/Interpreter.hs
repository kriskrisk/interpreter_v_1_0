module Interpreter where

import System.IO
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
evalArg (True, EVar pos ident) = do
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

evalExpr (Anon _ _ args (Block _ body)) = do
  env <- ask
  return $ createFun env args body

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

insertPair :: Env -> (Arg Pos, IORef Value) -> Env
insertPair m (Arg _ _ ident, val) = Map.insert ident val m
insertPair m (RefArg _ _ ident, val) = Map.insert ident val m

isRef :: Arg Pos -> Bool
isRef Arg{} = False
isRef RefArg{} = True

addIORef :: FunArg -> MyMonad (IORef Value)
addIORef (Val val) = liftIO $ newIORef val
addIORef (RefVal ioref) = return ioref

createFun :: Env -> [Arg Pos] -> [Stmt Pos] -> Value
createFun env args body = fun
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
interpretStmts (FnDef _ _ ident args (Block _ body) : rest) = do
  defFun <- liftIO $ newIORef Undefined
  local (Map.insert ident defFun) $ do
    env <- ask
    liftIO $ writeIORef defFun $ createFun env args body
    interpretStmts rest
interpretStmts (Decl _ _ decls : rest) = foldr declare (interpretStmts rest) decls
interpretStmts (stmt : stmts) = interpretStmt stmt >> interpretStmts stmts

interpretStmt :: Stmt Pos -> MyMonad ()
interpretStmt (Empty _) = return ()

interpretStmt (BStmt _ (Block _ stmts)) = interpretStmts stmts

interpretStmt (Ass _ ident expr) = do
  Just ioref <- asks (Map.lookup ident)
  val <- evalExpr expr
  liftIO . modifyIORef' ioref $ const val

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
  liftIO $ print val

interpretStmt (SExp _ expr) = do
  val <- evalExpr expr
  return ()


printResult :: Either String () -> IO ()
printResult res = case res of
  Left errMsg -> hPutStrLn stderr errMsg
  Right _ -> pure ()

interpretProg :: Program Pos -> IO ()
interpretProg (Program _ stmts) = runContT (runExceptT (runReaderT (interpretStmts stmts) Map.empty)) printResult
