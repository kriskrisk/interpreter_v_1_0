module Interpreter where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map
import Data.IORef

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
insertPair m (Arg _ ident, value) = Map.insert ident value m
createFun :: Ident -> Env -> [Arg] -> [Stmt] -> Value
createFun ident env args body = fun
  where
    fun = FunVal $ \argVals -> do
      argVars <- liftIO $ mapM newIORef argVals
      local (const $ foldl insertPair env $ zip args argVars) $ do
        interpretStmts body
        pure (IntVal 4) --TODO
        {- get return value -}

interpretStmts :: [Stmt] -> MyMonad ()
interpretStmts [] = pure ()
interpretStmts ((FnDef _ ident args (Block body)) : rest) = do
  env <- ask
  fun <- liftIO (newIORef (createFun ident env args body))
  local (Map.insert ident fun) $ interpretStmts rest
interpretStmts ((Decl _ decls) : rest) = foldr declare (interpretStmts rest) decls
interpretStmts (stmt : stmts) = interpretStmt stmt >> interpretStmts stmts

interpretStmt :: Stmt -> MyMonad ()
interpretStmt (BStmt (Block stmts)) = interpretStmts stmts

interpretStmt (Incr ident) = do
  Just ioref <- asks (Map.lookup ident)
  liftIO . modifyIORef' ioref $ \(IntVal i) -> IntVal (i + 1)

interpretStmt (Decr ident) = do
  Just ioref <- asks (Map.lookup ident)
  liftIO . modifyIORef' ioref $ \(IntVal i) -> IntVal (i - 1)


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
