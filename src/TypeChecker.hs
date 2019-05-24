module TypeChecker where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Maybe
import qualified Data.Map as Map

import AbsCH


type TypeEnv = Map.Map Ident Type
type TypeMonad a = ReaderT TypeEnv (ExceptT String IO) a

checkExpr :: Expr -> TypeMonad Type
checkExpr (ELitInt i) = return Int

checkExpr (EVar ident) = do
  Just t <- asks (Map.lookup ident)
  return t

checkExpr ELitTrue = return Bool

checkExpr ELitFalse = return Bool

checkExpr (EApp ident args) = do
  Just t <- asks (Map.lookup ident)
  case t of
    Fun tRet [tArgs] -> return tRet
    _ -> throwError "[TYPE ERROR]: not applicable"

checkExpr (EString s) = return Str

checkExpr (Neg e) = do
  t <- checkExpr e
  case t of
    Bool -> return Bool
    _ -> throwError "[TYPE ERROR]: not a boolean expression"

checkExpr (EMul e1 op e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Int, Int) -> return Int
    _ -> throwError "[TYPE ERROR]: not a integer value"

checkExpr (EAdd e1 op e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Int, Int) -> return IntVal
    _ -> throwError "[TYPE ERROR]: not a integer value"

checkExpr (ERel e1 op e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Int, Int) -> return Bool
    (Bool, Bool) ->
      case op of
        EQU -> return Bool
        NE -> return Bool
        _ -> throwError "[TYPE ERROR]: not allowed relation operation on boolean expressions"
     (Str, Str) ->
      case op of
        EQU -> return Bool
        NE -> return Bool
        _ -> throwError "[TYPE ERROR]: not allowed relation operation on strings"
    _ -> throwError "[TYPE ERROR]: expressions not comparable"

checkExpr (EAnd e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Bool, Bool) -> return Bool
    _ -> throwError "[TYPE ERROR]: not a boolean expressions"

checkExpr (EOr e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (Bool, Bool) -> return Bool
    _ -> throwError "[TYPE ERROR]: not a boolean expressions"

declare :: Type -> Item -> TypeMonad () -> TypeMonad ()
declare t (Init ident e) m = do
  t' <- checkExpr e
  case t' of
    t -> local (Map.insert ident t) m
    _ -> throwError "[TYPE ERROR]: wrong expression type"

checkStmts :: [Stmt] -> TypeMonad ()
checkStmts [] = pure ()
checkStmts ((FnDef t ident args (Block body)) : rest) = do
  defFun <- liftIO $ newIORef Undefined
  local (Map.insert ident defFun) $ do
    env <- ask
    liftIO $ writeIORef defFun $ createFun ident env args body
    checkStmts rest
checkStmts ((Decl t decls) : rest) = foldr (declare t) (checkStmts rest) decls
checkStmts (stmt : stmts) = checkStmt stmt >> checkStmts stmts
