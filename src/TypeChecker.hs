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

checkArg :: (Type, Expr) -> TypeMonad ()
checkArg (t, expr) = do
  t' <- checkExpr expr
  case (t' == t) of
    True -> pure ()
    _ -> throwError "[TYPE ERROR]: bad argument type"

checkExpr :: Expr -> TypeMonad Type
checkExpr (ELitInt i) = return Int

checkExpr (EVar ident) = do
  Just t <- asks (Map.lookup ident)
  return t

checkExpr ELitTrue = return Bool

checkExpr ELitFalse = return Bool

checkExpr (EApp ident args) = do
  t <- getIdentType ident
  case t of
    Fun tRet tArgs -> do
      mapM checkArg (zip tArgs args)
      return tRet
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
    (Int, Int) -> return Int
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

declareItem :: Type -> Item -> TypeMonad () -> TypeMonad ()
declareItem t (Init ident e) m = do
  t' <- checkExpr e
  case (t' == t) of
    True -> do
      local (Map.insert ident t) m
    _ -> throwError "[TYPE ERROR]: wrong expression type"
declareItem t (NoInit ident) m = local (Map.insert ident t) m

declareArg :: Arg -> TypeMonad () -> TypeMonad ()
declareArg (Arg t ident) m = do
  local (Map.insert ident t) m
declareArg (RefArg t ident) m = do
  local (Map.insert ident t) m

getArgType :: Arg -> Type
getArgType (Arg t _) = t
getArgType (RefArg t _) = t

checkStmts :: [Stmt] -> TypeMonad ()
checkStmts [] = pure ()
checkStmts ((FnDef retT ident args (Block body)) : rest) = do
  local (Map.insert ident (Fun retT (map getArgType args))) $ do
    local (Map.insert (Ident "$retType$") retT) $ foldr declareArg (checkStmts body) args
    checkStmts rest
checkStmts ((Decl t decls) : rest) = foldr (declareItem t) (checkStmts rest) decls
checkStmts (stmt : stmts) = checkStmt stmt >> checkStmts stmts

getIdentType :: Ident -> TypeMonad Type
getIdentType ident = do
  res <- asks (Map.lookup ident)
  case res of
    (Just t) -> return t
    _ -> throwError "[TYPE ERROR]: referenced identificator is not declared"

checkStmt :: Stmt -> TypeMonad ()
checkStmt Empty = return ()

checkStmt (BStmt (Block stmts)) = checkStmts stmts

checkStmt (Ass ident expr) = do
  t <- getIdentType ident
  t' <- checkExpr expr
  case (t' == t) of
    True -> pure ()
    _ -> throwError "[TYPE ERROR]: wrong expression type in assignment"

checkStmt (Incr ident) = do
  t <- getIdentType ident
  case t of
    Int -> pure ()
    _ -> throwError "[TYPE ERROR]: incrementing non integer value"

checkStmt (Decr ident) = do
  t <- getIdentType ident
  case t of
    Int -> pure ()
    _ -> throwError "[TYPE ERROR]: incrementing non integer value"

checkStmt (Ret expr) = do
  t <- getIdentType (Ident "$retType$")
  t' <- checkExpr expr
  case (t' == t) of
    True -> pure ()
    _ -> throwError "[TYPE ERROR]: wrong return type of a function"

checkStmt VRet = do
  t <- getIdentType (Ident "$retType$")
  case t of
    Void -> pure ()
    _ -> throwError "[TYPE ERROR]: wrong return type of a function"

checkStmt (Cond expr stmt) = do
  condT <- checkExpr expr
  case condT of
    Bool -> checkStmt stmt
    _ -> throwError "[TYPE ERROR]: expression in condition statement doesn't return boolean value"

checkStmt (CondElse expr stmtTrue stmtFalse) = do
  condT <- checkExpr expr
  case condT of
    Bool -> do
      checkStmt stmtTrue
      checkStmt stmtFalse
    _ -> throwError "[TYPE ERROR]: expression in condition statement is not a boolean value"

checkStmt (While expr stmt) = do
  t <- checkExpr expr
  case t of
    Bool -> checkStmt stmt
    _ -> throwError "[TYPE ERROR]: expression in loop condition is not a boolean value"

checkStmt (Print expr) = do
  checkExpr expr
  pure ()

checkStmt (SExp expr) = do
  checkExpr expr
  pure ()

checkProg :: Program -> IO (Either String ())
checkProg (Program stmts) = runExceptT (runReaderT (checkStmts stmts) Map.empty)
