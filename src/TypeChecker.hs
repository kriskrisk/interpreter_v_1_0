module TypeChecker where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Maybe
import qualified Data.Map as Map

import AbsCH
import TypesDef


throwErr :: Pos -> String -> TypeMonad a
throwErr pos msg = throwError ("[TYPE ERROR]: " ++ show (fromJust pos) ++ " " ++ msg)

getExprPos :: Expr Pos -> Pos
getExprPos x = case x of
    EVar a _ -> a
    ELitInt a _ -> a
    ELitTrue a -> a
    ELitFalse a -> a
    EApp a _ _ -> a
    EString a _ -> a
    Neg a _ -> a
    Not a _ -> a
    EMul a _ _ _ -> a
    EAdd a _ _ _ -> a
    ERel a _ _ _ -> a
    EAnd a _ _ -> a
    EOr a _ _ -> a

getOpPos :: RelOp Pos -> Pos
getOpPos x = case x of
  LTH a -> a
  LE a -> a
  GTH a -> a
  GE a -> a
  EQU a -> a
  NE a -> a

bool :: Type Pos
bool = Bool Nothing

int :: Type Pos
int = Int Nothing

str :: Type Pos
str = Str Nothing

void_ :: Type Pos
void_ = Void Nothing

getIdentType :: Pos -> Ident -> TypeMonad (Type Pos)
getIdentType pos ident = do
  res <- asks (Map.lookup ident)
  case res of
    (Just t) -> return t
    otherwise -> throwErr pos "referenced identificator is not declared"

checkArg :: (Type Pos, Expr Pos) -> TypeMonad ()
checkArg (t, expr) = do
  t' <- checkExpr expr
  if t == t'
    then pure ()
    else throwErr (getExprPos expr) "bad argument type"

checkExpr :: Expr Pos -> TypeMonad (Type Pos)
checkExpr (ELitInt _ _) = return int

checkExpr (EVar pos ident) = getIdentType pos ident

checkExpr (ELitTrue _) = return bool
checkExpr (ELitFalse _) = return bool

checkExpr (EApp pos ident args) = do
  t <- getIdentType pos ident
  case t of
    Fun _ tRet tArgs -> do
      mapM checkArg (zip tArgs args)
      return tRet
    otherwise -> throwErr pos "not applicable"

checkExpr (EString _ s) = return str

checkExpr (Neg pos e) = do
  t <- checkExpr e
  if t == int
    then return int
    else throwErr (getExprPos e) "not an integer"

checkExpr (EMul pos e1 op e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  if t1 == int
    then if t2 == int
      then return int
      else throwErr (getExprPos e2) "not an integer"
    else throwErr (getExprPos e1) "not an integer"

checkExpr (EAdd pos e1 op e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  if t1 == int
    then if t2 == int
      then return int
      else throwErr (getExprPos e2) "not an integer"
    else throwErr (getExprPos e1) "not an integer"

checkExpr (ERel pos e1 op e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  if t1 == t2
    then case t1 of
      Int _ -> return bool
      Bool _ -> case op of
        EQU _ -> return bool
        NE _ -> return bool
        otherwise -> throwErr (getOpPos op) "relation operation not allowed on boolean expressions"
      Str _ -> case op of
        EQU _ -> return bool
        NE _ -> return bool
        otherwise -> throwErr (getOpPos op) "relation operation not allowed on strings"
    else throwErr pos "relation operation on expressions of different type"

checkExpr (EAnd pos e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  if t1 == bool
    then if t2 == bool
      then return bool
      else throwErr (getExprPos e2) "not a boolean expressions"
    else throwErr (getExprPos e1) "not a boolean expressions"

checkExpr (EOr pos e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  if t1 == bool
    then if t2 == bool
      then return bool
      else throwErr (getExprPos e2) "not a boolean expressions"
    else throwErr (getExprPos e1) "not a boolean expressions"

declareItem :: Type Pos -> Item Pos -> TypeMonad () -> TypeMonad ()
declareItem t (Init pos ident e) m = do
  t' <- checkExpr e
  case (t' == t) of
    True -> do
      local (Map.insert ident t) m
    otherwise -> throwErr pos "wrong expression type"
declareItem t (NoInit _ ident) m = local (Map.insert ident t) m

declareArg :: Arg Pos -> TypeMonad () -> TypeMonad ()
declareArg (Arg _ t ident) m = do
  local (Map.insert ident t) m
declareArg (RefArg _ t ident) m = do
  local (Map.insert ident t) m

getArgType :: Arg Pos -> Type Pos
getArgType (Arg _ t _) = t
getArgType (RefArg _ t _) = t

checkStmts :: [Stmt Pos] -> TypeMonad ()
checkStmts [] = pure ()
checkStmts ((FnDef _ retT ident args (Block _ body)) : rest) = do
  local (Map.insert ident (Fun Nothing retT (map getArgType args))) $ do
    local (Map.insert (Ident "$retType$") retT) $ foldr declareArg (checkStmts body) args
    checkStmts rest
checkStmts ((Decl _ t decls) : rest) = foldr (declareItem t) (checkStmts rest) decls
checkStmts (stmt : stmts) = checkStmt stmt >> checkStmts stmts

checkStmt :: Stmt Pos -> TypeMonad ()
checkStmt (Empty _) = return ()

checkStmt (BStmt _ (Block _ stmts)) = checkStmts stmts

checkStmt (Ass pos ident expr) = do
  t <- getIdentType pos ident
  t' <- checkExpr expr
  if t == t'
    then pure ()
    else throwErr (getExprPos expr) "wrong expression type in assignment"

checkStmt (Incr pos ident) = do
  t <- getIdentType pos ident
  if t == int
    then pure ()
    else throwErr pos "incrementing non integer value"

checkStmt (Decr pos ident) = do
  t <- getIdentType pos ident
  if t == int
    then pure ()
    else throwErr pos "decrementing non integer value"

checkStmt (Ret pos expr) = do
  t <- getIdentType pos (Ident "$retType$")
  t' <- checkExpr expr
  if t == t'
    then pure ()
    else throwErr (getExprPos expr) "wrong type of expression in return statement"

checkStmt (VRet pos) = do
  t <- getIdentType pos (Ident "$retType$")
  if t == void_
    then pure ()
    else throwErr pos "wrong return type"

checkStmt (Cond pos expr stmt) = do
  condT <- checkExpr expr
  if condT == bool
    then checkStmt stmt
    else throwErr (getExprPos expr) "not a boolean expression"

checkStmt (CondElse pos expr stmtTrue stmtFalse) = do
  condT <- checkExpr expr
  if condT == bool
    then do
      checkStmt stmtTrue
      checkStmt stmtFalse
    else throwErr (getExprPos expr) "not a boolean expression"

checkStmt (While pos expr stmt) = do
  t <- checkExpr expr
  if t == bool
    then checkStmt stmt
    else throwErr (getExprPos expr) "not a boolean expression"

checkStmt (Print _ expr) = do
  checkExpr expr
  pure ()

checkStmt (SExp _ expr) = do
  checkExpr expr
  pure ()

checkProg :: (Program Pos) -> IO (Either String ())
checkProg (Program _ stmts) = runExceptT (runReaderT (checkStmts stmts) Map.empty)
