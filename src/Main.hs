module Main where

import System.IO
import System.Environment

import LexCH
import ParCH
import AbsCH

import ErrM

import Interpreter
import TypeChecker


main = do
  args <- getArgs
  fd <- openFile (head args) ReadMode
  program <- hGetContents fd
  case pProgram (myLexer program) of
    Bad errMsg -> hPutStrLn stderr errMsg
    Ok prog -> do
      res <- checkProg prog
      case res of
        Left errMsg -> hPutStrLn stderr errMsg
        Right _ -> interpretProg prog
