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
  args <- getArgs   -- TODO: Add case of missing file to interpret
  fd <- openFile (head args) ReadMode
  program <- hGetContents fd
  --print $ pProgram (myLexer program)
  let Ok prog = pProgram (myLexer program) in do
    res <- checkProg prog
    case res of
      Left errMsg -> print errMsg
      Right _ -> interpretProg prog
