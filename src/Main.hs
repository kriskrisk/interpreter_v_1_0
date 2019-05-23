module Main where

import System.IO
import System.Environment

import LexCH
import ParCH
import AbsCH

import ErrM

import Interpreter


main = do
  args <- getArgs   -- TODO: Add case of missing file to interpret
  fd <- openFile (head args) ReadMode
  program <- hGetContents fd
  let Ok prog = pProgram (myLexer program) in interpretProg prog
