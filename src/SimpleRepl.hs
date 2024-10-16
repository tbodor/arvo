module SimpleRepl
  ( repl
  )
where

import GHC.IO.Handle
import GHC.IO.Handle.FD (stdout)
import Eval (eval)
import Parser (parse)


repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  loop
  where
    loop = do
      putStr ">>> "
      input <- getLine
      evaluate input
      loop

evaluate :: String -> IO ()
evaluate expr = 
  case parse expr of
    Left err -> print err
    Right term -> print (eval [] term)
