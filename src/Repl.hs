module Repl
  ( repl
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.Version (showVersion)
import Interpreter (normalise)
import Parser (parse)
import Paths_arvo (version)
import System.Console.Haskeline.Completion
import System.Console.Repline

type Repl a = HaskelineT IO a

repl :: IO ()
repl =
  evalReplOpts $
    ReplOpts
      { banner = const . pure $ ">>> ",
        command = cmd,
        options = [],
        prefix = Nothing,
        multilineCommand = Nothing,
        tabComplete = Custom (completeWord Nothing " \t()." completer),
        initialiser = start,
        finaliser = end
      }

cmd :: String -> Repl ()
cmd input = liftIO $ evaluate input

evaluate :: String -> IO ()
evaluate expr =
  case parse expr of
    Left err -> print err
    Right term -> print (normalise [] term)

completer :: (Monad m) => (String -> m [Completion])
completer n = do
  let completions = [("\\", "λ")]
  return [Completion rep rep False | (name, rep) <- completions, n `isPrefixOf` name]

start :: Repl ()
start = liftIO $ putStrLn $ "Arvo " ++ showVersion version

end :: Repl ExitDecision
end = do
  liftIO $ putStrLn "Bye."
  return Exit
