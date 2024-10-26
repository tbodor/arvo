{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Repl
  ( repl,
  )
where

import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.Foldable (foldl')
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import Data.Version (showVersion)
import Interpreter (Value, normalise, uneval, eval)
import Parser (parseModule)
import qualified Parser (keywords)
import Paths_arvo (version)
import Syntax (Name, Term)
import System.Console.Haskeline.Completion
import System.Console.Repline
import Environment (Environment, extend, names, bindings, empty)


-- | Interpreter state
data Interpreter = Interpreter
  { -- | Term environment
    termEnv :: Environment Value
  }

initInterpreter :: Interpreter
initInterpreter = Interpreter empty

type Repl a = HaskelineT (StateT Interpreter IO) a

repl :: IO ()
repl =
  flip evalStateT initInterpreter $
    evalReplOpts
      ReplOpts
        { banner = const . pure $ ">>> ",
          command = exec "",
          options = opts,
          prefix = Just ':',
          multilineCommand = Nothing,
          tabComplete = Prefix (completeWord Nothing " \t().=" completer) defaultMatcher,
          initialiser = start,
          finaliser = end
        }

exec :: FilePath -> String -> Repl ()
exec file source = do
  interpreterState <- get
  declarations <- handleError $ parseModule file source
  let newState = interpreterState {termEnv = foldl' evalBinding (termEnv interpreterState) declarations}
  put newState

  -- Show normal form of the last evaluated expression
  case lookup "it" declarations of
    Nothing -> return ()
    Just value -> liftIO $ print $ normalise (termEnv newState) value

-- | Evaluate a binding (declaration) and update the environment with a new entry with the specified name, bound to the
--   result of evaluating the term
evalBinding :: Environment Value -> (Name, Term) -> Environment Value
evalBinding env (name, term) = env'
  where
    env' = extend name (eval env term) env

handleError :: (Show err) => Either err a -> Repl a
handleError (Right value) = return value
handleError (Left err) = do
  liftIO $ print err
  abort

-- Commands
opts :: [(String, String -> Repl ())]
opts =
  [ ("load", load),
    ("env", printEnvironment)
  ]

load :: String -> Repl ()
load file = do
  let fileName = trim file
  contents <- liftIO $ readFile fileName
  exec fileName contents

-- Yuck. There is Data.Text.strip, but we have String, not Text...
-- I wonder if (unpack . strip . pack) is better or worse...
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

printEnvironment :: String -> Repl ()
printEnvironment _ = do
  interpreterState <- get
  let env = termEnv interpreterState
  let bound = names env
  liftIO $ mapM_ print [(name, uneval bound value) | (name, value) <- bindings env]

-- Tab completion
completer :: (MonadState Interpreter m) => (String -> m [Completion])
completer n = do
  let replacements = [("\\", "λ")]
  let commands = map ((':' :) . fst) opts -- commands, with prepended activation prefix character (':')
  let keywords = filter (\kw -> isNothing (lookup kw replacements)) Parser.keywords
  env <- gets termEnv
  let declarations = names env
  let completions = [(c, c) | c <- keywords ++ declarations ++ commands]
  return [Completion rep name False | (name, rep) <- replacements ++ completions, n `isPrefixOf` name]

defaultMatcher :: (MonadIO m) => [(String, CompletionFunc m)]
defaultMatcher =
  [ (":load", fileCompleter)
  ]


start :: Repl ()
start = liftIO $ putStrLn $ "Arvo " ++ showVersion version

end :: Repl ExitDecision
end = do
  liftIO $ putStrLn "Bye."
  return Exit
