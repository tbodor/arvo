module Parser
  ( parse
  , rawParse
  , ParseError
  )
where

import Control.Arrow (ArrowChoice (left))
import Data.Void (Void)
import Syntax ( Term(..) )
import Text.Megaparsec hiding (ParseError, parse)
import qualified Text.Megaparsec (parse)
import Text.Megaparsec.Char

type Parser = Parsec Void String

newtype ParseError = ParseError {errorBundle :: ParseErrorBundle String Void}

instance Show ParseError where
  show = errorBundlePretty . errorBundle

variable, term, group, abstraction, application :: Parser Term
name :: Parser String
name = some letterChar

lambda :: Parser Char
lambda = char 'λ' <|> char '\\'

variable = Variable <$> name

term = abstraction <|> application

group = between (char '(') (char ')') term

abstraction = Lambda <$> between lambda (char '.') name <*> term

application = foldl1 Application <$> (abstraction <|> variable <|> group) `sepBy1` space

parse :: String -> Either ParseError Term
parse input = left ParseError (Text.Megaparsec.parse term "" input)


rawParse :: String -> Either (ParseErrorBundle String Void) Term
rawParse = Text.Megaparsec.parse term ""