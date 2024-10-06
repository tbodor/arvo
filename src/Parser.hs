module Parser
  ( parse
  , parseTerm
  , parseModule
  , rawParse
  , ParseError
  , keywords
  )
where

import Control.Arrow (ArrowChoice (left))
import Data.Void (Void)
import Syntax ( Term(..) )
import Text.Megaparsec hiding (ParseError, parse)
import qualified Text.Megaparsec (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

newtype ParseError = ParseError {errorBundle :: ParseErrorBundle String Void}

instance Show ParseError where
  show = errorBundlePretty . errorBundle


spaces :: Parser ()
spaces = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: String -> Parser String
symbol = L.symbol spaces

reserved :: String -> Parser ()
reserved keyword = lexeme (string keyword *> notFollowedBy alphaNumChar)


identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = some letterChar <?> "variable"
    check x =
      if x `elem` keywords
      then fail $ "keyword " ++ x ++ " is reserved"
      else return x

keywords :: [String]
keywords = ["λ", "\\"]


lambda :: Parser String
lambda = symbol "λ" <|> symbol "\\" <?> "λ"


variable, term, abstraction, application :: Parser Term

variable = Variable <$> identifier

term = abstraction <|> application

-- >>> parseTerm "λx.x"
-- Right λx.x


abstraction = Lambda <$> between lambda (symbol ".") identifier <*> term

application = foldl1 Application <$> (abstraction <|> variable <|> parens term) `sepBy1` space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

letin :: Parser Term
letin = undefined


-- >>> parseTerm "let id = λx.x in id id"
-- Right let id = λx.x in id id


type Binding = (String, Term)

-- | Parse a term and bind it to a special name 'it', holding the last evaluated expression
val :: Parser Binding
val = do
  t <- term
  return ("it", t)

-- | 'let' declaration, introduces a top level binding (in the global environment)
letdecl :: Parser Binding
letdecl = undefined

declaration :: Parser Binding
declaration = val

expression :: Parser Binding
expression = do
  x <- declaration
  _ <- optional (symbol ";")
  return x


-- | Parse module, a sequence of expression bindings separated by semicolons
modl :: Parser [Binding]
modl = many expression



parse :: Parser a -> FilePath -> String -> Either ParseError a
parse parser file input = left ParseError (Text.Megaparsec.parse parser file input)

parseTerm :: String -> Either ParseError Term
parseTerm = parse term ""

parseModule :: FilePath -> String -> Either ParseError [(String, Term)]
parseModule = parse (between space eof modl)


rawParse :: String -> Either (ParseErrorBundle String Void) Term
rawParse = Text.Megaparsec.parse term ""


-- >>> parseTerm "let id = λx.x in id id"
-- Right let id = λx.x in id id

-- >>> parse expression "" "let s = λx.λy.λz.x z (y z);\n s"
-- Right ("s",λx.λy.λz.x z (y z))

-- >>> parseModule "" "\nlet s = λx.λy.λz.x z (y z);\n\n  let k = λx.λy.y;\n\ns k k"
-- Right [("s",λx.λy.λz.x z (y z)),("k",λx.λy.y),("it",s k k)]

