module Parser
  ( exprBegin
  ) where

  import Control.Monad (void)
  import Struct
  import Data.Void
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import qualified Text.Megaparsec.Char.Lexer as Lexer

  type Parser = Parsec Void String

  sc :: Parser ()
  sc = Lexer.space space1 lineCmnt blockCmnt
    where
      lineCmnt  = Lexer.skipLineComment "//"
      blockCmnt = Lexer.skipBlockComment "/*" "*/"

  lexeme :: Parser a -> Parser a
  lexeme = Lexer.lexeme sc

  symbol :: String -> Parser String
  symbol = Lexer.symbol sc

  parens :: Parser a -> Parser a
  parens = between (symbol "(") (symbol ")")

  identifier :: Parser String
  identifier = (lexeme . try) p
    where
      p = (:) <$> letterChar <*> many (alphaNumChar <|> (char '_'))

  exprBegin :: Parser Expr
  exprBegin = between sc eof exprParser

  exprParser :: Parser Expr
  exprParser = (appParser >>= (\a -> (eve $ EAVE a) <|> (ea a))) <|> eve EVE

  eve :: (Var -> Expr -> Expr) -> Parser Expr
  eve f = do
    void (symbol "\\")
    var <- varParser
    void (symbol ".")
    expr <- exprParser
    return $ f var expr

  ea :: App -> Parser Expr
  ea app = EA <$> return app

  appParser :: Parser App
  appParser = atomParser >>= recApp . AA

  recApp :: App -> Parser App
  recApp app = (atomParser >>= recApp . (AAA app)) <|> return app

  atomParser :: Parser Atom
  atomParser =  AE <$> (parens exprParser)
            <|> AV <$> varParser

  varParser :: Parser Var
  varParser = VS <$> identifier
