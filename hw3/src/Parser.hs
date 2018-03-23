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
  identifier = (lexeme . try) (p >>= check)
    where
      p = (:) <$> letterChar <*> many (alphaNumChar <|> (char '_'))
      check x = if elem x rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

  rword :: String -> Parser ()
  rword w = lexeme (string w *> notFollowedBy alphaNumChar)

  rws :: [String]
  rws = ["let","in"]

  exprBegin :: Parser Expr
  exprBegin = between sc eof exprParser

  exprParser :: Parser Expr
  exprParser = avee <|> AAb <$> absParser

  avee :: Parser Expr
  avee = do
    rword "let"
    var <- varParser
    void (symbol "=")
    expr <- exprParser
    rword "in"
    inExpr <- exprParser
    return $ AVEE var expr inExpr

  absParser :: Parser Abs
  absParser = (appParser >>= (\a -> (eve $ EAVA a) <|> (ea a))) <|> eve EVA

  eve :: (Var -> Abs -> Abs) -> Parser Abs
  eve f = do
    void (symbol "\\")
    var <- varParser
    void (symbol ".")
    ab <- absParser
    return $ f var ab

  ea :: App -> Parser Abs
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
