module Main
  where

    import Simplifier (reformatExpr)
    import Parser (exprBegin)
    import Text.Megaparsec (parse)
    import Text.Megaparsec.Error (parseErrorPretty)

    main :: IO ()
    main = do
      str <- readFile "task3.in"
      case (parse exprBegin "" str) of
        Left err -> putStr (parseErrorPretty err)
        Right expr -> do
          let refExpr = reformatExpr expr
          putStr (show refExpr ++ "\n")
      return ()
