module Main
  where

    import Reductor (normilize)
    import Simplifier (reformatExpr)
    import Parser (exprBegin)
    import Text.Megaparsec (parse)
    import Text.Megaparsec.Error (parseErrorPretty)

    main :: IO ()
    main = do
      str <- readFile "task1.in"
      case (parse exprBegin "" str) of
        Left err -> putStr (parseErrorPretty err)
        Right expr -> do
          let refExpr = reformatExpr expr
          putStr (show refExpr ++ "\n")
          let norExpr = normilize refExpr
          writeFile "task2.out" (show norExpr)
      return ()
