module Main
  where

    import W (getType)
    import Resolver (insertType)
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
          result <- getType refExpr
          case result of
            Right (ctx, t) -> do
              let insT = insertType ctx t
              putStr ("Context: " ++ show ctx ++ "\n")
              writeFile "task3.out" (show insT)
            Left err -> do
              writeFile "task3.out" "Лямбда-выражение не имеет типа."
              putStr ("Error: " ++ show err ++ "\n")
      return ()
