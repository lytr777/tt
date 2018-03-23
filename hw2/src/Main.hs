module Main
  where

    import Resolver (resolveSystem, insertType, Matcher)
    import Typer (getTypeAnnotation, Context)
    import Simplifier (reformatExpr)
    import Parser (exprBegin)
    import Text.Megaparsec (parse)
    import Text.Megaparsec.Error (parseErrorPretty)
    import qualified Data.Map.Strict as Map

    main :: IO ()
    main = do
      str <- readFile "task2.in"
      case (parse exprBegin "" str) of
        Left err -> putStr (parseErrorPretty err)
        Right expr -> do
          let refExpr = reformatExpr expr
          putStr (show refExpr ++ "\n")
          let (s, ctx, t) = getTypeAnnotation refExpr
          putStr ("System: " ++ show s ++ "\n")
          putStr ("Context: " ++ show ctx ++ "\n")
          putStr ("Type: " ++ show t ++ "\n")
          case resolveSystem s of
            Just m  -> do
              putStr ("Matcher: " ++ show m ++ "\n")
              let insS = insertType t m
              let ctxStr = contexToString ctx m
              writeFile "task2.out" ((show insS) ++ ctxStr)
            Nothing -> do
              writeFile "task2.out" "Лямбда-выражение не имеет типа."
              putStr "Not solved\n"
      return ()

    contexToString :: Context -> Matcher -> String
    contexToString ctx = listToString (Map.toList ctx)
      where
        listToString (x:xs) m = "\n" ++ (fst x) ++ ": " ++
          show (insertType (snd x) m) ++ (listToString xs m)
        listToString [] _ = ""
