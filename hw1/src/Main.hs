module Main
  where

    import System.Clock
    -- import Reductor (normilize)
    import HeadReductor (normilize)
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
          before <- getTime Realtime
          let refExpr = reformatExpr expr
          putStr (show refExpr ++ "\n")
          let norExpr = normilize refExpr
          writeFile "task1.out" (show norExpr)
          after <- getTime Realtime
          putStr ((show $ sec (after - before)) ++ " second\n")
      return ()
