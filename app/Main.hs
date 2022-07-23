import Latte.Interpreter (runInterpret)
import Latte.Par (myLexer, pProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then error "Pass only file name!"
    else do
      s <- readFile $ head args
      run s

run :: String -> IO ()
run s =
  let tokens = myLexer s
   in case pProgram tokens of
        Left err -> do
          putStrLn "\nParse              Failed...\n"
          exitFailure
        Right tree -> do
          result <- runInterpret tree
          case result of
            Left except -> print except
            Right _ -> exitSuccess
