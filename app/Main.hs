module Main where

import System.IO
import System.Exit
import Adder (add)
import Binary (fromString, toString, Binary(..))


main :: IO ()
main = mainLoop [F]

mainLoop :: [Binary] -> IO ()
mainLoop acc = do
  num <- prompt "Enter a biary number: "
  if (not . isBinary) num then exit "Not a binary number.\nExiting"
  else do
    let bin = fromString num
        sum  = acc `add` bin
    putStrLn $ "Current total: " ++ toString sum
    mainLoop sum

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

isBinary :: String -> Bool
isBinary str = str == filter (\x -> x /= '1' && x /= '0') str

exit :: String -> IO ()
exit msg = do
  putStrLn msg
  exitSuccess
