module Main where

import System.IO
import System.Exit
import Adder (add)
import Binary (fromString, toString, toDecimal, fromDecimal, Binary(..))
import Text.Read


main :: IO ()
main = mainLoop [F]

mainLoop :: [Binary] -> IO ()
mainLoop acc = do
  num <- prompt "Enter a binary number: "
  case asBinary num of
    Nothing  -> exit "Not a number.\nExiting"
    Just bin -> do
      let sum  = acc `add` bin
      putStrLn "\nCurrent total:"
      putStrLn $ "\tBin: " ++ toString sum
      putStrLn $ "\tDec: " ++ (show . toDecimal) sum
      mainLoop sum

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


asBinary :: String -> Maybe [Binary]
asBinary str =
  if isBinary str then Just $ fromString str
  else case readMaybe str :: Maybe Integer of
    Just n  -> Just $ fromDecimal n
    Nothing -> Nothing

isBinary :: String -> Bool
isBinary str = str == filter (\x -> x == '1' || x == '0') str

exit :: String -> IO ()
exit msg = do
  putStrLn msg
  exitSuccess
