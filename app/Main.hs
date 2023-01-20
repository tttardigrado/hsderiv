{-# LANGUAGE PatternSynonyms #-}
module Main where

import System.Console.Haskeline
import Parser
import Lang
import Derivative

pattern Quit :: Maybe String
pattern Quit <- Just ":q"

pattern Print :: Maybe String
pattern Print <- Just ":p"

pattern Set :: [Char] -> Maybe [Char]
pattern Set re <- Just (':':'r':' ':re)

loop :: Re -> InputT IO ()
loop regex = do
    line <- getInputLine "user> "
    case line of
        Nothing  -> return ()
        Quit     -> return ()
        Print    -> outputStrLn (show regex) >> loop regex
        Set re   -> case parseRegex re of
            Right r -> loop r
            Left  m -> outputStrLn (show m) >> loop regex
        Just str -> do
            outputStrLn $ show $ matches str regex
            loop regex

main :: IO ()
main = runInputT defaultSettings $ loop Nil