{-# LANGUAGE PatternSynonyms #-}
module Main where

import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import Parser ( parseRegex )
import Lang ( Re(Nil) )
import Derivative ( matches )

pattern Quit :: Maybe String
pattern Quit <- Just ":q"

pattern Print :: Maybe String
pattern Print <- Just ":p"

pattern Set :: [Char] -> Maybe [Char]
pattern Set re <- Just (':':'s':' ':re)

loop :: Re -> InputT IO ()
loop regex = do
    line <- getInputLine "hsderiv> "
    case line of
        Nothing  -> return ()
        Quit     -> return ()
        Print    -> outputStrLn (show regex) >> loop regex
        Set re   -> case parseRegex re of
            Right r -> outputStrLn (show r) >> loop r
            Left  m -> outputStrLn (show m) >> loop regex
        Just str -> do
            outputStrLn $ show $ matches str regex
            loop regex

main :: IO ()
main = runInputT defaultSettings $ loop Nil