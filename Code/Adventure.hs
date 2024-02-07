module Main where

import World
import Actions

import Control.Monad
import System.IO
import System.Exit

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case actions cmd of
                            Just fn -> fn arg state
                            Nothing -> (state, "I don't understand")
process state [cmd]     = case commands cmd of
                            Just fn -> fn state
                            Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")

repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do print state
                putStr "What now? "
                hFlush stdout
                cmd <- getLine
                let (state', msg) = process state (words cmd)
                putStrLn msg
                if (won state') then do putStrLn winmessage
                                        return state'
                               else repl state'

main :: IO ()
main = do repl initState
          return ()
