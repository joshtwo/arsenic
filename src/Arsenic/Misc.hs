{-# LANGUAGE OverloadedStrings #-}

module Arsenic.Misc (
    confDir,
    confDir',
    getInput,
    getInputDef,
    askPrompt,
    reportIOError,
    tokenise,
    replace,
    DefaultOption(..),
    subIndex,
    subIndexBS,
    andJoin,
    commaJoin,
    (<+>),
    (<:>)
    ) where

import Text.Printf (printf)
import Data.Char (toLower)
import System.IO.Error
import Data.ByteString.Lazy.Char8 (ByteString)
import System.Environment (getEnv)
import qualified Data.ByteString as B
import Data.List (findIndex, isPrefixOf, tails)
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as L

-- | Directory where Arsenic's configuration goes
confDir :: IO String
confDir = liftM (++"/.arsenic") $ getEnv "HOME"

-- | Append to the default config directory
confDir' file = liftM (++"/"++file) confDir

-- | Grab input from the keyboard using a promp.
getInput :: String -> IO String
getInput prompt = 
    do putStr prompt
       text <- getLine
       if text==""
          then do putStrLn ">> You must enter a value."
                  getInput prompt
          else return text

-- | Grab input from the keyboard using a promp and a default value for when
-- the user enters nothing.
getInputDef :: String -> String -> IO String
getInputDef prompt defValue = 
    do putStr prompt
       input <- getLine
       if input == ""
          then return defValue
          else return input

data DefaultOption = DefaultYes | DefaultNo | NoDefault

-- | Have the user answer a yes or no question, with a potential default answer.
askPrompt :: String -> DefaultOption -> IO Bool
askPrompt prompt def =
    do option <- 
         case def of
              DefaultYes -> getInputDef (prompt++" [Y/n]: ") "y"
              DefaultNo  -> getInputDef (prompt++" [y/N]: ") "n"
              NoDefault  -> getInput (prompt++" [y/n]: ")
       case toLower $ head option of
            'y' -> return True
            'n' -> return False
            _   -> do putStrLn ">> You must say \"yes\" or \"no\"."
                      askPrompt prompt def

-- | Catch an error for the given action, and if one occurs, prefix
-- it with the given string
reportIOError str action = catch action (error . (str++) . ioeGetErrorString)

-- | An alias to "Data.ByteString.Lazy.Char8"'s @append@ function.
infixr 5 <+>
(<+>) :: ByteString -> ByteString -> ByteString
(<+>) = L.append

-- | An alias to "Data.ByteString.Lazy.Char8"'s @cons@ function.
infixr 5 <:>
(<:>) :: Char -> ByteString -> ByteString
(<:>) = L.cons

-- | Split a ByteString by a ByteString.
tokenise :: ByteString -> ByteString -> [ByteString]
tokenise x y = h : if L.null t then [] else tokenise x (L.drop (L.length x) t)
    where (h,t) = breakSubstring x y

-- | From Data.ByteString, no equivalent in Data.ByteString.Lazy*
breakSubstring pat src = search 0 src
    where search n s
            | L.null s             = (src,L.empty)
            | pat `L.isPrefixOf` s = (L.take n src,s)
            | otherwise            = search (n+1) (L.tail s)

-- | Do a string replacement.
replace :: ByteString -> ByteString -> ByteString -> ByteString
replace old new = L.intercalate new . tokenise old

-- | Find the location of a sublist within a list
subIndex sub list = findIndex (isPrefixOf sub) (tails list)

-- | Find the location of a @ByteString@ within another @ByteString@.
subIndexBS sub str = findIndex (L.isPrefixOf sub) (L.tails str)

-- | Join a list of @ByteString@s with commas
commaJoin :: [ByteString] -> ByteString
commaJoin = L.intercalate ", "

-- | Join a list of @ByteStrings@s with commas and end the list with "and"
andJoin :: [ByteString] -> ByteString
andJoin list
    | length list > 2 = let (front, [back]) = splitAt (length list - 1) list
                        in commaJoin front <+> " and " <+> back
    | length list == 2 = head list <+> " and " <+> last list
    | length list == 1 = head list
    | null list = ""
