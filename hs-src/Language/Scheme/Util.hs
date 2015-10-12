{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Language.Scheme.Util
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains general-purpose utility functions
-}

module Language.Scheme.Util
    ( countAllLetters
    , countLetters
    , escapeBackslashes
    , lastN'
    , strip
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as DL

-- |A utility function to escape backslashes in the given string
escapeBackslashes :: Text -> Text
escapeBackslashes = T.replace "\\" "\\\\"

-- | Remove leading/trailing white space from a string; based on corresponding 
--   Python function. Code taken from: 
--
--   <http://gimbo.org.uk/blog/2007/04/20/splitting-a-string-in-haskell/>
strip :: Text -> Text
strip = T.dropAround (`elem` [' ', '\n', '\t', '\r'])

-- |Count occurences of a letter in a list of strings
countAllLetters :: Char -> [Text] -> Int
countAllLetters c strs = sum $ map (countLetters c) strs

-- |Count occurences of a letter in a string
countLetters :: Char -> Text -> Int
countLetters c str = T.count (T.singleton c) str

-- | Take last n elements of a list, from:
--   <http://stackoverflow.com/q/17252851/101258>
lastN' :: Int -> [a] -> [a]
lastN' n xs = DL.foldl' (const .drop 1) xs (drop n xs)

