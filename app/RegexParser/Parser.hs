{-# LANGUAGE OverloadedStrings #-}
module RegexParser.Parser where

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

onSlide n = do
    string "///Onslide" 
    t <- manyTill anyChar "///Onslide"
    return $ mconcat ["\\onslide<",show n,">{\n" , t , "\n}\n"]

onslide n = parse (onSlide n)