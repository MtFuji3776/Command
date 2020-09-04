{-# LANGUAGE OverloadedStrings #-}
module Regex where

import Turtle
import qualified Data.Text as T

replace :: Pattern b -> T.Text -> T.Text -> T.Text
replace p t1 t = let ts = match (sepBy chars p) t
                     t' = last ts
                     between' z x y = if y == mempty then x <> y else x <> z <> y
                 in if t' == [] then t else foldr (between' t1) mempty t'

    -- pandocが出力したLaTeXから\tightlistコマンドを除去する関数
rmtightlist :: T.Text -> T.Text
rmtightlist = replace (text "\\tightlist") "" 

repquotations :: Turtle.FilePath -> IO ()
repquotations path = do
    t <- readTextFile path
    let ts = filter (/= mempty) $ cut "'''" t
    let t' = foldr (\x ys -> if ys == mempty then x <> ys else x <> "```" <> ys) mempty ts
    writeTextFile path t'