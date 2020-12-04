{-# LANGUAGE OverloadedStrings #-}
module PandocTool where

-- import Text.Pandoc.Readers.CommonMark
-- import Text.Pandoc.Options
-- import Text.Pandoc.Definition
-- import Text.Pandoc.Class
import Text.Pandoc
import Data.Text (Text,pack,unpack)
import qualified Data.Text.IO as TIO
import Text.Pandoc.PDF
import Text.Pandoc.Builder
import Turtle
import Regex



test :: IO ()
test = do
    file <- readFile "test.md"
    let opts = def
    result <- runIO $ do
        doc <- readMarkdown def $ pack file
        writeLaTeX opts doc
    rst <- handleError result
    TIO.putStrLn rst

-- writeLaTeX後にText値として加工できることを確認。ここでPreambleを挿入すれば楽勝なのでは。
    -- writeLaTeXの内容込みで編集できればラクなんだが。
test1 = do
    file <- readFile "test.md"
    result <- runIO $ do
        doc <- readMarkdown def $ pack file
        txt <- writeLaTeX def doc
        return $ preamble1 txt
    rst <- handleError result
    TIO.putStrLn rst
    TIO.writeFile "test.tex" rst

-- genPDF :: Text -> IO ()
genPDF file = do
    src     <- TIO.readFile (unpack file <> ".md")
    result  <- runIO $ do
        doc <- readMarkdown (def{readerExtensions = extensionsFromList [Ext_tex_math_dollars,Ext_raw_tex]}) src
        txt <- writeLaTeX def doc
        return $ preamble1 txt
    rst <- handleError result
    let rst' = rmtightlist rst
    TIO.putStrLn rst'
    TIO.writeFile (unpack file <> ".tex") rst'
    runIO $ shell ("lualatex " <>  file) ""

preamble1 :: Text -> Text
preamble1 txt = "\\documentclass{ltjsarticle}\n"
    -- <> "\\usepackage[utf8]{inputenc}\n"
    <> "\\usepackage{luatexja-otf}\n"
    <> "\\usepackage[]{luatexja,luatexja-fontspec}"
    <> "\\usepackage{booktabs}\n"
    <> "\\usepackage{array}\n"
    <> "\\usepackage{graphicx}\n"
    <> "\\usepackage{mathpazo}\n"
    <> "\\usepackage{amsmath}\n"
    <> "\\usepackage{amsthm}\n"
    <> "\\usepackage{amssymb}\n"
    <> "\\usepackage{amsfonts}\n"
    <> "\\usepackage[noheadfoot,top=10mm,bottom=10mm,hmargin=10mm]{geometry}\n"
    -- <> "\\usepackage{enumitem}\n"
    <> "\\usepackage{tikz}\n"
    -- <> "\\usepackage[epsilon]{backnaur}\n"
    -- <> "\\usepackage{bussproofs}\n"
    -- <> "\\usepackage[dvipdfmx]{hyperref}\n"
    -- <> "\\usepackage{pxjahyper}\n"
    <> "\\usetikzlibrary{matrix}\n"
    -- <> "\\usetikzlibrary{cd}\n"
    <> "\\usepackage{pgfcore}\n"
    <> "\\usepackage{color}\n"
    -- <> "\\usepackage{CJKutf8}"
    <> "\\newtheorem{dfn}{Def}\n"
    <> "\\newtheorem{thm}{Thm}\n"
    <> "\\newtheorem{cor}{Cor}\n"
    <> "\\newtheorem{prop}{Prop}\n"
    <> "\\newtheorem{rk}{remark}\n"
    <> "\\newtheorem{claim}{claim}\n"
    <> "\\newtheorem{recall}{recall}\n"
    <> "\\newtheorem{ques}{Q.}\n"
    -- <> "\\newenvironment{diagram}{\n"
    -- <> "\\begin{center}\\begin{tikzpicture}[-stealth]}{\\end{tikzpicture}\n"
    -- <> "\\end{center}}\n"
    -- <> "\\newenvironment{dialan}[1]\n"
    -- <> "{\\begin{diagram}\\node[matrix]}{\\end{diagram}}\n"
    -- <> "\\newcommand{\\vertical}[2]{\\draw[-Butt Cap] (0,-0.2) -- (0,#1.2) node[above]{\\small #2};}\n"
    -- <> "\\newcommand{\\obj}[4]{\\node #1 at (#2,#3) {#4};}\n"
    -- <> "\\newcommand{\\nolabel}[1]{\\fill #1 circle(2pt);}\n"
    -- <> "\\newcommand{\\anglebrace}[1]{\\langle #1 \\rangle}\n"
    -- <> "\\newcommand{\\reticle}[2]{\\draw[-Butt Cap] (#1 - 0.1,#2) -- (#1 + 0.1,#2);\n"
    -- <> "                         \\draw[-Butt Cap] (#1 , #2 - 0.1) -- (#1 , #2 + 0.1);\n"
    -- <> "                         \\fill[white] (#1 - 0.05,#2 - 0.05) rectangle (#1 + 0.05,#2 + 0.05);}\n"
    <> "\\date{\\today}\n"
    <> "\\author{俺}\n"
    <> "\\title{テスト}\n"
    <> "\\begin{document}\\maketitle\n"
    <> txt
    <> "\n\\end{document}\n"