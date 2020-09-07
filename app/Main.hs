{-# LANGUAGE OverloadedStrings #-}
module Main where


import System.Environment
import Turtle
import qualified Data.Text as T
import System.Random
--import Regex

main :: IO ()
main = do
    n <- getProgName
    args <- getArgs
    putStr $ n ++ concat args 


-- 休憩時間の筋トレ用トランプシャッフル
shuffle_ :: IO Int
shuffle_ = getStdRandom (randomR (1,13))

shuffle :: IO ()
shuffle = shuffle_ >>= print
-- ディレクトリ移動用関数たち

-- ディレクトリツリーの現在位置から親ノードへ移動
up :: MonadIO m => m ()
up = cd ".." 

-- homeがホームディレクトリを表すIO FilePath値。わかりやすい。
    -- 変化に耐えられるようIOモナドの値になっているのか。
gohome :: IO ()
gohome = home >>= \p -> cd p

-- Homeからlatexsに移動する
homeTodocuments :: IO ()
homeTodocuments = cd "documents"

documentsTolatexs :: IO()
documentsTolatexs = cd "latexs"

tolatexs :: IO ()
tolatexs = gohome <> cd "documents/latexs"

tonotes :: IO()
tonotes = gohome <> cd "documents/latexs/notes"

toreading :: IO()
toreading = tonotes <> cd "reading"

towriting :: IO()
towriting = tonotes <> cd "writing"

tothinking :: IO()
tothinking = tonotes <> cd "thinking"

totaskreports :: IO()
totaskreports = tonotes <> cd "taskreports"

toworksheets :: IO()
toworksheets = gohome <> cd "documents/latexs/worksheets"

topapers :: IO()
topapers = do
    gohome
    cd "documents/papers"

tocommand :: IO()
tocommand = gohome <> cd "haskell_testing/command/command"

totemplates :: IO()
totemplates = do
    gohome
    cd "documents/latexs/templates"

tohaskell :: IO()
tohaskell = gohome <> cd "haskell_testing"

topurescript :: IO ()
topurescript = gohome >> cd "purescriptprojects"

-- 主要ディレクトリに移動してPathを取得してくるコマンドライン
-- 共通の仕様として、Pathを取得したら元のディレクトリに戻ってくる。
pathOfReading :: IO Turtle.FilePath
pathOfReading = do
    p <- pwd
    toreading
    readingpath <- pwd
    cd p
    return readingpath




-- ファイル、ディレクトリを移動させるコマンド関数
mvtoreading :: Turtle.FilePath -> IO ()
mvtoreading name = do{p <- pathOfReading;mv name p}


-- gettemplates :: Turtle.FilePath -> IO Text
gettemplates name = do
    p <- pwd
    totemplates
    path <- do{p <- pwd ; return $ p <> name <.> "tex"} --指名したテンプレを絶対パスに変換
    t <- readTextFile path
    cd p
    return t

-- LaTeXのテンプレを取ってきた後、元いたディレクトリに指定した名前で.texファイルを作るコマンド関数
mklatexfile :: Turtle.FilePath -> Turtle.FilePath -> IO ()
mklatexfile name1 name2 = do
    t <- gettemplates name1
    path <- pwd
    writeTextFile (path <> name2 <.> "tex") t

mkuplatex :: Turtle.FilePath -> IO ()
mkuplatex = mklatexfile "tikzandbussproofs"

mkbeamer :: Turtle.FilePath -> IO ()
mkbeamer = mklatexfile "beamer"

-- LaTeXのお決まりパッケージを自動生成するコマンドライン

arrange :: IO () -> Turtle.FilePath -> Turtle.FilePath -> IO ()
arrange f filetype filename = do
    p <- pwd
    f
    mkdir filename
    cd filename
    if filetype == "latex" then mkuplatex filename else if filetype == "beamer" then mkbeamer filename else undefined
    mkdir "pandoc"
    cd "pandoc"
    touch "skelton.md"
    cd p

mkreading :: Turtle.FilePath -> IO()
mkreading filename = do
    p <- pwd
    toreading
    mkdir filename
    cd filename
    mkuplatex filename
    mkdir "pandoc"
    do  -- pandocディレクトリでの作業
        cd "pandoc"
        mkdir "documents"
        mkdir "img"
        touch "skelton.md"
    do  -- documents,imgディレクトリでの作業
        cd "documents"
        touch "url.md"
        cd ".."
        cd "img"
        touch "url.md"
    cd p

mkthinkingtempl :: Turtle.FilePath -> IO()
mkthinkingtempl filename = do
    p <- pwd
    mkdir filename
    cd filename
    mkdir "pandoc"
    cd "pandoc"
    do  p' <- pwd >> touch "Concept.md" >> mkdir "documents" >> mkdir "img" -- pandocディレクトリ内の動作
        cd "documents" >> touch "url.md" >> cd ".." --documentsディレクトリにurl.mdファイルを作成しpandocに戻る
        cd "img" >> touch "url.md" 
    cd p --最初にいたディレクトリに戻る

mkworksheet :: Turtle.FilePath -> Turtle.FilePath -> IO ()
mkworksheet = arrange toworksheets
    


-- パーサーと組み合わせて使うコンビネータ

  -- cutを使うとマッチングに空列が含まれることが多々あるのでそれを消す
delempty :: (Monoid a,Eq a) => [a] -> [a]
delempty = filter (/=mempty)

  -- skelton.mdの各大見出しを切り出して[Text]にして返す。# は除去
cutsharp :: Text -> [Text]
cutsharp = delempty . concatMap (cut "# ") . delempty . cut (option "\r" <> "\n")-- 二通りの改行文字に対応する必要あり

  -- cutsharpが返した[Text]を用いて、マークダウンファイルを複数生成する関数
  -- よくみるとリスト上のfoldの再帰である。mapM_を使った方がマナーが良いかも？
mkMDs_ :: [Text] -> IO ()
mkMDs_ [] = return ()
mkMDs_ (t:ts) = do
    let name = repSpaces.repHyphen $ t <> ".md" --ファイル名に限り、半角スペースとハイフンをアンダーバーに置換
        path = fromText name
    b <- testfile path -- 同じ名前のファイルがあるかチェック
    if b then mkMDs_ ts -- 同じ名前のファイルがあったら何もしない
         else do -- なければその名前のファイルを作る
        touch path
        writeTextFile path $ "# " <> t -- ファイルの冒頭に大見出しでタイトルを記入
        mkMDs_ ts

rmSideSpaces :: Text -> Text
rmSideSpaces = let r = do{spaces;c1 <- once(notChar ' ');t <- chars; c2 <- once (notChar ' '); spaces; return $ c1 <> t <> c2}
               in mconcat . match r -- 空白でない最初の文字と最後の文字は必ず一意に定まるので、mconcatを使っても安全

  -- skelton.mdから一連の.mdファイルを作る関数
  -- 
mkMDs :: Turtle.FilePath -> IO ()
mkMDs path = do
    t <- readTextFile path
    mkMDs_ . map rmSideSpaces . cutsharp $ t


--一般の置換関数
    --foldrのコールバック関数は、文末でマッチングした場合を取りこぼすので、改行文字を挿入して末尾でのマッチングを回避している
    --仮に改行文字をマッチングさせようとしていた場合、末尾で追加した改行文字が予想外の動作を起こす可能性はあるだろうか？考えるべし
    -- foldrが終わった後は、initで末尾に追加した改行文字を除去する
replace p t2 t = let ts = cut p (t<>"\n") in foldr (\x ys -> if ys == mempty then x <> ys else x <> t2 <> ys) mempty ts

    -- '''を```に置換する正規表現関数
repQuotations :: Text -> Text
repQuotations = replace (text "'''") "```"

    -- Pathを渡すとファイル中の'''を```に変換する関数
repQ :: Turtle.FilePath -> IO ()
repQ path = do
    t <- readTextFile path
    let t' = repQuotations t
    writeTextFile path t'

repSpaces :: Text -> Text
repSpaces = replace space "_"

repHyphen :: Text -> Text
repHyphen = replace (text "-") "_"

changeSpacesInFileName :: MonadIO io => Turtle.FilePath -> io ()
changeSpacesInFileName path = mv path . fromText . repSpaces . unwrap . toText $ path
    where unwrap x = case x of Left y -> y; Right z -> z

-- 現在のディレクトリのなかみ一覧表示
ls' :: MonadIO m => m ()
ls' = do
    p <- pwd
    view . ls $ p

code :: IO ExitCode
code = shell "code ." ""

push :: T.Text -> IO ExitCode
push m = shell "git add ." "" *> shell ("git commit -m " <> m) "" *> shell "git push origin master" ""

pull :: IO ExitCode
pull = shell "git pull origin master" ""
