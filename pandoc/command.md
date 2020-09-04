 2020/08/17-18

なるほど、確かにVimは面白いかもしれない。アウトラインプロセッサ式の書き方ならば脳内レンダリングも難しくはない。
## メモ、考察
- コマンドラインから引数を受け取る方法の一つ

    - runghc (ファイル名).hs （引数の列、空白区切り）
        - ファイルのあるディレクトリでなければ呼び出せない
    
-   Haskell製プログラムをbashから呼び出して使うには？

    - Pandocのように、どこからでも呼び出せるようにするにはどうすれば良いか？

    - 現状、LaTeXのプリアンブルのテンプレを指定するとそれで初期化してファイル生成(.tex)してくれるプログラムを作ろうと思っている。

        - pandocでMarkdownから変換したLaTeXの断片にプリアンブルを取り付けるプログラムとかも。
    
    - とりあえず、ファイルを読み込んでプリアンブルとdocument環境をつけるプログラムでも作るか。

- pandocってhomebrewでインストールしたっけ？だとしたらそこら辺にヒントがあるかも。

- Turtleを用いて、HaskellによるShellプログラミングを行うという選択肢を考える。

    - Haskellで詳細まで書けるならば、例えばどこのディレクトリにいようともlatexsディレクトリのNotesディレクトリのTaskReportディレクトリに新規LaTeX用ディレクトリを生成し、その中に.texファイルをプリアンブル付きで生成して、おまけにpandocディレクトリを生成するなどの定型操作ができるようになるのではないか？

- とりあえず、

    - LaTeXのプリアンブルを入れた新規ファイルの生成

    - Notes,TaskReports,haskell_testing,nodejs等、よく使うディレクトリに一発で飛ぶためのコマンド

    - 正規表現をワンライナーで渡して適用させるコマンド
あたりを作ることを最初の目標にしたい。

### VSCodeをTurtle関数で開けるか？

- LaTeXsディレクトリに行き、特定のディレクトリをワークスペースとしてVSCodeを開いてくれるコマンドラインを作れるか？

    - あるいは、GHCi上でVimを開けるか？(笑)

    - VSCodeのPathを通すことで、codeというコマンドでbashからVSCodeを開くことは可能になった。次はTurtleでcodeコマンドを実行する方法を探すべし。

### LaTeXのプリアンブルのテンプレートの扱いについて

- プリアンブルのテンプレはテンプレ保管用ディレクトリを用意しておき、そこに移動して指定したテンプレを取得してくるシェルスクリプトを書けば良いだろう。

- プリアンブルの種類は現状、

    - tikzandbussproofs

    - beamer

  の2種類しかない模様。

- 取得する方法は、cp(copy)関数か、またはreadTextFileとwriteTextFile関数を使うという選択肢が考えられる。


## LaTeXファイルとpandocディレクトリをセットで生成するためのコマンドライン関数作り


### 特定のディレクトリに一発で移動するコマンドライン関数の書き方

- Readingに新ディレクトリを作り、その中に移動して.texファイルをプリアンブル付きで作り、pandocディレクトリを作り、skelton.mdを作り、元にいたディレクトリに戻る関数は作れるか？

- とりあえずReadingディレクトリまで移動するコマンドライン作った。
    ```haskell
    gohome :: IO ()
    gohome = do
        p <- home
        cd p
    
    toReading :: IO ()
    toReading = do
        gohome
        cd "documents/latexs/notes/readings"
    ```

    - 同じ要領でディレクトリ移動は全部作れるはず。方針は
        1. homeディレクトリに飛ぶ
        2. そこからcdでPathを降りていく

### 現在のディレクトリに新しいディレクトリを作る
    
- 今いるディレクトリにpandocというディレクトリを作るには？

    - mkdir関数を使う。FilePathを引数に取るものだが、実際には作ろうとしているディレクトリ名だけ書けば自動で絶対パスに補正してくれる。

        - OverloadedStringsの下で
        ```haskell
        mkdir "pandoc"
        ```
        と打つだけで良い。

- 今いるディレクトリにpandocというディレクトリを作り、その中にskelton.mdというファイルを生成して元のディレクトリに戻るコマンドライン関数
    
    -
     ```haskell
    setskelton :: IO ()
    setskelton = do
        mkdir "pandoc"
        cd "pandoc"
        touch "skelton.md"
        cd ".."
    ```

### 新しいファイルを生成するコマンド関数

- 現在いるディレクトリに新しいファイルを生成する関数

    - 
    ```haskell
    touch "名前"
    ```
    で良い。

    - 名前を渡すと、その名前のディレクトリをReadingディレクトリに作成し、その中にmain.texファイルとpandocディレクトリを作り、pandocディレクトリにskelton.mdファイルを作成して元のディレクトリに戻る関数

    ```haskell
    mkreading    :: Turtle.FilePath -> IO ()
    mkreading xs = do
        p <- pwd --最後に戻るために現在地を持っておく
        toReading
        mkdir xs
        cd xs
        touch "main.tex"
        mkdir "pandoc"
        cd "pandoc"
        touch "skelton.md"
        cd p
    ```

- 例外を検出すると、作業途中でプログラムが中断される。その際、中断されるまでに作成したディレクトリやファイルは残存してしまう。これを回避するためにはどうすればいいか？

    - いわゆる継続モナドってやつを使って、エラーを検出したらそれまでの実行をなかったことにするのが正統だろうか？

    - それとももっと簡単に解決する方法があるか？


## 実装

とりあえずコンソールからrunghcでプログラムを実行し、付随した文字列をputStrする関数を作って実行した。

```haskell
module Main where

import System.Environment

main :: IO ()
main = do
    n <- getProgName
    args <- getArgs
    putStr $ n ++ concat args 

```

## ToDo

- 次はどうしたものか。コマンドラインから受け取った文字列に応じて、実行する関数が変わるプログラムにしてみるか？例えばlatexを受け取ったら1+1をprintし、beamerを受け取ったら"beam"をputStrする関数とか。

    - Turtleのチュートリアルを触ってみよう。そこで自分の知りたかったことがわかるかもしれない。

    - TurtleとSystem.Environment他が連携することがあるか考えてみるか。

    - GHCiを起動せずに、コマンドラインからそれらのプログラムを動かす方法があるか観察してみよう。


- Turtle.Patternのコンビネータで正規表現が忠実に再現されてるようなので、これを利用することを考える。ただし見る限りマッチングがメインで、置換などの操作は提供されていなさそうなので、その部分は自作する必要がありそうだ。

# 2020/08/20

