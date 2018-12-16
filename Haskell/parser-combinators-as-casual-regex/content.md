# 「正規表現はあんまり使わないかな。パーサー使うから」っていう人の気持ちがわかった！

皆さん、[Happy メリー Haskell クリスマス](https://qiita.com/advent-calendar/2018/haskell2) :snowman:
今日は文字列検索等で、正規表現ライブラリではなくパーサーコンビネーターライブラリを使う人（僕）の気持ちを理解してもらおうと思います！

![ペロくん](pero.jpg)

## 用語

本記事での「正規表現（ライブラリ）」とは以下のような、文字列リテラル（または文字列ライクなリテラル）で書かれた表現を解釈してから処理をする方式を指します。

（Haskellの例）

```haskell
>>> import Text.Regex.Posix
```

```haskell
>>> "abc" =~ "b" :: Bool
True
>>> "abc" =~ "b" :: Int
1
>>> "abc" =~ "b" :: String
"b"
```

またパーサーコンビネーターとは以下のような、正規表現と比べて文字列によらず表現を書き、処理をする方式を指します。

（Haskellの例）

```haskell
>>> import Data.Void (Void)
>>> import Text.Megaparsec (Parsec, parseTest, parse)
>>> import qualified Control.Applicative as P
>>> import qualified Text.Megaparsec.Char as P
>>> import qualified Text.Megaparsec.Char.Lexer as P hiding (space)
```

```haskell
>>> :{
    parser :: Parsec Void String (String, Int)
    parser = do
      hi <- P.many P.alphaNumChar
      P.space
      _10 <- P.decimal
      pure (hi, _10)
:}

>>> parse parser "no-name" "hi 10"
Right ("hi",10)
```

## なぜ正規表現でなくパーサー（コンビネーター）を使うのか

パーサーコンビネーターは正規表現の上位互換（※）であり、可読性が高いからです。
またそのプログラミング言語が静的型付きであれば、パーサーコンビネーターには静的型検査が加わります。
（正規表現は文字列リテラルを用いるので、その多くの場合、静的型検査は加わらない。）

※
実際にパーサーコンビネーターが正規表現のスーパーセットであるかは不明。
ある場合に正規表現を使いたいときに、そこでパーサーコンビネーターを使うことができる……程度の意味合い。

:metal::relieved::metal:

体感してみましょう。
僕の.ctags.d/kotlin.ctagsの一部を見てみましょう。

これはclassの検知をするための正規表現です。

```
/^[ \t]*(private|protected|public)?[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(data[ \t]*)?class[ \t]+([a-zA-Z0-9_]+)/\8/
```

……？？？ :thinking:
**激ヤバ**なのがわかるかと思います。

（もっといいctagsの書き方あったりしませんか？）

助けてﾊﾟｯｻｺﾝﾋﾞﾈｰﾖーー！！

```haskell
```

ｱｯｳｳｳｳｰﾝ!!
パーサーコンビネーター使いてえー！！！

1. なぜならパーサとは単純に正規表現を内包していて、それでいて型安全だからだ！

## 正規表現の問題

2. 文字列の正規表現による（わかりにくく誤った）例

## そのboost::xpressive (C++) による解決

3. boost::xpressiveによる上記例の修正

## boost::xpressiveって（Applicativeな）パーサーコンビネーターですよね

4. 3をHaskellの (Applicative) パーサに直す

## まとめ

なぜ正規表現でなくパーサーコンビネーターを使うのか？

1. 静的な誤りチェックが入るから
2. 読みやすいから

![ペロララ](pero-and-lala.jpg)
