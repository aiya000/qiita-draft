# 「正規表現はあんまり使わないかな。パーサー使うから」っていう人の気持ちがわかった！

皆さん、[Happy メリー Haskell クリスマス](https://qiita.com/advent-calendar/2018/haskell2) :snowman:
アドベントカレンダーお疲れさまでした。

- - -

今日は文字列検索等で正規表現ライブラリではなく、パーサーコンビネーターライブラリを使うようになった人（僕）の、それまで道筋を描いてみます。
そう、パーサーコンビネーターを使いたがる人は決して異常者ではないのです！

![ペロくん](pero.jpg)

## 前置き（用語）

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

それは**可読性が高い**からです。
またそのプログラミング言語が静的型付きであれば、パーサーコンビネーターには静的型検査が加わるからです。

正規表現で文字列リテラルを用いる場合の多くは、静的型検査は加わらないでしょう。

可読性の高さを感じてもらうために、僕の.ctags.d/kotlin.ctagsの一部を見てみます。
これはKotlinのclass名の検知をするための正規表現です。

```
/^[ \t]*(private|protected|public)?[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(data[ \t]*)?class[ \t]+([a-zA-Z0-9_]+)/\8/
```

……？？？ :thinking:
**激ヤバ**なのがわかるかと思います。
正直僕も、何を言っているかわからないです……。

ここで「彼」に助けを呼んでみます。
助けてﾊﾟｯｻｺﾝﾋﾞﾈｰﾖーー！！

（Haskellのmegaparsecでの例）

```haskell
type Parser = Parsec Void String

-- 💛💚💙
parseClass :: Parser String
parseClass = do
  P.optional $ token parseVisibility
  P.many $ token parseClassKind
  P.optional . token $ P.string "data"
  token $ P.string "class"
  name <- P.many P.alphaNumChar
  P.many P.anyChar
  pure name

parseVisibility :: Parser String
parseVisibility =
    P.string "private" <|>
    P.string "protected" <|>
    P.string "public"

parseClassKind :: Parser String
parseClassKind =
    P.string "abstract" <|>
    P.string "final" <|>
    P.string "sealed"

blank :: Parser String
blank = P.many P.spaceChar

token :: Parser String -> Parser String
token parseWord = do
  blank
  word <- parseWord
  blank
  pure word
```

おお、メンテナブルなコードができました！

確認

```haskell
main :: IO ()
main = do
  let parseTest' = parseTest @Void @String
  parseTest' parseClass "public data class You(val me: String)"
  parseTest' parseClass "sealed class Product {}"
```

出力

```
"You"
"Product"
```

ｱｯｳｳｳｳｰﾝ!!
文字列検索にはパーサーコンビネーター使いたいーﾝｵｵｵｵ!

### 正規表現コンビネーター（？）

ここまでで「文字列での正規表現の表現」に限界があることがわかりました。
でもいきなりそこでパーサーコンビネーターを持ち出すのはちょっと……ね？

うう〜ん表現が文字列じゃなくて、コンビネーターな正規表現ライブラリがあると、ちょうどいいんだけど。
そうboost::xpressiveのことです！

## そのboost::xpressive (C++) による解決

3. boost::xpressiveによる上記例の修正

## boost::xpressiveって（Applicativeな）パーサーコンビネーターですよね

4. 3をHaskellの (Applicative) パーサに直す

## まとめ

なぜ正規表現でなくパーサーコンビネーターを使うのか？

1. 静的な誤りチェックが入るから
2. 読みやすいから

![ペロララ](pero-and-lala.jpg)
