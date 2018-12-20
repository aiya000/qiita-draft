# 「正規表現はあんまり使わないかな。パーサー使うから」っていう人の気持ちがわかった！

皆さん、[Happy メリー Haskell クリスマス](https://qiita.com/advent-calendar/2018/haskell2) :snowman:
アドベントカレンダーお疲れさまでした :tada: :tada:

- - -

今日はズバリ「人はなぜ、パーサーに惹かれるのだろうか？」ということを追ってみます！
（もとい文字列検索等で正規表現ライブラリではなく、パーサーコンビネーターライブラリを使うようになった人（僕）の、それまで道筋を。）

![ペロくん](pero.jpg)

以下、筆者の私感による説明になります。

## この記事の対象者 / 非対象者
### この記事の対象者

- パーサーコンビネーターが難しいものだと思っている人
- パーサーコンビネーターをカジュアルに使ってみたい人

### この記事の非対象者

- パーサーコンビネーターの入門をしたくてこの記事にたどり着いた人

すみません、この記事は具体的な入門を促す記事ではありません。
参考までに……
僕はこの本でパーサーコンビネーターへの入門を果たしました :point_down:

- [プログラミングHaskell](https://www.amazon.co.jp/%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0Haskell-Graham-Hutton/dp/4274067815)

## なぜ正規表現でなくパーサー（コンビネーター）を使うのか（結論）

1. 可読性が高くなりやすい
    - メンテナブルになりやすい
2. 静的型検査が付くから
    - （その言語が静的型付けなら）
3. 正規表現と比べて扱いにくいということがない
    - 利点しかない

僕が正規表現と比べてパーサーを好むのは、**可読性が高い**（メンテナブルになりやすい）からです。

またそのプログラミング言語が静的型付きであれば、パーサーコンビネーターには静的型検査が加わるからです。
（正規表現で文字列リテラルを用いる場合の多くは、静的型検査は加わらないと思います。）

## 用語（前置き）

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

## 正規表現の困難（本編）

このひどい正規表現を見てください。
これは僕のKotlinのclass名の検知をするための正規表現です :thinking:

（[.ctags.d/kotlin.ctags](https://github.com/aiya000/dotfiles/blob/1c04565351cae71cffa2990842b156dac28fcd45/.ctags.d/kotlin.ctags)の一部）

```
/^[ \t]*(private|protected|public)?[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(data[ \t]*)?class[ \t]+([a-zA-Z0-9_]+)/\8/
```

……？？？ :thinking:
激ヤバなのがわかるかと思います。

### 正規表現コンビネーター（？）による救済

その救済の一つとして、**静的型付き**言語C++の`boost::xpressive`が思い浮かびます。
やってみましょう。

[コード全体](https://gist.github.com/4e7601d03623a079e3c8632eb13c5b07)

```cpp
namespace xp = boost::xpressive;

int main() {
    // 基本コンビネーター
    xp::sregex empty = xp::sregex::compile("");
    xp::sregex blank = *xp::space;
    // Converts a string to xp::sregex
    auto let = [empty](std::string x){
        return x >> empty;
    };

    // 目的たち
    xp::sregex visibility =
        (let("private") | "protected" | "public")
        >> blank
        ;

    xp::sregex kind =
        (let("abstract") | "final" | "sealed")
        >> blank
        ;

    xp::sregex class_ =
        blank
        >> !visibility
        >> !kind
        >> !let("data") >> blank
        >> "class" >> blank
        >> (xp::s1 = *(xp::alnum | '_'))
        >> *xp::_
        ;

    // Go!!
    xp::smatch what;
    std::string x = "public data class You(val me: String)";
    std::string y = "sealed class Product {}";

    if (xp::regex_match(x, what, class_)) {
        std::cout << what[1] << '\n';
    }
    if (xp::regex_match(y, what, class_)) {
        std::cout << what[1] << '\n';
    }
}
```

出力

```
You
Product
```

やったー！
**メンテナブル**ー！！

## パーサーコンビネーターによる救済

そこで貴方は思います。
「`boost::xpressive`もパーサーコンビネーターも、書く難易度は変わらないのではないか」と。

今度はパーサーコンビネーターで、また同じものを実装してみましょう。

[コード全体](https://github.com/aiya000/learning-Haskell/blob/a4397130c901749ab75b6a8310d343f6be8d433b/Text/Megaparsec/parse-kotlin-classes.hs)

```haskell
type Parser = Parsec Void String

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

parseClass :: Parser String
parseClass = do
  P.optional $ token parseVisibility
  P.many $ token parseClassKind
  P.optional . token $ P.string "data"
  token $ P.string "class"
  name <- P.many $ P.alphaNumChar <|> P.char '_'
  P.many P.anyChar
  pure name
```

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

そう、パーサーコンビネーターはカジュアルに使えるほど、手軽なものだったのです！

## まとめ

文字列での正規表現の表現よりも、正規表現コンビネーターやパーサーコンビネーターによる表現の方が可読性が高く、メンテナンスしやすいことがわかりました。
また静的型付き言語C++・Haskellではその表現を静的に検査することができました。
さらにパーサーコンビネーターはとてもカジュアルに使えるものだということがわかりました！

![ペロララ](pero-and-lala.jpg)

メリークリスマス :snowboarder:
