# Happyで詳細なエラーを表示する（行と列の番号,期待される字句）
## まとめ
　`%errorhandlertype explist`を指定し、
`State 現在の位置情報`モナドを用います。

- 前者について
    - [learning-Haskell/expected_tokens.y at 544245f1a960b9926f8662f65e590e731a320a40 - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/544245f1a960b9926f8662f65e590e731a320a40/Room/Happy/expected_tokens.y)
- 後者について
    - [learning-Haskell/line_column_numbers_on_an_error.y at 2c4805cdb471d492efa78dea4f96bda4f82f6619 - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/2c4805cdb471d492efa78dea4f96bda4f82f6619/Room/Happy/line_column_numbers_on_an_error.y)

## 概要
　`1 + 2`, `3 * (2 + 1)`のような、
自然数と`*`, `+`からなる入力を受理するのようなパーサをベースに、
それぞれの機能を追加します。

## パースエラー時に「指定されたトークン」と「本来期待されていた字句」を表示する

- 具体的なコード
    - [learning-Haskell/expected_tokens.y at 544245f1a960b9926f8662f65e590e731a320a40 - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/544245f1a960b9926f8662f65e590e731a320a40/Room/Happy/expected_tokens.y)

　パースエラー時に`{actual-wrong-token} is taken, but {expected-correct-tokens} are expected`みたいなやつを表示させます。

これはおおよそ、happyのファイル（.y or .ly）に`%errorhandlertype explist`を指定するだけで済みます。

　`%errorhandlertype explist`を指定すると、
`%error`に指定したparseError関数 (`%error { parseError }`) に求められる型が
`[Token] -> a`から`([Token], [String]) -> a`に変わります。

ここで`[String]`は「次に期待されていたトークンのリスト」
（またhappyがデフォルトで要求する`[Token]`は「入力のうち未だ解析していない（されなかった）、トークンの残り分）
ですので「次の入力としてysを期待していたが、xが入力された」ということがパースエラーで報告できるようになります。

例
```haskell
...
%error { parseError }
...
{
parseError :: ([Token], [String]) -> a
parseError (causeOfFail:_) expected = error $ show causeOfFail ++ " is taken, but " ++ show expected ++ " are expected"
parseError [] _ = error "a really unexpected condition was detected"

...
}
```

（ちなみにですが`QuasiQuotes`拡張とhereパッケージを使うとstring interpolationsで綺麗に書けます。
```haskell
{-# LANGUAGE QuasiQuotes #-}

import Data.String.Here (i)

parseError :: ([Token], [String]) -> a
parseError (x:_) ys = error [i|${show x} is taken, but ${show ys} are expected|]
parseError []    _  = error "a really unexpected condition was detected"
```
）

　これで、異常な入力`"10 20 30"`に対するパースエラーの内容が :point_down: のようになります。

```
explist.hs: Parse error, [TokenInt 20,TokenInt 30] is taken, but ["'+'","'*'"] are expected
CallStack (from HasCallStack):
error, called at expected_tokens.hs in main:Main
```

## エラーが発生した箇所（問題のトークンがある場所）を表示する

- 具体的なコード
    - [learning-Haskell/line_column_numbers_on_an_error.y at 2c4805cdb471d492efa78dea4f96bda4f82f6619 - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/2c4805cdb471d492efa78dea4f96bda4f82f6619/Room/Happy/line_column_numbers_on_an_error.y)

　:point_down: みたいなパースエラーを表示するようにします。

```
example.hs:2:1: parse error, ...
           ^ ^
        こんなやつ
```

　これは[公式](https://sites.google.com/site/paclearner/happy_jp/sec-monads-html)でも解説されていますが、
説明があっさりしすぎていてわからなかった :sob:

これは以下の2点に要約できます。

1. レキサ（字句解析器）にトークンと一緒にトークンの位置情報も返す
2. happyの側のトークン宣言にて、トークンと位置情報のペアを期待する

### 1について
　パーサは、parseError関数以外への関心は必要なく、
レキサに重要性がかかっています。

```haskell
type Parser a = State TokenPos a

data Token = TokenInt Int
           | TokenPlus
           | TokenTimes
           | TokenParensBegin
           | TokenParensEnd
  deriving (Show)

data TokenPos = TokenPos
  { colNum  :: Int
  , lineNum :: Int
  } deriving (Show)

_colNum :: Lens' TokenPos Int
_colNum = lens colNum $ \pos n -> pos { colNum = n }

_lineNum :: Lens' TokenPos Int
_lineNum = lens lineNum $ \pos n -> pos { lineNum = n }

parseError :: [(Token, TokenPos)] -> Parser a
parseError xs = do
  let TokenPos c l = snd $ head xs
  error [i|Parse error at (${show c}, ${show l}) with ${show $ map fst xs}|]

lexer :: String -> Parser [(Token, TokenPos)]
lexer xs = do
  pos <- get
  case xs of
    "" -> pure []
    ('(':xs) -> do
      _colNum <+= 1
      ((TokenParensBegin, pos):) <$> lexer xs
    ...
```

### 2について
　parseError以外で位置情報は必要ないので、
`_`パターンで握りつぶしてしまいます。

`%token`の受け取る各トークンの右項はパターンなので、
`_`が使えます。

```haskell
%token
    int { (TokenInt $$, _)      }
    '+' { (TokenPlus, _)        }
    '*' { (TokenTimes, _)       }
    '(' { (TokenParensBegin, _) }
    ')' { (TokenParensEnd, _)   }
```

…

以上です！

- - -

## 参考ページ

- [haskell - How to get nice syntax error messages with Happy? - Stack Overflow](https://stackoverflow.com/questions/5430700/how-to-get-nice-syntax-error-messages-with-happy)
- [Toward better GHC syntax errors - Dan Aloni](http://blog.aloni.org/posts/toward-better-ghc-syntax-errors/)
    - `%errorhandlertype explist`について書かれてなくて難しかった
- [RFC: On parse error - show the next possible tokens by da-x - Pull Request #46 - simonmar/happy - GitHub](https://github.com/simonmar/happy/pull/46)
- [sec-monads.html - Pac Learner](https://www.haskell.org/happy/doc/html/sec-monads.html#sec-line-numbers)
- [sec-monads.html - Pac Learner 日本語](https://sites.google.com/site/paclearner/happy_jp/sec-monads-html)
