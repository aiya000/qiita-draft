# Happyでパースエラーで列行番号とどんなミスマッチが起こったか報告する
## まとめ
　コード全文は結構長いのでこちら :point_down:

- [learning-Haskell/verbose_errors.y - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/3c9307776d0421a1aa3f1bea59adfd2593f519bc/Room/Happy/verbose_errors.y)

　重要なのは下記の2点です。

- lexerが`Token`と共に`TokenPos`を集積すること
- `%errorhandlertype explist`

## 仮定する知識

1. happyでパーサを書ける
1. レキサ、パーサが何をするものか、なんとなくだけどわかる
1. MonadTransとGeneralizedNewtypeDerivingでモナドを組み立てられる

- optional（わかるとソースが見やすい）
    1. lensが使える

## ここでしたいこと
　「Happyでパースエラーで列行番号とどんなミスマッチが起こったか報告する」とは？  
それは…

　happyはデフォルトで、詳細なパースエラーを表示するための機能を有効にしていません。

ですので、パースエラーが発生した際に以下のような情報量の少ない報告しかできません。  
これは`1 + 2 * 3`のような、
数値と`+`, `*`からなる入力を受理するパーサに、
異常な入力`1 2`を食わせた例です。

```
parse error with a token "2"
```

　しかし実用上、
どんな異常な入力が入力されて、そして
「本来どんな入力が期待されていたか」
「どの行の何文字目で異常な入力が行われたのか」
が必要だと思います。

例えば以下のように。

```
parse error at (Line=1, Column=3), 2 is got, but ['+', '*'] are expected.
```

　この記事ではそこまでの手立てを、
非負整数と`+`, `*`を受理する実際のコードの流れに沿ってまとめます :dog2:

　繰り返しになりますが、
コード全文は以下になります

:point_down: :point_down: :point_down:

- [learning-Haskell/verbose_errors.y - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/3c9307776d0421a1aa3f1bea59adfd2593f519bc/Room/Happy/verbose_errors.y)

:point_up: :point_up: :point_up:

## 解説
### おおまかな流れ

1. 入力をレキサに渡す `lexer :: String -> Processor [(Token, TokenPos)]`
1. レキサの結果をパーサに渡す `parser :: [(Token, TokenPos)] -> Processor Expr`
    - parserはhappyが生成します
1. パーサは正常な入力を受理し結果（`Expr`、もしくはパースエラーを返します `:: Either String Expr`

### 始まり
　やはり最初に見るのはmainですよね。

　mainはrunAppに各入力を渡しています。

```haskell
main :: IO ()
main = do
  -- This should be succeed
  print $ runApp "1 + 2 * (3 * 4)"
  -- These should be failed
  print $ runApp "1 + *"
  print $ runApp "10 20"
  print $ runApp "+"
```

runAppは入力をレキサに渡し、
さらにその結果をパーサに渡します。

```haskell
-- | Run 'lexer' and 'parser'
runApp :: String -> Either String Expr
runApp code = runProcessor $ lexer code >>= parser
```

runProcessorは、
lexerとparserの間の文脈を持つ`Processor`モナドを実行するものです。

`Processor`モナドは以下の文脈を保持します。

1. lexerにて、現在見ている位置情報（コード上での位置）を保持する（`MonadState TokenPos`）
1. lexer 、parserにて、もし異常な入力があった場合に異常値を返す（`MonadError String`）

```haskell
-- | A monad for between the lexer and the parser
newtype Processor a = Processor
  { unProcessor :: ExceptT String (State TokenPos) a
  } deriving ( Functor, Applicative, Monad
             , MonadError String
             , MonadState TokenPos
             )

-- | Run and extract a 'Processor'
runProcessor :: Processor a -> Either String a
runProcessor = unProcessor
               >>> (runExceptT :: ExceptT String (State TokenPos) a -> State TokenPos (Either String a))
               >>> (flip evalState initialPos :: State TokenPos (Either String a) -> Either String a)
  where
    -- All code starts with (1, 1)
    initialPos = TokenPos 1 1
```

これはhappyの`%monad { Processor }`に指定されていて、
各所…特に`parseError`を`a`から`Processor a`に持ち上げます。

`Token`はある文字またはひとかたまりの文字列を表すデータ型

```haskell
-- | A result of the lexer
data Token = TokenInt Int
           | TokenPlus
           | TokenTimes
           | TokenParensBegin
           | TokenParensEnd
  deriving (Show)
```

`TokenPos`は位置情報

```haskell
-- | A position of 'Token' on a code
data TokenPos = TokenPos
  { lineNum :: Int
  , colNum  :: Int
  } deriving (Show)
```

そして`Expr`は、このコードの最終結果を表す抽象構文木です。

```haskell
-- | A result of the parser
data Expr = ExprPlus Expr Expr
          | ExprTimes Expr Expr
          | ExprParens Expr
          | ExprInt Int
  deriving (Show)
```

### lexer
　lexerは文字を、
そのコード上の位置情報（`TokenPos`）と共に
トークン（`Token`）に変換しつつ、
現在位置を適切に加算していきます。

なおパターンマッチの`_`パターンの句は非負整数を変換しています。

```haskell
-- | Tokenize a code with the token position
lexer :: String -> Processor [(Token, TokenPos)]
lexer xs = do
  pos <- get
  case xs of
    "" -> pure []
    ('(':xs) -> do
      _colNum <+= 1
      ((TokenParensBegin, pos):) <$> lexer xs
    (')':xs) -> do
      _colNum <+= 1
      ((TokenParensEnd, pos):) <$> lexer xs
    ('*':xs) -> do
      _colNum <+= 1
      ((TokenTimes, pos):) <$> lexer xs
    ('+':xs) -> do
      _colNum <+= 1
      ((TokenPlus, pos):) <$> lexer xs
    (' ':xs) -> do
      _colNum <+= 1
      lexer xs
    ('\n':xs) -> do
      _lineNum <+= 1
      lexer xs
    _ -> do
      let ((y, ys):_) = lex xs
      case readMay y of
        Nothing -> throwError [i|fatal error! please open an issue with this message X( `couldn't read ${show y}`|]
        Just z  -> do
          _colNum <+= length y
          ((TokenInt z, pos):) <$> lexer ys
```

### parser
　ここが肝です。
parserはhappyによって記述されます。

```haskell
%name parser
%tokentype { (Token, TokenPos) }

%token
    int { (TokenInt $$, _)      }
    '+' { (TokenPlus, _)        }
    '*' { (TokenTimes, _)       }
    '(' { (TokenParensBegin, _) }
    ')' { (TokenParensEnd, _)   }

%left '*'
%left '+'

%%

Expr : Expr '+' Expr { ExprPlus $1 $3  }
     | Expr '*' Expr { ExprTimes $1 $3 }
     | '(' Expr ')'  { ExprParens $2   }
     | int           { ExprInt $1      }
```

`%tokentype { (Token, TokenPos) }`はparserとparseErrorがその列を引数として受け取ることを表します。
（`parser :: [(Token, TokenPos)] -> ?`）

`%token` はその引数の値がパーサ中でどのように表れるかを示します。

ただしここで`TokenPos`値が全て捨てられていることが重要です。
なぜなら今回、`TokenPos`はparseErrorによってのみ利用されるからです。

```haskell
%token
    int { (TokenInt $$, _)      }
    '+' { (TokenPlus, _)        }
    '*' { (TokenTimes, _)       }
    '(' { (TokenParensBegin, _) }
    ')' { (TokenParensEnd, _)   }
```

以下がパーサの実部です。
CFGとして見れるのではないかと思います。

この`Expr`パーサは推論によって戻り型が（データ型の方の）`Expr`であることを決定されます。

```haskell
Expr : Expr '+' Expr { ExprPlus $1 $3  }
     | Expr '*' Expr { ExprTimes $1 $3 }
     | '(' Expr ')'  { ExprParens $2   }
     | int           { ExprInt $1      }
```

#### parseError
　ここで異常な入力、
例えば`1 2`などであったときに
```
Expr '+' Expr -- "1 + 2"などを受理する
Expr '*' Expr -- "3 * 4"などを
'(' Expr ')'  -- "(5)"など
int           -- "10"な
```

その`1`は形式`int`によって受理されますが、
全体`1 2`は`int int`や`Expr int`, `Expr Expr`なのでいずれによっても受理されず、
消費されなかったトークン列`[TokenInt 2]`と共に
parseError関数が呼ばれます。

```haskell
-- | Throw a error with the reason (actual and expected values, where the parser is failed)
parseError :: ([(Token, TokenPos)], [String]) -> Processor a
parseError (((actual, pos):_), expected)
  = throwError [i|parse error at ${show $ pretty pos}, ${show $ pretty actual} is got, but ${show $ pretty expected} are expected.|]
parseError x
  = throwError [i|fatal error! please open an issue with this message X( `${show $ pretty x}`|]
```

#### %errorhandlertype explist について
　もし`%errorhandlertype explist`が指定されていなければ、
parseErrorの型は`[(Token, TokenPos)] -> Processor a`です。

`%errorhandlertype explist`はその引数に`[String]`を追加し、
`([(Token, TokenPos)], [String]) -> Processor a`にします。

`[String]`は「本来どんな入力が期待されていたか」です。

今回の入力`1 2`の`2`に対してparseErrorが呼び出さますが、
本来そこで期待されていたのは`+`と`*`です。

ですのでその場合に`[String]`には`['+', '*']`が渡されます。

### 以上
　以上、

- lexerが`Token`と共に`TokenPos`を集積すること
- `%errorhandlertype explist`

により、パースエラー（`parseError`）で下記のようなベーシックで実用的な情報を報告することができました。

```
parse error at (Line=1, Column=3), 2 is got, but ['+', '*'] are expected.
```

## まとめ
　レキサの側でトークンだけでなく、
元コード上でのそのトークンの位置情報を返すようにし、
パーサの側でパースエラーが起きたときにその位置情報を利用する。

　`%errorhandlertype explist`を指定することで`%error`に指定した関数で
「入力のうち消費できなかったもの（actual）」と共に
「ここで本来期待されていた入力（expected）」を受け取れるようにし、
パースエラーが起きたときにその情報を利用する。

　この2点によりパースエラーで、
parsec系のパーサコンビネータライブラリでパーサを手書きしたときと比べても
不足ないであろう情報量を報告することができました。

# 参考ページ

- [2.5. Monadic Parsers](https://www.haskell.org/happy/doc/html/sec-monads.html#sec-line-numbers)
- [sec-monads.html - Pac Learner](https://sites.google.com/site/paclearner/happy_jp/sec-monads-html)
- [haskell - How to get nice syntax error messages with Happy? - Stack Overflow](https://stackoverflow.com/questions/5430700/how-to-get-nice-syntax-error-messages-with-happy)
- [RFC: On parse error - show the next possible tokens by da-x - Pull Request #46 - simonmar/happy - GitHub](https://github.com/simonmar/happy/pull/46)

# 蛇足
　今回は簡単のためlexerとparserに共通のProcessorモナドを使用していますが、
練り込み度によりparserでは`MonadState TokenPos`を使用しない等の方法を取ることもできるかと思います。
