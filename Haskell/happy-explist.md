# Happyで（若干）詳細なエラーを表示する（explist）
## explist
`%errorhandlertype explist` :point_left: 大事

　.yファイルに`%errorhandlertype explist`を追加すると、
`%error { parseError }`のように指定したparseError関数に求められる型が
`[Token] -> a`から`([Token], [String]) -> a`に変わります。

ここで`[String]`は「次に期待されていたトークンのリスト」
（またhappyがデフォルトで要求する`[Token]`は「入力のうち未だ解析していない（されなかった）、トークンの残り分）
ですので「次の入力としてxsを期待していたが、iが入力された」ということがパースエラーで報告できるようになります。

## コード全文

- [learning-Haskell/explist.y at 1b668c67bcbaa7688fcdcb02cd4da1b3652200d5 - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/1b668c67bcbaa7688fcdcb02cd4da1b3652200d5/Room/Happy/explist.y)

:point_up:

　このパーサは`1 + 2`, `3 * (2 + 1)`のようなコードを解析するもので、
main関数中の`calc $ lexer "10 20 30"`でパースエラーを引き起こしています。

これで、パースエラーの内容は :point_down: のようになります。

```
explist.hs: Parse error, [TokenInt 20,TokenInt 30] is taken, but ["'+'","'*'"] are expected
CallStack (from HasCallStack):
error, called at explist.hs|180| 23 in main:Main
```

## 補足
　`%monad { Either String }`などと組み合わせるとなおよいと思います。

## 残念
### 行番号（列番号）について
```
example1.hs:2:1: parse error, ...
            ^ ^
         こんなやつ
```

　パースエラー時にこれを取得する方法は[ここ](https://sites.google.com/site/paclearner/happy_jp/sec-monads-html)
で解説されているものの、話がよくわからず。

誰か解説して欲しい。

　「シンプルですね。 Happy 自身の構文解析器のソースが手元にあれば、ソースを見てください。 構文解析器はまさに :point_down: のようなモナドを使用しています。」

```haskell
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a
```

と書いてあったものの、現在はuncurryされた

```haskell
type ParseResult = Either String
type P a = ReaderT (String, Int) ParseResult a
```

が用いられており、わからなかった。

## 参考ページ

- [haskell - How to get nice syntax error messages with Happy? - Stack Overflow](https://stackoverflow.com/questions/5430700/how-to-get-nice-syntax-error-messages-with-happy)
- [Toward better GHC syntax errors - Dan Aloni](http://blog.aloni.org/posts/toward-better-ghc-syntax-errors/)
    - `%errorhandlertype explist`について書かれてなくて難しかった
- [RFC: On parse error - show the next possible tokens by da-x - Pull Request #46 - simonmar/happy - GitHub](https://github.com/simonmar/happy/pull/46)
- [sec-monads.html - Pac Learner](https://www.haskell.org/happy/doc/html/sec-monads.html#sec-line-numbers)
- [sec-monads.html - Pac Learner 日本語](https://sites.google.com/site/paclearner/happy_jp/sec-monads-html)
