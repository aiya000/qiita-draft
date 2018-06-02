# Haskellでコンパイル時に「(ファイル名):L(現在の行）」を埋め込む

# 結論
　あるファイルでそのコードの場所を文字列として埋め込みたい場合は、このように実現することができます。

```haskell
let here = __FILE__ ++ ":L" ++ show (__LINE__ :: Int)
```

（コンパイル時ではなくプリプロセス時っぽそう）

# 起
　Haskellでは、関数が単純に例外を出す可能性がある場合は`Either e`もしくは`Maybe`を使うと思います。

```haskell
readFooFromSomewhere :: Bar -> Either Text Foo
readFooFromSomewhere bar =
  let result = {a result of a some calculation}
  case result of
       Baz -> Left "残念"
       Poi -> Right {some}
```

　これに加え`IO`文脈下では関数`fail`などによって、`Left e`や`Nothing`でない、マジモンの例外を出すことができます。
（危険、多用厳禁。`main`関数以外での使用を推奨しません :sob:）

`fail`は主に非正常系で使われると思います。

```haskell
readFooFromSomewhere :: IO ()
readFooFromSomewhere =
  result <- {a result of a some calculation}
  case result of
       Baz -> fail "残念"
       Poi -> return {some}
```

　`fail`が使われる場所はだいたい非正常系の処理だと思いますので通常は現れないはずですが、
もし現れた場合にissueで報告して欲しい場合はこんな感じにすると思います。

```haskell
readFooFromSomewhere :: IO ()
readFooFromSomewhere =
  result <- {a result of a some calculation}
  case result of
       -- vvv ここは絶対に通らないよ！もしここを通ったら木の下に埋めてもらっても構わないよ！
       Baz -> fail "Main.readFooFromSomewhere: fatal error! sorry, please report an issue if you see this :("
       -- ^^^
       Poi -> return {some}
```

# 承
　でもいちいち`"Main.readFooFromSomewhere"`とか書くのは面倒なので、どうせならコンパイル時とかプリプロセス時あたりで、自動で埋め込んで欲しいと思うのは自然なことですよね。

それについて選択肢はいくつか考えられます。

- プログラムを`--profile`おプション付きGHCでビルドして、[GHC.Stack.HasCallStack](https://www.stackage.org/haddock/lts-11.7/base-4.10.1.0/GHC-Stack.html#t:HasCallStack)と`currentCallStack`を使う
- TemplateHaskellを使う
- CPPを使う

　今回はCPPを使ってみます。

```haskell
{-# LANGUAGE CPP #-}

readFooFromSomewhere :: IO ()
readFooFromSomewhere =
  result <- {a result of a some calculation}
  let here = __FILE__ ++ ":L" ++ show (__LINE__ :: Int)
  case result of
       Baz -> fail $ here ++ " : fatal error! sorry, please report an issue if you see this :("
       Poi -> return {some}
```

できました。

# 参考

- [code generation - Haskell equivalent of C's \_\_LINE\_\_ - Stack Overflow](https://stackoverflow.com/questions/2314110/haskell-equivalent-of-cs-line)
