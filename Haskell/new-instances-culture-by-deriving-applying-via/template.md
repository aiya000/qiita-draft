# Haskellの「import-hiding-instaces問題」と「newtype-instance文化」

TODO: まとめ

## **import hiding instances問題**

現在のHaskell（GHC）では、`instance`へのimport-hidingができません。
例を見てみましょう。

```haskell
module Data.Meiwaku where

data Meiwaku = Meiwaku

-- 絶対にimportしたい！　超便利な関数。
veryVeryUseful :: Meiwaku -> Meiwaku
veryVeryUseful Meiwaku = Meiwaku

-- 絶対にimportしたくない！　突然の()インスタンス。
instance Semigroup () where
  _ <> _ = ()
```

```haskell
module Main where

import Data.Meiwaku (Meiwaku(..), veryVeryUseful)

main :: IO ()
main = do
  print $ veryVeryUseful Meiwaku
  print $ () <> ()
```

これをコンパイルすると、コンパイルエラーが起きるでしょう。
`instance Semigroup ()`が、Data.Semigroupで定義された`instance Semigroup ()`と重複しているせいです。

そう。
`import Data.Meiwaku (veryVeryUseful)`は、はた迷惑な`instance Semigroup ()`をもimportしてしまうのです。

しかしながら`veryVeryUseful`は絶対に使いたい。
ならば`import Data.Meiwaku hiding (instance Semigroup ())`するのがよいでしょう。

でもそれは、現在のHaskellではできません！

これをこの記事では「**import hiding instances問題**」と呼びます。

## import hiding instances問題との出会い

もう少しだけ、リアルワールド寄りな例を見てみます。

あなたは今、あるライブラリfooの作者です。

まずはその主要な機能であるデータ型Fooを定義します。

```haskell
!INCLUDE "./bad/Data/Foo.hs"
```

次に２つの、そのSemigroupインスタンスを定義します。


```haskell
!INCLUDE "./bad/Data/Foo/Baring.hs"
```

```haskell
!INCLUDE "./bad/Data/Foo/Baring.hs"
```

完成！
では、動作確認をしてみましょう。

```haskell
!INCLUDE "./bad/Main.hs"
```

```haskell
!INCLUDE "./bad/result.log"
```

ｱｲｴｴｴｴｴｴｴ!? ｴﾗｰ!?? ｴﾗｰ ﾅﾝﾃﾞ!?!?

どうやら`Data.Foo.Baring`及び`Data.Foo.Bazing`の2箇所で`instance Semigroup Foo`を定義してしまったことが問題になっているようです。
……ちゃんと選択的に、`veryVeryBenriBaring`と`veryVeryBenriBazing`だけimportしたはずなのに！？

そう、「import hiding instances問題」です。

### newtype-instance文化を導入する

では、礼節のあるHaskell文化にならい、`newtype`を使って解決しましょう。

この文化は`Data.Semigroup`の`Sum`や`Product`等で使われています。

- `instance Num a => Semigroup (Sum a)`
- `instance Num a => Semigroup (Product a)`

```haskell
!INCLUDE "./fixed/Data/Foo/Baring.hs"
```

```haskell
!INCLUDE "./fixed/Data/Foo/Bazing.hs"
```

BaringとBazingでも、Fooの便利な性質（ここでの`Eq`）を使いたいので、`DerivingVia`を用いています。
`DerivingVia`につきましては、下記のスライドのDerivingViaセクションをご覧ください。

- [「しんさんきぼう」のDerivingストラテジー](https://aiya000.github.io/Maid/haskell-day-2019-deriving)

```haskell
!INCLUDE "./fixed/Main.hs"
```

```haskell
!INCLUDE "./fixed/result.log"
```

これで、望んだ挙動を持つ`instance Semigroup Baring`・`instance Semigroup Bazing`を定義・利用することができました。
かつ、必要のない`Baring (..)`は、ちゃんとimportから除外されています！

このように、instanceの重複を避けるために、そのnewtypeにinstanceを定義することを、ここでは「**newtype-instance文化**」と呼びます。

### おまけ - ApplyingVia

でも私達が本当に作りたかったのは、`Semigroup Baring`と`Semigroup Bazing`という2つの型のインスタンスじゃなくて、ただひとつの型`Foo`への2つのインスタンスだったような？
そこで`ApplyingVia`です。

`ApplyingVia`拡張を用いると、下記のように、`Foo`の値を直接操作することができます。

```haskell
!INCLUDE "./fixed/Main2.hs"
```

それでは実行してみましょう……。

```haskell
!INCLUDE "./fixed/result2.log"
```

はい、すみません。
`ApplyingVia`はまだGHCに、マージされていない状態のようです。

- [-XApplyingVia — sort @(Int via Down Int) by Icelandjack - Pull Request #218 - ghc-proposals/ghc-proposals - GitHub](https://github.com/ghc-proposals/ghc-proposals/pull/218)

こうご期待。

## 「import-hiding-instaces問題」「newtype-instance文化」とは

TODO: まとめ
