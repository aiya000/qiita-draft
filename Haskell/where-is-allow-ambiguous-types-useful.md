# AllowAmbiguousTypesはどこで使える機能なの？ 具体例編
# 今回の例: ScopedTypeVariables、TypeApplicationsとの組み合わせ

何か全然よくわからない、
そういえばHaskell初心者だった頃によくGHCサジェストされた気がする
`AllowAmbiguousTypes`プラグマですが、
初めて「お前を使わせてくれ！」っていう場面に遭遇しました。

こちらです :point_down:

- - -

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

check :: forall a. (Group a, Eq a) => Bool
check = (empty :: a) <> empty == empty

main :: IO ()
main = do
  smallCheck 2 $ check @Sum
  smallCheck 2 $ check @RSum
```

```haskell
newtype Sum = Sum
  { unSum :: Int
  } deriving (Show, Eq, Num, Enum)

newtype RSum = RSum
  { unRSum :: Rational
  } deriving (Show, Eq, Num, Enum)

class Monoid a => Group a where
  inverse :: a -> a

instance Group Sum where
  inverse = negate

instance Group RSum where
  inverse = negate

instance Group () where
  inverse () = ()
```

# これは何？
最近は技術書典5で出す「矢澤にこ先輩といっしょに代数！」という本を書いているのですが :point_down:

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">技術書典5に当選しました🎉✨✨<br>にこちゃんがHaskellで数学を教えてくれる本を出します！<a href="https://twitter.com/hashtag/%E6%8A%80%E8%A1%93%E6%9B%B8%E5%85%B8?src=hash&amp;ref_src=twsrc%5Etfw">#技術書典</a><a href="https://twitter.com/hashtag/%E6%8A%80%E8%A1%93%E6%9B%B8%E5%85%B85?src=hash&amp;ref_src=twsrc%5Etfw">#技術書典5</a><a href="https://t.co/Ev4BPQX1PT">https://t.co/Ev4BPQX1PT</a> <a href="https://t.co/SGQtFopgdD">pic.twitter.com/SGQtFopgdD</a></p>&mdash; あいや🤘🙄🤘技術書典5@か74 (@public_ai000ya) <a href="https://twitter.com/public_ai000ya/status/1025340175512043520?ref_src=twsrc%5Etfw">2018年8月3日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

群の章を書いているところで
「任意の群は`e * e = e`だっけ？」
という疑問に駆られたので、
その確認を行ったものです。

```haskell
check :: forall a. (Group a, Eq a) => Bool
check = (empty :: a) <> empty == empty

smallCheck 2 $ check @Sum
smallCheck 2 $ check @RSum
```

ここで`a`は引数に表れていないので`AllowAmbiguousTypes`が必要になり、
`ScopedTypeVariables`により`a`を文中で使用。
そして使用側でその群インスタンスの型を…
引数に依らず指定しています。

引数に依らないというところが、
`AllowAmbiguousTypes`が必要になる要点でした :dog2:

![1533346377850.jpg](https://qiita-image-store.s3.amazonaws.com/0/84945/3e508d68-3598-ccce-dd1f-3efd03ee4ab3.jpeg)


# 宣伝
技術書典5に出した後は、
Boothでの電子書籍での販売を予定しております :dog:

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">技術書典5に当選しました🎉✨✨<br>にこちゃんがHaskellで数学を教えてくれる本を出します！<a href="https://twitter.com/hashtag/%E6%8A%80%E8%A1%93%E6%9B%B8%E5%85%B8?src=hash&amp;ref_src=twsrc%5Etfw">#技術書典</a><a href="https://twitter.com/hashtag/%E6%8A%80%E8%A1%93%E6%9B%B8%E5%85%B85?src=hash&amp;ref_src=twsrc%5Etfw">#技術書典5</a><a href="https://t.co/Ev4BPQX1PT">https://t.co/Ev4BPQX1PT</a> <a href="https://t.co/SGQtFopgdD">pic.twitter.com/SGQtFopgdD</a></p>&mdash; あいや🤘🙄🤘技術書典5@か74 (@public_ai000ya) <a href="https://twitter.com/public_ai000ya/status/1025340175512043520?ref_src=twsrc%5Etfw">2018年8月3日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

よろしこ
