# TemplateHaskell不要なレコードアクセサgeneric-lens🙄

おはこんにちクリスマス〜〜🤟🙄🤟
今回はmakeLenses, makePrismsの代替となるgeneric-lensについて紹介します！

![Haskell Day 2018会場はこちら](ララちゃんの画像.png)

本稿は以下の概念を既知とします。

- [lens: Lenses, Folds and Traversals](http://hackage.haskell.org/package/lens)
- `DefaultSignatures`
- `TypeApplications`

ただしそのいずれかがわからなくとも、単なるlens事例の一つのアバウトとしてもお読みいただけます。

## まとめ

- `DeriveGeneric`
    - [GHC.Generics - HaskellWiki](https://wiki.haskell.org/GHC.Generics)
    - [GHC.Generics](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html)
    - [Data.Aeson](http://hackage.haskell.org/package/aeson-1.4.1.0/docs/Data-Aeson.html#t:FromJSON)
    - [learning-Haskell/Main.hs - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/master/Language/Haskell/Extension/DeriveGeneric/Main.hs)
- generic-lens
    - [generic-lens: Generically derive traversals, lenses and prisms.](http://hackage.haskell.org/package/generic-lens)
    - [learning-Haskell/generic-lens.hs - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/master/Data/Generic/generic-lens.hs)

## generic-lensの概観

さてlensパッケージに付いてくるmakeLensesですが、このように`sweet`および`moon`というようなアクセサを、TemplateHaskellを用いて自動定義するものでした。

```haskell
data Sugar = Sugar
  { _sweet :: String
  , _moon :: Int
  } deriving (Generic, Show)

makeLenses ''Sugar

sugar :: Sugar
sugar = Sugar "me" 1000

sugar ^. sweet
-- "me"

sugar & moon .~ 10003
-- Sugar {sweet = "me", moon = 10003}
```

これに対してgeneric-lensはTemplateHaskellを用いず、このようにDeriveGenericとGenericなコンビネーターを用いて同じことができます。

```haskell
data Sugar = Sugar
  { sweet :: String
  , moon :: Int
  } deriving (Generic, Show)

sugar :: Sugar
sugar = Sugar "me" 1000

sugar ^. field @"sweet"
sugar & field @"moon" .~ 10003
```

パフォーマンスについてですが、アルティメットGHCマジックにより、（Genericインスタンスを？）手書きでない限りmakeLensesと同一レベルのようです。
（-O1では。-O1以外だとどうなんだろ。）

> The runtime characteristics of the derived optics is in most cases identical at -O1, in some cases only slightly slower than the hand-written version. This is thanks to GHC's optimiser eliminating the generic overhead.

## DeriveGenericの概観

これについては多く語られたものと思いますので、ここでは導入のために大雑把に済ませます :eyes:
正確な情報については以下を参照してください。

- [GHC.Generics - HaskellWiki](https://wiki.haskell.org/GHC.Generics)
- [GHC.Generics](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html)

ということで……

`DeriveGeneric`はデータ型への`derive (Generic)`を許可します。
そして型`A`への`derive (Generic)`は、そのメタ情報を持つ型`Rep A`を生成するものです。

さきほど提示した、`derive (Generic)`された`Sugar`型の情報を見てみましょう。
ただし内容を理解する必要はありません。
`type instance Rep Sugar`が生えていることを確認してください :sunglasses:

```haskell
>>> :i Sugar
data Sugar = Sugar {sweet :: String, moon :: Int}
        -- Defined at <interactive>:11:1
instance [safe] Show Sugar -- Defined at <interactive>:14:24
instance [safe] Generic Sugar -- Defined at <interactive>:14:15
type instance Rep Sugar
  = D1
      ('MetaData "Sugar" "Ghci1" "interactive" 'False)
      (C1
         ('MetaCons "Sugar" 'PrefixI 'True)
         (S1
            ('MetaSel
               ('Just "sweet")
               'NoSourceUnpackedness
               'NoSourceStrictness
               'DecidedLazy)
            (Rec0 String)
          :*: S1
                ('MetaSel
                   ('Just "moon")
                   'NoSourceUnpackedness
                   'NoSourceStrictness
                   'DecidedLazy)
                (Rec0 Int)))
        -- Defined at <interactive>:14:15
```

各ライブラリは`DefaultSignatures`拡張とこの型`Rep a`への使って、任意の`a`へのインスタンスを実装することができます。

例えばそのライブラリが以下のような`Serialize`型クラスを提供するとします。
その場合ユーザーは同じく以下のように、`derive (Generic)`することのみで`instance Serialize`宣言することができます。
明示的な`put`実装を書く必要がないのです。

```haskell
data Bit = I | O
  deriving (Show)

class Serialize a where
  put :: a -> [Bit]
  default put :: (Generic a, GSerialize (Rep a)) => a -> [Bit]
  put = gput . from

instance Serialize Sugar
```

つまり`DeriveGeneric`は、ユーザーに変わって型クラスのインスタンスを自動生成してくれるものです！

これの実用例としては[Data.Aeson](http://hackage.haskell.org/package/aeson-1.4.1.0/docs/Data-Aeson.html#t:FromJSON)が有名かもしれません :alien:

- - -

この実装の全体は以下にあります。

- [learning-Haskell/Main.hs - aiya000/learning-Haskell - GitHub](https://github.com/aiya000/learning-Haskell/blob/master/Language/Haskell/Extension/DeriveGeneric/Main.hs)

## generic-lensの詳細

準備が終わりましたので、ここから本編です！
generic-lensの提供するコンビネータ―を紹介します :dog2:

以下の公式ページに書いてあるものと、公式ページには書いていないいくつかのものを、順に見ていきましょう。

- [generic-lens: Generically derive traversals, lenses and prisms.](http://hackage.haskell.org/package/generic-lens)

いくつかの種別として、データ型とその値を定義しておきます :sunglasses:

- 単純な直積: `Sugar`
- 単純な直和: `Fluffy`
- 同じ型を1つ以上含む直積: `Point`
- 一方が他方を包含するような型: `Skeleton`, `Sans`

```haskell
data Sugar = Sugar
  { sweet :: String
  , moon :: Int
  } deriving (Generic, Show)

sugar :: Sugar
sugar = Sugar "me" 1000

data Fluffy = Asgore { kind :: String }
            | Toriel { kind :: String, aggressive :: () }
  deriving (Generic, Show)

asgore :: Fluffy
asgore = Asgore ":)"

toriel :: Fluffy
toriel = Toriel ":D" ()

data Point = Point Int Int
  deriving (Generic, Show)

point :: Point
point = Point 100 200

newtype Skeleton = Skeleton
  { skeleton :: String
  } deriving (Generic, Show)

data Sans = Sans
 skeleton :: String
  , lazy :: Int
  } deriving (Generic, Show)

sans :: Sans
sans = Sans ";E" 1
```

### Lens
#### field

fieldはあるレコードの名前を型として受け取り、それにアクセスします。

```haskell
-- 直積
sugar ^. field @"sweet"
sugar & field @"moon" .~ 10003
-- "me"
-- Sugar {sweet = "me", moon = 10003}

-- 直和
asgore ^. field @"kind"
toriel ^. field @"kind"
-- ":)"
-- ":D"
```

ただし`Asgore`が`aggressive`を含んでいないため、以下はコンパイル不可です。

```haskell
-- Not able to
toriel ^. field @"aggressive"
```

#### position

positionは引数の番目を受け取り、それにアクセスします。

```haskell
sugar ^. position @1
sugar ^. position @2
-- "me"
-- 1000

(10, ("yours", "mine")) ^. position @2 . position @1
-- "yours"

asgore ^. position @1
toriel ^. position @1
-- ":)"
-- ":D"
```

ただし`Asgore`が2引数目を含んでいないため、以下はコンパイル不可です。

```haskell
-- Not able to
toriel ^. position @2
```

#### typed

typedはレコードの型を受け取り、その値を返します。

```haskell
sugar ^. typed @String
asgore ^. typed @String
-- "me"
-- ":)"
```

ただし`Asgore`が`()`を含んでいないため、
また`Point`の`Int`が一意的でない（`Int`のレコードが2つある）ため、
以下はコンパイル不可です。

```haskell
-- Not able to
toriel ^. typed @()
point ^. typed @Int
```


#### supar

superは構造的部分型関係`S <: T`な`S`を`T`に型付けます。

……えっ？
なんかいきなり趣が違くない？ :thinking:

```haskell
sans ^. super @Skeleton
upcast sans :: Skeleton
-- Skeleton {skeleton = ";E"}
-- Skeleton {skeleton = ";E"}
```

#### the

theはfield・position・typicalの全てを合わせたコンビネータ―です。

- `Symobl` ==> `field`
- `Nat` ==> `position`
- `Type` ==> `typed`

```haskell
sugar ^. the @String
asgore ^. the @1
sans ^. the @"skeleton" -- I'm Sans. Sans the skeleton ;E

-- "me"
-- ":)"
-- ";E"
```

### Prism

generic-lensはlensesの他にprismsも提供しています。
いずれもその値コンストラクタについて言及するもののようです。

#### \_Ctor

\_Ctorは値コンストラクタ名を引数に取って、それにアクセスします。

```haskell
sugar  ^? _Ctor @"Sugar"
toriel ^? _Ctor @"Toriel"
-- Just ("me",1000)
-- Just (":D",())

asgore ^? _Ctor @"Toriel"
-- Nothing
```

### \_Typed

\_Typedはレコードの型を受け取り、その値を返します。

```haskell
asgore ^? _Typed @String
toriel ^? _Typed @(String, ())
-- Just ":)"
-- Just (":D",())
```

`Fluffy`の`String`は`Asgore`ですが、`Toriel`は`(String, ())`であるため、以下は失敗します。

```haskell
toriel ^? _Typed @String
-- Nothing
```

`()`な`Fluffy`の値コンストラクタは存在しないため、以下はコンパイル不可です。

```haskell
-- Not able to
toriel ^? _Typed @()
```

### \_As

the同様、\_Asは\_Ctor, \_Typedの合わせです。

- `Symbol` => `_Ctor`
- `Type` => `_Typed`

```haskell
toriel ^? _As @"Toriel"
asgore ^? _As @String
-- Just (":D",())
-- Just ":)"
```

### ???

最後にConstraintsに関するコンビネータ―`constraints'`を見て、終わります。

```haskell
twice :: (Applicative f, Num a) => a -> f a
twice = pure . (*2)
```

```haskell
constraints' @Num (twice @Identity) point
-- Identity (Point 200 400)
```

うおおおおお！ めっちゃすごくて感動しました！

ってあれ？ これもうLensもPrismも関係ないじゃないですか。

## まとめ

generic-lensは以下のlensesとprismsを提供してくれました。
またmakeLensesとは違いGenerics由来なので、TemplateHaskellから来る不毛な戦いに遭遇することを回避できます。
（まあそのような不毛な戦いに遭遇することは、あまり多くないとは思いますが :thinking:）

```haskell
>>> :t field
field :: (HasField field s t a b, Functor f)
      => (a -> f b) -> s -> f t

>>> :t position
position :: (HasPosition i s t a b, Functor f)
         => (a -> f b) -> s -> f t

>>> :t typed
typed :: (HasType a s, Functor f)
      => (a -> f a) -> s -> f s

>>> :t super
super :: (Subtype sup sub, Functor f)
      => (sup -> f sup) -> sub -> f sub

>>> :t the
the :: (HasType b s, Functor f)
    => (b -> f b) -> s -> f s

>>> :t _Ctor
_Ctor :: ( AsConstructor ctor s t a b
         , Profunctor.Choice.Choice p
         , Applicative f
         ) => p a (f b) -> p s (f t)

>>> :t _Typed
_Typed :: ( AsType a s
          , Profunctor.Choice.Choice p
          , Applicative f
          ) => p a (f a) -> p s (f s)

>>> :t _As
_As :: ( AsType a s
       , Profunctor.Choice.Choice p
       , Applicative f
       ) => p a (f a) -> p s (f s)

>>> :t constraints'
constraints' :: ( Generic s
                , GHasConstraints' c (GHC.Generics.Rep s)
                , Applicative f
                ) => (forall a. c a => a -> f a) -> s -> f s
```

メリークリスマス！ :santa: :snowman:
