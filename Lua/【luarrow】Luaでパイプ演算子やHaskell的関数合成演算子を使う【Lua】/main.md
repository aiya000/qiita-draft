---
title: 【luarrow】Luaでパイプ演算子やHaskell的関数合成演算子を使う【Lua】
tags: Lua LuaJIT neovim roblox 関数型プログラミング
---

<!--

```lua
local result = list
  % arrow(map(function(x) return x * 2 end))
  ^ arrow(filter(function(x) return x > 10 end))
  ^ arrow(reduce(function(acc, x) return acc + x end, 0))
```

-->

![preview-1.png](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/cc6973b1-58c4-42b9-8f35-eb3592aae0e3.png)

## 🚀 はじめに

Luaでプログラミングをしていると、このようなコードに遭遇することはありませんか？

```lua
print(foo(bar(baz(x), hoge)))
```

これは古典的な言語ではしばしば出会う、読みにくいコードです。
ただこのレベルでは、問題ないことが多いでしょう。

しかし高階関数を使うと、事態は深刻化します。
関数型プログラミングがようやく注目されていることもあり、現代の言語では高階関数は多用され、古典的な関数呼び出しでは、可読性が大きく下がります。

そして積もりが重なり、メンテナンス性は著しく低下します。

```lua
local result = reduce(filter(map(list, function(x)
  return x * 2
end), function(x)
  return x > 10
end), function(acc, x)
  return acc + x
end, 0)
```

やがて機能追加やバグ修正という**本質的な作業**よりも、そのためのコードリファクタリング・あるいはそのコードをよけて作業するための**副次的な作業**の方が、コストが高くなるのです。
末期的には、生産性は0に近づきます。

（「[Clean Architecture 達人に学ぶソフトウェアの構造と設計 Robert C. Martin(著), 角征典, 髙木正弘(訳)](https://amzn.to/4oobcog)」
36ページ「崩壊のサイン」の章から引用。[^source-of-evil]）

[^source-of-evil]: 「"図1-4は、開発者にとってこの曲線がどのように見えるかを示したものである。最初はほぼ100%だった生産性が、リリースするたびに低下していることがわかる。4回目のリリースからは、生産性は底を打っている。"」

- - -

そこで私はLuaライブラリ、**luarrow**を開発しました。

- [luarrow.lua - GitHub](https://github.com/aiya000/luarrow.lua)

luarrowはLuaにパイプライン演算子（`|>`）やHaskell風関数合成（`.`, `$`）を、**Luaなりに**導入するライブラリです。
演算子オーバーロードを活用し、ネストした関数呼び出しを代表とした、**Luaのコードを美しく**、読みやすい関数適用フローの方法を提供します。

<!--

```lua
local _ = 42
  % arrow(function(x) return x + 1 end)
  ^ arrow(function(x) return x * 10 end)
  ^ arrow(function(x) return x - 2 end)
  ^ arrow(print)  -- 419
```

-->

![preview-2.png](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/2bf882f0-3d92-414f-a56c-1703db565278.png)

## ✨ 特徴

全体像とその詳細のREADME.mdやドキュメントに記述していますので、筆者が特に強調したい点を説明します。

全体と詳細を読みたい方は、以下を参照してください。

- [README.md - luarrow](https://github.com/aiya000/luarrow.lua/blob/main/README.md)
- [docs - luarrow](https://github.com/aiya000/luarrow.lua/tree/main/doc)

### 🔀 真のパイプライン**演算子**

しばしば、パイプライン演算子がほしい場合に演算子を実装できず、パイプライン**関数**で実現することがあります。

```lua
local pipe = require('foo-package').pipe -- 何かしらのパッケージ

local result = pipe(
  list,
  map(function(x) return x * 2 end),
  filter(function(x) return x > 10 end),
  reduce(function(acc, x) return acc + x end, 0)
)
```

[^list-utils]

[^list-utils]: この例や前述のluarrowのコード例画像で使われている`map`、`filter`、`reduce`は、どこかで実装されていると仮定します。今はluarrowには実装されていないためです。しかし現在、実装が進行中です（これには、他にもリストを引数にした関数、`foldr`, `flatten`, `find`, `foldl1`, `sum`, `join`, `sort`などなどが含まれています）。 → [feat(luarrow.utils.list): Add comprehensive list manipulation module with curried API](https://github.com/aiya000/luarrow.lua/pull/25)

しかしluarrowはこれをよしとせず、演算子オーバーロードを用いるというアイデアを採用しました。

```lua
local arrow = require('luarrow').arrow

local result = list
  % arrow(map(function(x) return x * 2 end))
  ^ arrow(filter(function(x) return x > 10 end))
  ^ arrow(reduce(function(acc, x) return acc + x end, 0))
```

[^why-this-operators]

[^why-this-operators]: 実際のところ、パイプライン演算子は`|>`のように見えることが望ましいですが、Luaでは演算子オーバーロードが可能な演算子が限られているため、`%`と`^`を使っています。

これは以下のPHPコードと同等です。

```php
$result = 42
  |> (fn($x) => $x - 2)
  |> (fn($x) => $x * 10)
  |> (fn($x) => $x + 1);
```

`pipe`関数でもよしとするユーザーはそれでいいですが、
luarrowはそうでないユーザーに対し、**構文的美学**を提供します。

luarrowは「**関数型プログラミングへのロマン的追及**」を大切にしています。

### 🔗 Haskellスタイルの関数合成演算子

また、**Haskellスタイルの関数合成演算子**および**関数適用演算子**も提供しています。

```lua
local fun = require('luarrow').fun

local function plus_one(x) return x + 1 end
local function times_ten(x) return x * 10 end
local function minus_two(x) return x - 2 end

-- ポイントフリーによる関数定義
local k = fun(plus_one) * fun(times_tenj * fun(h)

-- 関数適用演算子
-- luarrowのfunにおいて、Pure Luaの k(42) と同等（arrowの%との混同に注意）
local result = k % 42
print(result)  -- 401
```

これはHaskellの

```haskell
k = plus_one . times_ten . minus_two
result = k $ 42
main = print result  -- 401
```

と同様です。
（ただし、Haskellにおいては`k 42`と書いても問題ありませんが、luarrowでは`%`が必要です。）

これは数学的に伝統のある`f ○ g ○ h`のような関数合成にも倣っています。

### ⚡ ほぼオーバーヘッドなし（LuaJIT環境）

**LuaJIT**を視野に入れるため、luarrowは**Lua 5.1準拠**で設計されています。[^disadvantages-lua-5-1]

[^disadvantages-lua-5-1]: luarrowが`<<`や`>>`などのわかりやすい演算子でフローを再現しなかったのは、そのためです。Lua 5.1はこれらのビット演算子をサポートしていません。

LuaJITはゲームエンジンなど、多くのLua処理系で採用されています。
これらの処理系においてluarrowは、前述のように読みにくい通常の関数呼び出しと、同様の速度を実現します。

実際、LuaJITで実地計測をしたところ、**0.0005秒以下**という結果が出ています。

- [ベンチマークスクリプト](https://github.com/aiya000/luarrow.lua/blob/main/scripts/benchmark-both.lua)

- LuaJITを採用している環境
    - **Neovim** [^notice-neovim]
    - ゲームエンジン
        - **Roblox**
        - Defold
        - CRYENGINE

[^notice-neovim]: LuaJITオプションでビルドされたNeovimを指します。HomebrewやLinuxbrewで入るような通常のNeovimはLuaJITを使っているので、Neovimリポジトリで自前ビルドをしているような人以外は、恩恵があります。

しかしながらこれを裏返すと、非常にパフォーマンスが重要な場面では（そのLua処理系にJITがない限り）luarrowは適していないかもしれない、ということでもあります。
これは非常に苦しい点です。
ただし多くの用途、例えば以下の用途など、カジュアルなものに対しては十分に高速です。

- 設定ファイルの処理
- ゲーム開発のスクリプティング
    - 特にLuaJITを採用しているゲームエンジン（後述）
- コマンドラインツール
- データ変換スクリプト

- - -

- [パフォーマンスについてのベストプラクティスおよびレポートについてのドキュメント](https://github.com/aiya000/luarrow.lua/blob/main/doc/examples.md#-performance-considerations)

## 💬 実際にいただいた質問・反論への回答

### ❓ Q1: 「メソッドチェーンでいいじゃん」

確かにメソッドチェーンも読みやすい記法です。
しかしメソッドチェーンはluarrowの代わりになることはできません。

メソッドチェーンはiteratorに特殊化されています。
一方、luarrowは**全ての値に一般化**されています。

より簡単に言うと、メソッドチェーンはリスト（配列）のようなLuaのクラスに対しては有効ですが、数値や文字列などの広い型に対しては使えません。

具体的な例を見てみましょう。

メソッドチェーンの制限:

```lua
-- メソッドチェーンの例（仮想的なiteratorライブラリ）
local result = iterator(list)
  :map(function(x) return x * 2 end)
  :filter(function(x) return x > 5 end)
  :reduce(function(acc, x) return acc + x end, 0)
  -- ここまではOK

-- しかしreduceの結果（数値）に対してさらに処理を続けたい場合は、メソッドチェーンは使えない。
-- result（数値）にはメソッドがないため
```

reduce後に数値を返した場合、メソッドチェーンは**そこで終了**します。
さらに処理を続けるには、別の行で書き直す必要があります。

```lua
local sum = iterator(list)
  :map(function(x) return x * 2 end)
  :filter(function(x) return x > 5 end)
  :reduce(function(acc, x) return acc + x end, 0)

-- チェインが切れる...
local average = sum / #list
print(average)
```

**luarrowなら一般化されているので、どんな値でも継続できます:**

```lua
local arrow = require('luarrow').arrow

local _ = list
  % arrow(map(function(x) return x * 2 end))
  ^ arrow(filter(function(x) return x > 5 end))
  ^ arrow(reduce(function(acc, x) return acc + x end, 0))
  -- ここでreduceは数値を返す
  ^ arrow(function(sum) return sum / #list end)  -- 数値に対してもそのまま続けられる！
  ^ arrow(print)  -- 424
```

luarrowは**値の型に関係なく**パイプラインを継続できるため、より柔軟で一般的なのです。

### ❓ Q2: 「ネストした関数呼び出しの方がいい」

「はじめに」での例を引用します。

```lua
print(foo(bar(baz(x), hoge)))
```

これは目をつむって、よいとします。
しかし以下はどうでしょうか。

```lua
local result = reduce(filter(map(list, function(x)
  return x * 2
end), function(x)
  return x > 10
end), function(acc, x)
  return acc + x
end, 0)
```

あなたは何度目を左右に動かしたでしょうか。

luarrowなら上から下、左から右に読めばいいのです。

```lua
local result = list
  % arrow(map(function(x) return x * 2 end))
  ^ arrow(filter(function(x) return x > 10 end))
  ^ arrow(reduce(function(acc, x) return acc + x end, 0))
```

### ❓ Q3: 「ドキュメントが明らかに宣伝目的だ」

そうではありません。
ユーザーのluarrowの原理と、メリットの理解のため、自然にそうなっています。

実際、不都合なことを避けるようなことはしていません。
例えば前述したものを再掲しますが、以下ではLuaJIT以外での不都合を公開しています。

- [パフォーマンスについてのベストプラクティスおよびレポートについてのドキュメント](https://github.com/aiya000/luarrow.lua/blob/main/doc/examples.md#-performance-considerations)

### ❓ Q4: 「`%`メタメソッドを持つ型で問題が起きるのでは？」

聡い読者の方は気づいたかもしれませんが、luarrowの「パイプライン演算子`%`, `^`」には、しつこく`arrow`関数が付属しています。
（同様に「Haskellスタイルの関数合成演算子`*`, `%`」では`fun`関数が付属しています。）

これは以下のPHPのコード例のように…（非常に残念なことに）美しくはありませんが

```php
$result = 42
  |> (fn($x) => $x - 2)
  |> (fn($x) => $x * 10)
  |> (fn($x) => $x + 1);
```

本クエスチョンへの回答になっています。
つまり、「`arrow`（`fun`）関数によって`metatable`を設定することで、他の`%`, `^`, `*`を持つ型とは区別されている」ということです。

そこで未だに問題が起こるというのであれば、それは根本的に動的型付けの問題であると考えます。

:::note info
Luaには`LuaCATS`などのアノテーションがあるので、そこで`---@type`などを駆使することで、ある程度の改善を試みることができるでしょう。

- [Annotations - LuaCATS](https://luals.github.io/wiki/annotations/)

ただし個人的にはまだLuaCATSはまだ未成熟で、漸進的型付け[^what-is-gradual-typing]としては運用できないとも思っています
:::

[^what-is-gradual-typing]: 漸進的型付けとは、TypeScriptを代表とする、静的型付けが可能でありつつも、TypeScriptの`any`などのように動的型付けにスイッチすることができる、型付けのことです。Gradual Typingとも。

## 📚 豊富なドキュメント

luarrowでは、充実したドキュメントを用意しています。

### 📖 ドキュメント

- **[API Reference](https://github.com/aiya000/luarrow.lua/blob/main/doc/api.md)** - 完全なAPI仕様書
    - `Fun`と`Arrow`の全メソッド・演算子の詳細
    - 型パラメータの説明
    - 使用例とTips
- **[Examples](https://github.com/aiya000/luarrow.lua/blob/main/doc/examples.md)** - 実践的な使用例集
    - 基本例から高度なパターンまで
    - パフォーマンスベンチマーク
    - 他のアプローチとの比較
    - LuaCATSとの連携方法

この記事では触れきれなかった詳細な情報や、より高度な使用例について知りたい方は、ぜひドキュメントをご覧ください。

### 📦 インストール方法

- luarocks

```shell-session
$ luarocks install luarrow
# 動作確認
$ eval $(luarocks path) && lua -e "local l = require('luarrow') ; print('Installed correctly!')"
```

- Gitから直接インストールする

```shell-session
$ git clone https://github.com/aiya000/luarrow.lua
$ cd luarrow.lua
$ make install-to-local
```

### 🔗 リンク

- GitHubリポジトリ: [aiya000/luarrow.lua](https://github.com/aiya000/luarrow.lua)
- luarocksパッケージ: [luarocks.org/modules/aiya000/luarrow](https://luarocks.org/modules/aiya000/luarrow)

## 🔮 今後の展望

luarrowは現在も開発中です。

### 📝 リスト操作モジュールの追加

- [feat(luarrow.utils.list)!: Add comprehensive list manipulation module with curried API](https://github.com/aiya000/luarrow.lua/pull/25)

追加される関数:

- map, filter, flat_map/concat_map, flatten, find
- foldl (reduce), foldr, foldl1, foldr1
- sum, product, join
- length, is_empty, head, tail, last, init
- reverse, sort, sort_by/sort_with, unique, group_by
- maximum, minimum

### 🗺️ その他の計画

今後、以下のような機能も追加していく予定です。

- 他のユーティリティ関数やコンビネーターの追加
- さらなるパフォーマンス最適化
- ドキュメントの充実化

もしluarrowに追加してほしい機能や、改善のアイデアがあれば、気軽にIssueを立ててください。
実装させていただくかもしれません。

フィードバックを歓迎しています！

## 🎯 まとめ

**luarrow**は、Luaにパイプライン演算子とHaskell風関数合成を導入するライブラリです。

この記事で紹介した主な特徴をまとめます：

- **パイプライン演算子**: `x % arrow(f) ^ arrow(g)` で左から右へのデータフロー
- **Haskellスタイルの関数合成**: `fun(f) * fun(g) % x` で右から左へのデータフロー
- **一般化された設計**: メソッドチェインと違い、あらゆる型の値に対して使える
- **高いパフォーマンス**: LuaJIT環境では実質的にオーバーヘッドなし
- **充実したドキュメント**: API Reference、Examples、ベンチマークなど

ネストした関数呼び出しや、高階関数の可読性に悩んでいる方は、ぜひluarrowを試してみてください。

- **リポジトリ**: [aiya000/luarrow.lua](https://github.com/aiya000/luarrow.lua)

皆さんのLuaコードがより美しく、より読みやすくなることを願っています。
