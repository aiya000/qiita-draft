# 【TypeScript】const x: {readonly a: number}でも、aプロパティは変更不可にはならない

こんにちは。
最近Haskellを書いていない、型エンジニアのaiya000です！

皆さんは下記コードの`readonly`で、`x.a`は変更されることがないと思っていませんか？
残念ですが……完全には**そうはなりません**……。

ちなみにここで`x.a`を変更するには、`as`などの[unsafe](https://qiita.com/kgtkr/items/1c136e1e4ccee8928bc8)な操作は必要としません。

```typescript
const x: { readonly a: number } = { a: 42 }
```

# `const`と`readonly`とは？
## `const`

上述のコードの`x.a`は、ある方法で変更され得ります。
しかし本来は**変更されるべきではない**です。

どうして上述のコードが不変であるべきなのか、改めて再確認してみましょう。
まず`const`は「再代入」を許しません。
次の例で表せます。

```typescript
const x: number = 42
x = 10  // コンパイルエラー
```

大丈夫です、このコードは**正しくコンパイルエラーになります**。

## `readonly`

次に`readonly`です。
下記に示すコードも、**正しくコンパイルエラーになります**。

```typescript
let x: { readonly a: number } = { a: 42 }

// ここはコンパイルエラーにならないので、
// 不変性を求める場合は通常、letではなくconstと組み合わせる。
x = { a: 10 }

// ここはコンパイルエラーになる。
x.a = 20
```

`x.a`（readonlyプロパティ）への再代入は、コンパイルエラーになります。

## `const` + `readonly`

TypeScriptでは、上述の2つを組み合わせて、不変なオブジェクトの変数を作ります。

```typescript
const x: { readonly a: number } = { a: 42 }
x = { a: 10 } // コンパイルエラー
x.a = 10      // コンパイルエラー
```

これで`x`はいかなる場合にも、プロパティの値が変わらないオブジェクトの変数になりました。
アプリケーションの設定など、アプリケーションの実行時に変更されたくない変数はこのテクニックを使うことで、実現することができます。

……

ごめんなさい**というのは嘘です**。
実際は、この`x`のような変数は、**変更され得ります**。

# 不変性と部分型付け

なぜ`const x: { readonly a: number } = { a: 42 }`のような変数は変更されうるのでしょうか。
その秘密にせまるために、通常あるべき、不変性の部分型付けを見てみます。

次のコードは、プログラミング意味論的に、通るべきです。
なぜなら「可変なオブジェクトは、一時的に不変にしても破綻しない」からです。

```typescript
// ある不変なオブジェクトの型
type Immutable = {
  readonly a: number
}

// ある可変なオブジェクトの型
type Mutable = {
  a: number
}

function f(x: Immutable): void {
  // xを使って処理をする
  console.log(x)
}

const x: Mutable = { a: 42 }

f(x)
```

例えばKotlinでは上述と同様に、`MutableList`の変数を、（Immutable）`List`の変数に代入することができます。

逆に次のコードは、プログラミング意味論を破綻させます。
なぜなら「不変なオブジェクトを一時的に可変にすると、不変性が破綻する」からです。

```typescript:illegal.ts
// ある不変なオブジェクトの型
type Immutable = {
  readonly a: number
}

// ある不変でないオブジェクトの型
type Mutable = {
  a: number
}

function f(x: Mutable): void {
  // xを使って処理をする
  x.a = 10
}

const x: Immutable = { a: 42 }
f(x)
console.log(x)
```

具体的には`{ readonly a: number} extends { a: number }`であるべきで、
`{ a: number} extends { readonly a: number }`であるべきではありません。

……

もうわかりましたね。
そう、このコード`illegal.ts`は、TypeScriptではコンパイルエラーになりません！

`{ readonly a: number} extends { a: number }`かつ
`{ a: number} extends { readonly a: number }`だからです！

# まとめ

例をシンプルに書き直します。

これは`x.a`を書き換えます。
不変性が壊れているのが、容易にわかります。

```typescript
const x: { readonly a: number } = { a: 42 }
const y: { a: number } = x

// x.aが変更されている
y.a = 10

console.log(y) // { a: 10 }

// コンパイルエラーはない
```

`as const`でも同様です。

```typescript
const x = { a: 42 } as const
const y: { a: number } = x

y.a = 10

console.log(y)

// コンパイルエラーはない
```

どうしてもTypeScriptで絶対の不変性が必要な場合には、`Object.freeze()`を使用して、実行時にエラーを送出させる必要があります。

```typescript
const x: { readonly a: number } = Object.freeze({ a: 42 })
const y: { a: number } = x

// ここで例外が送出される
y.a = 10
```

TypeScriptの`{readonly a: number}`型では、`a`プロパティは**変更不可になりません**！
