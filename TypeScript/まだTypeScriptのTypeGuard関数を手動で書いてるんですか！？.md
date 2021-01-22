# まだTypeScriptのTypeGuard関数を手動で書いてるんですか！？
# 結論

io-tsを使うと、TypeGuard関数[^word-type-guard-function]を手で書かなくて済む。

[^word-type-guard-function]: ここでの「TypeGuard関数」とはUser-Defined Type Guardのことを指します。以下、User-Defined Type Guardと呼称。

- [io-ts - GitHub](https://github.com/gcanti/io-ts)
- [io-tsのドキュメント](https://github.com/gcanti/io-ts/blob/master/index.md)

```typescript
import * as T from 'io-ts'
```

<details>
<summary>`interface {}`の定義と自動導出されたUser-Defined Type Guardの使用</summary>
<div>

```typescript
// interface
const objectX = T.type({
  x: T.number,
  y: T.string,
})

type ObjectX = T.TypeOf<typeof objectX>

/* Same as

interface ObjectX {
  x: number
  y: string
}

*/

const a: unknown = {
  x: 42,
  y: 'inu',
}

// ---------------------------------
// User-Defined Type Guardは自動的に導出される
// ---------------------------------
const isObjectX: (w: unknown) => w is ObjectX = objectX.is

if (isObjectX(a)) {
  console.log('a is an ObjectX.')
  console.log(a.x)
  console.log()
}

// a is an ObjectX.
// 42
//
```

</div>
</details>

<details>
<summary>和型`foo | bar`</summary>
<div>

```typescript
// union
const unionX = T.union([
  T.number,
  T.string,
])

type UnionX = T.TypeOf<typeof unionX>

const b: unknown = 'neko'

if (unionX.is(b)) {
  console.log('b is a UnionX.')
  console.log()
}

// b is a UnionX.
//
```

</div>
</details>

<details>
<summary>積型`foo & bar`の定義とpartialプロパティ`nya?: Mew`</summary>
<div>

```typescript
// partialを持つinterface
const objectY = T.intersection([
  T.type({x: T.number}),
  T.partial({y: T.string}),
])

type ObjectY = T.TypeOf<typeof objectY>

const c: unknown = { x: 42 }

if (objectY.is(c)) {
  console.log('c is an ObjectY.')
  console.log(c.x)
  console.log()
}

// c is an ObjectY.
// 42
//
```

</div>
</details>

<details>
<summary>おまけ: partialプロパティを定義するなら、`T.partial()`を使った方がいい</summary>
<div>

下記の用法の代わりに、上記の「積型`foo & bar`の定義とpartialプロパティ`nya?: Mew`」で使用した方法を使わないと、`y: undefined`を明示的に書く必要がでてしまう。

:point_down: :x:

```typescript
const objectYNotBetter = T.type({
  x: T.number,
  y: T.union([T.string, T.undefined]),
})

type ObjectYNotBetter = T.TypeOf<typeof objectYNotBetter>

if (!objectYNotBetter.is(c)) {
  console.log('c is not an ObjectYNotBetter, because c.y is omitted.')
}

const d: unknown = {
  x: 42,
  y: undefined,
}

if (objectYNotBetter.is(d)) {
  console.log('d is an ObjectYNotBetter.')
  console.log('We must define .y with undefined for kinds of objectYNotBetter.')
  console.log(d.y)
  console.log()
}

// c is not an ObjectYNotBetter, because c.y is omitted.
// d is an ObjectYNotBetter.
// We must define .y with undefined for kinds of objectYNotBetter.
// undefined
//

if (objectY.is(d)) {
  console.log('But, no way, d is an ObjectY.')
  console.log(d.y)
  console.log()
}

// But, no way, d is an ObjectY.
// undefined
//
```

</div>
</details>

# 概要

僕はもう疲れてしまいました。
あらわれるいくつもの型！！ 無限にUser-Defined Type Guardを書かなければいけない！！！
オラオラオラオラオラオラオラオラオラオラオラオラオラオラオラオラオラオラオラオラァ！！！

```typescript
interface InuNekoMaid {
  maid: boolean

  wan: Bow
  nya?: Mew

  // ...
}

// User-Defined Type Guard
function isInuNecoMaid(x: any): x is InuNekoMaid {
  return (
    typeof x.maid === 'boolean' &&
    isBow(x.wan) &&
    (x.nya === undefined || isMew(nya))

    // && ...
  )
}

// これを定義する作業が型の数だけ続く。
```

型の種類は可算無限個あると、古事記にも書かれています。
つまり最悪な場合、我々は限りなくUser-Defined Type Guardを書かなければいけない :thinking_face:

それはいけませんね。
型に対するマナーがなっていません！
ありえない話！！！！🤬😡🤬🤬😡🤬

io-tsを使いましょう。

- [io-ts - GitHub](https://github.com/gcanti/io-ts)
- [io-tsのドキュメント](https://github.com/gcanti/io-ts/blob/master/index.md)

# io-tsとは

io-tsとは`Type<A, O, I>`という、**コーデック（codec）**と呼ばれるものを提供する、TypeScriptのライブラリです。

`Type<A, O, I>`は以下の形で、`O, A, I`型の間の関数をまとめたクラスです。

```
           decode関数
       I  ----------->  (A | Errors)

           encode関数
       A  ---------->  O
       |
is関数 |
       v
    (x is A)
```

そしてその`Type<A, O, AI>`は、関数`T.type()`と、型関数`T.TypeOf<>`を用いて、自動的に生成されます。

`interface`・`X | Y`・`X & Y`などのような型への、`decode`・`encode`・`is`関数が自動で生成される、ということです。

ここで本稿の結論が出ました。
そう、`is`関数です！

```typescript
// interface
const objectX = T.type({
  x: T.number,
  y: T.string,
})

type ObjectX = T.TypeOf<typeof objectX>

/* Same as

interface ObjectX {
  x: number
  y: string
}

*/

// これ！！
const isObjectX: (w: unknown) => w is ObjectX = objectX.is
```

# まとめ

あとは[結論](#結論)のままに、各型を構築するだけです。

簡単！
最高！
やったー！！

# おまけ: partialプロパティを定義するなら、`T.partial()`を使った方がいい

io-tsで

```typescript
interface ObjectY {
  x: number
  y?: string  // partialプロパティ
}
```

を定義したいときは、以下のようにする :o:

```typescript
const objectY = T.intersection([
  T.type({x: T.number}),
  T.partial({y: T.string}),
])

type ObjectY = T.TypeOf<typeof objectY>
```

以下のようにしない :x:

```typescript
const objectYNotBetter = T.type({
  x: T.number,
  y: T.union([T.string, T.undefined]),
})

type ObjectYNotBetter = T.TypeOf<typeof objectYNotBetter>
```

前者は次のような値を表すが、

```typescript
// Compile OK

const y1: ObjectY = {
  x: 10,
}

const y2: ObjectY = {
  x: 10,
  y: 20,
}

const y3: ObjectY = {
  x: 10,
  y: undefined,
}
```

後者は次のような値を**表さない**。
（`.y`を省略できない。）

```typescript
// Compile error
const wrong: ObjectYNotBetter = { x: 10 }
```

# おわり

まだTypeScriptのTypeGuard関数を手動で書いてるんですか！？

マナー違反ですよ！！！
むせかえるような怠惰のにおい！！！
ありえない話！！！！🤬🤬😡🤬😡🤬
