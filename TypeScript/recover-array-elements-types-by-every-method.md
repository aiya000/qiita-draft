# TypeScript で配列の要素を再特定する（`xs is Array<Foo>` by `.every()`）

- 今週の標語: optional property 以外への`as`は unsafe

## 解決策: 関数でラップしてあげる

解決策

```typescript
function hasTypedElements<PropType>(
  xs: Array<unknown>,
  isTypedElement: (x: unknown) => x is PropType
): xs is Array<PropType> {
  return xs.every(isTypedElement)
}
```

確認

```typescript
/**
 * Checking for primitives
 */

// TypeScriptが `(x: unknown) => typeof x === 'string'` を `x is string` だと理解してくれないので、関数化する
function isString(x: unknown): x is string {
  return typeof x === 'string'
}

const xs: Array<unknown> = ['string']
if (!hasTypedElements(xs, isString)) {
  throw new Error('good bye')
}
const recovered0: Array<string> = xs
console.log(recovered0) // [ 'string' ]

try {
  const ys: Array<unknown> = [10]
  if (!hasTypedElements(ys, isString)) {
    throw new Error('good bye')
  }
  const recovered1: Array<string> = ys
  console.log(recovered1)
} catch (e) {
  console.log(e) // Error: good bye
}

/**
 * Checking for objects
 */

interface Foo {
  x: string
}

function isFoo(x: unknown): x is Foo {
  return typeof (x as { x?: string }).x === 'string'
}

const zs: Array<unknown> = [{ x: 'string' }]
if (!hasTypedElements(zs, isFoo)) {
  throw new Error('good bye')
}
const recovered2: Array<Foo> = zs
console.log(recovered2) //  { x: 'string' } ]
```

## そもそも期待していたこと

コード

```typescript
interface Foo {
  x: string
}

function isFoo(x: unknown): x is Foo {
  return typeof (x as { x?: string }).x === 'string'
}

const xs: Array<unknown> = [{ x: 'string' }]
if (!xs.every(isFoo)) {
  throw new Error('good bye')
}
const ys: Array<Foo> = xs
```

- 期待していたこと: コンパイルが通る
- 実際: `2322: Type 'unknown[]' is not assignable to type 'Foo[]'. Type 'unknown' is not assignable to type 'Foo'.`

`.every`！　お前型ガードしてくれよ！！

## 解決策

関数化により、`.every`をラップしてあげることにします。

```typescript
function hasTypedElements<PropType>(
  xs: Array<unknown>,
  isTypedElement: (x: unknown) => x is PropType
): xs is Array<PropType> {
  return xs.every(isTypedElement)
}
```

---

TypeScript が型をつけてくれる？
否。
私達が "TypeScript に" 型をつけて回るのだ。

許せん。
