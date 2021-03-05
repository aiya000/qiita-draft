# TypeScriptで、class以外の型にinterfaceをimplementsする
# 概要

TypeScript には`class`があるけど、あんまり使わないよね。
でも型に`interface`を実装したいときはあるよね。
でもでも`implements`は`class`にしかないよね。

じゃあ**型クラスパターン**を使おう！

# 結論

```typescript
/**
 * 目的の型がstrong-typedefパターン（newtypeパターン）で定義されていた場合に
 * （classでない場合に）、
 * interfaceを実装させたいときは、type-classパターンが使える。
 * @packageDocumentation
 */

/**
 * An example for 'newtype'.
 */
type AccessToken = string & { wrappingForAccessToken: never }

/**
 * いわゆる[スマートコンストラクタ](https://wiki.haskell.org/Smart_constructors)。
 *
 * 実運用ではAccessToken型と同じモジュールに置いておいて、
 * AccessTokenに「asするな！！！ makeAccessToken使え！！！」
 * って書いておくといいと思う。
 * ※1
 */
function makeAccessToken(x: string): AccessToken {
  return x as AccessToken
}

/**
 * [[makeAccessToken]]の双対。
 *
 * 本稿では使わないけど、newtypeパターンに必要なものとして挙げておく。
 * [[makeAccessToken]]と同じく「as使うな！！！ unwrapAccessToken使え！！！」
 * って書いておくといいと思う。
 */
function unwrapAccessToken(x: AccessToken): string {
  return x as string
}

/**
 * An example for 'type-class'.
 *
 * type-classは型引数を1つ（以上）受け取る。
 *
 * type-classは日本語では「型クラス」という。
 */
interface SerializableAsCookie<T> {
  saveToCookie(value: T): void
  getFromCookie(): T | null
  removeFromCookie(): void
}

/**
 * An example for "instance".
 *
 * type-classへの間接的なinterfaceの実装。
 * これをtype-class instance（型クラスインスタンス）と呼ぶ。
 * （単に「インスタンス」とも言うけど、OOPの文脈だとクラスインスタンスと紛らわしすぎるので、だいたいはフルネームで呼ぶ。）
 */
const instanceOfSerializableAsCookie: SerializableAsCookie<AccessToken> = {
  saveToCookie(value: AccessToken): void {
    throw new Error('Some implementation')
  },

  getFromCookie(): AccessToken | null {
    throw new Error('Some implementation')
  },

  removeFromCookie(): void {
    throw new Error('Some implementation')
  },
}

/**
 * type-classパターンの使用例。
 */
function saveAllToCookie<T>(serializer: SerializableAsCookie<T>, xs: Array<T>): void {
  for (const x of xs) {
    serializer.saveToCookie(x)
  }
}

// HaskellやScalaだと、serializerの指定はコンパイラが勝手にってくれる。
// 逆に言うとここを勝手にやってくれるだけなので、型クラスはそれらの言語でしか扱えないわけじゃないよ！
//
// ↑ここ重要！
saveAllToCookie(instanceOfSerializableAsCookie, [
  makeAccessToken('atasi-ha-inu-maid-da-nyan!'),
  makeAccessToken('itumo-mitekureru-minna-ga-suki-dayo!'),
  makeAccessToken('arigato-wan!'),
])
```

※1: https://qiita.com/kgtkr/items/1c136e1e4ccee8928bc8

- - -

Thanks :3
