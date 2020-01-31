# TypeScriptで**安全に**プロパティの実在を確認する方法

## 結: オプショナルプロパティとしてasする

`x: unknown`に対して`typeof (x as { 存在確認したいプロパティ名?: string }).存在確認したいプロパティ名 === 'そのプロパティの型'`する。

```typescript
interface Account {
  readonly id: string
  name: string
}

function isAccount(x: unknown): x is Account {
  return (
    typeof (x as { id?: string }).id === 'string' &&
    typeof (x as { name?: string }).name === 'string'
  )
}
```

TypeScriptの`as`はほとんどの場合に不健全で、責任が重すぎて使えたものではないですが、オプショナルプロパティ付きオブジェクトへの`unknown`からの`as`だけは健全っぽいです。
（
xが`object`であるなら、任意のプロパティ`foo`が存在するか、もしくは存在しないので。
また`x as { foo?: Bar }`はxが`object`であることの確認を含んでいるので。
）
