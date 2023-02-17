# WritableComputedRef<T>を作った気になっていた犬のあいや君

今日は僕と、Vue3の`WritableComputedRef<T>`の作り方を学ぼう。

## 作り方……？

```typescript
const props = withDefaults(
  defineProps<{
    modelValue?: number
  }>(),
  {
    modelValue: undefined,
  }
}

const emit = defineEmits<{
   (e: 'update:modelValue', value: number): void
}>()

// これだよ
const innerValue = computed(() => {
  get(): number {
    return props.modelValue ?? 0
  },
  set(value: number): void {
    emit('update:modelValue', value)
    console.log('みんなもあいさつしよう: ', value)
  },
})
```

## みんなは気づいたかな？

innerValueに書き込もうとすると、`computed value is readonly`って怒られるんだ。
なんでだろうね。

みなおして、みよう。

```typescript
// これだよ
const innerValue = computed(() => {
```

ここが、おかしいね。
`WritableComputedRef<number>`を作るためには、直接アクセサーを渡すはずだよね。
みんなもいっしょに、なおしてみよう。

```diff
-const innerValue = computed(() => {
+const innerValue = computed({
```

**正しい↓**

```typescript
// これだよ
const innerValue = computed({
  get(): number {
    return props.modelValue ?? 0
  },
  set(value: number): void {
    emit('update:modelValue', value)
    console.log('みんなもあいさつしよう: ', value)
  },
})
```

**正しい↑**

だれだよこんなコード書いたの。
`git blame`してやる。
だれだよこんなコード書いたの。

　
　
　
　　　　　　　　　　僕だ。

　

　　　　　　　　　　　　　　　　　　　　糸冬
　　　　　　　　　　　　　　　　　　　　----
