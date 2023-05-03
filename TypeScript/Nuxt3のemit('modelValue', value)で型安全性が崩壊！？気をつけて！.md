# 結

Nuxt3の`emit('update:modelValue', value)`が`modelValue`と疎結合なので、以下のような用法によって、型健全性が壊れます！！

```typescript
const props = defineProps<{
  modelValue: T
}>()

const emit = defineEmits<{
  (e: 'update:modelValue', value: U): void  // 間違ってvalueがTでなくUになっている
}>()

const fun = () => emit('update:modelValue', u)  // u: U
```

# 解説

解説する必要もないくらい単純なものですが、一応解説させていただきます :)

`e: 'update:modelValue'`を持つemitはVue.jsで特別扱いされています。
例えば以下のFoo.vueのbuttonをclickすると、`emit('update:modelValue', 42)`により、`props.modelValue`が42にセットされ、それによりindex.vueの`foo`も同様に42にセットされます。

```vue:Foo.vue
<template>
  <button @click="fun">42</button>
</template>

<script setup lang="ts">

</script>
const props = defineProps<{
  modelValue: number
}>()

const emit = defineEmits<{
  (e: 'update:modelValue', value: number): void
}>()

const fun = () => emit('update:modelValue', 42)
```

```vue:index.vue
<template>
  <div>
    <Foo v-model="foo">
    foo: {{ foo }}
  </div>
</template>

<script setup lang="ts">
const foo = ref(0)
</script>
```

ここで

```typescript:Foo.vue
const emit = defineEmits<{
  (e: 'update:modelValue', value: number): void
}>()

const fun = () => emit('update:modelValue', 42)
```

を

```typescript:Foo.vue
const emit = defineEmits<{
  (e: 'update:modelValue', value: Error): void
}>()

const fun = () => emit('update:modelValue', new Error('msg'))
```

に間違えてみましょう。
Foo.vueのmodelValueはnumberで

```typescript:Foo.vue
const props = defineProps<{
  modelValue: number
}>()
```

同様にindex.vueのfooもRef<number>なので

```typescript:index.vue
const foo = ref(0)
```

明らかにErrorになるのはおかしいです。

しかし実際はFoo.vueのbuttonをclickすると、fooの値が`new Error('msg')`になります。
つまりfooが`Ref<number>`なのに`Ref<Error>`の値が入っており、これは型健全性を損なっています。

# 結

**ここ**には注意しましょう。
型が矛盾します。

```typescript
const emit = defineEmits<{
  (e: 'update:modelValue', value: ここ): void
}>()
```
