# 型とテストに守られたナウなフロントエンド構築概要
## はじめに

この記事は :sparkles: [TypeScript Advent Calendar 2022](https://qiita.com/advent-calendar/2022/typescript) :sparkles: の、1日目の記事です！

ここでは某所で作成している、ナウで堅牢な開発構成をただただ紹介します。

起承転結などない。いいね？
ｱｯﾊｲ

## 結論

- Nuxt3
    - TypeScript + unsafe絶対コ□すマン
    - `vite`
    - `script setup`
    - ステート管理: composables
- Linter
    - eslint, stylelint, prettier
- Tests
    - Unit Tests: vitest
    - E2E Tests: Playwright
        - vitestとplaywrightのうまい繋ぎ方が見つからなかったため、vitestは使わずに開発サーバー（`yarn dev`）のDOMを直接叩いている
            - 一周回って「E2Eテストだし、逆にユーザーエンドに近いのでは」と思っている

## 解説
### Nuxt3

某所ではVue.jsのフレームワーク、Nuxt.jsを使っています。
ここでNuxt.jsの利点や詳細は省略しますが、ディレクトリ構成に沿った自動ルーティングや、`@/components`・`@/composables`の自動`import`ができる等、開発DXのかなり高いフロントエンドフレームワークになっています。

```typescript:@/composables/useExample.ts
import { Ref } from 'vue'

export type Example = {
  state: Ref<number | null>
  fetch: () => Promise<void>
}

export function useExample(): Example {
  const state = useState<number | null>('example', () => null)

  async function fetch(): Promise<void> {
    state.value = await something()
  }

  return {
    state: readonly(state),
    fetch,
  }
}
```

```vue:index.vue
<script setup lang="ts">
// ↓ 必要ない
// import { useExample } from '@/composables/useExample'

const example = useExample()

onMounted(async () => {
  await example.fetch()
})
</script>
```

この前に安定版がとうとう出ましたね！

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">【Nuxt 3、ついに登場】<br><br>＼📣Nuxt 3の正式リリース日は11/16（水）です！／ <a href="https://t.co/DhFAtox0IZ">https://t.co/DhFAtox0IZ</a></p>&mdash; NuxtLabs Japan by ZEN Advisor (@zen_nuxtlabs_jp) <a href="https://twitter.com/zen_nuxtlabs_jp/status/1591234325940412416?ref_src=twsrc%5Etfw">November 12, 2022</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

もちろんTypeScriptも使っています。
ここで必要な人間部品として、僕は本環境の「`as`・`any`などのunsafeコ□すマン」として活動しています。

TypeScriptはunsafeの温床で、レビューで叩き落としていかない限り、**自然に型と値のふるまいが乖離していきます**。
絶対unsafeコ□していこうな。

- 全TypeScriptプログラマーが読むべき参考資料↓
    - [TypeScriptのunsafeな操作まとめ](https://qiita.com/kgtkr/items/1c136e1e4ccee8928bc8)

本構成で特筆すべき部分は、**ステート管理にライブラリを使っていない**ところです。
composablesで十分だと考えているからです。
`useState()`いいぞ～！

`<script setup>`はもしかしたら知らない人がいるかもしれません。
これはVue3の`setup()`を**かなり**簡潔に書ける素晴らしい機能です。

Svelteの`<script>`に似ているかもしれません。
Svelteを知らない？
大丈夫、僕もそんなに知らない。

- [SFC `<script setup>` | Vue.js](https://v3.ja.vuejs.org/api/sfc-script-setup.html#sfc-script-setup)

### Linter

Linter三銃士をつれてきたよ :)
Liter三銃士！？

やはりコーディングルールは自動化されていなければいけません。
ということで某所でも、`eslint, stylelint, prettier`で自動化をしています。

eslintの`"extends"`はこのように、堅牢にしています。

```json
  "extends": [
    "@nuxtjs/eslint-config-typescript",
    "plugin:prettier/recommended",
    "plugin:nuxt/recommended",
    "eslint:recommended",
    "google",
    "prettier",
    // ...
  ],
```

prettierではセミコロンレス・`''`強制をしています。
この2つは、基本的人権なんだよなあ。
TypeScriptのおかげでできる設定です。たすかる。

```json
{
  "semi": false,
  "singleQuote": true
}
```

### Tests
#### Unit Tests

最後にテスト環境です。

Unitテストは`vitest`を使っています。

余談ですが、`vitest`ではIn-source testingが使えます。
今は某所環境では`.spec.ts`に記述するようにしていますが、こちらに切り替えるのもありよりのありですね！
本当はdoctestがしたい。

- [In-source testing | Guide | Vitest](https://vitest.dev/guide/in-source.html#in-source-testing)
- [doctest --- 対話的な実行例をテストする — Python 3.11.0b5 ドキュメント](https://docs.python.org/ja/3/library/doctest.html)

ちなみにテストではunsafe操作を個人的に許可しています。
テストは必ず停止し、そして異常はすぐに発現するからです。

テストは必ず停止する。そうだよね？

```typescript:SomeComponent.spec.ts
import { test, expect } from 'vitest'
import { mount } from '@vue/test-utils'
import SomeComponent from '@/components/SomeComponent.vue'

test('matches with snapshot', () => {
  const wrapper = mount(SomeComponent, {})
  expect(wrapper.getCurrentComponent()).toBeTruthy()
  expect(wrapper.html()).toMatchSnapshot()
})

test('shows a text when a button clicked', async () => {
  const wrapper = mount(SomeComponent)
  await wrapper.get('.some-button').trigger('click')
  expect(wrapper.get('some-text').text()).toBe('Clicked!')
})
```

In-source testingの例

```typescript:@/modules/array.ts
export const range = (begin: number, end: number) => ([...Array(to - from)].map((_, i) => (from + i)))

// モジュール内にテストが直接書ける！
if (import.meta.vitest) {
  const { it, expect } = import.meta.vitest

  it('includes end number', () => {
    expect(range(0, 10)).toContain(10)
  })
}
```

#### E2E Tests

vitestとplaywrightのうまい繋ぎ方が見つからなかったため、`nuxi dev`で建てたサーバーに直接テストを叩いています。

ちなみにplaywrightは、`.webServer`を設定すると、テスト実行時に勝手にサーバーを上げ下げしてくれます。
たすかる～～ :pray:

```typescript
import { devices, type PlaywrightTestConfig } from '@playwright/test'

/**
 * See https://playwright.dev/docs/test-configuration.
 */
const config: PlaywrightTestConfig = {
  // ...

  /* Run your local dev server before starting the tests */
  webServer: {
    command: 'nuxi dev',
    port: 3000,
  },

  // ...
}
```

テスト用モックAPIサーバーにはMSWを使っています。

- [MSW - Seamless API mocking library for browser and Node | Mock Service Worker](https://mswjs.io/)

`@/mocks/server`にテスト用サーバーが設定されています。

```typescript
import { test, expect } from '@playwright/test'
import { server } from '@/mocks/server'

test.describe('/index.html', () => {
  test.beforeAll(() => {
    server.listen()
  })

  test.afterAll(() => {
    server.close()
  })

  test.beforeEach(async ({ page }) => {
    await page.goto('http://localhost:8080')
  })

  test('shows a text when a button clicked', async ({ page }) => {
    await page.locator('.some-container > .button').click()
    expect(await page.locator('.some-container > p').textContent()).toBeDefined()
  })

  test('shows a todo list', async ({ page }) => {
    const state = await getSomeState()
    const lis = await page.locator('.todo-list > .list > .todo')
    if (state.data.todos.length > 0) {
      expect(lis).not.toBe(0)
    } else {
      expect(lis).toBe(0)
    }
  })
})
```

vitestとplaywrightはまだ連携できないけど、ほら、E2Eテストだし、一周回って逆にユーザーエンドに近いから、利点だよね！

## 終わり

なかなかいい環境にできたと思います。
開発が楽しいベース作りをしよう！
