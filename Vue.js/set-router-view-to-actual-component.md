# vue-test-utils で router-view の実コンポーネントを find する

擬似コード（実商業プロジェクトの一部）でお送りします。

## プロジェクト構成

App.vue

```vue
<template>
  <div id="app">
    <div id="nav">
      <router-link to="/mypage" id="to-mypage">マイページ</router-link>
    </div>

    <div id="container" class="container-sm">
      <router-view />
    </div>
  </div>
</template>

<script lang="ts">
import store from 'store.ts'
import { Account } from '@/data/Account'
import { Component, Vue } from 'vue-property-decorator'

@Component
export default class App extends Vue {
  public get account(): Account | null {
    return store.state.account
  }
}
</script>
```

MyPage.vue

```vue
<template>
  <div id="mypage">
    <h2>マイページ</h2>

    <div id="informations">
      <p id="email">email: {{ account.email || 'なし' }}</p>
      <p id="name">name: {{ account.name }}</p>
      <p id="screenName">screenName: {{ account.screenName }}</p>
      <p id="profile">profile: {{ account.profile }}</p>
    </div>
  </div>
</template>

<script lang="ts">
import store from 'store.ts'
import { Component, Vue } from 'vue-property-decorator'
import { logger } from '@/data/logger'

@Component
export default class MyPage extends Vue {
  public get account(): Account | null {
    return store.state.account
  }
}
</script>
```

Account.ts

```typescript
interface Account {
  name: string
  screenName: string
  profile: string
}
```

store.ts

```typescript
import { Account } from '@/data/Account'

Vue.use(Vuex)

interface State {
  account: Account | null
}

const state: State = {
  account: {
    name: 'aiya000',
    screenName: 'あいや',
    profile: 'Haskell',
  },
}

const mutations: MutationTree<State> = {
  login(state: State, payload: { account: Account }): void {
    state.account = payload.account
  },

  logout(state: State): void {
    state.account = null
  },
}

const store = new Vuex.Store({
  state,
  mutations,
})

export default store
```

## したいこと

テスト

```typescript
const account: Account = {
  name: 'aiya000',
  screenName: 'あいや',
  profile: 'Haskell',
}

it('shows informations after logging in', () => {
  const wrapper = shallowMount(App, {})
  store.commit('login', { account })

  wrapper.find('#to-mypage').trigger('click')
  console.log(wrapper.html())

  const info = wrapper
    .find('#container')
    .find('#mypage')
    .find('#informations')
  expect(info.find('#email').text()).to.contain('なし')
  expect(info.find('#name').text()).to.contain(account.name)
  expect(info.find('#screenName').text()).to.contain(account.screenName)
  expect(info.find('#profile').text()).to.contain(account.profile)
})
```

結果

```html
<div id="app">
  <div id="nav">
    <router-link-stub to="/mypage" tag="a" event="click" id="to-mypage">マイページ</router-link-stub>
  </div>
  <div id="container" class="container-sm">
    <router-view-stub name="default"></router-view-stub>
  </div>
</div>
```

```
   shows informations after logging in
     Error: [vue-test-utils]: find did not return #mypage, cannot call find() on empty Wrapper
      at throwError (dist/js/webpack:/node_modules/@vue/test-utils/dist/vue-test-utils.js:1417:1)
      at ErrorWrapper.find (dist/js/webpack:/node_modules/@vue/test-utils/dist/vue-test-utils.js:2168:1)
      at Context.<anonymous> (dist/js/webpack:/tests/unit/App.spec.ts:64:1)
```

`#mypage`があって欲しい場所に、スタブ（`<router-view-stub name="default"></router-view-stub>`）があるので、テストが失敗してしまいました。

## 実現方法

App.spec.ts

```typescript
import App from '@/App.vue'
import MyPage from '@/views/MyPage.vue'
import VueRouter from 'vue-router'
import store from '@/store'
import { Account } from '@/data/Account'
import { createLocalVue, shallowMount } from '@vue/test-utils'
import { expect } from 'chai'

const localVue = createLocalVue()
localVue.use(VueRouter)
const router = new VueRouter()

const account: Account = {
  name: 'aiya000',
  screenName: 'あいや',
  profile: 'Haskell',
}

describe('App.vue', () => {
  beforeEach(() => {
    store.commit('logout', {})
  })

  it('shows informations after logging in', () => {
    const wrapper = shallowMount(App, {
      localVue,
      router,
      stubs: { 'router-view-stub': MyPage },
    })
    store.commit('login', { account })

    wrapper.find('#to-mypage').trigger('click')
    console.log(wrapper.html())

    const info = wrapper
      .find('#container')
      .find('#mypage')
      .find('#informations')
    expect(info.find('#email').text()).to.contain('なし')
    expect(info.find('#name').text()).to.contain(account.name)
    expect(info.find('#screenName').text()).to.contain(account.screenName)
    expect(info.find('#profile').text()).to.contain(account.profile)
  })
})
```

結果

```html
<div id="app">
  <div id="nav">
    <router-link-stub to="/" tag="a" event="click">トップ</router-link-stub> |
    <router-link-stub to="/board" tag="a" event="click">目標一覧</router-link-stub> |
    <span>
      <router-link-stub to="/mypage" tag="a" event="click" id="to-mypage">マイページ</router-link-stub> |
      <router-link-stub to="/logout" tag="a" event="click" id="to-logout">ログアウト</router-link-stub>
    </span>
  </div>
  <div id="container" class="container-sm">
    <div id="mypage" name="default">
      <h2>マイページ</h2>
      <div id="informations">
        <p id="email">email: なし</p>
        <p id="name">name: aiya000</p>
        <p id="screenName">screenName: あいや</p>
        <p id="profile">profile: Haskell</p>
      </div>
    </div>
  </div>
</div>
```

```
✓ shows informations after logging in
```

## 結論

```typescript
const wrapper = shallowMount(App, {
  stubs: { 'router-view-stub': MyPage },
})
```

💪😎👍
