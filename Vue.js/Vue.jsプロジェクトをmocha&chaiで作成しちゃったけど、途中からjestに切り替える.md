# Vue.jsプロジェクトをmocha&chaiで作成しちゃったけど、途中からjestに切り替える

Hi :)

僕はWebアプリケーションを開発しています。
フレームワークには、Vue.jsを採用しました。
プロジェクトを始める際に`vue-cli`を使って初期化しました。

bootstrap4・TypeScriptも使用しています。

しかし……
やっと最近testがかける状態になったので書いていたところ、mochaちゃんに

「は？　ブラウザビルドされたFirebase？　知らないわ。私はnode派なの。」 [^a]

[^a]: mochaがテストにnode.jsを使うせいで、ブラウザビルド版のFirebaseのコードがうまくコンパイルできない。実装間の微妙な差異による。

と言われてしましました。

[まあ僕は天才ですから。環境をバッタバッタと、TypeScriptつきjestに切り替えてしまうわけです。](https://dic.nicovideo.jp/v/sm32492001)

## 主な流れ

- [TypeScript と一緒に使う | Vue Test Utils](https://vue-test-utils.vuejs.org/ja/guides/using-with-typescript.html)
- [jestのテストでcss、scss、less絡みでSyntaxErrorが起きる場合の対処法 - Qiita](https://qiita.com/github0013@github/items/303a32d3037d322e67c0)
- 他、細かなやつ

## mochaちゃんとchaiちゃんの削除

- @types/mocha
- @vue/cli-plugin-unit-mocha
- @vue/cli-plugin-babel
- chai

```shell-session
$ yarn remove @types/mocha @vue/cli-plugin-unit-mocha @vue/cli-plugin-babel chai
```

## 必要なもののインストール

- jest
- ts-jest
- vue-jest
- @types/jest
- babel-core
- @vue/cli-plugin-babel
- jest-css-modules

```shell-session
$ yarn add --dev jest ts-jest vue-jest @types/jest babel-core @vue/cli-plugin-babel jest-css-modules
```

## 環境の書き換え

```diff
diff --git a/package.json b/package.json
index 810de1c..c9dfdfd 100644
--- a/package.json
+++ b/package.json
@@ -5,7 +5,7 @@
   "scripts": {
     "serve": "vue-cli-service serve",
     "build": "vue-cli-service build",
-    "test:unit": "vue-cli-service test:unit",
+    "test": "jest",
     "lint": "vue-cli-service lint"
   },
   "dependencies": {
```

```diff
@@ -38,14 +37,38 @@
+  "jest": {
+    "moduleFileExtensions": [
+      "ts",
+      "js",
+      "json",
+      "vue",
+      "css",
+      "scss"
+    ],
+    "transform": {
+      ".*\\.(vue)$": "vue-jest",
+      "^.+\\.tsx?$": "ts-jest"
+    },
+    "testURL": "http://localhost/",
+    "testRegex": "(/tests/.+\\.(test|spec))\\.(jsx?|tsx?)$",
+    "moduleNameMapper": {
+      "^@/(.*)$": "<rootDir>/src/$1",
+      "\\.(css|scss)$": "<rootDir>/node_modules/jest-css-modules"
+    }
   }
 }
```

## テストの書き換え

```diff
index c21535f..8b10ac0 100644
--- a/tests/unit/App.spec.ts
+++ b/tests/App.spec.ts
@@ -4,7 +4,8 @@ import VueRouter from 'vue-router'
 import store from '@/store'
 import { Account } from '@/data/Account'
 import { createLocalVue, shallowMount } from '@vue/test-utils'
-import { expect } from 'chai'
+
+import 'jest'
 
 const localVue = createLocalVue()
 localVue.use(VueRouter)
@@ -24,15 +25,15 @@ describe('App.vue', () => {
   it('shows 「ログイン」 and 「アカウント登録」 before logging in', () => {
     const wrapper = shallowMount(App, { localVue, router })
 
-    expect(wrapper.find('#to-login').text()).to.contain('ログイン')
-    expect(wrapper.find('#to-signup').text()).to.contain('アカウント登録')
+    expect(wrapper.find('#to-login').text()).toBe('ログイン')
+    expect(wrapper.find('#to-signup').text()).toBe('アカウント登録')
   })
 
   it("doesn't show 「マイページ」 and 「ログアウト」 before logging in", () => {
     const wrapper = shallowMount(App, { localVue, router })
 
-    expect(() => wrapper.find('#to-mypage').text()).to.throw('find did not return')
-    expect(() => wrapper.find('#to-logout').text()).to.throw('find did not return')
+    expect(() => wrapper.find('#to-mypage').text()).toThrow('find did not return')
+    expect(() => wrapper.find('#to-logout').text()).toThrow('find did not return')
   })
 
   // Using store to test store directly
@@ -40,8 +41,8 @@ describe('App.vue', () => {
     const wrapper = shallowMount(App, { localVue, router })
     store.commit('login', { account })
 
-    expect(() => wrapper.find('#to-login').text()).to.throw('find did not return')
-    expect(() => wrapper.find('#to-signup').text()).to.throw('find did not return')
+    expect(() => wrapper.find('#to-login').text()).toThrow('find did not return')
+    expect(() => wrapper.find('#to-signup').text()).toThrow('find did not return')
   })
 
   it('shows 「マイページ」, informations on MyPage, and 「ログアウト」 after logged in', () => {
@@ -52,23 +53,23 @@ describe('App.vue', () => {
     })
     store.commit('login', { account })
 
-    expect(wrapper.find('#to-mypage').text()).to.contain('マイページ')
-    expect(wrapper.find('#to-logout').text()).to.contain('ログアウト')
+    expect(wrapper.find('#to-mypage').text()).toBe('マイページ')
+    expect(wrapper.find('#to-logout').text()).toBe('ログアウト')
 
     wrapper.find('#to-mypage').trigger('click')
     const info = wrapper
       .find('#container')
       .find('#mypage')
       .find('#informations')
-    expect(info.find('#email').text()).to.contain('なし')
-    expect(info.find('#name').text()).to.contain(account.name)
-    expect(info.find('#screenName').text()).to.contain(account.screenName)
-    expect(info.find('#profile').text()).to.contain(account.profile)
+    expect(info.find('#email').text()).toBe('なし')
+    expect(info.find('#name').text()).toBe(account.name)
+    expect(info.find('#screenName').text()).toBe(account.screenName)
+    expect(info.find('#profile').text()).toBe(account.profile)
   })
 
   it("doesn't show 「ログアウト」 after logged out", () => {
     const wrapper = shallowMount(App, { localVue, router })
     store.commit('logout', {})
-    expect(() => wrapper.find('#to-logout').text()).to.throw('find did not return')
+    expect(() => wrapper.find('#to-logout').text()).toThrow('find did not return')
   })
 })
```

## テストの実行

```shell-session
$ yarn test
yarn run v1.12.1
$ jest
 FAIL  tests/App.spec.ts
  App.vue
    ✓ shows 「ログイン」 and 「アカウント登録」 before logging in (61ms)
    ✓ doesn't show 「マイページ」 and 「ログアウト」 before logging in (10ms)
    ✓ doesn't show 「ログイン」 and 「アカウント登録」 after logged in (11ms)
    ✕ shows 「マイページ」, informations on MyPage, and 「ログアウト」 after logged in (24ms)
    ✓ doesn't show 「ログアウト」 after logged out (10ms)

  ● App.vue › shows 「マイページ」, informations on MyPage, and 「ログアウト」 after logged in

    expect(received).toBe(expected) // Object.is equality

    Expected: "なし"
    Received: "email: なし"

      62 |       .find('#mypage')
      63 |       .find('#informations')
    > 64 |     expect(info.find('#email').text()).toBe('なし')
         |                                        ^
      65 |     expect(info.find('#name').text()).toBe(account.name)
      66 |     expect(info.find('#screenName').text()).toBe(account.screenName)
      67 |     expect(info.find('#profile').text()).toBe(account.profile)

      at Object.<anonymous> (tests/App.spec.ts:64:40)

Test Suites: 1 failed, 1 total
Tests:       1 failed, 4 passed, 5 total
Snapshots:   0 total
Time:        2.732s
Ran all test suites.
error Command failed with exit code 1.
info Visit https://yarnpkg.com/en/docs/cli/run for documentation about this command.
```

はい通った！

- - - - -

おわり :hand::sunglasses:
