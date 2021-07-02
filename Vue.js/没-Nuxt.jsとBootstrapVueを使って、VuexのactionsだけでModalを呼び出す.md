これができるのって、画面遷移時だけじゃん
おわり

# Nuxt.js と BootstrapVue を使って、Vuex の actions だけで Modal を呼び出す

# 注意

今回はvuexの代わりにtyped-vuexを使用しています。
適宜読み替えてください。

# 結
## ModalInfo

下記を参照して、使用したい Modal（`BModal` | `b-modal`）のディレクティブに応じて、`ModalInfo`にプロパティを追加してください。

- [Modal | Components | BootstrapVue](https://bootstrap-vue.org/docs/components/modal)

今回の例では Modal の`title`に応じて、`ModalInfo#title`を追加してあります。

```typescript:data/ModalInfo.ts
export interface ModalInfo {
  /**
   * ところで「英単語は省略しない派」なのにmessageだけはmsgって書きたくなるのは僕だけですか？
   * message、頻度に対して単語長が長い……><
   */
  msg: string

  title?: string
}
```

## layouts/default.vue

ここにModalを置いておけば、全てのページ（`page/*.vue`）にModalが配置されることになります。

また`mounted()`でstoreの読み取りを行ってあげることで、

```typescript:layouts/default.vue
<template>
  <div>
    <BModal
      id="default-modal"
      :ok-only="true"
      :title="modalTitle"
      :modal-class="$style.modal"
      @click="initModalInfo"
    >
      {{ modalMsg }}
    </BModal>

    <div class="my-4">
      <Nuxt />
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator'

@Component
export default class LayoutDefault extends Vue {
  private get modalTitle(): string {
    return this.$accessor.modalInfo?.title ?? ''
  }

  private get modalMsg(): string {
    return this.$accessor.modalInfo?.msg ?? ''
  }

  private initModalInfo(): void {
    this.$accessor.hideModal()
  }

  private async mounted(): Promise<void> {
    try {
      if (this.$accessor.modalInfo !== null) {
        this.$bvModal.show('default-modal')
      }
    } catch (e) {
      logger.throwError(loggerTag, e)
    }
  }
}
</script>
```
