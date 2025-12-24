![](top.gif)

## メリークリスマス！

メリークリスマス！🌴
この記事は[Vim Advent Calendar 2025](https://adventar.org/calendars/11912)最終日、12/25の記事です🦌🎅✨

Neovimに一年の感謝を込めて書きました。
対戦よろしくお願いします！

## 「あっ、いまメモしたい！」その瞬間、窓が開く

プログラミングをしていると、こんな瞬間ありませんか？

- 「急にアイデアが浮かんだ！**いますぐメモしたい！**」
- 「このコード動くかな？**パッと試したい！**」
- 「ちょっと計算したい！**今すぐPythonを開きたい！**」
- 「APIのレスポンスを整形したい！**一時的にJSON書きたい！**」

そんなとき、ファイル名を考えたり、保存場所を決めたり...そんな手間、いりません。

**nvim-mado-scratch** なら、コマンド一発でスクラッチバッファが開きます。

- [nvim-mado-scratch - GitHub](https://github.com/aiya000/nvim-mado-scratch)

- - -

- `:MadoScratchOpenFile md float-aspect 0.8x0.9`: Markdown**ファイル**を**float window**で開く
- `:MadoScratchOpen ts vsp`: TypeScriptバッファを**一時バッファ**として開く（例えばそのまま`:QuickRun`ができる）
- `:MadoScratchOpenFile py sp 50`: Pythonファイルを現在のウィンドウを押しつぶして（縦サイズ50で開いて）、大きくPythonバッファを開く（もちろんこちらも`:QuickRun`などと連携が可能）
- `:MadoScratchOpenFile json float-fixed 80x80`: JSONファイルを固定サイズのfloat windowで開く。ファイルとして開くので`:!jq %`などもできる

### 「mado（窓）」に込めた想い

プラグイン名の「mado」は日本語で「窓」を意味します。
フローティングウィンドウ（浮遊する窓）として、あなたの作業空間にそっと現れる。
そんなイメージで名付けました。

必要なときにパッと開いて、用が済んだらサッと閉じる。
そんな軽快さを感じてほしいのです。

## どどんと発表！ フローティングウィンドウ対応！

nvim-mado-scratchの最大の特徴、それは

**フローティングウィンドウでスクラッチバッファを開けること！**

他の多くのスクラッチバッファプラグインは、splitやvsplitでの表示が中心です。
でも、nvim-mado-scratchは違います。

```vim
:MadoScratchOpen {filetype} {float-fixed|float-aspect} [size]
```

例:
```vim
:MadoScratchOpen md float-aspect 0.9x0.8
```

たったこれだけで、スクラッチバッファが**フローティングウィンドウ**でふわっと登場します。

![](float-aspect-demo.gif)

また[「インストール方法」](#インストール方法)セクションで説明しますが、セットアップオプションを`float-aspect`・`float-fixed`に設定することで、コマンド引数に明示的に指定しなくてもフローティングウィンドウで開けるようになります。

- - -

モダンなエディタっぽい、今風のかっこいい見た目。
作業中の画面を邪魔せず、必要な情報だけをサッと表示。
新世代のスクラッチバッファプラグイン。

それが**nvim-mado-scratch**です。

[^combination-with-cmdpalette]

[^combination-with-cmdpalette]: この記事のスクリーンショットは、nvim-mado-scratchを開く際に[cmdpalette.nvim](https://github.com/hachy/cmdpalette.nvim)を使用して撮っています。cmdpalette.nvimもfloat window最強説を唱える同志です。ぜひおすすめいたします。

- - -

もちろん他のscratchバッファ系プラグインのように、`:split`、`:vsplit`、`:tabnew` での表示もサポートしています。
['scratch.vimの機能を完全包含'](#scratch.vimの機能を完全包含)セクションを参照してください。

### nvim-mado-scratchって何？

改めて説明すると、**nvim-mado-scratch** は

- **ファイルパスを気にせず、瞬時にスクラッチバッファを開けるプラグイン**
- **Neovimのfloat windowファースト**にした設計
- キーマップ一つ、コマンド一つで即座に起動
- メモ書き、コードスニペットのテスト、一時的な作業に最適

「とにかく速く、とにかく手軽に」がコンセプトです。

### 基本的な使い方

`:MadoScratchOpenFile`で、scratchバッファ（メモファイル）を開きます。

```vim
" デフォルト設定でスクラッチバッファを開く
:MadoScratchOpenFile

" Markdownファイルとして開く
:MadoScratchOpenFile md

" TypeScriptファイルとして、vertical splitで開く
:MadoScratchOpenFile ts vsp

" 拡張子なしで開く
:MadoScratchOpenFile --no-file-ext
```
`:MadoScratchOpenFile`はファイルとしてscratchバッファを開きますが、
保存をしなくていい・Neovimを一度閉じて消えてしまってもいい場合は
`:MadoScratchOpen`が使えます。

```vim
:MadoScratchOpen
:MadoScratchOpen md
:MadoScratchOpen ts
:MadoScratchOpen --no-file-ext
```

たったこれだけ。
ファイル名も保存場所も考える必要なし。
思いついたら、すぐ書き始められます。

## 開き方は自由自在！ あなた好みの方法で

nvim-mado-scratchは、フローティングウィンドウだけじゃありません。
あなたの好みに合わせて、いろいろな開き方ができます。

### フローティングウィンドウ（イチオシ！）

まずはやっぱり、フローティングウィンドウ。

#### アスペクト比指定（float-aspect）

画面サイズに応じて自動調整されるアスペクト比指定も便利です：

```vim
" 画面の90%幅、80%高さでMarkdownを開く
:MadoScratchOpen md float-aspect 0.9x0.8

" 画面の80%幅、80%高さでHaskellを開く
:MadoScratchOpen hs float-aspect 0.8x0.8
```

#### 固定サイズ指定（float-fixed）

```vim
" 80x24のフローティングウィンドウでMarkdownを開く
:MadoScratchOpen md float-fixed 80x24

" 100x30の大きめウィンドウでPythonを開く
:MadoScratchOpen py float-fixed 100x30
```

**`幅x高さ`** の形式で固定サイズを指定できます。
広々使いたいときは大きく、ちょっとしたメモなら小さく。
自由自在です。
設定ファイルでデフォルトのサイズを決めておくこともできます：

```lua
require('mado-scratch').setup({
  default_open_method = {
    method = 'float-fixed',
    size = { width = 80, height = 24 }
  },
  -- または、アスペクト比指定（画面の80%幅、80%高さ）
  -- default_open_method = {
  --   method = 'float-aspect',
  --   scale = { width = 0.8, height = 0.8 }
  -- },
})
```

※ `float-aspect`および`float-fixed`は[plenary.nvim](https://github.com/nvim-lua/plenary.nvim)に依存します。でも全Neovimmerはplenary.nvimを入れているので、問題ないですよね？

### 水平分割・垂直分割・新規タブ

もちろん、伝統的な開き方も完全サポート。

```vim
" 水平分割（高さ5行）
:MadoScratchOpen md sp 5

" 垂直分割（幅80文字）
:MadoScratchOpen ts vsp 80

" 新規タブ
:MadoScratchOpen js tabnew
```

設定例：

```lua
-- 水平分割をデフォルトに
require('mado-scratch').setup({
  default_open_method = { method = 'sp', height = 15 },
})

-- 垂直分割をデフォルトに
require('mado-scratch').setup({
  default_open_method = { method = 'vsp', width = 30 },
})

-- 新規タブをデフォルトに
require('mado-scratch').setup({
  default_open_method = { method = 'tabnew' },
})
```

scratchバッファをいちいち自動で`:q`したくない人向けに、以下の設定もあります。

```lua
require('mado-scratch').setup({
  default_open_method = { method = 'sp', height = 15 },
  auto_hide_buffer = {
    when_tmp_buffer = true,  -- Auto-hide temporary buffers
    when_file_buffer = true, -- Auto-hide persistent buffers
  }
})
```

`:q<CR>`(`:q Enter`)ではなく`<C-w>j`(`Ctrl+w j`)で閉じていることに注目してください。

![](sp-auto-hide-demo.gif)

### 複数のバッファを同時に管理

「メモAには設計のアイデア、メモBには調査内容、メモCにはTODOリスト...」

そんな使い分けができます。

```vim
:MadoScratchOpen md       " 最近使ったバッファを開く
:MadoScratchOpenNext md   " 新しいバッファを作成
```

nvim-mado-scratchは、スクラッチバッファに自動で連番を振ります。
トピックごとにメモを分けられて、とっても便利。

整理したくなったら、一括削除もできます：

```vim
:MadoScratchClean
```

これで全てのスクラッチバッファとファイルがきれいさっぱり。

なお、ファイルの整理は意図的にこのプラグインには載せていません。
oil.nvimをおすすめします。

## vim-quickrunと組み合わせて最高になろう！

nvim-mado-scratchは、[vim-quickrun](https://github.com/thinca/vim-quickrun)と非常に相性がいいです。

### 書いて即実行！

```vim
" TypeScriptでコードを書く
:MadoScratchOpen ts

" （コードを書く）

" 即実行！
:QuickRun
```

![](quickrun-demo.gif)

**書く → 試す → 直す → 試す**

このサイクルが、信じられないくらい速い。

### どんな言語でも試せる

当然ですが、引数に拡張子を指定する単純仕様なので、任意のプログラミング言語で、そしてプログラミング言語以外のファイルタイプの全てで使えます。

- **Python**: ちょっとした計算やデータ処理
- **TypeScript**: 新しいAPIの動作確認
- **Rust**: アルゴリズムの実装テスト
- **Bash**: コマンドの組み立て確認
- **SQL**: クエリの動作確認

言語を問わず、思いついたコードをすぐ試せます。

### 永続バッファなら自動保存も

一時的な実験なら `:MadoScratchOpen` で十分。
でも、「このコード、後で見返したいな」というときは：

```vim
:MadoScratchOpenFile ts
```

こちらを使うと、実際のファイルとして保存されます。
しかも、**テキストを変更すると自動保存**されるので安心。

設定でファイルの保存場所もカスタマイズできます：

```lua
require('mado-scratch').setup({
  file_pattern = {
    when_tmp_buffer = '/tmp/mado-scratch-tmp-%d',
    when_file_buffer = vim.fn.expand('~/scratch/%d'),
  },
  auto_save_file_buffer = true, -- 自動保存を有効化（デフォルト）
})
```

これで `~/scratch/` ディレクトリに永続バッファが保存されます。
Prettierの設定ファイル（`.prettierrc`）などを置いておけば、整形も自動でできます。

## scratch.vimの機能を完全包含

既に[scratch.vim](https://github.com/mtth/scratch.vim)を使っている方もいるかもしれません。
scratch.vimは素晴らしいプラグインです。
scratch.vim以外にも、素晴らしいscratchバッファプラグインはいくつかあります。

しかし敢えて言います。

**nvim-mado-scratchはそれを包含し、さらに多くの機能を提供します。**

Neovimのモダンな機能を対象に設計されているからです。

### nvim-mado-scratchの追加機能

全てのアバウトは['nvim-mado-scratch - GitHub'](https://github.com/aiya000/nvim-mado-scratch)に記載されていますが、いくつか抜粋します。

#### 1. 柔軟なバッファ管理

- **複数バッファの管理**（`:MadoScratchOpenNext`）
    - 連番で管理される複数のスクラッチバッファ
    - 最近使ったバッファへの素早いアクセス（`:MadoScratchOpen`）
    - scratch.vimは1つのバッファしか扱えませんが、nvim-mado-scratchなら複数のトピックごとにメモを分けられます
    - 詳細: `:help :MadoScratchOpen` と `:help :MadoScratchOpenNext`

#### 2. バッファタイプの選択肢

- **一時バッファ** と **永続バッファ** の使い分け
    - 一時的な作業には `:MadoScratchOpen`
    - 保存が必要な作業には `:MadoScratchOpenFile`
    - 永続バッファは自動保存機能付き
    - 詳細: `:help :MadoScratchOpen` と `:help :MadoScratchOpenFile`

#### 3. 豊富なカスタマイズオプション

- **ファイルタイプ指定**: シンタックスハイライト、`:QuickRun` との連携など
- **開き方の選択**: `:split`、`:vsplit`、`:tabnew`、**フローティングウィンドウ**
- **サイズ制御**: バッファの高さ・幅を自由に指定
- **自動非表示の挙動**: scratch.vim互換モード（後述）
- **ファイル保存場所のカスタマイズ**:
  ```lua
  -- 一時バッファと永続バッファで保存場所を分ける
  require('mado-scratch').setup({
    file_pattern = {
      when_tmp_buffer = '/tmp/mado-scratch-tmp-%d',    -- :MadoScratchOpen用
      when_file_buffer = vim.fn.expand('~/scratch/%d'), -- :MadoScratchOpenFile用
    }
  })
  -- 永続バッファのディレクトリ（上記例では ~/scratch）に
  -- `.prettierrc` などの設定ファイルを置いておけば、
  -- フォーマッターなどとの連携も簡単です
  ```

### scratch.vim互換モード

「scratch.vimの挙動が好きだったんだよな…」という方も安心してください。

nvim-mado-scratchには、**scratch.vim互換モード**があります。

scratch.vimと同じように、ウィンドウから離れたときに自動でバッファを隠す設定ができます：

```lua
-- 両方のバッファタイプで自動非表示を有効化（scratch.vimと同じ挙動）
require('mado-scratch').setup({
  auto_hide_buffer = {
    when_tmp_buffer = true,
    when_file_buffer = true,
  },
})
```

一時バッファだけ自動非表示にすることもできます：

```lua
require('mado-scratch').setup({
  auto_hide_buffer = {
    when_tmp_buffer = true,  -- 一時バッファのみ自動非表示
  },
})
```

逆に、永続バッファだけ自動非表示にすることもできます：

```lua
require('mado-scratch').setup({
  auto_hide_buffer = {
    when_file_buffer = true, -- 永続バッファのみ自動非表示
  },
})
```

既存のscratch.vimユーザーも、安心して移行できます。

## インストール方法

### lazy.nvimを使う場合

```lua
{
  'aiya000/nvim-mado-scratch',
  dependencies = {
    'nvim-lua/plenary.nvim', -- フローティングウィンドウを使う場合は必須
  },
  opts = {
    -- お好みで設定（デフォルト値。それぞれ省略できます。お好みの項目のみ追加・変更してください）
    file_pattern = {
      when_tmp_buffer = '/tmp/mado-scratch-tmp-%d',
      when_file_buffer = '/tmp/mado-scratch-file-%d',
    },
    default_file_ext = 'md',
    default_open_method = { method = 'sp', height = 15 },
    auto_save_file_buffer = true,
    use_default_keymappings = false,  -- trueにするとデフォルトのキーマップが有効に
    auto_hide_buffer = {
      when_tmp_buffer = false,
      when_file_buffer = false,
    },
  },
}
```

### フローティングウィンドウをデフォルトにする場合

lazy.nvim

```lua
{
  'aiya000/nvim-mado-scratch',
  dependencies = {
    'nvim-lua/plenary.nvim', -- 必須！
  },
  opts = {
    -- アスペクト比指定の場合
    default_open_method = {
      method = 'float-aspect',
      scale = { width = 0.8, height = 0.8 }
    },
    -- または、固定サイズ指定の場合
    -- default_open_method = {
    --   method = 'float-fixed',
    --   size = { width = 80, height = 24 }
    -- },
    -- その他の設定...
    },
  },
}
```

### デフォルトキーマッピング

設定で`use_default_keymappings = true`にすると、以下のキーマップがすぐに使えます。

```lua
require('mado-scratch').setup({
  use_default_keymappings = true,
})
```

```vim
" すぐ開くコマンド（デフォルト設定で即実行）
<leader>b  → :MadoScratchOpen
<leader>B  → :MadoScratchOpenFile

" 引数を指定して開くコマンド（カーソルが引数入力位置で待機）
<leader><leader>b  → :MadoScratchOpen
<leader><leader>B  → :MadoScratchOpenFile
```

もちろん、独自のキーマップを定義することもできます。

```lua
-- カスタムキーマップの例
vim.keymap.set('n', '<leader>s', '<Cmd>MadoScratchOpen<CR>', { silent = true })
vim.keymap.set('n', '<leader>S', '<Cmd>MadoScratchOpenFile<CR>', { silent = true })
```

筆者は以下のように設定をしています。

```lua
-- lazy.nvim設定

{
  'aiya000/nvim-mado-scratch',
  dependencies = {
    'nvim-lua/plenary.nvim',
  },
  opts = {
    default_open_method = {
      method = 'float-aspect',
      scale = { width = 0.8, height = 0.8 }
    },
  },
}
```

```lua
vim.keymap.set('n', '<leader>b', '<Cmd>MadoScratchOpenFile md<CR>', { silent = true })
vim.keymap.set('n', '<leader>B', '<Cmd>MadoScratchOpenFileNext md<CR>', { silent = true })
vim.keymap.set('n', '<leader><leader>b', ':<C-u>MadoScratchOpenFile ')
```

### 設定オプション一覧

```lua
require('mado-scratch').setup({
  -- 一時バッファと永続バッファのファイルパターン
  file_pattern = {
    when_tmp_buffer = '/tmp/mado-scratch-tmp-%d',     -- :MadoScratchOpen用
    when_file_buffer = vim.fn.expand('~/scratch/%d'), -- :MadoScratchOpenFile用
  },

  -- デフォルト設定
  default_file_ext = 'md', -- デフォルトのファイル拡張子
  default_open_method = { method = 'sp', height = 15 }, -- デフォルトの開き方（詳細は :help mado-scratch-configuration）

  -- 挙動オプション
  auto_save_file_buffer = true,      -- 永続バッファの自動保存（InsertLeaveイベント時とバッファ破棄前）
  use_default_keymappings = false,   -- デフォルトキーマップを有効化

  -- 自動非表示設定（scratch.vim互換）
  auto_hide_buffer = {
    when_tmp_buffer = false,         -- 一時バッファを自動非表示
    when_file_buffer = false,        -- 永続バッファを自動非表示
  },
})
```

詳細は['nvim-mado-scratch - GitHub'](https://github.com/aiya000/nvim-mado-scratch)のREADME.md・および`:help mado-scratch.nvim` を参照してください。

## まとめ：Neovimユーザーに贈る、最高のスクラッチバッファ体験

nvim-mado-scratchは、こんな人におすすめです：

- **「思いついたらすぐメモしたい」** - ファイル名も保存場所も考えたくない
- **「コードスニペットをサクッと試したい」** - vim-quickrunとの組み合わせで最強
- **「モダンなエディタっぽい見た目が好き」** - フローティングウィンドウでかっこよく
- **「scratch.vimから乗り換えたい」** - 完全包含＋追加機能で、さらに便利に
- **「複数のメモを同時に管理したい」** - 連番管理で整理整頓

### このプラグインの魅力

1. **フローティングウィンドウ対応** - 他のスクラッチバッファプラグインにはない、モダンな見た目
2. **超高速起動** - キーマップ一つ、コマンド一つで即座に開く
3. **vim-quickrunとの相性抜群** - プログラマーの遊び場として最高
4. **scratch.vim完全包含** - 既存ユーザーも安心して移行できる
5. **柔軟なカスタマイズ** - 開き方、サイズ、ファイルタイプ、保存場所など、すべて自由自在
6. **モダンなLua実装** - Neovim専用、高速で型安全
7. **複数バッファ管理** - トピックごとにメモを分けて整理しやすい

「ファイルパスを気にせず、思いついたらすぐ書く」

そんな軽快なNeovimライフを、nvim-mado-scratchで始めてみませんか？

- - -

それでは、じゃあの！
筆者からのクリスマスプレゼントでした！
もらえ！
