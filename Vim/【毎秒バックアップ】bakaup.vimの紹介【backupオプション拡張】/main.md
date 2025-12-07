---
title: 【毎秒バックアップ】bakaup.vimの紹介【backupオプション拡張】
tags: Vim Git バックアップ vim-plugins neovim
---

![header.gif](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/d354e701-c07e-490b-8300-f936e3d47288.gif)

## はじめに

「あ、さっき消したコード、やっぱり必要だった…！」

「2時間前の状態に戻したいけど、gitにコミットしてない…」

「gitの操作間違えて、一度もコミットしていないファイルを消してしまった…」

Vimで作業していて、こんな経験はありませんか？

Vimには標準で`'backup'`オプションがあります。
これは`:write`の前に、元のファイルのバックアップを作成する機能です。

しかし、このバックアップは**1つ前の状態しか保存しません**。

```vim
" Vimの標準backup
:set backup
" → todo.md~ が作成される（1つ前の状態のみ）
```

つまり、何度`:write`しても、**最後の1つ前のバックアップ**しか残らないのです。
「3時間前の状態」や「今日の午前中の状態」に戻すことはできません。

- - -

そこで私はVimプラグイン、**bakaup.vim**を開発しました。

- [bakaup.vim - GitHub](https://github.com/aiya000/bakaup.vim)

bakaup.vimは、**毎回の`:write`でタイムスタンプ付きのバックアップを自動作成**するプラグインです。
Vimの標準`'backup'`オプションを拡張し、**日時・時刻ベースの完全なバージョン履歴**を提供します。

- - -

（時刻ベース毎に保存されるのをwatchする様子。）

![demo.gif](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/9301d877-13c4-41e5-99fc-32d84e552135.gif)

## 特徴

- 全体と詳細
    - [README.md - bakaup.vim](https://github.com/aiya000/bakaup.vim/blob/master/README.md)
    - [docs - bakaup.vim](https://github.com/aiya000/bakaup.vim/blob/master/doc/bakaup.vim.txt)

### 毎回の保存でタイムスタンプ付きバックアップ

bakaup.vimは、**毎回の`:write`**で新しいバックアップを作成します。

例えば、`/home/you/todo.md`を**2016-10-06 11:06**に保存すると、内容が

```
~/.vim_backup/2016-10-06/%home%you%todo.md_at_11:06
```

に保存されます。

**11:42**に保存すると

```
~/.vim_backup/2016-10-06/%home%you%todo.md_at_11:42
```

に保存されます。

```shell-session
$ cd ~/.vim_backup/2016-10-06 && ls %home%you%todo.md*
%home%you%todo.md_at_11:06
%home%you%todo.md_at_11:42
```

bakaup.vimを使うと、**好きな保存時点に戻れるのです**。
数時間前に削除した重要なコードを思い出したときも安心です！
gitよりも外側の安心を得たい、万が一を考える、あなたのために。

![preview](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/518ecb4a-0921-4548-83a8-b6ac02145eea.png)

### Vimの標準backupとの比較

Vim標準の`'backup'`オプションとbakaup.vimの違いを表にまとめます。

| 機能 | Vim標準の`'backup'` | bakaup.vim |
|------|---------------------|------------|
| バックアップ頻度 | 上書き前のみ | 毎回の`:write` |
| 履歴の深さ | **1つ**（最新のみ） | **無制限** |
| タイムスタンプ | なし | **時:分の精度** |
| 整理 | 同ディレクトリor単純なバックアップディレクトリ | **日付ごとにディレクトリ整理** |
| 復元 | 上書きリスクあり | 安全な履歴閲覧 |

**Vim標準のbackup:** `todo.md`を何度保存しても、`todo.md~`が**1つ**だけ（直前のバージョンのみ）。

**bakaup.vim:** 保存するたびに新しいタイムスタンプ付きバックアップが作成され、**完全なバージョン履歴**が得られます！

<!-- TODO: 比較図を追加
【作成方法】
オプション1: 実際のディレクトリを並べて撮影
  1. 2つのターミナルを開く
     左: Vim標準のbackupを使用したディレクトリ
       $ ls -la /path/to/normal/
       # todo.md と todo.md~ が1つだけ
     右: bakaup.vimのバックアップディレクトリ
       $ ls -la ~/.vim_backup/$(date +%Y-%m-%d)/
       # 複数のタイムスタンプ付きファイル

  2. 両方のターミナルを並べてスクリーンショット

オプション2: 図解を作成（draw.io、Excalidraw など）
  左側: 「Vim標準」
    todo.md
    todo.md~  ← 1つだけ

  右側: 「bakaup.vim」
    2025-01-15/
      %home%user%todo.md_at_10:30
      %home%user%todo.md_at_11:15
      %home%user%todo.md_at_14:42
      %home%user%todo.md_at_16:08  ← 無制限！

推奨: 2カラムの比較図、矢印で違いを強調
-->

### 使用例

bakaup.vimは、こんなシーンで役立ちます：

- **誤って削除したコードの復元** - 何時間も前のバージョンから復元可能
- **作業履歴のレビュー** - ドキュメントがどう進化したか確認
- **安全な実験** - いつでも戻れるので、思い切った変更を試せる
- **gitリポジトリ不要** - 設定ファイル、メモ、スクリプト、何でもOK！
- **gitコミットより細かい粒度** - 保存するたびに記録される

特に、設定ファイルの編集やドキュメント作成など、**gitを使わない作業**で威力を発揮します。

## インストール方法

### dein.vim

```vim
call dein#add('aiya000/bakaup.vim')
```

またはtomlを使う場合：

```toml
[[plugins]]
repo = 'aiya000/bakaup.vim'
```

遅延読み込みする場合（toml）：

```toml
[[plugins]]
repo = 'aiya000/bakaup.vim'
on_cmd = [
    'BakaupBackupExecute',
    'BakaupEnable',
    'BakaupDisable',
    'BakaupArchiveBackups',
    'BakaupSetBackupDir',
    'BakaupExplore',
    'BakaupTexplore',
    'BakaupVexplore',
    'BakaupSexplore',
]
```

### lazy.nvim

```lua
{
  'aiya000/bakaup.vim',
  -- オプション設定
  -- init = function()
  --  vim.g.bakaup_auto_backup = false -- 自動バックアップを無効化
  --  vim.g.bakaup_backup_dir = vim.fn.expand('~/my_backups') -- バックアップディレクトリをカスタマイズ
  -- end,
}
```

**自動バックアップはデフォルトで有効です！**
インストールしてVim/Neovimを再起動すれば、bakaup.vimが有効になり、毎回の`:write`でバックアップが作成されるようになります。

バックアップディレクトリなどの設定をカスタマイズしたい場合は、
プラグインが読み込まれる前に変数を設定するため、dein.vimなら`hooks`・lazy.nvimなら`init`関数を使ってください。
（lazy.nvimでVimプラグイン（Not **Neovim**プラグイン）を使うときは、`config`だとタイミングが遅い場合が多いです。`init`を使ってください。）

## 使い方

**自動バックアップはデフォルトで有効です！**

bakaup.vimをインストールしてVim/Neovimを再起動すれば、毎回の`:write`で自動的にバックアップが作成されます。

逆に自動バックアップを無効にしたい場合は
- `g:bakaup_auto_backup = v:false`
- `vim.g.bakaup_auto_backup = false`
を設定してください。

### カスタマイズ（オプション）

バックアップディレクトリをカスタマイズする場合：

```vim
let g:bakaup_backup_dir = expand('~/my_custom_backups')
```

自動バックアップを無効化する場合（前述のとおり）：

```vim
let g:bakaup_auto_backup = v:false
```

また、`:BakaupEnable`と`:BakaupDisable`コマンドでバックアップのオン/オフを切り替えられます。

### 手動バックアップ

自動バックアップを無効にしている場合や、今すぐバックアップを作りたい場合：

```vim
:BakaupBackupExecute
```

## ドキュメント

bakaup.vimでは、充実したドキュメントを用意しています。

### ドキュメント

- **[README.md](https://github.com/aiya000/bakaup.vim/blob/master/README.md)** - 概要と使い方
- **[ヘルプドキュメント](https://github.com/aiya000/bakaup.vim/blob/master/doc/bakaup.vim.txt)** - 完全なリファレンス
    - 全コマンドの詳細
    - 設定変数の説明
    - 使用例

この記事では触れきれなかった詳細な情報について知りたい方は、ぜひドキュメントをご覧ください。

Vim内からも`:help bakaup.vim`でドキュメントを読むことができます。

### リンク

- GitHubリポジトリ: [aiya000/bakaup.vim - GitHub](https://github.com/aiya000/bakaup.vim)

## まとめ

**bakaup.vim**は、Vimの標準`'backup'`オプションを拡張し、完全なバージョン履歴を提供するプラグインです。

この記事で紹介した主な特徴をまとめます：

- **毎回の保存でバックアップ**: `:write`のたびにタイムスタンプ付きバックアップを作成
- **無制限の履歴**: Vim標準の1つだけでなく、無制限にバックアップを保存
- **日付ごとに整理**: `{backup_dir}/{date}/{file}_at_{time}`形式で整理
- **デフォルトで有効**: インストールするだけで自動バックアップが始まる

「間違って消しちゃった…」「さっきの状態に戻したい…」という不安から解放されたい方は、ぜひbakaup.vimを試してみてください。

- **リポジトリ**: [aiya000/bakaup.vim](https://github.com/aiya000/bakaup.vim)

皆さんのVim/Neovimライフが、より安心で、より生産的になることを願っています🐕✨
