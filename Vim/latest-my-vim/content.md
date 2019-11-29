# 最近ぼくがVimに設定したこと

ハロー、ハッピーVimmer :tada:

今日はVim pluginと、それに対する僕の設定を紹介します。

- [僕の.vimrc](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vimrc)
- [僕の.vimディレクトリ](https://github.com/aiya000/dotfiles/tree/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vim)

## マクロの実行をdot repeat可能にした

まずはシンプルかつ最強なやつです。

- [**kana**/vim-repeat: Vim plugin: Enable to repeat last change by non built-in commands](https://github.com/kana/vim-repeat)

dot-repeatは通常1つの動作のみを繰り返しますが、これを使えば、以下のような繰り返しが可能です。

1. `qa` **マクロ記録開始**
1. `dt(` 動作1
1. `dip` 動作2
1. `3lD` 動作3
1. `+q` マクロ記録完了
1. `@a` **マクロを発動**
1. `.` 下に移動してから、上述の`dt(`から`3l`までを**実行**
1. `.` 下に移動してから(略
1. `.` 下に移動してからry
1. **繰り返し**

![sample](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/c67e7dbc-6a79-3133-f0cf-b8a1dccdc5d7.gif)

## Webページを非同期で表示するようにした

これは自作プラグインで、まだドキュメントの整備が終わっていない（する予定がない）のですが、せっかくなので紹介させてください。

- [aiya000/vim-webpage: Allows to open web pages on a Vim's popup window.](https://github.com/aiya000/vim-webpage)

これを以下のように設定すると

- [vim-webpage 設定](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vimrc#L808)

次のようにweblio辞書をVimのpopupで表示します。

![sample](https://raw.githubusercontent.com/aiya000/vim-webpage/master/sample.gif)

ドキュメントを追加して、あとは内部のバッファをいじれるようにすれば、もっと便利になると思いますが、使う人は僕以外いないし、いらんか。という。
だれかドキュメント書いて！

## 英和翻訳を非同期で表示するようにした

weblio辞書で和英ができるようになったので、あとは英和が必要です。

- [skanehira/translate.vim: translate language plugin](https://github.com/skanehira/translate.vim)

`<leader>k`で、選択した英文を翻訳してくれるようにしました。

- [キーマッピング - translate.vim](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vimrc#L1378)

![sample](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/1c41d82b-9adf-7a5c-56b1-0600c065b96c.gif)

## 日本語関連の操作をしやすくした - vim-fmap

これは`f`キー及び`FtT`キーの4つへ、キーバインディングを提供してくれるプラグインです。

例えば以下を実行すると

```vim
nmap <leader>f <Plug>(fmap-forward-f)
FNoreMap p （  " 大文字括弧開き
```

`<leader>fpb`キーで、Vimデフォルトの`f（`を実行してくれます。

- [aiya000/vim-fmap: Support key mappings for f, like nnoremap/vnoremap/onoremap](https://github.com/aiya000/vim-fmap)

![sample](https://raw.githubusercontent.com/aiya000/vim-fmap/master/sample.gif)

僕はこれを以下のように設定して、IMEオンオフなしで日本語文字にジャンプできるようにしています！
これでnormal・visual・operatorへのいずれでも、ジャンプが可能になります :point_down:

- [設定変数と内部キーマッピング（fキーマッピング） - vim-fmap](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vimrc#L622)
- [キーマッピング - vim-fmap](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vimrc#L1458)

```vim
let g:fmap_escape_keys = ['', '', '']

augroup vimrc
  autocmd VimEnter * FNoreMap / ・
  autocmd VimEnter * FNoreMap T ・
  autocmd VimEnter * FNoreMap tt …
  autocmd VimEnter * FNoreMap '' 　
  autocmd VimEnter * FNoreMap p （
  autocmd VimEnter * FNoreMap k 「
  autocmd VimEnter * FNoreMap K 〈
  autocmd VimEnter * FNoreMap -k 『
augroup END
```

詳しくは :point_down:

- [fキーをnoremap可能にするfmap.vim - Qiita](https://qiita.com/aiya000/items/365b98a3c550c98cf286)

## 括弧の操作を最強にした -  vim-operator-surround

いわずと知れたプラグインですが、これも僕は日本語文字に活用しています。
（もちろん日本語文字以外にも。）

- [rhysd/vim-operator-surround: Vim operator mapping to enclose text objects with surrounds like paren, quote and so on.](https://github.com/rhysd/vim-operator-surround)
- [キーマッピング - vim-operator-surround](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vimrc#L1429)
- [内部キーマッピング - vim-operator-surround](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vim/autoload/vimrc/dein/hook_source.vim#L6)
    - [`vimrc#delete_mostly_inner_surround()`](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vim/autoload/vimrc.vim#L198)
    - [`vimrc#replace_mostly_inner_surround()`](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vim/autoload/vimrc.vim#L225)
    - [`vimrc#append_choose_surround()`](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vim/autoload/vimrc.vim#L243)
    - [`vimrc#append_choose_surround_wide()`](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vim/autoload/vimrc.vim#L248)

例えばカーソル下に`word`という単語があれば、normalからの`gajK`キーでその周りに`「」`を追加することができ、つまりは`「word」`することができます。

これ :point_down: の執筆で大活躍でした。（宣伝）

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr"><a href="https://twitter.com/hashtag/%E3%81%9B%E3%81%A4%E3%83%A9%E3%83%9C?src=hash&amp;ref_src=twsrc%5Etfw">#せつラボ</a> 〜圏論の基本〜<br><br>数学を全く知らない方のための本。<br>数学的な事前知識を全く仮定せず、数学を学んでいくことができます。<br><br>- 数学って？<br>- どんなところが楽しいの？<br><br>読みやすい対話形式🐬<a href="https://twitter.com/hashtag/%E6%8A%80%E8%A1%93%E6%9B%B8%E5%85%B8?src=hash&amp;ref_src=twsrc%5Etfw">#技術書典</a> <a href="https://twitter.com/hashtag/%E6%8E%A8%E3%81%97%E7%A5%AD%E3%82%8AC%E3%81%AE%E9%99%A3?src=hash&amp;ref_src=twsrc%5Etfw">#推し祭りCの陣</a><a href="https://twitter.com/techbookfest?ref_src=twsrc%5Etfw">@techbookfest</a><br><br>サークル👇️ <a href="https://t.co/GfNM1HNHjK">https://t.co/GfNM1HNHjK</a><br>（え20C） <a href="https://t.co/ypV9qXf3Zf">https://t.co/ypV9qXf3Zf</a> <a href="https://t.co/y4x1eBjKYi">pic.twitter.com/y4x1eBjKYi</a></p>&mdash; あいや🤘🙄🤘技書博「3F-す02」せつラボ (@public_ai000ya) <a href="https://twitter.com/public_ai000ya/status/1173914515806670848?ref_src=twsrc%5Etfw">September 17, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

## gina.vimのコミット画面をcommittia.vimみたいにした

gina.vimは今季マジ推しのプラグインのうち、ひとつです。

- [lambdalisue/gina.vim: Asynchronously control git repositories in Neovim/Vim 8](https://github.com/lambdalisue/gina.vim)

そのまま使うでも便利なのですが、自前でキーマッピングを追加してみました。
見てみてください！ :point_down:

![sample](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/c7b8abaa-ba59-3df6-5786-f8c81e14dd77.gif)

ちなみに、このキーマッピングの本家は :point_down:
（これを直接利用しようと考えたのですが、どうやらターミナルからgit-commitする用だったので、諦めました。もしVim上からもできるようだったら、すみません。）

- [rhysd/committia.vim: A Vim plugin for more pleasant editing on commit messages](https://github.com/rhysd/committia.vim)

- [内部キーマッピング - gina.vim](https://github.com/aiya000/dotfiles/blob/573559ecb3dda3e774e3d790c96aa308eaf8948d/.vim/autoload/vimrc/dein/hook_source.vim#L45)

- - - - -

以上です。
Happy Viming！
