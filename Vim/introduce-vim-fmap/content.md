# fキーをnoremap可能にするfmap.vim

今回の主題 :point_down:

- [GitHub - aiya000/vim-fmap: Support key mappings for f, like nnoremap/vnoremap/onoremap](https://github.com/aiya000/vim-fmap)

![lala](lala.jpg)

## 事前知識 (noremap, fFtT)

Hi, メリークリスマス :snowman:

皆さんnoremapしてますか？
例えばnnoremapはnormalモードに対するキーマッピングを定義する機能で、以下のような機能を実現できました。

```vim
" <Space>h, j, k, lでwindowを上下左右に移動する
nnoremap <Space>h <C-w>h
nnoremap <Space>j <C-w>j
nnoremap <Space>k <C-w>k
nnoremap <Space>l <C-w>l
```

```vim
" 各レジスタの内容を表示する
nnoremap <silent> q: :<C-u>register<CR>
```

```
--- レジスタ ---
""   ような
"0   Vim Advent Calendar 2018ボツ記事（vital.vimで関数型プログラミング入門）^J
"1   :+1: ^J
"2   ## まとめ^J）"
...
```

またvimは標準でnormalモードの`f`キー、`F`, `t`, `T`キーへのマッピングを提供します。
例えばあなたが:point_down: の ^ の上（c）にカーソルを乗せている場合

```
a b c d e
    ^
```

`fe`キーを押下するとこの :point_down: 前方のeの位置に移動し

```
a b c d e
        ^
```

`Fb`を押したならこの後方のbの位置に

```
a b c d e
  ^
```

`te`なら前方のeの手前に

```
a b c d e
       ^
```

`Ta`なら後方のbの手前に移動します。

```
a b c d e
    ^
```

- `f{char}`: 後方の{char}に移動
- `F{char}`: 後方の{char}に移動
- `t{char}`: 前方の{char}の手前に移動
- `T{char}`: 前方の{char}の手前に移動

もちろんtextobjとしても使用できます。

```
a b c d e
    ^
```

:arrow_up:
`dfd`
:arrow_down:

```
a b  e
    ^
```

話はもう一度noremapに戻って……

**n**noremapはnormalモードに対するキーマッピングの定義で、他には以下の\*noremapコマンドがあります。

_ `:inoremap`: insertモード用
_ `:vnoremap`: virtual + selectモード用
_ `:cnoremap`: cmdモード用
_ `:tnoremap`: terminal
_ `:noremap`: 全部用

あれ……**f**noremapがないよ？

## 概要

fmap.vimは`:FNoremap`コマンドを提供します :flushed:

- [GitHub - aiya000/vim-fmap](https://github.com/aiya000/vim-fmap)

例えば以下のように設定することで

```vim
nmap f <Plug>(fmap-forward-f)
nmap F <Plug>(fmap-backward-f)
nmap t <Plug>(fmap-forward-t)
nmap T <Plug>(fmap-backward-t)
```

```vim
" 全角括弧の開き
FNoremap p （
```

normalモードの `fp` キーでこのような :point_down: カーソルジャンプをすることができます！

```
a （ b c d ） e
     ^
```

:arrow_down:

```
a （ b c d ） e
           ^
```

標準の`fFtT`を潰したくない場合は`<leader>f` (`FtT`) にマッピングすると便利です。

```vim
nmap <leader>f <Plug>(fmap-forward-f)
nmap <leader>F <Plug>(fmap-backward-f)
nmap <leader>t <Plug>(fmap-forward-t)
nmap <leader>T <Plug>(fmap-backward-t)
```

## 事例

fmap.vimは`fFtT`でひらがな、カタカナ、句読点等を打つのが面倒だったために作られました。
ですので日本語記事の執筆において、特に助けになります。
任せてください。

例えばこんなMarkdownのコードがあったとします。

```
にこ「ええ！ よく『関数型プログラミングにおいて再帰を直に書く必要はなく、畳込みを用いることができる』っていう言論があるけれど、Semigroupを使えば畳み込みすら直に扱わなくていいってことよ」
```

1行がとっても長くて、特定の場所にジャンプしにくいですね。
行頭から「畳込みを用いることができる」の「畳」にジャンプしたい場合は`f（IME切り替え）たたみ（変換）（Enter）（IME切り替え）`もしくは`f（IME切り替え）、（Enter）（IME切り替え`という長ったらしいストロークが必要です。

これが普通のMarkdown（※）なら`、`の後に改行を入れてもいいのですが

```
にこ「ええ！
よく『関数型プログラミングにおいて再帰を直に書く必要はなく、
畳込みを用いることができる』っていう言論があるけれど、
Semigroupを使えば畳み込みすら直に扱わなくていいってことよ」
```

Qiita Flavor Markdownは**なぜか**コードの改行に対して実際の改行 (`<br/>`) を入れてしまうので、そのようにしたくない場合にその手は使えません。

※ CommonMarkのこと

ここで`FNoreMap , 、`を用いると、`f,l`のみでジャンプすることが可能になります。
その後fmap.vimはvim標準の`;`をコロしませんので、`f,;l`でSemigroupの「S」に飛ぶこともできます。

あるいはfmap.vimはデフォルトで`'mi`で平仮名の「み」にジャンプするマッピングが有効になっているので、「畳」に飛ぶために`f'mihh`してもいいでしょう。

## まとめ

fmap.vimは`fFtT`の後のキーに対してマッピングを定義するためのプラグインです。

日本語記事の執筆活動の支援を目的に作られました。
ただし機能はもっと一般的なものですので、日本語以外の言語でも役立つでしょう。

よければ使ってね :metal::flushed::metal:

![lala](sleepy-lala.jpg)
