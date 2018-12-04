# fキーをnoremap可能にするfmap.vim

- [GitHub - aiya000/vim-fmap: Support key mappings for f, like nnoremap/vnoremap/onoremap](https://github.com/aiya000/vim-fmap)

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
例えばあなたが以下の ^ の上の位置にカーソルを乗せている場合

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

## 応用
## まとめ
