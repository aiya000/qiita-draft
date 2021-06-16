# Config.ahk等を編集したbug.nを、ビルドする方法
# 概要
## bug.nとは？

WindowsのキーマッピングソフトであるAutoHotkeyを用いて作られた、タイル型ウィンドウマネージャです！:sparkles:
（なんでキーマッピングソフトでウィンドウマネージャ作った？）

- [bug.n: Tiling Window Manager for Windows - GitHub](https://github.com/fuhsjr00/bug.n)

カスタマイズ性がやばく、ArchLinuxで[重度にxmonadを改造し](https://qiita.com/aiya000/items/be55d899e9c92dd57c35)使用していた僕を受け入れてくれる、最高の相棒です

通常はConfig.iniで設定を行います

しかしながらConfig.iniではカスタマイズできない内容が[たまにあるようです](https://github.com/fuhsjr00/bug.n/issues/178)
その場合、ソースを直に弄る必要があります

# 解決

ソースを弄った場合、もちろん再ビルドが必要です

以下の手順でビルドができます:+1::sparkles:

1. まだ対象のbug.nリポジトリをbug.nリポジトリをcloneする
1. 既にbug.nを起動している場合は、終了させる
1. ./toolsディレクトリをexplorer.exeで開く
1. ./tools/build.ahkを実行する
    - 同ディレクトリにデバッグログが出力されるので、もし失敗した場合はそちらを見るとよい
1. 出力された./bugn.exeを実行する
    - なお公式から提供された未改造bugn.exeよりも、起動がなぜか**かなり遅い**。うちの環境だけ？

Or

```shell-session
$ /mnt/c/Program\ Files/AutoHotkey/Compiler/Ahk2Exe.exe /in src/Main.ahk /out bugn.exe \
    && ./bugn.exe
```

（これは簡単だけど、体感的には前述の方法よりもさらに起動が重い気がする。）

- - -

やったー！！
