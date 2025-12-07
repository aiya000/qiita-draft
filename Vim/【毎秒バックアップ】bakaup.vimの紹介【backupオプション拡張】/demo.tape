# bakaup.vimのデモGIF生成用スクリプト
Output demo.gif

Set FontSize 18
Set FontFamily "Noto Sans Mono CJK JP"
Set Width 1600
Set Height 900
Set Theme "Dracula"

# tmuxセッションを開始して2ペインに分割
Type "tmux new-session -s bakaup-demo"
Enter
Sleep 1s

# 垂直分割
Ctrl+t
Type "v"
Sleep 1s

# 左ペインに移動
Ctrl+t
Left
Sleep 500ms

# 左ペイン: Vimでファイルを開く
Type "nvim /tmp/test.md"
Enter
Sleep 1s

# 右ペインに移動
Ctrl+t
Right
Sleep 500ms

# 右ペイン: バックアップディレクトリを監視
Type "watch -n 1 'ls -lht ~/.backup/neovim-backup/$(date +%Y-%m-%d)/ | head -10'"
Enter
Sleep 2s

# 左ペインに戻る
Ctrl+t
Left
Sleep 500ms

# Vimで編集開始
Type "i"
Sleep 500ms
Type "# bakaup.vimのテスト"
Sleep 500ms
Escape
Sleep 500ms

# 保存1回目
Type ":w"
Enter
Sleep 2s

# さらに編集
Type "o"
Sleep 500ms
Type "最初の編集"
Sleep 500ms
Escape
Sleep 500ms

# 保存2回目
Type ":w"
Enter
Sleep 2s

# さらに編集
Type "o"
Sleep 500ms
Type "2回目の編集"
Sleep 500ms
Escape
Sleep 500ms

# 保存3回目
Type ":w"
Enter
Sleep 2s

# さらに編集
Type "o"
Sleep 500ms
Type "3回目の編集"
Sleep 500ms
Escape
Sleep 500ms

# 保存4回目
Type ":w"
Enter
Sleep 3s

# 終了
Type ":q"
Enter
Sleep 1s

# tmuxセッションを終了
Type "exit"
Enter
Sleep 500ms

# 右ペインも終了
Type "exit"
Enter
Sleep 1s
