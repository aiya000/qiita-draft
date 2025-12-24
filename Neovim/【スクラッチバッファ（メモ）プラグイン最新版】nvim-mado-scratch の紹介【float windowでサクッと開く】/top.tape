Output top.gif

Set FontSize 18
Set FontFamily "Hack Nerd Font"
Set Width 1600
Set Height 900
Set Theme "Dracula"
Set Margin 15
Set MarginFill "#674EFF"
Set BorderRadius 10
Set TypingSpeed 100ms

Sleep 1s
Type "nvim"
Sleep 1s
Enter
Sleep 1.5s

Type "::MadoScratchBufferOpenFile"
Sleep 2s
Enter
Sleep 1s

# メモ書き開始
Set TypingSpeed 30ms
Type "i# 急にアイデアが浮かんだ！"
Sleep 500ms
Enter 2
Sleep 300ms
Type "## 新しいAPI設計"
Sleep 500ms
Enter 2
Sleep 300ms
Type "- ユーザー認証エンドポイント"
Sleep 400ms
Enter
Type "- データ取得の非同期処理"
Sleep 400ms
Enter
Type "- キャッシュ戦略の見直し"
Escape
Sleep 5s
Set TypingSpeed 100ms
# メモ書き終了
Type "q:wq"
Enter
Sleep 1s

Type ":e ~/App.ts"
Sleep 500ms
Enter

Type "ifunction someFirstFunction() {"
Enter
Sleep 200ms
Type "// 何をやるんだっけ…？"
Sleep 500ms
Escape
Type "::MadoScratchBufferOpenFile"
Enter
Sleep 3s
Escape 2
Type "S// 確認OK！ 実装スタート！"
Enter
Backspace 3 # コメント削除
Sleep 500ms
Type "."
Sleep 100ms
Type "."
Sleep 100ms
Type "."
Sleep 500ms
Enter
Type "}"
Sleep 1s
Escape

# 録画終了
Type ":wqa!"
Enter
Sleep 4s
