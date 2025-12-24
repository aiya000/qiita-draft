Output sp-auto-hide-demo.gif

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
Type "nvim ~/tmp/foo.md"
Sleep 1s
Enter
Sleep 1s
Type "iauto-hidding"
Escape
Sleep 1s

Type ":MadoScratchOpen"
Sleep 1s
Enter
Sleep 1s

# メモ書き開始
Type "iWriting some notes."
Sleep 500ms
Type "."
Sleep 100ms
Type "."
Sleep 100ms
Type "."
Sleep 1s

# メモ書き終了
Escape
Sleep 500ms
Ctrl+w
Type "j"
Sleep 2s

Type "oHidden! (without :q)"
Escape
Sleep 2s

# 録画終了
Type ":qa!"
Enter
Sleep 2s
