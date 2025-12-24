Output quickrun-demo.gif

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
Enter
Sleep 500ms

Type ":MadoScratchOpen ts"
Sleep 1s
Enter

# メモ書き開始
Set TypingSpeed 50ms
Type "iconsole.log('Thanks thinca! 🐟')"
Enter
Sleep 200ms
Type "console.log('Thanks thinca! 🐟')"
Enter
Sleep 200ms
Type "console.log('Thanks thinca! 🐟')"
Enter
Sleep 200ms
Type "console.log('Thanks thinca! 🐟')"
Enter
Sleep 500ms
Escape
Set TypingSpeed 100ms

# QuickRun実行
Type "q:QuickRun"
Enter
Sleep 500ms
Type "q:resize 100"
Enter
Sleep 2s

# 録画終了
Type ":qa!"
Enter
Sleep 2s
