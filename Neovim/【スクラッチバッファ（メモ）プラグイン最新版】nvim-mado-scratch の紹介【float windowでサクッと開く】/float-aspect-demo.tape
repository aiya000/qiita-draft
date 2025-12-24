Output float-aspect-demo.gif

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

Type ":MadoScratchOpen md float-aspect 0.9x0.8"
Sleep 2s
Enter
Sleep 1s

# メモ書き開始
Set TypingSpeed 30ms
Type "iI'm a Neovimmer!"
Enter
Sleep 1s
Type "We are the new breed!"
Sleep 1s
Set TypingSpeed 100ms
# メモ書き終了
Escape
Sleep 1s

# 録画終了
Type ":qa!"
Enter
Sleep 2s
