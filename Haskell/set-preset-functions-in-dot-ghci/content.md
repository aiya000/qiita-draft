# GHCiを起動したときに、定義しておいた関数などを自動ロードする

```~/.ghci:
:load ~/.ghci.hs
```

```haskell:~/.ghci.hs
module Preset where

import Prelude

xs :: [Int]
xs = [1 .. 10]
```

GHCiを起動

```
>>> xs
[1,2,3,4,5,6,7,8,9,10]
```

やったね！

#### NOTICE

（グローバルプロジェクトでない）haskell-stackプロジェクト内で `stack ghci` すると、プロジェクト内のモジュールのみがロードされるので、手動で `>>> import Preset` してあげる必要があるようです。
（もちろん `:module +Preset` でもおっけー！）

なんで……？？ 🤔
