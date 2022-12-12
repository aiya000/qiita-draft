# GitHubのPR最下部に表示される「「アレ」」は、このAPIでPOSTできる

本稿は :sparkles: [GitHub Actions Advent Calendar 2022](https://qiita.com/advent-calendar/2022/github-actions) :sparkles: 12日目の記事です :tada:

## 概要

本稿では、GitHubのPullRequestの最下部で表示される、上記画像の部分（=「アレ」）の概要を述べます。

- わかること
    - 「アレ」ってなんて名前なの？
    - 「アレ」を操るにはどうしたらいいの？

成功時の「アレ」
![success.png](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/7b012789-1516-cce5-bf9c-b7392db6f67e.png)

pending時の「アレ」
![failure.png](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/84945/84ab1761-c0b9-712c-7a38-93ce1fe0984d.png)

## 「アレ」 = **Commit Status**

名称について。

「アレ」は**Commit Status**と言うらしいです。

今回かかったうちの半分以上は、この名称を調べるのに使った気がします……。

これで名称がわかりました。

## Commit Status？ PRステータスじゃないの？ POSTする方法

Commit Statusはよく、テストカバレッジを表示したり、ビルドエラーを表示したり、lintの成功を表示したりします。
それ故に僕も「PRのステータスだ」と思っていたのですが、それは過ちです。

**このステータス部分は、PRの「最新コミット」に載るものです。**

- [コミットのステータス - GitHub Docs](https://docs.github.com/ja/rest/commits/statuses?apiVersion=2022-11-28)

具体的には下記例示のように、PRの最新コミットのshaにPOSTします。

```yaml:.github/workflows/coverage.yml
name: Coverage

on:
  pull_request:
    branches: [ "develop" ]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    - run: ...
    #
    # ...
    #
    - run: ./.github/workflows/post-coverage.sh ${{ secrets.GITHUB_TOKEN }} ${{ github.event.pull_request.head.sha || github.sha }}
```

```bash:./.github/workflows/post-coverage.sh
#!/bin/bash

github_token=$1
commit_head_sha=$2
result='Coverage 90%'  # 実際はここでカバレッジを集計する

# YOUR_NAME と REPO は、実際のリポジトリに置き換えてください。
# 例えば https://github.com/aiya000/dotfiles なら aiya000 と dotfiles。
# => aiya000/dotfiles

curl -X POST \
  -H 'Accept: application/vnd.github+json' \
  -H "Authorization: Bearer $github_token" \
  -H 'X-GitHub-Api-Version: 2022-11-28' \
  -d "{\"state\":\"success\",\"description\":\"$some_text\",\"context\":\"coverage-bot\"}" \
  "https://api.github.com/repos/YOUR_NAME/REPO/statuses/$commit_head_sha"
```

これでGitHubのPRに
```
✅ coverage-bot  ― Coverage 90%
```
と表示されるようになりました :tada:

## 終わりに

GitHub氏、その設計でよかったんか？ :thinking_face:
