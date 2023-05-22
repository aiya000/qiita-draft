# 概要

gitローカルリポジトリ内でgit-worktreeを使っていたら、eslintのデフォルト挙動である
「上位ディレクトリのeslintrcを全て見て、eslintrcがある全てのディレクトリでeslintを実行する」
で事故を起こしたので、修正をしました！

- [eslintの挙動 - Cascading and Hierarchy](https://eslint.org/docs/latest/use/configure/configuration-files#cascading-and-hierarchy)

- [git-worktreeとは](https://git-scm.com/docs/git-worktree)
    - 簡単に言うと、特定のブランチ専用のディレクトリを、ローカルリポジトリの下に生成するものです
    - 例えば `git clone foo && cd foo && git worktree add -b feature/bar feature/bar origin/develop`すると、./foo/feature/barが生成されます
    - ./foo/feature/barは、git-cloneした通常のローカルリポジトリと同じように扱えます

# 結論

['Cascading and Hierarchy'](https://eslint.org/docs/latest/use/configure/configuration-files#cascading-and-hierarchy)にある通り、`root: true`を指定してあげて、上位ディレクトリの捜査をやめさせます。

```json:./foo/feature/bar/.eslintrc.json
{
    "root": true,
    "extends": [
        ...
}
```

# エラー内容

git-worktreeを使うと仕様上、eslintrcがあるディレクトリは少なくとも2つになります。

- ./foo  // ①
    - feature
        - bar  // ②

```shell-session
$ cd ./foo/feature/bar
```

僕の場合、`./foo`でも`yarn install`をしていたので、まずは以下のエラーが出ました。
`./foo`でもeslintが実行されてしまっているからです。

```shell-session
$ eslint --fix --ext .ts src

Oops! Something went wrong! :(

ESLint: 8.39.0

ESLint couldn't determine the plugin "@typescript-eslint" uniquely.

- （リポジトリ）/feature/foo/node_modules/@typescript-eslint/eslint-plugin/dist/index.js (loaded in "--config")
- （リポジトリ）/node_modules/@typescript-eslint/eslint-plugin/dist/index.js (loaded in "../../.eslintrc.json")

Please remove the "plugins" setting from either config or remove either plugin installation.

If you still can't figure out the problem, please stop by https://eslint.org/chat/help to chat with the team.
```

こちらは単にこう直しました。

```shell-session
$ rm ../../node_modules
```

次

```shell-session
$ eslint --fix --ext .ts src

Oops! Something went wrong! :(

ESLint: 8.39.0

ESLint couldn't find the config "google" to extend from. Please check that the name of the config is correct.

The config "google" was referenced from the config file in "（リポジトリ）/.eslintrc.json".

If you still have problems, please stop by https://eslint.org/chat/help to chat with the team.
```

解決方法は['Cascading and Hierarchy'](https://eslint.org/docs/latest/use/configure/configuration-files#cascading-and-hierarchy)に書いてありました。

```json:./foo/feature/bar/.eslintrc.json
{
    "root": true,
    "extends": [
        ...
}
```

# おわり

おわり。
