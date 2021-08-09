VRChat APIを使うため、諸注意に留意してください :)

# 結

- これを使う: [favorite.sh - GitHub Gist](https://gist.github.com/8aadcec9833a530e32337601257f0f57)

```shell-session
$ ./favorite.sh world_id_list.json worlds1 auth=authcookie_XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
This world is favorite:
  favoriteId: wrld_6c828572-43da-49cd-b320-ef799438d189
  tags: ["worlds2"]
Skip this world:
  favoriteId: wrld_c710e8be-2e4b-407c-869e-0c3d5a3786e1
  error message: "world not found"
This world is favorite:
  favoriteId: wrld_d6dae663-d775-4967-9f59-1a47b48df247
  tags: ["worlds2"]

...
```

これは完全なプログラムではないので、使用する前に下記の手順で補完する必要があります。

（プログラムはMITライセンスで配布しているので、改良してくれてもいいのよ＞＜）

# 準備

1. `apiKey`を取得: https://vrchatapi.github.io/#/GettingStarted/QuickStart?id=client-api-key

```shell-session
$ apiKey=$(curl https://api.vrchat.cloud/api/1/config | jq '.apiKey')
```

2. 下記にも設定してください（下記の値は現在のもの）

```shell-sessoin
$ cat favorite.sh | head -3
#!/bin/bash

apiKey=JlE5Jldo5Jibnk5O5hTx6XVqsJu4WJ26
```

## コピー元アカウント側の準備

1. いつもの（[2FAを使っている場合はこちらから](#2fa)）

```shell-session
$ echo -n 'your_old_vrchat_account_id:password' | base64
XXXXXXXXXXXXXXXX=

$ auth_plain='XXXXXXXXXXXXXXXX='
```

2. `authcookie`を取得: `auth=authcookie_XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX`の部分をコピーしておく

```shell-session
$ curl --verbose \
    'https://api.vrchat.cloud/api/1/auth/user?apiKey=$apiKey' \
    -H "Authorization: Basic $auth_plain" 2>&1 \
    | grep 'auth=authcookie_'
< set-cookie: auth=authcookie_XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX; Max-Age=604800; Path=/; Expires=Mon, 16 Aug 2021 07:33:02 GMT; HttpOnly

$ auth_cookie='auth=authcookie_XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX'
```

- - -

<a name="2fa"></a>

2FAを使っている場合は、かわりにこちらを実行します

```shell-session
$ otp={2FAアプリから取得}
$ curl -X POST \
  'https://api.vrchat.cloud/api/1/auth/twofactorauth/totp/verify' \
    -H "Authorization: Basic $auth_plain" \
    -d "code=$otp" \
    2>&1 \
    | grep 'auth=authcookie_'
< set-cookie: auth=authcookie_XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX; Max-Age=604800; Path=/; Expires=Mon, 16 Aug 2021 07:33:02 GMT; HttpOnly

$ auth_cookie='auth=authcookie_XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX'
```

- - -


3. favorite済みワールドの取得

`n=100`は重要なので、省略しないでね！

（デフォルトだと20個くらいの取得になってしまう）

（
[**100が上限なので、100以上のワールドfavoriteがある場合はできない……？**](https://vrchatapi.github.io/#/FavoritesAPI/ListAllFavorites)

['【VRChat API】favorite済みワールド全てをunfavoriteする'](https://aiya000.github.io/posts/2020-08-16-vrchat-removing-all-favorite-worlds.html)を併用すると、よさそう
）

```shell-session
$ curl "https://api.vrchat.cloud/api/1/favorites?apiKey=$apiKey&type=world&n=100" \
    -b "$auth_cookie" \
    > favorites.json

$ cat favorites.json | jq '.[].favoriteId' | tee worlds.json
"wrld_7a88c811-7a97-4fcc-ad83-d295e5f177c1"
"wrld_72fab76b-651d-443d-87d7-326c4e8528a0"
...
```

必要に応じて、favorite枠ごとにjsonを分割してください  
分割した場合は、下記の`worlds1.json`のようなファイルを、以降の`worlds.json`と同じように処理してください

```shell-session
$ cat favorites.json | jq '.[] | select(.tags[] | contains("worlds1"))' | worlds1.json
```

jqわかんないので、手動でjsonの形に直します  
ここのやり方わかる人、この記事にPRください！

```shell-session
$ $EDITOR worlds.json
$ cat worlds.json
[
  "wrld_7a88c811-7a97-4fcc-ad83-d295e5f177c1",
  ...
  "wrld_72fab76b-651d-443d-87d7-326c4e8528a0"
]
```

## コピー先アカウント側の準備


1. 上記「いつもの」と同じ（[2FAを使っている場合は同じくこちら](#2fa)）

```shell-session
$ echo -n 'your_new_vrchat_account_id:password' | base64
XXXXXXXXXXXXXXXX=

$ auth_plain=$(echo -n 'your_new_vrchat_account_id:password' | base64)
```

2. `authcookie`を取得: `auth=authcookie_XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX`の部分をコピーしておく

```shell-session
$ curl --verbose \
    'https://api.vrchat.cloud/api/1/auth/user?apiKey=$apiKey' \
    -H "Authorization: Basic $auth_plain" 2>&1 \
    | grep 'auth=authcookie_'
< set-cookie: auth=authcookie_XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX; Max-Age=604800; Path=/; Expires=Mon, 16 Aug 2021 07:33:02 GMT; HttpOnly

$ auth_cookie='auth=authcookie_XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX'
```

3. 実際にfavoriteに登録する

とうとうやるぞ！！

favorite枠が通常は64個なのに気を付けて、切り分けておきます

例えば今favorite枠`worlds1`が24つのみ余っている場合は分割しておきます

```shell-session
$ jq '.[:24]' < worlds.json > part.json
```

ちゅーこってやっとこれです  
['favorite.sh - GitHub Gist'](https://gist.github.com/8aadcec9833a530e32337601257f0f57)をダウンロードしてきてね><

VRChatアカウントのBANを回避するために、1分に1つのワールドを処理します  
お茶を飲んでまってましょう :tea:

```shell-session
$ ./favorite.sh part.json worlds1 "$auth_cookie"
```

これが終わって、お茶も飲み終わったなら、残りも処理しちゃいましょう

```shell-session
# worlds2が35枠余ってる
$ jq ".[24:$((24 + 35))]" < worlds.json > part2.json
$ ./favorite.sh part2.json worlds2 "$auth_cookie"

# worlds3の枠がいっぱい余ってる
$ jq ".[$((24 + 35)):]" < worlds.json > part3.json
$ ./favorite.sh part3.json worlds3 "$auth_cookie"
```

# おわり

おわり
