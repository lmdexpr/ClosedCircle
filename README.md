# ClosedCircle
Web app for murder mystery games

# What is it?
* マーダーミステリを遊ぶ時に大量の印刷物を出さなくてもいいようにしたかった
* 最終的にはオンラインでのプレイを手助け出来るようにするのが目標
* 方針は「オフラインでもオンラインでもプレイできて、導入コストを極限まで減らす」こと
* つまり今後もバックエンドなしで完結させたい(DB使いたくない)

# Features
* JSONファイルにシナリオを書く
* GMとしてシナリオ全体を閲覧する
* GMは参加者にそれぞれのキャラクタートークンを配布する(システム外)
* PLとしてキャラクタートークンを入力すると自分のキャラクター情報が確認できる
* PLとしてエビデンストークンを入力すると証拠品の閲覧ができる

# TODO
* scenario.jsonを誰でも(ITリテラシーの低い人でも)生成できるツールが必要
* PLが証拠品を閲覧できる数に制限を付けたい
* 他PLとの交渉で(システム外で)得た証拠品についてもシステムで管理する
* 不正しようと思えばやりたい放題なのでその対策(フロントエンドだけでは厳しいか)
