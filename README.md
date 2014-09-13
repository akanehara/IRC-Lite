# IRC Lite

『プログラミングErlang』 11章 IRC Lite の演習のために

http://www.ohmsha.co.jp/data/link/978-4-274-06714-3/

で配布されているサンプルコードからIRC Liteに必要なモジュールだけを抽出して再構成したもの。

`gs`モジュールは18で廃止になるということなので、演習にとりくむ前にGUIツールキットを`wx`で置き換えた。 

> `wx:demo()` を見るに `wx_object` を実装するのが筋が良さそうだけど
> 今回はストレートに `io_widget` の `gs` を `wx` で置き換え

##### 起動方法

```
$ make chat_server
$ make chat_client
```

### 依存モジュールについて

**lib_chan** をはじめとする **IRC Lite** の依存モジュールはサブディレクトリ `lib` に収録している。

##### lib_chanのテスト

```
$ cd lib
$ make test_server
$ make test_client
```

**lib_chan** の 設定ファイルは `~/.erlang_config/lib_chan.conf` ではなく、`lib/test.conf` を読み取るように変更してある。
