+++
title = "Information Science"
author = ["ROCKTAKEY"]
date = 2020-08-04T00:00:00+09:00
lastmod = 2022-01-31T02:51:10+09:00
draft = false
+++

## <span class="org-todo done DONE">DONE</span> Information Science {#information-science}

主にEmacsの話になりそう。C++や競技プログラミングの話もするかも。


## <span class="org-todo done DONE">DONE</span> tab-bar-modeを探検する {#tab-bar-modeを探検する}


### 導入 {#導入}

Emacs-27.1で、 `tab-bar-mode` と `tab-line-mode` が追加された。

`tab-line-mode` では、[Figure 1](#figure--fig:tab-line-mode)にあるように、
ウィンドウ[^fn:1]
上部(ヘッダラインという)にバッファ一覧が
タブとして表示される。バッファ切り替えを行うときにそれをクリックすることで
バッファを切り替えられる。
実装を見ると、どうやら `window-next-buffers` と `window-prev-buffers` を用いて
表示するバッファを決めているようなので、さしあたりでは `C-x b` (`switch-bufer`)
の一部代替、及び `next/previous-buffer` の代替が目的だろう。
バッファリストが表示されるという点では `C-x C-b` (`list-buffer`)や `ibuffer` の
代替とも捉えられるが、横幅に限りがあることと、そもそも `window-next/prev-buffer`
では全バッファを網羅できないことを踏まえると、完全に置き換えるには貧弱だろう。
`tab-line-mode` に関しては、バッファ切り替えを文字以外で行うのに
慣れることができそうもないので、ここまでにしておく。

<a id="figure--fig:tab-line-mode"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/netlify/2020-d99103ba-db6f-5424-3053-087c12ab74be/tab-line-mode.png" caption="<span class=\"figure-number\">Figure 1: </span>`global-tab-line-mode` をオンにしたEmacs。各ウィンドウにタブ欄がある。" >}}

私が今回説明したいのは、 `tab-bar-mode` である。こちらは先程とは違い、
**1つのフレーム[^fn:1]につき1つ** 、一番上にタブ欄が表示される。
このタブが切り替えるのはウィンドウ構成、つまりどの位置になんのバッファを表示した
ウィンドウがあるかの情報、である。これはとても有用だ。
単純に表示領域が足りないからというのもあるが、やはり大きいのは、
突然別のことをやりたくなったときに今の状態を保持してさっと移り、
終わったら戻ってこれることだ。だれしも一度は経験した、「さっきなに見てたっけ」
「さっきまでなにがしたかったんだっけ」というのが格段に減ることだろう。
現実世界にもはやく実装されてほしい。今まで机の上を保存したいと何度思ったことか。

<a id="figure--fig:tab-bar-mode"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/netlify/2020-d99103ba-db6f-5424-3053-087c12ab74be/tab-bar-mode.png" caption="<span class=\"figure-number\">Figure 2: </span>`tab-bar-mode` をオンにしたEmacs。ウィンドウは複数あるがタブ欄は1つだけだ。" >}}


#### `elscreen` との比較 {#elscreen-との比較}

同じことをしてくれるパッケージには、
[`elscreen`](https://github.com/knu/elscreen) がある。こちらも同様にウィンドウ構成を切り替えられ、今まで利用してきたが、
いくつか欠点がある。

一つは、タブの表示が貧弱なことだ。
基本的にヘッダラインに表示しているので、(同じ内容なのに)全てのウィンドウで表示されたり、
左右に分割されているウィンドウでは半分しか見えなかったりといった感じで、
痒いところに手が届かない。やはりタイトルバーのようにフレーム一杯に広がっている領域が1つだけ欲しい。
そういうわけで過去に[navbar.el](https://github.com/conao3/navbar.el)というものの制作([元](https://github.com/papaeye/emacs-navbar)があるので正確には改修)に少しだけ携わった
ことがある(今もメンテナではあります)が、いかんせん古いコードだったので、かなりバギーで、
動作が怪しい。それに比べると、 `tab-bar-mode` はかなり優秀だと思う。
先程述べたとおり、 `tab-bar-mode` では1フレームにつき一つ、タブ一覧がフレーム一杯に広がっている。
それでいて、GNU Emacsに取り込まれていることからある程度の動作は保証されているからだ。

もう一つは、前段落とやや被るが、 `elscreen` は外部パッケージであることだ。
あくまでもGNU Emacsの外の存在である。ウィンドウ構成周りは元々複雑でバグが入りやすいため、
ウィンドウ構成の管理が外部パッケージだと、どうしても怪しい動作が目立つ。
その点 `tab-bar-mode` はGNU Emacsに取り込まれているので、安心できる。
もちろんバグが絶対にないわけではないが、本家に入っているなら安定性は重視されているだろう。

ただし、もちろん利点もある。 `elscreen` はかなりの古参なので、拡張がたくさんある。
`elscreen-persist` なんかは(MELPAからは消えているとはいえ)その筆頭だろう。
`tab-bar-mode` にそのような類のものはまだないので、
永続化したい人は乗り換えを待ったほうがいいかもしれない。


### 基本的な使い方とキーバインド {#基本的な使い方とキーバインド}

まず、 `M-x tab-bar-mode` で `tab-bar-mode` をオンにすることで上部にタブが出てくる
[^fn:2]。
気に入ったら `init.el` に以下のように書けばよい。

```emacs-lisp
(tab-bar-mode +1)
```

見た目については、アクティブなタブは `tab-bar-tab` 、
非アクティブなタブは `tab-bar-tab-inactive` のfaceを変更すれば変えられる。
といってもどちらも `tab-bar` にinheritしているので、
`tab-bar` のほうのfaceを変えるのがお手軽かもしれない。

と、ここまではただの見た目だ。これはとりあえず見た目を他と合わせたい自分用。
とりあえず、先にキーバインドを示しておく。prefixは `C-x t` で `tab-prefix-map`
に登録されているので、もしこれを変えたければ適宜 `define-key` すればよい。

<div class="table-caption">
  <span class="table-number">Table 1</span>:
  <code>tab-bar</code> 系のキーバインド(<code>C-x t</code> は省略)
</div>

| key        | function                     | summary       |
|------------|------------------------------|---------------|
| `2`        | `tab-new`                    | タブの新規作成 |
| `1`        | `tab-close-other`            | 現在のタブ以外全て削除 |
| `0`        | `tab-close`                  | 現在のタブの削除 |
| `o`        | `tab-next`                   | 次のタブへ移動 |
| `m`        | `tab-move`                   | 現在のタブの位置を移動 |
| `r`        | `tab-rename`                 | タブに名前を付ける |
| `RET`      | `tab-bar-select-tab-by-name` | 名前でタブを選択 |
| `b`        | `switch-to-buffer-other-tab` | バッファを新しいタブで開く |
| `C-f`, `f` | `find-file-other-tab`        | ファイルを新しいタブで開く |

各コマンドの詳細はこれから書くが、基本は `C-x t 2` でさくっとタブを作り、
おわったら `C-x t 0` で消す運用だろう。名前はお好みで `C-x t r` を使って付ける。
一斉に消したければ `C-x t 1` 。これくらい覚えていれば使えると思う。


#### 各キーの説明 {#各キーの説明}

`2` (`tab-new`)、 `1` (`tab-close-other`)、 `0` (`tab-close`)、 `o` (`tab-next`)に関しては、
バッファやウィンドウを操作する `C-x` 系の
`split-window-below` (`C-x 2`)、 `delete-other-windows` (`C-x 1`)、
`delete-window` (`C-x 0`)、 `other-window` (`C-x o`)に倣っているようだ。
たしかに、間に `t` を挟むだけだし、手に馴染んでいるのでわかりやすい。
3ストロークはちと多い気もするが、連発するものでもないのでそこまで問題なさそうだ。

`m` (`tab-move`)はすこしわかりづらいかもしれないが、要は表示されたタブの並び換えに使う
ものだ。現在のタブを一つ右に移動させる。並べ替えにどれほどの需要があるかは
不明だが、複数の関連するウィンドウ構成をまとめておきたいときには有用だろう。

`r` (`tab-rename`)はタブに名前を付けるものだ。renameの名が冠されているが、
デフォルトではタブには名前がなく、カレントバッファの名前が動的に表示される
([こちら](#tab-bar-tab-name-function)で変更可能)ので、
名前を最初につけるときにも使うことになる。フランクに使うなら
わざわざ名前はつけなくてもいいかもしれない。

`RET` (`tab-bar-select-tab-by-name`)は名前を入力してタブを選択する形だ。
名前がついてない場合はカレントバッファが候補の名前となる。
`completing-read` を直で呼んでいるので、 `ido` や `ivy` を使いたければ 他と同様に
`completing-read-function` を `ido-completing-read` や `ivy-completing-read` とすればよいだろう。

`b` (`switch-to-buffer-other-tab`)、 `f` 、 `C-f` (`find-file-other-tab`)も `C-x` 系に倣っている。
それぞれ選択したバッファ、ファイルを新しいタブで開くもので、丁度
`C-x b` (`switch-to-buffer`)と `C-x C-f` (`find-file`)に対応している。


#### `tab-bar-history-mode` {#tab-bar-history-mode}

また、 `tab-bar-history-mode` というものもある。これは各タブでウィンドウ構成の履歴を
辿れるようにするものだ。よほど重たいのでなければ、オンにしない手はない。

```emacs-lisp
(tab-bar-history-mode +1)
```

利用できる関数は2つだけ。 `tab-bar-history-back` と `tab-bar-history-forward` だ。
それぞれがウィンドウ構成履歴を戻る、進む役割をもつ。
また、タブ一覧の一番左側にでてくる2つの矢印にも同じものが割り当てられている。


### Custom variables {#custom-variables}

この節ではカスタマイズ変数の説明をする。括弧内は初期値。
あまり重要でなさそうなのは載せてないので、各自で調べてね☆


#### `tab-bar-show` (`t`) {#tab-bar-show--t}

先述のようなタブ操作系のキーバインドを使ったときに、自動で `tab-bar-mode` を
オンにして、タブを表示するかどうか。 `t` なら常にそうする。
`nil` なら常にそうしない。 `1` なら2つ以上のタブがあるときだけ表示する。


#### `tab-bar-new-tab-choice` (`t`) {#tab-bar-new-tab-choice--t}

新規タブがどの状態で始まるかどうか。 `t` ならカレントバッファ1つ。
`nil` なら現在のタブを複製。
文字列ならその名前のバッファ、なければファイルやディレクトリを探して開く。
関数ならその返り値のバッファを開く。個人的には動的に決められるようになっているのが
とても好み。よき。


#### `tab-bar-new-button-show` (`t`)、 `tab-bar-close-button-show` (`t`) {#tab-bar-new-button-show--t--tab-bar-close-button-show--t}

新規タブボタン、タブ削除ボタンを表示するかどうか。~t~ なら表示、 `nil` なら
非表示。 `tab-bar-close-button-show` の場合、 さらに
`selected` (カレントタブでのみ表示)と `non-selected` (カレントタブ以外で表示)の二つも可能。


#### `tab-bar-tab-hints` (`nil`) {#tab-bar-tab-hints--nil}

タブに連番を表示するかどうか。


#### `tab-bar-tab-name-function` (`tab-bar-tab-name-current`) {#tab-bar-tab-name-function}

名前がないとき、名前をどう決めるか。デフォルトではカレントバッファ名
を返す `tab-bar-tab-name-current` ([Figure 3](#figure--fig:tab-bar-tab-name-current))となっているが、
任意の関数に変更可能。
windowの数を数えて付け加えてくれる `tab-bar-tab-name-current-with-count`
([Figure 4](#figure--fig:tab-bar-tab-name-current-with-count))、
長さが `tab-bar-tab-name-truncated-max` を超えると省略してくれる
`tab-bar-tab-name-truncated` ([Figure 5](#figure--fig:tab-bar-tab-name-truncated))、
表示されている全バッファの名前をカンマで繋げてくれる
`tab-bar-tab-name-all` ([Figure 6](#figure--fig:tab-bar-tab-name-all))が用意されている。
自身で作った関数でももちろんOK。

<a id="figure--fig:tab-bar-tab-name-current"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/netlify/2020-d99103ba-db6f-5424-3053-087c12ab74be/tab-bar-tab-name-current.png" caption="<span class=\"figure-number\">Figure 3: </span>`tab-bar-tab-name-current` のとき(デフォルト)" >}}

<a id="figure--fig:tab-bar-tab-name-current-with-count"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/netlify/2020-d99103ba-db6f-5424-3053-087c12ab74be/tab-bar-tab-name-current-with-count.png" caption="<span class=\"figure-number\">Figure 4: </span>`tab-bar-tab-name-current-with-count` のとき" >}}

<a id="figure--fig:tab-bar-tab-name-truncated"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/netlify/2020-d99103ba-db6f-5424-3053-087c12ab74be/tab-bar-tab-name-truncated.png" caption="<span class=\"figure-number\">Figure 5: </span>`tab-bar-tab-name-truncated` のとき" >}}

<a id="figure--fig:tab-bar-tab-name-all"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/netlify/2020-d99103ba-db6f-5424-3053-087c12ab74be/tab-bar-tab-name-all.png" caption="<span class=\"figure-number\">Figure 6: </span>`tab-bar-tab-name-all` のとき" >}}


#### `tab-bar-new-tab-to` (`right`) {#tab-bar-new-tab-to--right}

新しいタブをどこに作るか。 `left` や `right` なら現在のタブの左右に、
`leftmost` や `rightmost` なら一番左や一番右に作る。


#### `tab-bar-close-tab-select` (`recent`) {#tab-bar-close-tab-select--recent}

現在のタブを閉じたとき、どのタブに移動するか。
`recent` なら最近利用したタブに、 `left` や `right` なら元々あったタブの左右の
タブに移動する。


#### `tab-bar-close-last-tab-choice` (`nil`) {#tab-bar-close-last-tab-choice--nil}

最後の1つのタブを閉じたときにどうするか。
`nil` ならなにもしない。 `delete-frame` なら現在のフレームを削除。
`tab-bar-mode-disable` なら `tab-bar-mode` をオフにする。
また、関数を渡すと閉じたときに関数を実行してくれる。


### 改造 {#改造}

せっかく1フレーム1つの領域を手に入れたので、自由に表示したい。
日付や時計、場合によってはbranchなんかも、1フレーム1つで十分だろう。

というわけで、しくみをざっくりと見て、真似してみようではないか。
と思って見てみると、なんだかおもしろい記述が。

```emacs-lisp
(global-set-key [tab-bar]
                `(menu-item ,(purecopy "tab bar") ignore
                            :filter tab-bar-make-keymap))
```

<div class="src-block-caption">
  <span class="src-block-number">Code 1</span>:
  tab-bar-mode.el L197-199
</div>

どうやら `[tab-bar]` にメニューアイテムを割り当てると画面上部に表示できるらしい。
おもしろい試みだ。 `navbar.el` ではバッファを表示させていたのに対し、
専用の領域を用意したわけか。(試しに `[tab-bar2]` に同じものを割り当ててみたが、
なにも表示されなかったので、おそらく新しく専用の領域を作ったのだろう。)

恥ずかしながら `menu-item` 形式の `global-set-key` は初めて見たので、
`define-key` のヘルプをみてみると、InfoファイルのExtended Menu Itemsの項に
説明があるらしい。(日本語版は(24.5のものだが)[ここ](https://ayatakesi.github.io/emacs/24.5/elisp_html/Extended-Menu-Items.html)にある。ほとんどかわっていないので
問題ない。)これを読むと、どうやら `:filter` に渡された関数 `tab-bar-make-keymap` は、
3番目の `ignore` というシンボルを受けとり(`tab-bar` では使ってないので適当なシンボル
をおいているっぽい)、キーマップを返すものらしい。本来は `ignore` の位置に
キーマップを直接置くが、動的に生成したいときは `:filter` を使う、という感じのようだ。

`tab-bar-make-keymap` は内部的には `tab-bar-make-keymap-1` を呼んでいるだけだが、
この関数は通常我々が使うようなキーマップとは毛色が異なり、
`cdr` の各要素のほとんどは _`(symbol`_ `menu-item` _`string function)`_ となっている。
_`symbol`_ は識別子で、 _`string`_ が表示される文字列だ。クリックすることで
_`function`_ が呼び出される。もし単に文字を表示したければ、 `tab-bar` の
セパレータの表示に倣って _`function`_ を `ignore` にすればよいだろう。

さあこれで準備は整った。要は `tab-bar-make-keymap-1` をハックして、
さっきの形式で文字列をいれてやればいいのだろう。

というわけで `tab-bar-display` というパッケージを作った。

{{< figure src="https://gh-card.dev/repos/ROCKTAKEY/tab-bar-display.svg" link="https://github.com/ROCKTAKEY/tab-bar-display" >}}

使いかたは簡単で、 `tab-bar-display-before` と `tab-bar-display-after` に
`format-mode-line` 形式で表示したいものを書いて、 `tab-bar-display-mode`
を `tab-bar-mode` と共にオンにしておけば、タブ一覧の前後にそれが表示される。

<a id="figure--fig:tab-bar-display-mode"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/netlify/2020-d99103ba-db6f-5424-3053-087c12ab74be/tab-bar-display.png" caption="<span class=\"figure-number\">Figure 7: </span>`tab-bar-display-mode` を用いた例" >}}


### まとめ {#まとめ}

`tab-bar-mode` はとても便利というはなしでした。
かなり便利そうなので `elcreen` を捨てて乗り換えようかしら。


## <span class="org-todo done DONE">DONE</span> chocolateyのインストールをするとMcAfeeに邪魔される {#chocolateyのインストールをするとmcafeeに邪魔される}

Haskellのコンパイラのghcをインストールしようとしたら、
chocolateyが必要と言われた。気になってはいたので、いれてみようと一念発起。

[chocolateyのインストールセクション](https://chocolatey.org/install)にPowershell用のインストールコマンドがあるので、
(`ExecutionPolicy AllSigned` したあと)ペーストして実行した。が、謎のエラーが。

```text { linenos=table, linenostart=1 }
$ Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
発生場所 行:1 文字:1
+ Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.Service ...
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
このスクリプトには、悪質なコンテンツが含まれているため、ウイルス対策ソフトウェアによりブロックされています。
    + CategoryInfo          : ParserError: (:) [], ParentContainsErrorRecordException
    + FullyQualifiedErrorId : ScriptContainedMaliciousContent
```

こいつとともにMcAfeeが「ウイルスいたから殺しといたわ!」と通知してきたので、
たぶんMcAfeeのせいだろう。ファイアウォールとリアルタイムスキャンを無効にするも、
変わらず。お手上げかと思ったが、[Qiitaの記事](https://qiita.com/konta220/items/95b40b4647a737cb51aa#2-chocolatey%E3%81%AE%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E3%82%92%E5%AE%9F%E8%A1%8C)をみつけた。ここにある画像の、3つ目のスクリプトは
全文みえているので、一か八かで実行。

```shell { linenos=table, linenostart=1 }
iwr https://chocolatey.org/install.ps1 -UseBasic Parsing | iex
```

最初のスクリプトとなんの差があったのかはわからないが、無事インストールできた。
よかった。


## <span class="org-todo done DONE">DONE</span> grugru.el ― カーソル下のthingをじゅんぐりに入れ替える {#grugru-dot-el-カーソル下のthingをじゅんぐりに入れ替える}

この記事は、[Emacs Advent Calendar 2021](https://qiita.com/advent-calendar/2021/emacs)の記事です。


### 導入 {#導入}

さてみなさん、突然ですが、コードを書いていて、「あっやっぱり `true` じゃなくて `false` だった」
「 `&&` じゃなくて `||` だった」などということが多々あるのではないでしょうか。そして、往々にして、
入れ替えることになる組み合わせは決まっているのではないでしょうか。こういうとき、いちいちバックスペースを
連打して打ち直すのは、思考のノイズになってしまってよくありません。そこで、単一のコマンドを使って、
あらかじめ登録しておいた一連の `thing` をじゅんぐりに出してくれる `grugru.el` というパッケージを開発しました。

{{< figure src="https://gh-card.dev/repos/ROCKTAKEY/grugru.svg" link="https://github.com/ROCKTAKEY/grugru" >}}

とりあえず使ってみたい人は、 `M-x package-install RET grugru RET` してください
(インストールできない場合は、[ここ](https://emacs-jp.github.io/packages/package)を参照)。
その後、

```emacs-lisp
(grugru-default-setup)
(global-set-key (kbd "C-:") #'grugru)
(grugru-highlight-mode)
```

を評価するなり `init.el` に貼り付けて再起動するなりしてみましょう。すると、じゅんぐりに変えられる
`thing` をハイライトしてくれるようになります。そこで `C-:` を押下すると、次の `thing` へと置き換えてくれます。

口で言ってもわかりにくいので、例として、C++の以下のようなソースコードを開いてみましょう。

```cpp
#include <iostream>
#include <vector>
#include <array>
#include <deque>

class c{
private:
    float f;
};

int main(){
    bool b = false;

    std::vector<int> v(1, 1);

    double d;

    std::cin >> d;

    if (d || b) {
        std::cout << d << "\n";
    } else {
        std::cout << b << "\n";
    }

    return 0;
}
```

ここにある シンボルはほとんどが `grugru` 可能です。以下にgifを置いておきます。
適当なところにカーソルをもっていって、ハイライトされたら `C-:` を押してみてください。
連打してもよいです。実際に使ってみたgifを以下においておきます。

<a id="figure--fig:c++-grugru-demo"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/35e323db33f4da1545c289f2741782c4ac04968b/c++-mode.gif" caption="<span class=\"figure-number\">Figure 8: </span>C++におけるgrugruのデモ" >}}


### 自分で定義する {#自分で定義する}

ここでは `grugru-default-setup` を使ってあらかじめ用意されたものを `grugru` していますが、
自分で定義することも可能です。定義するための関数は主に3つあります。

-   `(grugru-define-global GETTER STRINGS-OR-GENERATOR)`
-   `(grugru-define-on-major-mode MAJOR GETTER STRINGS-OR-GENERATOR)`
-   `(grugru-define-local GETTER STRINGS-OR-GENERATOR)`

書き方はとても簡単です。 `GETTER` の部分には、 `symbol` 、 `word` 、 `char` など、
どの範囲をひとつの `thing` としてみなすかを指定します。
`STRINGS-OR-GENERATOR` には、 `grugru` したい一連の文字列のリストを渡します。
`grugru-define-global` はEmacs全体を通じてそれらの文字列が `grugru` できるようになり、
`grugru-define-on-major-mode` は `MAJOR` で指定したメジャーモード(リストによる複数指定可)全体、
`grugru-define-local` は現在のバッファのみで `grugru` できるようになる、という違いがあります。
書き方の例を示します。

```emacs-lisp
(grugru-define-global 'word '("aaaa" "bbbb" "cccc"))
(grugru-define-on-major-mode '(c-mode c++-mode) 'symbol '("unsigned" "signed"))
(grugru-define-local 'char '("a" "b" "c"))
```

基本的には、 `GETTER` には `symbol` (Lispにおける識別子的な意味合い)を選んでおけば大丈夫です。
もし「camelCase」の「camel」の部分だけ、といった `symbol` 内の単語を対象にしたい場合は
`word` を、連続する記号類 (`&&` や `>>=` など)を対象にしたい場合は `non-alphabet`
を使用します。自前で定義することも可能ですが、高度な内容になるため[高度な内容](#technical-configuration)に預けます。

もし大文字小文字の情報を無視して定義したい場合は、 `STRINGS-OR-GENERATOR` に以下のように
書けばよいです。

```emacs-lisp
;; 誤植があったためコードを修正しました
(grugru-define-global 'word (grugru-metagenerator-keep-case '("aaa" "bbb" "ccc")))
;; AAA -> BBB -> CCC のような `grugru' が可能!
```

実は `STRINGS-OR-GENERATOR` には `GENERATOR` と呼ばれる関数を書くことができますが、
高度な内容になるため[高度な内容](#technical-configuration)に預けます。

また、 `grugru-define-global` と `grugru-define-on-major-mode` はいっぺんに定義するためのマクロが
用意されています。以下の3つは全て等価になります(上2つはLispの文法上等しい)。

```emacs-lisp
(grugru-define-multiple
 (fundamental-mode
  . ((word . ("aaa" "bbb" "ccc"))
     (symbol . ("xxx" "yyy" "zzz"))
     (word . ("abc" "def" "ghi"))))
  (word . ("aaaa" "bbbb" "cccc"))
  (symbol . ("xxxx" "yyyyy" "zzzzz"))
  (word . ("abcd" "defd" "ghid")))

(grugru-define-multiple
 (fundamental-mode
   (word "aaa" "bbb" "ccc")
   (symbol "xxx" "yyy" "zzz")
   (word "abc" "def" "ghi"))
  (word "aaaa" "bbbb" "cccc")
  (symbol "xxxx" "yyyyy" "zzzzz")
  (word "abcd" "defd" "ghid"))

(progn
  (progn
     (grugru-define-on-major-mode 'fundamental-mode 'word '("aaa" "bbb" "ccc"))
     (grugru-define-on-major-mode 'fundamental-mode 'symbol '("xxx" "yyy" "zzz"))
     (grugru-define-on-major-mode 'fundamental-mode 'word '("abc" "def" "ghi")))
   (grugru-define-global 'word '("aaaa" "bbbb" "cccc"))
   (grugru-define-global 'symbol '("xxxx" "yyyyy" "zzzzz"))
   (grugru-define-global 'word '("abcd" "defd" "ghid")))
```

ちなみに、複雑な `grugru` にも対応すべく、 `GETTER` や `STRING-OR-GENERATOR` には関数を
与えることが可能になっています。詳細はここでは省きますが、
`GETTER` は引数なしでカーソル位置から `thing` の始点と終点のコンスセルを返し、
`STRING-OR-GENERATOR` は第一引数として貰った候補が有効なら次の候補(第二引数が `non-nil` なら前の候補)
を返せばよいです。


#### 高度な内容 {#technical-configuration}

この項の内容はやや高度です。Emacs Lispをある程度理解している方向けになります。興味のないかたは飛ばしていただいて
かまいません。

`grugru-define-*` における `GETTER` と `STRINGS-OR-GENERATOR` は、本質的には関数です。
`GETTER` で指定できる `symbol` や `word` は `grugru-getter-alist` から `alist-get` を通じて関数に置き換えられます。
`STRINGS-OR-GENERATOR` で指定できる文字列のリストは、 `grugru-strings-metagenerator` に格納された高階関数に通すことで
関数 `GENERATOR` に置き換えられます。以下はそれらを内部で行っている関数です。

```emacs-lisp
(defun grugru--get-getter-function (getter)
  "Get getter function from GETTER."
  (setq getter (or (cdr (assq getter grugru-getter-alist)) getter))
  (pcase getter
    ((pred functionp)
     getter)
    ((pred integerp)                    ;「local な `grugru' をinteractiveに定義する」を参照
     (apply-partially #'grugru--metagetter-with-integer getter))
    (_ `(lambda () ,getter))))

(defun grugru--get-generator (strings-or-generator)
  "Return generator from STRINGS-OR-GENERATOR."
  (if (functionp strings-or-generator) strings-or-generator
    (funcall grugru-strings-metagenerator strings-or-generator)))
```

<!--list-separator-->

-  Getter

    `GETTER` は、引数をとらず、2つのポイントをコンスセルにして返す関数です。
    返り値は `bounds-of-thing-at-point` と同じで、 `car` は現在のバッファにおける開始位置、 `cdr` は終了位置のポイントになります。
    `grugru` は、この間にある部分を `GENERATOR` に渡し、次の候補を得ようと試みます。

    `GETTER` の例として、 `word` に対応する `GETTER` を以下に示しておきます。

    ```emacs-lisp
    (defun grugru--getter-word ()
      "Get beginning/end of word at point."
      (if (or (eq (point) (point-at-eol))
              (string-match "[-\\[\\]_:;&+^~|#$!?%'()<>=*{}.,/\\\\\n\t]\\| "
                            (buffer-substring (point) (1+ (point)))))
          (save-excursion (cons (subword-left) (subword-right)))
        (save-excursion
          (let ((x (subword-right))
                (y (subword-left)))
            (cons y x)))))
    ```

<!--list-separator-->

-  Generator

    `GENERATOR` は、1つの引数と1つの省略可能引数をとり、 文字列 `next-string` 、 `(valid-bounds . next-string)~、 ~nil` のどれかを返します。
    一つ目の引数は文字列 `STRING` で、 `GENERATOR` はこの文字列の次の文字列を返します。もし省略可能な第二引数 `REVERSE` が
    `non-nil` である場合、次の文字列の代わりに **前** の文字列を返します。次の文字列として該当するものがない場合は `nil` を
    返します。 `valid-bounds` は `(BEGIN . END)` のリストです。
    これは `bounds-of-thing-at-point` の返り値とほぼ同じ意味合いですが、 `STRING` の開始位置を `0` としたときの位置になります。
    `valid-bounds` を指定するち、指定した範囲のどこにもカーソルがない場合は該当する文字列なし(つまり返り値 `nil`)として扱ってくれます。
    次の文字列を算出するのにより広い範囲の文字列を必要とするが、事実上のターゲット文字列はもっと狭い、もしくは
    カーソル位置がある位置にない場合は対象にするべきでない、というようなケースで使えます
    (例: 関数の呼び出しに用いられている括弧だけを対象にしたい。/
    関数名を `grugru` するときに、一緒に引数の順番も入れ替えたいが、カーソルが引数上にあるときは対象にしたくない。)。

    `GENERATOR` の例として、 `grugru-default.el` で定義されている `grugru-default@emacs-lisp+nth!aref` を以下に示しておきます。
    この `generator` は、カーソルが `nth` か `aref` にある場合に限り、 `(nth 1 lst)` と `(aref lst 1)` のような組み合わせを
    `grugru` します。
    なお、 `grugru-utils-lisp-exchange-args` は関数呼び出し形をしたS式として `read` できる文字列と
    数学的な意味での置換(permutation)を与えることで、引数部分を置換した文字列を返してくれる関数です。

    ```emacs-lisp
    (defun grugru-default@emacs-lisp+nth!aref (str &optional _)
      "Return STR exchanged `nth' and `aref' with argument permutation."
      (cond
       ((string-match "^(\\_<\\(nth\\)\\_>" str)
        (cons
         (cons (match-beginning 1) (match-end 1))
         (grugru-utils-lisp-exchange-args
          (replace-match "aref" nil nil str 1)
          '(2 1))))
       ((string-match "^(\\_<\\(aref\\)\\_>" str)
        (cons
         (cons (match-beginning 1) (match-end 1))
         (grugru-utils-lisp-exchange-args
          (replace-match "nth" nil nil str 1)
          '(2 1))))))
    ```


### local な `grugru` をinteractiveに定義する {#interactive-grugru-define}

実は、 `grugru-define-local` は、interactiveに定義することができます。
バッファ使い捨ての `grugru` をぱっと定義したいときに有用です。実行すると、単に置換したい
2つの文字列を聞かれます。略語の展開のような意図で使うことを想定しているため、デフォルトの `GETTER` は
「カーソルから1つ目の候補の文字列の長さ分前に戻った部分まで」となっています。
また、リージョンがアクティブなら中身を2つ目の文字列として自動で入力されます。
もし `GETTER` や置換したい文字列の数を指定したい場合は、前置引数 `C-u` を付けて実行してください。

<a id="figure--fig:grugru-define-local-interactively"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/698f33489645a6e7b0c29d879771dbb15fa3fcd9/grugru-define-local.gif" caption="<span class=\"figure-number\">Figure 9: </span>`grugru-define-local` をinteractiveに使う" >}}


### 選択肢の中から `grugru` を選んで適用する {#選択肢の中から-grugru-を選んで適用する}

さて、たくさんの `grugru` を定義すると、同じ `thing` に対して複数の `grugru` 候補がある場合が出てくる
かもしれません。その場合、 `grugru` コマンドは単に一番優先度の高いものを実行します。優先度は
「local&gt;major-mode&gt;global」になっていて、同じ優先度の中では「後に定義されるほど強い」というふうに
なっています。

しかし、せっかく定義しても使えなければ意味がありません。そこで、 `grugru-select` というものが用意
されています。とても素直な関数なので使ってみればわかると思いますが、
(あれば)複数の `grugru` 候補の中から適用したいものを選択し、さらにどの文字列へと置換するのかを選択する、
というものです。この関数は、複数の `grugru` 候補がある場合だけでなく、 `grugru` をたくさん連打しないと
目的の文字列まで到達できない時に、絞り込みによって一気に到達するのにも有用です。画像では使って
いませんが、 `ivy` などの候補選択ライブラリと一緒に使うとより快適だと思います。

<a id="figure--fig:grugru-select"></a>

{{< figure src="https://raw.githubusercontent.com/ROCKTAKEY/images/netlify/2021-eb9f86f0-ee65-1824-4063-ed0d0ba15dbf/grugru-select.png" caption="<span class=\"figure-number\">Figure 10: </span>`grugru-select` による置換先の選択画面" >}}


### `grugru` を再定義する {#grugru-を再定義する}

`grugru` を実行して、やっぱりこれは違うな、と思ったとき、その場でサクっと再定義できると便利ですよね。
設定ファイルにこだわりがない場合は、 `grugru-edit` を利用するとよいです。現在のカーソルで有効な
`grugru` を再定義できます。

この関数で再定義した値のうち、グローバルなものとメジャーモードローカルのものは
`grugru-edit-save-file` に保存されます。 `init.el` に以下のような記述をすれば、
その設定は次回起動時にも読み込まれます。

```emacs-lisp
(grugru-edit-load)
```

保存されたファイルは単なるEmacs Lispの式ですので、気に入らないことがあれば手で編集しても構いません。

ただし、 `grugru-edit` を使うと設定が分散してしまうので、 `init.el` に設定を集約することにこだわりがある人には
おすすめできません。 `leaf` のキーワードにも対応していますし、当然直接定義を書いても問題ないので、
そういう人は素直に `init.el` に書いてください。

再定義関数は `grugru-redefine-*` のような名前になっていて、最後の引数として新しい `STRING-OR-GENERATOR`
を与えることで再定義できます。 `nil` を与えれば無効にできます。
`init.el` に直接書く場合、自分で定義したものは書き換えれば済むので、
主にデフォルトの挙動を変えたいときに使うことになりそうです。


### 独立した `grugru` を定義する {#独立した-grugru-を定義する}

ここまで、なるべく負荷を減らすべく、 `grugru` という単一のコマンドによって `thing` の置換を
行うようにしてきました。しかし、時には、もしくは人によっては、「いくつかの `thing` だけを
置換するようなコマンドを独立して定義したい」ということがあると思います。そういう人のために、
`grugru-define-GENERATOR` というものが用意されています。基本的な引数は `defun` に準じます。
`body` 部の文法はメジャーモード指定ができない以外は `grugru-define-multiple` と同様です。
たとえば以下の `three-state` は、"water"、"ice"、"vapor"の3つと、"solid"、"liquid"、"gas"の3つ
のみを順に置換することができ、それ以外はいっさい置換できません。

```emacs-lisp
(grugru-define-generator three-state ()
 "Docstring. This is optional."
 (symbol . ("water" "ice" "vapor"))
 (symbol . ("solid" "liquid" "gas")))
```


### 結論 {#結論}

ここに書ききれていないことも多々ありますので、詳細は[README.org](https://github.com/ROCKTAKEY/grugru#readme)
を見てね!! 質問などあれば、コメント欄、githubのDiscussionやissue(日本語可能)、メールなどなんでもいいのでぜひ
ご連絡ください!!

みなさんも `grugru` といっしょに、ストレスのないコーディングを楽しみましょう!!


### お願い {#お願い}

私があまり知らない言語では、どのような `thing` が置換されうるのかが分からないため、デフォルトが
あまり充実していないのが現状です。issueでもPRでも、SlackやTwitterのDMでも構いませんし、
フォーマットの有無や日本語英語も問いませんので、どの言語(どれかひとつの言語で構いません)に
どのような `grugru` できそうなペアがあるか(1つだけでも構いません)、教えていただけると嬉しいです。

あと、良かったら投げ銭してってね!


## <span class="org-todo done DONE">DONE</span> Surface Laptop 1にUbuntuを(LUKSによる暗号化つきで)インストールする {#surface-laptop-1にubuntuを--luksによる暗号化つきで--インストールする}

基本的には[参考](#surface-linux-reference)にあるサイトに準拠しています。


### 必要なもの {#必要なもの}

-   USBハブ (USB端子が1つしかないため、以下の2つを同時に接続するために必要)
-   USBキーボード (途中までSurfaceにもとからついているキーボードは使えません)
-   USBメモリ (Ubuntuのブート用)


### ブート用のUbuntuをUSBメモリに用意する {#ブート用のubuntuをusbメモリに用意する}

基本[ここ](https://zenn.dev/sprout2000/articles/52caaffa9ad3fa#1.-usb-%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB%E3%83%87%E3%82%A3%E3%82%B9%E3%82%AF%E3%81%AE%E4%BD%9C%E6%88%90)のとおりにやればよい。あらかじめ作っておいてある人もいそう。ここ以外にもたくさん文献があると思うので
ここでは説明しない。


### UEFIでセキュアブートを無効にする {#uefiでセキュアブートを無効にする}

音量アップボタン(F6)を押しつづけながら、電源を入れる。
もしくは、Windowsを立ち上げて「設定&gt;更新とセキュリティ&gt;PCの起動をカスタマイズする&gt;今すぐ再起動&gt;
トラブルシューティング&gt;詳細オプション&gt;UEFIファームウェアの設定」と行く。UEFIは、BIOSの進化版みたいな
ものらしい。

UEFIに入ったら、「Security&gt;Secure Boot&gt;Change configuration」をクリックして、「Microsoft only」から
「None」に変更する。LUKSで暗号化すればどうせSSDの中身はパスワードなくして読めないので、
セキュアブートはなくても問題ない気がしている(本当か？)。
後でLUKSのパスワードを求められたときには堅牢なパスワードにしておきましょう。

最後に、Boot configurationタブで、USB Storageを一番上にもっていく。これで、Windowsではなく
USBにあるUbuntuが先に立ち上がるようになる。


### Ubuntuを起動する {#ubuntuを起動する}

まず、SurfaceにUbuntuの入ったUSBとキーボードを、USBハブ経由で繋ぐ。
そして、「exit&gt;Restart now」すると、Ubuntuが立ち上がる。


### Ubuntuをインストール {#ubuntuをインストール}

基本は下記のURLに従ってもらえればいいが、暗号化するのに注意がある。
「インストールの種類」のタイミングで、「ディスクを削除してUbuntuをインストール」すると思うが、
ここで「高度な機能」をクリックし、「新しいUbuntu」の「インストールにLVMを使用する」を選択し、
「安全のために新しいUbuntuのインストールを暗号化する」にチェックを入れて欲しい。
これで、bitlockerと同様にディスクが暗号化される。

URLは[これ](https://linuxfan.info/ubuntu-20-04-install-guide)。「ストレージ設定」の見出しのところに
その画面がある。この記事では暗号化をしていないので、一切触れられていない。
この後パスワードを入力させられる(パスワードなしでもいけるが、セキュアブートなしだと危うそう)。
そのとき、バックアップキーが表示されるので、パスワードを忘れたときのためにどこかにメモっておく。


### Surface用のカーネルを導入 {#surface用のカーネルを導入}

次に、Surfaceの各種デバイスが使えるよう、専用のカーネルをインストールする。
基本は[ここ](https://github.com/linux-surface/linux-surface/wiki/Installation-and-Setup#debian--ubuntu)に買いてある通りでよい。ただし、

```shell { linenos=table, linenostart=1 }
sudo apt install linux-surface-secureboot-mok
sudo update-grub
```

に関しては、セキュアブートしないのならば必要ない。
私はこれを実行した上で、UEFIにおいてセキュアブートを「Microsoft and 3rd party CA」としても
セキュアブートできなかったので断念した。余力があればチャレンジしてほしい。


### キーボードをLUKSパスワード入力時にも使えるようにする {#キーボードをluksパスワード入力時にも使えるようにする}

このまま再起動すると、起動時にLUKSのパスワードを聞かれる。
しかし、その時点で読み込まれるファイルシステムはSurfaceのキーボードのドライバを含んでいないらしく、
外部キーボードでしか入力できない。これでは使いものにならないので、最初に読まれるモジュールを追加する。

エディタは問わないが、 `/etc/initramfs-tools/modules` を開いて、以下を末尾に追加する。
root権限が必要なので注意。

```shell { linenos=table, linenostart=1 }
surface_aggregator
surface_aggregator_registry
surface_hid_core
surface_hid
intel_lpss
intel_lpss_pci
8250_dw
```

その後、以下を実行する。こちらも `sudo` が必要。

```shell { linenos=table, linenostart=1 }
update-initramfs -u
```


### おわり {#おわり}

あとは再起動すれば、SurfaceのロゴとともにLUKSのパスワードを聞かれるので、Surface備え付けのキーボードで
パスワードを打てることがわかる。


### 参考 {#surface-linux-reference}

-   <https://zenn.dev/sprout2000/articles/52caaffa9ad3fa>
-   <https://github.com/linux-surface/linux-surface>

[^fn:1]: Emacsにおいては、一般に言うウィンドウをフレームと言い、フレームを分割したものをウィンドウと言う。
[^fn:2]: なお、この操作は必須ではない。名前とは矛盾するが、 `tab-bar-mode` がオフでも
    `tab` 系の関数を使うことができる。
