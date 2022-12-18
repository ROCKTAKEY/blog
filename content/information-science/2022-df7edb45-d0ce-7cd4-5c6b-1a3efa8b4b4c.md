+++
title = "Emacsのパッケージを作る際の手引き"
author = ["ROCKTAKEY"]
date = 2022-12-19T04:32:00+09:00
lastmod = 2022-12-19T04:33:20+09:00
tags = ["Emacs", "Emacs-Lisp"]
draft = false
+++

{{< figure src="https://img.shields.io/badge/Emacs%20Advent%20Calendar%202022-12%E6%97%A5%E7%9B%AE-d60a34.svg?style=flat-square&logo=qiita" link="https://qiita.com/advent-calendar/2022/emacs" >}}


## 序文 {#序文}

Emacsは完全に素のまま使うと若干不便なところがたくさんあるので、
Emacs Lispを利用してカスタマイズすることがあるかと思います。単にEmacsの変数を触る程度であれば問題はありませんが、自分の欲しい機能を設定ファイル `init.el` に直接書いていると、保守の観点であまりよろしくありませんし、人に使ってもらうのも大変になってしまいます。

一方、ある程度大きくなった機能をパッケージへと切り出しておけば、履歴も設定ファイルとは別に管理できますし、人に使ってもらうのも簡単です。この記事では、Emacs Lispのパッケージの雛形を詳細に解説し、みなさんがパッケージを作る一助にしたいと思います。

なお、この記事は雛形の展開結果を元に話を進めていますが、この記事はこの雛形を利用して欲しいがために作ったというより、パッケージを作り方を伝授するために作ったものです。なので、別にこのテンプレートを利用しなくてもよいですし、しなくてもよいように説明します。


## 今回利用するサービス及びソフトウェア {#今回利用するサービス及びソフトウェア}


### 外部サービス {#外部サービス}

この記事では、開発を便利に進めるためにいくつかの外部サービスを利用します。それらの外部サービスを利用したくない場合はテンプレートをそのまま利用はできませんが、それぞれのファイルの役割は逐一解説し、各自で個々の要素を利用できるよう努めます。

以下に、利用する外部サービスを列挙します。

-   [GitHub](https://github.com/)
    -   GitHubによるgitホスティング
    -   [GitHub Actions](https://docs.github.com/ja/actions)
        -   GitHub上でテストなどを行うために計算リソースを使わせてくれるサービス
        -   テスト及びlintを行うために利用
    -   利用する場合、[ここ](https://github.com/join)からアカウントを作っておいてください。
-   [CodeCov](https://about.codecov.io/)
    -   テストカバレッジを取得してまとめてくれるサービス
    -   利用する場合は[ここ](https://docs.codecov.com/docs)に示されるようにアカウントを作り、GitHubと連携しておいてください。publicレポジトリなら `CODECOV_TOKEN` は不要です。
-   [shields.io](https://shields.io/)
    -   バッジを生成してくれるサービス
    -   READMEにおいてテストの状態やライセンス、タグなどを表示するのに使用
    -   アカウントは必要ありません
    -   いいサービスだと思ったら是非[寄付](https://opencollective.com/shields)して差し上げましょう


### ローカルで利用するソフトウェア {#ローカルで利用するソフトウェア}

以下に、ローカルで利用するソフトウェアを示します。

-   [Emacs](https://www.gnu.org/software/emacs/)
-   [keg.el](https://github.com/conao3/keg.el)
    -   Emacs Lispのプロジェクトの依存を管理するパッケージマネージャ及び便利ツール
    -   グローバル環境(つまりユーザーのEmacs環境)を汚さずにパッケージの依存をインストールできる(`npm` みたいなもの)
    -   類似ツールとして[Cask](https://github.com/cask/cask)などがある
    -   インストールは簡単で `git clone https://github.com/conao3/keg.el.git ~/.keg` を実行して `~/.keg` ディレクトリに `keg.el` をインストールした後、
        `PATH=$HOME/.keg/bin:$PATH` としてパスを通すだけです。
-   [Cookiecutter](https://github.com/cookiecutter/cookiecutter)
    -   テンプレートを展開するソフトウェア
    -   [GitHubのテンプレート機能](https://docs.github.com/ja/repositories/creating-and-managing-repositories/creating-a-template-repository)はレポジトリの内容をまるまるコピーすることしかできないため、テンプレートの展開時に対話的に入力を入れられるこのソフトウェアを選んだ


## Cookiecutterを利用してテンプレートを展開してみる {#cookiecutterを利用してテンプレートを展開してみる}

まずはテンプレートを展開し、それにより生成されたコードを解説していく方向でいこうと思います。もしテンプレートを利用したくない場合、この章は飛ばして構いません。私の作成した、今回使うテンプレートは[ここ](https://github.com/ROCKTAKEY/cookiecutter-emacs-lisp)にあります。
`emacs-lisp-package-sample` という名前のGithubレポジトリで、
`sample` というパッケージ及びプロジェクトの名前のパッケージを想定して作ってみましょう。

```shell
cookiecutter https://github.com/ROCKTAKEY/cookiecutter-emacs-lisp
```

すると、以下のようなプロンプトが表れます。

```text
project_name [project-name]:
```

ここではプロジェクト名を聞いているので、 `sample` と入力してEnterキーを押します。この値はEmacs Lispパッケージの `feature` 名及び各シンボルの接頭辞として利用されます。

するとさらに以下のようなプロンプトが得られます。

```text
project_name [project-name]: sample
github_repository_name [sample]:
```

このように、対話的にプロジェクトを作成することができます。ここではGitHubのレポジトリ名を聞いているので `emacs-lisp-package-sample` と入力します。プロジェクトへのURLの生成及びディレクトリ名に用います。

```text
project_short_description [Short description of your project]: This is sample for package
```

ここではプロジェクトの概要を端的に入力します。
README及びメインのEmacs Lispファイルの説明文などに用いられます。

```text
full_name [Your Name]: Your Name
```

ここではあなたの本名を入力します。著作権表記に用います。必ずしも本名でなければならないわけではありません。

```text
email [email@example.com]: youremail@example.com
```

ここにはemailアドレスを入力します。あなたの連絡先を示すのに用います。

```text
github_username [github]: username
```

ここにはあなたのGitHubのユーザー名を入力します。この値もレポジトリのURLを生成するのに利用します。

```text
year [2022]:
```

ここには現在の年を入力します。著作権の始まりを表すのに利用します。空欄にすれば自動で現在の年が入力されます。

```text
Select keyword:
1 - abbrev
2 - bib
3 - c
4 - calendar
5 - comm
6 - convenience
7 - data
8 - docs
9 - emulations
10 - extensions
11 - faces
12 - files
13 - frames
14 - games
15 - hardware
16 - help
17 - hypermedia
18 - i18n
19 - internal
20 - languages
21 - lisp
22 - local
23 - maint
24 - mail
25 - matching
26 - mouse
27 - multimedia
28 - news
29 - outlines
30 - processes
31 - terminals
32 - tex
33 - tools
34 - unix
35 - vc
36 - wp
Choose from 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36 [1]: 33
```

とても長いプロンプトが出てきました。ここには、このプロジェクトがどのキーワードに対応するかどうかを入力します。
`<f1> p` (`finder-by-keyword`)をEmacsで実行するとそれぞれのキーワードが何を表すかを見ることができます。以下に内容を転記しておきます。このキーワードはメインのEmacs Lispファイルのヘッダ及び `defgroup` に必要なのですが、キーワードはかなり偏ったものが多いため、 `tools` を選ばざるを得ないことが多いです。今回も `tools` を表す `33` を入力します。

```text
abbrev        abbreviation handling, typing shortcuts, and macros
bib           bibliography processors
c             C and related programming languages
calendar      calendar and time management tools
comm          communications, networking, and remote file access
convenience   convenience features for faster editing
data          editing data (non-text) files
docs          Emacs documentation facilities
emulations    emulations of other editors
extensions    Emacs Lisp language extensions
faces         fonts and colors for text
files         file editing and manipulation
frames        Emacs frames and window systems
games         games, jokes and amusements
hardware      interfacing with system hardware
help          Emacs help systems
hypermedia    links between text or other media types
i18n          internationalization and character-set support
internal      code for Emacs internals, build process, defaults
languages     specialized modes for editing programming languages
lisp          Lisp support, including Emacs Lisp
local         code local to your site
maint         Emacs development tools and aids
mail          email reading and posting
matching      searching, matching, and sorting
mouse         mouse support
multimedia    images and sound
news          USENET news reading and posting
outlines      hierarchical outlining and note taking
processes     processes, subshells, and compilation
terminals     text terminals (ttys)
tex           the TeX document formatter
tools         programming tools
unix          UNIX feature interfaces and emulators
vc            version control
wp            word processing
```

これによってディレクトリ `emacs-lisp-package-sample` がカレントディレクトリに生成されます。生成されたディレクトリは、以下のような構造になっているはずです。

```text
.
├── .dir-locals.el
├── .github
│   └── workflows
│       └── test.yml
├── .gitignore
├── Keg
├── LICENSE
├── README.org
├── codecov.yml
├── sample.el
└── test
    └── sample-test.el
```

各ファイルの役割を説明していきます。


## パッケージを構成する各ファイルの役割 {#パッケージを構成する各ファイルの役割}

前章を飛ばした方のために、もう一度ディレクトリ構造を示します。
`sample` というプロジェクト名であることに留意してください。

```text
.
├── .dir-locals.el
├── .github
│   └── workflows
│       └── test.yml
├── .gitignore
├── Keg
├── LICENSE
├── README.org
├── codecov.yml
├── sample.el
└── test
    └── sample-test.el
```

これらの各ファイルについて解説していきます。先にEmacs Lisp以外のファイル(テスト周りを除く)を説明し、その後Emacs Lispファイルを説明、最後にテスト周りのファイルについて触れます。


### README.org {#readme-dot-org}

耳にたこができるほどよく見る言説かもしれませんが、
READMEは、このパッケージを調べて辿りついた人が最初に見るファイルです。端的にパッケージの概要を示しつつ、ドキュメントを付けておきましょう。テンプレートでは以下のような内容になっています。org-modeで書いています。

```org
[[https://github.com/username/sample][https://img.shields.io/github/tag/username/sample.svg?style=flat-square]]
[[file:LICENSE][https://img.shields.io/github/license/username/sample.svg?style=flat-square]]
[[https://codecov.io/gh/username/sample?branch=master][https://img.shields.io/codecov/c/github/username/sample.svg?style=flat-square]]
[[https://github.com/username/sample/actions][https://img.shields.io/github/workflow/status/username/sample/test/master.svg?style=flat-square]]
* sample: This is sample for package

* How to Use?
* License
  This package is licensed by GPLv3. See [[file:LICENSE][LICENSE]].
```

最初の4行はShields.ioによって生成されたバッジです。もしGitLabなどの他のサービスでホスティングする場合はURLを適宜変更する必要があります。

始めのヘッドラインにはパッケージの説明と概要を入れています。ここに詳細な説明を適宜書いてください。動画やGIFを撮ってここに置いておくとどんなパッケージなのかがグッとわかりやすくなります。その際は[keycast](https://github.com/tarsius/keycast)パッケージを利用してモードラインに現在のキー入力を表示させるとよいです。

その他最低限のヘッドラインを用意しています。ライセンスについては後述します。


### LICENSE {#license}

ファイルの内容は長いので省略します。ここには[GNU General Public License version 3.0](https://www.gnu.org/licenses/gpl-3.0.html)(GPLv3)のライセンス文がそのまま書かれています。
Emacs自体がGPLv3の下で公開されていることもあり、Emacs LispのパッケージもGPLv3の下で公開されることが多いです。そのため、ここではGPLv3を採用しています。もちろん他のライセンスでも構いませんが、今後パッケージアーカイブに入れてもらう可能性を考えると、GPLv3に互換のあるライセンスであることが望ましいです。また、GPLv3以外を利用する場合は後述のようにメインのEmacs Lispファイルのヘッダのライセンスに関する文言も変更する必要があります。


### .gitignore {#dot-gitignore}

```text
flycheck_*
*.elc
.keg/
```

.gitignoreファイルです。Git管理のレポジトリにおいて、Gitの履歴に含めないようなファイル群を指定しています。
[Flycheck](https://www.flycheck.org/en/latest/)を利用する場合にたまに見え隠れするファイル `flycheck_*` 、バイトコンパイル後のファイル `*.elc` 、及び後述するプロジェクトの依存するパッケージ群のあるディレクトリ `.keg/` (`npm` で言う `node_modules` 相当)
の3種を指定しています。


### sample.el {#sample-dot-el}

```emacs-lisp
;;; sample.el --- This is sample for package  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  yourname

;; Author: yourname <youremail@example.com>
;; Keywords: tools

;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/username/emacs-lisp-package-sample

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is sample for package

;;; Code:

(defgroup sample ()
  "This is sample for package"
  :group 'tools
  :prefix "sample-"
  :link '(url-link "https://github.com/username/emacs-lisp-package-sample"))

(provide 'sample)
;;; sample.el ends here
```

ここには様々な情報が書かれています。分解して説明します。

```emacs-lisp
;;; sample.el --- This is sample for package  -*- lexical-binding: t; -*-
```

1行目はファイル名、概要、 `-*- lexical-binding: t; -*-` の3つの内容が書いてあります。この3つはいずれも(ほぼ)必須の定型フォーマットになります。特に概要の部分は、パッケージアーカイブに登録した場合にパッケージの概要として表示されます。なお、概要にピリオドは不要です。

`-*- lexical-binding: t; -*-` は見慣れない記述かもしれません。これは[ファイルローカルな変数を指定する記法](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html)です。ここでは `lexical-binding` を `t` に指定しています。これによりレキシカルバインディングを有効にします。レキシカルバインディングは日本語では字句的束縛で、端的に言えば「関数の実行に関数が ****定義された時の**** 環境を利用する」ことを意味します。一方Emacs Lispではダイナミックバインディングを採用します。これは日本語では動的束縛で、端的には「関数の実行に関数が ****実行される時の**** 環境を利用する」ことを意味します。通常のプログラミング言語の大半は字句的束縛を採用していることもあり、特に問題がない場合はEmacs Lispでも字句的束縛を採用します。定義していない変数にアクセスしようとするなどの行儀の悪いコードを書かなければ、あまり意識する必要はありません。

```emacs-lisp
;; Copyright (C) 2022  yourname

;; Author: yourname <youremail@example.com>
;; Keywords: tools

;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/username/emacs-lisp-package-sample
```

これはパッケージにおけるヘッダコメントで、メタデータが書かれています。コピーライト表記及び筆者の情報として `yourname` と `youremal@example.com` が利用されています。また、キーワードについてもここで指定しています(選択肢については `<f1> p` `finder-by-keyword` を参照)。

さらにバージョンを指定しています。バージョン表記は通常SemVer表記(0.0.0表記)を用います。バージョンをきちんと管理していないパッケージも多いですが、他のパッケージから依存されることなどを考えると、きちんとこまめにバージョンを切ったほうがよいです。

ホームページとしてURLも指定します。ここではGitHubを前提とし、
`username` というユーザー名、 `emacs-lisp-package-sample` というレポジトリ名でURLを生成していますが、
GitLabでもよいですし、独自に用意した任意のホームページで構いません。

特筆すべきは `Package-Requires` です。ここには依存するパッケージを書いていきます。
Emacsそのものもパッケージと同様に扱い、最低どのバージョンを求めるかをここに書きます。デフォルトでは `24.1` からの指定となりますが、正直かなり古いので `25.1` や `26.1` を指定してもいいと思います。特に `24.4` や `24.5` あたりでかなり劇的に変化を遂げたので、そのあたりになってしまうこともあります。後述するlinterによって最低のEmacsバージョンを自動で教えてくれるので、そのままでもよいかもしれません。

他のパッケージに依存する場合、バージョンは該当パッケージのヘッダコメント `Version` にあるバージョンを指定します。たとえば[dash.el](https://github.com/magnars/dash.el)の `2.19.1` と[s.el](https://github.com/magnars/s.el)の `1.13.1` を依存として利用したいなら、以下のように書きます。

```emacs-lisp
;; Package-Requires: ((emacs "24.1") (dash "2.19.1") (s "1.13.1"))
```

このように、 `Package-Requires` に `(package-name "version")` のリストを渡すことで依存を記述します。ここに記述された依存は、このパッケージのインストール時や後述する `keg install` を初めとした開発ツールによって自動でインストールされます。

```emacs-lisp
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
```

ここにはライセンスの説明が書いてあります。
GPLv3が前提となっています。ここは定型文なので、ライセンスとしてGPLv3を利用するのであればこのままで構いません。

```emacs-lisp
;;; Commentary:

;; This is sample for package
```

ここにはパッケージの詳細な説明を記述します。テンプレートでは概要をそのまま書いていますが、ここには ****詳細な説明**** を書いてください。パッケージアーカイブに登録した場合、パッケージの詳細として表示されます。
READMEから生成するのもよいかもしれません。

```emacs-lisp
;;; Code:
```

```emacs-lisp
(provide 'sample)
;;; sample.el ends here
```

上記のように、実際のコードの部分は `;;; Code:` コメントの行で始まり、
featureを提供する `(provide 'sample)` 節と `;;; sample.el ends here` コメントで終わる必要があります。コメント自体は動作に必須というわけではないですが、コーディング規約的なところで決まっており、後に説明するlinterで警告されます。

```emacs-lisp
(defgroup sample ()
  "This is sample for package"
  :group 'tools
  :prefix "sample-"
  :link '(url-link "https://github.com/username/emacs-lisp-package-sample"))
```

実際のコードとしてテンプレートから展開されるのはこの部分のみです。パッケージを書く際はこの下に書いていくことになります。

ここで定義しているのは `group` です。これは主にカスタマイズ変数を体系的に管理するために利用するもので、第一引数にグループ名を指定します。通常はパッケージ名をそのままグループ名としますが、所属するアイテム数が大きくなってしまうのであればカテゴリ毎に複数グループに分けても構いません。

第二引数は通常空リストです。グループに所属するシンボルとその編集方法をリストによって組にしたものをリストとして連ねたものを渡すことができますが、変数定義のときにグループを指定するのが一般的です。

第三引数はドキュメントです。1行目はピリオドで終わり、各行は最大でも80桁であることが望ましいです。ここではテンプレートに入れた文字列の都合上ピリオドなしになっています。これ以降の引数はキーワード引数となっていて、 `:keyword1 value1 :keyword2 value2 ...`
のような値を渡します。

`:group` として親となるグループを指定します。さきほどヘッダのコメントに書いたキーワードはグループとして必ず存在するため、ここに設定されていますが、他に適切な親グループが存在する場合(例えば派生パッケージでは派生元のパッケージのグループ)はそれを設定してください。

`:prefix` には、このグループにおける各シンボルの接頭辞を指定します。
Emacs Lispには名前空間が実質的に存在しないため、変数や関数がどのパッケージに所属するかを接頭辞で表します。通常はパッケージ名にハイフンを付けたものにします。

`:link` にはURLなどのリンクを渡します。 `url-link` 以外にもinfoファイルへのリンクである `info-link` やファイルへのリンクである `file-link` を渡すことができます。複数渡したい場合は何度もこの引数を指定すればよいです。


#### コードの書き方について {#コードの書き方について}

ここではコードの書き方の作法について述べます。
Emacs Lispは書けるけど、パッケージの作法がよくわからない、という人向けの節です。

<!--list-separator-->

-  グローバル変数定義

    グローバル変数を定義する方法は2種類あります。その二つを説明したあと、命名について述べます。

    <!--list-separator-->

    -  `defvar`

        ひとつは `defvar` を使った定義方法です。以下のように使います。

        ```emacs-lisp
        (defvar variable-name initial-value
          "document.")
        ```

        これによって初期値 `initial-value` の `variable-name` というグローバル変数が定義されます。
        `"document."` の部分には変数の説明が入ります。
        1行目はピリオドで終わる必要があり、各行は最大でも80桁であることが望ましいです。ドキュメントは省略可能ではありますが、可能な限り付けてください。後に述べるlinterで警告されます。これは通常のグローバル変数で、ユーザーに直接変更されることを想定しません。

    <!--list-separator-->

    -  `defcustom`

        もうひとつは `defcustom` を使った定義方法です。以下のように使います。

        ```emacs-lisp
        (defcustom customizable-variable-name initial-value
          "document."
          :group 'sample
          :version "0.0.0"
          :type 'number
          ;; :safe t
          ;; :risky t
          ;; :local t
          )
        ```

        最初の3つの引数は `defvar` と同様で、 `initial-value` を初期値とする
        `customizable-variable-name` という名前のカスタマイズ変数
        (`defcustom` で定義したグローバル変数はカスタマイズ変数と呼ぶ)を定義し、
        `"document."` で説明します。カスタマイズ変数はユーザーに変更されることを想定した変数です。そのため、ユーザーのための情報をキーワード引数として渡すことができるようになっています。代表的な例は以下になります。

        <!--list-separator-->

        -  `:group`

            所属するグループを与えます。通常はそのパッケージで定義したグループを与えます。

        <!--list-separator-->

        -  `version`

            その変数が初めて導入されたバージョンを与えます。

        <!--list-separator-->

        -  `type`

            型を示します。これは **静的型付けによる型エラーを検知するためのものではなく** 、ユーザーがGUIを用いて値を変更するときの入力支援や、ユーザーによる変更に対する型エラーを通達するためのものです。かなり複雑なので詳細は[マニュアル](https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization-Types.html)を参照して欲しいのですが、よく使うものだけ挙げておきます。

            `string`
            : 文字列

            `number`
            : 数値

            `integer`
            : 整数

            `float`
            : 浮動小数点数

            `regexp`
            : 正規表現

            `file`
            : ファイル

            `directory`
            : ディレクトリ

            `boolean`
            : 真偽値 (`nil` か `t`)

            `function`
            : 関数

            `variable`
            : 変数

            `(choice type1 type2 type3 ...)`
            : `type1` `type2` `type3` ... のいずれかの型。複雑な型を作るのに便利。

            `(const value)`
            : `value` という値のみを取ることができる定数型

            `(repeat type)`
            : `type` 型の要素がいくつか連なっているリスト

            `(alist :key-type type1 :value-type type2)`
            : `type1` 型のキーと `type2` 型の値で構成される連想リスト

            `(plist :key-type type1 :value-type type2)`
            : `type1` 型のキーと `type2` 型の値で構成されるプロパティリスト

        <!--list-separator-->

        -  `:safe` 、 `:risky`

            これは、その変数が第三者に自由に変更されても安全かどうかを規定します。
            Emacsにはファイルローカルな変数の値をあらかじめファイルの冒頭や末尾、 `.dir-locals.el` に書いておくことで、ファイルを開いたときに自動でその値を代入してくれる機能があります。通常インデントにタブを用いるか、インデントの幅をどうするかなどをEmacsに教えるのに利用しますが、悪用されれば任意のコードを実行されてしまうかもしれません。かといって全ての変数についてユーザーに尋ねていては鬱陶しいです。そこで、その変数が他人に書き換えられたときに危険な作用を及ぼし得るか否かをあらかじめ教えておくアプローチを
            Emacsはとっています。知らない人に勝手に代入されて絶対に安全ならば `:safe` を `t` に、危険なことが分かっている場合は `:risky` を `t` にします。なにも与えなければ、デフォルトの挙動となります。デフォルトにどう扱うかはユーザーのポリシーによりますが、通常はユーザーに一度だけ尋ね、承認されたら以降 `:safe` とします。
            `:risky` な変数は常にユーザーに尋ねます。 `:safe` な変数の場合はユーザーに尋ねず代入します。

        <!--list-separator-->

        -  `:local`

            そのカスタマイズ変数を既定でバッファローカルな変数とします。グローバル変数なのにローカルであることに不自然さを感じるかもしれませんが、あくまでもグローバル変数がバッファによって異なるようにできる、というだけです。グローバルスコープがバッファによって変化すると捉えてもよいかもしれません。

    <!--list-separator-->

    -  命名法

        Emacs Lispには名前空間が実質的に存在せず、従って公開変数や非公開変数のような機構も存在しません。つまり、あらゆる変数や関数はユーザーがアクセスできます。これは熟練のEmacs Lisperがアドバイスなどを介してカスタマイズをしやすいという利点もありますが、ライトユーザーにとってみれば利用されることを想定した変数や関数を簡単に見分けられないのは困ります。そこで、Emacs Lispでは名前によってユーザー用かどうかを区別します。具体的には、先程グループの定義で示した接頭辞の末尾のハイフンが1つならばユーザー用、二つなら内部実装用です。例を示します。

        ユーザー用
        : `sample-command` 、 `sample-command-for-you`

        内部実装用
        : `sample--function` 、 `sample--function-for-me`

        先程述べたように、 `defcustom` はユーザーに変更されることを想定しているので、常にハイフンは1つです。一方 `defvar` についてはどちらもあり得ます。ユーザーに変更されることは想定していなくても、ユーザーに値を参照されることを想定している可能性があるためです。このような命名は、ユーザーに「この関数や変数は互換性を保つ対象である」と明示する役割があると言えます。

<!--list-separator-->

-  関数定義

    関数定義に書くことはあまりないです。 `defun` を利用し、以下のように書きます。

    ```emacs-lisp
    (defun sample-function-name (arg1 arg2 &optional arg3)
      "Add ARG1, ARG2 and ARG3."
      (interactive)
      (+ (or arg1 1) (or arg2 1) (or arg3 1)))
    ```

    第一引数に関数名を渡します。命名については[命名法の節](#命名法)を参照してください。

    第二引数には引数のリストを渡します。
    `&optional` 以降の引数は渡さなくてもよく、渡されなければ `nil` を束縛します。
    `&rest` の後の1つの引数は残りの全ての引数をリストとして束縛します。

    第三引数には関数の説明(ドキュメント)を書きます。今まで通り各行は80桁以下、1行目はピリオドで終わるのが望ましいです。注意点として、このドキュメントには全ての引数の説明が出てくる必要があります。引数は全てを大文字にして記述します。これを行わなかった場合、後述するlinterに警告を受けます。

    残り部分は実引数が仮引数に束縛された状態で実行されます。一番上の `(interactive)` だけは特別で、これを付けることにより、ユーザーが `M-x` (`execute-command`)やキー割り当てによって直接(Emacs Lispとしての明示的な評価を介さずに)
    実行できるようになります。このようにユーザーが直接呼び出すことのできる関数を **コマンド** と呼びます。
    `interactive` について説明するとそれだけで一記事になってしまうので、ここでは割愛します。詳しくは[マニュアル(翻訳)](https://ayatakesi.github.io/lispref/28.2/elisp-ja.html#Defining-Commands)をご覧ください。

<!--list-separator-->

-  マクロ定義

    この節は難しいので、飛ばしても構いません。大半のパッケージはマクロを定義していないので、パッケージを作るにあたってそこまで支障はないです。

    マクロ定義は基本的に関数定義とほとんど同様です。実行時の違いは1つだけで、関数は実引数を評価したものを仮引数に束縛して評価するのに対し、マクロは実引数を評価せずに仮引数に束縛して評価し、返り値をさらに評価します。すなわち、引数として与えられるのは値ではなく値になる前の式そのもので、それを使って新しい式を作成し、その式が最初からそこに書いてあったかのように評価します。定義は `defmacro` を用いて以下のように行います。

    ```emacs-lisp
    (defmacro sample-macro-name (arg1 arg2)
      "Add ARG1 and ARG2."
      `(+ ,arg1 ,arg2))
    ```

    各引数の意味は関数のときと全く同じです。

    パッケージにおいてマクロを定義する際には一つ注意があります。それは、ローカル変数のリークです。ローカル変数を使うような式へと変形する場合、普通に書くと以下のようになります。

    ```emacs-lisp
    (defmacro sample-macro-name-8 (arg1 arg2)
      "ARG1とARG2と8をかけ算する"
      `(let ((a 8))
         (* ,arg1 ,arg2 a)))
    ```

    一見なんら問題ないように見えますが、これには問題があります。実際の展開先を見るとわかります。

    ```emacs-lisp
    ;; 展開前
    (sample-macro-name-8 1 2)

    ;; 展開後
    (let ((a 8))
      (* 1 2 a))
    ```

    これを見てわかる通り、展開先に `a` が残ってしまいます。このような単純な場合ではこの `a` のなにが問題かわからないかもしれません。次の例を見てみましょう。

    ```emacs-lisp
    ;; 展開前
    (let ((a 1))
      (sample-macro-name-8 a 2))

    ;; 展開後
    (let ((a 1))
      (let ((a 8))
        (* a 2 a)))
    ```

    これでおかしさがわかるでしょうか。展開前を見るかぎり、この式は `8` と `a=1` と `2` をかけ算して `16` になりそうです。しかし実際の展開結果を見ると、マクロの展開によって新たに出てきた `let` 文によって `a`
    の値が上書きされてしまいます。結果返り値は `168` となります。

    このような事故を防ぐための方法があります。それは、「絶対に衝突しない名前(シンボル)」を利用することです。たとえば `a` という名前の衝突しないシンボルは `(make-symbol "a")` で生成できます。「 `a` という名前なのだから衝突してるじゃん」とみなさん考えると思います。実はEmacs Lispには名前空間が1つだけあり、普通に書いたシンボル(例えば `a`)はその名前空間に登録されたものになります。 `(make-symbol "a")` で生成されたシンボルは名前こそ `a` ですが、どこの名前空間にも登録されていないシンボルになります。つまりこのシンボルは衝突のしようがありません。

    このようなシンボルは同じ `(make-symbol "a")` で複数回生成しても毎回異なるシンボルを返すので、適当な変数に束縛してからそれぞれの場所(`let` 節の変数定義や変数の使用場所)に割り振ってあげます。

    ```emacs-lisp
    (defmacro sample-macro-name-8 (arg1 arg2)
      "ARG1とARG2と8をかけ算する"
      (let ((a (make-symbol "a")))
       `(let ((,a 8))
         (* ,arg1 ,arg2 ,a))))
    ```

    外側の `(let ((a ...)))` において `a` というシンボルを利用していますが、これは展開時に残らないので問題ありません。関数内で変数を利用するのと同様です。
    `a` という変数に衝突しないシンボルを束縛し、利用する場所で `,a` とすることでそこにそのシンボルを書いたかのような効果を発揮します(なお、これは `` ` `` の影響下だからできることです)。これによって、変数を一切リークさせずにマクロ定義行うことができました。


### test/sample-test.el {#test-sample-test-dot-el}

このファイルにはテストを書き入れます。パッケージ開発を行うにあたって、テストは重要です。テストをきちんと書いておけば、既存の機能が壊れてないことを担保しながら、安全に新しい機能を追加していくことができます。

```emacs-lisp
;;; sample-test.el --- Test for sample

;; Copyright (C) 2022  yourname

;; Author: yourname <youremail@example.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test for sample

;;; Code:

(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'sample)




(provide 'sample-test)
;;; sample-test.el ends here
```

始まりと終わりの部分はメインのファイルとほぼ同じです。単なるテストファイルなので、ヘッダではバージョンなどのパッケージそのものの情報は省略され、筆者など最低限の情報だけが書かれています。

```emacs-lisp
(require 'ert)
```

`ert` はテストを定義・実行するための標準パッケージです。ドキュメントは[ここ](https://www.gnu.org/software/emacs/manual/html_node/ert/)にあります。テストの定義は `ert-deftest` を利用して以下のように書きます。

```emacs-lisp
(ert-deftest sample-test-name ()
  "docuent."
  ...)
```

基本的には関数定義と同様です。ドキュンメント `"documents."` は任意で、テストにドキュンメントを付けている人はあまり見たことがありません。本当は付けたほうがよいかもしれないですが、何をテストしているのか名前や内容から読み取れず、ドキュンメントがないと理解できないテストとなっている場合はテストが複雑すぎるかもしれません。また、テストの名前 `sample-test-name` は関数の名前空間とは独立ですが、テストを管理する単一の名前空間に存在することに留意し、関数や変数と同様にパッケージ名の接頭辞 `sample-` を付けてください。なにも思いつかなければテスト対象の関数名をそのまま利用することもできます。なお、引数 `()` は `ert-deftest` を関数っぽく見せるためだけに存在しているダミー引数で常に空リストです。

`...` の部分にテストの内容を書きます。ここには任意の式を書くことができます。テストを実際に行う式は以下の4種類があります。

`should`
: `(should 式)` のように用い、 `式` が非 `nil` な値を返せばテストは成功、そうでなければ失敗します。

`should-not`
: `(should-not 式)` のように用い、 `式` が `nil` を返せばテストは成功、そうでなければ失敗します。

`should-error`
: `(should-error 式)` のように用い、 `式` が実行時にエラーを吐けば成功、そうでなければ失敗します。

式
: そのまま式を書くと、エラーを吐かなければ成功、エラーを吐けば失敗します。

たとえば、前に出てきた関数 `sample-macro-name-8` に引数 `1` と `2` を与えると `16` を返し、
`2` と `5` を与えると `80` を返すことをテストするには以下のように書きます。

```emacs-lisp
(ert-deftest sample-macro-name-8 ()
  (should (eq (sample-macro-name-8 1 2) 16))
  (should (eq (sample-macro-name-8 2 5) 80)))
```

ただし、マクロの変数リークはこのようなテストでは検知できないので、依然として気をつける必要があります。

テストを今動かしているEmacs上で実行するためには、 `ert` コマンドを使います。
`M-x ert` とすると、どのテストを実行するか聞かれます。 `t` を渡せば現在定義されている全てのテストを実行し、テストの名前を渡せばそれを実行します。たとえば `sample-macro-name-8` とテスト自体を評価した状態で `ert` を渡し、 `sample-macro-name-8` と入力すると、以下のような `*ert*` バッファが得られるはずです。

```text
Selector: sample-macro-name-8
Passed:  1
Failed:  0
Skipped: 0
Total:   1/1

Started at:   2022-12-16 10:38:54+0900
Finished.
Finished at:  2022-12-16 10:38:54+0900

.
```

上段にはテスト結果のまとめが書いてあります。中段には実行のログが書いてあります。下段には `.` と書いてあります。特にこの下段にはテストの一覧が1テスト1文字で書かれています。
`.` は成功、 `F` は失敗を表します。今回は1つしかテストを実行していないので1文字ですが、複数のテストを同時に行えば `...` や `..F` のように複数の文字が横に連なります。


#### カバレッジ計測 {#カバレッジ計測}

テンプレートには以下の記述があります。

```emacs-lisp
(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))
```

[undercover.el](https://github.com/undercover-el/undercover.el)は、テストにおけるカバレッジ(コード全体の行のうちどれくらいの割合がテストされているか)を計測してくれるパッケージです。
`undercover` マクロでカバレッジの出力フォーマットやファイル名を指定しています。今回はCodecovというサービスにカバレッジを送信するので、 `'codecov` をフォーマットとして指定しています。
Codecovへのカバレッジレポートの送信は後述するGitHub Actionsで行うので、 `:send-report` には `nil` を指定します。その他のオプションの詳細は[undercover.elのREADME](https://github.com/undercover-el/undercover.el)をご覧ください。

`undercover` パッケージは、 `undercover` 節よりも後に `require` したものを対象にカバレッジ計測を行うこと、及びバイトコンパイル後の `.elc` ファイルではカバレッジ計測できないこと(行の情報が消失している)を留意してください。後述する `keg run test-all`  ではバイトコンパイル前後の両方でテストを行いますが、カバレッジを計測できるのはバイトコンパイル前のみです。


#### テストにおける注意点 {#テストにおける注意点}

<!--list-separator-->

-  環境を汚さない

    テストを行う際は、環境を汚さないよう配慮しましょう。これは地球の環境問題への配慮ではなく、あなたのEmacsの実行環境のことを指しています。それには「あなたの作っているパッケージ自体」も含みます。テストにおいて、あなたの作ったパッケージのグローバル変数や、他のパッケージのグローバル変数を直接変更するのは控えましょう。

    `let` を利用することで一時的にグローバル変数を別の値に束縛しておけます。これを用いると、一時的にグローバル変数をローカル変数のように扱えます。つまり、スコープを抜けると元の値に戻ること、 `let` の内側での該当変数の変更は一切グローバルに波及しないことを保証します。ただし、Emacs Lispにおけるリストは参照渡しなので、あるリストの途中を変更したときに、同じリストの参照を保持している変数の値は実質的に変更されます。

    この束縛は `let` 環境の内側のすべての関数に波及します。つまり、 `let` 内側で呼びだした関数が該当変数を変更した場合も、
    `let` で定義したローカル変数のほうしか変更できず、グローバルな値は変更されません。これはまさにダイナミックバインディング(動的束縛)の効果になります。実はEmacsでは、グローバル変数として存在する変数と全く同じ変数を `let` で束縛すると、自動で動的束縛にします。これにより、 `lexical-binding: t` とした場合でも今回のようなグローバル変数のスコープ内変更を行えるようになっています。

    他にも `with-temp-buffer` や `skip-unless` など、環境を汚さないための手法がいくつかありますが、そこまで頻度はあまり高くないので、ここでは解説しません。
    [公式のマニュアル](https://www.gnu.org/software/emacs/manual/html_node/ert/Tests-and-Their-Environment.html)をご覧ください。

<!--list-separator-->

-  マクロ定義における注意

    マクロをテストする場合、マクロはテスト定義の時点で展開されてしまうため、マクロを定義しなおしたらテストも定義しなおさねばなりません。また、場合によっては、マクロ展開を行った式の返り値のテストだけでなく、
    `macroexpand-1` などの展開結果を `equal` で比較するなど、マクロ展開そのもののテストを行うこともあります。


### Keg {#keg}

このファイルでは[keg.el](https://github.com/conao3/keg.el)というツールのプロジェクト毎の設定が書かれています。この節では `keg.el` に限らず、Emacsプロジェクトにおいて導入しておくと便利なCLIツールについて書いています。

ここでは一旦例として `keg.el` を用いますが、他も大抵の仕組みは同じだと思います。他には[cask](https://github.com/cask/cask)が有名です。

```emacs-lisp
(source gnu nongnu melpa)

(package (sample
          (recipe . (:fetcher github :repo "ROCKTAKEY/sample-emacs-lisp"))))

(dev-dependency undercover)

(script
 (test
  (let ((test-files (mapcar (lambda (file) (file-relative-name file "."))
                            (directory-files "test" t "\\.el$"))))
    (keg-shell
     `("keg" "emacs" "--batch" "-Q" "-L" "."
       ,@(mapcar (apply-partially #'concat "--load=") test-files)
       "-f" "ert-run-tests-batch-and-exit"))))
 (test-all
  (keg-shell "keg clean-elc"
             "keg run test"
             "keg build"
             "keg run test"
             "keg clean-elc")))
```

`(source gnu nongnu melpa)` ではソースとなるパッケージのアーカイブを指定しています。メインのEmacs Lispファイルのヘッダに書いた依存を、このソースから落としてきます。それぞれ[GNU ELPA](https://elpa.gnu.org/)、[NonGNU ELPA](https://elpa.nongnu.org/)、[MELPA](https://melpa.org/#/)から落とすことを指定しています。これらは有名どころのパッケージアーカイブです。

`package` 節ではレシピ等の情報を指定しています。
`keg.el` は複数パッケージに対応しており、 `(sample ...)` によってパッケージ `sample` についての設定を行うようになっています。
`...` の部分では `alist` によって情報を指定します。たとえば `(recipe . ...)` でレシピの情報を指定しています。レシピはツールによって対応しているものとしていないものがあります。レシピとは、パッケージをビルドする際の情報をリストにまとめたものです。このあたりは若干方言がありますが、基本文法は[MELPAの説明](https://github.com/melpa/melpa#recipe-format)がわかりやすく、よく用いられています。
`plist` のうち、代表的なものを以下に示します。

`:fetcher`
: ソースコードをどこから取ってくるかです。今回はGitHubでホストしているので `github` としています。他にも `gitlab` や単に `git` のようなものも指定できます。

`:repo`
: レポジトリの名前です。 `:fetcher` で `gitlab` や `github` を指定した場合にのみ用います。
    `git` の場合は かわりに `:url` でフルのURLを指定してください。

`:files`
: パッケージに含めるファイルの正規表現のリスト。通常は指定しなくてもよしなにしてくれますが、複数パッケージを含む場合は明示的にファイル名を指定します。

`dev-dependency` は、開発においてのみ必要な依存です。ここでは前述のカバレッジ計測ツールである `undercover` を指定しています。

`(script ...)` では、 `keg run` コマンドで実行するEmacs Lispの式が書けるようになっています。ツールにより似たような機能があるかどうか、書けるのがEmacs Lispかシェルスクリプトかのような違いはあると思います。ここでは `test` と `test-all` の二つのコマンドを定義しています。それぞれ `keg run test` と `keg run test-all` で実行できます。

`test`
: こちらでは、まずは `test/` 以下にあるテストファイルを全て列挙し、それを `--load` と結合して `keg emacs --batch` に渡しています。
    `--load` にファイルを渡すことで、Emacs Lispファイルがロードされます。また、 `-L .` によってパッケージ自体を読み込んでいます。 `-L` は `--load=` とほとんど同じで、後ろに渡されたファイルをロードします。同時に `-f ert-run-tests-batch-and-exit` を渡し、定義されたテストを実行しています。 `-f` は後ろに渡された関数を実行します。

    なお、 `keg emacs` は単にEmacsを実行するものですが、環境変数 `EMACSLOADPATH` を通じてEmacsに `load-path` を伝えます。この変数を渡さないと、Emacsは依存をどこにインストールしたのかを知ることができません。どのツールにもこの機能は実装されているかと思います。

    `keg-shell` については後述します。


`test-all`
: こちらでは、 `keg-shell` という1つの関数のみを呼び出しています。この関数は与えられた文字列をシェルコマンドとして順に実行していきます。文字列の代わりにリストを与えることもできます。その場合、各要素(文字列)をエスケープした上で結合し、シェルコマンドとして実行します。

    ここでは3種類、5つの文字列を与えています。 `keg clean-elc` はバイトコンパイル後のファイルの削除、
    `keg run test` は先ほど定義した `test` スクリプトの実行、 `keg build` はバイトコンパイルを行います。


#### CLIツールによるテストの実行 {#cliツールによるテストの実行}

テストの章ではテストを実行中のEmacsで実行しましたが、使用中の環境は我々の設定が読み込まれていたり、逆に開発中のパッケージを評価し忘れていたりと、不確実性がそこそこにあります。そこで、コマンドラインツールを利用してテストを実行していきましょう。今回利用しているのは `keg.el` でした。先ほど `script` で定義した `test` を使ってテストを走らせましょう。

```shell
keg run test
```

はじめには `keg install` と同等の処理が入ります。これは依存のインストールを行います。必要であれば先に実行しておくとよいです。なお、そこでいくつかの警告が出力されますが、それは依存先によるものなので、無視してよいです。もし気になるのであれば、依存先にコントリビュートしに行きましょう。

次に、 `keg run test` の処理が入ります。ここではテストを行います。以下の部分がメインです。

```text
Exec command: keg emacs --batch -Q -L . --load\=test/sample-test.el -f ert-run-tests-batch-and-exit
Exec command: emacs --batch -Q -L . --load\=test/sample-test.el -f ert-run-tests-batch-and-exit
Running 1 tests (2022-12-18 04:04:28+0900, selector ‘t’)
   passed  1/1  sample-macro-name-8 (0.000074 sec)

Ran 1 tests, 1 results as expected, 0 unexpected (2022-12-18 04:04:28+0900, 0.000188 sec)
```

`*ert*` バッファとそうかわらないので、あまり説明することはないです。テストが失敗した場合は以下のように表示されます。

```text
Exec command: keg emacs --batch -Q -L . --load\=test/sample-test.el -f ert-run-tests-batch-and-exit
Exec command: emacs --batch -Q -L . --load\=test/sample-test.el -f ert-run-tests-batch-and-exit
Running 1 tests (2022-12-18 04:08:19+0900, selector ‘t’)
Test sample-macro-name-8 backtrace:
  ert-fail(((should (eq (sample-macro-name-8 1 2) 13)) :form (eq 16 13
  (if (unwind-protect (setq value-2 (apply fn-0 args-1)) (setq form-de
  (let (form-description-4) (if (unwind-protect (setq value-2 (apply f
  (let ((value-2 'ert-form-evaluation-aborted-3)) (let (form-descripti
  (let* ((fn-0 #'eq) (args-1 (condition-case err (let ((signal-hook-fu
  (let ((lexical-binding nil)) (let* ((fn-0 #'eq) (args-1 (condition-c
  (lambda nil (let ((lexical-binding nil)) (let* ((fn-0 #'eq) (args-1
  ert--run-test-internal(#s(ert--test-execution-info :test #s(ert-test
  ert-run-test(#s(ert-test :name sample-macro-name-8 :documentation ni
  ert-run-or-rerun-test(#s(ert--stats :selector t :tests [#s(ert-test
  ert-run-tests(t #f(compiled-function (event-type &rest event-args) #
  ert-run-tests-batch(nil)
  ert-run-tests-batch-and-exit()
  command-line-1(("-L" "." "--load=test/sample-test.el" "-f" "ert-run-
  command-line()
  normal-top-level()
Test sample-macro-name-8 condition:
    (ert-test-failed
     ((should
       (eq
    (sample-macro-name-8 1 2)
    13))
      :form
      (eq 16 13)
      :value nil))
   FAILED  1/1  sample-macro-name-8 (0.000108 sec)

Ran 1 tests, 0 results as expected, 1 unexpected (2022-12-18 04:08:19+0900, 0.095499 sec)

1 unexpected results:
   FAILED  sample-macro-name-8

Script "test" exit abnormally with status code 1
```

まずは長いバックトレースが表示されます。バックトレースはEmacsがS式を評価し、エラーを起こすまでの軌跡です。バックトレースは名前の通り、下から上に向かって時間が進むことに注意してください。

その後、テストの条件がどう失敗したのかを示してあります。その部分を抜粋します。

```text
Test sample-macro-name-8 condition:
    (ert-test-failed
     ((should
       (eq
    (sample-macro-name-8 1 2)
    13))
      :form
      (eq 16 13)
      :value nil))
   FAILED  1/1  sample-macro-name-8 (0.000108 sec)
```

見方は割とそのままで、 `:form` が実行された式、 `:value` が返された値です。式の比較を `equal` で行った場合は、 `:explanation` として追加で情報が与えられます
(`eq` はプリミティブの比較及び参照の比較しか行わないが、 `equal` はリストの要素を掘って比較する)。リストの長さが違うとか、リストのどこの要素が異なる、というような情報が得られます。

なお、 `keg run test` は単にテストを実行するだけですが、 `keg run test-all` はバイトコンパイルする前とした後の両方でテストを行います。使い方はほとんど同じです。 CIなどでは特に理由がなければ `keg run test-all` を使うとよいです。


### .github/workflows/test.yml {#dot-github-workflows-test-dot-yml}

このファイルでは、GitHub Actionsによるテストなどの実行をするための流れを記述しています。
GitHub Actionsにおいて1つの環境で実行されるフローをジョブと言い、それをいくつか集めた実行単位をワークフローと呼びます。
1つのワークフローは必ず同じ操作で誘発され、同時に実行されます。

```yaml
name: test

on: [push, pull_request]
```

`name` でこのワークフローの名前を指定します。
`on` では、どのような操作がこのワークフローの実行を誘発するかを指定します。
`push` はコミットのプッシュを行ったとき、 `pull_request` プルリクエストを行ったときにこのワークフローを実行することを表します。

```yaml
jobs:
  test:
    ...
  lint:
    ...
```

`jobs` には各ジョブの内容を指定します。
1つ下にキーとして名前を書きます。ここでは `test` と `lint` の2つをジョブとして指定します。

```yaml
test:
  runs-on: ${{ matrix.os }}
  continue-on-error:
    ${{ contains(fromJson('["snapshot"]'), matrix.emacs_version) }}
  strategy:
    fail-fast: false
    matrix:
      os:
        - ubuntu-latest

      emacs_version:
        - '24.1'
        - '24.2'
        - '24.3'
        - '24.4'
        - '24.5'
        - '25.1'
        - '25.2'
        - '25.3'
        - '26.1'
        - '26.2'
        - '26.3'
        - '27.1'
        - '27.2'
        - '28.1'
        - '28.2'
        - 'snapshot'
```

先に `strategy.matrix` について説明します。
`strategy.matrix.xxx` にリストを渡すと、 その数だけジョブが生成されます。それぞれのジョブでは `matrix.xxx` にリストの各要素の値が束縛されるため、その値を利用することで様々な条件でジョブを実行することができます。また、 `matrix.xxx` と `matrix.yyy` のように複数設定した場合、全ての組み合わせのジョブが生成されます。これが `matrix` と言われている所以です。

`${{...}}` はGitHub Actions特有の文法で、関数の実行や変数からの値の取り出しに用います。


#### test {#test}

ここで `test:` の先頭まで戻ります。
`runs-on` では、そのジョブを実行するOSを指定します。Ubuntuだけでなく、macOSやWindowsも利用可能です。ここでは後からOSを追加できるよう、 `${{ matrix.os }}` として `matrix.os` に `ubuntu-latest` を指定しています。もしmacOSでもテストしたいのであれば、以下のように `macos-latest` を追加してください。ただし、Ubuntuに比べてかなり遅く、CIを終えるのにかなり時間がかかるようになります。

```yaml { hl_lines=["9"] }
jobs:
  test:
    runs-on: ${{ matrix.os }}
    # ...
    strategy:
      matrix:
        os:
          - ubuntu-latest
          - macos-latest
```

`continue-on-error` を `true` にすると、そのジョブが失敗しても成功したかのように扱ってくれます。

```yaml { hl_lines=["4-5"] }
jobs:
  test:
    # ...
    continue-on-error:
      ${{ contains(fromJson('["snapshot]'), matrix.emacs_version) }}
```

ここでは `${{ contains(fromJson('[snapshot"]'), matrix.emacs_version) }}` という複雑な条件を与えていますが、簡単に言えば `matrix.emacs_version` が `snapshot` の場合だけ失敗を許容します。現状 `snapshot` のみなので単に `${{ matrix.emacs_version == "snapshot" }}` でも構いませんが、たまに無視したいバージョンを追加したい場合があるので、手軽に追加できるようこのような書きかたになっています。

`strategy.fail-fast` に `true` を指定することで、ジョブが1つ失敗しても他のジョブを最後まで実行します。デフォルトでは `true` となっていて、1つのジョブが失敗すると残りのジョブは自動で中断されてしまいます。これにはリソースを節約するメリットがある一方、全ての環境の組み合わせでの結果の成否を得ることができないため、環境要因の切り分けがしにくくて不便です。そのため、ここで `false` に指定します。

```yaml { hl_lines=["4"] }
jobs:
  test:
    strategy:
      fail-fast: false
```

ここでは `matrix.emacs_version` の設定をしています。もしサポートするEmacsバージョンを上げる場合は、ここも適宜削ってください。

```yaml { hl_lines=["5-21"] }
jobs:
  test:
    strategy:
      # ...
      emacs_version:
        - '24.1'
        - '24.2'
        - '24.3'
        - '24.4'
        - '24.5'
        - '25.1'
        - '25.2'
        - '25.3'
        - '26.1'
        - '26.2'
        - '26.3'
        - '27.1'
        - '27.2'
        - '28.1'
        - '28.2'
        - 'snapshot'
```

`steps` がジョブのメインの処理を書く部分です。

```yaml
jobs:
  test:
    # ...
    steps:
      - uses: actions/checkout@v3
      - uses: purcell/setup-emacs@master
        with:
          version: ${{ matrix.emacs_version }}
      - uses: conao3/setup-keg@master

      - run: keg install
      - run: keg run test-all
      - uses: codecov/codecov-action@v2
```

主に利用するのは `uses` と `run` の2種類です。
`run` は単に与えたシェルコマンドを実行します。
`uses` は、他の人が定義したActionを実行します。
Actionとは、ワークフローで不特定多数の人に利用してもらうことを想定した実行単位で、引数を取ることもできます。環境変数を通じてAPIトークンなどにアクセスできる場合があるので、信用のできるものだけを利用してください。なお、 `@` の後ろにブランチやタグを指定することである特定のバージョンのActionsを利用することができます。

ここで利用しているActionは以下の通りです。

`actions/checkout`
: 当該レポジトリの当該コミットにチェックアウト(つまりテスト対象のコミットをファイルシステムに展開)する。

`purcell/setup-emacs`
: Emacsを環境にインストールする。 `version` 引数によりインストールするEmacsのバージョンを指定できる。

`conao3/setup-keg`
: `keg.el` を環境にインストールする。

`codecov/codecov-action`
: テストのカバレッジをCodecovに送信する。

`run` に指定されている `keg install` と `keg run test-all` については前述のとおりで、それぞれ依存のインストール、バイトコンパイル前後のテスト実行を担います。


#### lint {#lint}

ほとんどはtestと同じなので説明するところはあまりありません。唯一説明していないのは、 `keg lint` の部分です。

```yaml { hl_lines=["11"] }
jobs:
  lint:
    runs-on: ubuntu-latest
    continue-on-error: ${{ true }}
    steps:
      - uses: actions/checkout@v1
      - uses: purcell/setup-emacs@master
        with:
          version: '28.2'
      - uses: conao3/setup-keg@master
      - run: keg lint
```

シェルコマンド `keg lint` は、以下の3つの操作を行います。これらの操作がエラーを吐かないことは動作自体に必須ではないですが、パッケージとしてはエラーや警告を吐かないことが望ましいです。なんらかのエラーや警告が発生する場合、行儀の悪いパッケージとみなされる場合があります(例えばMELPAでは直すまで登録できません)。特に `package-lint` は依存周りの問題を検出してくれるので、従ったほうがよいです。なお、他のツールでも同様にこのような機能がついていると思われます。場合によっては追加でさらにインデント周りをやってくれたりするかもしれません。

[`package-lint`](https://github.com/purcell/package-lint)
: パッケージとしての要件を満たしているかを検証してくれます。Emacsの最低バージョンで対応していない関数を利用してしまっていないか、ヘッダのバージョンや説明文がきちんと書かれているか、接頭辞が正しく運用されているか、などを確かめてくれます。詳しくは[package-lintのGitHub](https://github.com/purcell/package-lint)及び[Emacs Lispのコーディング規約](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html)をご覧ください。もし今利用しているEmacsで利用したい場合は、 `M-x package-install RET package-lint RET` などを用いてインストールしたあと、該当バッファで `package-lint-current-buffer` コマンドを実行してください。

`checkdoc`
: ドキュメント周りの問題を指摘してくれます。引数がすべて入っているか、ピリオドの後にスペースが2つ入っているか、ドキュメントの各行が80桁以下になっているか、などを確認します。詳細は [checkdoc.elのソースコード](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/checkdoc.el)の説明文(Commentary)をご覧ください。なお、これは標準搭載のパッケージなので、インストールは必要ありません。もし今利用しているEmacsで利用したい場合は、該当バッファで `checkdoc` コマンドを実行してください。

`byte-compile`
: コンパイルしたときにエラーや警告が出ないかどうかを確認します。もし今利用しているEmacsで利用したい場合は、 `byte-compile-file` コマンドを実行して該当ファイルを指定してください。

これらは通るのが望ましいが絶対に通さないといけないものではないため、 `continue-on-error` は `true` としています。


### .dir-locals.el {#dot-dir-locals-dot-el}

このファイルは、前述したファイルローカル変数を、ディレクトリ単位で設定するためのファイルです。
`((メジャーモード (変数 . 値) ...)...)` のような構造になります。このファイルのあるディレクトリ以下にあるファイルを開いたとき、そのファイルで立ち上がったメジャーモードに応じて変数の値をファイルローカルに指定できます。

```emacs-lisp
((emacs-lisp-mode (package-lint-main-file . "sample.el")))
```

ここでは、Emacs Lispファイルを開いたときに `package-lint-main-file` を `"sample.el"` に束縛しています。

`package-lint-main-file` は文字通り、
`package-lint` においてパッケージのメインのファイルとして認識されるべきファイルを指定します。メインのファイル以外にはヘッダにバージョンや説明は必要ない、メインのファイル以外は接頭辞をファイル名と一致させる必要はない、など、
lintを行うにあたってメインのファイルとそれ以外で違いがあるため、この変数を束縛しています。

なお、初期状態では1ファイルのみの構成なので、このファイルはなくても構いません。複数ファイルにしたときに自動で効果を発揮します。


### codecov.yml {#codecov-dot-yml}

先ほど述べたカバレッジの計測ツール `undercover` で計測したカバレッジは、前述したGitHub ActionsでCodecovへと送信されます。その際、当然テストを行った回数だけカバレッジが送信されます。今回の雛形におけるGitHub ActionsではEmacsの各バージョンでテストを行うようにしているので、全てが届くまでCodecovに待ってもらう必要がある場合があります。それを設定しているのがこのファイルです。15種類のバージョンについてテストしている(snapshotバージョンのEmacsは不安定なので除く)ので、
`codecov.notify.after_n_builds` に `15` を指定しています。

```yaml
codecov:
    notify:
        after_n_builds: 15
```

なぜ `notify` なのかと思うかもしれません。実はここで指定しているのはGitHubへと送られてくる通知の遅延設定です。適切にCodecovの設定をすると、各コミットのカバレッジ計測の結果をコミットステータスとして通知してくれます。しかし、最初の1つのカバレッジだけを採用して通知してしまうと、Emacsのバージョンによる分岐を含む場合に正しいカバレッジを通知することができません。そこで、このファイルでいくつのカバレッジが送信されるのかをあらかじめCodecovに教えています。


## このテンプレートにおいて、対応Emacsバージョンを上げるのにやらねばならないこと {#このテンプレートにおいて-対応emacsバージョンを上げるのにやらねばならないこと}

ここまでで、パッケージを作る際に必要な情報はほぼ説明しました。あと少しだけ、このテンプレートを用いているときに、対応Emacsバージョンを上げる場合になにをすればよいかを纏めておきます。

1.  メインのファイルのヘッダの `Package-Requires: ((emacs "24.1"))` を上げたいバージョンに変更する。
2.  `.github/workflows/test.yml` の `jobs.test.strategy.matrix.emacs_version` に渡しているリストの中から、サポート対象外となるバージョンを取り除く。
3.  `codecov.yml` の数字を、取り除いたバージョン分だけ減らす。ここの数字は `jobs.test.strategy.matrix.emacs_version` の個数から `snapshot` の分の1を引いた数と一致する。


## 最後に {#最後に}

今回は、パッケージを作る際の雛形の解説を通じ、パッケージを作る際のノウハウを説明しました。かなり長くなってしまいましたが、だからこそ抜けや漏れがある可能性は多いにあります。もしなにか間違っている点、おかしな点、理解できなかった点、質問などがあれば、コメントやメール、[Emacs JP](https://emacs-jp.github.io/)のSlackなどでなんなりとご連絡ください。
Emacs JPには私よりも長くEmacsを使っている猛者たちがたくさんいます。
