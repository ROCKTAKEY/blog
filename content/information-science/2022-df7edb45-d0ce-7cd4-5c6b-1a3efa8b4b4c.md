+++
title = "Emacsのパッケージをつくってみよう"
author = ["ROCKTAKEY"]
lastmod = 2022-12-12T19:28:20+09:00
tags = ["Emacs", "Emacs-Lisp"]
draft = true
+++

{{< figure src="https://img.shields.io/badge/Emacs%20Advent%20Calendar%202022-12%E6%97%A5%E7%9B%AE-d60a34.svg?style=flat-square&logo=qiita" link="https://qiita.com/advent-calendar/2022/emacs" >}}


## 序文 {#序文}

Emacsは完全に素のまま使うと若干不便なところがたくさんあるので、Emacs Lispを利用してカスタマイズすることがあるかと思います。
単にEmacsの変数を触る程度であれば問題はありませんが、自分の欲しい機能を設定ファイル `init.el` に直接書いていると、
保守の観点であまりよろしくありませんし、人に使ってもらうのも大変になってしまいます。

一方、ある程度大きくなった機能をパッケージへと切り出しておけば、履歴も設定ファイルとは別に管理できますし、
人に使ってもらうのも簡単です。この記事では、Emacs Lispのパッケージを実際に作成し、
みなさんがパッケージを作る一助にしたいと思います。


## 今回利用するサービス及びソフトウェア {#今回利用するサービス及びソフトウェア}


### 外部サービス {#外部サービス}

この記事では、開発を便利に進めるためにいくつかの外部サービスを利用します。
それらの外部サービスを利用したくない場合はテンプレートをそのまま利用はできませんが、
それぞれのファイルの役割は逐一解説し、各自で個々の要素を利用できるよう努めます。

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
-   [Cookiecutter](https://github.com/cookiecutter/cookiecutter)
    -   テンプレートを展開するソフトウェア
    -   [GitHubのテンプレート機能](https://docs.github.com/ja/repositories/creating-and-managing-repositories/creating-a-template-repository)はレポジトリの内容をまるまるコピーすることしかできないため、
        テンプレートの展開時に対話的に入力を入れられるこのソフトウェアを選んだ


## Cookiecutterを利用してテンプレートを展開してみる {#cookiecutterを利用してテンプレートを展開してみる}

まずはテンプレートを展開し、それにより生成されたコードを解説していく方向でいこうと思います。
もしテンプレートを利用したくない場合、この章は飛ばして構いません。
私の作成した、今回使うテンプレートは[ここ](https://github.com/ROCKTAKEY/cookiecutter-emacs-lisp)にあります。
`emacs-lisp-package-sample` という名前のGithubレポジトリで、
`sample` というパッケージ及びプロジェクトの名前のパッケージを想定して作ってみましょう。

```shell
cookiecutter https://github.com/ROCKTAKEY/cookiecutter-emacs-lisp
```

すると、以下のようなプロンプトが表れます。

```text
project_name [project-name]:
```

ここではプロジェクト名を聞いているので、 `sample` と入力してEnterキーを押します。
この値はEmacs Lispパッケージの `feature` 名及び各シンボルの接頭辞として利用されます。

するとさらに以下のようなプロンプトが得られます。

```text
project_name [project-name]: sample
github_repository_name [sample]:
```

このように、対話的にプロジェクトを作成することができます。
ここではGitHubのレポジトリ名を聞いているので `emacs-lisp-package-sample` と入力します。
プロジェクトへのURLの生成及びディレクトリ名に用います。

```text
project_short_description [Short description of your project]: This is sample for package
```

ここではプロジェクトの概要を端的に入力します。
README及びメインのEmacs Lispファイルの説明文などに用いられます。

```text
full_name [Your Name]: Your Name
```

ここではあなたの本名を入力します。
著作権表記に用います。必ずしも本名でなければならないわけではありません。

```text
email [email@example.com]: youremail@example.com
```

ここにはemailアドレスを入力します。あなたの連絡先を示すのに用います。

```text
github_username [github]: username
```

ここにはあなたのGitHubのユーザー名を入力します。
この値もレポジトリのURLを生成するのに利用します。

```text
year [2022]:
```

ここには現在の年を入力します。著作権の始まりを表すのに利用します。
空欄にすれば自動で現在の年が入力されます。

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

とても長いプロンプトが出てきました。
ここには、このプロジェクトがどのキーワードに対応するかどうかを入力します。
`<f1> p` (`finder-by-keyword`)をEmacsで実行するとそれぞれのキーワードが何を表すかを見ることができます。
以下に内容を転記しておきます。
このキーワードはメインのEmacs Lispファイルのヘッダ及び `defgroup` に必要なのですが、
キーワードはかなり偏ったものが多いため、 `tools` を選ばざるを得ないことが多いです。
今回も `tools` を表す `33` を入力します。

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

これによってディレクトリ `emacs-lisp-package-sample` がカレントディレクトリに生成されます。
生成されたディレクトリは、以下のような構造になっているはずです。

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

これらの各ファイルについて解説していきます。


### README.org {#readme-dot-org}

耳にたこができるほどよく見る言説かもしれませんが、
READMEは、このパッケージを調べて辿りついた人が最初に見るファイルです。
端的にパッケージの概要を示しつつ、ドキュメントを付けておきましょう。
テンプレートでは以下のような内容になっています。
\#+BEGIN_SRC org -n
[![](https://img.shields.io/github/tag/username/sample.svg?style=flat-square)](https://github.com/username/sample)
[![](https://img.shields.io/github/license/username/sample.svg?style=flat-square)](LICENSE)
[![](https://img.shields.io/codecov/c/github/username/sample.svg?style=flat-square)](https://codecov.io/gh/username/sample?branch=master)
[![](https://img.shields.io/github/workflow/status/username/sample/test/master.svg?style=flat-square)](https://github.com/username/sample/actions)
