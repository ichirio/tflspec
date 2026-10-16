# AI チャットで表の code SPEC を作る -- テスター向けガイド

このガイドは、試験の **TOC/SHELL ワークブック**と **ADaM（XPT）** から、
**レポートファイル**（`spec/outputs/<id>.yml`、code SPEC）を作り、そこから
**RTF の表**を出すまでの手順です。チャット AI は社内で認められたものを使います。
すべて手元の PC の R で動き、tflspec 自体は何も送信しません。テキストを
チャットに貼り付け、回答を貼り戻すだけです。

英語版: 同じフォルダの `tflspec-ai-table-guide.md`
（`system.file("ai", package = "tflspec")`）。

## AI に渡すもの・渡さないもの

| チャットに入れるもの | 絶対に渡さないもの |
|---|---|
| `tfl_ai_prompt()` が作るプロンプト: 指示とスキーマ、**TOC とシェルのテキスト**（チャットが Excel を読めて、そちらがよければワークブックそのもの）、**ADaM サマリー**（データセット名・変数名・ラベル・型と、SEX や PARAMCD のような短いコードリストの値）、試験の `spec/data.yml` と `spec/defaults.yml` | **XPT ファイル**やレコードを含むデータ全般、被験者 ID、日付、結果 |

最初の依頼の前に `ai/adam_summary.md` を一度読んでください。レコードは
含みませんが、文字変数については短い値の一覧（15 個まで、各 40 文字未満）が
出ます。社内ルールで送れないものがあれば削除してください。

## 0. インストール（初回のみ）

```r
# GitHub から（または社内ミラーから）
remotes::install_github("ichirio/rtfreporter")
remotes::install_github("ichirio/tflspec")
install.packages(c("cards", "cardx", "haven"))   # ARD と XPT の読み込み
```

## 1. まずサンプルで試す（10 分）

tflspec に同梱のファイルで全工程を通します。架空の TOC/SHELL ワークブック
（人口統計、最良総合効果と ORR、SOC/PT 別有害事象、臨床検査のシフト表の 4 表）、
XPT に書き出したサンプル ADaM、保存済みの AI の回答を使います。最後に RTF が
4 つできれば準備完了です。

```r
library(tflspec)
d <- file.path(tempdir(), "sample-study")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
for (x in c("adsl", "adae", "adrs", "adlb")) {
  df <- getExportedValue("tflspec", x)
  df[] <- lapply(df, function(v) if (is.factor(v)) as.character(v) else v)
  haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
}
xlsx <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")

tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01", overwrite = TRUE)
adam  <- tfl_ai_adam_summary(file.path(d, "adam"))
shell <- tfl_ai_read_shell(xlsx)
ctx   <- tfl_ai_context("table", outputs = shell$toc$output_id, shell = shell,
                        datasets = adam, study_dir = d)
p     <- tfl_ai_prompt("table", ctx)          # 本番ではこれを貼り付ける
answer <- readLines(system.file("ai", "example", "onco-answer.md", package = "tflspec"))
a <- tfl_ai_parse(answer, "table")
tfl_ai_check(a, ctx)                          # 0 行なら問題なし
tfl_ai_unpack(a, d, overwrite = TRUE)

st   <- tfl_read_study_spec(d)
outs <- tfl_read_outputs(d)
ard  <- tfl_build_ard(st, dir = d, save = FALSE)
for (id in names(outs)) {
  o    <- tfl_resolve_output(outs[[id]], st)
  plan <- tfl_content_run(o, ard[ard$output_id == id, ])
  rtfreporter::generate_rtfreport(tfl_report(o, content = plan, study = st),
                                  file.path(d, paste0(id, ".rtf")), overwrite = TRUE)
}
list.files(d, pattern = "rtf$")
```

## 2. 試験フォルダを作る

```text
mystudy/
  TOC_SHELL.xlsx        社内様式の TOC とシェル（1 つのワークブック）
  adam/                 ADaM の XPT: adsl.xpt, adae.xpt, adlb.xpt ...
```

```r
library(tflspec)
setwd("mystudy")
dir.create("ai", showWarnings = FALSE)        # プロンプトと回答の置き場
tfl_ai_study_init(".", adam = "adam", study_id = "ABC-123")
```

`tfl_ai_study_init()` は `study.yml`、`spec/data.yml`（XPT ごとのデータセット、
ADSL の解析対象集団フラグ -- SAFFL、FASFL、ITTFL など -- ごとの解析対象集団
`<flag> == "Y"`）、`spec/defaults.yml`（ページのヘッダー・フッター、件数セルの
書式）を書きます。**`spec/data.yml` を開いて解析対象集団（id とラベル）を
確認してください。** AI はこの id で集団を指定します。

## 3. ADaM を要約し、ワークブックを読む

```r
adam  <- tfl_ai_adam_summary("adam", file = "ai/adam_summary.md")
shell <- tfl_ai_read_shell("TOC_SHELL.xlsx", file = "ai/toc_shell.md")
shell                       # シート、TOC で見つかった帳票、そのシェル
shell$toc[c("output_id", "title", "shell")]
```

見つかったものを確認します。

* **TOC の帳票**（`shell$toc`）。TOC シートが見つからない場合はシート名を
  指定します: `tfl_ai_read_shell("TOC_SHELL.xlsx", toc_sheet = "目次")`。
* **各帳票のシェル**（`shell` 列）。`NA` はその番号のシートが見つからなかった
  ということです。手順 5 でシートを指定します（`sheets =`）。
* `ai/toc_shell.md` が AI の読むテキストです。1 行が 1 行（`<行>: <列>=<値> | ...`）、
  結合セルは 1 回だけ範囲付き（`A1:E1=`）、インデントは空白で表します。

## 4. 表をバッチに分ける

プロンプトは **1 回の依頼で同じ系統の表 5〜10 個**を想定しています。

```r
b <- tfl_ai_batches(shell, size = 8)
b                           # output_id, title, family, batch
table(b$batch)
```

系統: disposition（内訳）、demographics（背景）、exposure（曝露）、ae（有害事象）、
labs（臨床検査）、vitals（バイタル）、efficacy（有効性）、pk、other。表題の語
（英語・日本語）で分けます。社内独自の系統:
`tfl_ai_batches(shell, families = c(pro = "(?i)EQ-5D|QLQ"))`。

## 5. 1 バッチ分のプロンプトを作る

```r
ids <- b$output_id[b$batch == "demographics-1"]
ctx <- tfl_ai_context("table", outputs = ids, shell = shell, datasets = adam,
                      study_dir = ".", language = "Japanese")
p <- tfl_ai_prompt("table", ctx)
p$nchar                                   # 文字数
writeLines(format(p), "ai/prompt_demographics-1.md", useBytes = TRUE)
```

* プロンプトには、これらの帳票の TOC 行とシェルだけが入ります。シェルが
  見つからなかった帳票は、シートを足して読み直し、それを `shell` に渡します:
  `tfl_ai_read_shell("TOC_SHELL.xlsx", outputs = ids, sheets = "DMシェル")`。
* `language = "Japanese"`: AI は前提（assumptions）を日本語で書きます
  （プロンプト本体は英語です）。
* **レビュー済みの見本を使う。** 系統の最初の 1 表が正しくなったら（必要なら
  手で直して）、残りをそれに倣わせます:
  `tfl_ai_context("table", outputs = rest, ..., examples = "spec/outputs/T-14-3-1.yml")`。

## 6. チャットに貼り付け、回答を保存する

1. `ai/prompt_demographics-1.md` を開き、全文をコピーして**新しいチャット**に
   1 つのメッセージとして貼り付けます。入力欄に収まらない場合は、ファイルを
   添付して「添付の指示に従ってください」と書くか、`---` の行より前を
   チャットのシステム指示（カスタム指示）に入れ、残りを送ります。
2. AI は `yaml` のブロック 1 つで答えます。**回答全体**をテキストファイル
   `ai/answer_demographics-1.md`（UTF-8）に保存します。

## 7. 回答をチェックする

```r
a  <- tfl_ai_parse(readLines("ai/answer_demographics-1.md", encoding = "UTF-8"), "table")
a                                         # 読めた帳票、前提
pr <- tfl_ai_check(a, ctx)
pr                                        # where, problem, severity
```

`tfl_ai_check()` は各レポートファイルを tflspec と同じように読み、content の
各項目をスキーマと照合し（存在しない関数・引数、型の誤り、必須引数の欠落）、
各解析の項目とメソッド、解析対象集団、データセットと変数を ADaM サマリーと
照合し、ARD プログラムと各表のコードを書いてみます（実行はしません）。

**エラーがあれば**、同じチャットに戻します。

```r
writeLines(tfl_ai_repair_prompt(a, pr), "ai/repair_demographics-1.md", useBytes = TRUE)
```

このファイルを次のメッセージとして貼り付け、新しい回答を
`ai/answer_demographics-1.md` に上書き保存して、もう一度チェックします。
2〜3 往復が普通です。警告（warning）は確認の価値がありますが、先へ進めます。
同じエラーが繰り返される場合は、手順 8 の後で YAML を手で直してください。

## 8. レポートファイルを書き出す

```r
tfl_ai_unpack(a, ".")                     # spec/outputs/<id>.yml, spec/_assumptions.yml
```

バッチごとにファイルが追加され、前提は `spec/_assumptions.yml` に加わります
（各項目: `output_id`、`where`、`item`、`assumption`、`question`）。
**前提を読んでください。** AI の推測（群の変数、フラグ、来院、分母など）です。
同じバッチを書き直すときは `overwrite = TRUE`。

## 9. 表を実行する

```r
st   <- tfl_read_study_spec(".")
outs <- tfl_read_outputs(".")
ard  <- tfl_build_ard(st, dir = ".", save = FALSE)
dir.create("output", showWarnings = FALSE)
for (id in ids) {
  o    <- tfl_resolve_output(outs[[id]], st)
  plan <- tfl_content_run(o, ard[ard$output_id == id, ])
  rtfreporter::generate_rtfreport(tfl_report(o, content = plan, study = st),
                                  file.path("output", paste0(id, ".rtf")), overwrite = TRUE)
}
```

* `tfl_build_ard()` でエラーが出たら、1 帳票ずつ実行して特定します:
  `tfl_build_ard(st, dir = ".", output_id = "T-14-1-1", save = FALSE)`。
  エラーメッセージをチャットに貼る（「T-14-1-1 でこのエラーが出ました。
  ファイルを直してください」）か、ファイルを手で直します。
* `cat(tfl_content_code(o), sep = "\n")` で表のコード、
  `tfl_ard_code(st, output_id = id)` で ARD プログラムを見られます。

## 10. フィードバックに書いてほしいこと

バッチごとに: 系統と表の数、チェックの往復回数、繰り返し出たエラー、手で
直したところ、かかった時間、RTF がシェルどおりだったか。`ai/` フォルダ
（プロンプト、回答、修正プロンプト）は残してください。ただし共有は社内だけに
してください。
