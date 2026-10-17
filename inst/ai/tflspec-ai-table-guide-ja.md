# AI チャットで表の code SPEC を作る

## 1. 何をする機能か

試験の **TOC/SHELL ワークブック**と **ADaM（XPT）**から、チャット AI が各表の
**code SPEC**（`spec/outputs/<id>.yml`）を書き、tflspec がそれを確かめて **RTF** を
作ります。すべて手元の PC の R で動き、tflspec は何も送信しません。プロンプトを
チャットに貼り、回答を貼り戻すだけです。

英語版: 同じフォルダの `tflspec-ai-table-guide.md`（`system.file("ai", package = "tflspec")`）。

## 2. どこに何を置くか

```text
mystudy/
  TOC_SHELL.xlsx        TOC とシェル（1 つのワークブック）
  adam/                 adsl.xpt, adae.xpt, adlb.xpt ...
```

インストール（初回のみ）: `remotes::install_github(c("ichirio/rtfreporter", "ichirio/tflspec"))`
と `install.packages(c("cards", "cardx", "haven"))`。

## 3. 何を実行するか

```r
library(tflspec)
setwd("mystudy")
dir.create("ai", showWarnings = FALSE)

# writes study.yml, spec/data.yml, spec/defaults.yml
tfl_ai_study_init(".", adam = "adam", study_id = "ABC-123")
adam  <- tfl_ai_adam_summary("adam", file = "ai/adam_summary.md")
shell <- tfl_ai_read_shell("TOC_SHELL.xlsx", file = "ai/toc_shell.md")
b     <- tfl_ai_batches(shell, size = 8)          # 1 回の依頼は同じ種類の表 5〜10 本

ids <- b$output_id[b$batch == "demographics-1"]
ctx <- tfl_ai_context("table", outputs = ids, shell = shell, datasets = adam,
                      study_dir = ".", language = "Japanese")
writeLines(format(tfl_ai_prompt("table", ctx)), "ai/prompt_demographics-1.md",
           useBytes = TRUE)
```

最初の依頼の前に見るもの:

| 見るもの | 理由 |
|---|---|
| `spec/data.yml` | 解析対象集団（ADSL のフラグ `SAFFL`・`FASFL` … ごとに 1 つ）。AI は id で呼ぶ |
| `shell$toc` | 見つかった帳票。TOC のシートが見つからないとき: `tfl_ai_read_shell(..., toc_sheet = "Index")` |
| `shell$toc$shell` | `NA`＝シェルのシートが見つからない: `tfl_ai_read_shell(..., outputs = ids, sheets = "DM shells")` |
| `ai/adam_summary.md` | レコードは無いが、短い値の一覧（15 個まで）が出る。社内で送れないものは消す |

## 4. AI に何を渡すか

| 渡すもの | 渡さないもの |
|---|---|
| `ai/prompt_<batch>.md` の全文を、**新しいチャット**に 1 通で（長すぎるときはファイルを添付し「添付の指示に従ってください」） | **XPT ファイル**、レコードを含むデータ全般（被験者 ID、日付、結果） |

プロンプトには、指示、この帳票たちの TOC とシェルのテキスト、ADaM サマリー、
`spec/data.yml` と `spec/defaults.yml` がもう入っています。

## 5. AI の回答をどこに置くか

回答の**全文**（`yaml` のブロック 1 つ）を `ai/answer_<batch>.md`（UTF-8）に保存します。

## 6. 確かめる・YAML にする・RTF を作る

```r
a  <- tfl_ai_parse(readLines("ai/answer_demographics-1.md", encoding = "UTF-8"), "table")
pr <- tfl_ai_check(a, ctx)                 # スキーマ・解析・変数を確かめる（何も実行しない）
pr                                         # 0 行なら問題なし

# エラーがあれば同じチャットに返し、新しい回答で上書きして、もう一度確かめる
writeLines(tfl_ai_repair_prompt(a, pr), "ai/repair_demographics-1.md", useBytes = TRUE)

tfl_ai_unpack(a, ".")                      # spec/outputs/<id>.yml, spec/_assumptions.yml

st   <- tfl_read_study_spec(".")
outs <- tfl_read_outputs(".")
ard  <- tfl_build_ard(st, dir = ".", save = FALSE)
dir.create("output", showWarnings = FALSE)
for (id in ids) {
  o    <- tfl_resolve_output(outs[[id]], st)
  plan <- tfl_content_run(o, subset(ard, output_id == id))
  rtfreporter::generate_rtfreport(tfl_report(o, content = plan, study = st),
                                  file.path("output", paste0(id, ".rtf")), overwrite = TRUE)
}
```

| 関数 | すること |
|---|---|
| `tfl_ai_parse()` | 回答を読む（帳票と AI の仮定） |
| `tfl_ai_check()` | 各項目をスキーマ・解析対象集団・ADaM サマリーと照らす |
| `tfl_ai_repair_prompt()` | エラーを次のメッセージにする（2〜3 往復がふつう） |
| `tfl_ai_unpack()` | `spec/outputs/<id>.yml` を書く。`overwrite = TRUE` で書き直し |
| `tfl_build_ard()` | ARD。エラーの帳票を探すときは 1 本ずつ: `output_id = "T-14-1-1"` |
| `tfl_content_run()`, `tfl_report()` | 表とページ → RTF |

**`spec/_assumptions.yml` を読む**: AI の推測（群の変数、フラグ、来院、分母）。
`tfl_content_code(o)` と `tfl_ard_code(st, output_id = id)` でコードを見られます。

**同じ種類の残りの表**は、見直した 1 本に倣わせる:
`tfl_ai_context("table", outputs = rest, ..., examples = "spec/outputs/T-14-3-1.yml")`。

## 見本で試す（10 分）

tflspec 同梱のファイル（4 表の TOC/SHELL ワークブック、XPT にした見本 ADaM、
保存済みの回答）で一通り動かします。最後に RTF が 4 つできれば準備完了です。

```r
library(tflspec)
d <- file.path(tempdir(), "sample-study")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
for (x in c("adsl", "adae", "adrs", "adlb")) {
  df <- getExportedValue("tflspec", x)
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
}
tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01", overwrite = TRUE)
ex    <- system.file("ai", "example", package = "tflspec")
shell <- tfl_ai_read_shell(file.path(ex, "onco_toc_shell.xlsx"))
ctx   <- tfl_ai_context("table", outputs = shell$toc$output_id, shell = shell,
                        datasets = tfl_ai_adam_summary(file.path(d, "adam")), study_dir = d)
a <- tfl_ai_parse(readLines(file.path(ex, "onco-answer.md")), "table")
tfl_ai_check(a, ctx)
tfl_ai_unpack(a, d, overwrite = TRUE)
# このあと 6 の `st <- tfl_read_study_spec(d)` から（ids <- names(outs)、RTF は d に）
```

`ai/` フォルダ（プロンプトと回答）は残してください。共有は社内だけに。
