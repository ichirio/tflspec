# AI チャットで表の code SPEC を作る

## 1. 何をする機能か

試験の **TOC/SHELL ワークブック**と **ADaM（XPT）**から、チャット AI が各表の
**code SPEC**（`spec/outputs/<id>.yml`）を書き、tflspec がそれを確かめて **RTF** を
作ります。すべて手元の PC の R で動き、tflspec は何も送信しません。依頼を
チャットに貼り、回答を保存するだけです。

英語版: 同じフォルダの `tflspec-ai-table-guide.md`（`system.file("ai", package = "tflspec")`）。

## 2. どこに何を置くか

```text
mystudy/
  TOC_SHELL.xlsx        TOC とシェル（1 つのワークブック）
  adam/                 adsl.xpt, adae.xpt, adlb.xpt ...
  ai/requests/          tflspec が書く: 1 依頼 1 ファイル（手順 3）
  ai/answers/           AI の回答をここに保存する（手順 5）
  ai/repairs/           tflspec が書く: 送り返す直しの依頼（手順 6）
```

インストール（初回のみ。新しい R には `remotes` がありません。`broom` は cardx の
信頼区間に要ります）:

```r
install.packages(c("remotes", "cards", "cardx", "haven", "broom"))
remotes::install_github(c("ichirio/rtfreporter", "ichirio/tflspec"))
```

## 3. 何を実行するか

```r
library(tflspec)
setwd("mystudy")
# study.yml, spec/data.yml, spec/defaults.yml を書く
tfl_ai_study_init(".", adam = "adam", study_id = "ABC-123")
adam  <- tfl_ai_adam_summary("adam", file = "ai/adam_summary.md")
shell <- tfl_ai_read_shell("TOC_SHELL.xlsx", file = "ai/toc_shell.md")

r <- tfl_ai_write_requests(".", shell, adam, size = 3, language = "Japanese")
r                                   # 依頼名・帳票・文字数
```

`ai/requests/<依頼名>.md`（`demographics-1.md`、`ae-1.md` …、1 つの種類ごと）と
一覧 `ai/requests/index.md` ができます。TOC の図は 1 図 1 依頼（`figure-1.md` …）で、
シェルを入れ、図の design で答えてもらいます。
TOC とシェルが別のブックのとき: `tfl_ai_prepare("mystudy", "TOC.xlsx",
shells = "SHELLS.xlsx", ...)`。あとで別のブック（Listing、PK の図のブック）を
足すとき: 同じ呼び方に `add = TRUE`。そこにある依頼と回答は残り、新しい依頼が
横に足されます。

同じことを 1 行で（`study.yml` が既にあれば消さないので、何度でも実行できます。
ワークブックの読み取りの結果も表示します）:

```r
r <- tfl_ai_prepare("mystudy", "mystudy/TOC_SHELL.xlsx", study_id = "ABC-123",
                    size = 3, language = "Japanese")
```

ワークブックの文字がうまく読み取れず、**チャットが添付の Excel を読めるときだけ**（文字しか貼れないチャットは既定の `"text"` のまま）:
`workbook = "both"`（または `"attach"`）。各依頼に、帳票のシェルがワークブックの
どこにあるか（シート・セル・TOC の行）が書かれるので、依頼と一緒に
`TOC_SHELL.xlsx` をチャットごとに添付します。`"both"` は文字も入れ、食い違えば
添付を優先させます。

最初の依頼の前に見るもの:

| 見るもの | 理由 |
|---|---|
| `spec/data.yml` | 解析対象集団（ADSL のフラグ `SAFFL`・`FASFL` … ごとに 1 つ）。AI は id で呼ぶ |
| `shell$toc` | 見つかった帳票。TOC のシートが見つからないとき: `tfl_ai_read_shell(..., toc_sheet = "Index")` |
| `shell$toc$shell` | `NA`＝シェルのシートが見つからない: `tfl_ai_read_shell(..., sheets = c("DM shells", ...))` |
| `ai/adam_summary.md` | レコードは無いが、短い値の一覧（15 個まで）が出る。社内で送れないものは消す |

**シートの帳票番号。** 依頼を書く前に、各シェルのシートがどの番号を作るかを
確かめます。`tfl_ai_prepare(..., dry_run = TRUE)$numbers` は、シートに書かれた
（またはシート名にある）番号 1 つが 1 行で、`sheet`・`number`・`title`・
`toc_id`（TOC の帳票、無ければ `NA`）・`make`（そのシートの依頼で作る帳票か）・
`source` です。参照として書かれただけの番号（「Table 14.2.1 を参照」）も規則では
作る帳票になるので、`numbers = data.frame(sheet = "t14.1.1", number = "Table 14.2.1",
make = FALSE)`（参照だけ。依頼には参照として残る）か、`numbers = list(t14.1.1 =
"Table 14.1.1")`（そのシートで作る番号。ほかの番号は参照だけ）で直します。TOC に
無い番号を `TRUE` にすると、警告を出してそのまま帳票にします。直した内容は依頼を
書くときに `ai/sheet_numbers.csv` に保存され、次に `numbers` を渡すまで以後の
呼び出しで使われます。

## 4. AI に何を渡すか

| 渡すもの | 渡さないもの |
|---|---|
| `ai/requests/<依頼名>.md` を 1 つ、全文を**新しいチャット**に 1 通で貼る | **XPT ファイル**、レコードを含むデータ全般（被験者 ID、日付、結果） |

各依頼には、指示、その帳票の TOC の行とシェル、ADaM サマリー、`spec/data.yml` と
`spec/defaults.yml` がもう入っています。

**まず、そのままの形で 1 依頼**。1 つの依頼は、指示が約 23,000 文字＋その帳票の分
（帳票 1 つで約 33,000 文字）です。

1. 依頼を 1 つ、そのまま新しいチャットに貼る。
2. **入らないとき**（長すぎると言われる、途中で切れる）は、指示を分けて依頼を
   書き直す:

   ```r
   r <- tfl_ai_write_requests(".", shell, adam, size = 3, instructions = "once",
                              overwrite = TRUE)
   ```

   依頼ごとに**新しいチャット**で、先に `ai/requests/00-instructions.md` を貼り、
   次にその依頼を貼る（約 10,000 文字）。
3. それでも長いときは、`size` を小さくするか、`max_chars = 20000`（超えた依頼を分ける）。

## 5. AI の回答をどこに置くか

回答の**全文**（`yaml` のブロック 1 つ）を `ai/answers/<同じ名前>.md` に
保存します。`ae-1.md` への回答は `ai/answers/ae-1.md`（フォルダは
`tfl_ai_write_requests()` が作ります）。

文字コードは **UTF-8**（BOM の有無、Windows の改行はどちらでもよい）。メモ帳では
「名前を付けて保存」で「文字コード: UTF-8」「ファイルの種類: すべてのファイル」
（そうしないと `.txt` が付く。`ae-1.md.txt` も読みます）。ANSI（Shift-JIS）で
保存したファイルは、そう書いた `error` になります。

## 6. 確かめる・YAML にする・RTF を作る

```r
res <- tfl_ai_collect_answers(".")   # 読む・確かめる・spec/outputs/<id>.yml を書く
res                                  # 依頼ごとに状態・帳票・次にすること
```

表示は、依頼ごとの状態（`ok`、`warning`、`error`、`no answer`）、帳票、
**次にすること**（next_step）と、帳票ごとの最初の問題です。
エラーのある回答はファイルを書かず、直しの依頼を `ai/repairs/<依頼名>.md` に
書きます。それを**同じチャット**に貼り、新しい回答で `ai/answers/<依頼名>.md` を
上書きして、もう一度 `tfl_ai_collect_answers(".")`（2〜3 往復がふつう）。

回答は、最初に作る帳票の一覧（`reports:`。id、タイトル、TOC の行の分け方、
TOC の行）を書きます。表示にその本数が出て、`ai/manifest.csv` に全部の帳票と
状態が並びます。TOC と本数を見比べてください（TOC の 1 行が複数の帳票に
なることがあります）。「listed but not answered」（一覧にあるが帳票が無い）と
「answered but not listed」（帳票はあるが一覧に無い）も見てください。

続けて、書けた帳票すべての RTF を作ります:

```r
tfl_ai_run(".")   # もう一度読み、帳票ごとに ARD と RTF: ai/output/tfl/<id>.rtf
```

`ai/status.md`（帳票ごとの状態、RTF かできなかった理由、次にすること）を書き、
回答の疑問を `ai/questions.md` にまとめます。1 本が失敗しても、ほかは作ります。

| 関数 | すること |
|---|---|
| `tfl_ai_write_requests()` | 1 依頼 1 ファイル（`size`、`max_chars`、`instructions`） |
| `tfl_ai_collect_answers()` | `ai/answers/` を読み、依頼ごとに確かめ、帳票ファイルを書く |
| `tfl_ai_run()` | 読んだ後、帳票ごとに ARD と RTF。`ai/status.md`・`ai/questions.md` |
| `tfl_ai_compare()` | 帳票ごとに 1 行: テンプレートかコード SPEC か、値の数、検査、直しの回数（`ai/compare.csv`） |
| `tfl_build_ard()` | ARD。エラーの帳票を探すときは 1 本ずつ: `output_id = "T-14-1-1"` |
| `tfl_content_run()`, `tfl_report()` | 表とページ → RTF |

**`spec/_assumptions.yml` を読む**: AI の推測（群の変数、フラグ、来院、分母）。
`tfl_content_code(o)` と `tfl_ard_code(st, output_id = id)` でコードを見られます。

## 1 試験分（200 帳票くらいまで）を回す

1. **小さく始める**: `size = 2` か `3`。種類ごと（`demographics-1`、`ae-1`、
   `efficacy-1` …）に 1 依頼ずつ回し、RTF をシェルと見比べる。
2. **種類ごとに 1 本を手で直し**、それを見本にして依頼を書き直す。見本は種類の名前を
   付けて渡すと、その種類の依頼にだけ入り、AI はそれに倣う:
   `tfl_ai_write_requests(..., examples = c(demographics = "spec/outputs/T-14-1-1.yml", ae = "spec/outputs/T-14-3-1.yml"), overwrite = TRUE)`。
3. 回答が正しいうちは **`size` を上げる**（5〜8）。
4. **依頼の回数の目安**: 種類ごとに 帳票数 ÷ `size` の切り上げ。200 帳票なら
   `size = 5` で 40〜45 回、`size = 8` で 25〜30 回。`nrow(r$requests)` で分かる。
5. `tfl_ai_collect_answers(".")` はいつ実行してもよい。そこにある回答を全部読み、
   残り（`no answer`、`error`）を一覧にする。

## コーディングエージェントで（VS Code の GitHub Copilot）

エージェントが依頼を止まらずに全部こなし、自分で決められないことは 1 つの
ファイルにまとめます。

1. 会社が試験データに使うことを認めた AI を使う。チャット（`mode = "chat"`）に
   送るのは ADaM の要約だけ。エージェントは試験フォルダで作業し、要約で決まらない
   こと（データの値の書き方、変数の有無、群の水準）を確かめるときだけ、R で ADaM を
   読む（データのファイルをエディタで開かない）。
2. `tfl_ai_prepare("mystudy", "mystudy/TOC_SHELL.xlsx", adam = "C:/data/ABC-123/adam", study_id = "ABC-123", mode = "agent")`。
   依頼のほかに、`AGENTS.md` と `.github/copilot-instructions.md`（エージェント
   への指示）、`run.R`（`tfl_ai_run()`）、`ai/questions.md`、`ai/decisions.md` が
   できます。
3. VS Code で `mystudy` を開き、エージェントのチャットに「Follow AGENTS.md: do
   every request of ai/requests/index.md.」（上のフォルダを開いたときは
   「Follow mystudy/AGENTS.md ...」）。依頼ごとに回答を保存し、`run.R` を実行し、
   `ai/status.md` を読んで直し、次へ進みます。
4. 終わったら `ai/questions.md` に答える（複数の帳票の同じ疑問は 1 つにまとまり、
   関わる帳票が並びます）。帳票の一覧の直し（`ai/expected_outputs.csv`、
   `tfl_ai_expected_write()`）があれば、`tfl_ai_corrections()` で回答と直しを
   `ai/corrections.md` にまとめます。エージェントに「the answers and corrections
   are written」と伝えると、回答を `ai/decisions.md` に移し、直しの節を 1 つずつ
   済ませて、関わる帳票を作り直します。

新しい試験では、AI の帳票ファイルは下書きとして `ai/spec/outputs/` に、RTF は
`ai/output/tfl/` にできます。確かめた帳票を `tfl_ai_adopt()` で採用すると、試験の
`spec/outputs/` に写り、帳票一覧に出ます（AI は採用した物を上書きしません）。
このフォルダは tflplanner で試験として開けます（`spec/toc.yml` に帳票の一覧）。
前の形の試験（下書きが `spec/outputs/` にある）は `tfl_ai_migrate()` で移せます。

どの依頼にも入るはずの物は、エージェント用に `ai/` に 1 回だけ書かれます:
`ai/instructions-table.md` と `ai/instructions-figure.md`（指示と schema）、
`ai/templates.md`（テンプレート）、それに `ai/adam_summary.md`。依頼はそれらの
名前を挙げるだけなので、1 本が数千字になります。

## テンプレートかコード SPEC か（`templates`）

`tfl_ai_prepare(templates = TRUE)`（既定）は、合うテンプレートがあればそれで答える
よう AI に頼みます（`template: {name, args}`、既定と違う引数だけ）。合わなければ
コード SPEC で答えます。テンプレートの一覧は、依頼を書くときの登録簿から作ります
（tflspec の物、会社の物、試験の変種）。チャットでは依頼ごとに一覧が入り、
エージェントでは `ai/templates.md` に 1 回だけ書かれて、各依頼にはその帳票に合い
そうなテンプレートの名前が入ります。`max_chars` を超えるチャットの依頼では、引数を
名前だけにした短い一覧になります。`templates = FALSE` はコード SPEC だけを頼みます
（チャットの依頼はテンプレート以前と同じ）。

2 つを比べるには、同じ TOC を 2 つのフォルダに設定を変えて用意し、それぞれ答えさせ、
それぞれで `tfl_ai_compare(".")` を実行します。帳票ごとに 1 行（何で答えたか
`template:<名前>` か `code`、帳票ファイルの値の数、検査の結果、直しの回数）が
`ai/compare.csv` に書かれます。

## 見本で試す（10 分）

tflspec 同梱のファイル（4 表と KM 図 1 つの TOC/SHELL ワークブック、XPT にした
見本 ADaM、5 つの依頼それぞれへの保存済みの回答。`figure-1` が図の依頼）で
一通り動かします。最後に 5 帳票すべてが `ok` なら準備完了です。

```r
library(tflspec)
d <- file.path(tempdir(), "sample-study")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
  df <- getExportedValue("tflspec", x)
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
}
tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01", overwrite = TRUE)
ex    <- system.file("ai", "example", package = "tflspec")
shell <- tfl_ai_read_shell(file.path(ex, "onco_toc_shell.xlsx"))
r     <- tfl_ai_write_requests(d, shell, tfl_ai_adam_summary(file.path(d, "adam")),
                               overwrite = TRUE)
# 保存済みの回答を、チャットの回答の代わりに置く（1 依頼に 1 つ）
invisible(file.copy(list.files(file.path(ex, "answers"), full.names = TRUE),
                    file.path(d, "ai", "answers"), overwrite = TRUE))
tfl_ai_collect_answers(d)            # 5 reports in 4 requests (a shell sheet each, the figure its own): ok 5 ...
```

`ai/` フォルダ（依頼と回答）は残してください。共有は社内だけに。

## うまくいかない時

| 見えたもの | すること |
|---|---|
| `shell$toc` が空: TOC のシートが見つからない | `tfl_ai_read_shell("TOC_SHELL.xlsx", toc_sheet = "<TOC のシート名>")` |
| `shell$toc$shell` が `NA`: 帳票番号でシェルが見つからない | そのシートを足す: `tfl_ai_read_shell("TOC_SHELL.xlsx", sheets = c("<シェルのシート>", ...))` |
| チャットがメッセージが長すぎると言う | `tfl_ai_write_requests(..., instructions = "once")`、`size` を小さく、または `max_chars = 20000` |
| `tfl_ai_collect_answers()` で `error`（"no answer block found"） | 回答を全部コピーしていない: 最初の ```` ``` ```` から最後までコピーし直す |
| `error`（"is not UTF-8"） | 回答を UTF-8 で保存し直す（メモ帳: 名前を付けて保存 → 文字コード: UTF-8） |
| "Not read (no request of that name)" | 回答のファイル名を依頼の名前にする（`ai/requests/index.md`）: `ae-1.md` |
| 文字にしたシェルがおかしい（セルが抜ける、順が違う） | チャットが添付の Excel を読めるときだけ、Excel を添付する: `tfl_ai_prepare(..., workbook = "both")` か `tfl_ai_write_requests(..., workbook = "both")` にして、`TOC_SHELL.xlsx` をチャットごとに添付 |
| `print(shell)` の最後が "Next: no TOC found" か "no shell found" | 書かれているとおりに渡す: `toc_sheet = "<シート名>"` か `sheets = c(...)` |
| 2〜3 回直しても同じエラー | `spec/outputs/<id>.yml` を手で直し、手順 6 を実行 |
| `tfl_build_ard()` が止まる: `The package "broom" (>= 1.0.8) is required.` | `install.packages("broom")`（ほかのパッケージの名前が出たときも、それを入れる） |
| `tfl_build_ard()` が止まる | 帳票を絞って探す: `tfl_build_ard(st, dir = ".", output_id = "T-14-1-1", save = FALSE)` |
