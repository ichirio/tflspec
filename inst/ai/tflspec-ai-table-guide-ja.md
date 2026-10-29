# コーディングエージェントで表の code SPEC を下書きする

## 1. 何をする機能か

試験の **TOC/SHELL ワークブック**と **ADaM（XPT）**から、コーディングエージェント
（VS Code の GitHub Copilot のエージェントなど）が各表の **code SPEC** の下書き
（`ai/spec/outputs/<id>.yml`）を書きます。エージェントは、帳票を決めるのに
まだ決まっていないことを**チャットで質問**し、あなたの答えを試験の決定として
残し、各帳票の骨組み（見た目は含まない）を書きます。tflspec は下書きを確かめて
**RTF** を作り、仕上げは tflplanner のビルダーで行って採用します。すべて手元の
PC の R で動き、tflspec は何も送信しません。

日本語版: 同じフォルダの `tflspec-ai-table-guide-ja.md`（`system.file("ai", package = "tflspec")`）。

## 2. どこに何を置くか

```text
mystudy/
  TOC_SHELL.xlsx        TOC とシェル（1 つのワークブック）
  adam/                 adsl.xpt, adae.xpt, adlb.xpt ...
  AGENTS.md             tflspec が書く: 「ai/AGENTS.md を読む」（手順 3）
  ai/AGENTS.md          tflspec が書く: エージェントへの指示（手順 3）
  ai/requests/          tflspec が書く: 1 依頼 1 ファイル（手順 3）
  ai/decisions.md       チャットでのあなたの答え、1 決定 1 行（手順 5）
  ai/spec/outputs/      エージェントの下書き、1 帳票 1 ファイル（手順 5）
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
r <- tfl_ai_prepare("mystudy", "mystudy/TOC_SHELL.xlsx", adam = "adam",
                    study_id = "ABC-123", language = "Japanese")
r                                   # 依頼名・帳票・文字数・次にすること
```

この 1 回の呼び出しで次のことを行います。試験のフォルダがなければ作る
（`study.yml`、`spec/data.yml`、`spec/defaults.yml`。既にある `study.yml` は残すので、
何度でも実行できます）、ADaM の要約（`ai/adam_summary.md`: 変数、ラベル、短い値の
一覧。レコードは無し）、ワークブックの読み取り（`ai/toc_shell.md`）、依頼
（`ai/requests/<依頼名>.md`。既定では 1 帳票 1 依頼で `T-14-1-1.md` …、
`size = 3` なら 1 つの種類の帳票を 3 つずつまとめて `demographics-1.md`、
`ae-1.md` …。一覧は `ai/requests/index.md`）、どの依頼にも繰り返し入るはずの物を
1 回だけ書いたもの（`ai/instructions-table.md`、`ai/instructions-figure.md`、
`ai/templates.md`）、そしてエージェント用のファイル:
`ai/AGENTS.md`（エージェントへの指示: 何を読むか、どう質問するか、下書きに何を入れるか）、
`AGENTS.md` と `.github/copilot-instructions.md`（短い文: `ai/AGENTS.md` を読む）、
`run.R`、`ai/decisions.md`、`ai/agent_prompt.md`（エージェントへの 1 つのメッセージ）。TOC の図は 1 図 1 依頼（`figure-1.md` …）で、そのシェルと、
下書きする図の帳票が入ります。1 つずつ実行する場合:
`tfl_ai_study_init()`、`tfl_ai_adam_summary()`、`tfl_ai_read_shell()`、
`tfl_ai_write_requests()`。

TOC とシェルが別のブックのとき: `tfl_ai_prepare("mystudy",
"TOC.xlsx", shells = "SHELLS.xlsx", ...)`。あとで別のブック（Listing、PK の図の
ブック）を足すとき: 同じ呼び方に `add = TRUE`。そこにある依頼と下書きは残り、
新しい依頼が横に足されます。

最初の依頼の前に見るもの:

| 見るもの | 理由 |
|---|---|
| `spec/data.yml` | 解析対象集団（ADSL のフラグ `SAFFL`・`FASFL` … ごとに 1 つ）。エージェントは id で呼ぶ |
| `r$toc` | 見つかった帳票。TOC のシートが見つからないとき: `tfl_ai_prepare(..., toc_sheet = "Index")` |
| `r$toc$shell` | `NA`＝シェルのシートが見つからない: `tfl_ai_prepare(..., sheets = c("DM shells", ...))` |
| `ai/adam_summary.md` | レコードは無いが、短い値の一覧（15 個まで）が出る。社内で認められていないものは消す |
| `ai/AGENTS.md` | エージェントが読んでよいものと必ず質問すること。`study.yml` の `ai:` キーで規則を変えられる（後述） |

**シートの帳票番号。** 依頼を書く前に、各シェルのシートがどの番号を作るかを
確かめます。`tfl_ai_prepare(..., dry_run = TRUE)$numbers` は、シートに書かれた
（またはシート名にある）番号 1 つが 1 行で、`sheet`・`number`・`title`・
`toc_id`（TOC の項目、無ければ `NA`）・`make`（そのシートの依頼で作る帳票か）・
`source` です。参照として書かれただけの番号（「Table 14.2.1 を参照」）も規則では
作る帳票になるので、`numbers = data.frame(sheet = "t14.1.1", number = "Table 14.2.1",
make = FALSE)`（参照だけ。依頼には参照として残る）か、`numbers = list(t14.1.1 =
"Table 14.1.1")`（そのシートで作る番号。ほかの番号は参照だけ）で直します。TOC に
無い番号を `TRUE` にすると、警告を出してそのまま帳票にします。どのシートにも番号が
書かれていない TOC の項目は、その番号（`Table 15.2.4.2.b` のような枝番付きも）を
書いたシートが 1 つだけならそのシートの帳票にし（`source` が `"guess"`）、2 つ以上
なら各シートの候補として示します。直した内容は依頼を書くときに
`ai/sheet_numbers.csv` に保存され、次に `numbers` を渡すまで以後の呼び出しで
使われます。

**エージェントの規則**は `study.yml` の `ai:` キーです（試験標準の
`spec/defaults.yml` に先に書いてもよく、`study.yml` が優先されます）:

```yaml
ai:
  language: ja          # チャットの言語（既定は "en"）
  ask_when: request     # 依頼の帳票の質問をまとめて聞く。"report": 1 帳票ずつ
  carry: [all, dataset, section]   # どの決定を聞かずに後の帳票へ適用するか
  max_rounds: 3         # 同じ項目を聞き直す最大回数。超えたら要確認の印
  footnotes_at: footer  # シェルの脚注の置き場所: "footer" または "table"
```

## 4. エージェントが読むもの

| 読むもの | 読まないもの |
|---|---|
| 渡された依頼と、そこに名前のあるファイル: `ai/instructions-*.md`、`ai/templates.md`、`ai/adam_summary.md`、`spec/data.yml`、`spec/defaults.yml`、`ai/decisions.md` | **XPT ファイル**、レコードを含むデータ全般（被験者 ID、日付、結果）。ほかのフォルダ、ほかのファイル |

会社が試験データに使うことを認めた AI を使ってください。エージェントに渡るのは
ADaM の要約で、データではありません。`AGENTS.md` に、開いてよいものが書かれます。
`tfl_ai_prepare(..., agent_reads = c("workbook", "adam"))` にすると、ワークブック
（シェルの文字が切れて見えるとき、依頼が示す場所を読む）と ADaM（変数の値を確かめる。
データのファイルをエディタで開くことはしない）も、PC にある道具で読めます。
`docs = ` で文書（SAP、データガイド）を渡すと、`ai/docs/` に写されます。
エージェントは何も入れず、何も実行しません。

各依頼は数千文字です。エージェントの手順、その帳票に当てはまる決定、帳票の TOC の行と
シェル、読むファイルの名前が入ります。`max_chars` を超える依頼は分けられます。

## 5. エージェントの質問・あなたの決定・下書き

1. VS Code で `mystudy` を開き、エージェントのチャットに `ai/agent_prompt.md` の
   文を 1 回だけ渡します。エージェントは `ai/requests/index.md` の依頼を順に、
   1 つずつすべて行います。
2. エージェントが、各帳票を決めるのにまだ決まっていないこと（解析対象集団、
   パラメータ、治療の変数、パーセントの分母、何を数えるか、欠測値、サブグループの
   条件）を、番号付きで**質問**します。質問には帳票の id とタイトル、選択肢と既定が
   書かれます。**チャットで答えて**ください。「default」と答えると既定を採ります。
   答えで決まらないときは同じ項目をもう一度聞き（最大 3 回）、それでも決まらなければ
   最も可能性の高い選択肢を採って*要確認*の印を付けます。
3. 答えは `ai/decisions.md` に 1 決定 1 行で記録され
   （`- D1 [all] treatment_var.SAF: TRT01A (chat, T-14-3-1)`）、以後の帳票に適用されます。
   決定が成り立つと確信できるとき（決定の範囲が `all`、またはその帳票のデータセット・
   TOC の節で、`carry` の指定どおり）は聞かずに、そうでないときは「ここも同じか」と
   聞きます。始める前に自分で行を書いておくこともできます（範囲のない行は `all`）。
4. エージェントは各帳票の**骨組み**を `ai/spec/outputs/<id>.yml` に書きます:
   `id`、`kind`、`label`、`population`、`data`、`ard`（解析データ、解析）、内容
   （合うテンプレートがあればテンプレート、なければ `table_plan` と `plan_cells`）、
   シェルに桁数があればその桁数、`page: titles` とシェルの脚注（`footnotes_at`）。
   `ai:` の下には、`assumptions`（それぞれ `from: D<n>`）と `confirm`（要確認）が
   入ります。列幅、見出し行、ページ、罫線、フォントは書きません。それらは試験の
   既定とビルダーが担当します。そのあと次の依頼に進みます。
5. すべての依頼が終わると止まり、書いたファイル、足した決定、要確認のことを知らせます。
   それを確かめてください（手順 6）。依頼の合間に止めて、あとで **「continue」** と
   伝えることもできます。エージェントは、下書きのない（または確認で受け付けなかった）
   最初の依頼から続け、あなたの決定は残ります。

帳票ではなく作業についての質問（ファイルが無い、依頼が不明確）も、エージェントは
チャットで聞きます。

## 6. 下書きを確かめる・YAML にする・RTF を作る

```r
res <- tfl_ai_collect_answers(".")   # 下書きを読む・確かめる・書き直す
res                                  # 依頼ごとに状態・帳票・次にすること
```

tflplanner の「検査」も同じことをします。各依頼の下書きを、その依頼への回答として
読み、依頼と照らして確かめ（schema、解析、すべてのデータセットと変数を ADaM の
要約と、試験のファイル）、書き直します。列幅が補われ、仮定に番号が付き
（`spec/_assumptions.yml`）、`ai:` ブロックは残ります。表示は、依頼ごとの状態
（`ok`、`warning`、`error`、`no answer`）、帳票、**次にすること**と、帳票ごとの
最初の問題です。エラーのある下書きはそのままにし、直しの依頼を
`ai/repairs/<依頼名>.md` に書きます。それを**エージェントのチャット**に貼ると
下書きを直すので、もう一度 `tfl_ai_collect_answers(".")`（2〜3 往復がふつう）。
YAML が読めない下書きは理由を添えて受け付けません。エージェントに伝えれば、
形を直します。
仮定が誤っているとき（`spec/_assumptions.yml`、`tfl_ai_assumptions()`）: 正しい内容を
エージェントにチャットで伝えるか、ビルダーで下書きを直します。

`tfl_ai_confirm(".")` は、エージェントが要確認とした項目（帳票、項目、理由）を
一覧にします。ビルダーで決めてください。要確認があっても下書きの採用は止まりません。
決まるまで、`tfl_check_spec()` が `check` の行として示します。

表示には帳票の本数が出て、`ai/manifest.csv` に全部の帳票と状態が並びます。TOC と
本数を見比べてください（TOC の 1 行が複数の帳票になることがあります）。

続けて、書けた帳票すべての RTF を作ります:

```r
tfl_ai_run(".")   # もう一度確かめ、帳票ごとに ARD と RTF: ai/output/tfl/<id>.rtf
```

`ai/status.md`（帳票ごとの状態、RTF かできなかった理由、次にすること）を書きます。
1 本が失敗しても、ほかは作ります。コマンドラインから `Rscript run.R` でも同じです。

| 関数 | すること |
|---|---|
| `tfl_ai_prepare()` | 試験のフォルダ、ADaM の要約、ワークブックの読み取り、依頼、エージェント用のファイル |
| `tfl_ai_write_requests()` | 1 依頼 1 ファイル（`size`、`max_chars`） |
| `tfl_ai_collect_answers()` | `ai/spec/outputs/` を読み、依頼ごとに下書きを確かめ、書き直す |
| `tfl_ai_confirm()` | エージェントが要確認とした項目 |
| `tfl_ai_run()` | 確かめた後、帳票ごとに ARD と RTF。`ai/status.md` |
| `tfl_ai_adopt()` | 確かめた下書きを試験の `spec/outputs/` へ |
| `tfl_ai_compare()` | 帳票ごとに 1 行: テンプレートかコード SPEC か、値の数、検査、直し（`ai/compare.csv`） |
| `tfl_build_ard()` | ARD。エラーの帳票を探すときは 1 本ずつ: `output_id = "T-14-1-1"` |
| `tfl_content_run()`, `tfl_report()` | 表とページ → RTF |

**`spec/_assumptions.yml` を読む**: エージェントが決定（`from: D<n>`）から、または
3 回聞いても決まらず自分で採ったもの。`tfl_content_code(o)` と
`tfl_ard_code(st, output_id = id)` でコードを見られます。

確かめた下書きは `tfl_ai_adopt()` で採用します。試験の `spec/outputs/` に写され、
帳票一覧に出ます（エージェントは採用した物を上書きしません）。このフォルダは
tflplanner で試験として開けます（`spec/toc.yml` に帳票の一覧）。前の形の試験
（下書きが `spec/outputs/` にある）は `tfl_ai_migrate()` で移せます。

## 1 試験分（200 表くらいまで）を回す

1. **小さく始める**: 種類ごとに 1 依頼（`T-14-1-1`、`T-14-3-1` …）を頼み、RTF を
   シェルと見比べる。
2. **種類ごとに 1 本を手で直し**、それを見本にして依頼を書き直す。見本は種類の名前を
   付けて渡すと、その種類の依頼にだけ入り、エージェントはそれに倣う:
   `tfl_ai_prepare(..., examples = c(demographics = "spec/outputs/T-14-1-1.yml", ae = "spec/outputs/T-14-3-1.yml"), overwrite = TRUE)`。
3. **進めながら確かめる**: `tfl_ai_collect_answers(".")` は、それまでに書かれた下書きを
   読みます。`size = 3`〜`5` で 1 つの種類の帳票を 1 依頼にまとめ、質問もまとめて聞きます。
4. **決定は引き継がれる**: 範囲が `all` やデータセットの決定は以後のすべての帳票に
   当てはまり、エージェントが聞くのは帳票ごとではなく種類ごとに 1 回です。
5. `tfl_ai_collect_answers(".")` はいつ実行してもよい。そこにある下書きを全部読み、
   残り（`no answer`、`error`、要確認）を一覧にする。

## テンプレートかコード SPEC か（`templates`）

既定（`templates = TRUE`）では、各依頼がエージェントに、合う試験のテンプレートが
あればそれで帳票を下書きするよう頼みます（`template: {name,
args}`、既定と違う引数だけ。テンプレートにない部分は帳票自身の内容の項目で）。合わなければ
コード SPEC で答えます。`ai/templates.md` には、依頼を書くときに登録されていた
テンプレート（tflspec の物、会社の物、試験の変種）が引数つきで並び、各依頼には
その帳票に合いそうなテンプレートの名前が入ります。`templates = FALSE` はコード
SPEC だけを頼みます。この設定は各依頼の 1 行目（`<!-- templates: TRUE -->`）と
`ai/status.md` に書かれます。どちらを使ったかは `tfl_ai_compare()` が帳票ごとに
1 行で示します。

## 見本で試す（10 分）

tflspec 同梱のファイル（4 表と KM 図 1 つの TOC/SHELL ワークブック、XPT にした
見本 ADaM、5 帳票それぞれの保存済みの下書き。エージェントが書いた物）で
一通り動かします。最後の行が 5 帳票すべて `ok` と表示すれば準備完了です。

```r
library(tflspec)
d <- file.path(tempdir(), "sample-study")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
  df <- getExportedValue("tflspec", x)
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
}
ex <- system.file("ai", "example", package = "tflspec")
r  <- tfl_ai_prepare(d, file.path(ex, "onco_toc_shell.xlsx"), adam = "adam",
                     study_id = "XYZ-ONC-01", size = 3, overwrite = TRUE)
# the stored drafts stand in for the agent's: one a report
invisible(file.copy(list.files(file.path(ex, "drafts"), full.names = TRUE),
                    file.path(d, "ai", "spec", "outputs"), overwrite = TRUE))
tfl_ai_collect_answers(d)            # 5 reports in 5 requests (the figure its own): ok 5 ...
```

`ai/` フォルダ（依頼、決定、下書き）は残してください。共有は社内だけに。

## うまくいかない時

| 見えたもの | すること |
|---|---|
| `r$toc` が空: TOC のシートが見つからない | `tfl_ai_prepare(..., toc_sheet = "<TOC のシート名>")` |
| `r$toc$shell` が `NA`: 帳票番号でシェルが見つからない | そのシートを足す: `tfl_ai_prepare(..., sheets = c("<シェルのシート>", ...))` |
| エージェントが質問せずに下書きを書く、または決定で決まることを聞く | `AGENTS.md` が変えられたか、読まれていない: `tfl_ai_prepare(..., overwrite = TRUE)` で書き直し、試験のフォルダそのものを VS Code で開く |
| `tfl_ai_collect_answers()` で `error`（"the YAML cannot be read"） | エージェントに伝える: ファイル（`ai/spec/outputs/<id>.yml`）の形を直す |
| "Not read (no request asks for them)" | TOC のどの帳票の名前でもない下書き: id は `ai/requests/index.md` にある |
| 文字にしたシェルがおかしい（セルが抜ける、順が違う） | エージェントにワークブックを読ませる: `tfl_ai_prepare(..., agent_reads = "workbook")`。各依頼に、シェルがファイルのどこにあるかが書かれる |
| `print(r)` の最後が "Next: no TOC found" か "no shell found" | 書かれているとおりに渡す: `toc_sheet = "<シート名>"` か `sheets = c(...)` |
| 2〜3 回直しても同じエラー | `ai/spec/outputs/<id>.yml` を手で（またはビルダーで）直し、手順 6 を実行 |
| `tfl_build_ard()` が止まる: `The package "broom" (>= 1.0.8) is required.` | `install.packages("broom")`（ほかのパッケージの名前が出たときも、それを入れる） |
| `tfl_build_ard()` が止まる | 帳票を絞って探す: `tfl_build_ard(st, dir = ".", output_id = "T-14-1-1", save = FALSE)` |
