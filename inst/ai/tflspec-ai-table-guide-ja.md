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

## おすすめの AI の環境

| 水準 | 環境 |
|---|---|
| **確認済み** | Claude デスクトップアプリの Code タブ（Claude Code）で Opus または Fable。VS Code + GitHub Copilot（エージェントモード）で Fable（2026-11-02 に会社の PC で実施）。 |
| **動くと見込むが未確認** | VS Code + Copilot で Opus（Opus は Claude Code でのみ試しました）。VS Code の Claude Code 拡張機能（Code タブと同じエンジン。単独では未確認）。Cursor など、試験フォルダのファイルを読み書きでき `Rscript` を実行できるエージェントツール: B の流れに従います。 |
| **未検証** | Copilot の GPT 系モデル: Opus や Fable と同等とは考えないでください。 |
| **おすすめしない** | Sonnet と Haiku。試しでは、Sonnet は数字が変わる決め方（シフト表の「最悪」。3 回中 2 回）を誤り、キットの決まりも破りました。Haiku は質問はよいものの、直しが当て推量です。 |

Opus または Fable を選べるプランを選んでください。

必要な条件:
1. 試験の `ai` フォルダをワークスペースとして開き、エージェントがその中だけで作業するようにします
   （VS Code では 5 節の手順 1 のとおり `ai/<試験>.code-workspace`）。
2. エージェントがターミナルで `Rscript` を実行できること。できない場合は、あなたが
   試験フォルダで `Rscript run.R <request>` を実行して `c` と入力します。
3. VS Code + Copilot では、設定（Ctrl+, を押して名前で検索）
   （[VS Code の AI 設定](https://code.visualstudio.com/docs/agents/reference/ai-settings.md)）:
   - `github.copilot.chat.summarizeAgentConversationHistory.enabled`: オン（既定）;
   - `chat.agent.maxRequests`: 既定の 25 から、たとえば 1000 に上げる;
   - `chat.tools.terminal.autoApprove`: `"/^Rscript /": true` を足す;
   - 組織によって設定が固定されているときは、IT 部門に相談します。
4. 本番の試験の前に、自分の PC で確かめます（末尾の「会社の PC で始める前に」）。

### A. Claude デスクトップアプリの Code タブ

1. Code タブを開き、入力欄の下のプロジェクトフォルダの選択で、試験の `ai` フォルダ
   （`agent_prompt.md` があるフォルダ）を選びます。
2. 入力欄の下のモデルピッカーで Opus または Fable を選びます。
3. 最初のメッセージとして `ai/agent_prompt.md` の文を貼り付けます。
4. 質問にチャットで答えます。止まるたびに `c`（次の依頼）、`5` などの数（次の 5 件を
   まとめて）、`a`（残り全部をまとめて）のどれかを返します。
5. 初めて `Rscript` の許可を求められたら許可します。作成と確認ができます。
6. 行き詰まったと言われたら、そのメッセージのとおりにして、`c` と入力します。

### B. VS Code の GitHub Copilot（エージェントモード）または Claude Code 拡張機能

1. 5 節のとおり `ai/<試験>.code-workspace` をダブルクリックします（または File > Open Workspace from File）。
2. チャットを開きます。Copilot では Agent の選択とモデル（言語モデル）の選択を行います
   （Fable は確認済み、Opus は見込み）。Claude Code 拡張機能では、入力欄の下の
   モデル名をクリックして選びます。
3. Copilot のみ: 上の条件 3 の三つの設定を行います（Ctrl+, を押して各名前で検索）。
4. 以降は A と同じです: `ai/agent_prompt.md` を貼り付け、答え、`c` / 数 / `a`、
   行き詰まったらメッセージに従います。

## 2. どこに何を置くか

```text
mystudy/
  TOC_SHELL.xlsx        TOC とシェル（1 つのワークブック）
  adam/                 adsl.xpt, adae.xpt, adlb.xpt ...
  AGENTS.md             tflspec が書く（試験フォルダの直下）: 「ai/AGENTS.md を読む」（手順 3）
  ai/AGENTS.md          tflspec が書く: エージェントへの指示（手順 3）
  ai/requests/          tflspec が書く: 1 依頼 1 ファイル（手順 3）
  ai/decisions.md       チャットでのあなたの答え、1 決定 1 行（手順 5）
  ai/spec/outputs/      エージェントの下書き、1 帳票 1 ファイル（手順 5）
  ai/repairs/           tflspec が書く: 送り返す直しの依頼（手順 6）
```

インストール（初回のみ。新しい R には `remotes` がありません。`broom` は cardx の
信頼区間に要ります）:

```r
# tflplanner の記事も同じ一覧を入れます
install.packages(c("remotes", "cards", "cardx", "haven", "broom", "broom.helpers", "parameters", "dplyr", "tidyr", "readxl", "officer", "ggsurvfit", "patchwork"))
ref <- "ai-test-2026-11-09"  # 試験のタグ。伝えられたものを使います
remotes::install_github(paste0("ichirio/rtfreporter@", ref), upgrade = "never")
remotes::install_github(paste0("ichirio/tflspec@", ref), upgrade = "never")
remotes::install_github(paste0("ichirio/tflplanner@", ref), upgrade = "never")
```

伝えられたタグを使います（開発版なら `next`）。出来合いのサンプルで先に流れを試すには、
`tflplanner::create_ai_sample_study()` を使います（tflplanner の AI の記事を参照）。

## 3. 何を実行するか

```r
library(tflspec)
r <- tfl_ai_prepare("mystudy", "mystudy/TOC_SHELL.xlsx", adam = "adam",
                    study_id = "ABC-123", language = "Japanese")
r                                   # シート・シェルのシート・依頼、そのあと「Next:」の行
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
`AGENTS.md` と `.github/copilot-instructions.md`（試験フォルダの直下と `ai/` の中の両方。短い文: `ai/AGENTS.md` を読む。エージェントが従うのは `ai/AGENTS.md` です）、
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
書かれていない TOC の項目は、その番号（`Table 15.2.4.2.b` のように最後にパート記号が付いた番号も）を
書いたシートが 1 つだけならそのシートの帳票にし（`source` が `"guess"`）、2 つ以上
なら各シートの候補として示します。直した内容は依頼を書くときに
`ai/sheet_numbers.csv` に保存され、次に `numbers` を渡すまで以後の呼び出しで
使われます。

**エージェントの規則**は `study.yml` の `ai:` キーです（試験標準の
`spec/defaults.yml` に先に書いてもよく、`study.yml` が優先されます）:

```yaml
ai:
  language: ja          # チャットの言語（既定は "en"）
  run: one              # "one": 依頼を 1 つ処理してメニュー。"batch": 全部を一度に、質問は最後にまとめて
  requests_per_chat: 0  # この依頼数で止まり、新しいチャットを求める（0: 制限なし）
  carry: [all, dataset, section]   # どの決定を聞かずに後の帳票へ適用するか
  max_rounds: 3         # 同じ項目を聞き直す最大回数。超えたら要確認
  footnotes_at: footer  # シェルの脚注の置き場所: "footer" または "table"
  review: {figure: 3, table: 2, listing: 1}  # エージェントが作った帳票を確かめる深さ: 0〜3、または none
```

## 4. エージェントが読むもの

| 読むもの | 読まないもの |
|---|---|
| 渡された依頼と、そこに名前のあるファイル: `ai/instructions-*.md`、`ai/templates.md`、`ai/adam_summary.md`、`spec/data.yml`、`spec/defaults.yml`、`ai/decisions.md`、`ai/reference/`（会社の規約とヒント。あるとき） | **XPT ファイル**、レコードを含むデータ全般（被験者 ID、日付、結果）。ほかのフォルダ、ほかのファイル |

会社が試験データに使うことを認めた AI を使ってください。エージェントに渡るのは
ADaM の要約で、データではありません。`AGENTS.md` に、開いてよいものが書かれます。
`tfl_ai_prepare(..., agent_reads = c("workbook", "adam"))` にすると、ワークブック
（シェルの文字が切れて見えるとき、依頼が示す場所を読む）と ADaM（変数の値を確かめる。
データのファイルをエディタで開くことはしない）も、PC にある道具で読めます。
`docs = ` で文書（SAP、データガイド）を渡すと、`ai/docs/` に写されます。
エージェントは何も入れません。実行するのは 1 つのコマンド `Rscript ../run.R <依頼名>`（作業場所の `ai/` から）だけで、
下書きの帳票を作って確かめるために使います（7 節）。

各依頼は数千文字です。エージェントの手順、その帳票に当てはまる決定、帳票の TOC の行と
シェル、読むファイルの名前が入ります。`max_chars` を超える依頼は分けられます。

## 5. エージェントの質問・あなたの決定・下書き

1. VS Code で試験の `ai` フォルダを開き（`ai/<試験>.code-workspace`（`<試験>` は試験フォルダの名前。例では `ai/mystudy.code-workspace`）をダブルクリック。
   エージェントにはそのフォルダだけが見え、`spec/`・プログラム・データは見えません）、
   エージェントのチャットに `ai/agent_prompt.md` の
   文を 1 回だけ渡します。エージェントは `ai/requests/index.md` の依頼を順に、
   1 つずつすべて行います。
2. エージェントが、各帳票を決めるのにまだ決まっていないこと（解析対象集団、
   パラメータ、治療の変数、パーセントの分母、何を数えるか、欠測値、サブグループの
   条件）を、番号付きで**質問**します。質問には帳票の id とタイトル、選択肢と既定が
   書かれます。**チャットで答えて**ください。「default」と答えると既定を採ります。
   答えで決まらないときは同じ項目をもう一度聞き（最大 3 回）、それでも決まらなければ
   最も可能性の高い選択肢を採って*要確認*にします。
3. 答えは `ai/decisions.md` に 1 決定 1 行で記録され
   （`- D1 [all] treatment_var.SAF: TRT01A (chat, T-14-3-1)`）、以後の帳票に適用されます。
   決定が成り立つと確信できるとき（決定の範囲が `all`、またはその帳票のデータセット・
   TOC の節で、`carry` の指定どおり）は聞かずに、そうでないときは「ここも同じか」と
   聞きます。始める前に自分で行を書いておくこともできます（範囲のない行は `all`）。
4. エージェントは各帳票の**骨組み**を `ai/spec/outputs/<id>.yml` に書きます:
   `id`、`kind`、`label`、`population`、`data`、`ard`（解析データ、解析）、内容
   （合うテンプレートがあればテンプレート、なければ `table_plan` と `plan_cells`）、
   シェルに桁数があればその桁数、`plan_columns: {widths}`（シェルの比率から）、
   `page: titles` とシェルの脚注（`footnotes_at`）。
   `ai:` の下には、`assumptions`（それぞれ `from: D<n>`）と `confirm`（要確認）が
   入ります。見出し行、ページ、罫線、フォントは書きません。それらは試験の
   既定とビルダーが担当します。そのあとは `run` に従って進みます（下記）。
5. すべての依頼が終わると止まり、書いたファイル、足した決定、要確認のことを知らせます。
   それを確かめてください（手順 6）。依頼の合間に止めて、あとで **「c」**（または「continue」）と
   伝えることもできます。エージェントは `ai/progress.md` から続け、あなたの決定は残ります。

帳票ではなく作業についての質問（ファイルが無い、依頼が不明確）も、エージェントは
チャットで聞きます。

### エージェントが依頼を進めるやり方（`run`）

`study.yml` の `ai: run` で、エージェントの始め方を決めます。既定は `one` です。
- **`one`**（既定）: エージェントは依頼を **1 つ**処理します。質問をチャットで聞き、
  下書きを書き、作って確かめます。そのあとメニューを出して止まります。
  `c` = 次の依頼、数字 = 次の N 依頼をまとめて（例: `5`）、`a` = 残りを全部まとめて。
  依頼を 1 つ試して確かめ、そのあと `a` で残りを一度に任せる、という使い方に向いて
  います。（`続ける`・`次`・`continue` も `c`、`全部`・`all` も `a` の意味です。）
- **`batch`**: 全部を一度に。質問は最後に 1 回です。どの質問も既定を採り、質問を
  `ai/questions-end.md`（バッチごとに 1 節）に控え、すべてを作って確かめたあと、
  **1 通でまとめて**質問します。変えたい答えだけを返してください。変えた答えを
  引いている帳票だけが、書き直されて作り直されます。

依頼は 1 つずつ別に処理され、途中の結果はそのつど（下書き、`ai/decisions.md`、
`ai/questions-end.md`、作業の待ち行列である `ai/progress.md` の行）ファイルに書かれ
ます。エージェントのチャットが要約されたり失敗したりしても、そのファイルから続きます。
前の決定がある帳票に当てはまるかは、その帳票を処理するときに、その帳票が判断します。

**質問だけでなく、採った既定をすべて読んでください。既定は実行ごとに変わることが
あります。** `ai/defaults.md`（`run.R` の最初のビルドで書かれます）に、重要なものから順に並んでいます（数値が変わるもの、
行や統計量が変わるもの、文言や見た目が変わるもの）。バッチの最後のメッセージには、
エージェント自身が選んだ既定（文書に由来しないもの）を話題ごとにまとめた最初の節の、
最初の 2 つのグループがそのまま載り、残りは件数だけです。既定を変えるときも、答えと同じ書き方です
（`D<n>: ...` または `<帳票> <項目>: ...`）。

**所要時間（見積もり）**

| やり方 | 試験 | 質問が来る時 | エージェントの実行 | あなたが質問に答える時間 | 答えたあと |
|---|---|---|---|---|---|
| one、そのあと「残りを全部」 | シェル 1 / 帳票 2 | 約 1 分後（その依頼の質問） | 約 5〜6 分 | 数分 | 作り直し、約 2〜3 分 |
| one、そのあと「残りを全部」 | シェル 50 / 帳票 100 | 最初の依頼: 約 1 分後。以後は約 5 時間後にバッチで 1 回 | 約 5 時間 | 約 20〜40 分 | 変えた帳票 1 つにつき約 1〜2 分 |
| batch | シェル 50 / 帳票 100 | 約 5 時間後にバッチで 1 回 | 約 5 時間 | 約 20〜40 分 | 変えた帳票 1 つにつき約 1〜2 分 |

見積もりです。24 帳票のテストの結果で更新します。

長い実行では、VS Code の `chat.agent.maxRequests`（既定は 25。25 ステップで
エージェントが「続けますか？」と止まります）を上げ、Rscript を確認なしで実行させて
ください: `chat.tools.terminal.autoApprove: {"/^Rscript /": true}`。

VS Code は長いチャットを自動で要約します。エージェントは `ai/progress.md` から続けます。
代わりに、`ai: requests_per_chat` を指定すると、その依頼数で止まります。

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

## 7. エージェントが自分の下書きを作って確かめる

依頼の下書きを書いたあと、エージェントは `ai/`（作業場所）から `Rscript ../run.R <依頼名>` を
実行します（あなたが実行するときは、試験のフォルダで `Rscript run.R <依頼名>` です）。R が下書きを確かめ、ARD と RTF を作り、図を描きます。ADaM を読むのは
R で、エージェントではありません。

出力フォルダの各 RTF の隣（下書きを別にしている試験では `ai/output/tfl/`。図の PNG も
ここ）に、R が次を書きます:

- `<id>.R`: その帳票のプログラム。
- `<id>-facts.txt`: できあがったものを言葉にしたもの。表では、見出し、行、空のセル、
  続いてページ（使った行数と 1 ページに入る行数、ページのヘッダーとフッター）。
  図では、ラベル、軸、凡例、レイヤー。

エージェントはこれらをシェルと見比べ、下書きを直して作り直します。直すのは**最大 2 回**
です。それでも違うときは、図は自分の ggplot2 コードで描き、表は下書きのまま差分を
*要確認*として残します。

**確かめる深さ**は、帳票の種類ごとに決まります:

| レベル | エージェントが読むもの |
|---|---|
| `0` | 検査の結果: `ai/status.md`、直しの依頼 |
| `1` | ＋プログラム（`<id>.R`） |
| `2` | ＋ facts（`<id>-facts.txt`） |
| `3` | ＋出力: 図の PNG、表のページ |
| `none` | その種類は作らない。あなたが `run.R` を実行する |

既定は、図が 3、表が 2（表が 1 ページを超えるときはページも読む）、Listing が 1 です。
`study.yml` の `ai: review: {figure: 3, table: 2, listing: 1}` で変えます。試験標準の
`spec/defaults.yml` に会社の既定を書いてもかまいません。変えたあとは、エージェントへの
指示を書き直します:

```r
tfl_ai_refresh_agents(".")   # study.yml から AGENTS.md を書き直す。下書きと決定は残る
```

tflplanner では、AI タブの選択欄が同じことをします。

実測の一例: 見本の試験で 5 帳票を 1 回回すと、どのレベルでも 110〜120k トークンで
した。深さによる費用の差は小さいということです。

エージェントが R を実行できないとき（ターミナルが拒否される、R が見つからない）は、
そう伝えて、あなたに、試験のフォルダでの `Rscript run.R <依頼名>`（または tflplanner の AI タブ）の実行を
頼みます。そのあと、同じファイルを読みます。

## 8. 過去の TFL から会社のヒントを作る

会社の帳票には、その会社らしい作りがあります。エージェントは過去の RTF からそれを学び、
すべての試験で使うヒントとして残せます。

会社の AI フォルダは、オプション `tflspec.ai_home` です（tflplanner は `<home>/ai` に
します）:

```text
ai/                     会社の AI フォルダ
  past_tfl/             過去の RTF、1 試験 1 サブフォルダ
  AGENTS-learn.md       tflspec が書く: エージェントへの課題（手順 2）
  learn/                tflspec が書く: R が RTF から読んだもの
  company_hints.yml     エージェントが書く: ヒント（手順 4）
  reference/            会社の規約（あれば）
```

tflplanner を使わないときは、先にフォルダを決めます: `options(tflspec.ai_home = "C:/.../company-ai")`
（または各呼び出しに `dir =` を渡します）。

1. 過去の RTF の表を `past_tfl/` に写します。エージェントに見せてよいファイルだけを
   使ってください。
2. `tfl_ai_style_prompt(standard = <会社標準>)` を実行します。会社標準は `kind`・`key`・
   `value` のデータフレームです。標準がなくても動きます。

   ```r
   options(tflspec.ai_home = "C:/.../company-ai")
   std <- data.frame(kind = c("all", "demog", "ae_soc_pt"),
                     key = c("font", "n_form", "first_row"),
                     value = c("Courier New 9 pt", "(N=xx)", "Any TEAE"))
   tfl_ai_style_prompt(standard = std)
   ```

   kind は次のとおりです: all, demog, ae_soc_pt, ae, param_change, shift, listing, figure, other。
   key は次のとおりです: rules, spanner_rule, stub_ratio, font, orientation, titles, footnotes, n_form, categorical, zero, continuous, digits, stub_heading, first_row, sort。

3. AI フォルダを VS Code で開き、エージェントに `AGENTS-learn.md` を渡します。
4. エージェントが `company_hints.yml` を書きます。この会社の帳票の特徴を種類ごとに、
   何本のファイルに見られたかを添えて書きます。ファイルが会社標準と違う点は
   `conflicts:` に並べ、ヒントとしては書きません。
5. `tfl_ai_style_read()` で読みます。1 行ずつ採用する必要はありません。

これ以後に準備する試験には、`ai/reference/` にそのコピーが入ります。すでに準備した
試験には `tfl_ai_refresh_agents(".")` で入ります。

試験のエージェントが決めるときの優先順位:

1. シェル、SAP、試験の文書、標準
2. ヒント（`from: "company hints"`。半数に満たないファイルにしか見られないヒントは
   *要確認*にする）
3. 通常の慣習
4. あなたへの質問

ヒントがシェル、SAP、標準に優先することはありません。

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

これは tflspec 自身の腫瘍の見本です（5 帳票、下書きは同梱）。tflplanner の AI タブには別の見本、試験 DMY-001（8 帳票、エージェントが下書きしたもの）があります。そちらは tflplanner の記事を見てください。

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

## 既知の制限

- `shift` / `lab_shift`: ベースラインのコードリストに Missing と Total を入れて、`levels` はコードリストのラベル（Low, Normal, High）で書きます。

## 会社の PC で始める前に

- [ ] エージェントのチャットで、ターミナルで `Rscript --version` を実行するよう頼みます。
  - 実行を許されない、または VS Code が毎回確認してくるときは、作って確かめる手順は
    あなたが `Rscript run.R` を実行する形になります（7 節）。
  - `Rscript` が見つからないときは、`AGENTS.md` に、R 自身の `Rscript.exe` を探す
    PowerShell の書き方があります。
- [ ] エージェントに、作った PNG（`ai/output/tfl/<id>.png`）を開いて、何が見えるか言わせ
  ます。画像が見えなければ、代わりに `<id>-facts.txt` を読むので、何も変えなくて
  かまいません。
- [ ] ターミナルが見つける R にパッケージが入っているか確かめます:
  `Rscript -e "packageVersion('tflspec')"`。
- [ ] 会社の AI フォルダ: 使うなら、過去の TFL と `company_hints.yml`。
- [ ] 長い実行では、VS Code の `chat.agent.maxRequests`（既定は 25。25 ステップで
  エージェントが「続けますか？」と止まります）を上げ、Rscript を確認なしで実行させます:
  `chat.tools.terminal.autoApprove: {"/^Rscript /": true}`。

## うまくいかない時

| 見えたもの | すること |
|---|---|
| `r$toc` が空: TOC のシートが見つからない | `tfl_ai_prepare(..., toc_sheet = "<TOC のシート名>")` |
| `r$toc$shell` が `NA`: 帳票番号でシェルが見つからない | そのシートを足す: `tfl_ai_prepare(..., sheets = c("<シェルのシート>", ...))` |
| エージェントが質問せずに下書きを書く、または決定で決まることを聞く | `AGENTS.md` が変えられたか、読まれていない: `tfl_ai_prepare(..., overwrite = TRUE)` で書き直し、試験の `ai` フォルダを VS Code で開く（`ai/<試験>.code-workspace`） |
| `tfl_ai_collect_answers()` で `error`（"the YAML cannot be read"） | エージェントに伝える: ファイル（`ai/spec/outputs/<id>.yml`）の形を直す |
| "Not read (no request asks for them)" | TOC のどの帳票の名前でもない下書き: id は `ai/requests/index.md` にある |
| 文字にしたシェルがおかしい（セルが抜ける、順が違う） | エージェントにワークブックを読ませる: `tfl_ai_prepare(..., agent_reads = "workbook")`。各依頼に、シェルがファイルのどこにあるかが書かれる |
| `print(r)` の最後が "Next: no TOC found" か "no shell found" | 書かれているとおりに渡す: `toc_sheet = "<シート名>"` か `sheets = c(...)` |
| 2〜3 回直しても同じエラー | `ai/spec/outputs/<id>.yml` を手で（またはビルダーで）直し、手順 6 を実行 |
| `tfl_build_ard()` が止まる: `The package "broom" (>= 1.0.8) is required.` | `install.packages("broom")`（ほかのパッケージの名前が出たときも、それを入れる） |
| `tfl_build_ard()` が止まる | 帳票を絞って探す: `tfl_build_ard(st, dir = ".", output_id = "T-14-1-1", save = FALSE)` |
| エージェントが R を実行できないと言う | 試験のフォルダで自分で `Rscript run.R <依頼名>` を実行し、エージェントに「done」と伝える |
| エージェントが止まり、行き詰まったと言う | そのメッセージのとおりにする（`/compact`、`ai/agent_prompt.md` を渡した新しいチャット、または Rscript の許可）。そのあと `c` と入力。`ai/progress.md` から続きます |
| `<id>-facts.txt` に、表が 1 ページを超えるとある | ページはビルダーが決めるので、エージェントは*要確認*にする |
| `AGENTS.md` に古い確認レベルが残っている | `tfl_ai_refresh_agents(".")` |
