# AI 連携の試し方（テスター向け）

架空の試験 **DMY-001**（Example Pharma）で、AI のエージェントが帳票の SPEC を書く流れを、tflplanner の「**AI 連携**」タブで試します。材料はすべて架空です。答え（SPEC や期待する帳票）は入っていません。SPEC はエージェントが書きます。

必要なもの: R 4.1 以上、VS Code と AI のチャット（エージェントモード）。

## 0. 準備（1 回だけ）

伝えられたタグでパッケージを入れ、tflplanner のホームを作ります。ホームは **VS Code でフォルダとして開ける場所の下**に置きます。

```r
install.packages("remotes")
remotes::install_github("ichirio/rtfreporter@<タグ>", upgrade = "never")
remotes::install_github("ichirio/tflspec@<タグ>", upgrade = "never")
remotes::install_github("ichirio/tflplanner@<タグ>", upgrade = "never")
install.packages(c("haven", "readxl", "officer", "cards", "cardx", "dplyr", "tidyr", "ggsurvfit",
                   "patchwork", "broom", "broom.helpers", "parameters"))
tflplanner::setup_tflplanner(home = "C:/work/tflplanner-home", language = "ja")
```

## 1. 材料を作る

```r
tflspec::tfl_ai_sample_study("C:/work/tflplanner-home/ai-sample")
```

この関数が作るのは**材料だけ**です。試験そのもの（`studies/DMY-001`）はまだできません。試験は、次の手順 1 の 1 つめ（「新規試験」）で作ります。

`ai-sample` の下に次ができます。

- `adam/`: ADaM 5 つ（adsl・adae・adlb・advs・adtte の `.xpt`。haven が無い時は `.rds`）。advs はどのシェルでも使いません。
- `inputs/toc.xlsx`: TOC（帳票 8 つ）。`inputs/shells.xlsx`: 注釈付きシェル（6 つ。1 シート＝1 シェル）。
- `inputs/adam_spec.xlsx`: ADaM の仕様（Pinnacle 21 の形）。`inputs/define.xml` は同じ内容の define.xml。
- `inputs/sap.docx`: SAP（参考資料）。

材料には、エージェントがチャットで質問すべき所をわざと残しています。黙って決めずに聞いてくるかを見てください。

## 2. 手順 1「プロンプトを書く」

`tflplanner::run_app()` で起動し、「**AI 連携**」タブ →「**1 プロンプトを書く**」で上から入れます。

1. 「**新規試験**」を選び、ID に `DMY-001`、試験名に `Example Pharma DMY-001（架空）` と入れます。ここで試験（`studies/DMY-001`）ができます。
2. 「**TOC / SHELL ブック**」に `inputs/toc.xlsx`、「**SHELL ブック**」に `inputs/shells.xlsx` を選びます。
3. 「**ADaM フォルダ**」に `adam` フォルダを選びます（欄の下に「5 ファイル」と出ます）。
4. 「**ADaM の仕様**」に `inputs/adam_spec.xlsx` を選びます。
5. 「**参考資料を追加**」で `inputs/sap.docx` を選びます。
6. 「**読み取って確かめる**」を押し、右側で確かめます。
   - TOC 項目が 8 つ、シェルが 6 つ。
   - 「シート」で `General conventions`（全帳票の共通の決まり）を「**その他のシート（シェルではない）**」に動かし、「**当てはめて読み直す**」を押します。左の「**全依頼に足すシート**」で `General conventions` を選びます。
7. 「**依頼を書く**」を押します。

## 3. 手順 2「エージェントで SPEC を作成」（VS Code）

1. 「**2 エージェントで SPEC を作成**」でプロンプトをコピーします。
2. VS Code で試験の **ai フォルダ**（`C:/work/tflplanner-home/studies/DMY-001/ai`）を開きます。`ai/DMY-001.code-workspace` をダブルクリックしても同じです。試験フォルダ（`DMY-001`）や、その上の `studies` は開きません（エージェントが読むのは ai フォルダの中だけです）。
3. チャットをエージェントモードにして、プロンプトを貼って送ります。
4. エージェントの質問（集団・桁数など）にチャットで答えます（例「1: FAS、2: 生データの桁数＋1」）。止まったら「**続けて**」と送ります。
5. パッケージを入れようとしたり、フォルダを探し回ったりしたら、「AGENTS.md に従い、下書きを書くだけにして」と送ります。

## 4. 手順 3「SPEC を検査」

1. 「**3 SPEC を検査**」で「検査」を押します（途中でも押せます）。
2. 帳票ごとに **ERROR**（読み込めない）・**WARNING**（プログラムが作れない）・**OK** が出ます。
3. ERROR・WARNING の行の「直しの文」をコピーし、同じチャットに貼って送ります。直ったら、もう一度「検査」を押します。

## 5. 手順 4「試験エリアにコピー」

1. 「**4 試験エリアにコピー**」で、コピーする帳票にチェックします。
2. 「チェックした帳票を試験に採用（写す）」を押すと、試験の帳票一覧に入ります。
3. エージェントの仮定が出たら、正しいか・違うかを答えます。違う所は修正の依頼として、チャットに送れます。
4. 「すべての依頼の帳票を作る」で帳票を作り、RTF を開いて見ます。

## 記録してほしいこと

- 最初の下書きまでの時間と、全体の時間。
- 質問の数と、答えやすかったか。わざと決められないようにした所を、聞いてきたか。
- 3 回直しても ERROR・WARNING が消えなかった帳票。
- 詰まった所と、その時の画面の文（スクリーンショット）。
