# The prompt that sends an answer's problems back

One user turn for the same chat: the problems of an answer as a table,
the schema of the parts they are in (only those: the chat already has
the first prompt), and the request to return the whole block again,
corrected. Paste it as the next message of the chat; in API mode it is
the next user message of the conversation.

## Usage

``` r
tfl_ai_repair_prompt(answer, problems = NULL, context = NULL)
```

## Arguments

- answer:

  A `tfl_ai_answer`
  ([`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md)).

- problems:

  The problems to send: by default those of
  [`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md)
  with `context`.

- context:

  The
  [`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)
  of the prompt, for the checks.

## Value

The prompt, one string of class `tfl_ai_text`.

## Examples

``` r
answer <- c("```yaml",
            "tflspec_ai: {task: toc, version: 1}",
            "toc:",
            "- {output_id: T-14-1-1, title: Demographics, colour: red}",
            "```")
tfl_ai_repair_prompt(tfl_ai_parse(answer, "toc"))
#> Your answer had these problems. An error must be fixed; a warning is a name or value worth checking again.
#> 
#> | severity | where | problem |
#> |---|---|---|
#> | error | toc row 1 field colour | is not a column of toc; its columns are output_id, type, title, population, footnote, program, file, note, section, datasets, label |
#> | warning | toc row 1 field type | blank is not table, figure or listing; read as table |
#> 
#> The schema of toc:
#> 
#> `toc` is a list of rows, one a report. Its columns:
#> 
#> - `output_id`: The report's number as the documents give it, unique in the TOC (required). Example: `T-14-1-1`
#> - `type`: The kind of report: table, figure or listing. Example: `table`
#> - `title`: The title lines in order, without the report number and the analysis set; ` | ` between lines. Example: `Summary of Demographic and Baseline Characteristics`
#> - `population`: The analysis set the report is on, as the documents name it; it becomes the last title line. Example: `Safety Population`
#> - `footnote`: The footnote lines in order; ` | ` between lines. Example: `Percentages are based on the number of subjects in the analysis set.`
#> - `program`: The name of the program that makes the report. Example: `t_14_1_1.R`
#> - `file`: The name of the report's output file. Example: `t_14_1_1.rtf`
#> - `note`: A remark for people; never read by tflspec.
#> - `section`: The heading the report is listed under in the documents. Example: `14.1 Demographic and Baseline Data`
#> - `datasets`: The ADaM datasets the report reads; ` | ` between them. Example: `ADSL`
#> - `label`: The report's number as printed in its title. Example: `Table 14.1.1`
#> 
#> Return the whole block again, corrected, with the same header. Keep everything that was right and change only what the problems name. When a problem asks for a name the context does not list, leave the item out and say so under `assumptions`.
```
