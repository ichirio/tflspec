# ============================================================================
#  Document import, level 1: a study's define as code lists and labels
# ----------------------------------------------------------------------------
#  A define (Define-XML 2.0 / 2.1, CDISC's standard XML; or the workbook it
#  is made from) already says what each dataset's variables are called,
#  their labels and the values each may take with their decodes.  These are
#  read by rule -- no AI, no guessing -- into tidy data frames, and the code
#  lists a report needs are turned into rows of the spec's `codelists`
#  sheet.  Nothing is written: the rows are a draft for the user (or
#  tflplanner) to take in.
# ============================================================================

.define_odm <- "http://www.cdisc.org/ns/odm/v1.3"
.define_xml_ns <- "http://www.w3.org/XML/1998/namespace"

#' Read a Define-XML (2.0 or 2.1)
#'
#' Reads a study's `define.xml` -- the CDISC standard description of its
#' datasets -- by rule (\pkg{xml2}), and gives its datasets, variables and
#' code lists as tidy data frames.  The result is what
#' [tfl_define_codelists()] turns into a report's `codelists` rows.
#'
#' * `datasets`: one row an `ItemGroupDef`: `dataset`, `label`, `class`,
#'   `subclass`, `structure`, `purpose`, `repeating`, `reference`, `keys`
#'   (the key variables in key order, `" | "` between them), `order`, `oid`.
#' * `variables`: one row a variable of a dataset (an `ItemRef` and its
#'   `ItemDef`), in the dataset's order: `dataset`, `variable`, `label`,
#'   `type`, `length`, `digits` (significant digits), `format`,
#'   `codelist` (the code list's OID), `mandatory`, `order`, `key`, `role`,
#'   `origin`, `oid`.
#' * `codelists`: one row a value (`CodeListItem` or `EnumeratedItem`), in
#'   the file's order: `codelist` (OID), `name`, `type`, `value` (the coded
#'   value), `decode` (`NA` for an enumerated item), `order`
#'   (`OrderNumber`), `rank`, `extended` (`def:ExtendedValue="Yes"`), `code`
#'   (the NCI code, `Alias` of context `nci:ExtCodeID`), `kind` (`"item"`
#'   or `"enumerated"`).
#' * `external`: code lists that point to a dictionary (`ExternalCodeList`,
#'   e.g. MedDRA): `codelist`, `name`, `dictionary`, `version`.  They hold
#'   no values.
#' * `study`: `study_name`, `description`, `protocol`, `define_version`,
#'   `file_oid`.
#'
#' Elements are found by their namespace URIs, not by prefix, so a file
#' that binds the namespaces to other prefixes reads the same.  Texts
#' (`TranslatedText`) are taken in `lang`; else the one without `xml:lang`;
#' else the first.  Value-level metadata, methods, comments and documents
#' are not read.
#'
#' @param path A `define.xml`.
#' @param lang The language of the texts to take (`xml:lang`), e.g. `"en"`
#'   or `"ja"`.
#' @return A `tfl_define`: a list of the data frames above, with
#'   `attr(, "source")` (the file, `"define.xml"`, the Define-XML version).
#' @seealso [tfl_read_define_spec()] for a define spec workbook;
#'   [tfl_define_codelists()].
#' @examples
#' if (requireNamespace("xml2", quietly = TRUE)) {
#'   def <- tfl_read_define(system.file("extdata", "define", "define.xml",
#'                                      package = "tflspec"))
#'   def
#'   def$variables[def$variables$dataset == "ADSL", c("variable", "label")]
#'   def$codelists[def$codelists$codelist == "CL.SEX", ]
#' }
#' @export
tfl_read_define <- function(path, lang = "en") {
  .ard_need("xml2", "tfl_read_define()")
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .ard_stop(sprintf("tfl_read_define(): no file '%s'.", path))
  }
  doc <- tryCatch(xml2::read_xml(path), error = function(e) {
    .ard_stop(sprintf("tfl_read_define(): '%s' is not XML: %s", path,
                      conditionMessage(e)))
  })
  ns <- .define_ns(doc, path)
  mdv <- xml2::xml_find_first(
    doc, "/odm:ODM/odm:Study/odm:MetaDataVersion", ns)
  if (inherits(mdv, "xml_missing")) {
    .ard_stop(sprintf(
      "tfl_read_define(): '%s' has no ODM/Study/MetaDataVersion.", path))
  }
  text <- function(nodes, at) .define_text(nodes, at, ns, lang)
  attr1 <- function(nodes, name) xml2::xml_attr(nodes, name, ns)

  gv <- xml2::xml_find_first(doc, "/odm:ODM/odm:Study/odm:GlobalVariables",
                             ns)
  gv_text <- function(el) {
    x <- xml2::xml_find_first(gv, paste0("odm:", el), ns)
    if (inherits(x, "xml_missing")) NA_character_ else trimws(xml2::xml_text(x))
  }
  study <- list(study_name = gv_text("StudyName"),
                description = gv_text("StudyDescription"),
                protocol = gv_text("ProtocolName"),
                define_version = attr1(mdv, "def:DefineVersion"),
                file_oid = xml2::xml_attr(xml2::xml_root(doc), "FileOID"))

  # the variables, as their ItemDefs say
  items <- xml2::xml_find_all(mdv, "odm:ItemDef", ns)
  item <- data.frame(
    oid = attr1(items, "OID"),
    variable = attr1(items, "Name"),
    label = text(items, "odm:Description/odm:TranslatedText"),
    type = attr1(items, "DataType"),
    length = .define_int(attr1(items, "Length")),
    digits = .define_int(attr1(items, "SignificantDigits")),
    format = attr1(items, "def:DisplayFormat"),
    codelist = .define_first_attr(items, "odm:CodeListRef", "CodeListOID",
                                  ns),
    origin = .define_first_attr(items, "def:Origin", "Type", ns),
    stringsAsFactors = FALSE)

  # the datasets and, through their ItemRefs, each one's variables
  igs <- xml2::xml_find_all(mdv, "odm:ItemGroupDef", ns)
  cls_attr <- attr1(igs, "def:Class")                       # 2.0
  cls_el <- .define_first_attr(igs, "def:Class", "Name", ns)  # 2.1
  refs <- lapply(igs, function(ig) {
    r <- xml2::xml_find_all(ig, "odm:ItemRef", ns)
    data.frame(oid = attr1(r, "ItemOID"),
               mandatory = attr1(r, "Mandatory"),
               order = .define_int(attr1(r, "OrderNumber")),
               key = .define_int(attr1(r, "KeySequence")),
               role = attr1(r, "Role"),
               stringsAsFactors = FALSE)
  })
  ds_names <- attr1(igs, "Name")
  datasets <- data.frame(
    dataset = ds_names,
    label = text(igs, "odm:Description/odm:TranslatedText"),
    class = ifelse(is.na(cls_el), cls_attr, cls_el),
    subclass = .define_first_attr(igs, "def:Class/def:SubClass", "Name", ns),
    structure = attr1(igs, "def:Structure"),
    purpose = attr1(igs, "Purpose"),
    repeating = attr1(igs, "Repeating"),
    reference = attr1(igs, "IsReferenceData"),
    keys = vapply(refs, function(r) {
      k <- r[!is.na(r$key), , drop = FALSE]
      if (!nrow(k)) return(NA_character_)
      paste(item$variable[match(k$oid[order(k$key)], item$oid)],
            collapse = " | ")
    }, ""),
    order = seq_along(igs),
    oid = attr1(igs, "OID"),
    stringsAsFactors = FALSE)
  variables <- do.call(rbind, c(list(.define_variables_empty()), lapply(
    seq_along(igs), function(i) {
      r <- refs[[i]]
      if (!is.null(r$order) && !all(is.na(r$order))) {
        r <- r[order(r$order, na.last = TRUE), , drop = FALSE]
      }
      it <- item[match(r$oid, item$oid), , drop = FALSE]
      data.frame(dataset = rep(ds_names[i], nrow(r)),
                 variable = it$variable, label = it$label, type = it$type,
                 length = it$length, digits = it$digits, format = it$format,
                 codelist = it$codelist, mandatory = r$mandatory,
                 order = r$order, key = r$key, role = r$role,
                 origin = it$origin, oid = r$oid,
                 stringsAsFactors = FALSE)
    })))
  rownames(variables) <- NULL

  # the code lists, one row a value
  cls <- xml2::xml_find_all(mdv, "odm:CodeList", ns)
  codelists <- do.call(rbind, c(list(.define_codelists_empty()), lapply(
    cls, function(cl) {
      it <- xml2::xml_find_all(cl, "odm:CodeListItem | odm:EnumeratedItem",
                               ns)
      n <- length(it)
      if (!n) return(NULL)
      data.frame(
        codelist = rep(attr1(cl, "OID"), n),
        name = rep(attr1(cl, "Name"), n),
        type = rep(attr1(cl, "DataType"), n),
        value = attr1(it, "CodedValue"),
        decode = text(it, "odm:Decode/odm:TranslatedText"),
        order = .define_int(attr1(it, "OrderNumber")),
        rank = suppressWarnings(as.numeric(attr1(it, "Rank"))),
        extended = .define_yes(attr1(it, "def:ExtendedValue")),
        code = .define_first_attr(
          it, "odm:Alias[@Context='nci:ExtCodeID']", "Name", ns),
        kind = ifelse(xml2::xml_name(it) == "EnumeratedItem", "enumerated",
                      "item"),
        stringsAsFactors = FALSE)
    })))
  rownames(codelists) <- NULL
  ext <- xml2::xml_find_all(mdv, "odm:CodeList[odm:ExternalCodeList]", ns)
  external <- data.frame(
    codelist = attr1(ext, "OID"),
    name = attr1(ext, "Name"),
    dictionary = .define_first_attr(ext, "odm:ExternalCodeList",
                                    "Dictionary", ns),
    version = .define_first_attr(ext, "odm:ExternalCodeList", "Version",
                                 ns),
    stringsAsFactors = FALSE)

  .define_new(study, datasets, variables, codelists, external,
              source = list(file = path, kind = "define.xml",
                            version = attr(ns, "version")))
}

# The namespaces by URI: `odm` (ODM 1.3), `def` (Define-XML 2.0 or 2.1),
# `xml`.  The prefixes the file uses do not matter.
.define_ns <- function(doc, path) {
  uris <- unique(unname(as.character(xml2::xml_ns(doc))))
  root <- xml2::xml_root(doc)
  if (xml2::xml_name(root) != "ODM" || !.define_odm %in% uris) {
    .ard_stop(sprintf(paste0(
      "tfl_read_define(): '%s' is not a Define-XML: its root is not an ",
      "ODM 1.3 element (%s)."), path, .define_odm))
  }
  def <- grep("^http://www\\.cdisc\\.org/ns/def/v[0-9.]+$", uris,
              value = TRUE)
  ver <- sub("^.*/v", "", def)
  ok <- ver %in% c("2.0", "2.1")
  if (!any(ok)) {
    .ard_stop(sprintf(paste0(
      "tfl_read_define(): '%s' is not a Define-XML 2.0 or 2.1 (its def ",
      "namespace is %s)."), path,
      if (length(def)) paste(def, collapse = ", ") else "missing"))
  }
  structure(c(odm = .define_odm, def = def[ok][[1L]], xml = .define_xml_ns),
            version = ver[ok][[1L]])
}

# Each node's text at `at` (a TranslatedText), in `lang` when there.
.define_text <- function(nodes, at, ns, lang) {
  vapply(seq_along(nodes), function(i) {
    tt <- xml2::xml_find_all(nodes[[i]], at, ns)
    if (!length(tt)) return(NA_character_)
    l <- xml2::xml_attr(tt, "xml:lang", ns)
    l1 <- tolower(sub("-.*$", "", l))
    pick <- which(!is.na(l) & l1 == tolower(lang))
    if (!length(pick)) pick <- which(is.na(l))
    if (!length(pick)) pick <- 1L
    trimws(xml2::xml_text(tt[[pick[[1L]]]]))
  }, "")
}

# Each node's first child at `at`: its attribute `name` (NA for none).
.define_first_attr <- function(nodes, at, name, ns) {
  vapply(seq_along(nodes), function(i) {
    x <- xml2::xml_find_first(nodes[[i]], at, ns)
    if (inherits(x, "xml_missing")) NA_character_ else
      xml2::xml_attr(x, name, ns)
  }, "")
}

.define_int <- function(x) suppressWarnings(as.integer(x))

.define_yes <- function(x) {
  !is.na(x) & toupper(trimws(x)) %in% c("YES", "Y", "TRUE", "1")
}

.define_variables_empty <- function() {
  data.frame(dataset = character(), variable = character(),
             label = character(), type = character(), length = integer(),
             digits = integer(), format = character(),
             codelist = character(), mandatory = character(),
             order = integer(), key = integer(), role = character(),
             origin = character(), oid = character(),
             stringsAsFactors = FALSE)
}

.define_codelists_empty <- function() {
  data.frame(codelist = character(), name = character(), type = character(),
             value = character(), decode = character(), order = integer(),
             rank = numeric(), extended = logical(), code = character(),
             kind = character(), stringsAsFactors = FALSE)
}

.define_new <- function(study, datasets, variables, codelists, external,
                        source) {
  structure(list(study = study, datasets = datasets, variables = variables,
                 codelists = codelists, external = external),
            source = source, class = "tfl_define")
}

#' @export
print.tfl_define <- function(x, ...) {
  src <- attr(x, "source")
  cat(sprintf("<tfl_define> %s%s\n", src$kind,
              if (!is.null(src$version) && !is.na(src$version))
                paste0(" ", src$version) else ""))
  if (!is.na(x$study$study_name %||% NA)) {
    cat(sprintf("  study: %s\n", x$study$study_name))
  }
  cat(sprintf("  %d dataset(s): %s\n", nrow(x$datasets),
              paste(utils::head(x$datasets$dataset, 10L), collapse = ", ")))
  cat(sprintf("  %d variable(s), %d with a code list\n", nrow(x$variables),
              sum(!is.na(x$variables$codelist))))
  cat(sprintf("  %d code list(s), %d value(s); %d external dictionar%s\n",
              length(unique(x$codelists$codelist)), nrow(x$codelists),
              nrow(x$external), if (nrow(x$external) == 1L) "y" else "ies"))
  invisible(x)
}

# ----------------------------------------------------------------------------
#  A define spec workbook, through an import profile
# ----------------------------------------------------------------------------

#' An import profile for a define spec workbook
#'
#' Says which sheet and which column of a define spec workbook is what, for
#' [tfl_read_define_spec()].  `"p21"` is the common Pinnacle 21 layout:
#' sheets `Datasets`, `Variables`, `Codelists` and `Dictionaries`, with
#' their usual column names.  A company whose workbook differs changes the
#' entries that differ, e.g. `map = list(variables = list(label = "Variable
#' Label"))` in [tfl_read_define_spec()], or edits the whole list and
#' passes it as `profile`.
#'
#' Each part is a list: `sheet` (the sheet's name), then field = column
#' name.  A field set to `NA` is not read.  The fields:
#'
#' * `datasets`: `dataset` (required), `label`, `class`, `structure`,
#'   `purpose`, `keys`, `repeating`, `reference`;
#' * `variables`: `dataset`, `variable` (both required), `label`, `type`,
#'   `length`, `digits`, `format`, `codelist`, `mandatory`, `order`, `role`,
#'   `origin`;
#' * `codelists`: `codelist` (its ID; required), `name`, `type`,
#'   `codelist_code` (the code list's NCI code), `order`, `value` (the term;
#'   required), `code` (the term's NCI code), `decode`, `extended`;
#' * `external` (read when the sheet is there): `codelist`, `name`,
#'   `dictionary`, `version`.
#'
#' @param name The profile: `"p21"`.
#' @return A list of four lists.
#' @examples
#' str(tfl_define_profile()$codelists)
#' @export
tfl_define_profile <- function(name = "p21") {
  name <- match.arg(name)
  list(
    datasets = list(sheet = "Datasets", dataset = "Dataset",
                    label = "Description", class = "Class",
                    structure = "Structure", purpose = "Purpose",
                    keys = "Key Variables", repeating = "Repeating",
                    reference = "Reference Data"),
    variables = list(sheet = "Variables", dataset = "Dataset",
                     variable = "Variable", label = "Label",
                     type = "Data Type", length = "Length",
                     digits = "Significant Digits", format = "Format",
                     codelist = "Codelist", mandatory = "Mandatory",
                     order = "Order", role = "Role", origin = "Origin"),
    codelists = list(sheet = "Codelists", codelist = "ID", name = "Name",
                     type = "Data Type", codelist_code = "NCI Codelist Code",
                     order = "Order", value = "Term", code = "NCI Term Code",
                     decode = "Decoded Value", extended = NA),
    external = list(sheet = "Dictionaries", codelist = "ID", name = "Name",
                    dictionary = "Dictionary", version = "Version"))
}

.define_required <- list(datasets = "dataset",
                         variables = c("dataset", "variable"),
                         codelists = c("codelist", "value"),
                         external = "codelist")

#' Read a define spec workbook (an import profile)
#'
#' Reads the workbook a define is made from -- by default in the Pinnacle 21
#' layout ([tfl_define_profile()]) -- and gives the same `tfl_define` as
#' [tfl_read_define()], so [tfl_define_codelists()] takes either.
#'
#' * Sheet and column names are matched ignoring case and surrounding
#'   blanks.  A sheet the profile names that is not there is an error (but
#'   `external`, read when there); so is a required column.  An optional
#'   column that is not there is read as empty and listed in
#'   `attr(, "unread")` (part, field, column), for the user to see.
#' * A term is `extended` when the profile maps an `extended` column (`Yes`,
#'   `Y`, `TRUE`, `1`); else when its code list has an NCI code and the term
#'   has none (how Pinnacle 21 marks a sponsor's term).
#' * A code list's `kind` is `"item"` when its values have decodes, else
#'   `"enumerated"`.
#' * `order`, `length` and `digits` must be whole numbers.
#'
#' @param path An `.xlsx` workbook (needs \pkg{readxl}).
#' @param map Changes to `profile`: a list of parts, each a list of fields,
#'   e.g. `list(codelists = list(sheet = "CT", decode = "Decode"))`.
#' @param profile The profile: a name for [tfl_define_profile()], or a whole
#'   profile list.
#' @return A `tfl_define` (see [tfl_read_define()]), with
#'   `attr(, "unread")`.
#' @examples
#' if (requireNamespace("readxl", quietly = TRUE)) {
#'   xlsx <- tempfile(fileext = ".xlsx")
#'   openxlsx::write.xlsx(list(
#'     Datasets = data.frame(Dataset = "ADSL",
#'                           Description = "Subject-Level Analysis Dataset"),
#'     Variables = data.frame(Order = 1:2, Dataset = "ADSL",
#'                            Variable = c("SEX", "AGE"),
#'                            Label = c("Sex", "Age"),
#'                            Codelist = c("CL.SEX", NA)),
#'     Codelists = data.frame(ID = "CL.SEX", Name = "Sex", Order = 1:2,
#'                            Term = c("M", "F"),
#'                            `Decoded Value` = c("Male", "Female"),
#'                            check.names = FALSE)), xlsx)
#'   def <- tfl_read_define_spec(xlsx)
#'   def$codelists
#'   attr(def, "unread")
#' }
#' @export
tfl_read_define_spec <- function(path, map = NULL, profile = "p21") {
  fn <- "tfl_read_define_spec()"
  .ard_need("readxl", fn)
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .ard_stop(sprintf("%s: no file '%s'.", fn, path))
  }
  prof <- .define_profile(profile, map, fn)
  have <- readxl::excel_sheets(path)
  part <- function(p) {
    spec <- prof[[p]]
    sh <- have[match(tolower(trimws(spec$sheet)), tolower(trimws(have)))]
    if (is.na(sh)) {
      if (identical(p, "external")) return(NULL)
      .ard_stop(sprintf(paste0("%s: '%s' has no sheet '%s' (the profile's ",
                               "%s); its sheets are %s."),
                        fn, basename(path), spec$sheet, p,
                        paste(sQuote(have), collapse = ", ")))
    }
    d <- as.data.frame(readxl::read_excel(path, sh, col_types = "text",
                                          .name_repair = "minimal"))
    d[] <- lapply(d, function(v) {
      v <- trimws(as.character(v))
      v[!is.na(v) & !nzchar(v)] <- NA_character_
      v
    })
    keep <- rowSums(!is.na(d)) > 0L
    row_no <- which(keep) + 1L   # the sheet's row numbers, under the header
    d <- d[keep, , drop = FALSE]
    fields <- setdiff(names(spec), "sheet")
    out <- list()
    unread <- list()
    for (f in fields) {
      col <- spec[[f]]
      out[[f]] <- rep(NA_character_, nrow(d))
      if (is.null(col) || is.na(col)) next
      hit <- match(tolower(trimws(col)), tolower(trimws(names(d))))
      if (is.na(hit)) {
        if (f %in% .define_required[[p]]) {
          .ard_stop(sprintf(
            paste0("%s: sheet '%s' has no column '%s' (the %s's %s); its ",
                   "columns are %s."),
            fn, sh, col, p, f, paste(sQuote(names(d)), collapse = ", ")))
        }
        unread[[length(unread) + 1L]] <- data.frame(
          part = p, field = f, column = col, stringsAsFactors = FALSE)
        next
      }
      out[[f]] <- d[[hit]]
    }
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    for (f in intersect(c("order", "length", "digits"), fields)) {
      out[[f]] <- .define_whole(out[[f]], sh, spec[[f]], row_no, fn)
    }
    for (f in .define_required[[p]]) {
      miss <- which(is.na(out[[f]]))
      if (length(miss)) {
        .ard_stop(sprintf("%s: sheet '%s', row(s) %s: no %s (column '%s').",
                          fn, sh, paste(row_no[miss], collapse = ", "), f,
                          spec[[f]]))
      }
    }
    attr(out, "unread") <- unread
    out
  }
  ds <- part("datasets")
  vs <- part("variables")
  cl <- part("codelists")
  ex <- part("external")
  unread <- c(attr(ds, "unread"), attr(vs, "unread"), attr(cl, "unread"),
              attr(ex, "unread"))

  datasets <- data.frame(
    dataset = ds$dataset, label = ds$label, class = ds$class,
    subclass = NA_character_, structure = ds$structure,
    purpose = ds$purpose, repeating = ds$repeating,
    reference = ds$reference,
    keys = vapply(ds$keys, .define_keys, "", USE.NAMES = FALSE),
    order = seq_len(nrow(ds)), oid = NA_character_,
    stringsAsFactors = FALSE)
  # variables in the datasets' order, then their own
  vs$.row <- seq_len(nrow(vs))
  vs <- vs[order(match(toupper(vs$dataset), toupper(ds$dataset)), vs$order,
                 vs$.row, na.last = TRUE), , drop = FALSE]
  variables <- data.frame(
    dataset = vs$dataset, variable = vs$variable, label = vs$label,
    type = vs$type, length = vs$length, digits = vs$digits,
    format = vs$format, codelist = vs$codelist, mandatory = vs$mandatory,
    order = vs$order, key = NA_integer_, role = vs$role,
    origin = vs$origin, oid = NA_character_, stringsAsFactors = FALSE)
  # the key sequence from the datasets' key variables
  for (i in seq_len(nrow(datasets))) {
    k <- datasets$keys[i]
    if (is.na(k)) next
    k <- strsplit(k, " | ", fixed = TRUE)[[1L]]
    here <- toupper(variables$dataset) == toupper(datasets$dataset[i])
    variables$key[here] <- match(toupper(variables$variable[here]),
                                 toupper(k))
  }
  rownames(variables) <- NULL

  # a code list's name and NCI code are on its first row in some layouts
  fill <- function(v) {
    first <- tapply(v, cl$codelist, function(x) x[!is.na(x)][1L])
    ifelse(is.na(v), unname(first[cl$codelist]), v)
  }
  cl$name <- fill(cl$name)
  cl$type <- fill(cl$type)
  cl$codelist_code <- fill(cl$codelist_code)
  has_decode <- tapply(!is.na(cl$decode), cl$codelist, any)
  extended <- if (!is.null(prof$codelists$extended) &&
                  !is.na(prof$codelists$extended) &&
                  !"extended" %in% vapply(unread, `[[`, "", "field")) {
    .define_yes(cl$extended)
  } else {
    !is.na(cl$codelist_code) & is.na(cl$code)
  }
  codelists <- data.frame(
    codelist = cl$codelist, name = cl$name, type = cl$type,
    value = cl$value, decode = cl$decode, order = cl$order,
    rank = NA_real_, extended = extended, code = cl$code,
    kind = ifelse(has_decode[cl$codelist], "item", "enumerated"),
    stringsAsFactors = FALSE)
  rownames(codelists) <- NULL
  external <- if (is.null(ex)) {
    data.frame(codelist = character(), name = character(),
               dictionary = character(), version = character(),
               stringsAsFactors = FALSE)
  } else {
    ex[c("codelist", "name", "dictionary", "version")]
  }
  study <- list(study_name = NA_character_, description = NA_character_,
                protocol = NA_character_, define_version = NA_character_,
                file_oid = NA_character_)
  out <- .define_new(study, datasets, variables, codelists, external,
                     source = list(file = path, kind = "define spec",
                                   version = NA_character_))
  attr(out, "unread") <- do.call(rbind, c(
    list(data.frame(part = character(), field = character(),
                    column = character(), stringsAsFactors = FALSE)),
    unread))
  out
}

# The profile with `map`'s changes, checked.
.define_profile <- function(profile, map, fn) {
  prof <- if (is.character(profile)) tfl_define_profile(profile) else profile
  if (!is.list(prof)) {
    .ard_stop(sprintf("%s: `profile` is a name or a list.", fn))
  }
  if (!is.null(map)) {
    if (!is.list(map) || is.null(names(map))) {
      .ard_stop(sprintf(
        paste0("%s: `map` is a list of parts: ",
               "list(variables = list(label = \"...\"))."), fn))
    }
    prof <- utils::modifyList(prof, map)
  }
  known <- tfl_define_profile()
  bad <- setdiff(names(prof), names(known))
  if (length(bad)) {
    .ard_stop(sprintf("%s: the profile has no part %s; the parts are %s.",
                      fn, paste(sQuote(bad), collapse = ", "),
                      paste(names(known), collapse = ", ")))
  }
  for (p in names(known)) {
    if (is.null(prof[[p]])) prof[[p]] <- known[[p]]
    bad <- setdiff(names(prof[[p]]), names(known[[p]]))
    if (length(bad)) {
      .ard_stop(sprintf(paste0("%s: the profile's %s has no field %s; ",
                               "the fields are %s."),
                        fn, p, paste(sQuote(bad), collapse = ", "),
                        paste(names(known[[p]]), collapse = ", ")))
    }
    for (f in c("sheet", .define_required[[p]])) {
      v <- prof[[p]][[f]]
      if (is.null(v) || length(v) != 1L || is.na(v) || !nzchar(v)) {
        .ard_stop(sprintf("%s: the profile's %s must name its `%s`.", fn, p,
                          f))
      }
    }
  }
  prof
}

.define_whole <- function(x, sheet, col, row_no, fn) {
  n <- suppressWarnings(as.numeric(x))
  bad <- which(!is.na(x) & (is.na(n) | n != round(n)))
  if (length(bad)) {
    .ard_stop(sprintf(
      "%s: sheet '%s', column '%s', row(s) %s: not a whole number (%s).", fn,
      sheet, col, paste(row_no[bad], collapse = ", "),
      paste(sQuote(x[bad]), collapse = ", ")))
  }
  as.integer(n)
}

# "USUBJID, AETERM" / "USUBJID AETERM" / one a line -> "USUBJID | AETERM"
.define_keys <- function(x) {
  if (is.na(x)) return(NA_character_)
  k <- strsplit(x, "[,;|[:space:]]+")[[1L]]
  k <- k[nzchar(k)]
  if (!length(k)) NA_character_ else paste(k, collapse = " | ")
}

# ----------------------------------------------------------------------------
#  The define's code lists as a report's `codelists` rows
# ----------------------------------------------------------------------------

#' A define's code lists as a report's `codelists` rows
#'
#' Turns the code lists of the variables a report uses into rows of the
#' spec's `codelists` sheet: `value` the coded value, `label` its decode
#' (blank for an enumerated value: it prints as itself), `order` its order
#' number.  With `labels`, the variables' labels too, as the code list of
#' `variable` (its values the variables' names): the spec's way of giving a
#' table its variables' labels.  The rows are a draft: nothing is written.
#'
#' Code lists are per report: every row carries an `output_id`, so it is
#' required.  Several output ids give the rows once for each.
#'
#' A variable is looked up in the datasets given (all when `datasets` is
#' `NULL`).  When it is in several with different code lists (`PARAMCD` of
#' ADVS and of ADLB) that is an error naming them: give `datasets`.  What
#' cannot be taken from the define is listed in `attr(, "set_by_hand")`
#' (`variable`, `reason`): a variable it does not have (also a warning), one
#' with no code list, one whose code list is an external dictionary
#' (MedDRA ...).  Each row's `note` says where it came from (`define:
#' CL.SEX`; `extended` for a sponsor's value), and is never read by the
#' spec.
#'
#' @param define A `tfl_define` ([tfl_read_define()],
#'   [tfl_read_define_spec()]), or the path of a `define.xml` or of a define
#'   spec `.xlsx` (read with the Pinnacle 21 profile).
#' @param variables The variables (names, matched ignoring case); `NULL`:
#'   every variable of `datasets` that has a code list.
#' @param output_id The report(s) the rows are for (required).
#' @param datasets The datasets to look in; `NULL` for all.
#' @param labels Add the variables' labels (the code list of `variable`).
#' @return A data frame of the `codelists` sheet's columns, all character:
#'   `output_id`, `variable`, `value`, `label`, `order`, `note`; with
#'   `attr(, "set_by_hand")`.
#' @examples
#' if (requireNamespace("xml2", quietly = TRUE)) {
#'   def <- tfl_read_define(system.file("extdata", "define", "define.xml",
#'                                      package = "tflspec"))
#'   tfl_define_codelists(def, c("SEX", "AGEGR1", "AGE"),
#'                        output_id = "T-14-1-1", datasets = "ADSL")
#' }
#' @export
tfl_define_codelists <- function(define, variables = NULL, output_id,
                                 datasets = NULL, labels = TRUE) {
  fn <- "tfl_define_codelists()"
  if (missing(output_id) || !is.character(output_id) ||
      !length(output_id) || anyNA(output_id) ||
      any(!nzchar(trimws(output_id)))) {
    .ard_stop(sprintf(paste0(
      "%s: `output_id` is required -- code lists are per report: ",
      "output_id = \"T-14-1-1\"."), fn))
  }
  define <- .define_get(define, fn)
  v <- define$variables
  if (!is.null(datasets)) {
    miss <- setdiff(toupper(datasets), toupper(define$datasets$dataset))
    if (length(miss)) {
      .ard_stop(sprintf("%s: the define has no dataset %s; it has %s.", fn,
                        paste(sQuote(miss), collapse = ", "),
                        paste(define$datasets$dataset, collapse = ", ")))
    }
    v <- v[toupper(v$dataset) %in% toupper(datasets), , drop = FALSE]
  }
  if (is.null(variables)) {
    variables <- unique(v$variable[!is.na(v$codelist)])
  }
  cl <- define$codelists
  hand_var <- hand_why <- character()
  rows <- list()
  lab_rows <- list()
  for (want in unique(variables)) {
    here <- v[toupper(v$variable) == toupper(want), , drop = FALSE]
    if (!nrow(here)) {
      hand_var <- c(hand_var, want)
      hand_why <- c(hand_why, "not in the define")
      next
    }
    var <- here$variable[[1L]]
    if (labels) {
      lab <- here$label[!is.na(here$label)]
      if (length(lab)) {
        lab_ds <- here$dataset[!is.na(here$label)]
        lab_rows[[length(lab_rows) + 1L]] <- data.frame(
          variable = "variable", value = var, label = lab[[1L]],
          order = NA_character_,
          note = sprintf("define: %s.%s", lab_ds[[1L]], var),
          stringsAsFactors = FALSE)
        if (length(unique(lab)) > 1L) {
          hand_var <- c(hand_var, var)
          hand_why <- c(hand_why, sprintf(
            "its label differs between datasets (%s); the first taken",
            paste(sprintf("%s: %s", lab_ds, sQuote(lab)), collapse = ", ")))
        }
      } else {
        hand_var <- c(hand_var, var)
        hand_why <- c(hand_why, "no label in the define")
      }
    }
    oids <- unique(here$codelist[!is.na(here$codelist)])
    if (length(oids) > 1L) {
      .ard_stop(sprintf(paste0(
        "%s: %s has different code lists in different datasets (%s); ",
        "give `datasets` to say which."), fn, var,
        paste(sprintf("%s: %s", here$dataset, here$codelist)[
          !is.na(here$codelist)], collapse = ", ")))
    }
    if (!length(oids)) {
      hand_var <- c(hand_var, var)
      hand_why <- c(hand_why, "no code list in the define")
      next
    }
    vals <- cl[cl$codelist == oids, , drop = FALSE]
    if (!nrow(vals)) {
      ex <- define$external[define$external$codelist == oids, , drop = FALSE]
      hand_var <- c(hand_var, var)
      hand_why <- c(hand_why, if (nrow(ex)) {
        sprintf("its code list %s is an external dictionary (%s%s)", oids,
                ex$dictionary[[1L]],
                if (is.na(ex$version[[1L]])) "" else
                  paste0(" ", ex$version[[1L]]))
      } else {
        sprintf("its code list %s has no values in the define", oids)
      })
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(
      variable = var, value = vals$value,
      label = ifelse(is.na(vals$decode) | vals$decode == vals$value,
                     NA_character_, vals$decode),
      order = ifelse(is.na(vals$order), NA_character_,
                     as.character(vals$order)),
      note = paste0("define: ", oids,
                    ifelse(vals$extended, ", extended", "")),
      stringsAsFactors = FALSE)
  }
  one <- do.call(rbind, c(list(data.frame(
    variable = character(), value = character(), label = character(),
    order = character(), note = character(), stringsAsFactors = FALSE)),
    rows, lab_rows))
  out <- do.call(rbind, lapply(output_id, function(id) {
    cbind(data.frame(output_id = rep(id, nrow(one)),
                     stringsAsFactors = FALSE), one)
  }))
  rownames(out) <- NULL
  attr(out, "set_by_hand") <- data.frame(
    variable = hand_var, reason = hand_why, stringsAsFactors = FALSE)
  nf <- hand_var[hand_why == "not in the define"]
  if (length(nf)) {
    warning(sprintf("%s: not in the define: %s.", fn,
                    paste(nf, collapse = ", ")), call. = FALSE)
  }
  out
}

# A tfl_define, or one read from a path.
.define_get <- function(define, fn) {
  if (inherits(define, "tfl_define")) return(define)
  if (is.character(define) && length(define) == 1L) {
    ext <- tolower(tools::file_ext(define))
    if (ext == "xml") return(tfl_read_define(define))
    if (ext %in% c("xlsx", "xlsm", "xls")) return(tfl_read_define_spec(define))
    .ard_stop(sprintf("%s: a define is a .xml or an .xlsx; got '%s'.", fn,
                      define))
  }
  .ard_stop(sprintf(paste0(
    "%s: `define` is what tfl_read_define() or tfl_read_define_spec() ",
    "gives, or a file; got %s."), fn, class(define)[[1L]]))
}
