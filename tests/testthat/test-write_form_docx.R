# The Word authoring form is half of a contract: everything asserted here is
# something `read_form_docx()` (step 3) will rely on. The structural checks go
# through the raw WML rather than through officer's summary whenever the
# stricter statement is about elements (a `w:vMerge` restart with no
# continuation is invisible to `docx_summary()` but fatal to the parser).

skip_if_no_docx <- function() {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
}

# One row per Word paragraph of a table cell, columns taken BY NAME: officer
# added `para_id` between 0.7.6 and 0.7.7, so any positional access would
# break on the next release.
form_cells <- function(path) {
  s <- officer::docx_summary(officer::read_docx(path))
  stopifnot(all(c("content_type", "text", "table_index", "row_id", "cell_id",
                  "col_span", "row_span") %in% names(s)))
  s[s$content_type == "table cell", , drop = FALSE]
}

# The key column of one block, hints dropped: the reader only ever reads the
# first line of a key cell.
form_keys <- function(cells, table_index) {
  b <- cells[cells$table_index == table_index & cells$cell_id == 1L, ,
             drop = FALSE]
  b <- b[order(b$doc_index), , drop = FALSE]
  sub("\n.*$", "", b$text)
}

form_values <- function(cells, table_index) {
  b <- cells[cells$table_index == table_index & cells$cell_id == 2L, ,
             drop = FALSE]
  b <- b[order(b$doc_index), , drop = FALSE]
  b$text
}

form_document_xml <- function(path) {
  dir <- tempfile()
  dir.create(dir)
  utils::unzip(path, files = "word/document.xml", exdir = dir)
  xml2::read_xml(file.path(dir, "word", "document.xml"))
}

one_lang_spec <- function(kinds) {
  lss_example_spec(kinds = kinds, lang = "fr")
}

# The rows a Question block must carry, derived from `lss_kinds` alone -- the
# same rule the renderer implements, restated independently so a drift in
# either one fails the test.
expected_keys <- function(kind, chrome) {
  row <- lss_kinds[match(kind, lss_kinds$kind), , drop = FALSE]
  keys <- chrome$meta_type
  if (isTRUE(row$collects_response)) keys <- c(keys, chrome$meta_mandatory)
  keys <- c(keys, chrome$meta_filter, chrome$form_wording, chrome$item_help)
  if (row$options == "required") keys <- c(keys, chrome$item_options)
  if (isTRUE(row$exclusive_allowed)) keys <- c(keys, chrome$item_exclusive)
  if (row$rows == "required") keys <- c(keys, chrome$form_rows)
  if (row$columns == "required") keys <- c(keys, chrome$form_columns)
  if (row$max_answers_rule != "none") {
    keys <- c(keys, chrome$form_min_answers, chrome$form_max_answers)
  }
  if (isTRUE(row$other_allowed)) keys <- c(keys, chrome$form_other_position)
  keys
}


test_that("lss_template_docx() writes a document with one table per block", {
  # Sweeps the whole kind table, or the five chrome languages, through a
  # real Word document. Exhaustive on purpose, and expensive: it runs
  # locally and in CI, where nothing is on a ten-minute budget.
  skip_on_cran()
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)

  expect_invisible(lss_template_docx(path, lang = "fr"))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 5000)

  spec <- lss_example_spec(lang = "fr")
  n_questions <- sum(vapply(spec$groups, function(g) length(g$questions),
                            integer(1)))
  cells <- form_cells(path)
  expect_identical(
    length(unique(cells$table_index)),
    1L + length(spec$groups) + n_questions + length(spec$quotas)
  )
  # every block has exactly two columns and no header part
  expect_setequal(unique(cells$cell_id), c(1L, 2L))
  expect_false(any(cells$is_header))
})

test_that("the first table is the Survey block, in the chrome language", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  lss_template_docx(path, lang = "fr", kinds = c("single", "text"))

  cells <- form_cells(path)
  first <- min(cells$table_index)
  keys <- form_keys(cells, first)
  chrome <- lss_chrome_strings("fr")
  expect_identical(keys[[1L]], chrome$form_block_survey)
  expect_identical(
    keys,
    c(chrome$form_block_survey, chrome$form_title, chrome$cover_languages,
      chrome$welcome_text_title, chrome$end_text_title)
  )
  # the title row's value cell stays empty; the Languages cell carries the
  # declared codes
  values <- form_values(cells, first)
  expect_identical(values[[1L]], "")
  expect_identical(values[[3L]], "fr")
})

test_that("a Question block carries exactly the rows its kind implies", {
  # Nine kinds through a real Word document, one more kind sweep: local
  # and CI keep it, the CRAN budget does not.
  skip_on_cran()
  skip_if_no_docx()
  chrome <- lss_chrome_strings("fr")
  kinds <- c("single", "multiple", "array", "array5", "ranking", "display",
             "dropdown", "yesno", "multitext")
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  spec <- one_lang_spec(kinds)
  write_form_docx(spec, path, lang = "fr")

  cells <- form_cells(path)
  tables <- sort(unique(cells$table_index))
  questions <- unlist(lapply(spec$groups, `[[`, "questions"), recursive = FALSE)
  # 1 survey + per group (1 group block + its questions) + quotas, in order
  q_tables <- tables[vapply(tables, function(ti) {
    identical(form_keys(cells, ti)[[1L]], chrome$form_block_question)
  }, logical(1))]
  expect_identical(length(q_tables), length(questions))

  for (i in seq_along(questions)) {
    q <- questions[[i]]
    keys <- form_keys(cells, q_tables[[i]])
    expect_identical(
      keys, c(chrome$form_block_question, expected_keys(q$kind, chrome)),
      info = paste("kind", q$kind)
    )
    # the question code lives in the value cell of the title row
    expect_identical(form_values(cells, q_tables[[i]])[[1L]], q$code)
  }
})

test_that("every kind of the table gets its rows, and only those", {
  # Sweeps the whole kind table, or the five chrome languages, through a
  # real Word document. Exhaustive on purpose, and expensive: it runs
  # locally and in CI, where nothing is on a ten-minute budget.
  skip_on_cran()
  skip_if_no_docx()
  chrome <- lss_chrome_strings("en")
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  spec <- lss_example_spec(lang = "en")
  write_form_docx(spec, path, lang = "en")

  cells <- form_cells(path)
  tables <- sort(unique(cells$table_index))
  questions <- unlist(lapply(spec$groups, `[[`, "questions"), recursive = FALSE)
  q_tables <- tables[vapply(tables, function(ti) {
    identical(form_keys(cells, ti)[[1L]], chrome$form_block_question)
  }, logical(1))]
  expect_identical(length(q_tables), nrow(lss_kinds))
  for (i in seq_along(questions)) {
    expect_identical(
      form_keys(cells, q_tables[[i]])[-1L],
      expected_keys(questions[[i]]$kind, chrome),
      info = paste("kind", questions[[i]]$kind)
    )
  }
})

test_that("option, exclusive, cap and other-position values follow the contract", {
  skip_if_no_docx()
  chrome <- lss_chrome_strings("fr")
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  # blocks follow the order of `lss_kinds`: single, multiple, array, ranking
  spec <- one_lang_spec(c("single", "multiple", "ranking", "array"))
  write_form_docx(spec, path, lang = "fr")
  cells <- form_cells(path)
  tables <- sort(unique(cells$table_index))
  q_tables <- tables[vapply(tables, function(ti) {
    identical(form_keys(cells, ti)[[1L]], chrome$form_block_question)
  }, logical(1))]
  block <- function(i) {
    k <- form_keys(cells, q_tables[[i]])
    v <- form_values(cells, q_tables[[i]])
    stats::setNames(v, k)
  }

  single <- block(1L)
  # every coded option is written `code = label`; the native other option is
  # written with the reserved word of the chrome language
  expect_match(single[[chrome$item_options]], "^1 = ")
  expect_match(single[[chrome$item_options]], "\nAutre = ")
  expect_identical(single[[chrome$form_other_position]],
                   chrome$form_other_position_end)
  expect_identical(single[[chrome$meta_mandatory]], chrome$mandatory_yes)
  # Type carries the spec kind, not the many-to-one review label
  expect_identical(single[[chrome$meta_type]], "single")

  multiple <- block(2L)
  expect_identical(multiple[[chrome$item_exclusive]], "4")
  expect_identical(multiple[[chrome$form_max_answers]], "3")
  expect_identical(multiple[[chrome$form_min_answers]], "")
  expect_identical(multiple[[chrome$form_other_position]],
                   sprintf(chrome$form_other_position_after_fmt, "3"))

  ranking <- block(4L)
  expect_identical(ranking[[chrome$form_min_answers]], "3")
  expect_identical(ranking[[chrome$form_max_answers]], "3")

  array <- block(3L)
  expect_match(array[[chrome$form_rows]], "^1 = ")
  expect_match(array[[chrome$form_columns]], "^1 = ")
  # several values in one cell are separated by soft returns
  expect_identical(length(strsplit(array[[chrome$form_columns]], "\n")[[1L]]), 4L)
})

test_that("no cell is ever merged and every table has two grid columns", {
  # Sweeps the whole kind table, or the five chrome languages, through a
  # real Word document. Exhaustive on purpose, and expensive: it runs
  # locally and in CI, where nothing is on a ten-minute budget.
  skip_on_cran()
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  lss_template_docx(path, lang = "fr")

  doc <- form_document_xml(path)
  ns <- xml2::xml_ns(doc)
  tables <- xml2::xml_find_all(doc, "/w:document/w:body/w:tbl", ns)
  expect_gt(length(tables), 1L)
  for (tbl in tables) {
    expect_identical(
      length(xml2::xml_find_all(tbl, ".//w:tblGrid/w:gridCol", ns)), 2L)
    expect_identical(length(xml2::xml_find_all(tbl, ".//w:vMerge", ns)), 0L)
    expect_identical(length(xml2::xml_find_all(tbl, ".//w:hMerge", ns)), 0L)
    expect_identical(length(xml2::xml_find_all(tbl, ".//w:tbl", ns)), 0L)
    spans <- xml2::xml_attr(
      xml2::xml_find_all(tbl, ".//w:gridSpan", ns), "val", ns)
    expect_true(all(is.na(spans) | spans == "1"))
    for (tr in xml2::xml_find_all(tbl, "w:tr", ns)) {
      expect_identical(length(xml2::xml_find_all(tr, "w:tc", ns)), 2L)
    }
  }
})

test_that("the template carries the lssdoc-template-version document property", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  lss_template_docx(path, lang = "fr", kinds = "single")

  props <- officer::doc_properties(officer::read_docx(path))
  expect_true("lssdoc-template-version" %in% props$tag)
  expect_identical(props$value[props$tag == "lssdoc-template-version"], "1")
  expect_identical(props$value[props$tag == "lssdoc-template-version"],
                   as.character(LSS_FORM_VERSION))
  expect_identical(props$value[props$tag == "lssdoc-chrome-lang"], "fr")
  # the property must also be readable without officer, the way the reader
  # will do it
  dir <- tempfile()
  dir.create(dir)
  utils::unzip(path, files = "docProps/custom.xml", exdir = dir)
  x <- xml2::read_xml(file.path(dir, "docProps", "custom.xml"))
  node <- xml2::xml_find_first(
    x, "//*[local-name()='property'][@name='lssdoc-template-version']")
  expect_identical(as.integer(trimws(xml2::xml_text(node))), LSS_FORM_VERSION)
})

test_that("the form renders in every chrome language with its own labels", {
  # Sweeps the whole kind table, or the five chrome languages, through a
  # real Word document. Exhaustive on purpose, and expensive: it runs
  # locally and in CI, where nothing is on a ten-minute budget.
  skip_on_cran()
  skip_if_no_docx()
  for (lang in c("en", "fr", "de", "es", "it")) {
    path <- tempfile(fileext = ".docx")
    lss_template_docx(path, lang = lang, kinds = c("single", "array"))
    chrome <- lss_chrome_strings(lang)
    cells <- form_cells(path)
    keys <- unlist(lapply(sort(unique(cells$table_index)), function(ti) {
      form_keys(cells, ti)
    }))
    expect_true(chrome$form_block_survey %in% keys, info = lang)
    expect_true(chrome$form_block_group %in% keys, info = lang)
    expect_true(chrome$form_block_question %in% keys, info = lang)
    expect_true(chrome$form_block_quota %in% keys, info = lang)
    expect_true(chrome$form_rows %in% keys, info = lang)
    unlink(path)
  }
})

test_that("the language suffix appears only when several languages are declared", {
  skip_if_no_docx()
  chrome <- lss_chrome_strings("fr")
  mono <- tempfile(fileext = ".docx")
  bi <- tempfile(fileext = ".docx")
  on.exit(unlink(c(mono, bi)), add = TRUE)

  one <- lss_spec(
    title = "Titre", languages = "fr",
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "yesno", text = "Oui ou non ?")))))
  two <- lss_spec(
    title = c(fr = "Titre", en = "Title"), languages = c("fr", "en"),
    groups = list(list(title = c(fr = "G", en = "G"), questions = list(
      list(code = "q1", kind = "yesno",
           text = c(fr = "Oui ou non ?", en = "Yes or no?"))))))

  write_form_docx(one, mono, lang = "fr")
  write_form_docx(two, bi, lang = "fr")

  keys_of <- function(path) {
    cells <- form_cells(path)
    unlist(lapply(sort(unique(cells$table_index)),
                  function(ti) form_keys(cells, ti)))
  }
  km <- keys_of(mono)
  expect_true(chrome$form_wording %in% km)
  expect_false(any(grepl("\\[", km)))

  kb <- keys_of(bi)
  # every localizable row is written once per language, primary first, and
  # ALL of them carry the suffix, the primary one included
  expect_true(paste0(chrome$form_wording, " [fr]") %in% kb)
  expect_true(paste0(chrome$form_wording, " [en]") %in% kb)
  expect_false(chrome$form_wording %in% kb)
  expect_false(chrome$form_title %in% kb)
  # non-localizable rows are never suffixed
  expect_true(chrome$meta_type %in% kb)
  expect_true(chrome$cover_languages %in% kb)
  expect_lt(which(kb == paste0(chrome$form_wording, " [fr]")),
            which(kb == paste0(chrome$form_wording, " [en]")))
})

test_that("a line break in a single-line field is refused, naming the field", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)

  bad_option <- lss_spec(
    title = "Titre", languages = "fr",
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "single", text = "Question ?",
           options = list(list(text = "Oui\nvraiment"), list(text = "Non")))))))
  expect_error(write_form_docx(bad_option, path, lang = "fr"),
               class = "lssdoc_bad_form_value")
  expect_error(write_form_docx(bad_option, path, lang = "fr"),
               regexp = "q1")
  expect_error(write_form_docx(bad_option, path, lang = "fr"),
               regexp = "Options")

  bad_title <- lss_spec(
    title = "Deux\nlignes", languages = "fr",
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "yesno", text = "Oui ou non ?")))))
  expect_error(write_form_docx(bad_title, path, lang = "fr"),
               class = "lssdoc_bad_form_value")

  bad_group <- lss_spec(
    title = "Titre", languages = "fr",
    groups = list(list(title = "G\nH", questions = list(
      list(code = "q1", kind = "yesno", text = "Oui ou non ?")))))
  expect_error(write_form_docx(bad_group, path, lang = "fr"),
               class = "lssdoc_bad_form_value")

  # a multi-line question text, by contrast, is legitimate: it is written as
  # several lines of one cell
  ok <- lss_spec(
    title = "Titre", languages = "fr",
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "yesno", text = "Premiere ligne\nSeconde ligne")))))
  expect_invisible(write_form_docx(ok, path, lang = "fr"))
  cells <- form_cells(path)
  txt <- paste(cells$text, collapse = " | ")
  expect_true(grepl("Premiere ligne\nSeconde ligne", txt, fixed = TRUE))
})

test_that("lss_template_docx() writes a template for all 21 kinds", {
  # Sweeps the whole kind table, or the five chrome languages, through a
  # real Word document. Exhaustive on purpose, and expensive: it runs
  # locally and in CI, where nothing is on a ten-minute budget.
  skip_on_cran()
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  lss_template_docx(path)
  cells <- form_cells(path)
  chrome <- lss_chrome_strings("fr")
  n_blocks <- length(unique(cells$table_index))
  # 1 survey + 2 groups + 21 questions + 1 quota
  expect_identical(n_blocks, 1L + 2L + nrow(lss_kinds) + 1L)
  # every kind name appears as a Type value
  values <- unlist(lapply(sort(unique(cells$table_index)),
                          function(ti) form_values(cells, ti)))
  expect_true(all(lss_kinds$kind %in% values))
  # the blank template carries its hints, in the KEY cell, after a soft
  # return, so the reader's "first line" rule discards them
  keys_raw <- cells$text[cells$cell_id == 1L]
  expect_true(any(grepl(paste0("^", chrome$item_options, "\n"), keys_raw)))
})

test_that("write_form_docx() validates its arguments", {
  skip_if_no_docx()
  spec <- lss_example_spec(kinds = "yesno")
  expect_error(write_form_docx(spec, tempfile(), lang = "ja"),
               class = "lssdoc_bad_chrome_lang")
  expect_error(write_form_docx(spec, 1L), class = "lssdoc_bad_path")
  expect_error(write_form_docx("not a spec", tempfile()),
               class = "lssdoc_bad_spec")
  expect_error(lss_template_docx(tempfile(), kinds = "nosuchkind"),
               class = "lssdoc_bad_spec")
})

test_that("the key column of a Question block is stable", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  spec <- one_lang_spec("multiple")
  write_form_docx(spec, path, lang = "fr")
  cells <- form_cells(path)
  chrome <- lss_chrome_strings("fr")
  tables <- sort(unique(cells$table_index))
  ti <- tables[vapply(tables, function(i) {
    identical(form_keys(cells, i)[[1L]], chrome$form_block_question)
  }, logical(1))][[1L]]
  expect_snapshot(cat(form_keys(cells, ti), sep = "\n"))
})

test_that("the other option is written with the reserved word of the form", {
  skip_if_no_docx()
  chrome <- lss_chrome_strings("fr")
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  # One rule, language-independent: the bare reserved word when the spec gives
  # NO label, `<word> = <label>` as soon as it gives one -- including a label
  # that happens to read "Autre" or "Other", which is content like any other
  # and must survive the round-trip unchanged.
  spec <- lss_spec(
    title = c(fr = "Titre", en = "Title"), languages = c("fr", "en"),
    groups = list(list(title = c(fr = "G", en = "G"), questions = list(
      list(code = "q1", kind = "single",
           text = c(fr = "Oui ou non ?", en = "Yes or no?"),
           options = list(list(text = c(fr = "Oui", en = "Yes")),
                          list(text = c(fr = "Autre", en = "Other"),
                               other = TRUE))),
      list(code = "q2", kind = "single",
           text = c(fr = "Et ensuite ?", en = "And then?"),
           options = list(list(text = c(fr = "Oui", en = "Yes")),
                          list(text = c(fr = "Autre chose", en = "Something else"),
                               other = TRUE)))))))
  write_form_docx(spec, path, lang = "fr")
  cells <- form_cells(path)
  values <- unlist(lapply(sort(unique(cells$table_index)),
                          function(ti) form_values(cells, ti)))
  # a label is never dropped, even when it reads like the reserved word
  expect_true(any(values == paste0("1 = Oui\n", chrome$form_other, " = Autre")))
  expect_true(any(values == paste0("1 = Yes\n", chrome$form_other, " = Other")))
  expect_true(any(values == paste0("1 = Oui\n", chrome$form_other, " = Autre chose")))
  expect_true(any(values == paste0("1 = Yes\n", chrome$form_other,
                                   " = Something else")))
  expect_false(any(grepl(paste0(chrome$form_other, " = $"), values)))

  # No label at all: the bare reserved word, and never an empty right-hand
  # side. `lss_spec()` refuses an option without text, so this case cannot
  # come from a validated spec -- it is the shape the form itself accepts
  # (an author who types just "Autre") and the writer must stay symmetrical
  # with the reader on it.
  bare <- form_option_lines(
    list(list(code = "1", text = list(fr = "Oui")), list(other = TRUE)),
    "fr", chrome, "question q1", chrome$item_options)
  expect_identical(bare, c("1 = Oui", chrome$form_other))
})

test_that("lss_other_keywords() covers the five chrome languages", {
  # read-side vocabulary: the writer always writes the reserved word of the
  # chrome language, `read_form_docx()` (step 3) accepts any of these
  keywords <- lss_other_keywords()
  for (lang in c("en", "fr", "de", "es", "it")) {
    expect_true(tolower(lss_chrome_strings(lang)$form_other) %in% keywords,
                info = lang)
  }
  expect_true("autre" %in% keywords)
  expect_true("-oth-" %in% keywords)
})

test_that("no key row of a Question block repeats a block-title word", {
  # A title-row word is how `read_form_docx()` tells one block from the next,
  # so a key row carrying it would read as the start of a new block. Checked
  # on the rows rather than on a rendered file, which covers all 21 kinds in
  # all five languages for the price of one render.
  for (lang in c("en", "fr", "de", "es", "it")) {
    chrome <- lss_chrome_strings(lang)
    words <- tolower(unlist(chrome[c(
      "form_block_survey", "form_block_group", "form_block_question",
      "form_block_quota")]))
    spec <- lss_example_spec(lang = if (identical(lang, "fr")) "fr" else "en")
    questions <- unlist(lapply(spec$groups, `[[`, "questions"),
                        recursive = FALSE)
    for (q in questions) {
      rows <- form_rows_for_question(q, lss_kinds, chrome, spec$languages,
                                     hints = TRUE)
      keys <- tolower(vapply(rows, `[[`, character(1), "key"))
      expect_identical(intersect(keys, words), character(0),
                       info = paste(lang, q$kind))
      # and the wording row is there under its own label
      expect_true(tolower(chrome$form_wording) %in% keys,
                  info = paste(lang, q$kind))
    }
  }
})

test_that("an lss object goes through as_lss_spec(), an empty one is refused", {
  skip_if_no_docx()
  # an `lss` is a list too: without the class guard it would be re-validated
  # as a spec and fail with a field-level message. Since step 3b it is
  # CONVERTED instead, so an unusable one fails as an unusable survey.
  fake <- structure(list(languages = "fr", groups = NULL), class = "lss")
  expect_error(write_form_docx(fake, tempfile(fileext = ".docx")),
               class = "lssdoc_bad_lss")
  expect_error(write_form_docx(fake, tempfile(fileext = ".docx")),
               class = "lssdoc_error")
})

test_that("another classed object is still refused as an unsupported input", {
  skip_if_no_docx()
  audit <- structure(list(findings = list()), class = "lss_audit")
  expect_error(write_form_docx(audit, tempfile(fileext = ".docx")),
               class = "lssdoc_unsupported_input")
  expect_error(write_form_docx(audit, tempfile(fileext = ".docx")),
               class = "lssdoc_error")
  expect_error(write_form_docx(audit, tempfile(fileext = ".docx")),
               regexp = "lss_spec")
})

test_that("a tab becomes one space and a carriage return never survives", {
  skip_if_no_docx()
  expect_identical(lss_form_sanitize("a\tb"), "a b")
  expect_identical(lss_form_sanitize("a\rb"), "ab")
  expect_identical(lss_form_sanitize(c("a\t\tb", "c")), c("a  b", "c"))

  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  spec <- lss_spec(
    title = "Titre", languages = "fr",
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "single", text = "Question ?",
           options = list(list(text = "Oui\tvraiment"),
                          list(text = "Non")))))))
  expect_invisible(write_form_docx(spec, path, lang = "fr"))
  cells <- form_cells(path)
  expect_true(any(grepl("1 = Oui vraiment", cells$text, fixed = TRUE)))
  expect_false(any(grepl("\t", cells$text, fixed = TRUE)))
  expect_false(any(grepl("\r", cells$text, fixed = TRUE)))
})

test_that("every table is preceded by two paragraphs and the body ends with sectPr", {
  skip_if_no_docx()
  # Word merges two tables that touch into one: the pair of empty paragraphs
  # between two blocks is structural, and the reader keys on it.
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  lss_template_docx(path, lang = "fr", kinds = c("single", "array", "text"))

  doc <- form_document_xml(path)
  ns <- xml2::xml_ns(doc)
  body <- xml2::xml_find_first(doc, "/w:document/w:body", ns)
  kids <- xml2::xml_children(body)
  names_kids <- xml2::xml_name(kids)
  tbl <- which(names_kids == "tbl")
  expect_gt(length(tbl), 1L)
  expect_true(all(tbl > 2L))
  expect_identical(unique(names_kids[tbl - 1L]), "p")
  expect_identical(unique(names_kids[tbl - 2L]), "p")
  # the paragraph just before a table is always empty; the one before that is
  # empty too, except ahead of the first block, where it is the document title
  expect_identical(unique(xml2::xml_text(kids[tbl - 1L])), "")
  expect_identical(unique(xml2::xml_text(kids[tbl[-1L] - 2L])), "")
  # nothing between two tables but those two paragraphs
  expect_false(any(diff(tbl) < 3L))
  expect_identical(names_kids[[length(names_kids)]], "sectPr")
})

test_that("lss_template_docx() writes a multilingual blank form", {
  # Two blank templates written and read back: local and CI keep it.
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  out <- tempfile(fileext = ".docx")
  lss_template_docx(out, lang = "fr", languages = c("fr", "en"),
                    kinds = c("single", "text"))
  expect_true(file.exists(out))

  back <- read_form_docx(out)
  expect_identical(back$languages, c("fr", "en"))
  # Every text carries both languages: the reader refuses a missing one.
  q <- back$groups[[1]]$questions[[1]]
  expect_setequal(names(q$text), c("fr", "en"))

  # The chrome language stays independent of the content languages.
  en_form <- tempfile(fileext = ".docx")
  lss_template_docx(en_form, lang = "en", languages = c("fr", "en"),
                    kinds = "single")
  expect_identical(read_form_docx(en_form)$languages, c("fr", "en"))
})
