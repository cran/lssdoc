# The reader is the other half of the form contract: what `write_form_docx()`
# writes, `read_form_docx()` must read back as the same specification. The
# round trips below are therefore the real test -- everything else is a
# refusal that must stay precise -- and they compare CANONICALIZED specs
# (`canonical_spec()`), not raw objects, because the contract carries a survey,
# not an R field order.
#
# The reader itself needs only xml2 and utils::unzip. officer and flextable
# appear here to GENERATE fixtures (and, once, to build a document the reader
# must refuse), never to read one.

skip_if_no_docx <- function() {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
}

# Build an arbitrary form document with the writer's own block builder, so a
# fixture can only differ from a real form in the cells this file changes.
form_fixture <- function(path, blocks, version = as.character(LSS_FORM_VERSION),
                         chrome_lang = "fr", stray = NULL) {
  chrome <- lss_chrome_strings(chrome_lang)
  theme <- lss_render_theme()
  theme$content_width_in <- lss_content_width_in("A4-portrait")
  theme$chrome <- chrome
  theme$chrome_lang <- chrome_lang
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Questionnaire")
  for (i in seq_along(blocks)) {
    b <- blocks[[i]]
    rows <- lapply(b$rows, function(r) form_row(r[[1L]], r[[2L]]))
    doc <- form_add_block(doc, theme, rows, b$title, b$value %||% "")
    if (!is.null(stray) && i == 1L) doc <- officer::body_add_par(doc, stray)
  }
  if (!is.na(version)) {
    doc <- officer::set_doc_properties(
      doc,
      values = list(`lssdoc-template-version` = version,
                    `lssdoc-chrome-lang` = chrome_lang))
  }
  print(doc, target = path)
  path
}

# A minimal valid document: one survey, one group, one single-choice question.
form_blocks <- function(chrome = lss_chrome_strings("fr")) {
  list(
    list(title = chrome$form_block_survey, rows = list(
      list(chrome$form_title, "Questionnaire d'essai"),
      list(chrome$cover_languages, "fr"),
      list(chrome$welcome_text_title, ""),
      list(chrome$end_text_title, ""))),
    list(title = chrome$form_block_group, rows = list(
      list(chrome$form_title, "Profil"),
      list(chrome$description_title, ""))),
    list(title = chrome$form_block_question, value = "Q1", rows = list(
      list(chrome$meta_type, "single"),
      list(chrome$meta_mandatory, "Oui"),
      list(chrome$meta_filter, ""),
      list(chrome$form_wording, "Participez-vous ?"),
      list(chrome$item_help, ""),
      list(chrome$item_options, c("1 = Oui", "2 = Non")),
      list(chrome$form_other_position, "")))
  )
}

# Replace one row of one block of `form_blocks()`.
form_set_row <- function(blocks, block, key, value) {
  rows <- blocks[[block]]$rows
  for (k in seq_along(rows)) {
    if (identical(rows[[k]][[1L]], key)) rows[[k]][[2L]] <- value
  }
  blocks[[block]]$rows <- rows
  blocks
}

form_add_row <- function(blocks, block, key, value) {
  blocks[[block]]$rows <- c(blocks[[block]]$rows, list(list(key, value)))
  blocks
}

# A parsing context for the unit tests that call the internals directly.
form_test_ctx <- function(lang = "fr", where = "Question Q1") {
  ctx <- form_new_ctx("fixture.docx", collect = FALSE)
  ctx$chrome_lang <- lang
  ctx$chrome <- lss_chrome_strings(lang)
  ctx$languages <- "fr"
  ctx$where <- where
  ctx$code <- "Q1"
  ctx
}

form_xml <- function(inner) {
  xml2::read_xml(paste0(
    "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
    "<w:body>", inner, "</w:body></w:document>"))
}

form_tbl_xml <- function(inner) {
  xml2::read_xml(paste0(
    "<w:tbl xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
    inner, "</w:tbl>"))
}


# ---- round trips -------------------------------------------------------------

test_that("a specification of all 21 kinds survives the round trip", {
  # Exhaustive by design -- every authorable kind, or every chrome
  # language -- and each pass writes and re-reads a whole Word document.
  # It stays exhaustive locally and in CI; on CRAN the refusals below
  # cover the reader, and the smaller round trips cover the contract.
  skip_on_cran()
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)

  spec <- lss_example_spec(lang = "fr")
  write_form_docx(spec, path, lang = "fr")
  back <- read_form_docx(path)

  expect_s3_class(back, "lss_spec")
  expect_identical(canonical_spec(back), canonical_spec(spec))

  # and the survey they describe is the same file, byte for byte
  a <- tempfile(fileext = ".lss")
  b <- tempfile(fileext = ".lss")
  on.exit(unlink(c(a, b)), add = TRUE)
  write_lss(spec, a, sid = 100001L)
  write_lss(back, b, sid = 100001L)
  expect_identical(readBin(a, "raw", file.size(a)),
                   readBin(b, "raw", file.size(b)))
})

test_that("the round trip holds in each of the five chrome languages", {
  # Exhaustive by design -- every authorable kind, or every chrome
  # language -- and each pass writes and re-reads a whole Word document.
  # It stays exhaustive locally and in CI; on CRAN the refusals below
  # cover the reader, and the smaller round trips cover the contract.
  skip_on_cran()
  skip_if_no_docx()
  kinds <- c("single", "multiple", "array", "ranking", "text", "display")
  for (lang in c("en", "fr", "de", "es", "it")) {
    path <- tempfile(fileext = ".docx")
    spec <- lss_example_spec(kinds = kinds, lang = lang)
    write_form_docx(spec, path, lang = lang)
    back <- read_form_docx(path)
    expect_identical(canonical_spec(back), canonical_spec(spec),
                     info = paste("chrome language", lang))
    unlink(path)
  }
})

test_that("a two-language form reads its suffixed keys back", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)

  spec <- lss_spec(
    title = c(fr = "Enquete bilingue", en = "Bilingual survey"),
    languages = c("fr", "en"),
    welcome = c(fr = "Bienvenue.", en = "Welcome."),
    end_text = c(fr = "Merci.", en = "Thank you."),
    groups = list(list(
      title = c(fr = "Profil", en = "Profile"),
      description = c(fr = "Quelques questions.", en = "A few questions."),
      questions = list(
        list(code = "q1", kind = "single", mandatory = TRUE,
             text = c(fr = "Participez-vous ?", en = "Do you take part?"),
             help = c(fr = "Une seule reponse.", en = "One answer only."),
             other_position = "end",
             options = list(
               list(text = c(fr = "Oui", en = "Yes")),
               list(text = c(fr = "Non", en = "No")),
               list(text = c(fr = "Autre situation", en = "Another situation"),
                    other = TRUE))),
        list(code = "q2", kind = "multiple", relevance = "q1 = 1",
             text = c(fr = "Lesquelles ?", en = "Which ones?"),
             max_answers = 2,
             options = list(
               list(text = c(fr = "La cantine", en = "The canteen")),
               list(text = c(fr = "La creche", en = "Childcare")),
               list(text = c(fr = "Aucune", en = "None"), exclusive = TRUE)))))),
    quotas = list(list(
      question = "q1", code = "2", limit = 0L,
      name = c(fr = "Refus", en = "Declined"),
      message = c(fr = "Merci quand meme.", en = "Thank you anyway.")))
  )

  write_form_docx(spec, path, lang = "fr")
  back <- read_form_docx(path)
  expect_identical(canonical_spec(back), canonical_spec(spec))
  expect_identical(back$languages, c("fr", "en"))
})

test_that("the blank template reads back as the example specification", {
  # Exhaustive by design -- every authorable kind, or every chrome
  # language -- and each pass writes and re-reads a whole Word document.
  # It stays exhaustive locally and in CI; on CRAN the refusals below
  # cover the reader, and the smaller round trips cover the contract.
  skip_on_cran()
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  kinds <- c("single", "dropdown", "multiple", "array", "array5", "ranking",
             "multitext", "text", "date", "display")
  lss_template_docx(path, lang = "fr", kinds = kinds)

  back <- read_form_docx(path)
  expect_identical(canonical_spec(back),
                   canonical_spec(lss_example_spec(kinds = kinds, lang = "fr")))
})

test_that("a fixture built row by row reads back", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  form_fixture(path, form_blocks())

  spec <- read_form_docx(path)
  expect_s3_class(spec, "lss_spec")
  expect_identical(spec$languages, "fr")
  q <- spec$groups[[1L]]$questions[[1L]]
  expect_identical(q$code, "Q1")
  expect_identical(q$kind, "single")
  expect_true(q$mandatory)
  expect_identical(vapply(q$options, function(o) o$code, character(1)),
                   c("1", "2"))
})


# ---- the file and its marker -------------------------------------------------

test_that("a file that is not a .docx is refused", {
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  writeLines("this is not a zip archive", path)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_file")
  expect_error(read_form_docx(tempfile(fileext = ".docx")),
               class = "lssdoc_bad_form_file")
  expect_error(read_form_docx(42), class = "lssdoc_bad_form_file")
})

test_that("a document without the version property is refused by cause", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  form_fixture(path, form_blocks(), version = NA_character_)

  expect_error(read_form_docx(path), class = "lssdoc_bad_form_marker")
  expect_error(read_form_docx(path), regexp = "lssdoc-template-version")
  # the message names the three known causes
  msg <- tryCatch(read_form_docx(path), error = conditionMessage)
  expect_match(msg, "Document Inspector|pasted|not generated by lssdoc")
})

test_that("a document of another contract version is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  form_fixture(path, form_blocks(), version = "99")
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_marker")
  expect_error(read_form_docx(path), regexp = "99")
})

test_that("form_marker() reads the property from a path alone", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  form_fixture(path, form_blocks())
  expect_identical(form_marker(path), as.integer(LSS_FORM_VERSION))
})


# ---- structural refusals ------------------------------------------------------

test_that("tracked changes and content controls are refused", {
  ctx <- form_test_ctx()
  ins <- form_xml("<w:p><w:ins><w:r><w:t>added</w:t></w:r></w:ins></w:p>")
  expect_error(form_refuse_document_features(ins, ctx),
               class = "lssdoc_bad_form_layout")
  expect_error(form_refuse_document_features(ins, ctx), regexp = "tracked changes")

  del <- form_xml("<w:p><w:del><w:r><w:delText>gone</w:delText></w:r></w:del></w:p>")
  expect_error(form_refuse_document_features(del, ctx),
               class = "lssdoc_bad_form_layout")

  sdt <- form_xml("<w:sdt><w:sdtContent><w:p><w:r><w:t>x</w:t></w:r></w:p></w:sdtContent></w:sdt>")
  expect_error(form_refuse_document_features(sdt, ctx),
               class = "lssdoc_bad_form_layout")
  expect_error(form_refuse_document_features(sdt, ctx), regexp = "content control")

  clean <- form_xml("<w:p><w:r><w:t>plain</w:t></w:r></w:p>")
  expect_null(form_refuse_document_features(clean, ctx))
})

test_that("a nested table and a merged cell are refused, with the row named", {
  ctx <- form_test_ctx()
  cell <- function(inner) paste0("<w:tc>", inner, "</w:tc>")
  para <- function(text) paste0("<w:p><w:r><w:t>", text, "</w:t></w:r></w:p>")
  grid2 <- "<w:tblGrid><w:gridCol/><w:gridCol/></w:tblGrid>"

  nested <- form_tbl_xml(paste0(
    grid2, "<w:tr>", cell(para("Type")),
    cell(paste0("<w:tbl>", grid2, "<w:tr>", cell(para("x")), cell(para("y")),
                "</w:tr></w:tbl>")),
    "</w:tr>"))
  expect_error(form_table_rows(nested, 3L, ctx), class = "lssdoc_bad_form_layout")
  expect_error(form_table_rows(nested, 3L, ctx), regexp = "nested table")

  merged <- form_tbl_xml(paste0(
    grid2, "<w:tr>", cell(para("Type")), cell(para("single")), "</w:tr>",
    "<w:tr>",
    "<w:tc><w:tcPr><w:gridSpan w:val=\"2\"/></w:tcPr>", para("both"), "</w:tc>",
    "</w:tr>"))
  expect_error(form_table_rows(merged, 2L, ctx), class = "lssdoc_bad_form_layout")
  expect_error(form_table_rows(merged, 2L, ctx), regexp = "row 2")

  vmerge <- form_tbl_xml(paste0(
    grid2, "<w:tr>",
    "<w:tc><w:tcPr><w:vMerge w:val=\"restart\"/></w:tcPr>", para("Type"), "</w:tc>",
    cell(para("single")), "</w:tr>"))
  expect_error(form_table_rows(vmerge, 1L, ctx), class = "lssdoc_bad_form_layout")

  three <- form_tbl_xml(paste0(
    "<w:tblGrid><w:gridCol/><w:gridCol/><w:gridCol/></w:tblGrid>",
    "<w:tr>", cell(para("a")), cell(para("b")), cell(para("c")), "</w:tr>"))
  expect_error(form_table_rows(three, 1L, ctx), class = "lssdoc_bad_form_layout")
  expect_error(form_table_rows(three, 1L, ctx), regexp = "3 columns")

  ragged <- form_tbl_xml(paste0(
    grid2, "<w:tr>", cell(para("only one")), "</w:tr>"))
  expect_error(form_table_rows(ragged, 1L, ctx), class = "lssdoc_bad_form_layout")
})

test_that("a merged cell written by flextable is refused end to end", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")

  df <- data.frame(key = c(chrome$form_block_survey, chrome$form_title),
                   value = c("", "Titre"), stringsAsFactors = FALSE)
  ft <- flextable::delete_part(flextable::flextable(df), part = "header")
  ft <- flextable::merge_at(ft, i = 2L, j = 1:2, part = "body")
  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::set_doc_properties(
    doc, values = list(`lssdoc-template-version` = as.character(LSS_FORM_VERSION)))
  print(doc, target = path)

  expect_error(read_form_docx(path), class = "lssdoc_bad_form_layout")
})

test_that("a three-column block written by flextable is refused end to end", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")

  df <- data.frame(key = c(chrome$form_block_survey, chrome$form_title),
                   value = c("", "Titre"), extra = c("", "de trop"),
                   stringsAsFactors = FALSE)
  ft <- flextable::delete_part(flextable::flextable(df), part = "header")
  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, ft)
  doc <- officer::set_doc_properties(
    doc, values = list(`lssdoc-template-version` = as.character(LSS_FORM_VERSION)))
  print(doc, target = path)

  expect_error(read_form_docx(path), class = "lssdoc_bad_form_layout")
  expect_error(read_form_docx(path), regexp = "column")
})

test_that("Word's automatic list numbering is detected and refused", {
  ctx <- form_test_ctx()
  numbered <- xml2::read_xml(paste0(
    "<w:tc xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
    "<w:p><w:pPr><w:numPr><w:ilvl w:val=\"0\"/><w:numId w:val=\"3\"/></w:numPr>",
    "</w:pPr><w:r><w:t>Oui</w:t></w:r></w:p>",
    "<w:p><w:r><w:t>Non</w:t></w:r></w:p></w:tc>"))
  cell <- form_cell_lines(numbered)
  expect_true(cell$numbered)
  expect_identical(cell$lines, c("Oui", "Non"))

  # numId 0 is Word's "no list", and must not trip the refusal
  plain <- xml2::read_xml(paste0(
    "<w:tc xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
    "<w:p><w:pPr><w:numPr><w:numId w:val=\"0\"/></w:numPr></w:pPr>",
    "<w:r><w:t>Oui</w:t></w:r></w:p></w:tc>"))
  expect_false(form_cell_lines(plain)$numbered)

  block <- list(kind = "question", rows = list(list(
    table = 3L, row = 6L, key_lines = "Options",
    value_lines = c("Oui", "Non"), numbered = TRUE)))
  expect_error(form_block_entries(block, ctx), class = "lssdoc_bad_form_layout")
  expect_error(form_block_entries(block, ctx), regexp = "automatic list")
})

test_that("a paragraph is cut on soft returns and keeps its runs together", {
  p <- xml2::read_xml(paste0(
    "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
    "<w:r><w:t xml:space=\"preserve\">1 = Pre</w:t></w:r>",
    "<w:r><w:t>mier</w:t></w:r>",
    "<w:r><w:br/></w:r>",
    "<w:r><w:t>2 = Second</w:t></w:r>",
    "<w:r><w:br w:type=\"page\"/></w:r>",
    "</w:p>"))
  expect_identical(form_para_lines(p), c("1 = Premier", "2 = Second"))
})


# ---- block and key errors -----------------------------------------------------

test_that("a table that does not open a block is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")
  blocks <- form_blocks(chrome)
  blocks[[1L]]$title <- "Bloc inconnu"
  form_fixture(path, blocks)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_block")
})

test_that("an unknown field is refused and the accepted labels are listed", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  blocks <- form_add_row(form_blocks(), 3L, "Couleur", "bleue")
  form_fixture(path, blocks)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_key")
  expect_error(read_form_docx(path), regexp = "Couleur")
})

test_that("the same field twice in one block is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")
  blocks <- form_add_row(form_blocks(chrome), 3L, chrome$meta_type, "multiple")
  form_fixture(path, blocks)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_key")
  expect_error(read_form_docx(path), regexp = "twice")
})

test_that("a value without a label is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  blocks <- form_add_row(form_blocks(), 3L, "", "une note egaree")
  form_fixture(path, blocks)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_key")
})

test_that("a language suffix that is not declared is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")
  blocks <- form_set_row(form_blocks(chrome), 3L, chrome$form_wording, "")
  blocks <- form_add_row(blocks, 3L, paste0(chrome$form_wording, " [xx]"),
                         "Participez-vous ?")
  form_fixture(path, blocks)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_key")
  expect_error(read_form_docx(path), regexp = "xx")
})

test_that("a language suffix on a field that takes none is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")
  blocks <- form_set_row(form_blocks(chrome), 3L, chrome$meta_type, "")
  blocks <- form_add_row(blocks, 3L, paste0(chrome$meta_type, " [fr]"), "single")
  form_fixture(path, blocks)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_key")
  expect_error(read_form_docx(path), regexp = "no language suffix")
})

test_that("a question before any group block is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  blocks <- form_blocks()
  form_fixture(path, blocks[c(1L, 3L)])
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_block")
  expect_error(read_form_docx(path), regexp = "before any")
})

test_that("a group with no question is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  blocks <- form_blocks()
  form_fixture(path, blocks[c(1L, 2L)])
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_block")
  expect_error(read_form_docx(path), regexp = "no question")
})

test_that("a duplicate question code is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  blocks <- form_blocks()
  blocks <- c(blocks, list(blocks[[3L]]))
  form_fixture(path, blocks)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_value")
  expect_error(read_form_docx(path), regexp = "duplicate")
  # and the author is told where the first occurrence is: survey, group,
  # question make the first Q1 block table 3
  expect_error(read_form_docx(path), regexp = "already used by table 3")
})

test_that("a question block with no code, or a bad one, is refused", {
  # A battery: one whole Word document written and parsed per case.
  # Exhaustive locally and in CI; the single-document refusals around
  # it keep the reader covered on CRAN.
  skip_on_cran()
  skip_if_no_docx()
  blocks <- form_blocks()

  no_code <- tempfile(fileext = ".docx")
  on.exit(unlink(no_code), add = TRUE)
  b1 <- blocks
  b1[[3L]]$value <- ""
  form_fixture(no_code, b1)
  expect_error(read_form_docx(no_code), class = "lssdoc_bad_form_block")

  bad_code <- tempfile(fileext = ".docx")
  on.exit(unlink(bad_code), add = TRUE)
  b2 <- blocks
  b2[[3L]]$value <- "1 mauvais"
  form_fixture(bad_code, b2)
  expect_error(read_form_docx(bad_code), class = "lssdoc_bad_form_value")
})

test_that("text next to a block title that is not a question is refused", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  blocks <- form_blocks()
  blocks[[2L]]$value <- "un commentaire"
  form_fixture(path, blocks)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_block")
})


# ---- value errors -------------------------------------------------------------

test_that("a required field left empty is refused", {
  # A battery: one whole Word document written and parsed per case.
  # Exhaustive locally and in CI; the single-document refusals around
  # it keep the reader covered on CRAN.
  skip_on_cran()
  skip_if_no_docx()
  chrome <- lss_chrome_strings("fr")

  no_wording <- tempfile(fileext = ".docx")
  on.exit(unlink(no_wording), add = TRUE)
  form_fixture(no_wording, form_set_row(form_blocks(chrome), 3L,
                                        chrome$form_wording, ""))
  expect_error(read_form_docx(no_wording), class = "lssdoc_bad_form_value")

  no_langs <- tempfile(fileext = ".docx")
  on.exit(unlink(no_langs), add = TRUE)
  form_fixture(no_langs, form_set_row(form_blocks(chrome), 1L,
                                      chrome$cover_languages, ""))
  expect_error(read_form_docx(no_langs), class = "lssdoc_bad_form_value")

  no_options <- tempfile(fileext = ".docx")
  on.exit(unlink(no_options), add = TRUE)
  form_fixture(no_options, form_set_row(form_blocks(chrome), 3L,
                                        chrome$item_options, ""))
  expect_error(read_form_docx(no_options), class = "lssdoc_bad_form_value")

  too_few <- tempfile(fileext = ".docx")
  on.exit(unlink(too_few), add = TRUE)
  form_fixture(too_few, form_set_row(form_blocks(chrome), 3L,
                                     chrome$item_options, "1 = Oui"))
  expect_error(read_form_docx(too_few), class = "lssdoc_bad_form_value")
  expect_error(read_form_docx(too_few), regexp = "at least 2")
})

test_that("an unknown or ambiguous type is refused", {
  # A battery: one whole Word document written and parsed per case.
  # Exhaustive locally and in CI; the single-document refusals around
  # it keep the reader covered on CRAN.
  skip_on_cran()
  skip_if_no_docx()
  chrome <- lss_chrome_strings("fr")

  unknown <- tempfile(fileext = ".docx")
  on.exit(unlink(unknown), add = TRUE)
  form_fixture(unknown, form_set_row(form_blocks(chrome), 3L, chrome$meta_type,
                                     "questionnaire"))
  expect_error(read_form_docx(unknown), class = "lssdoc_bad_form_value")
  expect_error(read_form_docx(unknown), regexp = "unknown type")

  ambiguous <- tempfile(fileext = ".docx")
  on.exit(unlink(ambiguous), add = TRUE)
  form_fixture(ambiguous, form_set_row(form_blocks(chrome), 3L, chrome$meta_type,
                                       chrome$type_single_choice))
  expect_error(read_form_docx(ambiguous), class = "lssdoc_bad_form_value")
  expect_error(read_form_docx(ambiguous), regexp = "ambiguous")

  # a label that names exactly one kind is accepted
  ctx <- form_test_ctx()
  expect_identical(form_parse_kind("Classement", ctx, "Type"), "ranking")
  expect_identical(form_parse_kind("Ranking", ctx, "Type"), "ranking")
  expect_identical(form_parse_kind("Array, 5-point scale", ctx, "Type"), "array5")
})

test_that("a malformed option line is refused, line by line", {
  # One whole Word document per malformed line, eight of them: the
  # battery is worth its seconds locally and in CI, not on CRAN.
  skip_on_cran()
  skip_if_no_docx()
  chrome <- lss_chrome_strings("fr")

  # Wider than any code LimeSurvey stores anywhere: the reader itself refuses
  # it, since past that width the left side of `=` cannot be a code at all.
  long_code <- tempfile(fileext = ".docx")
  on.exit(unlink(long_code), add = TRUE)
  form_fixture(long_code, form_set_row(form_blocks(chrome), 3L, chrome$item_options,
                                       c("abcdefghijklmnopqrstu = Oui", "2 = Non")))
  expect_error(read_form_docx(long_code), class = "lssdoc_bad_form_value")
  expect_error(read_form_docx(long_code), regexp = "invalid code")

  # Between the two widths: a single choice stores its options as ANSWERS, in
  # five characters, so the specification refuses it at assembly -- with the
  # storage named, not the field.
  answer_code <- tempfile(fileext = ".docx")
  on.exit(unlink(answer_code), add = TRUE)
  form_fixture(answer_code, form_set_row(form_blocks(chrome), 3L, chrome$item_options,
                                         c("abcdef = Oui", "2 = Non")))
  expect_error(read_form_docx(answer_code), class = "lssdoc_bad_form_spec")
  expect_error(read_form_docx(answer_code), regexp = "stores this list as answers")

  # The same code on a multiple choice, whose options are stored as
  # SUBQUESTIONS (20 characters), is a valid code and reads back verbatim.
  sq_code <- tempfile(fileext = ".docx")
  on.exit(unlink(sq_code), add = TRUE)
  blocks <- form_set_row(form_blocks(chrome), 3L, chrome$meta_type, "multiple")
  blocks <- form_set_row(blocks, 3L, chrome$item_options,
                         c("STRESS = Stress", "2 = Non"))
  form_fixture(sq_code, blocks)
  spec <- read_form_docx(sq_code)
  expect_identical(spec$groups[[1L]]$questions[[1L]]$options[[1L]]$code, "STRESS")

  empty_label <- tempfile(fileext = ".docx")
  on.exit(unlink(empty_label), add = TRUE)
  form_fixture(empty_label, form_set_row(form_blocks(chrome), 3L,
                                         chrome$item_options, c("1 =", "2 = Non")))
  expect_error(read_form_docx(empty_label), class = "lssdoc_bad_form_value")

  duplicate <- tempfile(fileext = ".docx")
  on.exit(unlink(duplicate), add = TRUE)
  form_fixture(duplicate, form_set_row(form_blocks(chrome), 3L, chrome$item_options,
                                       c("1 = Oui", "1 = Non")))
  expect_error(read_form_docx(duplicate), class = "lssdoc_bad_form_value")
  expect_error(read_form_docx(duplicate), regexp = "twice")

  two_others <- tempfile(fileext = ".docx")
  on.exit(unlink(two_others), add = TRUE)
  form_fixture(two_others, form_set_row(form_blocks(chrome), 3L, chrome$item_options,
                                        c("1 = Oui", "Autre", "Autre = Encore")))
  expect_error(read_form_docx(two_others), class = "lssdoc_bad_form_value")
})

test_that("a field that does not apply to the kind is refused when filled", {
  # A battery: one whole Word document written and parsed per case.
  # Exhaustive locally and in CI; the single-document refusals around
  # it keep the reader covered on CRAN.
  skip_on_cran()
  skip_if_no_docx()
  chrome <- lss_chrome_strings("fr")
  blocks <- form_set_row(form_blocks(chrome), 3L, chrome$meta_type, "text")
  blocks <- form_set_row(blocks, 3L, chrome$item_options, "1 = Oui")

  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  form_fixture(path, blocks)
  expect_error(read_form_docx(path), class = "lssdoc_bad_form_key")
  expect_error(read_form_docx(path), regexp = "does not apply")

  # blank, the very same row is ignored: an author may change the type
  # without deleting the rows the old type used
  ok <- tempfile(fileext = ".docx")
  on.exit(unlink(ok), add = TRUE)
  blank <- form_set_row(form_blocks(chrome), 3L, chrome$meta_type, "text")
  blank <- form_set_row(blank, 3L, chrome$item_options, "")
  form_fixture(ok, blank)
  spec <- read_form_docx(ok)
  expect_identical(spec$groups[[1L]]$questions[[1L]]$kind, "text")
})

test_that("the position of Other, the caps and the quota fields are validated", {
  ctx <- form_test_ctx()
  expect_identical(form_parse_other_position("Fin", ctx, "Position")$position, "end")
  expect_identical(form_parse_other_position("D\u00E9but", ctx, "Position")$position,
                   "beginning")
  after <- form_parse_other_position("Apr\u00E8s A3", ctx, "Position")
  expect_identical(after$position, "specific")
  expect_identical(after$code, "A3")
  # an author on a keyboard without accents types "Apres A3": the
  # accent-insensitive fallback must still hand back the code AS TYPED --
  # `A3`, never `a3`, since LimeSurvey answer codes are case-sensitive
  unaccented <- form_parse_other_position("Apres A3", ctx, "Position")
  expect_identical(unaccented$position, "specific")
  expect_identical(unaccented$code, "A3")
  expect_identical(form_parse_other_position("APRES A3", ctx, "Position")$code, "A3")
  es <- form_test_ctx(lang = "es")
  expect_identical(form_parse_other_position("Despues de A3", es, "Position")$code,
                   "A3")
  expect_identical(form_parse_other_position("specific:2", ctx, "Position")$code, "2")
  expect_error(form_parse_other_position("au milieu", ctx, "Position"),
               class = "lssdoc_bad_form_value")

  expect_identical(form_parse_integer("3", ctx, "Max"), 3L)
  expect_error(form_parse_integer("trois", ctx, "Max"),
               class = "lssdoc_bad_form_value")
  expect_error(form_parse_integer("0", ctx, "Max"), class = "lssdoc_bad_form_value")
  expect_identical(form_parse_integer("0", ctx, "Limite", min = 0L), 0L)

  expect_identical(form_parse_quota_action("terminer le questionnaire", ctx, "Action"),
                   "terminate")
  expect_identical(form_parse_quota_action("1", ctx, "Action"), "terminate")
  expect_error(form_parse_quota_action("laisser le r\u00E9pondant modifier", ctx,
                                       "Action"),
               class = "lssdoc_bad_form_value")
  expect_error(form_parse_quota_action("rien", ctx, "Action"),
               class = "lssdoc_bad_form_value")

  expect_identical(form_parse_condition("Q1 = 2", ctx, "Condition"),
                   list(question = "Q1", code = "2"))
  expect_error(form_parse_condition("Q1 in [1, 2]", ctx, "Condition"),
               class = "lssdoc_bad_form_value")
  expect_error(form_parse_condition("Q1 = autre", ctx, "Condition"),
               class = "lssdoc_bad_form_value")
})

test_that("what lss_spec() refuses comes back as a form error", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")
  blocks <- form_set_row(form_blocks(chrome), 3L, chrome$meta_type, "multiple")
  blocks <- form_set_row(blocks, 3L, chrome$item_options,
                         c("1 = La cantine", "2 = La creche"))
  blocks <- form_add_row(blocks, 3L, chrome$form_max_answers, "5")
  form_fixture(path, blocks)

  expect_error(read_form_docx(path), class = "lssdoc_bad_form_spec")
  expect_error(read_form_docx(path), class = "lssdoc_bad_form")
  expect_error(read_form_docx(path), class = "lssdoc_bad_spec")
  expect_error(read_form_docx(path), regexp = "Q1")

  # the block AND the form label of the offending field are named
  out <- check_form_docx(path)
  expect_true(any(out$class == "lssdoc_bad_form_spec"))
  expect_identical(out$code[out$class == "lssdoc_bad_form_spec"], "Q1")
  expect_identical(out$field[out$class == "lssdoc_bad_form_spec"],
                   chrome$form_max_answers)
})

test_that("a spec error names the question it is about, not a shorter code", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")

  # Q1 is valid; Q10 is not. Matching the code against the message by
  # substring made `Q1` -- scanned first -- steal the error about `Q10`.
  blocks <- form_blocks(chrome)
  q10 <- blocks[[3L]]
  q10$value <- "Q10"
  q10$rows <- list(
    list(chrome$meta_type, "multiple"),
    list(chrome$form_wording, "Pourquoi ?"),
    list(chrome$item_options, c("1 = La cantine", "2 = La creche")),
    list(chrome$form_max_answers, "5"))
  form_fixture(path, c(blocks, list(q10)))

  expect_error(read_form_docx(path), class = "lssdoc_bad_form_spec")
  expect_error(read_form_docx(path), regexp = "Q10")

  out <- check_form_docx(path)
  spec_rows <- out[out$class == "lssdoc_bad_form_spec", , drop = FALSE]
  expect_identical(nrow(spec_rows), 1L)
  expect_identical(spec_rows$code, "Q10")
  expect_identical(spec_rows$field, chrome$form_max_answers)
  expect_match(spec_rows$block, "Q10")
})


# ---- stray text and the dry run ------------------------------------------------

test_that("text outside the blocks warns without blocking the read", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  form_fixture(path, form_blocks(), stray = "Note pour plus tard")

  expect_warning(spec <- read_form_docx(path), class = "lssdoc_form_stray_text")
  expect_s3_class(spec, "lss_spec")
})

test_that("check_form_docx() collects every problem and prints them", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")

  blocks <- form_blocks(chrome)
  blocks <- form_add_row(blocks, 3L, "Couleur", "bleue")
  second <- blocks[[3L]]
  second$value <- "Q2"
  second$rows <- list(
    list(chrome$meta_type, "wobble"),
    list(chrome$form_wording, ""))
  blocks <- c(blocks, list(second))
  form_fixture(path, blocks, stray = "Une note egaree")

  out <- check_form_docx(path)
  expect_s3_class(out, "lss_form_check")
  expect_s3_class(out, "data.frame")
  expect_identical(names(out),
                   c("severity", "class", "block", "code", "field", "message"))
  expect_gte(nrow(out), 3L)
  expect_true(any(out$class == "lssdoc_bad_form_key"))
  expect_true(any(out$class == "lssdoc_bad_form_value"))
  expect_true(any(out$severity == "warning"))
  # the question code is carried by the rows of the block it came from
  expect_true(any(out$code == "Q2", na.rm = TRUE))
  # cli writes its own output to the message stream, as print.lss_audit() does
  printed <- paste(utils::capture.output(print(out), type = "message"),
                   collapse = "\n")
  expect_match(printed, "form check")
  expect_match(printed, "problem")
  expect_match(printed, "unknown field")
  capped <- paste(utils::capture.output(print(out, n = 1L), type = "message"),
                  collapse = "\n")
  expect_match(capped, "more problem")
})

test_that("check_form_docx() reports a clean document as clean", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  form_fixture(path, form_blocks())
  out <- check_form_docx(path)
  expect_identical(nrow(out), 0L)
  printed <- paste(utils::capture.output(print(out), type = "message"),
                   collapse = "\n")
  expect_match(printed, "No problems")
})

test_that("check_form_docx() reports a fatal problem alone", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  form_fixture(path, form_blocks(), version = NA_character_)
  out <- check_form_docx(path)
  expect_identical(nrow(out), 1L)
  expect_identical(out$class, "lssdoc_bad_form_marker")
})

test_that("a table that does not open a block is refused, wherever it sits", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")

  # a block closes at every table boundary: a table whose first row carries an
  # ordinary key (a deleted title row, a block Word split in two, an unrelated
  # table pasted in) must be refused, NOT appended to the block above -- where
  # its rows would come back as duplicate fields of the wrong question
  blocks <- form_blocks(chrome)
  stray <- list(title = chrome$meta_type, value = "single",
                rows = list(list(chrome$meta_mandatory, "Non")))
  q2 <- blocks[[3L]]
  q2$value <- "Q2"
  q2$rows <- list(list(chrome$meta_type, "wobble"),
                  list(chrome$form_wording, "Et ensuite ?"))
  form_fixture(path, c(blocks, list(stray), list(q2)))

  expect_error(read_form_docx(path), class = "lssdoc_bad_form_block")
  expect_error(read_form_docx(path), regexp = "does not start with a block title row")

  # and in check mode the stray table is one problem among the others, not a
  # wall the scan stops at: the bad Type of the block AFTER it is reported too
  out <- check_form_docx(path)
  expect_true(any(out$class == "lssdoc_bad_form_block"))
  expect_true(any(out$class == "lssdoc_bad_form_value"))
  expect_true(any(out$code == "Q2", na.rm = TRUE))
  expect_match(paste(out$message, collapse = "\n"), "wobble")
  # the orphan table is reported once, not once per row it holds
  expect_identical(sum(grepl("does not start with a block title row", out$message)),
                   1L)
})

test_that("a bad row does not discard the blocks of a pasted table", {
  ctx <- form_new_ctx("fixture.docx", collect = TRUE)
  ctx$chrome <- lss_chrome_strings("fr")
  chrome <- ctx$chrome
  cell <- function(inner) paste0("<w:tc>", inner, "</w:tc>")
  para <- function(text) paste0("<w:p><w:r><w:t>", text, "</w:t></w:r></w:p>")
  grid2 <- "<w:tblGrid><w:gridCol/><w:gridCol/></w:tblGrid>"
  tbl <- form_tbl_xml(paste0(
    grid2,
    "<w:tr>", cell(para(chrome$form_block_question)), cell(para("Q1")), "</w:tr>",
    "<w:tr><w:tc><w:tcPr><w:gridSpan w:val=\"2\"/></w:tcPr>", para("fusionnee"),
    "</w:tc></w:tr>",
    "<w:tr>", cell(para(chrome$meta_type)), cell(para("single")), "</w:tr>"))

  # collect mode keeps the readable rows and records the one bad row
  rows <- form_table_rows(tbl, 2L, ctx)
  expect_length(rows, 2L)
  expect_identical(rows[[2L]]$key_lines, chrome$meta_type)
  expect_length(ctx$problems, 1L)
  expect_identical(ctx$problems[[1L]]$class, "lssdoc_bad_form_layout")

  # strict mode still stops at the first offending row
  strict <- form_test_ctx()
  expect_error(form_table_rows(tbl, 2L, strict), class = "lssdoc_bad_form_layout")
})

test_that("a first group that fails to parse does not orphan its questions", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  chrome <- lss_chrome_strings("fr")

  blocks <- form_set_row(form_blocks(chrome), 2L, chrome$form_title, "")
  blocks <- form_set_row(blocks, 3L, chrome$item_options, "1 = Oui")
  form_fixture(path, blocks)

  out <- check_form_docx(path)
  # the empty group title is reported once
  expect_true(any(out$class == "lssdoc_bad_form_value"))
  # and the question is NOT accused of appearing before any group block: a
  # group block WAS seen, it simply did not parse
  expect_false(any(grepl("before any", out$message)))
  # its own problem is reported instead
  expect_match(paste(out$message, collapse = "\n"), "at least 2")
  expect_true(any(out$code == "Q1", na.rm = TRUE))
})


# ---- Word hazards --------------------------------------------------------------

test_that("Word's capitalization does not change what a cell means", {
  ctx <- form_test_ctx()
  expect_identical(form_canon_filter("Count(Q4) >= 2"), "count(Q4) >= 2")
  expect_identical(form_canon_filter("count (Q4)>=2"), "count(Q4)>=2")
  expect_identical(form_canon_filter("Q2 In [1, Autre]"), "Q2 in [1, autre]")
  expect_identical(form_canon_filter("Q2 in [1, Sonstiges]"), "Q2 in [1, autre]")
  expect_identical(form_canon_filter("Q01Single = 1"), "Q01Single = 1")
  expect_identical(form_canon_filter("count(Q4) \u2265 2"), "count(Q4) >= 2")

  # the other keyword is canonicalized in the `code = value` form too: the
  # English template's own hint documents `Q1 = other`, so an author following
  # it must not be refused. Every other value token stays as typed.
  expect_identical(form_canon_filter("Q1 = Other"), "Q1 = autre")
  expect_identical(form_canon_filter("Q1 = Sonstiges"), "Q1 = autre")
  expect_identical(form_canon_filter("Q1 = Otro"), "Q1 = autre")
  expect_identical(form_canon_filter("Q1 = Altro"), "Q1 = autre")
  expect_identical(form_canon_filter("Q1 = Autre"), "Q1 = autre")
  expect_identical(form_canon_filter("Q1 = 1"), "Q1 = 1")
  expect_identical(form_canon_filter("Q1 = A3"), "Q1 = A3")
  expect_identical(form_canon_filter("Q1=Other"), "Q1 = autre")

  expect_identical(form_parse_kind("Single", ctx, "Type"), "single")
  expect_identical(form_parse_kind("ARRAY5", ctx, "Type"), "array5")
  for (yes in c("Oui", "oui", "OUI", "Yes", "Ja", "S\u00ED", "S\u00EC", "y",
                "TRUE", "1")) {
    expect_true(form_parse_mandatory(yes, ctx, "Obligatoire"), info = yes)
  }
  for (no in c("Non", "non", "No", "Nein", "n", "false", "0", "")) {
    expect_false(form_parse_mandatory(no, ctx, "Obligatoire"), info = no)
  }
  expect_error(form_parse_mandatory("soft", ctx, "Obligatoire"),
               class = "lssdoc_bad_form_value")
  expect_error(form_parse_mandatory("peut-\u00EAtre", ctx, "Obligatoire"),
               class = "lssdoc_bad_form_value")
})

test_that("Word's typography does not change what a cell means", {
  ctx <- form_test_ctx()
  # non-breaking spaces and a fullwidth equals sign around the separator
  nbsp <- form_clean_line("1\u00A0=\u00A0Libell\u00E9")
  expect_identical(nbsp, "1 = Libell\u00E9")
  expect_identical(form_parse_item(nbsp, 1L, ctx, "Options"),
                   list(code = "1", text = "Libell\u00E9", other = FALSE))
  full <- form_clean_line("2\uFF1DDeux")
  expect_identical(form_parse_item(full, 2L, ctx, "Options"),
                   list(code = "2", text = "Deux", other = FALSE))

  # a key wrapped in guillemets, and one with a trailing colon
  map <- form_label_map("question", "fr")
  expect_identical(map[[form_norm("\u00AB Libell\u00E9 \u00BB")]], "wording")
  expect_identical(map[[form_norm("Type :")]], "type")
  expect_identical(map[[form_norm("R\u00E9ponses max.")]], "max_answers")
  expect_identical(map[[form_norm("reponses max")]], "max_answers")

  # curly quotes inside a LABEL are content and survive untouched
  label <- "L\u2019ambiance d\u2019\u00E9quipe"
  expect_identical(form_parse_item(form_clean_line(label), 1L, ctx, "Options"),
                   list(code = NA_character_, text = label, other = FALSE))

  # letter codes keep their case
  expect_identical(form_parse_item("A = Alpha", 1L, ctx, "Options"),
                   list(code = "A", text = "Alpha", other = FALSE))
  expect_identical(form_parse_item("Y = Yes", 1L, ctx, "Options")$code, "Y")

  # the reserved word, alone or with a label, in any language
  expect_true(form_parse_item("Autre", 3L, ctx, "Options")$other)
  expect_identical(form_parse_item("Autre", 3L, ctx, "Options")$text, "Autre")
  other <- form_parse_item("Other = Please specify", 3L, ctx, "Options")
  expect_true(other$other)
  expect_identical(other$text, "Please specify")
  expect_true(form_parse_item("Sonstiges", 3L, ctx, "Options")$other)
  # a label that merely contains the word is an ordinary option
  expect_false(form_parse_item("Autre raison", 3L, ctx, "Options")$other)
})

test_that("a curly-quoted label round-trips through a real document", {
  skip_if_no_docx()
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  # curly quotes, guillemets and an em dash are CONTENT and come back untouched
  label <- "L\u2019ambiance d\u2019\u00E9quipe \u2014 \u00AB vraiment \u00BB"
  # a non-breaking space is not content: it is one of the characters Word
  # inserts on its own, and the reader normalizes it to a plain space
  nbsp_label <- "Autre\u00A0: pr\u00E9cisez"
  spec <- lss_spec(
    title = "Typographie",
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "single", text = "Qu\u2019en pensez-vous ?",
           options = list(list(text = label), list(text = nbsp_label)))))))
  write_form_docx(spec, path, lang = "fr")
  back <- read_form_docx(path)
  options <- back$groups[[1L]]$questions[[1L]]$options
  expect_identical(options[[1L]]$text$fr, label)
  expect_identical(options[[2L]]$text$fr, "Autre : pr\u00E9cisez")
  expect_false(options[[2L]]$other)
})


# ---- the dictionaries cannot drift ---------------------------------------------

test_that("no two fields of a block share a normalized label", {
  for (block in names(form_field_defs)) {
    for (lang in FORM_CHROME_LANGS) {
      chrome <- lss_chrome_strings(lang)
      labels <- vapply(form_field_defs[[block]],
                       function(d) form_norm(chrome[[d$key]]), character(1))
      expect_false(anyDuplicated(labels) > 0L,
                   info = paste(block, lang, paste(labels, collapse = " / ")))
      expect_true(all(nzchar(labels)), info = paste(block, lang))
    }
  }
})

test_that("every block word names one block kind, in five languages", {
  for (lang in FORM_CHROME_LANGS) {
    chrome <- lss_chrome_strings(lang)
    for (kind in c("survey", "group", "question", "quota")) {
      word <- chrome[[paste0("form_block_", kind)]]
      expect_identical(form_block_kind(word), kind,
                       info = paste(lang, kind, word))
    }
  }
  expect_true(is.na(form_block_kind("Couleur")))
})

test_that("the reader's question fields are exactly the writer's rows", {
  chrome <- lss_chrome_strings("fr")
  for (kind in lss_kinds$kind) {
    q <- list(code = "Q1", kind = kind)
    rows <- form_rows_for_question(q, lss_kinds, chrome, "fr")
    written <- vapply(rows, function(r) r$key, character(1))
    fields <- form_kind_fields(kind)
    labels <- vapply(fields, function(f) chrome[[form_field_defs$question[[f]]$key]],
                     character(1))
    expect_setequal(written, unname(labels))
  }
})

test_that("canonical_spec() normalizes counts, defaults and field order", {
  spec <- lss_spec(
    title = "Demo",
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "multiple", text = "Deux\nlignes",
           max_answers = "2", attributes = list(min_answers = "1"),
           options = list(list(text = "Un"), list(text = "Deux"),
                          list(text = "Trois"),
                          list(text = "Autre", other = TRUE)))))),
    quotas = NULL)
  canon <- canonical_spec(spec)
  q <- canon$groups[[1L]]$questions[[1L]]
  expect_identical(q$max_answers, 2L)
  expect_identical(q$attributes$min_answers, 1L)
  expect_identical(q$text$fr, "Deux\nlignes")
  # the position of the other option is written as a value by the form, so the
  # canonical form fills the default in
  expect_identical(q$other_position, "end")
  expect_identical(names(q)[1:5],
                   c("code", "kind", "text", "mandatory", "options"))
  expect_null(q$relevance)

  # the lines of a multi-line text are joined the same way whether they arrive
  # as a vector (welcome text, several paragraphs) or already joined
  vector_form <- canonical_spec(list(
    languages = "fr", title = "Demo", welcome = c("Un.", "Deux."),
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "text", text = c("Deux", "lignes")))))))
  expect_identical(vector_form$welcome$fr, "Un.\nDeux.")
  expect_identical(vector_form$groups[[1L]]$questions[[1L]]$text$fr,
                   "Deux\nlignes")
})
