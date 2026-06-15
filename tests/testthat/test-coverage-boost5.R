# Targeted tests for the remaining gaps: the XML section parser edge
# branches, render-time argument validation, and the render_audit /
# raw-filter paths that the default render does not exercise.

test_that("lss_section parses fields, empty rows and missing cells", {
  doc <- xml2::read_xml(paste0(
    "<document>",
    "<full><fields><fieldname>a</fieldname><fieldname>b</fieldname></fields>",
    "<rows>",
    "<row><a>1</a><b>x</b></row>",
    "<row><a>2</a></row>",            # b missing -> NA
    "<row><a></a><b>y</b></row>",     # a present-but-empty -> ""
    "</rows></full>",
    # A section that declares fields but has no rows -> zero-row data frame.
    "<empty><fields><fieldname>c</fieldname></fields><rows></rows></empty>",
    # A section with no fields at all -> NULL.
    "<nofields><rows><row><z>1</z></row></rows></nofields>",
    "</document>"
  ))
  full <- lssdoc:::lss_section(doc, "full")
  expect_identical(nrow(full), 3L)
  expect_identical(full$a, c("1", "2", ""))
  expect_true(is.na(full$b[2]))
  expect_identical(full$b[1], "x")

  empty <- lssdoc:::lss_section(doc, "empty")
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), "c")

  expect_null(lssdoc:::lss_section(doc, "nofields"))
  expect_null(lssdoc:::lss_section(doc, "absent"))
})

test_that("render_questionnaire validates base_size bounds", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  expect_error(render_questionnaire(lss, out, base_size = 99),
               class = "lssdoc_bad_base_size")
  expect_error(render_questionnaire(lss, out, base_size = 3),
               class = "lssdoc_bad_base_size")
  expect_error(render_questionnaire(lss, out, base_size = NA_real_),
               class = "lssdoc_bad_base_size")
})

test_that("render_audit threads font, colors, authors and description overrides", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "audit_demo.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  res <- render_audit(
    read_lss(path), out, chrome_lang = "en",
    font = "Arial", font_code = "Consolas",
    colors = list(primary = "#5C9F1A", accent = "#7FA82E"),
    authors = list(list(name = "Amal Tawfik", affiliation = "HES-SO")),
    description = "Audit run for project XYZ."
  )
  expect_identical(res, out)
  expect_true(file.exists(out))
  txt <- paste(officer::docx_summary(officer::read_docx(out))$text, collapse = " | ")
  expect_true(grepl("Amal Tawfik", txt))
})

test_that("the table template surfaces the raw filter line when asked", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(read_lss(path), out, template = "table",
                       languages = c("fr", "de"), chrome_lang = "en",
                       show_raw_filter = TRUE)
  txt <- paste(officer::docx_summary(officer::read_docx(out))$text, collapse = " | ")
  expect_true(grepl("==", txt, fixed = TRUE))
})
