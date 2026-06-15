# Targeted tests that exercise render and helper branches the main
# suite leaves uncovered: the PDF converter guards, the methodological
# label edge cases, the audit printer with findings, and a rich cover
# (authors + ORCID, description with a URL, admin + privacy settings,
# per-item headings with audit markers, attributes, raw filter).

# ---- .docx_to_pdf guards and soffice discovery ---------------------

test_that(".docx_to_pdf rejects a missing .docx and a bad pdf path", {
  expect_error(
    .docx_to_pdf("does-not-exist.docx", tempfile(fileext = ".pdf")),
    class = "lssdoc_bad_input"
  )
  docx <- tempfile(fileext = ".docx")
  writeLines("not really a docx", docx)
  on.exit(unlink(docx), add = TRUE)
  expect_error(
    .docx_to_pdf(docx, NA_character_),
    class = "lssdoc_bad_output"
  )
})

test_that(".docx_to_pdf gives an actionable error when LibreOffice is absent", {
  docx <- tempfile(fileext = ".docx")
  writeLines("not really a docx", docx)
  on.exit(unlink(docx), add = TRUE)
  testthat::local_mocked_bindings(lss_find_soffice = function() NULL)
  expect_error(
    .docx_to_pdf(docx, tempfile(fileext = ".pdf")),
    class = "lssdoc_missing_soffice"
  )
})

test_that("lss_find_soffice returns NULL or a single existing path", {
  x <- lss_find_soffice()
  expect_true(is.null(x) || (is.character(x) && length(x) == 1L && file.exists(x)))
})

# ---- Methodological / legacy label edge cases ----------------------

test_that("lss_question_label degrades to 'Unknown type' with no usable input", {
  expect_identical(lss_question_label(NA_character_), "Unknown type")
})

test_that("lss_methodological_label maps the array and misc legacy codes", {
  out <- lss_methodological_label(c("A", "B", "C", "E", "H", ":", "Q", ";", "I"))
  expect_identical(
    out,
    c("Single choice", "Single choice", "Single choice", "Single choice",
      "Single choice", "Single choice", "Text", "Text", "Display")
  )
})

test_that("lss_type_info returns a default 'other' row for an unknown code", {
  info <- lss_type_info("ZZ")
  expect_identical(info$family, "other")
  expect_false(info$has_answers)
  expect_match(info$label, "Unknown type")
})

# ---- print.lss_audit with findings ---------------------------------

test_that("print.lss_audit lists findings and respects the n cap", {
  path <- system.file("extdata", "demo_survey.lss",
                      package = "lssdoc")
  skip_if_not(file.exists(path))
  au <- audit_lss(read_lss(path))
  skip_if(au$n_findings == 0L, "fixture has no findings to print")
  out <- cli::cli_fmt(print(au, n = 1L))
  expect_true(any(grepl("finding", out)))
  # The cap leaves the rest summarized rather than printed in full.
  if (au$n_findings > 1L) {
    expect_true(any(grepl("more|as.data.frame", out)))
  }
})

# ---- Rich cover + per-item headings + attributes -------------------

test_that("a render with authors, description, admin/privacy and item headings succeeds", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss",
                      package = "lssdoc")
  skip_if_not(file.exists(path))

  logo <- tempfile(fileext = ".png")
  grDevices::png(logo, width = 60, height = 60); grid::grid.rect(); grDevices::dev.off()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(c(out, logo)), add = TRUE)

  authors <- list(
    list(name = "Ada Lovelace", affiliation = "Analytical Engine Lab",
         orcid = "0000-0002-1825-0097"),
    list(name = "Plain Author", affiliation = "", orcid = "")
  )

  expect_no_error(
    render_questionnaire(
      read_lss(path), out, chrome_lang = "en",
      logo = logo,
      authors = authors,
      description = "Methods note: see https://example.org/doi for details.",
      show_admin_settings = TRUE, show_privacy_settings = TRUE,
      show_item_heading = TRUE, show_audit = TRUE,
      show_technical_attrs = TRUE, show_raw_filter = TRUE
    )
  )
  s <- officer::docx_summary(officer::read_docx(out))
  txt <- paste(s$text[!is.na(s$text)], collapse = " | ")
  expect_true(grepl("Ada Lovelace", txt, fixed = TRUE))
  expect_true(grepl("0000-0002-1825-0097", txt, fixed = TRUE))
})
