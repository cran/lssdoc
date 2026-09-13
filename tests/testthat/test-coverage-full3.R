# Additional coverage for reachable branches missed by earlier batches.
# Each test targets a specific uncovered region identified from covr:
# the read_lss malformed-XML fallback, the cover admin/privacy metadata
# rows (gated by show_admin_settings / show_privacy_settings), and the
# table template rendering of the demo survey.

test_that("read_lss reports malformed XML that starts with a tag", {
  # Passes the byte-level pre-check (first non-ws byte is '<') but xml2
  # cannot parse it, so the tryCatch fallback abort fires.
  tmp <- tempfile(fileext = ".lss")
  writeLines("<LimeSurveyDocType>Survey<unterminated", tmp)
  expect_error(read_lss(tmp), class = "lssdoc_invalid_xml")

  tmp2 <- tempfile(fileext = ".lss")
  writeLines("<a><b></a>", tmp2)  # mismatched tags
  expect_error(read_lss(tmp2), class = "lssdoc_invalid_xml")
})

test_that("cover page renders admin and privacy settings when requested", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  lss <- read_lss(system.file("extdata", "demo_survey.lss", package = "lssdoc"))

  # Admin settings live on survey_language_settings (alias / end URL /
  # end URL description) and surveys (active). Inject them on every row
  # so the primary-language lookup finds them.
  ls <- lss$survey_language_settings
  ls$surveyls_alias <- "PILOT-2026"
  ls$surveyls_url <- "https://example.org/thanks"
  ls$surveyls_urldescription <- "Return to portal"
  lss$survey_language_settings <- ls

  sv <- lss$surveys
  sv$active <- "Y"
  sv$anonymized <- "Y"
  sv$save <- "Y"
  sv$datestamp <- "Y"
  sv$ipaddr <- "N"
  sv$refurl <- "N"
  lss$surveys <- sv

  out <- tempfile(fileext = ".docx")
  expect_no_error(
    render_questionnaire(
      lss, out,
      languages             = c("en", "fr"),
      template              = "table",
      show_admin_settings   = TRUE,
      show_privacy_settings = TRUE,
      authors = list(
        list(name = "Jane Doe", affiliation = "HESAV",
             orcid = "0009-0001-2345-6789"),
        list(name = "John Roe")  # no affiliation, no orcid
      ),
      description = paste0(
        "Validated for the pilot. See https://example.org/projects/xyz ",
        "for the protocol."
      )
    )
  )
  expect_true(file.exists(out))
})

test_that("table template renders the full demo across four languages", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  lss <- read_lss(system.file("extdata", "demo_survey.lss", package = "lssdoc"))
  langs <- intersect(c("en", "fr", "de", "es", "it"), lss$languages)[1:4]
  langs <- langs[!is.na(langs)]

  out <- tempfile(fileext = ".docx")
  expect_no_error(
    render_questionnaire(
      lss, out,
      languages        = langs,
      template         = "table",
      chrome_lang      = "en",
      show_welcome     = TRUE,
      show_endtext     = TRUE,
      show_description = TRUE,
      show_groups      = TRUE,
      show_toc         = TRUE,
      show_audit       = TRUE,
      show_index       = TRUE
    )
  )
  expect_true(file.exists(out))
})
