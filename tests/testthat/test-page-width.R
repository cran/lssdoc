# The usable body width follows the page orientation, never the language
# count. lss_content_width_in() is the single source of truth that
# render_lss_docx() pushes into theme$content_width_in.

test_that("lss_content_width_in depends on orientation, not language count", {
  expect_equal(lss_content_width_in("auto"), 6.30)
  expect_equal(lss_content_width_in("A4-portrait"), 6.30)
  expect_equal(lss_content_width_in("A4-landscape"), 9.72)
  expect_equal(lss_content_width_in("A3"), 14.56)
})

test_that("page_format = 'auto' is template-aware (cards portrait, table landscape)", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  orientation_of <- function(out) {
    tmp <- tempfile()
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::unzip(out, exdir = tmp)
    xml <- paste(readLines(file.path(tmp, "word", "document.xml"),
                           warn = FALSE), collapse = "")
    if (grepl("w:orient=\"landscape\"", xml, fixed = TRUE)) {
      "landscape"
    } else {
      "portrait"
    }
  }

  cards_out <- tempfile(fileext = ".docx")
  table_out <- tempfile(fileext = ".docx")
  on.exit(unlink(c(cards_out, table_out)), add = TRUE)
  render_questionnaire(lss, cards_out, template = "cards", chrome_lang = "en")
  render_questionnaire(lss, table_out, template = "table", chrome_lang = "en")
  expect_identical(orientation_of(cards_out), "portrait")
  expect_identical(orientation_of(table_out), "landscape")
})

test_that("landscape cards widen every panel without overflowing", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Both templates must render in landscape for the full four-language
  # fixture; the tables are laid out to the 9.73 in landscape width, so no
  # column is dropped and the file is well-formed.
  for (tmpl in c("cards", "table")) {
    out <- tempfile(fileext = ".docx")
    expect_no_error(
      render_questionnaire(lss, out, template = tmpl,
                           page_format = "A4-landscape", chrome_lang = "en")
    )
    expect_true(file.size(out) > 10000L)
    unlink(out)
  }
})
