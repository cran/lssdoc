# Unit tests for the small render_utils helpers, plus an opt-in
# end-to-end PDF test that runs only when LibreOffice is installed.

test_that("lss_truncate_title handles empty, short and over-budget text", {
  expect_identical(lss_truncate_title(NULL), "")
  expect_identical(lss_truncate_title(NA_character_), "")
  expect_identical(lss_truncate_title(""), "")
  expect_identical(lss_truncate_title("short title"), "short title")
  out <- lss_truncate_title(strrep("a", 100), max_chars = 80L)
  expect_equal(nchar(out), 80L)
  expect_true(endsWith(out, "..."))
})

test_that("lss_yes_no localizes Y/N/S and passes other values through", {
  expect_identical(lss_yes_no(NA_character_), "—")
  expect_identical(lss_yes_no(""), "—")
  expect_identical(lss_yes_no("Y"), "Yes")
  expect_identical(lss_yes_no("N"), "No")
  expect_identical(lss_yes_no("S"), "No")          # soft mandatory -> No
  expect_identical(lss_yes_no("weird"), "weird")   # unknown -> passthrough
  theme <- lss_render_theme()
  theme$chrome <- lss_chrome_strings("fr")
  expect_identical(lss_yes_no("Y", theme), theme$chrome$mandatory_yes)
})

test_that("lss_strip_group_number_prefix removes common author numbering", {
  expect_identical(lss_strip_group_number_prefix(NA_character_), NA_character_)
  expect_identical(lss_strip_group_number_prefix("1. Studies"), "Studies")
  expect_identical(lss_strip_group_number_prefix("2) Health"), "Health")
  expect_identical(lss_strip_group_number_prefix("3 - Work"), "Work")
  expect_identical(lss_strip_group_number_prefix("4: Income"), "Income")
  expect_identical(lss_strip_group_number_prefix("1.2. Sub"), "Sub")
  expect_identical(
    lss_strip_group_number_prefix("Section A - Demographics"), "Demographics"
  )
  expect_identical(
    lss_strip_group_number_prefix("No prefix here"), "No prefix here"
  )
})

test_that("lss_language_label maps known codes and passes unknown ones through", {
  expect_identical(lss_language_label("fr"), "Français")
  expect_identical(lss_language_label("xx"), "xx")   # unknown -> passthrough
})

test_that("lss_compose renders lists, super/subscript and an empty placeholder", {
  skip_if_not_installed("flextable")
  theme <- lss_render_theme()
  expect_no_error(lss_compose("", theme))                              # empty -> em-dash
  expect_no_error(lss_compose("<ul><li>one</li><li>two</li></ul>", theme))
  expect_no_error(lss_compose("<ol><li>a</li><li>b</li></ol>", theme))
  expect_no_error(lss_compose("E = mc<sup>2</sup>", theme))
  expect_no_error(lss_compose("H<sub>2</sub>O", theme))
  expect_no_error(lss_compose_plain("", theme))                        # empty -> em-dash
  expect_no_error(lss_compose_plain("plain text", theme))
})

test_that("render_questionnaire produces a real PDF when LibreOffice is available", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if(is.null(lss_find_soffice()), "LibreOffice not installed")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(path, out, chrome_lang = "en", languages = c("fr", "de"))
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 1000)
})
