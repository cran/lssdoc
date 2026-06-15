test_that("the taxonomy is well formed", {
  tx <- lss_question_types()
  expect_true(all(
    c(
      "code", "label", "family",
      "has_answers", "has_subquestions", "has_scales", "display_only"
    ) %in% names(tx)
  ))
  expect_false(anyNA(tx$code))
  expect_false(any(duplicated(tx$code)))
  expect_type(tx$has_answers, "logical")
  expect_false(anyNA(tx[c("has_answers", "has_subquestions", "has_scales", "display_only")]))
})

test_that("structural flags match known types", {
  expect_true(lss_type_info("L")$has_answers)
  expect_false(lss_type_info("L")$has_subquestions)
  expect_true(lss_type_info("M")$has_subquestions)
  expect_true(lss_type_info("1")$has_scales)
  expect_true(lss_type_info("X")$display_only)
  expect_identical(lss_type_info("F")$family, "array")
})

test_that("lss_type_label is vectorized and degrades for unknown codes", {
  expect_identical(
    lss_type_label(c("L", "M", "Z")),
    c("List (radio)", "Multiple choice", "Unknown type (Z)")
  )
})

test_that("lss_question_label prefers the type code then the theme name", {
  # Known legacy code wins.
  expect_identical(lss_question_label("L", "listradio"), "List (radio)")
  # Unknown code falls back to a known theme name (plugin types).
  expect_identical(lss_question_label(NA, "ranking"), "Ranking")
  # Unknown code and unknown theme name still name both, never drops.
  expect_identical(
    lss_question_label("Z", "plugin_fancy"),
    "Unknown type (Z / plugin_fancy)"
  )
})

test_that("every question type in the bundled fixtures is recognized", {
  for (file in c("demo_survey.lss", "audit_demo.lss")) {
    path <- system.file("extdata", file, package = "lssdoc")
    skip_if_not(file.exists(path))
    lss <- read_lss(path)
    labels <- lss_question_label(
      lss$questions$type,
      lss$questions$question_theme_name
    )
    expect_false(any(grepl("Unknown", labels)), info = file)
  }
})

test_that("lss_methodological_label collapses the LimeSurvey taxonomy", {
  # Single-choice family: radios, dropdowns, predefined Y/N, gender,
  # 5-point and array all read as "Single choice" -- the response
  # domain (Y/N, M/F, 1..5, custom codes) lives in the Value section,
  # not in the Type cell.
  for (code in c("L", "!", "Y", "G", "5", "F", "1")) {
    expect_identical(
      lss_methodological_label(code), "Single choice",
      info = sprintf("type %s should collapse to 'Single choice'", code)
    )
  }
  expect_identical(lss_methodological_label("O"), "Single choice with comment")
  expect_identical(lss_methodological_label("M"), "Multiple choice")
  expect_identical(lss_methodological_label("P"), "Multiple choice with comment")
  expect_identical(lss_methodological_label("N"), "Number")
  expect_identical(lss_methodological_label("K"), "Number")
  # S (short free text) is single-line; T (long) and U (huge) free text
  # are both multi-line and collapse to one label -- they differ only in
  # textarea size.
  expect_identical(lss_methodological_label("S"), "Text (short)")
  expect_identical(lss_methodological_label("T"), "Text (long)")
  expect_identical(lss_methodological_label("U"), "Text (long)")
  expect_identical(lss_methodological_label("D"), "Date")
  expect_identical(lss_methodological_label("R"), "Ranking")
  expect_identical(lss_methodological_label("|"), "File upload")
  expect_identical(lss_methodological_label("*"), "Computed")
  expect_identical(lss_methodological_label("X"), "Display")
})

test_that("lss_methodological_label is vectorized and falls back for unknowns", {
  out <- lss_methodological_label(c("L", "M", "N", "T"))
  expect_identical(out, c("Single choice", "Multiple choice", "Number", "Text (long)"))
  # Unknown legacy code falls back to the theme name via the legacy
  # label function -- never drops the question silently.
  expect_identical(
    lss_methodological_label(NA, "ranking"),
    "Ranking"
  )
  expect_identical(
    lss_methodological_label("Z", "plugin_fancy"),
    "Unknown type (Z / plugin_fancy)"
  )
})
