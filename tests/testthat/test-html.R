test_that("empty, NA, and whitespace inputs yield no blocks", {
  expect_length(lss_html_to_blocks(NULL), 0L)
  expect_length(lss_html_to_blocks(NA_character_), 0L)
  expect_length(lss_html_to_blocks(""), 0L)
  expect_length(lss_html_to_blocks("   \n  "), 0L)
  expect_identical(lss_html_to_text(NA_character_), "")
})

test_that("plain text becomes a single paragraph", {
  blocks <- lss_html_to_blocks("Just plain text")
  expect_length(blocks, 1L)
  expect_identical(blocks[[1]]$type, "paragraph")
  expect_identical(blocks[[1]]$runs[[1]]$text, "Just plain text")
})

test_that("inline formatting marks the right runs", {
  blocks <- lss_html_to_blocks(
    "Quel mode <strong>principalement </strong>pour venir ?"
  )
  runs <- blocks[[1]]$runs
  bold <- Filter(function(r) isTRUE(r$bold), runs)
  expect_length(bold, 1L)
  expect_match(bold[[1]]$text, "principalement")

  u <- lss_html_to_blocks("<p>Au <u>cours,</u> ok</p>")[[1]]$runs
  expect_true(any(vapply(u, function(r) isTRUE(r$underline), logical(1))))
})

test_that("HTML entities are decoded", {
  expect_identical(
    lss_html_to_text("Caf&eacute; &amp; th&eacute; &lt;ok&gt;"),
    "Café & thé <ok>"
  )
})

test_that("lists become ordered/unordered list items with nesting level", {
  blocks <- lss_html_to_blocks("<ul><li>Un</li><li>Deux</li></ul>")
  expect_length(blocks, 2L)
  expect_true(all(vapply(blocks, function(b) b$type == "list_item", logical(1))))
  expect_true(all(vapply(blocks, function(b) b$level == 1L, logical(1))))
  expect_false(blocks[[1]]$ordered)

  ol <- lss_html_to_blocks("<ol><li>A</li></ol>")
  expect_true(ol[[1]]$ordered)
})

test_that("br becomes a linebreak run and text flattening keeps it", {
  blocks <- lss_html_to_blocks("Line one<br>line two")
  has_break <- any(vapply(
    blocks[[1]]$runs,
    function(r) isTRUE(r$linebreak),
    logical(1)
  ))
  expect_true(has_break)
  expect_identical(lss_html_to_text("Line one<br>line two"), "Line one\nline two")
})

test_that("unknown inline tags stay transparent", {
  blocks <- lss_html_to_blocks('Go <a href="x">here</a> now <span>ok</span>')
  expect_match(lss_html_to_text('Go <a href="x">here</a> now'), "Go here now")
  expect_true(length(blocks) >= 1L)
})

test_that("paragraphs separate blocks and text lines", {
  expect_identical(
    lss_html_to_text("<p>One</p><p>Two</p>"),
    "One\nTwo"
  )
})
