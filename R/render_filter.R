# Filter humanizer for LimeSurvey relevance expressions.
#
# Extracted from R/render_lss_docx.R; the functions here translate the
# raw LimeSurvey conditional syntax (e.g. !is_empty(Q1.NAOK) && (Q1.NAOK
# == 1)) into the editorial mathematical form (Q1 = 1) used in the
# Filter cell of every rendered question. Audited and tested in
# tests/testthat/test-humanize_relevance.R.

#' Best-effort translation of a LimeSurvey relevance expression into a
#' human-readable form.
#'
#' Recognized patterns:
#' * `is_empty(X.NAOK)` -> "X is empty"
#' * `!is_empty(X.NAOK)` -> "X is answered"
#' * `strlen(X.NAOK) == 0` -> "X is empty"; `strlen(X.NAOK) > 0` ->
#'   "X is answered" (the same predicates written via string length)
#' * `intval(X.NAOK) OP N` -> "X OP N" (the integer cast is invisible
#'   to a methodologist)
#' * `regexMatch("pat", X.NAOK)` -> "X matches \"pat\""
#' * `that.X.NAOK` -> "X" (LimeSurvey group references lose the
#'   structural prefix)
#' * `X.NAOK == N` -> "X = N"; `X.NAOK != N` -> "X \u2260 N";
#'   `X.NAOK <= N` -> "X \u2264 N" (idem `>=`, `<`, `>`)
#' * `&&` -> "AND"; `||` -> "OR"
#' * Multiple disjunctions on the **same variable** collapse to set
#'   notation: `X == 1 || X == 2 || X == 3` -> "X \u2208 {1, 2, 3}"
#' * Two bounds on the **same variable** collapse to an encased range:
#'   `X >= 18 && X <= 65` -> "18 \u2264 X \u2264 65"
#'
#' All Boolean and predicate tokens (`AND`, `OR`, `is answered`,
#' `is empty`, `matches`) localize via `theme$chrome$filter_*` when a
#' theme is supplied. The function strips balanced outer parentheses.
#' When the expression cannot be matched it is returned unchanged so
#' the raw text is never lost.
#'
#' @param x A character relevance expression as stored in LimeSurvey.
#' @param theme Optional theme list. When `NULL`, English chrome is used.
#' @return A single human-readable string. The localized "All" token for
#'   `1`, empty, or `NA`.
#' @keywords internal
#' @noRd
lss_humanize_relevance <- function(x, theme = NULL) {
  chrome <- lss_filter_chrome(theme)
  if (is.null(x) || is.na(x) || !nzchar(x) || identical(x, "1")) {
    return(chrome$all)
  }
  s <- as.character(x)
  s <- lss_strip_outer_parens(s)

  # Step 0: preprocess LimeSurvey wrappers and group references so the
  # subsequent steps see canonical `X.NAOK` references.
  # - `that.Q1.NAOK` is a group reference (LimeSurvey routes inside
  #   one group via `that.code`); the prefix is structural noise.
  # - `intval(X.NAOK)` wraps a string value into an integer for
  #   comparison; the cast is invisible to a methodologist.
  # - `regexMatch("pat", X.NAOK)` becomes `X matches "pat"` so the
  #   reader sees a predicate, not a function call.
  # - `strlen(X.NAOK) > 0` is the LimeSurvey idiom for "answered";
  #   `strlen(X.NAOK) == 0` for "empty". Map both to the same chrome
  #   strings as `is_empty()`.
  s <- gsub("\\bthat\\.([A-Za-z0-9_]+)", "\\1", s, perl = TRUE)
  s <- gsub("\\bintval\\s*\\(\\s*([A-Za-z0-9_]+\\.NAOK)\\s*\\)", "\\1",
            s, perl = TRUE)
  s <- gsub("\\bregexMatch\\s*\\(\\s*\"([^\"]*)\"\\s*,\\s*([A-Za-z0-9_]+)\\.NAOK\\s*\\)",
            paste0("\\2 ", chrome$matches, " \"\\1\""), s, perl = TRUE)
  s <- gsub("\\bstrlen\\s*\\(\\s*([A-Za-z0-9_]+)\\.NAOK\\s*\\)\\s*>\\s*0",
            paste0("\\1 ", chrome$answered), s, perl = TRUE)
  s <- gsub("\\bstrlen\\s*\\(\\s*([A-Za-z0-9_]+)\\.NAOK\\s*\\)\\s*(==|<=)\\s*0",
            paste0("\\1 ", chrome$empty), s, perl = TRUE)

  # Step 1: collapse the LimeSurvey "answered-and-equals" idiom on the
  # SAME variable. LimeSurvey's conditional designer emits
  # `!is_empty(X.NAOK) && (X.NAOK OP value)` as a defensive guard;
  # for human review the guard is noise so we drop it.
  idiom_left <- paste0(
    "!\\s*is_empty\\(([A-Za-z0-9_]+)\\.NAOK\\)\\s*&&\\s*",
    "\\(\\s*\\1\\.NAOK\\s*(==|!=|>=|<=|>|<)\\s*([^)&|]+)\\s*\\)"
  )
  idiom_right <- paste0(
    "\\(\\s*([A-Za-z0-9_]+)\\.NAOK\\s*(==|!=|>=|<=|>|<)\\s*([^)&|]+)\\s*\\)",
    "\\s*&&\\s*!\\s*is_empty\\(\\1\\.NAOK\\)"
  )
  for (i in seq_len(5L)) {
    before <- s
    s <- gsub(idiom_left, "\\1.NAOK \\2 \\3", s, perl = TRUE)
    s <- gsub(idiom_right, "\\1.NAOK \\2 \\3", s, perl = TRUE)
    if (identical(s, before)) break
  }

  # Step 1b: drop the quotes LimeSurvey puts around a scalar answer code
  # in a comparison (`X.NAOK == "1"`). They are string literals in the
  # Expression Manager, but a bare code reads cleaner for a reviewer
  # (`workstatus = 1`, not `workstatus = "1"`) and the Value section
  # already documents the code. Quotes are kept around values that contain
  # whitespace (so multi-word strings stay delimited) and around regex
  # patterns (`X matches "pat"`, already converted in step 0 and never
  # adjacent to a comparison operator, so untouched here).
  s <- gsub("(\\.NAOK\\s*(?:==|!=|>=|<=|>|<)\\s*)\"([^\"\\s]+)\"",
            "\\1\\2", s, perl = TRUE)

  # Step 2: collapse `X == a || X == b || X == c` (same variable)
  # to "X \u2208 {a, b, c}". Same for negation with `!=` and `&&` ->
  # "X \u2209 {a, b, c}". Repeat while the pattern keeps shrinking
  # so chained sets of any length collapse.
  for (i in seq_len(8L)) {
    before <- s
    s <- lss_collapse_set(s, op = "==", join = "||")
    s <- lss_collapse_set(s, op = "!=", join = "&&")
    if (identical(s, before)) break
  }

  # Step 3: collapse `X >= a && X <= b` (same variable) to
  # "a \u2264 X \u2264 b" (and the strict variants).
  s <- lss_collapse_range(s)

  # Step 4: predicate forms.
  s <- gsub("!\\s*is_empty\\(([A-Za-z0-9_]+)\\.NAOK\\)",
            paste0("\\1 ", chrome$answered), s, perl = TRUE)
  s <- gsub("\\bis_empty\\(([A-Za-z0-9_]+)\\.NAOK\\)",
            paste0("\\1 ", chrome$empty), s, perl = TRUE)
  s <- gsub("([A-Za-z0-9_]+)\\.NAOK", "\\1", s, perl = TRUE)
  s <- gsub("\\s*&&\\s*", paste0(" ", chrome$and, " "), s)
  s <- gsub("\\s*\\|\\|\\s*", paste0(" ", chrome$or, " "), s)
  # Step 5: comparison operators rendered with Unicode math symbols
  # so they read at a glance for a methodologist: U+2260 (\u2260),
  # U+2264 (\u2264), U+2265 (\u2265). Order matters: substitute the
  # two-character forms first so the single `==` rule does not consume
  # the `=` of `!=` / `<=` / `>=`.
  s <- gsub("\\s*!=\\s*", " \u2260 ", s)
  s <- gsub("\\s*<=\\s*", " \u2264 ", s)
  s <- gsub("\\s*>=\\s*", " \u2265 ", s)
  s <- gsub("\\s*==\\s*", " = ", s)
  s <- lss_strip_outer_parens(s)
  trimws(s)
}

#' Localized filter token chrome with English fallback
#'
#' @keywords internal
#' @noRd
lss_filter_chrome <- function(theme = NULL) {
  defaults <- list(
    all      = "All",
    and      = "AND",
    or       = "OR",
    answered = "is answered",
    empty    = "is empty",
    matches  = "matches",
    inset    = "\u2208",
    notinset = "\u2209"
  )
  if (is.null(theme) || is.null(theme$chrome)) return(defaults)
  pick <- function(key, fallback) {
    v <- theme$chrome[[paste0("filter_", key)]]
    if (is.null(v) || !nzchar(v)) fallback else v
  }
  list(
    all      = pick("all",      defaults$all),
    and      = pick("and",      defaults$and),
    or       = pick("or",       defaults$or),
    answered = pick("answered", defaults$answered),
    empty    = pick("empty",    defaults$empty),
    matches  = pick("matches",  defaults$matches),
    inset    = defaults$inset,
    notinset = defaults$notinset
  )
}

#' Collapse repeated `X OP a JOIN X OP b JOIN X OP c` on the same
#' variable into set notation `X \u2208 {a, b, c}` (or `\u2209` when
#' negation).
#'
#' @keywords internal
#' @noRd
lss_collapse_set <- function(s, op, join) {
  # Match the simplest 2-term pair first; the outer loop in the caller
  # extends the captured set on subsequent iterations by re-matching
  # the produced set against another `X == v` clause.
  var_pat <- "([A-Za-z0-9_]+)(?:\\.NAOK)?"
  val_pat <- "([^)&|\\s]+)"
  op_re <- gsub("([=!<>])", "\\\\\\1", op, perl = TRUE)
  join_re <- if (identical(join, "||")) "\\|\\|" else "&&"
  # Pair pattern: X OP a JOIN X OP b
  pair_re <- paste0(
    "\\b", var_pat, "\\s*", op_re, "\\s*", val_pat,
    "\\s*", join_re, "\\s*",
    "\\b\\1(?:\\.NAOK)?", "\\s*", op_re, "\\s*", val_pat
  )
  # When the rhs is already an existing set marker (built on a previous
  # iteration), append the next value into the set.
  set_token <- if (identical(op, "==")) "\u2208" else "\u2209"
  s <- gsub(
    pair_re,
    paste0("\\1 ", set_token, " {\\2, \\3}"),
    s, perl = TRUE
  )
  # Extension pattern: existing set `X \u2208 {...}` JOIN `X OP v`
  ext_re <- paste0(
    "\\b", var_pat, "\\s+", set_token, "\\s+\\{([^}]+)\\}",
    "\\s*", join_re, "\\s*",
    "\\b\\1(?:\\.NAOK)?", "\\s*", op_re, "\\s*", val_pat
  )
  s <- gsub(
    ext_re,
    paste0("\\1 ", set_token, " {\\2, \\3}"),
    s, perl = TRUE
  )
  s
}

#' Collapse `X >= a && X <= b` (same variable) to `a \u2264 X \u2264 b`.
#' Supports the four combinations of strict / non-strict bounds.
#'
#' @keywords internal
#' @noRd
lss_collapse_range <- function(s) {
  var_pat <- "([A-Za-z0-9_]+)(?:\\.NAOK)?"
  val_pat <- "([^)&|\\s]+)"
  # X (>=|>) a && X (<=|<) b -> a (\u2264|<) X (\u2264|<) b
  patterns <- list(
    c(">=", "<=", "\u2264", "\u2264"),
    c(">=", "<",  "\u2264", "<"),
    c(">",  "<=", "<",      "\u2264"),
    c(">",  "<",  "<",      "<")
  )
  for (p in patterns) {
    lo_op <- p[1]; hi_op <- p[2]; lo_disp <- p[3]; hi_disp <- p[4]
    lo_re <- gsub("([=<>])", "\\\\\\1", lo_op, perl = TRUE)
    hi_re <- gsub("([=<>])", "\\\\\\1", hi_op, perl = TRUE)
    pat <- paste0(
      "\\b", var_pat, "\\s*", lo_re, "\\s*", val_pat,
      "\\s*&&\\s*",
      "\\b\\1(?:\\.NAOK)?", "\\s*", hi_re, "\\s*", val_pat
    )
    s <- gsub(
      pat,
      paste0("\\2 ", lo_disp, " \\1 ", hi_disp, " \\3"),
      s, perl = TRUE
    )
  }
  s
}

#' Strip balanced outer parentheses up to a few levels deep
#' @keywords internal
#' @noRd
lss_strip_outer_parens <- function(s) {
  for (i in seq_len(8L)) {
    inner <- sub("^\\s*\\((.*)\\)\\s*$", "\\1", s, perl = TRUE)
    if (identical(inner, s) || !lss_parens_balanced(inner)) break
    s <- inner
  }
  s
}

#' Check whether parentheses are balanced in a string
#' @keywords internal
#' @noRd
lss_parens_balanced <- function(s) {
  depth <- 0L
  for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
    if (ch == "(") {
      depth <- depth + 1L
    } else if (ch == ")") {
      depth <- depth - 1L
      if (depth < 0L) return(FALSE)
    }
  }
  depth == 0L
}

