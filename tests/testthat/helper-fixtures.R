# Shared fixtures, parsed once per session.
#
# `demo_survey.lss` is 367 KB, 47 questions and four languages: parsing it
# costs a couple of seconds, and three quarters of the files here open the
# same file. Parse each shipped example once and hand out the cached object.
# R copies on modification, so a test that edits what it gets never touches
# the cache -- and anything that is not a shipped example is read straight
# through, so a test that writes a file and reads it back is never served a
# stale parse.

lss_fixture_cache <- new.env(parent = emptyenv())

lss_cached <- function(path) {
  key <- basename(path)
  if (!key %in% c("demo_survey.lss", "audit_demo.lss")) return(read_lss(path))
  hit <- lss_fixture_cache[[key]]
  if (is.null(hit)) {
    hit <- read_lss(path)
    lss_fixture_cache[[key]] <- hit
  }
  hit
}

# The four-language demo survey, parsed once.
demo_lss <- function() {
  lss_cached(system.file("extdata", "demo_survey.lss", package = "lssdoc"))
}

# The deliberately flawed eight-question, two-language survey, parsed once.
flawed_lss <- function() {
  lss_cached(system.file("extdata", "audit_demo.lss", package = "lssdoc"))
}

# A real `lss` object small enough to render in a second: the package's own
# example specification cut to a single-choice, a multiple-choice and a free-
# text question,
# written out and read back, in two declared languages. Everything a test
# about the CHROME (the labels lssdoc puts around the content) needs -- a
# cover, a table of contents (which needs two groups, hence the free-text
# question in a family of its own), two type labels, a value list -- without
# the 47 questions that make the demo survey slow to render.
small_lss <- function(languages = c("fr", "de")) {
  key <- paste0("small:", paste(languages, collapse = "-"))
  hit <- lss_fixture_cache[[key]]
  if (is.null(hit)) {
    spec <- lss_example_spec(kinds = c("single", "multiple", "text"),
                             languages = languages)
    file <- tempfile(fileext = ".lss")
    suppressMessages(write_lss(spec, file))
    hit <- read_lss(file)
    unlink(file)
    lss_fixture_cache[[key]] <- hit
  }
  hit
}
