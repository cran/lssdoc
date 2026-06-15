#' Signal a classed lssdoc error
#'
#' Internal helper used throughout the package to raise errors that
#' downstream code can dispatch on by class rather than by matching the
#' message string. Every error carries the parent class `lssdoc_error`
#' plus a specific leaf class.
#'
#' @param message The error message, passed to [cli::cli_abort()].
#' @param class A character vector of leaf classes prepended to
#'   `lssdoc_error`.
#' @param ... Additional arguments forwarded to [cli::cli_abort()].
#' @param call The calling environment, used to build the error call.
#'
#' @return Never returns; always raises a condition.
#' @keywords internal
#' @noRd
lssdoc_abort <- function(message, class = NULL, ..., call = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = c(class, "lssdoc_error"),
    ...,
    call = call,
    .envir = call
  )
}

#' Signal a classed lssdoc warning
#'
#' Counterpart to `lssdoc_abort()` for recoverable adjustments. Carries the
#' parent class `lssdoc_warning` plus a specific leaf class.
#'
#' @inheritParams lssdoc_abort
#' @return Invisibly `NULL`; called for its side effect.
#' @keywords internal
#' @noRd
lssdoc_warn <- function(message, class = NULL, ..., call = rlang::caller_env()) {
  cli::cli_warn(
    message,
    class = c(class, "lssdoc_warning"),
    ...,
    .envir = call
  )
}
