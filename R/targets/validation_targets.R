#' Validation Targets
#'
#' @description
#' Factory function that creates standalone validation targets.
#'
#' @name validation_targets
NULL

#' Create validation targets
#'
#' @description
#' Creates standalone validation targets that span multiple subprocesses.
#' Most subprocess-specific validation is handled within the relevant factory.
#'
#' @return List of validation targets
#'
#' @export
create_validation_targets <- function() {
  list(
    # Placeholder for any cross-cutting validation targets
    # Most validation targets are included in their respective subprocess factories
    NULL
  )
}
