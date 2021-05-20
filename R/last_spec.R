# Adapted from ggplot2::last_plot()

.spec_store <- function() {
  .last_spec <- NULL

  list(
    get = function() .last_spec,
    set = function(value) .last_spec <<- value
  )
}
.store <- .spec_store()

#' Set the spec to be fetched by last_spec()
#'
#' @seealso [last_spec()]
#' @export
#' @keywords internal
set_last_spec <- function(value) .store$set(value)


#' Retrieve the spec from the last time Voyager ran.
#'
#' @export
#' @keywords internal
last_spec <- function() .store$get()
