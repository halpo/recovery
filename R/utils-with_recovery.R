

#' Run code with recovery
#'
#' @inheritParams withr::with_options
#'
#' @export
with_recovery <- function(code){
    old <- options(error=recovery)
    on.exit(set_options(old))
    force(code)
}


