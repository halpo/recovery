#' @importFrom purrr map map_lgl map_chr
#' @importFrom pkgcond %!in%
#' @importFrom dplyr coalesce lag
#' @importFrom utils head tail menu limitedLabels packageName
#' @importFrom methods getPackageName

NULL

#' Recover from an Error or Condition
#'
#' Use this as a replacement for [base::recover()].
#' This has the advantage of improved labels as well as being able to
#' exclude calls from specific packages, such as purrr or magrittr
#' which tend to produce many extra calls irrelevent to debugging.
#'
#' @param exclude.namespaces The packages from which to exclude calls.
#'           That is functions from these namespaces are not listed.
#'           Default is called from option 'recovery.exclude.namespaces',
#'           and defaults to base, magrittr and purrr.
#'
#'
#' @export
recovery <-
function(exclude.namespaces = getOption("recovery.exclude.namespaces", c('base', 'magrittr', 'purrr'))){
    nframes <- sys.nframe()
    if (identical(sys.function(nframes), recovery)) nframes <- nframes-1L
    if (!nframes) {
        message("Nothing to recover from")
        return()
    }
    pkgs <- seq(nframes) %>% map(sys.function) %>% map(environment) %>%
        map(topenv) %>% map_chr(get_namespace_name)
    calls <- sys.calls()[seq.int(nframes)]

    x <- ((pkgs %!in% exclude.namespaces) | (seq.int(nframes) == 1L))


    frame.numbers <- seq.int(nframes)[x]

    sys.functions <- map(frame.numbers, sys.function)

    hide <- map_lgl(map(sys.functions, attr, 'hideFromDebugger'), isTRUE)
    if(any(hide)){
        x <- x[!hide]
        frame.numbers <- frame.numbers[!hide]
    }
    # call.labels <- map_chr(frame.numbers, call_label, width=50)
    # srcrefs <- map_chr(calls[x], get_call_srcref)
    # calling.package <- coalesce(head(lag(pkgs),nframes)[x], 'R_GlobalEnv')
    # label <- paste0('[', pkgs[x], ']', ifelse(is.na(srcrefs), ': ', paste0(srcrefs, ': ')), call.labels)

    repeat {
        which <- menu(label, title = "\nEnter a frame number, or 0 to exit  ")
        if (which) {
            frame.num <- frame.numbers[which]
            eval( substitute(browser(skipCalls = skip), list(skip =7-frame.num))
                , envir = sys.frame(frame.num)
                )
        } else break
    }
}
if(FALSE){# development

    test_recovery <- function(...)recovery()

    test_recovery()

    withr::with_options(list(happy='day'), {
        walk(1, test_recovery)
    })

    withr::with_options(list(error=recovery), {
        throws_error <- function(...)stop("an error message")
        (1:10) %>% map(throws_error, another.argument = "that's waaaaaaaaaaaaaay too long")
    })
}



.onAttach <- function(libname, pkgname){
    options(error=recovery)
}

