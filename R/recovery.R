#' @importFrom purrr map map_lgl map_chr
#' @importFrom pkgcond %!in% %<<% %<<<%
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
#'           Default is called from recovery option 'exclude.namespaces',
#'           and defaults to base, magrittr and purrr.
#' @param exclude.function.classes When functions have a class assigned
#'           they may be excluded by class with this argument.
#' @param exclude.same.parent Used to exclude those calls where it's
#'           parent is the same as the callers parent, which mostly
#'           occurs with [eval()] statements.
#'
#'
#' @export
recovery <-
function( exclude.namespaces = recovery_opt(exclude.namespaces)
        , exclude.function.classes = recovery_opt(exclude.function.classes)
        , exclude.same.parent = recovery_opt(exclude.same.parent)
        ){
    nframes <- sys.nframe()
    if (identical(sys.function(nframes), recovery)) nframes <- nframes-1L
    if (!nframes) {
        message("Nothing to recover from")
        return()
    }
    data <- call_stack_data(n)
    keep <- ( data$is.root.call
            | !( data$namespace %in% exclude.namespaces
               | data$same.parent & exclude.same.parent
               | data$fun.class %in% exclude.function.classes
               )
            )
    frame.numbers <- data[keep, 'call.num']
    label <- with(data[keep,], {
            paste0('[', namespace, ']'
                  , ifelse(is.na(srcrefs), ': ', paste0(srcrefs, ': '))
                  , label)
    })
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





.options <- new_option_set('recovery', defaults =
    list( exclude.namespaces = c('base', 'magrittr', 'purrr')
        , exclude.same.parent = TRUE
        , exclude.function.classes = 'fseq'
        ))

#' Recovery Options
#'
#' Get or set options for recovery package functions.
#'
#' @param name name of the option to retrieve, given as a raw string or
#'             an unevaluated symbol
#' @param class name of the class(es) to ignore for [recovery()].
#' @param ns name of packages to ignore for [recovery()].
#'
#' @export
recovery_opt <- function(name){
    .options$get(deparse(substitute(name)))
}
#' @describeIn recovery_opt Add a class to types of functions ignored on recovery.
#' @export
recovery_ignore_function_class <- function(class).options$add('exclude.namespaces', class)
#' @describeIn recovery_opt Add a package/namespace to those ignored on recovery.
#' @export
recovery_ignore_namespace <- function(ns)


.onAttach <- function(libname, pkgname){
    .options$reset_all()
    options(error=recovery)
}

