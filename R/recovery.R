#' @importFrom purrr map map_lgl map_chr pmap_chr
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
#' @param ... passed on to `filter_call_stack` options listed below.
#' @param exclude.namespaces The packages from which to exclude calls.
#'           That is functions from these namespaces are not listed.
#'           Default is called from recovery option 'exclude.namespaces',
#'           and defaults to base, magrittr and purrr.
#' @param exclude.function.classes When functions have a class assigned
#'           they may be excluded by class with this argument.
#' @param exclude.same.parent Used to exclude those calls where it's
#'           parent is the same as the callers parent, which mostly
#'           occurs with [eval()] statements.
#' @param exclude.anonymous.functions should anonymous/unnamed functions
#'           be ecxluded? TRUE by default.
#' @param exclude.function.names regular expressions to match function
#'           names that should be excluded.  Default is all functions
#'           beginning with a '.' are hidden.
#'
#' @export
recovery <- function(...){
    nframes <- sys.nframe()
    if (identical(sys.function(nframes), recovery)) nframes <- nframes-1L
    if (!nframes) {
        message("Nothing to recover from")
        return()
    }
    raw.data <- call_stack_data(nframes-1L)
    data <- filter_call_stack(...)
    # keep <- ( data$is.root.call
    #         | !( data$namespace %in% exclude.namespaces
    #            | data$same.parent & exclude.same.parent
    #            | data$fun.class %in% exclude.function.classes
    #            | data$hidden
    #            | (exclude.anonymous.functions & data$call.symbol == .anonymous.call)
    #            | mgrepl(data$call.symbol, exclude.function.names)
    #            )
    #         )
    # frame.numbers <- data[keep, 'call.num']
    label <- pull(transmute(data, recovery_labels(namespace, srcrefs, label)))
    repeat {
        which <- menu(label, title = "\nEnter a frame number, or 0 to exit  ")
        if (which) {
            frame.num <- data$call.num[which]
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
attr(recovery, 'hideFromDebugger') <- TRUE

recovery_labels <- function( namespace, srcrefs, label) {
    paste0('[', namespace, ']'
          , ifelse(is.na(srcrefs), ': ', paste0(srcrefs, ': '))
          , label)
}
if(FALSE){#@testing


}

#' @describeIn recovery
filter_call_stack <-
function( data = call_stack_data(sys.parent())
        , exclude.namespaces          = recovery_opt(exclude.namespaces)
        , exclude.function.classes    = recovery_opt(exclude.function.classes)
        , exclude.same.parent         = recovery_opt(exclude.same.parent)
        , exclude.anonymous.functions = recovery_opt(exclude.anonymous.functions)
        , exclude.function.names      = recovery_opt(exclude.function.names)
        ){
    keep <- !data$hidden &
            ( data$is.root.call
            | !( data$namespace %in% exclude.namespaces
               | data$same.parent & exclude.same.parent
               | data$fun.class %in% exclude.function.classes
               | (exclude.anonymous.functions & data$call.symbol == .anonymous.call)
               | mgrepl(data$call.symbol, exclude.function.names)
               )
            )
    data[keep,]
}
if(FALSE){
    test1 <- structure(function(...){call_stack_data()}
                      , hideFromDebugger=TRUE)
    .test2 <- function(...){test1()}
    test_call_stack_data <- function(...){.test2(...)}

    data <- 3 %>% test_call_stack_data
    if (nrow(data)>11) data <- tail(data, 11)

    expect_equal( nrow(data), 11)
    expect_equal( tail(data, 3)$call.symbol
                , c('test_call_stack_data', '.test2', 'test1'))

    expect_equal(sum(data$hidden), 1)

    # period functions
    filtered <- filter_call_stack( data
                                 , .options$get_default('exclude.namespaces')
                                 , .options$get_default('exclude.function.classes')
                                 , .options$get_default('exclude.same.parent')
                                 , .options$get_default('exclude.anonymous.functions')
                                 , .options$get_default('exclude.function.names')
                                 )
    expect_equal(nrow(filtered), 2L)
    expect_equal(filtered$label, c( '3 %>% test_call_stack_data'
                                  , 'test_call_stack_data(.)'
                                  ))



}


.options <- new_option_set('recovery', defaults =
    list( exclude.namespaces = c('base', 'magrittr', 'purrr')
        , exclude.same.parent = TRUE
        , exclude.function.classes = 'fseq'
        , exclude.anonymous.functions = TRUE
        , exclude.function.names = '^\\.'
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

