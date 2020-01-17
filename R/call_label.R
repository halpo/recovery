#' @importFrom magrittr functions

# Utilities -------------------------------------------------------

get_namespace_name <- function(env){
    if (is.function(env)) Recall(topenv(environment(env))) else
    if (isNamespace(env)) packageName(env) else
    if (identical(env, .GlobalEnv)) "R_GlobalEnv" else
    if (identical(env, baseenv())) "base" else
    NA_character_
}
is_infix <- function(sym){
    grepl("^[\\+\\-\\*/\\^%\\|&]", as.character(sym), perl=TRUE)
}
fix_backticks <- function(x)sub("^`([^`]+)`", "\\1", x)


.anonymous.call <- "\u00ABanonymous\u00BB"
get_call_symbol <-
function( call
        , nested = deparse
        , ...
        , anonymous = .anonymous.call
        , other = NA_character_
        ){
    if (is.symbol(call[[1]])) return(as.character(call[[1]])) else
    if (is.function(call[[1]])) return(anonymous) else
    if (is.call(call[[1]]))
        if (is.function(nested)) return(nested(call[[1]])) else
        return(nested)
    return(other)
}
if(FALSE){#@testing
    call <- substitute(a(b))
    expect_equal(get_call_symbol(call), 'a')

    call <- substitute(a(100), list(a=function(n){rnorm(n)}))
    expect_equal(get_call_symbol(call), "\u00ABanonymous\u00BB")
    expect_equal(get_call_symbol(call, anonymous='ANON'), "ANON")

    call <- substitute(a[[1]](b))
    expect_equal(get_call_symbol(call), "a[[1]]")
    expect_equal(get_call_symbol(call, nested=Recall), "[[")
    expect_equal(get_call_symbol(call, nested='nested'), "nested")
}



# Reduce ----------------------------------------------------------
call_label_reduce <-
function( call
        , maxwidth=getOption("width")
        , fixes = fix_backticks ){
    nested.calls <- map_lgl(call, is.call)
    singletons <- !nested.calls
    deparsed <- deparse(call)
    if (sum(nchar(deparsed)) > maxwidth){
        len.parts <- integer(length(call))
        len.parts[singletons] <- nchar(as.character(call))[singletons] +
            ifelse( which(singletons) == 1L
                  | which(singletons) == length(call)
                  , ifelse( map_lgl(call[singletons], is_infix)
                          , 2L, 1L)
                  , 2L)
        total.singletons <- sum(len.parts[singletons])

        if (any(nested.calls)) for(i.arg in which(nested.calls)) {
            available.width <- maxwidth - cumsum(len.parts)[i.arg]
            if (available.width > 5L) {
                if (call[[i.arg]][[1]]=='{') {
                    call[[i.arg]] <- substitute({...})
                    len.parts[i.arg] <- 5L
                } else {
                    usable.width <- available.width - ifelse(i.arg==length(call), 1L, 6L)
                    arg.label <- Recall(call[[i.arg]], usable.width, fixes)
                    call[[i.arg]] <- attr(arg.label, 'call')
                    len.parts[i.arg] <- nchar(arg.label) + ifelse(i.arg == length(call), 1L, 2L)
                }
            } else {
                call[[i.arg]] <- as.name("...")
                len.parts[i.arg] <- 3L + ifelse(i.arg == length(call), 1L, 2L)
            }
        }
        is.within <- cumsum(len.parts)<maxwidth

        if (!all(is.within))
            call <- as.call(c(as.list(call[is.within]), as.name('...')))
    }
    structure( fixes(paste(trimws(deparse(call)), collapse='')), call=call)
}
if(FALSE){
    call <- substitute(map(., with_progress(my_function_name, 100L, title="freaking long title"), another.arg))
    call_label_reduce(call, 50)

    infix.call <- substitute((1:10) %>% map(with_progress(throws_error), another.arg = "waaaaaaaaaaaaaay too long"))
    call_label_reduce(infix.call, 50)

    multiline.call <- substitute(with(globalenv(), {
        points <- rnorm(100)
        plot(points)
    }))
    call_label_reduce(multiline.call, 50)

    body.call <- substitute({hello})
    call_label_reduce(body.call)
}

# Resolve ---------------------------------------------------------

#' traceback a symbol chain to the originator call outside of a package
#'
#' @param sym the symbol to trace
#' @param i the number of the frame
#' @param package = the package to resolve out of
resolve_out <- function( sym, i
                       , package=get_namespace_name(sys.function(i))
                       ){
    purrr.call <- match.call( sys.function(i), sys.call(i))
    val <- getElement( as.list(purrr.call), sym)
    if(i<=1 || get_namespace_name(sys.function(i-1))!=package)
        return(val)
    Recall(val, i-1L, package)
}

call_label_resolve_purrr <-
function( i
        , ...
        , call = sys.call(i)
        ){
    orig.name <- resolve_out(call[[1]], i-1L, 'purrr')
    as.name(paste0( call[[1]], "<<", orig.name, ">>"))
}
package_called_from <- function(i)getPackageName(sys.frame(i-1L))
called_from_purrr <- function(i)package_called_from(i) == 'purrr'


# Methods ---------------------------------------------------------

#' @export
call_label.default <-
function( i
        , call=sys.call(i)
        , fun =sys.function(i)
        , ..., width = 50){
    tryCatch({
        if (called_from_purrr(i)) {
            call[[1]] <- call_label_resolve_purrr(i, call=call)
        }
        call_label_reduce(call=call, maxwidth=width, ...)
    }, error=function(...){
        limitedLabels(list(sys.call(i)))
    })
}

# @export
# call_label.fseq <-
# function( i
#         , call
#         , fun = sys.function(i)
#         , ...
#         , width=50){
#     call <- base::call('%>%', as.name('...'), body(functions(fun)[[1]]))
#     call_label_reduce(call, maxwidth=width)
# }
# if(FALSE){#@testing
#     fun <- . %>% f %>% g
#     label <- call_label.fseq(0, substitute(), fun)
#     expect_equal()
# }





#' S3 generic for generating call labels
#'
#' This generates call labels with extra information and interpolating
#' some symbols.  Dispatch is performed on the function type.
#'
#' @param i the call number
#' @param fun the function called
#' @param call the call to label
#' @param ... passed on or ignored
#' @param width the maximum width of the call label.
#'
#' @export
call_label <-
function(i, call=sys.call(i), fun = sys.function(i), ..., width=50L)
    UseMethod('call_label', fun)

call_labels <-
function(n= sys.nframe()){
    purrr::map_chr(seq(sys.nframe()-n+1L, sys.nframe()), call_label)
}
if(FALSE){#@testing
    test_call_labels <- function(n)call_labels(n)

    test_call_labels(2)
    expect_equal( . <- test_call_labels(2)
                , c('test_call_labels(2)', '#1: call_labels(n)')
                )

    labels <- 5 %>% test_call_labels
}

