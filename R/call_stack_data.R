

get_call_srcref <- function(v){
    srcref <- attr(v, "srcref")
    if (!is.null(srcref)) {
        srcfile <- attr(srcref, "srcfile")
        paste0(basename(srcfile$filename), "#", srcref[1L])
    } else NA_character_
}
first_class <- function(x)head(class(x), 1L)


# Call Stack Data -------------------------------------------------
call_stack_data <- function(n=sys.nframe()-1L){
    i <- seq(1, n)
    calls <- head(sys.calls(), n)
    funs  <- lapply(i, sys.function)

    call.syms <- map(calls, getElement, 1L) %>% map(is.name)

    structure(
    data.frame( call.num    = i
              , label       = pmap_chr(list(i=i, call=calls, fun=funs), call_label)
              , srcrefs     = map_chr(calls, get_call_srcref)
              , namespace   = map_chr(funs, get_namespace_name)
              , fun.class   = map_chr(funs, first_class)
              , hidden      = map_lgl(map(funs, attr, 'hideFromDebugger'), isTRUE)
              , call.symbol = map_chr(calls, get_call_symbol)
              , same.parent = head(lag(diff(sys.parents()) == 0L, 1, FALSE), n)
              , is.root.call= head(sys.parents(), n)==0L
              # , called.from.namespace
              #              = dplyr::lag(map_chr(funs, get_namespace_name), 1)
              # , parent.num = head(sys.parents(), n)
              # , parent.ns = sys.parents() %>% head(n) %>% map(. %>% sys.frame %>% topenv) %>% map_chr(get_namespace_name)
              , stringsAsFactors=FALSE)
    , calls=calls, funs=funs)
}
if(FALSE){#@testing
    test_call_stack_data <- function(...){call_stack_data()}

    data <- test_call_stack_data()

    data <- 3 %>% test_call_stack_data

    data
if(interactive()){
    getOption('error')
    map(1, stop)


}

}

