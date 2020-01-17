




#' Match multiple regular expressions.
mgrepl <- function(x, patterns, ..., .simplify){
    matches <- map(patterns, grepl, x=x, ...) %>%
               unlist() %>% matrix(length(x), length(patterns)) %>%
               structure(dimnames = list(x,patterns))
               # set_names(patterns) %>% unlist() %>% mat
    if (missing(.simplify)) {
        return(matches)
    } else {
        .simplify <- rlang::as_function(.simplify)
        apply(matches, 1, .simplify)
    }
}


if(FALSE){#@testing

    x <- stringr::words

    patterns <- c('^a', 'b')

    raw <- mgrepl(x, patterns)
    expect_is(raw, 'matrix')
    expect_true(is.logical(raw))
    expect_equal(colnames(raw), patterns)
    expect_equal(rownames(raw), x)

    a <- mgrepl(x, patterns, .simplify='any')
    expect_true(is.logical(a))
    expect_equal(names(a), x)

    s <- mgrepl(x, patterns, .simplify=sum)
    expect_true(is.integer(s))
    expect_equal(names(s), x)

    w <- mgrepl(x, patterns, .simplify=which)
    expect_is(w, 'list')
    expect_equal(names(w), x)

}



