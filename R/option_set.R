


new_option_set <-
function( prefix = packageName()
        , sep="::"
        , defaults = list()
        , restrict = FALSE
        ){
    option_name <- function(name){
        if (restrict)
            pkgcond::assert_that( name %in% names(defaults)
                                , msg = "Options are in restricted mode and" %<<%
                                    sQuote(name) %<<% "is not a defined option"
                                , type = 'invalid-option'
                                )
        paste(prefix, name, sep=sep)
    }

    get_default = function(name)getElement(defaults, name)

    get = function(name, default=get_default(name))
            getOption(option_name(name), default)
    set = function(name, value){
        new <- structure(list(value), names = option_name(name))
        options(new)
    }
    add = function(name, value){
        old <- getOption(option_name(name))
        new <- c(old, value)
        set(name, new)
    }
    reset = function(name){
        set(name, getElement(defaults, name))
    }
    reset_all = function(){
        for (name in names(defaults)) reset(name)
    }

    ls = function(all=TRUE){
        names <- names(defaults)

        if (restrict) return(names)
        other.names <-
            sub(paste0(prefix, sep), '',  fixed=TRUE,
                grep(paste0(prefix, sep), names(.Options), value=TRUE)
            )
        base::union(names, other.names)
    }

    .self <- environment()
    lockEnvironment(.self)
    .self
}




