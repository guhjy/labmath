parse.amount <- function(x) {
    if (is.null(x) || is.na(x)) return(NA)
    i <- regexpr("[:/]", x, perl=T)
    if (i > 0) {
        n <- as.numeric(substr(x, 1, i-1))
        d <- as.numeric(substring(x, i+1))
        if (substr(x, i, i) == ":") {
            n / (n+d)
        }
        else {
            n / d
        }
    }
    else {
        as.numeric(x)
    }
}

my.paste <- function(..., sep=" ", filter.null=TRUE) {
    parts <- list(...)
    parts <- lapply(parts, function(x) {
        if (length(x) > 1) {
            do.call(my.paste, c(x, list(sep=sep, filter.null=FALSE)))
        }
        else x
    })
    w <- unlist(lapply(parts, function(x) is.null(x) || is.na(x)))
    if (any(w) && !filter.null) {
        NULL
    }
    else {
        paste(parts[!w], collapse=sep)
    }
}

paste.list <- function(v) {
    x <- length(v)
    switch(x, v[1], paste(v, collapse=" and "), paste(paste(v[-x], collapse=", "), v[x], sep=" and "))
}
