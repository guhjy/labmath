#Common solvents
WATER = "water 1 g/ml"

#' Describe a solution in plain english.
#' 
#' @param ... one or more solutes, specified as concentrations 
#' (see details).
#' @param solvent the solvent used in the solution 
#' (see details). Defaults to water.
#' 
#' @details 
#' Each solute is a string of the form: "<amount> <unit> <name>",
#' where `amount` is a value that can be converted to a real number
#' using `as.numeric()`, `unit` is a unit of concentration, and `name` 
#' is how you want the solute displayed. Valid units are:
#' * %ww : percentage of solute as a fraction of total solution weight
#' * %wv (or just %) : percentage of solute as a fraction of total volume
#' * %vv : percentage of a liquid solute as a fraction of final 
#' solution volume
#' * M[<mw>] : molarity (moles/L), with the molecular weight of the
#' solute specified in brackets. kM, mM, and uM are also recognized.
#' 
#' A solvent is a string of the form "<name> <amount> <w/v units>".
recipe <- function(name, ..., solvent=WATER) {
    solutes <- lapply(list(...), parse.solute)
    solvent <- parse.solvent(solvent)
    structure(list(name=name, solutes=solutes, solvent=solvent), class="recipe")
}

#' Create a recipe for making a certain volume of solution.
make <- function(recipe, vol, ...) {
    solution(
        lapply(recipe$solutes, function(s) {
            amount <- get.amount.for(s, recipe$solvent, vol, ...)
            list(amount=amount, name=solute$name)
        },
        parse.measure(vol), solvent$name
    )
}

solution <- function(solutes, solvent.vol, solvent.name) {
    structure(list(solutes=solutes, solvent.vol=solvent.vol, solvent.name=solvent.name), class="solution")
}

#' Print a nicely formatted recipe.
#' TODO: markdown
print.recipe <- function(r, markdown=FALSE) {
    print(paste("To make", r$solvent, r$name, ", combine:")
    for (i in 1:length(r$solutes)) {
        print(paste0(i, ". ", r$amount, " ", r$name))
    }
    print(paste("and bring to", r$solvent.vol, "with", r$solvent.name))
}

dilute <- function()

parse.solvent <- function(x) {
    parts <- unlist(strsplit(x, ' ', fixed=TRUE))
    stopifnot(length(parts) == 3)
    name <- parts[1]
    amount <- parse.amount(parts[2])
    units <- unlist(strsplit(parts[3], "/"))
    solvent <- list(name=name, amount=amount, wt.vol=units)
    class(solvent) <- "solvent"
    solvent
}

parse.solute <- function(x) {
    parts <- unlist(strsplit(x, ' ', fixed=TRUE))
    stopifnot(length(parts) == 3)
    amount <- parse.amount(as.numeric(parts[1]))
    unit <- parts[2]
    name <- parts[3]
    if (substr(unit, 1, 1) == "%") {
        solute <- list(amount=amount, unit=unit, name=name)
        if (solute == "%") 
            class(solute) <- "WV"
        else
            class(solute) <- toupper(substring(unit, 2)))
    }
    else {
        unit <- unlist(strsplit('[\[\]]', unit, perl=TRUE))
        mw <- NA
        if (length(unit) > 1) {
            mw <- measure(as.numeric(unit[2]), "g")
            unit <- unit[1]
        }
        solute <- list(amount=measure(amount, unit), name=name, mw=mw)
        class(solute) <- "Molarity"
    }
    solute
}

#' Get amount of solute for given amount of solvent.
#' @S3method 
get.amount.for <- UseMethod("get.amount.for")
get.amount.for.WW <- function(solute, solvent, vol) {
    vol <- get.as(parse.measure(vol), solvent$wt.vol[2])
    wt <- (solute$amount / 100) * solvent$amount * vol$amount
    measure(wt, solvent$wt.vol[1])
}
get.amount.for.WV <- function(solute, solvent, vol) {
    vol <- get.as(parse.measure(vol), solvent$wt.vol[2])
    wt <- (solute$amount / 100) * vol$amount
    measure(wt, solvent$wt.vol[1])
}
get.amount.for.VV <- function(solute, solvent, vol) {
    vol <- parse.measure(vol)
    wt <- (solute$amount / 100) * vol$amount
    measure(wt, vol$unit)
}

#' Get amount of solute for given amount of solvent.
#' Returns amount in grams if `wt=TRUE` and `!is.na(solute$mw)`,
#' otherwise returns amount in moles.
get.amount.for.Molarity <- function(solute, solvent, vol, wt=TRUE) {
    vol <- parse.measure(vol)
    moles <- get.as(solute$amount, "mol") * get.as(vol, "L")
    if (wt && !is.na(solute$mw)) {
        measure(moles * solute$mw, "g")
    }
    else {
        measure(moles, "moles")
    }
}

parse.measure <- function(x) {
    if (class(x) == 'measure') return(x)
    parts <- unlist(strsplit(x, ' ', fixed=TRUE))
    stopifnot(length(parts) == 2)
    amount <- as.numeric(parts[1])
    unit <- parts[2]
    measure(amount, unit)
}

measure <- function(amount, unit) {
    structure(list(amount=amount, unit=unit), class="measure")
}

print.measure(measure) {
    
}

get.as <- function(measure, unit) {
    if (unit == measure$unit) {
        measure
    }
    else {
        new.amt <- convert(measure$amount, measure$unit, unit)
        measure(new.amt, unit)
    }
}

parse.amount <- function(x) {
    x <- unlist(strsplit(x), "[:/]", perl=T)
    if (length(x) == 1) {
        as.numeric(x)
    }
    else {
        n <- as.numeric(x[1])
        d <- as.numeric(x[3])
        if (x[2] == ':') {
            n / (n+d)
        }
        else {
            n / d
        }
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
