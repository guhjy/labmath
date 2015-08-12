Ingredient <- setRefClass("Ingredient",
    fields=c("name", "amount", "unit"),
    methods=list(
        convert=function(target.unit) {
            target.amt <- convert(.self$amount, .self$unit, target.unit)
            Ingredient(.self$name, target.amt, target.unit)
        }
    )
)

Solution <- setRefClass("Solution",
    fields=c("name", "solvent", "solutes"),
    methods=list(
        dilute=function(solute=NULL, final.conc=NULL, final.vol=NULL) {

        },
    )
)


#' Create a weight/weight (w/w) solution, given one or more
#' solutes at certain percentages (specified as decimal fractions)
#' a solvent, and a final volume.
ww.solution <- function(solute.names, solute.pcts, solvent="water", wt.unit="g") {
    stopifnot(length(solute.names) == length(solute.pcts))
    solutes <- sapply(seq_along(solute.names), function(i) {
        Ingredient(solute.names[i], )
    })
}

#' Dilute a solution. You must specify three of the four parameters and the
#' fourth will be calculated.
#'
#' @param conc concentration
#' @param vol volume of solution
#' @param final.conc desired final concentration, specified as a fraction
#' @param final.vol desired final volume of solution
dilute <- function(conc=NULL, vol=NULL, final.conc=NULL, final.vol=NULL,
                   solute.name=NA, solvent.name=NA, vol.units=NA) {
    args <- as.list(environment())
    stopifnot(sum(unlist(lapply(args, is.null))) <= 1)
    if (is.null(conc)) {
        args$conc <- (final.vol - vol) * final.conc / vol
    }
    else if (is.null(vol)) {
        args$vol <- (final.vol * final.conc) / (conc + final.conc)
    }
    else if (is.null(final.conc)) {
        args$final.conc <- (vol * conc) / (final.vol - vol)
    }
    else if (is.null(final.vol)) {
        args$final.vol <- ((vol * conc) / final.conc) + vol
    }
    class(args) <- "dilution"
    args
}

summary.dilution <- function(d, round.pct=0, round.vol=4) {
    my.paste(round(d$vol, round.vol), d$vol.units, round(d$conc * 100, round.pct), "%", d$solute.name,
             "+", round(d$final.vol - d$vol, round.vol), d$vol.units, d$solvent.name,
             "=", round(d$final.vol, round.vol), d$vol.units, round(d$final.conc * 100, round.pct), "%", d$solute.name)
}

#' Compute weight(s) of solute(s) required to make a solution
#' of a given volume, given the required molarity and the
#' molecular weight(s) of the solute(s).
molar.solution <- function(vol, mol, mw, solvent.name=NA, solute.names=NA,
                           vol.units="L", mol.units="mol", mw.units="g") {
    stopifnot(length(mol) == length(mw))
    recipe <- list(solvent.name=solvent.name, solvent.vol=vol, solvent.units=vol.units,
                   solute.names=solute.names, solute.units=mw.units)
    if (vol.units != "L") {
        vol <- convert(vol, vol.units, "L")
    }
    if (mol.units != "mol") {
        mol <- convert(mol, mol.units, "mol")
    }
    if (mw.units != "g") {
        mw <- convert(mw, mw.units, "g")
    }
    wts <- sapply(seq_along(mol), function(i) vol * mol[i] * mw[i])
    if (mw.units != "g") {
        wts <- convert(wts, "g", mw.units)
    }
    recipe$solute.wts=wts
    class(recipe) <- "recipe"
    recipe
}

summary.recipe <- function(r) {
    solutes <- paste.list(sapply(seq_along(r$solute.wts), function(i)
        my.paste(r$solute.wt[i], r$solute.units, r$solute.names[i])))
    my.paste("Combine", solutes, "and bring to", r$solvent.vol, r$solvent.units, c("with", r$solvent.name))
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
