#' Dilute a solution whose concentration is specified
#' as a percentage (e.g. 37% w/v or 10% v/v). You must
#' specify three of the four parameters and the fourth will be calculated.
#'
#' @param conc concentration, specfied as a fraction (e.g. 0.37 for 37% concentration)
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
    class(args) <- 'dilution'
    args
}

summary.dilution <- function(d, round.pct=0, round.vol=4) {
    my.paste(round(d$vol, round.vol), d$vol.units, round(d$conc * 100, round.pct), "%", d$solute.name,
             "+", round(d$final.vol - d$vol, round.vol), d$vol.units, d$solvent.name,
             "=", round(d$final.vol, round.vol), d$vol.units, round(d$final.conc * 100, round.pct), "%", d$solute.name)
}

my.paste <- function(..., sep=" ") {
    parts <- list(...)
    w <- unlist(lapply(parts, function(x) is.null(x) || is.na(x)))
    paste(parts[!w], collapse=sep)
}
