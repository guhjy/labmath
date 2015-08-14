setRefClass("Base",
    methods=list(
        show=function() {
            print(.self$format())
        }
    )
)
as.character.Base <- format.Base <- function(x) x$format()

Measure <- setRefClass("Measure",
    fields=c(amount="numeric", unit="character"),
    contains="Base",
    methods=list(
        initialize=function(str=NULL, amount=1, unit="") {
            if (!is.null(str)) {
                parts <- unlist(strsplit(str, ' ', fixed=TRUE))
                if (length(parts) == 1) {
                    amount <- 1
                    unit <- parts[1]
                }
                else {
                    amount <- as.numeric(parts[1])
                    unit <- parts[2]
                }
            }
            .self$amount <- amount
            .self$unit <- unit
            .self
        },
        get.as=function(x) {
            if (x == .self$unit) {
                .self
            }
            else {
                if (class(x) != "Measure") {
                    x <- Measure(unit=x)
                }
                new.amt <- convertr::convert(.self$amount, .self$unit, x$unit) / x$amount
                Measure(amount=new.amt, unit=x$unit)
            }
        },
        format=function() {
            paste(.self$amount, .self$unit)
        }
    )
)

Solvent <- setRefClass("Solvent",
    fields=c(name="character", wt="Measure", vol="Measure"),
    contains="Base",
    methods=list(
        initialize=function(str=NULL, name="", wt=Measure(), vol=Measure()) {
            if (!is.null(str)) {
                parts <- unlist(strsplit(str, ' ', fixed=TRUE))
                stopifnot(length(parts) == 3)
                name <- parts[1]
                units <- unlist(strsplit(parts[3], "/", fixed=TRUE))
                wt <- Measure(amount=parse.amount(parts[2]), unit=units[1])
                vol <- Measure(unit=units[2])
            }
            .self$name <- name
            .self$wt <- wt
            .self$vol <- vol
            .self
        },
        format=function() {
            paste(.self$name, .self$wt, "/", .self$vol)
        }
    )
)

# Common solvents
WATER = Solvent("water 1 g/ml")

Recipe <- setRefClass("Recipe",
    fields=c(name="character", solutes="data.table", solvent="Solvent"),
    contains="Base",
    methods=list(
        initialize=function(name, solute.table, ..., solvent=WATER) {
            .self$name <- name
            .self$solutes <- solute.table
            for (solute in list(...)) {
                .self$add.solute(solute)
            }
            if (is.character(solvent)) {
                solvent <- Solvent(solvent)
            }
            .self$solvent <- solvent
            .self
        },
        add.solute=function(str=NULL, name=NULL, amount=NULL, unit=NULL, mw=NULL) {
            if (!is.null(str)) {
                parts <- unlist(strsplit(str, ' ', fixed=TRUE))
                stopifnot(length(parts) == 3)
                amount <- parse.amount(as.numeric(parts[1]))
                unit <- parts[2]
                name <- parts[3]
            }
            .self$solutes <- rbindlist(list(.self$solutes, .self$make.solute(name, amount, unit)))
        },
        make.solute=function(name, amount, unit) {
            list(name=name, amount=amount, unit=unit)
        },
        format=function(markdown=FALSE) {
            .self$name
        }
    )
)

Solution <- setRefClass("Solution",
    contains="Recipe",
    methods=list(
        #' Describe a solution in plain english.
        #'
        #' @param name recipe name
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
        #' * mol[<mw>] : molarity (moles/L), with the molecular weight of the
        #' solute specified in brackets. kmol, mmol, and umol are also recognized.
        #'
        #' A solvent is a string of the form "<name> <amount> <w/v units>".
        initialize=function(name, ..., solvent=WATER) {
            callSuper(name, data.table(name=character(), amount=numeric(), unit=character(), mw=numeric()), ..., solvent=solvent)
            .self
        },
        make.solute=function(name=NULL, amount=NULL, unit=NULL) {
            mw <- NA
            if (unit == "%") {
                unit <- "%wv"
            }
            else if (substr(unit, 1, 1) != "%") {
                unit <- unlist(strsplit(unit, '[\\[\\]]', perl=TRUE))
                if (length(unit) > 1) {
                    mw <- as.numeric(unit[2])
                    unit <- unit[1]
                }
            }
            list(name=name, amount=amount, unit=unit, mw=mw)
        },
        get.amount.for=function(solute, vol, as.wt=TRUE) {
            solute <- .self$solutes[eval(solute),]
            if (class(vol) != "Measure") {
                vol <- Measure(vol)
            }
            get.amount.for.conc(solute, .self$solvent, vol, as.wt)
        },
        make=function(vol, as.wt=TRUE) {
            if (is.character(vol)) {
                vol <- Measure(vol)
            }
            solutes <- .self$solutes[, get.amount.for.conc(.SD, .self$solvent, vol, as.wt, as.measure=FALSE)]
            Mixture(.self$name, vol, solute.table=solutes, solvent=.self$solvent)
        },
        #' Print a nicely formatted recipe.
        format=function(markdown=FALSE) {
            print(paste0("To make ", .self$name, ", combine:"))
            print(.self$solutes[,paste0(.I, ". ", amount, " ", unit, " ", name)])
            print(paste("and bring to volume with", .self$solvent$name))
        }
    )
)

Mixture <- setRefClass("Mixture",
    fields=c(amount="Measure"),
    contains="Recipe",
    methods=list(
        initialize=function(name, amount, ..., solute.table=NULL, solvent=WATER) {
            if (is.null(solute.table)) {
                solute.table <- data.table(name=character(), amount=numeric(), unit=character())
            }
            callSuper(name, solute.table, ..., solvent=solvent)
            .self$amount <- amount
            .self
        },
        format=function(markdown=FALSE) {
            print(paste0("To make ", .self$amount, " ", .self$name, ", combine:"))
            print(.self$solutes[,paste0(.I, ". ", amount, " ", unit, " ", name)])
            print(paste("and bring to", .self$amount, "with", .self$solvent$name))
        }
    )
)

get.amount.for.conc <- function(solute, solvent, vol, as.wt=TRUE, as.measure=TRUE) {
    retval <- switch(solute$unit,
           "%ww"={
               vol <- vol$get.as(solvent$vol)
               wt <- (solute$amount / 100) * solvent$amount * vol$amount
               list(name=solute$name, amount=wt, unit=solvent$vol$unit)
           },
           "%wv"={
               vol <- vol$get.as(solvent$vol)
               wt <- (solute$amount / 100) * vol$amount
               list(name=solute$name, amount=wt, unit=solvent$wt$unit)
           },
           "%vv"={
               wt <- (solute$amount / 100) * vol$amount
               list(name=solute$name, amount=wt, unit=vol$unit)
           },
           {
               moles <- convertr::convert(solute$amount, solute$unit, "mol") * vol$get.as("L")$amount
               if (as.wt && !is.na(solute$mw)) {
                   list(name=solute$name, amount=moles * solute$mw, unit="g")
               }
               else {
                   list(name=solute$name, amount=moles, unit="moles")
               } 
           }
    )
    if (as.measure) {
        do.call(Measure, retval)
    }
    else {
        retval
    }
}