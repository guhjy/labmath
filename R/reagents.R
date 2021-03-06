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
            solutes <- list(...)
            if (length(solutes) == 0) {
                solutes <- list(name)
            }
            for (solute in solutes) {
                .self$add.solute(solute)
            }
            if (is.character(solvent)) {
                solvent <- Solvent(solvent)
            }
            .self$solvent <- solvent
            .self
        },
        add.solute=function(str=NULL, name=NULL, amount=NULL, unit=NULL, per.vol=NULL, per.unit=NULL, mw=NULL) {
            if (!is.null(str)) {
                parts <- stringr::str_match(str, "([\\d\\.]+) (\\w+)(?:/(?:([\\d\\.]+) )?(\\w+))? (.+?)(?: <(\\d+)>)?$")[1,-1]
                if (is.na(parts[1])) {
                    stop(paste("Invalid solute", str))
                }
                amount <- parse.amount(parts[1])
                unit <- parts[2]
                per.unit <- parts[4]
                if (is.na(parts[3])) {
                    per.vol <- 1
                }
                else {
                    per.vol <- parse.amount(parts[3])
                }
                name <- parts[5]
                mw <- NA
                if (!is.na(parts[6])) {
                    mw <- as.numeric(parts[6])
                }
            }
            .self$solutes <- rbindlist(list(.self$solutes, 
                .self$make.solute(name, amount, unit, per.vol, per.unit, mw)))
        },
        make.solute=function(name, amount, unit, per.vol, per.unit, mw) {
            list(name=name, amount=amount, unit=unit, per.vol=per.vol, per.unit=per.unit, mw=mw)
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
        #' Each solute is a string of the form: "amount unit name [<mw>]",
        #' where `amount` is a value that can be converted to a real number
        #' using `as.numeric()`, `unit` is a unit of concentration, and `name`
        #' is how you want the solute displayed. Optionally, the molecular weight
        #' (g/mol) can be provided in angle brackets. Valid units are:
        #' * %ww : percentage of solute as a fraction of total solution weight
        #' * %wv (or just %) : percentage of solute as a fraction of total volume
        #' * %vv : percentage of a liquid solute as a fraction of final
        #' solution volume
        #' * mol : molarity (moles/L). kmol, mmol, and umol are also recognized.
        #' E.g. "5 mmol NaCl <28>" means "5 millimolar NaCl, which has a molecular
        #' weight of 28 g/mol"
        #' * unit[/vol unit] : absolute quantity, or quantity per volume. The
        #' unit can be anything. If it is a recognized SI unit, then an exact
        #' amount will be calculated when making a mixture, otherwise it will be
        #' assumed to be a non-divisible entity. For example, some enzyme mixes 
        #' designed to be added in excess come in tablets or tubes sufficient up to 
        #' some volume. All of the following are valid:
        #' ** "1 tablet/50 ml proteinase inhibitor" means "add 1 tablet of proteinase
        #' inhibitor per 50 ml of solution, rounding up.
        #' ** "1 g/ml NaCl" is the same as "1 %wv NaCl"
        #'
        #' A solvent is a string of the form "<name> <amount> <w/v units>".
        initialize=function(name, ..., solvent=WATER) {
            tab <- data.table(name=character(), amount=numeric(), unit=character(), 
                              mw=numeric(), per.vol=numeric(), per.unit=character())
            callSuper(name, tab, ..., solvent=solvent)
            .self
        },
        make.solute=function(name, amount, unit, per.vol=NA, per.unit=NA, mw=NA) {
            if (unit == "%") {
                unit <- "%wv"
            }
            list(name=name, amount=amount, unit=unit, per.vol=per.vol, per.unit=per.unit, mw=mw)
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
           mol=, kmol=, mmol=, umol={
               moles <- convertr::convert(solute$amount, solute$unit, "mol") * vol$get.as("L")$amount
               if (as.wt && !is.na(solute$mw)) {
                   list(name=solute$name, amount=moles * solute$mw, unit="g")
               }
               else {
                   list(name=solute$name, amount=moles, unit="moles")
               } 
           }, # else
           {
               # absolute amount
               if (is.na(solute$per.vol)) {
                   list(name=name, amount=solute$amount, unit=solute$unit)
               }
               # unit/volume
               else {
                   per.vol <- Measure(amount=solute$per.vol, unit=solute$per.unit)
                   num.vols <- vol$amount / per.vol$get.as(vol$unit)$amount
                   # if the unit is valid SI, we compute the exact amount, otherwise
                   # the ingredient is assumed to be indivisible, i.e. add the amount
                   # for every per.vol amount of volume, rounding up.
                   # TODO: change to use is.supported.unit() when available
                   is.valid < tryCatch({
                       convertr::convert(1, solute$unit, solute$unit)
                       TRUE
                   }, error=function(e) FALSE)
                   if (!is.valid) {
                       num.vols <- ceiling(num.vols)
                   }
                   amount <- solute$amount * num.vols
                   list(name=name, amount=amount, unit=solute$unit)
               }
           }
    )
    if (as.measure) {
        Measure(amount=retval$amount, unit=retval$unit)
    }
    else {
        retval
    }
}