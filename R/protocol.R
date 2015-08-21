#' A protocol is a series of steps for completing an experiment. Each step
#' is a text description, but can include macros (see details).
#' 
#' @details
#' A step looks like this:
#' 
#' <pre>1. Add #{5 mL OF lysis buffer IN Tube1} and shake for @{1 min}.</pre>
#' 
#' The #{...} and @{...} parts are macros. There are four types of macros:
#'
#' #{<reagent>} : reagent discriptor. The simplest description is a reagent
#' name, e.g. #{saline}, which just makes sure that reagent is included in 
#' the materials list. To specify an amount of reagent, use 'OF' to separate
#' the amount from the reagent name, e.g. #{1 mL OF saline}. Alternatively,
#' to indicate a dilution, use 'TO' to separate the solution from the final
#' concentration, e.g. #{formaldehyde TO 1%}. Finally, a specific container
#' can be specified after 'IN', to indicate that multiple additions happen 
#' in the same container, e.g. #{1 mL OF saline IN Tube1}.
#' 
#' @{<time>} : specifies a duration, such as a wait time, e.g. @{45 min}.
#' 
#' ${<equipment>} : specifies a required piece of equipment to include in
#' the materials list.
#' 
#' !{<TYPE> } : include a NOTE, WARNING, or ERROR message.
Protocol <- setRefClass("Protocol",
    fields=c(days="list", materials="data.table", solutions="list", containers="list"),
    methods=list(
        initialize=function(..., materials=NULL, solutions=NULL) {
            for (day in list(...)) {
                .self$add.day(day)
            }
            if (is.null(materials) || ncol(materials) == 0) {
                materials <- data.table(name=character(), description=character())
            }
            .self$materials <- materials
            .self$solutions <- solutions
            .self$containers <- containers
        },
        add.day=function(str) {
            .self$days <- c(.self$days, Day(str, .self))
        }
    )
)

Day <- setRefClass("Day",
    methods=list(
        initialize=function(str, protocol, day.num) {
            steps <- unlist(strsplit(str, "^\\s*\\d+\\. {2}", perl=TRUE))
            steps <- gsub("[\\n\\s]+", " ", steps, perl=TRUE)
            steps <- steps[!is.na(steps) & nchar(steps) > 0]
            steps <- lapply(seq_along(steps), function(i) {
                Step(steps[i], protocol, day.num, i)
            })
        }
    )
)

Step <- setRefClass("Step",
    fields=c(text="character", timer="Timer", notes=list()),
    methods=list(
        initialize=function(str, protocol, day.num, step.num) {
            macros <- str_match_all(str, "[#@!\\$]\\{.*?\\}")[[1]]
            if (length(macros) == 0) {
                text <- str
            }
            else {
                new.str <- ""
                for (i in 1:nrow(macros)) {
                    new.str <- paste0(new.str, substr(str, ifelse(i==1, 1, end[i-1]+1), start[i]-1))
                    mtype <- substr(macros[i], 1, 1)
                    mtext <- substr(macros[i], 3, nchar(macros[i])-1)
                    new.text <- switch(mtype,
                        "#" = {
                            soln <- str_match(mtext, "(.+?) (OF|TO) (.+?( IN (.+))|$)")[1,]
                            if (is.na(soln[1])) {
                                protocol$add.materials(mtext)
                            }
                            else {
                                x <- soln[2]
                                y <- soln[4]
                                container <- soln[6]
                                
                                if (soln[3] == "OF") {
                                    # make solution for certain volume, then add to container
                                    solution <- protocol$get.solution(y)
                                    mixture <- solution$make(x)
                                    protocol$add.reagents(mixture, container)
                                    new.text <- paste0(new.text, paste(x, "of", y, "in", container)
                                }
                                else {
                                    # dilute solution to appropriate concentration
                                    solution <- protocol$get.solution(x)
                                    if (is.null(container)) {
                                        amount <- solution$dilute.to(y)
                                        new.text <- paste0(new.text,
                                            paste0(amount, " per L ", x, " (final concentration ", y, ")"))
                                    }
                                    else {
                                        container.vol <- protocol$.get.container.vol(container)
                                        amount <- solution$dilute.to(y, container.vol)
                                        protocol$add.to.container(amount)
                                        new.text <- paste0(new.text, 
                                            paste0(amount, " ", x, " in ", container, " (final concentration ", y, ")"))
                                    }
                                }
                            }
                        },
                        "@" = {
                            timer <- Timer(mtext)
                            .self$timer <- timer
                            format(timer)
                        },
                        "$" = {
                            protocol$add.materials(mtext)
                        },
                        "!" = {
                            note.num <- length(.self$notes) + 1
                            note <- Note(mtext, paste(day.num, step.num, note.num, sep="."))
                            .self$notes <- c(.self$notes, note)
                            paste0("(see ", note, ")")
                        }
                    )
                    new.str <- paste0(new.str, new.text)
                }
                new.str <- paste0(new.str, substring(str, end[length(i)] + 1))
                .self$text <- new.str
            }
        }
    )
)
