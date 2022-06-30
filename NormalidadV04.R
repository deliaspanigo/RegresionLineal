
# Librerias
library("stats")
# namel<-function (vec){


source("lib.R")

namel(c("A", "B"))
################################################################################################


database <- mtcars
database[1,1] <- NA
database[3,3] <- NA

decimals <- 4
# vars <- 2

# vars <- c(2:8)
vars <- c("cyl",  "disp", "hp")
performance <- "each.one"
# vars <- c("cyl")

RScience.Test.Normal <- function(database = NULL,
                                    vars = NULL,
                                    performance = "each.one"){

  # Explanation about substitutions
  # _the_var_ : is a specific colname(database), only one

  # If is only one var
  if(length(vars) == 1) {

    # Diferents counts
    n_database <- nrow(database)
    n_mini_database <- nrow(na.omit(database[column_name]))
    n_na <- n_database - n_mini_database
    n_info <- data.frame(n_database, n_mini_database, n_na)


    # Details and BattleShip for 'vars'
    column_order <- vars
    column_name <- vars
    all_columns <- colnames(database)

    # If 'vars' is not a numeric object
    if(!is.numeric(vars)){

      # Must change order column order
      dt_general_order <- table(c(vars, all_columns)) == 2
      column_order <- c(1:length(dt_general_order))[dt_general_order]

      # Else must change column name
    } else column_name <- colnames(database)[column_order]

    # Letter for the column
    column_letter <- num2let(column_order)

    # Performance!
    if(performance == "all.in") database <- na.omit(database[column_name])

    # General sentence
    general_sentence <- "stats::shapiro.test(x = na.omit(database[,_the_var_]))"

    # Repleacement and each sentence
    replacement <- paste0("'", column_name, "'")
    sentence <- gsub(pattern = "_the_var_",
                          replacement = replacement,
                          x = general_sentence)

    # Normal Test (Shapiro-Wilk)
    test.normal <- eval(parse(text = sentence))





    # The original information and details (tabla01)
    results.original <-  sapply(test.normal, base::unlist)
    results.original <- t(results.original)
    results.original <- as.data.frame(results.original)
    results.original <- results.original[,c(1:3)]
    results.original[,1] <-  as.numeric(as.character(results.original[,1]))
    results.original[,2] <-  as.numeric(as.character(results.original[,2]))
    results.original <- cbind(column_name, column_order, column_letter, results.original)
    results.original <- cbind(results.original, n_info)

    ##############################################################################



    # Our Outputs
    the_exit <- list(results.original, list(test.normal))
    names(the_exit) <- c("Results", "R.Outputs")
    names(the_exit$R.Outputs) <- vars

    # whoamI added
    whoamI <- "Rscience.Test.Normal()"
    the_exit <- list(whoamI, the_exit)
    names(the_exit) <- c("whoamI?", "NormalTest")

    # Succeful return
    return(the_exit)


  } else

    # If are more...
    if(length(vars) > 1) {

      # Statistic Order
      statistic_order <- c(1:length(vars))

      # For each var...
      for(i in seq_along(vars)){

        # New Report
        new_report <-   RScience.TestNormalidad(database = database,
                                                vars  = vars[i],
                                                performance = performance)

        # First...
        if(i == 1) all_report <- new_report else

          # Next
          if(i > 1) {

            all_report[[2]][[1]] <- rbind(all_report[[2]][[1]], new_report[[2]][[1]])
            all_report[[2]][[2]] <- rbind(all_report[[2]][[2]], new_report[[2]][[2]])
          }
      }

      # Names
      names(all_report[[2]][[2]]) <- vars



      # Successful Return
      return(all_report)


    }

}



all_report <- RScience.Test.Normal(database = database,
                                      vars = vars)

all_report$NormalTest$Results
