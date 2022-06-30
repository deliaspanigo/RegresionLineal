
# Librerias
library("stats")


source("lib.R")




namel(c("A", "B"))
################################################################################################


database <- mtcars
database[1,1] <- NA
database[3,3] <- NA

# columnas_seleccionadas <- c(2:8)
vars <- c("cyl",  "disp", "hp")

alpha <- 0.05

RScience.Test.Correlation.Pearson <- function(database = NULL,
                                              vars = c(1:ncol(database)),
                                              alpha = 0.05){


  # Confidence
  confidence <- 1 - alpha

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


  all_combinations <- as.data.frame(matrix(NA, 1, 2))
  for(k1 in 1:(length(column_name)-1)) for(k2 in (k1+1):length(column_name)){
    all_combinations <- rbind(all_combinations, column_name[c(k1, k2)])
  }
  all_combinations <- na.omit(all_combinations)
  colnames(all_combinations) <- c("X1", "X2")
  rownames(all_combinations) <- c(1:nrow(all_combinations))


    # General sentence
    general_sentence <- "stats::cor.test(x = na.omit(database[,_the_vars_])[,1],
                                         y = na.omit(database[,_the_vars_])[,2],
                                         alternative = 'two.sided',
                                         method = 'pearson',
                                         conf.level = _confidence_)"


    test.cor.pearson <- list()

    for(k in 1:nrow(all_combinations)) {
    # Repleacement and each sentence
    replacement <- paste0("c('", all_combinations[k,1], "' , '", all_combinations[k,2], "')")
    sentence <- gsub(pattern = "_the_vars_",
                     replacement = replacement,
                     x = general_sentence)

    sentence <- gsub(pattern = "_confidence_",
                     replacement = confidence,
                     x = sentence)

    # Pearson Test Correlation
    test.cor.pearson[[k]] <- eval(parse(text = sentence))

    names(test.cor.pearson)[k] <- paste0("correlation", k)
    }




    }


    # The original information and details (tabla01)
    results.original <-  sapply(test.cor.pearson, base::unlist)
    results.original <- t(results.original)
    results.original <- results.original[,-8] # Take off data.name
    results.original <- as.data.frame(results.original)

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
                                                vars  = vars[i])

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


mis_analisis_normalidad_var <- RScience.TestNormalidad(base = base, columnas_seleccionadas =  c(3:6),
                                                       decimales = 4,alfa = 0.05, corte = 0.001)

mis_analisis_normalidad_var[[1]]
