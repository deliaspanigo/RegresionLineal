
source("lib.R")

# Conjunto basico
database <- mtcars

all_columns <- colnames(database)


x_var <- colnames(database)[2:ncol(database)]
y_var <- "mpg"



status_list <- function(counter){

  the_list <- list()
  for(k in 1:counter) the_list[[k]] <- c(F,T)

  return(the_list)
}

Rscience.Test.LinearRegresion.AllModels <- function(database = NULL, y_var = NULL, x_var = NULL){

  # All columns
  all_columns <- colnames(database)

  # X details
  x_name_var <- x_var
  x_order_var <- x_var
  if(is.numeric(x_var)) x_name_var <- all_columns[x_var] else x_order_var <- match(x_name_var, all_columns)
  x_letter_var <- num2let(x_order_var)

  # Y details
  y_name_var <- y_var
  y_order_var <- y_var
  if(is.numeric(y_var)) y_name_var <- all_columns[y_var] else y_order_var <- match(y_name_var, all_columns)
  y_letter_var <- num2let(y_order_var)

  battle_ship <- c(y_name_var, x_name_var)

  # All X logical combinations
  all_combinations <- expand.grid(status_list(counter = length(x_var)))
  colnames(all_combinations) <- x_name_var

  # The Results
  LinearRegresion.Results <- list()
  Normal.Results <- list()

  #LinearRegresion.Results
  for(k in 1:nrow(all_combinations)){

    # X vars for each model
    x_selected <- colnames(all_combinations)[as.vector(as.matrix(all_combinations[k,]))]

    # Correction for Model Null
    if(length(x_selected) == 0) x_selected <- "" else
      if(length(x_selected) == 1 && is.na(x_selected)) x_selected <- ""

    # Each test
     LinearRegresion.Results[[k]] <- Rscience.Test.RegLin.OneModel(database = database, y_var = y_var, x_var = x_selected)$OneModel$model.1
      names(LinearRegresion.Results)[k] <- paste0("model.", k)

  }

  # Normal.Results
  for(k in 1:nrow(all_combinations)){

    # X vars for each model
    x_selected <- colnames(all_combinations)[as.vector(as.matrix(all_combinations[k,]))]

    # Correction for Model Null
    if(length(x_selected) == 0) x_selected <- "" else
      if(length(x_selected) == 1 && is.na(x_selected)) x_selected <- ""

      if(x_selected[1] == "") {


      Normal.Results[[k]] <- list(c(NA), c(NA))
      names(Normal.Results)[k] <- c("Results", "R.Outputs")

      }else
      {
      # Each test
        Normal.Results[[k]] <- RScience.Test.Normal(database = database,
                                                    vars = x_selected)$NormalTest


      }

      names(Normal.Results)[k] <- paste0("model.", k)
  }


  # Last added
  whoamI <- "Rscience.LinearRegresion.AllModels()"
  the_exit <- list(whoamI,LinearRegresion.Results, Normal.Results)
  names(the_exit) <- c("whoamI?", "LinearRegresion.AllModels", "Normal.Results")

  # Successful Return
  return(the_exit)
}


my_results <- Rscience.Test.LinearRegresion.AllModels(database = database, y_var = y_var, x_var = x_var)

names(my_results$LinearRegresion.AllModels)

