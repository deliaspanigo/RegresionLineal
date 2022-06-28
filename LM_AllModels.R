
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

Rscience.LM.AllModels <- function(database = NULL, y_var = NULL, x_var = NULL){

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

  # All X logical combinations
  all_combinations <- expand.grid(status_list(counter = length(x_var)))
  colnames(all_combinations) <- x_name_var

  # The Results
  the_results <- list()
  for(k in 1:nrow(all_combinations)){

    # X vars for each model
    x_selected <- colnames(all_combinations)[as.vector(as.matrix(all_combinations[k,]))]

    # Correction for Model Null
    if(length(x_selected) == 0) x_selected <- "" else
      if(length(x_selected) == 1 && is.na(x_selected)) x_selected <- ""

    # Each test
      the_results[[k]] <- Rscience.LM.OneModel(database = database, y_var = y_var, x_var = x_selected)$model
    names(the_results)[k] <- paste0("model.", k)

  }

  # Last added
  whoamI <- "Rscience.LM.AllModels()"
  the_exit <- list(whoamI, the_results)
  names(the_exit) <- c("whoamI?", "AllModels")

  # Successful Return
  return(the_exit)
}


my_results <- Rscience.LM.AllModels(database = database, y_var = y_var, x_var = x_var)
my_results
