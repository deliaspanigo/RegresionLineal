

source("lib.R")

# Conjunto basico
database <- mtcars

all_columns <- colnames(database)


x_var <- colnames(database)[2:ncol(database)]
y_var <- "mpg"
performance = "all.in"
alpha <- 0.05

Rscience.Test.LinearRegresion.OneModel <- function(database = NULL,
                                                   x_var = NULL,
                                                   y_var = NULL){

  # All columns
  all_columns <- colnames(database)

  # X details for major models
  if(length(x_var) >= 1 && x_var[1] != "") {
  x_name_var <- x_var
  x_order_var <- x_var
  if(is.numeric(x_var)) x_name_var <- all_columns[x_var] else x_order_var <- match(x_name_var, all_columns)
  x_letter_var <- num2let(x_order_var)


  # Modelo
  model <- paste0(x_name_var, collapse = " + ")
  }

  # X details for Model Null
  else {
    x_name_var <- NA
    x_order_var <- NA
    x_letter_var <- NA
    model <- " 1"
  }

  # Y details
  y_name_var <- y_var
  y_order_var <- y_var
  if(is.numeric(y_var)) y_name_var <- all_columns[y_var] else y_order_var <- match(y_name_var, all_columns)
  y_letter_var <- num2let(y_order_var)

  # X and Y details in specific objects
  y_details <- data.frame(y_name_var, y_order_var, y_letter_var)
  x_details <- data.frame(x_name_var, x_order_var, x_letter_var)

  # Battle Ship
  battle_ship <- na.omit(c(x_name_var, y_name_var))
  mini_database <- na.omit(database[battle_ship])

  # Model Structure
  sentence <- paste0("lm(", y_name_var, " ~ ", model, ", data = mini_database)", collpase ="")

  # Results of my regresion
  my_results <- eval(parse(text = sentence))
  the_summary <- summary(my_results)

  # Special Details
  the_r2 <- the_summary$"r.squared"
  the_adj_r2 <- the_summary$"adj.r.squared"
  the_aic <- AIC(my_results)

  if(sum(x_var != "") > 0) {
  pvalue_r2 <- pf(q = the_summary$fstatistic[1], df1 = the_summary$fstatistic[2], df2 = the_summary$fstatistic[3], lower.tail = F)
  pvalue_adj_r2 <- pvalue_r2
  } else
    if(length(x_var) == 1 && x_var == "") {
      pvalue_r2 <- NA
      pvalue_adj_r2 <- pvalue_r2
    }

  # Model Fit Estimators
  model_fit <- c(the_r2, the_adj_r2, the_aic)
  names(model_fit) <- c("r.squared", "adj.r.squared", "aic")

  pvalue_model_fit <- c(pvalue_r2, pvalue_adj_r2, NA)
  names(pvalue_model_fit) <- c("pvalue.r2", "pvalue.adj.r2", "pvalue.aic")

  # Count
  n_database <- nrow(database)
  n_mini_database <- nrow(mini_database)
  n_na <- n_database - n_mini_database
  n_info <- data.frame(n_database, n_mini_database, n_na)
#  names(n_info) <- c("n.database", "n.mini.database", "n.na")


  armado01 <- "y_details, x_details, n_info, sentence, model_fit, pvalue_model_fit, the_summary, my_results"

  armado02 <- paste0("list(", armado01, ")")
  armado03 <- gsub(", ", "', '", armado01)
  armado04 <- paste0("c('", armado03, "')")

  the_exit <- eval(parse(text = armado02))
  names(the_exit) <- eval(parse(text = armado04))
  the_exit <- list(the_exit)
  names(the_exit) <- "model.1"

  whoamI <- "Rscience.LM.OneModel()"
  the_exit <- list(whoamI, the_exit)
  names(the_exit) <- c("whoamI?", "OneModel")

  return(the_exit)

}


aver <- Rscience.Test.LinearRegresion.OneModel(database = database, y_var = y_var, x_var = x_var)
aver

aver$OneModel
