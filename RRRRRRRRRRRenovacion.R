
# Libreries

# Database
database <- mtcars

source("lib.R")

# Libreries
library(stringi)




Rscience.Confidence <- function(alpha = NULL){

  confidence <- 1 - alpha

  return(confidence)
}

# Details and BattleShip for 'vars'
VarDescription <- function(vars = NULL, original_columns = NULL){

  column_order <- vars
  column_name <- vars

  # # If 'vars' is numeric, we have the position
  # for each var in the database.
  # We must change 'column_order'.
  # Else... if 'vars' is a character object, we must detect de original order
  if(is.numeric(vars)) column_name <- original_columns[column_order] else
    if(is.character(vars)) column_order <- match(vars, original_columns)

  # Letter for the column
  column_letter <- num2let(column_order)

  the_exit <- cbind(column_name, column_order, column_letter)
  the_exit <- as.data.frame(the_exit)
  return(the_exit)
}

nDescription <- function(database = NULL, selected_vars = NULL){

n_database <- nrow(database)
n_mini_database <- nrow(na.omit(database[selected_vars]))
n_na <- n_database - n_mini_database

n_info <- data.frame(n_database, n_mini_database, n_na)

return(n_info)

}


nDigits <- function(x){

  truncX <- floor(abs(x))
  armado <- c()

  if(length(truncX) == 1) {
  if(truncX != 0){
    armado[1] <- floor(log10(truncX)) + 1
  } else {
    armado[1] <- 1
  }

  } else for(k in 1:length(truncX)) armado[k] <- nDigits(truncX[k])

  return(armado)
}

StockNumber <- function(vector_numbers = NULL) {

  n_digits <- nDigits(vector_numbers)
  max_count_digits <- max(n_digits)

  # Almost 2 digits
  if(max_count_digits == 1) max_count_digits <- 2

  count_new_digits <- max_count_digits - n_digits

  add_new <- strrep(x = "0", times = count_new_digits)

  stock_number <- paste0(add_new, vector_numbers)

  return(stock_number)
}

DiferentPairs <- function(vars = NULL){

  # combination of pairs
  count_pairs <- length(vars)*(length(vars)-1)/2
  the_colnames <- c("Order", "X1", "X2")

  matrix_pairs <- as.data.frame(matrix(data = NA, nrow = count_pairs, ncol = length(the_colnames)))
  colnames(matrix_pairs) <- the_colnames

  internal_count <- 0
  for(k1 in 1:(length(vars)-1)) for(k2 in (k1+1):length(vars)) {
    internal_count <- internal_count + 1

    matrix_pairs[internal_count, 1] <- internal_count
    matrix_pairs[internal_count, 2] <- vars[k1]
    matrix_pairs[internal_count, 3] <- vars[k2]
  }

  return(matrix_pairs)

}

Reference.Normal <- function(vars = NULL,
                             shapirowilk_obj = "NormalTestShapiroWilk."){

  order <- c(1:length(vars))
  var_names <- vars
  stock_number <- StockNumber(vector_numbers = order)
  NameShapiroWilk <- paste0(shapirowilk_obj, stock_number)
  matrix_pairs_mod <- as.data.frame(cbind(order, vars, NameShapiroWilk))

  return(matrix_pairs_mod)

}


Reference.Cor.Pearson <- function(vars = NULL,
                          pearson_obj = "CorTestPearson."){

  # Diferent Pairs
  ref_matrix <- DiferentPairs(vars = vars)

  # Count of paris
  count_pairs <- nrow(ref_matrix)

  # New
  stock_number <- StockNumber(vector_numbers = 1:count_pairs)
  NamePearson <- paste0(pearson_obj, stock_number)
  ref_matrix <- cbind(ref_matrix, NamePearson)
  ref_matrix <- as.data.frame(ref_matrix)

  return(ref_matrix)

}

Reference.Cor.Spearman <- function(vars = NULL,
                                  spearman_obj = "CorTestSpearman."){

  # Diferent Pairs
  ref_matrix <- DiferentPairs(vars = vars)

  # Count of paris
  count_pairs <- nrow(ref_matrix)

  # New
  stock_number <- StockNumber(vector_numbers = 1:count_pairs)
  NameSpearman <- paste0(spearman_obj, stock_number)
  ref_matrix <- cbind(ref_matrix, NameSpearman)
  ref_matrix <- as.data.frame(ref_matrix)

  return(ref_matrix)

}

Reference.LinearRegresion <- function(x_vars = NULL,
                                                y_vars = NULL,
                                                LinearRegresion_obj = "LinearRegresionTest."){

  order <- c(1:length(y_vars))
  y_details <- y_vars
  x_details <- paste0(x_vars, collapse = ";")
  stock_number <- StockNumber(vector_numbers = order)
  NameLinearRegresion <- paste0(LinearRegresion_obj, stock_number)
  ref_matrix <- cbind(order, y_details, x_details, NameLinearRegresion)
  ref_matrix <- as.data.frame(ref_matrix)

  return(ref_matrix)

}






x_var <- c("cyl", "hp", "wt")
y_var <- "mpg"

# Global Options
decimals <- 4
alpha <- 0.05
confidence <- Rscience.Confidence(alpha = alpha)

performance <- "all.in"

####################################################################################

code <- list()

code[[1]] <- '
# Libreries
library(stringi)

####
source("lib.R")
source("lib2.R")

####
'

code[[2]] <- '
# Global Options
decimals <- 4
alpha <- 0.05
confidence <- Rscience.Confidence(alpha = alpha)

# # # # # #
database <- mtcars

x_var <- c("cyl", "hp", "wt")
y_var <- "mpg"

# Global Options
decimals <- 4
alpha <- 0.05
confidence <- Rscience.Confidence(alpha = alpha)
# # # # # #
'

code[[3]] <- '
    # Original columns
    original_columns <- colnames(database)

    # Vars Details
    x_details <- VarDescription(vars = x_var, original_columns = original_columns)
    y_details <- VarDescription(vars = y_var, original_columns = original_columns)
    battle_ship <- c(x_details$column_name, y_details$column_name)

    # n Info
    n_info <- nDescription(database = database, selected_vars = battle_ship)

    # minibase
    minibase <- na.omit(database[battle_ship])


    # Reference Tables
    ref_Normal <- Reference.Normal(vars = x_details$column_name)
    ref_CorPearson <- Reference.Cor.Pearson(vars = x_details$column_name)
    ref_CorSpearman <- Reference.Cor.Spearman(vars = x_details$column_name)
    ref_LinearRegresion <- Reference.LinearRegresion(y_vars = y_details$column_name, x_vars = x_details$column_name)
'

eval(parse(text = code[[1]]))
eval(parse(text = code[[2]]))
eval(parse(text = code[[3]]))

# Adress
adress <- c("Normal", "CorPearson", "CorSpearman", "LinearRegresion")

# Take all the same way
All_Reference <- list(ref_Normal, ref_CorPearson, ref_CorSpearman, ref_LinearRegresion)
names(All_Reference) <- adress

# Count Each Sentences (ces)
ces <- rep(NA, length(adress))
names(ces) <- adress
ces["Normal"] <- nrow(ref_Normal)
ces["CorPearson"] <- nrow(ref_CorPearson)
ces["CorSpearman"] <- nrow(ref_CorSpearman)
ces["LinearRegresion"] <- nrow(ref_LinearRegresion)


# General Sentence (GS)
GS <- namel.na(adress)
GS[["Normal"]] <- "shapiro.test(x = _the_data_[,'_each_var_x_'])"
GS[["CorPearson"]] <- "cor.test(x = _the_data_[,'_var_x1_'], y = _the_data_[,'_var_x2_'],
                                                    alternative = 'two.sided',
                                                    method = 'pearson',
                                                    conf.level = _confidence_,
                                                    exact = FALSE)"

GS[["CorSpearman"]] <- "cor.test(x = _the_data_[,'_var_x1_'], y = _the_data_[,'_var_x2_'],
                                                    alternative = 'two.sided',
                                                    method = 'spearman',
                                                    conf.level = _confidence_,
                                                    exact = FALSE)"

GS[["LinearRegresion"]] <- "lm(formula = _var_y_ ~ _all_var_x_, data = _the_data_)"



# Standard Deteccion Name (sdn)
sdn <- c("Pattern", "Replacement")


# General Changes
GeneralChanges <- list()
GeneralChanges[[1]] <- data.frame()
GeneralChanges[[1]] <- rbind(GeneralChanges[[1]], c("_the_data_", "minibase"))
GeneralChanges[[1]] <- rbind(GeneralChanges[[1]], c("_confidence_", confidence))
GeneralChanges[[1]] <- rbind(GeneralChanges[[1]], c("_all_var_x_", paste0(x_details$column_name, collapse = " + ")))
GeneralChanges[[1]] <- rbind(GeneralChanges[[1]], c("_var_y_", y_details$column_name))
for(k in 1:length(GeneralChanges)) colnames(GeneralChanges[[k]]) <- sdn



# Normal Changes
NormalChanges <- list()
NormalChanges[[1]] <- as.data.frame(cbind(rep("_each_var_x_", nrow(ref_Normal)), ref_Normal$vars))
for(k in 1:length(NormalChanges)) colnames(NormalChanges[[k]]) <- sdn

# Pearson Changes
PearsonChanges <- list()
PearsonChanges[[1]] <- as.data.frame(cbind(rep("_var_x1_", nrow(ref_CorPearson)), ref_CorPearson$X1))
PearsonChanges[[2]] <- as.data.frame(cbind(rep("_var_x2_", nrow(ref_CorPearson)), ref_CorPearson$X2))
for(k in 1:length(PearsonChanges)) colnames(PearsonChanges[[k]]) <- sdn

# Spearman Changes
SpearmanChanges <- PearsonChanges

# Linear Regresion Changes
LinearRegresionChanges <- list()
LinearRegresionChanges[[1]] <- as.data.frame(cbind(rep("_var_y_", nrow(ref_LinearRegresion)), ref_LinearRegresion$y_details))
LinearRegresionChanges[[2]] <- as.data.frame(cbind(rep("_all_var_x_", nrow(ref_LinearRegresion)), ref_LinearRegresion$x_details))
for(k in 1:length(LinearRegresionChanges)) colnames(LinearRegresionChanges[[k]]) <- sdn

All_Changes <- list(NormalChanges, PearsonChanges, SpearmanChanges, LinearRegresionChanges)
names(All_Changes) <- adress

# Generate.SpecificSentences.RegresionLineal <- function(GS, ces, All_Reference, GeneralChanges, All_Changes) {


# Specific Sentences
SpecificSentences <- namel(names(GS))
for(k in names(SpecificSentences)){
  opt01 <- rep(GS[[k]], ces[k])
  opt02 <- All_Reference[[k]][,ncol(All_Reference[[k]])]
 SpecificSentences[[k]] <- namel2(opt01, opt02) # list(rep(GS[[k]], ces[k]))
}

# General Substitution
for(k1 in 1:length(SpecificSentences)) {
  for(k2 in 1:length(SpecificSentences[[k1]])) {
    for(k3 in 1:length(GeneralChanges)) {
  SpecificSentences[[k1]][[k2]] <-  stri_replace_all_fixed(str = SpecificSentences[[k1]][[k2]],
                                                         pattern = GeneralChanges[[k3]]$Pattern,
                                                         replacement = GeneralChanges[[k3]]$Replacement,
                                                         vectorize_all = F)
    }
  }
}

# Mega Substitution
for(k1 in 1:length(SpecificSentences)) {
  for(k2 in 1:length(SpecificSentences[[k1]])) {
    for(k3 in 1:length(All_Changes[[k1]])) {
      for(k4 in 1:nrow(All_Changes[[k1]][[k3]])) {

    SpecificSentences[[k1]][[k2]] <- stri_replace_all_fixed(str = SpecificSentences[[k1]][[k2]],
                                                            pattern = All_Changes[[k1]][[k3]]$"Pattern"[k4],
                                                            replacement = All_Changes[[k1]][[k3]]$"Replacement"[k4],
                                                            vectorize_all = F)
      }
    }
  }
}



# }


# Fusion Sentences (FS)
FS <- namel(names(SpecificSentences))
for(k in 1:length(SpecificSentences)) {
  FS[[k]] <- paste0(REF[[k]][,ncol(REF[[k]])], " <- ", SpecificSentences[[k]])
  names(FS[[k]]) <- REF[[k]][,ncol(REF[[k]])]
}

code[[4]] <- ""
for(k1 in 1:length(FS)) for(k2 in 1:length(FS[[k1]]))  code[[4]] <- c(code[[4]], FS[[k1]][k2])

code[[4]] <- paste0(code[[4]], collapse = "\n")


ROutput <- namel.na(names(FS))
for(k1 in 1:length(FS)) ROutput[[k1]] <- namel.na(names(FS[[k1]]))

for(k1 in 1:length(ROutput)) for(k2 in 1:length(ROutput[[k1]])) {

  # Esto es la ejecucion de cada sentencia
  # Al evaluar la sentencia se ejecuta el test y todo se guarda en
  # un objeto de nombre previamente definido
  eval(parse(text = FS[[k1]][k2]))

  # Creamos dos objetos que nos serviran, por un lado para asignar el
  # nuevo objeto a la lista de salidas de R...
  # Inmediatamente luego de ser asignado, el objeto creado en el paso anterior
  # es eliminado.
  # La idea de esto es que el script ejecute tal cual cada sentnecia, y ver asi
  # que todo funciona.
 aver01 <- paste0("ROutput[[",k1, "]][[", k2, "]] <- names(FS[[", k1, "]])[", k2, "]", collapse = "")
 aver02 <- paste0("remove(", names(FS[[k1]])[k2], ")")

 eval(parse(text = aver01))
 eval(parse(text = aver02))

}



#            eval(parse(text = code[[2]]))

todo <- unlist(code)
