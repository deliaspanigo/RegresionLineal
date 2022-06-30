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
  ObjName <- paste0(shapirowilk_obj, stock_number)
  matrix_pairs_mod <- as.data.frame(cbind(order, vars, ObjName))

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
  ObjName <- paste0(pearson_obj, stock_number)
  ref_matrix <- cbind(ref_matrix, ObjName)
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
  ObjName <- paste0(spearman_obj, stock_number)
  ref_matrix <- cbind(ref_matrix, ObjName)
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
  ObjName <- paste0(LinearRegresion_obj, stock_number)
  ref_matrix <- cbind(order, y_details, x_details, ObjName)
  ref_matrix <- as.data.frame(ref_matrix)

  return(ref_matrix)

}

##########


MegaDAVID <- function(database = NULL, x_var = NULL, y_var = NULL, decimals = NULL,
                      alpha = NULL, confidence = NULL){
  the_code <- list()

  the_code[[1]] <- '
# Libreries
library(stringi)

####
source("lib.R")
source("lib2.R")

####
'

  the_code[[2]] <- '
# Global Options
decimals <- _decimals_
alpha <- _alpha_
confidence <- Rscience.Confidence(alpha = alpha)

# # # # # #
database <- mtcars

x_var <- _vector_x_
y_var <- _vector_y_


# # # # # #
'
  armado_x <- paste0("c('", paste0(x_var, collapse="', '"), "')")
  armado_y <- paste0("c('", paste0(y_var, collapse="', '"), "')")


  the_code[[2]] <-  stri_replace_all_fixed(str = the_code[[2]],
                         pattern = c("_vector_x_", "_vector_y_", "_decimals_", "_alpha_"),
                         replacement = c(armado_x, armado_y, decimals, alpha),
                         vectorize_all = F)

  the_code[[3]] <- '
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

\n\n\n\n\n\n\n\n
'

#  eval(parse(text = the_code[[1]]))
#  eval(parse(text = the_code[[2]]))
  eval(parse(text = the_code[[3]]))

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



  # Standard Deteccion Names (sdn)
  sdn <- c("Pattern", "Replacement", "Destiny")


  # General Changes
  GeneralChanges <- list()
  GeneralChanges[[1]] <- data.frame()
  GeneralChanges[[1]] <- rbind(GeneralChanges[[1]], c("_the_data_", "minibase"))
  GeneralChanges[[1]] <- rbind(GeneralChanges[[1]], c("_confidence_", confidence))
  GeneralChanges[[1]] <- rbind(GeneralChanges[[1]], c("_all_var_x_", paste0(x_details$column_name, collapse = " + ")))
  GeneralChanges[[1]] <- rbind(GeneralChanges[[1]], c("_var_y_", y_details$column_name))
  for(k in 1:length(GeneralChanges)) colnames(GeneralChanges[[k]]) <- sdn[c(1,2)]



  # Normal Changes
  NormalChanges <- list()
  NormalChanges[[1]] <- as.data.frame(cbind(rep("_each_var_x_", nrow(ref_Normal)),
                                            ref_Normal$vars,
                                            ref_Normal$ObjName)
                                      )
  for(k in 1:length(NormalChanges)) colnames(NormalChanges[[k]]) <- sdn


  # Pearson Changes
  PearsonChanges <- list()
  PearsonChanges[[1]] <- as.data.frame(cbind(rep("_var_x1_", nrow(ref_CorPearson)),
                                             ref_CorPearson$X1,
                                             ref_CorPearson$ObjName))
  PearsonChanges[[2]] <- as.data.frame(cbind(rep("_var_x2_", nrow(ref_CorPearson)),
                                             ref_CorPearson$X2,
                                             ref_CorPearson$ObjName))

  for(k in 1:length(PearsonChanges)) colnames(PearsonChanges[[k]]) <- sdn


  # Spearman Changes
  SpearmanChanges <- list()
  SpearmanChanges[[1]] <- as.data.frame(cbind(rep("_var_x1_", nrow(ref_CorSpearman)),
                                             ref_CorSpearman$X1,
                                             ref_CorSpearman$ObjName))
  SpearmanChanges[[2]] <- as.data.frame(cbind(rep("_var_x2_", nrow(ref_CorSpearman)),
                                             ref_CorSpearman$X2,
                                             ref_CorSpearman$ObjName))

  for(k in 1:length(SpearmanChanges)) colnames(SpearmanChanges[[k]]) <- sdn



  # Linear Regresion Changes
  LinearRegresionChanges <- list()
  LinearRegresionChanges[[1]] <- as.data.frame(cbind(rep("_var_y_", nrow(ref_LinearRegresion)),
                                                     ref_LinearRegresion$y_details,
                                                     ref_LinearRegresion$ObjName))

  LinearRegresionChanges[[2]] <- as.data.frame(cbind(rep("_all_var_x_", nrow(ref_LinearRegresion)),
                                                     ref_LinearRegresion$x_details,
                                                     ref_LinearRegresion$ObjName))
  for(k in 1:length(LinearRegresionChanges)) colnames(LinearRegresionChanges[[k]]) <- sdn

  All_Changes <- list(NormalChanges, PearsonChanges, SpearmanChanges, LinearRegresionChanges)
  names(All_Changes) <- adress

  # Generate.SpecificSentences.RegresionLineal <- function(GS, ces, All_Reference, GeneralChanges, All_Changes) {


  # Empty Structure
  Empty_Structure <- list()

  # Specific Sentences
  Empty_Structure <- namel(names(GS))
  for(k in names(Empty_Structure)){
    opt01 <- All_Reference[[k]][,ncol(All_Reference[[k]])]
    opt02 <- rep(NA, ces[k])
    Empty_Structure[[k]] <- namel2(names = opt01, vec = opt02) # list(rep(GS[[k]], ces[k]))

    remove(opt01, opt02)
  }

  # New: SpecificSentences
  SpecificSentences <- Empty_Structure

  # Load to SpecificSentences from GS
  for(k1 in names(SpecificSentences)) for(k2 in 1:length(SpecificSentences[[k1]])){

    SpecificSentences[[k1]][[k2]] <- GS[[k1]]

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
    for(k1 in names(All_Changes)) {
      for(k2 in 1:length(All_Changes[[k1]])) {
        for(k3 in 1:nrow(All_Changes[[k1]][[k2]])) {

        obj_name_to_chance <- All_Changes[[k1]][[k2]]$Destiny[k3]

        SpecificSentences[[k1]][[obj_name_to_chance]] <- gsub(pattern = All_Changes[[k1]][[k2]]$"Pattern"[k3],
                                                replacement = All_Changes[[k1]][[k2]]$"Replacement"[k3],
                                                x = SpecificSentences[[k1]][[obj_name_to_chance]])

          # SpecificSentences[[k1]][[k2]] <- stri_replace_all_fixed(str = SpecificSentences[[k1]][[k2]],
          #                                                         pattern = All_Changes[[k1]][[k3]]$"Pattern"[k4],
          #                                                         replacement = All_Changes[[k1]][[k3]]$"Replacement"[k4],
          #                                                         vectorize_all = F)
        }
      }
    }





  # Fusion Sentences (FS)
  FS <- SpecificSentences
  for(k1 in names(FS)) for(k2 in 1:length(FS[[k1]])){

    destino <- All_Reference[[k1]][k2, ncol(All_Reference[[k1]])]
    sentencia <- SpecificSentences[[k1]][[k2]]
    FS[[k1]][[k2]] <- paste0(destino, " <- ", sentencia)

    remove(destino, sentencia)
  }



  internal_code <- namel2(names(FS), "")
  for(k1 in names(FS)) for(k2 in 1:length(FS[[k1]]))  internal_code[[k1]] <- paste0(unlist(FS[[k1]]), collapse = "\n")
  for(k1 in names(internal_code)) internal_code[[k1]] <- paste0(paste0("# ", k1), "\n", internal_code[[k1]], "\n\n", collapse = "")

  the_code[[4]] <- paste0(internal_code, collapse = "\n")

  the_code[[5]] <- "\n\n\n\n\n\n\n\n"

  ROutput <- Empty_Structure
  for(k1 in names(FS)) for(k2 in names(FS[[k1]])) {

    # Esto es la ejecucion de cada sentencia
    # Al evaluar la sentencia se ejecuta el test y todo se guarda en
    # un objeto de nombre previamente definido
    obj_name <- strsplit(FS[[k1]][[k2]], " <- ")[[1]][1]

    eval(parse(text = FS[[k1]][[k2]]))

    # Creamos dos objetos que nos serviran, por un lado para asignar el
    # nuevo objeto a la lista de salidas de R...
    # Inmediatamente luego de ser asignado, el objeto creado en el paso anterior
    # es eliminado.
    # La idea de esto es que el script ejecute tal cual cada sentnecia, y ver asi
    # que todo funciona.
    ROutput[[k1]][[k2]] <- eval(parse(text = obj_name))

    aver01 <- paste0("remove(", obj_name, ")")
    eval(parse(text = aver01))

    remove(obj_name)

  }



  #            eval(parse(text = code[[2]]))

  the_code <- unlist(the_code)
  WhoamI <- "Rsience.LinearRegresion()"

  the_exit <- list(ROutput, the_code, All_Reference, WhoamI)
  names(the_exit) <- c("ROutput", "the_code", "All_Reference", "WhoamI")

  return(the_exit)
}
