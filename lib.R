
library(stringi) # https://stackoverflow.com/questions/33949945/replace-multiple-strings-in-one-gsub-or-chartr-statement-in-r


Rscience.languages <- function(name = NULL){

  the_languages <- c("EN", "ES", "IT")
  the_functions <- c("pValueDecision")

  language_functions <- namel(the_functions)

  for(k in 1:length(language_functions)) language_functions[[k]] <- namel(the_languages)

  remove(k)

  # 1) pValueDecision()
  {
  set_default <- c("No", "Yes")
  set_matrix <- as.data.frame(matrix(NA, length(set_default), length(the_languages)))
  rownames(set_matrix) <- set_default
  colnames(set_matrix) <- the_languages
  set_matrix$EN <- c("No", "Yes")
  set_matrix$ES <- c("No", "Si")
  set_matrix$IT <- c("No", "Si")

  language_functions[["pValueDecision"]] <- list(set_matrix)

  remove(set_default, set_matrix)
  }


  return(language_functions[[name]])

}

round2 <- function(x, n) {
  posneg <- sign(x)

  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z*posneg
}

pValueAll <- function(pvalue_original = NULL, decimals = 4, limit = 0.001){

  pvalue <- namel(c("Original", "Rounded", "External"))
  pvalue[["Original"]] <- pvalue_original
  pvalue[["Rounded"]] <- round2(pvalue_original, decimals)
  pvalue[["External"]] <- pValorExterno(valor_p = pvalue$Rounded, corte = limit)

  valor_p_externo <- pvalue[["Rounded"]]
  valor_p_externo[pvalue[["Rounded"]] < limit] <- paste0("<", limit)

  pvalue[["External"]] <- valor_p_externo

  return(pvalue)
}


pValueDecision <- function(pvalue_original = NULL, alpha = NULL, language = "EN"){

  # Take specifics language options for 'pValueDecision'
  language_options <- Rscience.languages(name = "pValueDecision")[[1]]

  # Empty decision
  decision <- rep(NA, length(pvalue_original))

  # If pvalue >= alpha
  decision[pvalue_original >= alpha] <- language_options["No", language]

  # If pvalue < alpha
  decision[pvalue_original < alpha] <- language_options["Yes", language]



  return(decision)

}

ArmadoFrase <- function(decision = NULL, frase_no = NULL, frase_si = NULL, language = "EN"){

  # Vector inicial
  frase <- rep(NA, length(decision))

  # Posicion para si y no
  frase[decision == "No"] <- frase_no
  frase[decision == "Si"] <- frase_si

  # Salida exitosa
  return(frase)
}

ArmadoEspecial <- function(valor_p_original = NULL,
                           valor_p_externo = NULL,
                           alfa = NULL, rotulos = NULL,
                           frase_mayor = NULL, frase_igual = NULL,
                           frase_menor = NULL){

  # Armado inicial
  frase_especial <- rep(NA, length(valor_p_original))

  # Frase mayor
  frase_especial[valor_p_original > alfa] <- frase_mayor

  # Frase igual
  frase_especial[valor_p_original == alfa] <- frase_igual

  # Frase menor
  frase_especial[valor_p_original < alfa] <- frase_menor

  # Colocamos el valor alfa
  frase_especial <- gsub("_alfa_", alfa, frase_especial)

  # Coloramos el valor p externo
  frase_especial <- sapply(seq_along(frase_especial), function(x) gsub("_valor_p_", valor_p_externo[x], frase_especial[x]))

  # Colocamos al nombre de la variable
  frase_especial <- sapply(seq_along(frase_especial), function(x) gsub("_mi_variable_", rotulos[x], frase_especial[x]))


  return(frase_especial)
}

num2let <- function(n, lets = LETTERS) {
  base <- length(lets)
  if (length(n) > 1) return(sapply(n, num2let, lets = lets))
  stopifnot(n > 0)
  out <- ""
  repeat {
    if (n > base) {
      rem <- (n-1) %% base
      n <- (n-1) %/% base
      out <- paste0(lets[rem+1], out)
    } else return( paste0(lets[n], out) )
  }
}


let2num <- function(x, lets = LETTERS) {
  base <- length(lets)
  s <- strsplit(x, "")
  sapply(s, function(x) sum((match(x, lets)) * base ^ seq(length(x) - 1, 0)))
}


#helper function (convert vector to named list)
namel<-function (vec){
  tmp <-as.list(vec)
  names(tmp) <- as.character(unlist(vec))
  tmp
}

#helper function (convert vector to named list)
namel.na<-function (vec){
  tmp <- as.list(rep(NA, length(vec)))
  names(tmp) <- as.character(unlist(vec))
  tmp
}



#helper function (convert vector to named list)
namel2<-function (vec, names){
  tmp <-as.list(vec)
  names(tmp) <- names
  tmp
}

OpcionesDeColumnas <- function(my_names = ""){

  # Letras
  letras_elegidas <- paste0("(", num2let(c(1:length(my_names))), ")")

  # Visual del usuario
  visual_usuario <- paste0(letras_elegidas, " - ", my_names)


  # Armamos el vector de salida
  vector_salida <- my_names
  names(vector_salida) <- visual_usuario

  return(vector_salida)
}



MyLetter <- function(Base = NULL, the_col = NULL) {



  if(is.null(Base)) return(NULL)
  if(is.null(the_col)) return(NULL)
  if(the_col == "") return(NULL)
  if(sum(colnames(Base) == the_col) == 0) return(NULL)

  dt_col <- colnames(Base) == the_col
  pos_col <- c(1:length(dt_col))
  the_col <- pos_col[dt_col]
  my_letter <- num2let(the_col)

  return(my_letter)
}



EstructuraGeneral <- function(all_columns = NULL,
                              columnas_seleccionadas = NULL,
                              base = NULL){


  vector_n_original <- nrow(base)
  vector_n_final <- nrow(na.omit(base))
  vector_na <- vector_n_original
  vector_n_final <-
  cada_sentencia <- rep(NA, length(columnas_seleccionadas))

  # Detalle de las variables seleccionadas
  # Necesitamos tener tanto el numero de orden como el nombre
  # El ingreso de las columnas seleccionadas puede ser en numero o con
  # nombre de categorias.
  orden_analisis <- seq_along(columnas_seleccionadas)
  orden_base <- columnas_seleccionadas
  mis_variables <- columnas_seleccionadas
  if(!is.numeric(columnas_seleccionadas)){
    dt_orden_general <- table(c(columnas_seleccionadas, all_columns)) == 2
    orden_base <- c(1:length(dt_orden_general))[dt_orden_general]

  } else mis_variables <- colnames(base)[orden_base]

  mis_letras <- num2let(orden_base)




  salida <- data.frame(orden_analisis, mis_variables, mis_letras, orden_base,
                       vector_n_original, vector_na, vector_n_final, cada_sentencia)

  return(salida)
}



