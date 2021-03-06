
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
limit <- 0.001
alpha <- 0.05
# vars <- 2

# vars <- c(2:8)
# vars <- c("cyl",  "disp", "hp")
vars <- c("cyl")
language = "EN"

RScience.TestNormalidad <- function(database = NULL,
                                      vars = NULL,
                                      alpha = 0.05,
                                      decimals = 4,
                                      limit = 0.001,
                                      language = "EN"){

  # Explanation about substitutions
  # _the_var_ : is a specific colname(database), only one

  if(length(vars) == 1) {

      # Internal Langueage options
      the_languages <- c("EN", "ES", "IT")

      # Languge options (list)
      language_options <- list()


      # Language01
      the_rows01 <- c("major_phrase", "equal_phrase", "minor_phrase")
      language_options[[1]] <- as.data.frame(matrix(NA, length(the_rows01), length(the_languages)))
      colnames(language_options[[1]]) <- the_languages
      rownames(language_options[[1]]) <- the_rows01

      # EN
      language_options[[1]]["major_phrase", "EN"] <- "El valor p es _valor_p_.
                            El valor alpha es _alpha_.
                            El valor p es mayor al valor de alpha, por lo tanto NO se rechaza la hipótesis nula.
                            La variable _mi_variable_ posee una distribución estadísticamente normal."

      language_options[[1]]["equal_phrase", "EN"] <- "El valor p es _valor_p_.
                          El valor alpha es _alpha_.
                          El valor p es igual al valor de alpha, por lo tanto NO se rechaza la hipótesis nula.
                          La variable _mi_variable_ posee una distribución estadísticamente normal."

      language_options[[1]]["minor_phrase", "EN"] <- "El valor p es _valor_p_.
                          El valor alpha es _alpha_.
                          El valor p es menor al valor de alpha, por lo tanto se rechaza la hipótesis nula.
                          La variable _mi_variable_ no posee una distribución estadísticamente normal."


      language_options[[1]][, "ES"] <- language_options[[1]][, "EN"]
      language_options[[1]][, "IT"] <- language_options[[1]][, "EN"]





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



        # Sentencia General
        general_sentence <- "stats::shapiro.test(x = na.omit(database[,_the_var_]))"

        # Repleacement and each sentence
        replacement <- paste0("'", column_name, "'")
        each_sentence <- gsub(pattern = "_the_var_",
                              replacement = replacement,
                              x = general_sentence)

        # Diferents counts
        n_total <- nrow(database)
        n_real <- nrow(na.omit(database[column_name]))
        n_na <- n_total - n_real




        # Normal Test (Shapiro-Wilk)
        test.normal <- eval(parse(text = each_sentence))
    #    test.normal <- list(test.normal)
    #    names(test.normal) <- vars


        # The original information and details (tabla01)
        results.original <-  sapply(test.normal, base::unlist)
        results.original <- t(results.original)
        results.original <- as.data.frame(results.original)
        results.original[,1] <-  as.numeric(as.character(results.original[,1]))
        results.original[,2] <-  as.numeric(as.character(results.original[,2]))
        results.original <- cbind(column_name, column_order, column_letter, results.original)

      # Same importants objects about pvalues
      pvalue <- pValueAll(pvalue_original = results.original$p.value, decimals = decimals,
                          limit = limit)

      # Vector alpha
      vector_alpha <- rep(alpha, length(pvalue$"Original"))

      # Decision and phrase 01
      decision01 <- pValueDecision(pvalue_original = pvalue$Original, alpha = alpha, language = language )
      phrase01 <- ArmadoFrase(decision = decision01,
                           frase_no = "No posee distribucion Normal",
                           frase_si = "Posee distribucion Normal")

      ##############################################################################
      # Decision and phrase 02
      decision02 <- ArmadoFrase(decision = decision01,
                                    frase_no = "No rechazo Hipotesis Nula",
                                    frase_si = "Rechazo Hipotesis Nula")


      phrase02 <- ArmadoEspecial(valor_p_original = pvalue$"Original",
                                          valor_p_externo = pvalue$"External",
                                          alfa = alpha,
                                          frase_mayor = language_options[[1]]['major_phrase', language],
                                          frase_igual = language_options[[1]]['equal_phrase', language],
                                          frase_menor = language_options[[1]]['minor_phrase', language],
                                          rotulos = column_name)

      ##############################################################################


      results.completed <- data.frame(column_name,
                                  column_order,
                                  column_letter,
                                  results.original$method,
                                  results.original$statistic.W,
                                  n_total,
                                  n_real,
                                  n_na,
                                  pvalue$"Original",
                                  pvalue$"Rounded",
                                  pvalue$"External",
                                  vector_alpha,
                                  decision01,
                                  phrase01,
                                  decision02,
                                  phrase02,
                                  each_sentence)

      colnames(results.completed) <- c("Variables", # 1
                                   "Numero Columna",# 2
                                   "Letra Columna", # 3
                                   "Test", # 4
                                   "Estadistico W", # 5
                                   "n Base", # 6
                                   "Cantidad NA", # 7
                                   "n Minibase", # 8
                                   "Valor p original", # 9
                                   "Valor p redondeado", # 10
                                   "Valor p externo", # 11
                                   "alpha", # 12
                                   "¿Posee distribucion Normal?", # 13
                                   "Frase", # 14
                                   "Hipotesis", # 15
                                   "Explicacion", # 16
                                   "Sentencias") #17

      results.completed[,5] <- round2(results.completed[,5], decimals)

      # Resume Results
      special_selecion <- c(1, 2, 3, 4, 5, 6, 9, 12, 13, 14, 17)

      results.resume <- results.completed[special_selecion]

      # Un armado de referencia
      referencens <- as.data.frame(cbind(column_name, column_order, column_letter))


      the_exit <- list(referencens, results.completed, results.resume,
                       results.original, list(test.normal))

      names(the_exit) <- c("Referencias", "Tabla Completa", "Tabla Resumen",
                         "Tabla Original", vars)

      return(the_exit)


  } else
    if(length(vars) > 1) {


      statistic_order <- c(1:length(vars))

      for(i in seq_along(vars)){


          new_report <-   RScience.TestNormalidad(database = database,
                                             vars  = vars[i],
                                             decimals = 4,
                                             alpha = 0.05,
                                             limit = 0.001)

        if(i == 1) all_report <- new_report else
          if(i > 1) {

            all_report[[1]] <- rbind(all_report[[1]], new_report[[1]])
            all_report[[2]] <- rbind(all_report[[2]], new_report[[2]])
            all_report[[3]] <- rbind(all_report[[3]], new_report[[3]])
            all_report[[4]] <- rbind(all_report[[4]], new_report[[4]])
            all_report[[5]][[i]] <- new_report[[5]][[1]]
           # names(all_report[[5]])[i] < vars[i]
          }
      }

      names(all_report[[5]]) <- vars



#      names(all_report[[5]]) <- vars

      return(all_report)


    }

}



# RScience.TestNormalidad <- function(base = NULL,
#                                     vars = NULL,
#                                     alpha = 0.05,
#                                     decimals = 4,
#                                     limit = 0.001) {
# }


all_report <- RScience.TestNormalidad(database = database,
                        vars = vars,
                        alpha = alpha,
                        limit = limit)

all_report[[5]]
