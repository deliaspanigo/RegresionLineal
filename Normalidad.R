
# Librerias
library("stats")
# namel<-function (vec){


source("lib.R")

namel(c("A", "B"))
################################################################################################


base <- mtcars
base[1,1] <- NA
base[3,3] <- NA

decimales <- 4
corte <- 0.001
alfa <- 0.05
# columnas_seleccionadas <- c(2:8)
columnas_seleccionadas <- c("cyl",  "disp", "hp")

RScience.TestNormalidad <- function(base = NULL, columnas_seleccionadas = c(1:ncol(base)), alfa = 0.05,
                                    decimales = 4, corte = 0.001){


  # Valor p > alfa
  frase_mayor <- "El valor p es _valor_p_.
                    El valor alfa es _alfa_.
                    El valor p es mayor al valor de alfa, por lo tanto NO se rechaza la hipótesis nula.
                    La variable _mi_variable_ posee una distribución estadísticamente normal."

  # Valor p = alfa
  frase_igual <- "El valor p es _valor_p_.
                    El valor alfa es _alfa_.
                    El valor p es igual al valor de alfa, por lo tanto NO se rechaza la hipótesis nula.
                    La variable _mi_variable_ posee una distribución estadísticamente normal."

  # Valor p < alfa
  frase_menor <- "El valor p es _valor_p_.
                    El valor alfa es _alfa_.
                    El valor p es menor al valor de alfa, por lo tanto se rechaza la hipótesis nula.
                    La variable _mi_variable_ no posee una distribución estadísticamente normal."


  all_columns <- colnames(base)


# Test de Normalidad
################################################################################

# Vamos a tener una base y una base_mod.
# Esto es para poder hacer un na.omit() a cada variable por separado.
# Preparamos entonces las sentencias prefabricadas para base_mod.

  sentencia_general <- "stats::shapiro.test(base_mod[,_the_var_])"

  # Aqui salen el orden de analisis, el nombre de las variables, las letras
  # y el numero respecto a las columnas de la base de datos.
  armado_general <- EstructuraGeneral(all_columns = all_columns,
                                      columnas_seleccionadas = columnas_seleccionadas)

  # Armamos las sentencias de ejecucion
  for(i in seq_along(mis_variables)){
    reemplazo <- paste0("'", armado_general$mis_variables[i], "'")
    armado_general$cada_sentencia[i] <- gsub("_the_var_", reemplazo, sentencia_general)
  }






  # Test de Normalidad
  test_normalidad_var <- namel(mis_variables)
  for(i in seq_along(cada_sentencia)){

      # Base Modificada para cada analsis
      base_mod <- na.omit(base[columnas_seleccionadas[i]])

      # Detealles variosQGIS
      vector_n_original[i] <- nrow(base)
      vector_na[i] <- sum(is.na(base[mis_variables[i]]))
      vector_n_final[i] <- nrow(base_mod)

      test_normalidad_var[[i]] <-  eval(parse(text = armado_general$cada_sentencia[i]))

  }


  data_original <-  sapply(test_normalidad_var, base::unlist)
  data_original <- t(data_original)
  data_original <- as.data.frame(data_original)
  data_original[,1] <-  as.numeric(as.character(data_original[,1]))
  data_original[,2] <-  as.numeric(as.character(data_original[,2]))

  # Separamos algun objeto que otro
  valor_p_original <- data_original$p.value

  # Mas futuras Nuevas columnas
  valor_p_redondeado <- round2(valor_p_original, decimales)
  valor_p_externo <- pValorExterno(valor_p = valor_p_redondeado, corte = corte)

  vector_alfa <- rep(alfa, length(valor_p_externo))
  decision <- Decision(valor_p = valor_p_original, alfa = alfa)
  frase <- ArmadoFrase(decision = decision,
                       frase_no = "No posee distribucion Normal",
                       frase_si = "Posee distribucion Normal")

  hipotesis_nula <- ArmadoFrase(decision = decision,
                                frase_no = "No rechazo Hipotesis Nula",
                                frase_si = "Rechazo Hipotesis Nula")


  frase_explicativa <- ArmadoEspecial(valor_p_original = valor_p_original,
                                      valor_p_externo = valor_p_externo,
                                      alfa = alfa,
                                      frase_mayor = frase_mayor,
                                      frase_igual = frase_igual,
                                      frase_menor = frase_menor,
                                      rotulos = mis_variables)


  data_completa <- data.frame(orden_analisis,
                            mis_variables,
                            orden_base,
                            mis_letras,
                            data_original$method,
                            data_original$statistic.W,
                            vector_n_original,
                            vector_na,
                            vector_n_final,
                            valor_p_original,
                            valor_p_redondeado,
                            valor_p_externo,
                            vector_alfa,
                            decision,
                            frase,
                            hipotesis_nula,
                            frase_explicativa,
                            cada_sentencia)

  colnames(data_completa) <- c("Orden Analisis",
                               "Variables",
                               "Numero Columna",
                               "Letra Columna",
                               "Test",
                               "Estadistico W",
                               "n Base",
                               "Cantidad NA",
                               "n Minibase",
                               "Valor p original",
                               "Valor p redondeado",
                               "Valor p externo",
                               "Alfa",
                               "¿Posee distribucion Normal?",
                               "Frase",
                               "Hipotesis",
                               "Explicacion",
                               "Sentencias")

  data_completa$`Estadistico W` <- round2(data_completa$`Estadistico W`, decimales)

  # Tabla Resumen
  seleccion_resumen <- c(1, 2, 4, 5, 6, 9, 12, 13, 14, 17)

  data_resumen <- data_completa[seleccion_resumen]

  # Un armado de referencia
  armado_referencia <- data_completa[,c(1,2,3)]


  salida <- list(armado_referencia, data_completa, data_resumen, data_original, test_normalidad_var)
  names(salida) <- c("Referencias", "Tabla Completa", "Tabla Resumen", "Tabla Original", "Test Normalidad")
  return(salida)

}


mis_analisis_normalidad_var <- RScience.TestNormalidad(base = base, columnas_seleccionadas =  c(3:6),
                        decimales = 4,alfa = 0.05, corte = 0.001)

mis_analisis_normalidad_var[[2]]
