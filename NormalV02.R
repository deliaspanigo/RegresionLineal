
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
variable <- 2

# columnas_seleccionadas <- c(2:8)
columnas_seleccionadas <- c("cyl",  "disp", "hp")

RScience.TestNormalidad02 <- function(base = NULL,
                                      variable = NULL,
                                      alfa = 0.05,
                                      decimales = 4,
                                      corte = 0.001){


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


  # Detalles y Batalla Naval de la variable elegida
  orden_columna <- variable
  nombre_columna <- variable
  all_columns <- colnames(base)

  if(!is.numeric(variable)){
    dt_orden_general <- table(c(variable, all_columns)) == 2
    orden_columna <- c(1:length(dt_orden_general))[dt_orden_general]

  } else nombre_columna <- colnames(base)[orden_columna]

  letra_columna <- num2let(orden_columna)



  # Sentencia General
  sentencia_general <- "stats::shapiro.test(x = na.omit(base[,_the_var_]))"

  reemplazo <- paste0("'", nombre_columna, "'")
  sentencia_salida <- gsub("_the_var_", reemplazo, sentencia_general)


  n_total <- nrow(base)
  n_real <- nrow(na.omit(base[nombre_columna]))
  n_na <- n_total - n_real





  test_normalidad_var <- eval(parse(text = sentencia_salida))



  data_original <-  sapply(test_normalidad_var, base::unlist)
  data_original <- t(data_original)
  data_original <- as.data.frame(data_original)
  data_original[,1] <-  as.numeric(as.character(data_original[,1]))
  data_original[,2] <-  as.numeric(as.character(data_original[,2]))
  data_original <- cbind(nombre_columna, orden_columna, letra_columna, data_original)

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
                                      rotulos = nombre_columna)


  data_completa <- data.frame(nombre_columna,
                              orden_columna,
                              letra_columna,
                              data_original$method,
                              data_original$statistic.W,
                              n_total,
                              n_real,
                              n_na,
                              valor_p_original,
                              valor_p_redondeado,
                              valor_p_externo,
                              vector_alfa,
                              decision,
                              frase,
                              hipotesis_nula,
                              frase_explicativa,
                              sentencia_salida)

  colnames(data_completa) <- c("Variables",
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
  seleccion_resumen <- c(1, 2, 3, 4, 5, 6, 9, 12, 13, 14, 17)

  data_resumen <- data_completa[seleccion_resumen]

  # Un armado de referencia
  armado_referencia <- data_completa[,c(1,2,3)]


  salida <- list(armado_referencia, data_completa, data_resumen, data_original,
                 test_normalidad_var)
  names(salida) <- c("Referencias", "Tabla Completa", "Tabla Resumen",
                     "Tabla Original", "Test Normalidad")
  return(salida)

}



RScience.TestNormalidad <- function(base = NULL,
                                    columnas_seleccionadas = NULL,
                                    alfa = 0.05,
                                    decimales = 4,
                                    corte = 0.001) {
parte01 <- NULL
parte02 <- NULL
parte03 <- NULL
parte04 <- NULL
parte05 <- list()

columna_orden <- c(1:length(columnas_seleccionadas))

for(i in seq_along(columnas_seleccionadas)){


  nuevo <-   RScience.TestNormalidad02(base,
                            variable  = columnas_seleccionadas[i],
                            decimales = 4,
                            alfa = 0.05,
                            corte = 0.001)

  parte01 <- rbind(parte01, nuevo[[1]])
  parte02 <- rbind(parte02, nuevo[[2]])
  parte03 <- rbind(parte03, nuevo[[3]])
  parte04 <- rbind(parte04, nuevo[[4]])
  parte05[[i]] <- nuevo[[5]]

}

parte01 <- cbind(columna_orden, parte01)
parte02 <- cbind(columna_orden, parte02)
parte03 <- cbind(columna_orden, parte03)
parte04 <- cbind(columna_orden, parte04)

colnames(parte01)[1] <- "Orden Analisis"
colnames(parte02)[1] <- "Orden Analisis"
colnames(parte03)[1] <- "Orden Analisis"
colnames(parte04)[1] <- "Orden Analisis"

salida <- list(parte01, parte02, parte03, parte04, parte05)
names(salida) <- names(nuevo)

names(salida[[5]]) <- columnas_seleccionadas

return(salida)
}


RScience.TestNormalidad(base = base,
                        columnas_seleccionadas = columnas_seleccionadas,
                        alfa = alfa,
                        corte = corte)
