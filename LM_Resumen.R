
Rscience.LM.Resumen <- function(my_results = NULL){



# Count of models
count_models <- length(my_results[[2]])

# Colnames for a new data.frame()
new_cols <- rownames(my_results[[2]][[count_models]]$the_summary$coefficients)
sub_seleccion <- c(1, 2, 3, 4, 5, 6)

# Take the first to start... and after that rbind()
general_report <- my_results[[2]]$model.1[sub_seleccion]
if(count_models > 1){
  for(k1 in 2:count_models){
  for(k2 in 1:length(general_report)){
    general_report[[k2]] <- rbind(general_report[[k2]], my_results$AllModels[[k1]][[k2]])
  }
  }
}

##############################################################################################

# Is only one Y
all_y_details <- general_report$y_details
all_model_fit <- general_report$model_fit
all_pvalue_model_fit <- general_report$pvalue_model_fit
all_n_info <- general_report$n_info
all_sentence <- general_report$sentence

remove(general_report)

# All coeff - Intercept and Slopes
all_coeff <- as.data.frame(matrix(NA, count_models, length(new_cols)))
colnames(all_coeff) <- new_cols

# Al pvalue for coeff
all_pvalue_coeff <- all_coeff

# New Order!
for(k1 in 1:count_models){

  the_table <- my_results[[2]][[k1]]$the_summary $coefficients
  the_position <- rownames(the_table)
  the_coeff <- the_table[,1]
  the_pvalue_coeff <- the_table[,4]

  all_coeff[k1, the_position] <- the_coeff
  all_pvalue_coeff[k1, the_position] <- the_pvalue_coeff

  remove(the_table, the_position, the_coeff, the_pvalue_coeff)
}

##################################################################################



# Details about x vars for each model
new_rownames <- colnames(my_results$AllModels$model.1$x_details)
all_x_details <- as.data.frame(matrix(NA, count_models, length(new_rownames)))
colnames(all_x_details) <- new_rownames

# Count about how many x vars there are in each model
all_x_count <- rep(NA, nrow(all_x_details))

# For....
for(k1 in 1:count_models){

  the_table <- my_results[[2]][[k1]]$x_details
  all_x_count[k1] <- nrow(the_table) - 1
  for(k2 in 1:ncol(the_table)) all_x_details[k1, k2] <- paste0(the_table[,k2], collapse = ";")

}



#########################################

armado01 <- "all_y_details, all_x_details, all_n_info, all_coeff, all_pvalue_coeff, all_model_fit, all_pvalue_model_fit, all_x_count, all_sentence"

armado02 <- paste0("list(", armado01, ")")
armado03 <- gsub(", ", "', '", armado01)
armado04 <- paste0("c('", armado03, "')")

the_exit <- eval(parse(text = armado02))
names(the_exit) <- eval(parse(text = armado04))

whoamI <- "Rscience.LM.Resumen()"
the_exit <- list(whoamI, the_exit)
names(the_exit) <- c("whoamI?", "Resumen")


return(the_exit)
}


Rscience.LM.Resumen(my_results = my_results)
