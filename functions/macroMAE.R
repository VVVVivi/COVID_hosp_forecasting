maroMAE <- function(df_pred, num_category){
  macroMAE_vec <- c()
  for (i in 1:num_category){
    if (i %in% df_pred$hosp){
      mat <- df_pred[which(df_pred$hosp == i),]
      macroMAE_tmp <- mae(mat$hosp, mat$pred_level)
    }else{
      macroMAE_tmp <- 0 
    }
    macroMAE_vec <- c(macroMAE_vec, macroMAE_tmp)
  }
  macroMAE <- sum(macroMAE_vec)/num_category
  
  return(macroMAE)
}
