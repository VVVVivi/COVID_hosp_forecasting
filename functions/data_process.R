cut_interval_vec <- function(data){
  
  ub <- max(data, na.rm = TRUE)*1.1
  lb <- 0
  
  local.min <- which.min(data)
  local.max <- which.max(data)
  
  incidence.vec <- data
  
  for (i in 1:length(local.min)){
    incidence.vec[local.min[i]] <- ifelse(local.min[i] == 0, local.min[i], lb)
  }
  
  incidence.vec[local.max] <- ub
  
  return(incidence.vec)
}

rw_train_1week <- function(data, i){
  if (i == 0) {
    return (NULL)
  }else{
    return(data[(data$year22 == 1) & (as.numeric(data$Week) <= i),])
  } 
}

rw_test <- function(data, i){
  return(data[(data$year22 == 1) & (as.numeric(data$Week) == i),])
}


rw_train_general <- function(data, i, nWeek_ahead){
  train <- data[(data$year22 != 1),]
  weeknum <- dim(train)[1]
  
  if(nWeek_ahead == 2){
    if(i <= 1){
      return(train[1:(weeknum-(1-i)),]) 
    }else{
      return(rbind(train, data[(data$year22 == 1) & (as.numeric(data$Week) < i),]))
    }
  }
  
  if(nWeek_ahead == 3){
    if(i <= 2){
      return(train[1:(weeknum-(2-i)),]) 
    }else{
      return(rbind(train, data[(data$year22 == 1) & (as.numeric(data$Week) < i-1),]))
    }
  }
  
  if(nWeek_ahead == 4){
    if(i <= 3){
      return(train[1:(weeknum-(3-i)),]) 
    }else{
      return(rbind(train, data[(data$year22 == 1) & (as.numeric(data$Week) < i-2),]))
    }
  }

}


maroMAE <- function(pred, num_category){
  macroMAE_vec <- c()
  for (i in 1:num_category){
    if (i %in% pred$Obs){
      mat <- pred[which(pred$Obs == i),]
      macroMAE_tmp <- mae(mat$Obs, mat$Pred)
    }else{
      macroMAE_tmp <- 0 
    }
    macroMAE_vec <- c(macroMAE_vec, macroMAE_tmp)
  }
  macroMAE <- sum(macroMAE_vec)/num_category
  
  return(macroMAE)
}
