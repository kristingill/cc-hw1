#' temperature risk
#'
#' compute temperature or precipitation risk for urban dwellers
#' @param data climate data like temperature or precipitation
#' @param threshold a threshold of temperature or precipitation
#' @param consecutive_days, length of the heat or precipitation event
#' @param date, as date
#' @return consecutive_days

consecutive = function(data, consecutive_days, threshold) {
  
  # create empty lists
  temp_over <- list()
  
  temp_final <- list()
  
  mean_temp <- 0
  
  mean_temp_all <- list()
  
  cons_days_all <- list()
  
  for (i in 1:length(temp)) {
    
    # if temperature is over the temperature threshold, add it to the list
    if (data[i] > threshold) {
      temp_over <- append(temp_over, data[i])
    }
    
    else {
      # how many days the temperature is over the threshold, can count this now that the condition is no longer met
      cons_days <- length(temp_over) 
      
      # if the length of time that temperature is over the threshold meets our conditions for an extreme event, 
      # add this list of temperatures to the final list
      if (cons_days >= consecutive_days){
        temp_final <- append(temp_final, temp_over)
        mean_temp <- mean(as.numeric(temp_over))
        mean_temp_all <- append(mean_temp_all, mean_temp)
        cons_days_all <- append(cons_days_all, cons_days)
      }
      
      # restart the empty list to find the next event
      temp_over = list()
    }
  }
  
  consecutive_days <- as.data.frame(unlist(mean_temp_all)) %>% 
    mutate(cons_days = unlist(cons_days_all))
  
  return(consecutive_days = consecutive_days)
}