update_input_data = function(input_data, year, catch, surveys) {
  
  input_data$obsC  = c(input_data$obsC, catch$catch)
  input_data$timeC = c(input_data$timeC, input_data$timeC[length(input_data$timeC)] + 1)
  input_data$dtc   = c(input_data$dtc, 1)
  
  if (year == 1) {
    
    input_data$obsI[[5]] = surveys$I1
    input_data$obsI[[6]] = surveys$I2
    input_data$obsI[[7]] = surveys$I3
    input_data$obsI[[8]] = surveys$I4
    
    input_data$timeI[[5]] = 2020 + year + 0.5
    input_data$timeI[[6]] = 2020 + year + 0.5
    input_data$timeI[[7]] = 2020 + year + 0.5
    input_data$timeI[[8]] = 2020 + year + 0.5
    
    input_data$stdevfacI[[5]] = 1
    input_data$stdevfacI[[6]] = 1
    input_data$stdevfacI[[7]] = 1
    input_data$stdevfacI[[8]] = 1
      
  } else {
    
    input_data$obsI[[5]] = c(input_data$obsI[[5]], surveys$I1)
    input_data$obsI[[6]] = c(input_data$obsI[[6]], surveys$I2)
    input_data$obsI[[7]] = c(input_data$obsI[[7]], surveys$I3)
    input_data$obsI[[8]] = c(input_data$obsI[[8]], surveys$I4)
    
    input_data$timeI[[5]] = c(input_data$timeI[[5]], input_data$timeI[[5]][year-1] + 1) 
    input_data$timeI[[6]] = c(input_data$timeI[[6]], input_data$timeI[[6]][year-1] + 1) 
    input_data$timeI[[7]] = c(input_data$timeI[[7]], input_data$timeI[[7]][year-1] + 1) 
    input_data$timeI[[8]] = c(input_data$timeI[[8]], input_data$timeI[[8]][year-1] + 1) 
    
    input_data$stdevfacI[[5]] = c(input_data$stdevfacI[[5]], 1)
    input_data$stdevfacI[[6]] = c(input_data$stdevfacI[[6]], 1)
    input_data$stdevfacI[[7]] = c(input_data$stdevfacI[[7]], 1)
    input_data$stdevfacI[[8]] = c(input_data$stdevfacI[[8]], 1)
    
  }
 
  return(input_data)
   
}


# 
# input2 = update_input_data(input_data = input_data, year = 1, surveys = surveys)
# 
# input3 = update_input_data(input_data = input2, year = 2, surveys = surveys)
# 
# input4 = update_input_data(input_data = input3, year = 3, surveys = surveys)
# 




