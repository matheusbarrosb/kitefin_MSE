make_input_data = function(data_directory) {
  
  raw_data = read.csv(paste0(data_directory, "gata.csv")) # read-in data
  
  catch_years = min(raw_data$timeC):max(raw_data$timeC)
  n_years     = length(catch_years)
  
  # indexing
  idx_01 = 15:27
  idx_02 = 6:21
  idx_03 = 33:40
  idx_04 = 26:31
  
  # survey indices
  obsI = list(
    raw_data$obsI[idx_01],    # gillnet 
    raw_data$obsI.1[idx_02],  # handline
    raw_data$obsI.2[idx_03],  # DCF 1
    raw_data$obsI.3[idx_04]   # DCF 2
  )
  
  timeI = list(
    raw_data$timeI[idx_01] + 0.5,
    raw_data$timeI[idx_02] + 0.5,
    raw_data$timeI[idx_03] + 0.5,
    raw_data$timeI[idx_04] + 0.5
  )
  
  # CVs to SDs
  stdevfacI = list(
    ifelse(is.na(raw_data$cv.obsI[idx_01]), 1, raw_data$cv.obsI[idx_01]/mean(raw_data$cv.obsI[idx_01], na.rm = TRUE)),
    ifelse(is.na(raw_data$cv.obsI.1[idx_02]), 1, raw_data$cv.obsI.1[idx_02]/mean(raw_data$cv.obsI.1[idx_02], na.rm = TRUE)),
    ifelse(is.na(raw_data$cv.obsI.2[idx_03]), 1, raw_data$cv.obsI.2[idx_03]/mean(raw_data$cv.obsI.2[idx_03], na.rm = TRUE)),
    ifelse(is.na(raw_data$cv.obsI.3[idx_04]), 1, raw_data$cv.obsI.3[idx_04]/mean(raw_data$cv.obsI.3[idx_04], na.rm = TRUE))
  )
  
  input_data = list(
    obsC      = raw_data$recons,
    timeC     = catch_years,
    dtc       = rep(1, n_years),
    obsI      = obsI,
    timeI     = timeI,
    stdevfacI = stdevfacI
  )
  
  # setup priors
  input_data$priors$logn      = c(log(2), 0.5, 1)
  input_data$priors$logsdc    = c(log(0.2)- 0.5*0.5^2, 0.5, 1)
  input_data$priors$logsdb    = c(log(0.07)- 0.5*0.5^2, 0.5, 1)
  input_data$priors$logr      = c(log(0.0525) - 0.5*0.5^2, 0.5, 1) # r prior based on fishbase
  input_data$priors$logbkfrac = c(log(0.8) - 0.5*0.5^2, 0.5, 1) # bK depletion level
  
  return(input_data)
  
}

# data_directory = file.path(here::here(), "data/")
# 
# make_input_data(data_directory = data_directory)

