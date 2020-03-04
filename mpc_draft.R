# info:
# 
# The output should be the prediction of the building’s representative temperature for the next hour. 
# 
# The boundary conditions data provided in the attachment could be used to develop the controller. 
# The output of the designed MPC would be a vector composed of two arrays. 
# The heat pumps status and heat pump’s supply water temperature.

# this data will come from
# the systems measurements (at time t-1):
# "time"
# "hp_cons"
# "hp_status"  
# "hp_tset"
# "hp_cop"
# "tfloor"     
# "ti"
# "hg"
# "vent"       
# 
# this data will come from
# weather forecast (at time t):
# "te"
# "GHI"
# "BHI"        
# "sunAz"
# "sunEl"
# "humidity"   
# "windSpeed"
# "windBearing"
# 
# the models (calculate_mode_<variable>) need:
# - params (the optimized parameters 
#           for the pre-tuning of variables: 
#           alpha, lags, ar, nharmonics)
# - df (contains all the variables of the merge 
#       between house and weather with the 
#       respective results of the tuning. 
#       everything at time t)
# but the results of the training of the model 
# consist of considerably less variables than 
# the entire df.
# the complete df has ~100 => for example in: colnames(result_q$df)
# and the variables needed to calculate the forecasting with the model are ~ 17 => for example this is seen in: length(result_q$mod$coefficients)
# - train_dates (random sample of days chosen from the df$time column)
# 
# WRONG CONCEPT:
# we should develop the function which runs the model that will be inside the MPC.
# this function will be the result of the already trained model, but:
#   - the input df will be the one which only contains the "selected varibales" 
#   - the train_dates will be replaced with the "day to train"
# 
# MPC:

df_ <- df

# inicialize range for tset  
min_hp_tset <- min(df$hp_tset)
max_hp_tset <- max(df$hp_tset) 

# date_to_predict <- as.POSIXct(x = "2019-01-31 23:00:00 UTC", tz = "UTC") 
# rows_to_filter = seq(from = date_to_predict - days(1), to = date_to_predict - hours(1), by = "hour")
# df <- df[df$time %in% rows_to_filter, ]

df_price <- read_excel_allsheets("data/electricity_price.xlsx")$Sheet1
colnames(df_price) <- c("daytime", "price")
df_price$daytime <- c(0:23)

features <- list("0"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "1"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "2"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "3"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "4"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "5"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "6"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "7"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "8"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "9"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "10"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "11"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "12"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "13"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "14"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "15"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "16"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "17"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "18"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "19"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "20"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "21"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "22"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 "23"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete")
                 )

optimizer_MPC <- function(X, class_per_feature, nclasses_per_feature, names_per_feature, levels_per_feature, 
                          df, mod_q, mod_ti, mod_tfloor, df_price, time_to_predict, params){
  
  #X=sample(c(0,1),nBits,replace=T)
  params_hp_tset_24h <- decodeValueFromBin(X, class_per_feature, nclasses_per_feature, levels_per_feature = levels_per_feature)
  names(params_hp_tset_24h) <- names_per_feature

  # the predicted variables will be tagged as "_0" 
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_tfloor = mod_tfloor,
    df = df,
    rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% as.Date(time_to_predict),
    hp_tset_24h = params_hp_tset_24h, #ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    ts_prediction = NULL
  )
  
  # # constrains:
  # if (condition) {
  #   # max and min range (comfort bands) for ti defined based on the time of the day
  #   temp_min = c(rep(x = 20, times = 12), rep(x = 17, times = 12))
  #   temp_max = c(rep(x = 28, times = 12), rep(x = 25, times = 12))
  #   # should repeat this for the tfloor?
  # }
  
  # cost function:
  # sum over 24 hours
  # check units (in price is euro/MWh and consumption dont know if is MWh or kWh)??
  score <- sum(df_price$price*predv$hp_cons_l0)

  if (is.finite(score)){
    return(score)
  } else {return(-10000000000000)}
}

# for example:
time_to_predict <- as.POSIXct(x = "2019-01-31 23:00:00 UTC", tz = "UTC") 

ga(
  type = "binary",
  fitness = optimizer_MPC,
  nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features))),
  class_per_feature = mapply(function(i){i[['class']]},features),
  nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
  levels_per_feature = lapply(function(i){i[["levels"]]}, X = features), 
  names_per_feature = names(features),
  time_to_predict = time_to_predict,
  params = params,
  mod_q = mod_q,
  mod_ti = mod_ti,
  mod_tfloor = mod_tfloor,
  df_price = df_price,
  selection = gabin_tourSelection,
  df = df,
  popSize = 32,
  maxiter = 10,
  monitor = gaMonitor2,
  parallel = 8,
  elitism = 0.08,
  pmutation = 0.05
)

# TODO: arent we using the measured consumption to do the prediction_  
  
