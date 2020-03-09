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

# inicialize range for tset  
min_hp_tset <- min(df$hp_tset)
max_hp_tset <- max(df$hp_tset) 

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

# for example:
time_to_predict <- as.POSIXct(x = "2019-01-15 23:00:00 UTC", tz = "UTC")
# there is a problem with the base spline of tfloor_l0 when doing the predict of mod_q
# Ill try looking the "i" values (watching different "times_to_predict") for which the error is occurring in order to try to infer where the problem is
# time_to_predict <- as.POSIXct(x = "2019-01-22 23:00:00 UTC", tz = "UTC")

# df_original <- df
# df <- df_original

############################################################## start (DEBUGGING THE PROBLEM)

# nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features)))
# class_per_feature = mapply(function(i){i[['class']]},features)
# nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features)
# levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
# names_per_feature = names(features)
# 
# # X = sample(c(0,1),nBits,replace=T)
# # params_hp_tset_24h <- c("30", "30", "30", "30", "30", "30", "30", "30", "30", "30", "30", "30",
# #                           "30", "30", "30", "30", "30", "30", "30", "30", "30", "30", "30", "30")
# params_hp_tset_24h <- decodeValueFromBin(X, class_per_feature, nclasses_per_feature, levels_per_feature = levels_per_feature)
# names(params_hp_tset_24h) <- names_per_feature
# 
# rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% as.Date(time_to_predict)
# hp_tset_24h = numeric(nrow(df))
# hp_tset_24h[rows_to_filter] <- params_hp_tset_24h
# 
# 
# # prediction_scenario <- function(...
# df <- tune_model_input(df,params)
# df <- df[rows_to_filter,]
# 
# hp_tset_24h <- hp_tset_24h[rows_to_filter]
# 
# df$date <- as.Date(df$time, tz="Europe/Berlin")
# ts_prediction <- smartAgg(df,"date",function(x){min(x,na.rm=T)},"time",catN = F)$time
# 
# calculate_df <- function(ts_prediction){
# 
#   # Filter the initial dataset
#   df_ <- df[df$time>=ts_prediction & df$time<=(ts_prediction+hours(23)),]
#   df_$ts_prediction <- ts_prediction
# 
#   # Assign the control variables of the scenario
#   df_$hp_tset_l0 <- as.numeric(hp_tset_24h[df$time %in% df_$time])
#   df_$hp_status_l0 <- ifelse(is.na(hp_tset_24h[df$time %in% df_$time]),0,1)
# 
#   # Iterate for each timestep (1 hour)
#   for (i in 1:nrow(df_)){
#     print(i)
#     #i=12
#     # Calculate the floor temperature and indoor temperature under free floating conditions
#     df_$hp_cons_l0[i] <- 0
#     tfloor_ff <- df_$tfloor_l0[i] <- predict(mod_tfloor, df_[i,])
#     ti_ff <- df_$ti_l0[i] <- predict(mod_ti, df_[i,])
#     # If the heat pump should be on, estimate the heat pump consumption and
#     # re-estimate the floor and indoor temperatures considering the heat input.
#     if(df_$hp_status_l0[i]==1 && !is.na(tfloor_ff) && !is.na(ti_ff)){
#       df_$tfloor_l0[i] <- df_$hp_tset_l0[i]
#       df_$hp_cons_l0[i] <- predict(mod_q, df_[i,])
#       if(df_$hp_cons_l0[i]>0){
#         df_$tfloor_l0[i] <- predict(mod_tfloor, df_[i,])
#         df_$ti_l0[i] <- predict(mod_ti, df_[i,])
#         # If heat pump consumption estimation is negative, then consider the indoor and
#         # floor temperatures estimated with free floating conditions
#       } else {
#         df_$hp_cons_l0[i] <- 0
#         df_$hp_status_l0[i] <- 0
#         df_$hp_tset_l0[i] <- NA
#         df_$tfloor_l0[i] <- tfloor_ff
#         df_$ti_l0[i] <- ti_ff
#       }
#     }
# 
#     # Reassign the ti calculated values to next timesteps lagged values
#     for (l in 1:max(params[c("mod_tfloor_ar","mod_hp_cons_lags_tfloor","mod_ti_lags_dti")])){
#       tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("tfloor_l",l)] <- df_$tfloor_l0[i]}}, error=function(e){next})
#     }
#     for (l in 1:max(params[c("mod_hp_cons_ar","mod_tfloor_lags_hp_cons","mod_tfloor_ar","mod_ti_lags_dti")])){
#       tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("hp_cons_l",l)] <- df_$hp_cons_l0[i]}}, error=function(e){next})
#       tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("hp_status_l",l)] <- df_$hp_status_l0[i]}}, error=function(e){next})
#     }
#     for (l in 1:max(params[c("mod_ti_ar","mod_ti_lags_infiltrations")])){
#       tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("ti_l",l)] <- df_$ti_l0[i]}}, error=function(e){next})
#     }
#     df$dti[i] <- df$ti_l0[i] - df$tfloor_l0[i]
#     for (l in 1:max(c(params[c("mod_ti_lags_dti","mod_tfloor_lags_dti")]))){
#       tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("dti_l",l)] <- df_$dti_l0[i]}}, error=function(e){next})
#     }
#     df$dte_l0[i] <- (rowMeans(data.frame(df$ti_l1[i],df$ti_l0[i])) - rowMeans(data.frame(df$te_l1[i],df$te_l0[i])))
#     df$infiltrations_l0[i] <- ifelse(df$dte_l0[i]>0,df$dte_l0[i],0) * df$windSpeed[i]
#     for (l in 1:max(c(params[c("mod_ti_lags_infiltrations")]))){
#       tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("infiltrations_l",l)] <- df_$infiltrations_l0[i]}}, error=function(e){next})
#     }
#     df$dtf_l0[i] <- df$tfloor_l0[i] - df$te_l0[i]
#     for (l in 1:max(c(params[c("mod_hp_cons_lags_tfloor")]))){
#       tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("dtf_l",l)] <- df_$dtf_l0[i]}}, error=function(e){next})
#     }
#   }
# 
#   return(df_)
# }
# 
# df_test <- calculate_df(ts_prediction)
# score <- sum(df_price$price * df_test$hp_cons_l0)

############################################################## end (DEBUGGING THE PROBLEM)



optimization_results <- suppressMessages(
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
)

params_hp_tset_24h <- decodeValueFromBin(binary_representation = optimization_results@solution[1,],
                                         class_per_feature = mapply(function(i){i[['class']]},features),
                                         nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
                                         levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
                                         )

names(params) <- names(features)
print(params)




# TODO: arent we using the measured consumption to do the prediction_  






