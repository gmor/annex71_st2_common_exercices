source("functions.R")

# Libraries for mathematics
library(expm)
library(splines)
library(mgcv)

# Libraries for graphics
# library(cowplot)
library(ggplot2)
library(gridExtra)
# library(extrafont)
# loadfonts()

# Libraries for data wrangling
library(padr)
library(lubridate)
library(parallel)
library(oce)
library(GA)
library(data.table)
library(readxl) 


######################################################### IMPORT DATA #########################################################


house <- read_excel_allsheets("data/Twin_house_O5_exp1_60minR1.xlsx")$Sheet1
weather <- read_excel_allsheets("data/Twin_house_weather_exp1_60min_compensated.xlsx")$`Weather_Station_IBP_2018-11-01_`
weather[,c("sunAz","sunEl")] <- do.call(cbind,oce::sunAngle(weather$DATE,latitude = 47.874,longitude = 11.728))[,c("azimuth","altitude")]

df_house <- data.frame("time"=house$DATE,
                 "hp_cons"=house$hp_el_cons,
                 "hp_status"=house$`hp_status_command (u1)`,
                 "hp_tset"=house$`hp_supply_temp_command(u2)`,
                 "hp_cop"=house$COP,
                 "tsupply"=house$HeatPump_actual_Tsup,
                 "ti"=house$`Building's representative Temperature of the current time step(volume-averaged)`,
                 "hg"=rowSums(house[,grepl("_sum__hin_",colnames(house))]),
                 #"air_h"=rowSums(house[,grepl("_SUA_IHS_elP",colnames(house))]),
                 #"air_s"=rowSums(house[,grepl("_SUA_fan_elP",colnames(house))]),
                 #"air_e"=rowSums(house[,grepl("_EHA_fan_elP",colnames(house))]),
                 "vent"=rowSums(
                   data.frame((house$o5_Vent_child1_SUA_AT*house$o5_Vent_child1_SUA_VFR-house$o5_child1_AT*house$o5_Vent_child1_EHA_VFR),
                              house$o5_Vent_child2_SUA_AT*house$o5_Vent_child2_SUA_VFR-house$o5_child2_AT*house$o5_Vent_child2_EHA_VFR,
                              house$o5_Vent_living_SUA_AT*house$o5_Vent_living_SUA_VFR-(house$o5_living_AT+house$o5_bath_AT)*house$o5_Vent_bath_EHA_VFR
                    ))
                 )

df_weather <- data.frame(
                 "time"=weather$DATE,
                 "te"=weather$AmbientAirTemperature,
                 "GHI"=weather$Radiation_Global,
                 "BHI"=weather$Radiation_Global-weather$Radiation_Diffuse,
                 "sunAz"=weather$sunAz,
                 "sunEl"=weather$sunEl,
                 "humidity"=weather$RelativeHumidity,
                 "windSpeed"=weather$WindSpeed,
                 "windBearing"=weather$WindDirection
)

df <- merge(df_house,df_weather)

ggplot(reshape2::melt(df,"time")) + geom_line(aes(time,value)) + facet_wrap(~variable,ncol=1,scales="free_y")


######################################################### TRAINING #########################################################


# Training and validation datasets
all_dates <- sort(unique(as.Date(df$time,"Europe/Madrid")))
eligible_dates <- all_dates[2:length(all_dates)]
# train_dates <- sample(eligible_dates,size = length(eligible_dates)*0.9,replace = F)
# val_dates <- eligible_dates[!(eligible_dates %in% train_dates)]

train_dates <- eligible_dates[1:floor(length(eligible_dates)*0.75)]
val_dates <- eligible_dates[!(eligible_dates %in% train_dates)]

# Optimize the alpha values of the low pass filters
features <- list("alpha_te"=list(min=0,max=0.9,n=31,class="float"),
                 "alpha_BHI"=list(min=0,max=0,n=0,class="float"),
                 "alpha_GHI"=list(min=0,max=0,n=0,class="float"),
                 "alpha_ws"=list(min=0,max=0.9,n=15,class="float"),
                 "mod_hp_cons_ar"=list(min=1,max=6,n=5,class="int"),
                 "mod_hp_cons_lags_tsupply"=list(min=1,max=1,n=0,class="int"),
                 "mod_hp_cons_lags_te"=list(min=0,max=0,n=0,class="int"),
                 "mod_hp_cons_lags_humidity"=list(min=0,max=0,n=0,class="int"),
                 "mod_tsupply_ar"=list(min=1,max=5,n=4,class="int"),
                 "mod_tsupply_lags_hp_cons"=list(min=0,max=3,n=3,class="int"),
                 "mod_tsupply_lags_dti"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_ar"=list(min=1,max=5,n=4,class="int"),
                 "mod_ti_lags_te"=list(min=0,max=3,n=3,class="int"),
                 "mod_ti_lags_dti"=list(min=0,max=6,n=6,class="int"),
                 "mod_ti_lags_GHI"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_lags_BHI"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_infiltrations"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_lags_humidity"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_ventilation"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_hg"=list(min=0,max=5,n=5,class="int"),
                 "mod_ti_solar_gains"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_infiltrations"=list(min=0,max=2,n=2,class="int"),
                 "sunAzimuth_nharmonics"=list(min=2,max=5,n=3,class="int"),
                 "windBearing_nharmonics"=list(min=2,max=5,n=3,class="int")
                 )

optimization_results <- suppressMessages(
  ga(
    type = "binary",
    fitness = optimizer_model_parameters,
    nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){i[['n']]},features))),
    min_per_feature = mapply(function(i){i[['min']]},features),
    max_per_feature = mapply(function(i){i[['max']]},features),
    nclasses_per_feature = mapply(function(i){i[['n']]},features),
    class_per_feature = mapply(function(i){i[['class']]},features),
    names_per_feature = names(features),
    selection = gabin_tourSelection,
    df = df,
    train_dates = train_dates,
    val_dates = val_dates,
    popSize = 32,
    maxiter = 10,
    monitor = gaMonitor2,
    parallel = 16,
    elitism = 0.08,
    pmutation = 0.05)
)

params <- decodeValueFromBin(binary_representation = optimization_results@solution[1,],
                             class_per_feature = mapply(function(i){i[['class']]},features), 
                             nclasses_per_feature = mapply(function(i){i[['n']]},features),
                             min_per_feature = mapply(function(i){i[['min']]},features),
                             max_per_feature = mapply(function(i){i[['max']]},features))
names(params) <- names(features)
# print(params)

# Calculate the models with consumption as output (result_q) and indoor temperature as output (result_ti).
result_q <- calculate_model_q(params, df, train_dates, output="model")
result_ti <- calculate_model_ti(params, df, train_dates, output="model")
result_tsupply <- calculate_model_tsupply(params, df, train_dates, output="model")
mod_q <- result_q$mod
mod_ti <- result_ti$mod
mod_tsupply <- result_tsupply$mod
df_mod <- result_ti$df
cpgram(mod_q$model$hp_cons_l0-mod_q$fitted.values)
cpgram(mod_ti$model$ti_l0-mod_ti$fitted.values)
cpgram(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values)
# summary(mod_tsupply)
# summary(mod_q)
# summary(mod_ti)

# Validation of the models in 24h predictions
predv <- prediction_scenario(
  mod_q = mod_q, 
  mod_ti = mod_ti,
  mod_tsupply = mod_tsupply,
  df = df,
  rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates,
  hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
  params = params,
  ts_prediction = NULL
)

grid.arrange(
  ggplot(predv)+
    geom_line(aes(time,tsupply))+
    geom_line(aes(time,tsupply_l0),col=2)+
    geom_point(aes(time,ifelse(hp_status_l0>0,hp_tset_l0,NA)),col=3)+
    ylab("Supply temperature [ºC]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
    theme_bw(),
  ggplot(predv)+
    geom_line(aes(time,ti))+
    geom_line(aes(time,ti_l0),col=2)+
    ylab("Indoor temperature [ºC]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
    theme_bw(),
  ggplot(predv)+
    geom_line(aes(time,hp_cons))+
    geom_line(aes(time,hp_cons_l0),col=2)+
    ylab("HP electricity [Wh]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
    theme_bw(), 
  ncol=1
)

######################################################### MPC #########################################################


# The output should be the prediction of the optimum heat pump's status (ON/OFF) and set point temperature 
# for the next 24 hours (both parameters are saved in the vector: params_hp_tset_24h). 

# Import the "price" dataset
df_price <- read_excel_allsheets("data/electricity_price.xlsx")$Sheet1
colnames(df_price) <- c("daytime", "price")
df_price$daytime <- c(0:23)

# Initialize range for tset  
min_hp_tset <- min(df$hp_tset) 
max_hp_tset <- max(df$hp_tset) -3

# Initialize the limits in which the vector params_hp_tset_24h will live
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

# Initialize the time to predict, for example: (took a date inside val_dates: (2019-01-23 : 2019-02-01))
time_to_predict <- as.POSIXct(x = "2019-01-29 23:00:00 UTC", tz = "UTC")

# Constrains:
# max and min range (comfort bands) for ti defined based on the time of the day
# Thermal comfort band: 20-24 during the day (7:00 to 23:00)
#                       18-22 during the night (23:00 to 7:00)

ti_min = c(rep(x = 18, times = 7), rep(x = 20, times = 24-7)) 
ti_max = c(rep(x = 22, times = 7), rep(x = 24, times = 24-7)) 

# Using min temperature as starting point
params_hp_tset_24h = rep(x = min_hp_tset, times = 24)

# Using the measured set temperature as starting point
# params_hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset)
# rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% as.Date(time_to_predict)
# params_hp_tset_24h = params_hp_tset_24h[rows_to_filter]  
# params_hp_tset_24h = as.character(params_hp_tset_24h)
# params_hp_tset_24h[is.na(params_hp_tset_24h)] <- "NA"

# params_hp_tset_24h = rep(x = "NA", times = 12)
# rep(x = min_hp_tset, times = 12)
# 
# params_hp_tset_24h = character(24)
# mean_hour_price = floor(mean(as.numeric(c(floor(mean(which(df_price$price == max(df_price$price)))), floor(mean(which(df_price$price == min(df_price$price)))) ))))
# 
# params_hp_tset_24h[mean_hour_price:24] = "NA"
# params_hp_tset_24h[1:mean_hour_price-1] = floor(mean(c(min_hp_tset, max_hp_tset)))

suggestions = decodeBinFromValue(values = params_hp_tset_24h, class_per_feature = mapply(function(i){i[['class']]},features), 
                                 nclasses_per_feature =  mapply(function(i){length(i[["levels"]])},features), 
                                 levels_per_feature = lapply(function(i){i[["levels"]]}, X = features))

optimization_results_MPC <- suppressMessages(
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
    mod_tsupply = mod_tsupply,
    ti_min = ti_min,
    ti_max = ti_max,
    df_price = df_price,  
    selection = gabin_tourSelection,
    df = df,
    suggestions = suggestions,
    keepBest = TRUE,
    popSize = 64,
    maxiter = 20,
    monitor = gaMonitor2,
    parallel = 16,
    elitism = 0.08,
    pmutation = 0.05
  )
)

params_hp_tset_24h_opt <- as.numeric(decodeValueFromBin(binary_representation = optimization_results_MPC@solution[1,],
                                                        class_per_feature = mapply(function(i){i[['class']]},features),
                                                        nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
                                                        levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
))

# Comparison of the curves of the optimizer with the validation results 

hp_tset_24h <- numeric(length = nrow(df))
rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% as.Date(time_to_predict)
hp_tset_24h[rows_to_filter] = params_hp_tset_24h_opt

predv <- prediction_scenario(
  mod_q = mod_q, 
  mod_ti = mod_ti,
  mod_tsupply = mod_tsupply,
  df = df,
  rows_to_filter = rows_to_filter,
  hp_tset_24h = hp_tset_24h,
  params = params,
  ts_prediction = NULL
)

# Comparison of results:
optimized_price = sum(df_price$price * predv$hp_cons_l0)
predv$hourly_optimized_price = df_price$price * predv$hp_cons_l0
old_price = sum(df_price$price * df[rows_to_filter, "hp_cons"])
predv$hourly_old_price = df_price$price * df[rows_to_filter, "hp_cons"]

# diff = old_price - optimized_price
# # Savings respect to the old_price?
# saving = diff/old_price


p <- grid.arrange(
  ggplot(predv)+
    geom_line(aes(time,tsupply))+
    geom_line(aes(time,tsupply_l0),col=2)+
    geom_point(aes(time,ifelse(hp_status_l0>0,hp_tset_l0,NA)),col=3)+
    ylab("Supply temperature [ºC]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
    theme_bw(),
  ggplot(predv)+
    geom_line(aes(time,ti))+
    geom_line(aes(time,ti_l0),col=2)+
    ylab("Indoor temperature [ºC]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
    theme_bw(),
  ggplot(predv)+
    geom_line(aes(time,hp_cons))+
    geom_line(aes(time,hp_cons_l0),col=2)+
    ylab("HP electricity [Wh]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
    theme_bw(), 
  ncol=1,
  top = paste0("Savings = ",round(saving, digits = 2)*100,"%")
)

ggsave(plot = p, filename = paste0("MPC_savings_",unique(date(predv$ts_prediction)),".pdf"))

####################################################### testing MPC in several cases ######################

# Import the "price" dataset
df_price <- read_excel_allsheets("data/electricity_price.xlsx")$Sheet1
colnames(df_price) <- c("daytime", "price")
df_price$daytime <- c(0:23)

# Initialize range for tset  
min_hp_tset <- min(df$hp_tset) 
max_hp_tset <- max(df$hp_tset) -3

# Initialize the limits in which the vector params_hp_tset_24h will live
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

ti_min = c(rep(x = 18, times = 7), rep(x = 20, times = 24-7)) 
ti_max = c(rep(x = 22, times = 7), rep(x = 24, times = 24-7)) 

# Using min temperature as starting point
params_hp_tset_24h = rep(x = min_hp_tset, times = 24)


for (date in as.character(val_dates)) {
 
  # Initialize the time to predict, for example: (took a date inside val_dates: (2019-01-23 : 2019-02-01))
  time_to_predict <- as.POSIXct(x = paste0(date," 23:00:00 UTC"), tz = "UTC")

  suggestions = decodeBinFromValue(values = params_hp_tset_24h, class_per_feature = mapply(function(i){i[['class']]},features), 
                                   nclasses_per_feature =  mapply(function(i){length(i[["levels"]])},features), 
                                   levels_per_feature = lapply(function(i){i[["levels"]]}, X = features))

  optimization_results_MPC <- suppressMessages(
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
      mod_tsupply = mod_tsupply,
      ti_min = ti_min,
      ti_max = ti_max,
      df_price = df_price,  
      selection = gabin_tourSelection,
      df = df,
      suggestions = suggestions,
      keepBest = TRUE,
      popSize = 64,
      maxiter = 20,
      monitor = gaMonitor2,
      parallel = 16,
      elitism = 0.08,
      pmutation = 0.05
    )
  )
  
  params_hp_tset_24h_opt <- as.numeric(decodeValueFromBin(binary_representation = optimization_results_MPC@solution[1,],
                                                          class_per_feature = mapply(function(i){i[['class']]},features),
                                                          nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
                                                          levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
  ))

  # Comparison of the curves of the optimizer with the validation results 
  
  hp_tset_24h <- numeric(length = nrow(df))
  rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% as.Date(time_to_predict)
  hp_tset_24h[rows_to_filter] = params_hp_tset_24h_opt
  
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    df = df,
    rows_to_filter = rows_to_filter,
    hp_tset_24h = hp_tset_24h,
    params = params,
    ts_prediction = NULL
  )
  
  # Comparison of results:
  optimized_price = sum(df_price$price * predv$hp_cons_l0)
  predv$hourly_optimized_price = df_price$price * predv$hp_cons_l0
  old_price = sum(df_price$price * df[rows_to_filter, "hp_cons"])
  predv$hourly_old_price = df_price$price * df[rows_to_filter, "hp_cons"]
  
  diff = old_price - optimized_price
  # Savings respect to the old_price?
  saving = diff/old_price

  p <- grid.arrange(
    ggplot(predv)+
      geom_line(aes(time,tsupply))+
      geom_line(aes(time,tsupply_l0),col=2)+
      geom_point(aes(time,ifelse(hp_status_l0>0,hp_tset_l0,NA)),col=3)+
      ylab("Supply temperature [ºC]")+
      facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
      scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
      theme_bw(),
    ggplot(predv)+
      geom_line(aes(time,ti))+
      geom_line(aes(time,ti_l0),col=2)+
      ylab("Indoor temperature [ºC]")+
      facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
      scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
      theme_bw(),
    ggplot(predv)+
      geom_line(aes(time,hp_cons))+
      geom_line(aes(time,hp_cons_l0),col=2)+
      ylab("HP electricity [Wh]")+
      facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
      scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
      theme_bw(), 
    ncol=1,
    top = paste0("Savings = ",round(saving, digits = 2)*100,"%")
  )
  
  # ggsave(plot = p, filename = paste0("MPC_savings_",date,".pdf"))
  
}



