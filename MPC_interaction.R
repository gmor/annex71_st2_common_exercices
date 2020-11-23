# �IMPORTANT TODO! CHANGE 24HS TO 12HS?

# pulse secure:
# r0753014
# MPCsubex1

setwd("~/annex71_st2_common_exercices")
source("functions.R")

# Libraries for mathematics
library(expm)
library(splines)
library(mgcv)

# Libraries for graphics
library(cowplot)
library(ggplot2)
library(gridExtra)
library(extrafont)
library(reshape2)
loadfonts()

# Libraries for data wrangling
library(padr)
library(lubridate)
library(parallel)
library(oce)
library(GA)
library(data.table)
library(readxl) 
library(doParallel)

# library to integrate Python
library(reticulate)

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

# ggplot(reshape2::melt(df,"time")) + geom_line(aes(time,value)) + facet_wrap(~variable,ncol=1,scales="free_y")


######################################################### TRAINING MODEL #########################################################


# Training and validation datasets
all_dates <- sort(unique(as.Date(df$time,"Europe/Madrid")))
eligible_dates <- all_dates[2:length(all_dates)]
# train_dates <- sample(eligible_dates,size = length(eligible_dates)*0.9,replace = F)
# val_dates <- eligible_dates[!(eligible_dates %in% train_dates)]

# train_dates <- eligible_dates[1:floor(length(eligible_dates)*0.75)]
# val_dates <- eligible_dates[!(eligible_dates %in% train_dates)]

train_dates <- eligible_dates[eligible_dates>as.POSIXct(x = "2019-01-02 23:00:00 UTC", tz = "UTC")] #Excluiding the test dates
val_dates <- eligible_dates[!(eligible_dates %in% train_dates)] #Same as testing dates


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
                 "mod_ti_lags_dti"=list(min=2,max=9,n=7,class="int"),
                 "mod_ti_lags_GHI"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_BHI"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_lags_infiltrations"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_lags_humidity"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_ventilation"=list(min=0,max=1,n=1,class="int"),
                 "mod_ti_lags_hg"=list(min=0,max=7,n=7,class="int"),
                 "mod_ti_solar_gains"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_infiltrations"=list(min=0,max=2,n=2,class="int"),
                 "sunAzimuth_nharmonics"=list(min=2,max=5,n=3,class="int"),
                 "windBearing_nharmonics"=list(min=2,max=5,n=3,class="int")
)

# nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){i[['n']]},features)))
# min_per_feature = mapply(function(i){i[['min']]},features)
# max_per_feature = mapply(function(i){i[['max']]},features)
# nclasses_per_feature = mapply(function(i){i[['n']]},features)
# class_per_feature = mapply(function(i){i[['class']]},features)
# names_per_feature = names(features)

optimization_results <- suppressMessages(
  ga(
    type = "binary",##MODEL
    fitness = optimizer_model_parameters,##MODEL
    nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){i[['n']]},features))),##MODEL
    min_per_feature = mapply(function(i){i[['min']]},features),##DATA TO RUN
    max_per_feature = mapply(function(i){i[['max']]},features),##DATA TO RUN
    nclasses_per_feature = mapply(function(i){i[['n']]},features),##DATA TO RUN
    class_per_feature = mapply(function(i){i[['class']]},features),##DATA TO RUN
    names_per_feature = names(features),##DATA TO RUN
    selection = gabin_tourSelection,##MODEL
    df = df,##DATA TO RUN
    train_dates = train_dates,##DATA TO RUN
    val_dates = val_dates,##DATA TO RUN
    popSize = 32,#32 ##MODEL
    maxiter = 15,#10 ##MODEL
    monitor = gaMonitor2,##MODEL
    parallel = "snow", #change for windows  ##MODEL
    elitism = 0.08,#0.08 ##MODEL
    pmutation = 0.1)#0.05 ##MODEL
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
acf(mod_q$model$hp_cons_l0-mod_q$fitted.values)
acf(mod_ti$model$ti_l0-mod_ti$fitted.values)
acf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$tsupply_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$te_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$hg_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$BHI_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$vent_l0)

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

######################################################### SIMULATION INICIALIZATION #########################################################

setwd("~/annex71_st2_common_exercices")

# The output should be the prediction of the optimum heat pump's status (ON/OFF) and set point temperature 
# for the next 24 hours (both parameters are saved in the vector: params_hp_tset_24h). 

# Import the "price" dataset
df_price <- read_excel_allsheets("data/electricity_price.xlsx")$Sheet1
colnames(df_price) <- c("daytime", "price")
df_price$daytime <- c(0:23)
price <- df_price$price

source("C:/Users/gerar/Desktop/Config entrenament/df_866-df2900/functions.R")

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
                 "11"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete")#,
                 #"12"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "13"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "14"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "15"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "16"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "17"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "18"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "19"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "20"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "21"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "22"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete"),
                 # "23"=list(levels = c(as.character(seq(from = min_hp_tset, to = max_hp_tset, by = 1)), "NA"), class = "discrete")
                )


# Using min temperature as starting point
# params_hp_tset_24h = rep(x = min_hp_tset, times = 24)
params_hp_tset_24h = rep(x = min_hp_tset, times = 12)

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



# Initialize the time to predict, for example: (took a date inside val_dates: (2019-01-23 : 2019-02-01))
time_to_predict <- as.POSIXct(x = "2018-12-19 19:00:00 UTC", tz = "UTC") #Time the MPC starts
time_to_predict_step <- as.POSIXct(x = "2018-12-19 14:00:00 UTC", tz = "UTC") #Start time of Modelica
delay <- as.numeric(difftime(time_to_predict, time_to_predict_step)) #Hours thet Modelica will run without MPC

#To use Python environment
use_condaenv()

py<-import("os")
py<-import("fmpy")
py<-import("sys")

source_python("C:/Users/gerar/PycharmProjects/untitled1/venv/Scripts/Run_Modelica.py")

#Load Modelica model
simulation = simulate_with_input()

fmu <- simulation[[1]]
vr_input1 <- simulation[[2]]
vr_input2 <- simulation[[3]]
vr_output1 <- simulation[[4]]
vr_output2 <- simulation[[5]]

#Set time to run Modelica (the same as the Python code)
time <- 30463200
step_size <- 3600
Tsup <- 21.0
HPs <- 0.0
Tcost <- 0

# Constrains:
# max and min range (comfort bands) for ti defined based on the time of the day
# Thermal comfort band: 20-24 during the day (7:00 to 23:00)
#                       18-22 during the night (23:00 to 7:00)
ti_min = c(rep(x = 18, times = 7), rep(x = 20, times = 24-7)) 
ti_max = c(rep(x = 22, times = 7), rep(x = 24, times = 24-7)) 

#To take the forward X hours temperature boundaries/prices are needed 2 days
ti_min_2days = c(ti_min, ti_min)
ti_max_2days = c(ti_max, ti_max)
price_2days = c(price, price)

df_result <- data.frame("time_to_predict_step" = {},
                        "time" = {},
                        "Tsup_optim" = {},
                        "HPs_optim" = {}, 
                        "Temp_aver" = {}, 
                        "hp_el" = {},
                        "cost" = {},
                        "Tcost" = {},
                        "price_position (h+1)" = {},
                        "price" = {},
                        "ti_min" = {},
                        "ti_max" = {},
                        "MPC_fitnessValue" = {}
                        ) 

######################################################### LOOP Before MPC #########################################################

#Loop without using any MPC to run the iterations without lags

for (i in 1:delay) {
  
  Tsup <- df$tsupply[df$time==time_to_predict_step]
  HPs <- df$hp_status[df$time==time_to_predict_step]
  
  step <- Iteration(fmu, vr_input1, vr_input2, vr_output1, vr_output2, time, step_size, Tsup, HPs)
  # Iteration return -> fmu, input1, input2, output1(Tav), output2(hp_el)
  
  #Extract data from the step simulation
  input1 <- step[[2]]
  input2 <- step[[3]]
  output1 <- step[[4]]
  output2 <- step[[5]]
  cost <- df_price$price[hour(time_to_predict_step)+1]/1000000*output2
  Tcost = Tcost + cost
  print(paste(i, output1, output2, Tcost, time))
  
  df_result <- rbind(df_result, data.frame("time_to_predict_step" = time_to_predict_step,
                                           "time" = time,
                                           "Tsup_optim" = Tsup,
                                           "HPs_optim" = HPs, 
                                           "Temp_aver" = output1, 
                                           "hp_el" = output2,
                                           "cost" = cost,
                                           "Tcost" = Tcost,
                                           "price_position (h+1)" = hour(time_to_predict_step)+1,
                                           "price" = df_price$price[(hour(time_to_predict_step)+1)],
                                           "ti_min" = ti_min[(hour(time_to_predict_step)+1)],
                                           "ti_max" = ti_max[(hour(time_to_predict_step)+1)],
                                           "MPC_fitnessValue" = 0
                                            )
                    )
  
  #Energy consumed in time to predict to have simulated temperature at time to predict +1
  df$hp_cons[df$time==time_to_predict_step] = step[[5]]
  #Inside temperature at time to predict +1 due to the effect of use the energy between time to predict and time to predict +1
  df$ti[df$time == time_to_predict_step + hours(1)] = step[[4]]
  
  #Update time for next step
  time_to_predict_step <- as.POSIXct(x = time_to_predict_step + hours(1), format = '%Y-%m-%d %H:%M:%S')
  time = time + step_size
}

print(df_result)

df_sol_time <- data.frame("time_to_predict_step" = {})

df_sol_Tset <- data.frame("Tset[1]" = {},
                          "Tset[2]" = {},
                          "Tset[3]" = {},
                          "Tset[4]" = {},
                          "Tset[5]" = {},
                          "Tset[6]" = {},
                          "Tset[7]" = {},
                          "Tset[8]" = {},
                          "Tset[9]" = {},
                          "Tset[10]" = {},
                          "Tset[11]" = {},
                          "Tset[12]" = {}
)

df_sol_Tave <- data.frame("Tave[1]" = {},
                          "Tave[2]" = {},
                          "Tave[3]" = {},
                          "Tave[4]" = {},
                          "Tave[5]" = {},
                          "Tave[6]" = {},
                          "Tave[7]" = {},
                          "Tave[8]" = {},
                          "Tave[9]" = {},
                          "Tave[10]" = {},
                          "Tave[11]" = {},
                          "Tave[12]" = {}
)

df_sol_Text <- data.frame("Text[1]" = {},
                          "Text[2]" = {},
                          "Text[3]" = {},
                          "Text[4]" = {},
                          "Text[5]" = {},
                          "Text[6]" = {},
                          "Text[7]" = {},
                          "Text[8]" = {},
                          "Text[9]" = {},
                          "Text[10]" = {},
                          "Text[11]" = {},
                          "Text[12]" = {}
)

df_costs <- data.frame("penalty" = {},
                       "iteration_price" = {}
)

######################################################### MPC LOOP #########################################################
max_time = 31676400+10 #Modelica max time
# n_steps =1
# max_time = time + step_size*(n_steps-1) + 10

while (time < max_time) {
  
  ti_min = ti_min_2days[(hour(time_to_predict_step)+1):(hour(time_to_predict_step)+12)]
  ti_max = ti_max_2days[(hour(time_to_predict_step)+1):(hour(time_to_predict_step)+12)]
  price = price_2days[(hour(time_to_predict_step)+1):(hour(time_to_predict_step)+12)]
  
  # nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features)))
  # class_per_feature = mapply(function(i){i[['class']]},features)
  # nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features)
  # levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
  # names_per_feature = names(features)
  # time_to_predict = time_to_predict_step
  
  optimization_results_MPC <- suppressMessages(
    ga(
      type = "binary",
      fitness = optimizer_MPC,
      nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features))),
      class_per_feature = mapply(function(i){i[['class']]},features),
      nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
      levels_per_feature = lapply(function(i){i[["levels"]]}, X = features),
      names_per_feature = names(features),
      time_to_predict = time_to_predict_step,
      params = params,
      mod_q = mod_q,
      mod_ti = mod_ti,
      mod_tsupply = mod_tsupply,
      ti_min = ti_min,
      ti_max = ti_max,
      price = price,
      selection = gabin_tourSelection,
      df = df,
      suggestions = suggestions,
      keepBest = TRUE,
      popSize = 100, #64
      maxiter = 20, #20
      monitor = gaMonitor2,
      parallel = "snow", #16
      elitism = 0.10, #0.08
      pmutation = 0.15, #0.05
      maxFitness = 0
    )
  )
  
  params_hp_tset_24h_opt <- as.numeric(decodeValueFromBin(binary_representation = optimization_results_MPC@solution[1,],
                                                          class_per_feature = mapply(function(i){i[['class']]},features),
                                                          nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
                                                          levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
  ))
  print(params_hp_tset_24h_opt)
  

  params_hp_tset_sugg = c(params_hp_tset_24h_opt[-1], NA)
  for (i in 1:length(params_hp_tset_sugg)) {
    if (is.na(params_hp_tset_sugg[i])) {
      params_hp_tset_sugg[i] <- min_hp_tset
    }
  }
  suggestions = decodeBinFromValue(values = params_hp_tset_sugg, class_per_feature = mapply(function(i){i[['class']]},features),
                                   nclasses_per_feature =  mapply(function(i){length(i[["levels"]])},features),
                                   levels_per_feature = lapply(function(i){i[["levels"]]}, X = features))

  RETURN <- optimizer_MPC(X = optimization_results_MPC@solution[1,],
                             class_per_feature = mapply(function(i){i[['class']]},features),
                             nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
                             names_per_feature = names(features),
                             levels_per_feature = lapply(function(i){i[["levels"]]}, X = features),
                             df = df,
                             mod_q = mod_q,
                             mod_ti = mod_ti,
                             mod_tsupply = mod_tsupply,
                             ti_min = ti_min,
                             ti_max = ti_max,
                             price = price,
                             time_to_predict = time_to_predict_step,
                             params = params,
                             post_analysis=T)
  
  penalty <- RETURN$penalty
  iteration_price <- RETURN$iteration_price
  
  
  df_sol_time <- rbind(df_sol_time, data.frame("time_to_predict_step" = time_to_predict_step))
  
  df_sol_Tset <- rbind(df_sol_Tset, data.frame("Tset[1]" = params_hp_tset_24h_opt[1],
                                               "Tset[2]" = params_hp_tset_24h_opt[2],
                                               "Tset[3]" = params_hp_tset_24h_opt[3],
                                               "Tset[4]" = params_hp_tset_24h_opt[4],
                                               "Tset[5]" = params_hp_tset_24h_opt[5],
                                               "Tset[6]" = params_hp_tset_24h_opt[6],
                                               "Tset[7]" = params_hp_tset_24h_opt[7],
                                               "Tset[8]" = params_hp_tset_24h_opt[8],
                                               "Tset[9]" = params_hp_tset_24h_opt[9],
                                               "Tset[10]" = params_hp_tset_24h_opt[10],
                                               "Tset[11]" = params_hp_tset_24h_opt[11],
                                               "Tset[12]" = params_hp_tset_24h_opt[12]
                                                )
                        )
  
  df_sol_Tave <- rbind(df_sol_Tave, data.frame("Tave[1]" = RETURN$predv.ti_l0[1],
                                               "Tave[2]" = RETURN$predv.ti_l0[2],
                                               "Tave[3]" = RETURN$predv.ti_l0[3],
                                               "Tave[4]" = RETURN$predv.ti_l0[4],
                                               "Tave[5]" = RETURN$predv.ti_l0[5],
                                               "Tave[6]" = RETURN$predv.ti_l0[6],
                                               "Tave[7]" = RETURN$predv.ti_l0[7],
                                               "Tave[8]" = RETURN$predv.ti_l0[8],
                                               "Tave[9]" = RETURN$predv.ti_l0[9],
                                               "Tave[10]" = RETURN$predv.ti_l0[10],
                                               "Tave[11]" = RETURN$predv.ti_l0[11],
                                               "Tave[12]" = RETURN$predv.ti_l0[12]
                                                )
                        )
  
  df_sol_Text <- rbind(df_sol_Text,  data.frame("Text[1]" = RETURN$predv.te_l0[1],
                                                "Text[2]" = RETURN$predv.te_l0[2],
                                                "Text[3]" = RETURN$predv.te_l0[3],
                                                "Text[4]" = RETURN$predv.te_l0[4],
                                                "Text[5]" = RETURN$predv.te_l0[5],
                                                "Text[6]" = RETURN$predv.te_l0[6],
                                                "Text[7]" = RETURN$predv.te_l0[7],
                                                "Text[8]" = RETURN$predv.te_l0[8],
                                                "Text[9]" = RETURN$predv.te_l0[9],
                                                "Text[10]" = RETURN$predv.te_l0[10],
                                                "Text[11]" = RETURN$predv.te_l0[11],
                                                "Text[12]" = RETURN$predv.te_l0[12]
                                                )
                        )
  df_costs <- rbind(df_costs, data.frame("penalty" = penalty[1],
                                            "iteration_price" = iteration_price[1]
                                            )
                      )
  
  # print(df_sol_Text)
  # print(df_sol_Tave)

  Tsup_optim <- params_hp_tset_24h_opt[1]
  HPs_optim = 1.0
  if (is.na(Tsup_optim)) {
    Tsup_optim = 0.0
    HPs_optim = 0.0
  }
  
  step <- Iteration(fmu, vr_input1, vr_input2, vr_output1, vr_output2, time, step_size, Tsup_optim, HPs_optim)
  input1 <- step[[2]]
  input2 <- step[[3]]
  output1 <- step[[4]]
  output2 <- step[[5]]
  cost <- df_price$price[hour(time_to_predict_step)+1]/1000000*output2
  Tcost = Tcost + cost
  print(paste(time_to_predict_step, output1, output2, Tcost, time))
  
  df_result <- rbind(df_result, data.frame("time_to_predict_step" = time_to_predict_step,
                                           "time" = time,
                                           "Tsup_optim" = Tsup_optim,
                                           "HPs_optim" = HPs_optim, 
                                           "Temp_aver" = output1, 
                                           "hp_el" = output2,
                                           "cost" = cost,
                                           "Tcost" = Tcost,
                                           "price_position (h+1)" = hour(time_to_predict_step)+1,
                                           "price" = df_price$price[(hour(time_to_predict_step)+1)],
                                           "ti_min" = ti_min_2days[(hour(time_to_predict_step)+1)],
                                           "ti_max" = ti_max_2days[(hour(time_to_predict_step)+1)],
                                           "MPC_fitnessValue" = optimization_results_MPC@fitnessValue
                                            ))
  
  
  #Energy consumed in time to predict to have simulated temperature at time to predict +1
  df$hp_cons[df$time==time_to_predict_step] = step[[5]]
  #Inside temperature at time to predict +1 due to the effect of use the energy between time to predict and time to predict +1
  df$ti[df$time == time_to_predict_step + hours(1)] = step[[4]]
  
  time = time + step_size
  time_to_predict_step <- time_to_predict_step + hours(1)

  # print(df_result)                   
  
}

Total_cost_BM <- df_result$Tcost[length(df_result$Tcost)]-df_result$Tcost[df_result$time_to_predict_step == as.POSIXct(x = "2018-12-19 18:00:00 UTC", tz = "UTC")]
Total_cost_BM

# END_fmu(fmu)

######################################################### PLOTS #########################################################

df_plot <- df_result[, c("time_to_predict_step", "Tsup_optim", "HPs_optim", "Temp_aver", "hp_el", "cost", "price")]
df_plot <- melt(df_plot, "time_to_predict_step") 
df_plot$variable <- as.factor(df_plot$variable)

ggplot(df_plot) + 
  geom_line(aes(x = time_to_predict_step, y = value)) +
  facet_wrap(~as.factor(variable), ncol = 1, scales = "free_y")

# df_result$hour = hour(df_result$time_to_predict_step)
# 
# df_ti_min = data.frame("hour" = c(0:23), 
#                        "ti_min" = c(rep(x = 18, times = 7), rep(x = 20, times = 24-7)))
# df_result = merge(x = df_result, y = df_ti_min, by = "hour")
# 
# df_ti_max = data.frame("hour" = c(0:23), 
#                        "ti_max" = c(rep(x = 22, times = 7), rep(x = 24, times = 24-7)))
# df_result = merge(x = df_result, y = df_ti_max, by = "hour")


ggplot() +
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$Temp_aver)) +
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$ti_min, color = "red")) + 
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$ti_max, color = "blue")) 


df_disconfort <- df_result[df_result$time_to_predict_step >= as.POSIXct(x = "2018-12-19 19:00:00 UTC", tz = "UTC"),]
# delta_limit <- 3
# lambda <- 2
disconfort <- {}
for (i in 1:length(df_disconfort$time_to_predict_step)) {
  if (df_disconfort$ti_min[i] > df_disconfort$Temp_aver[i]) {
    x <- (df_disconfort$ti_min[i]-df_disconfort$Temp_aver[i])
    disconfort[i] <- x #(-exp(lambda*x)/(x-delta_limit))-(1/delta_limit)
  } else {
    disconfort[i] <- 0
  }
}

max(disconfort)
sum(disconfort)
hist(disconfort)
disconfort <- disconfort[disconfort != 0]
hist(disconfort)

penalty <- zoo::rollapply(disconfort, width=12, align="left",partial=T,FUN=function(x){sum(x,na.rm=T)})

disconfort <- {}
delta_limit <- 3
lambda <- 2
x_disconfort <- {}

for (i in 1:length(df_disconfort$time_to_predict_step)) {
  if (df_disconfort$ti_min[i] > df_disconfort$Temp_aver[i]) {
    x <- (df_disconfort$ti_min[i]-df_disconfort$Temp_aver[i])
    # disconfort[i] <- (-exp(lambda*x)/(x-delta_limit))-(1/delta_limit)
    x_disconfort[i] <- x
  } else {
    # disconfort[i] <- 0
    x_disconfort[i] <- 0
  }
}
x_disconfort <- x_disconfort[x_disconfort != 0]
hist(x_disconfort)


