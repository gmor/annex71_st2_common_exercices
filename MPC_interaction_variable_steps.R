# ¿IMPORTANT TODO! CHANGE 24HS TO 12HS?

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
library(plotly)
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

#library ML
library(autoxgboost)

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

house2 <- read_excel_allsheets("data/validationdataset2_for_ARX.xlsx")$Sheet1

df_house2 <- data.frame("time"=house2$DATE,
                       "hp_cons"=house2$hp_el_cons,
                       "hp_status"=house2$hp_status,
                       "hp_tset"=house2$Tsupply,
                       "hp_cop"=house2$COP,
                       "tsupply"=house2$HeatPump_actual_Tsup,
                       "ti"=house2$Taver,
                       "hg"= house2$Internal_heat_gains,
                       "vent"= house2$total_VFR
                      )
df2 <- merge(df_house2,df_weather)


# ggplot(reshape2::melt(df,"time")) + geom_line(aes(time,value)) + facet_wrap(~variable,ncol=1,scales="free_y")


######################################################### TRAINING MODEL #########################################################


# Training and validation datasets
all_dates <- sort(unique(as.Date(df$time,"Europe/Madrid")))
eligible_dates <- all_dates[2:length(all_dates)]
# train_dates <- sample(eligible_dates,size = length(eligible_dates)*0.9,replace = F)
# val_dates <- eligible_dates[!(eligible_dates %in% train_dates)]

# train_dates <- eligible_dates[1:floor(length(eligible_dates)*0.75)]
# val_dates <- eligible_dates[!(eligible_dates %in% train_dates)]

#train_dates <- eligible_dates[eligible_dates>as.POSIXct(x = "2019-01-02 23:00:00 UTC", tz = "UTC")] #Excluiding the test dates
train_dates <- sample(eligible_dates,round(length(eligible_dates)*0.8,0),replace = F)
val_dates <- eligible_dates[!(eligible_dates %in% train_dates)] #Same as testing dates


# Optimize the alpha values of the low pass filters
features <- list("alpha_te"=list(min=0,max=0.9,n=31,class="float"),
                 "alpha_BHI"=list(min=0,max=0,n=0,class="float"),
                 "alpha_GHI"=list(min=0,max=0,n=0,class="float"),
                 "alpha_ws"=list(min=0,max=0.9,n=15,class="float"),
                 "mod_hp_cons_ar"=list(min=1,max=4,n=3,class="int"),
                 "mod_hp_cons_lags_tsupply"=list(min=0,max=3,n=3,class="int"),
                 "mod_hp_cons_lags_te"=list(min=0,max=3,n=3,class="int"),#2,2
                 "mod_hp_cons_lags_humidity"=list(min=0,max=0,n=0,class="int"),
                 "mod_hp_cons_lags_ti"=list(min=1,max=4,n=3,class="int"),
                 "mod_tsupply_ar"=list(min=1,max=5,n=4,class="int"),
                 "mod_tsupply_lags_hp_cons"=list(min=0,max=4,n=4,class="int"),#"mod_tsupply_lags_hp_cons"=list(min=0,max=3,n=3,class="int"),
                 "mod_tsupply_lags_dti"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_ar"=list(min=3,max=5,n=2,class="int"),#"mod_ti_ar"=list(min=1,max=9,n=8,class="int"), #5,3
                 "mod_ti_ar2"=list(min=1,max=2,n=1,class="int"),#
                 "mod_ti_lags_te"=list(min=2,max=2,n=0,class="int"),
                 "mod_ti_lags_diff"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_dti"=list(min=1,max=5,n=4,class="int"),#"mod_ti_lags_dti"=list(min=2,max=9,n=7,class="int"),
                 "mod_ti_lags_GHI"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_BHI"=list(min=1,max=3,n=2,class="int"),
                 "mod_ti_lags_infiltrations"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_lags_humidity"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_ventilation"=list(min=0,max=1,n=1,class="int"),
                 "mod_ti_lags_hg"=list(min=0,max=5,n=5,class="int"),#"mod_ti_lags_hg"=list(min=0,max=7,n=7,class="int"),
                 "mod_ti_solar_gains"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_infiltrations"=list(min=0,max=0,n=0,class="int"),
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
    popSize = 64,#32 ##MODEL
    maxiter = 15,#10 ##MODEL
    monitor = gaMonitor2,##MODEL
    parallel = "snow", #change for windows  ##MODEL
    elitism = 0.08,#0.08 ##MODEL
    pmutation = 0.05)#0.05 ##MODEL
)

params <- decodeValueFromBin(binary_representation = optimization_results@solution[1,],
                             class_per_feature = mapply(function(i){i[['class']]},features), 
                             nclasses_per_feature = mapply(function(i){i[['n']]},features),
                             min_per_feature = mapply(function(i){i[['min']]},features),
                             max_per_feature = mapply(function(i){i[['max']]},features))
names(params) <- names(features)

# print(params)
# Calculate the models with consumption as output (result_q) and indoor temperature as output (result_ti).
result_q <- calculate_model_q(params, df, train_dates, output="model") #df
mod_q <- result_q$mod

result_ti <- calculate_model_ti(params, df, train_dates, output="model") #df
mod_ti <- result_ti$mod

result_tsupply <- calculate_model_tsupply(params, df, train_dates, output="model") #df
mod_tsupply <- result_tsupply$mod


#Test to simulate ti with ML
library(mlr)
library(mlrMBO)
library(autoxgboost)
library(zoo)
#dfr <- df[,!(colnames(df) %in% c("vent", "windSpeed", "windBearing", "humidity", "sunAz", "sunEl", "GHI"))]
ti_mod <- as.list(rollmean(df["ti"], 4, align = "center", fill = "extend"))
df_mod <- df#[,!(colnames(df) %in% "ti")]
df_mod["ti"] <- as.numeric(ti_mod)
result_ti <- calculate_model_ti_AutoXGboost(params, df_mod, train_dates, output="model") #df
mod_ti <- result_ti$mod


#Graphics to check if the ARX model fits
cpgram(mod_q$model$hp_cons_l0-mod_q$fitted.values)
cpgram(mod_ti$model$ti_l0-mod_ti$fitted.values)
cpgram(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values)

acf(mod_q$model$hp_cons_l0-mod_q$fitted.values)
acf(mod_ti$model$ti_l0-mod_ti$fitted.values)
acf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values)

summary(mod_q)
summary(mod_ti)
summary(mod_tsupply)

ccf(mod_q$model$hp_cons_l0-mod_q$fitted.values,mod_q$model$hp_cons_l0)
ccf(mod_q$model$hp_cons_l0-mod_q$fitted.values,mod_q$model$te_raw_l0)

ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$tsupply_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$te_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$hg_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$BHI_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$vent_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$ti_l0)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$`as.factor(hp_status_l1)`)
ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$infiltrations_l0)
# ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$)

ccf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,mod_tsupply$model$tsupply_l0)
ccf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,mod_tsupply$model$`as.factor(hp_status_l0)`)
ccf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,mod_tsupply$model$hp_cons_l0)
ccf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,mod_tsupply$model$te_raw_l0)

plot(model)

vec <- {}
for (i in 1:length(mod_ti$fitted.values)) {
  vec <- c(vec, mod_ti$fitted.values[[i]])
}

plot(1:length(mod_ti$fitted.values), mod_ti$model$ti_l0-vec)

plot(mod_ti$residuals)

# Validation of the models in 24h predictions
predv <- prediction_scenario(
  mod_q = mod_q, 
  mod_ti = mod_ti,
  mod_tsupply = mod_tsupply,
  df = df, #df
  rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates,#val_dates,
  hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
  params = params,
  ts_prediction = NULL
)

grid.arrange(
  ggplot(predv)+
    geom_line(aes(time,tsupply))+
    geom_line(aes(time,tsupply_l0),col=2)+
    geom_point(aes(time,ifelse(hp_status_l0>0,hp_tset_l0,NA)),col=3)+
    ylab("Supply temperature [ÂºC]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
    theme_bw(),
  ggplot(predv)+
    geom_line(aes(time,ti))+
    geom_line(aes(time,ti_l0),col=2)+
    ylab("Indoor temperature [ÂºC]")+
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

######################################################### Compute R2 of ti#########################################################

#### R2 of the first dataset
#Dates taken into account (validation dates)
forecast_dates_df <- df$time[df$time>=as.POSIXct(x = "2018-12-19 23:00:00 UTC", tz = "UTC") & df$time<=(val_dates[length(val_dates)] + days(1))] #& df$time<=(val_dates[length(val_dates)] + days(1))
train_dates_df <- df$time[df$time>=train_dates[1]]

# val_dates
# train_dates

#Forecast and storage of the results
ti_df <- {}
ti_l0_df <- {}
ii_df <- c()
for (i in 1:length(forecast_dates_df)) {
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    df = df,#df,
    rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% forecast_dates_df,
    hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    ts_prediction = forecast_dates_df[i]#NULL
  )
  
  ti_df <- c(ti_df, predv$ti) #vector of the 12h real values in each iteration
  ti_l0_df <- c(ti_l0_df, predv$ti_l0) #vector of the 12h forecasts in each iteration
  ii_df <- c(ii_df,1:nrow(predv))
}
#Compute R2
R2_df <- 1-sum((ti_df-ti_l0_df)^2)/sum((ti_df-mean(ti_df))^2)
R2_df

# plot(ti_df,ti_l0_df) + abline(a = 0, b = 1, col="red")
ggplot(data.frame(ti_df=ti_df,ti_l0_df=ti_l0_df,ii_df=ii_df))+geom_point(aes(ti_df,ti_l0_df)) + facet_wrap(~ii_df)

ggplot(data.frame(ti_df=ti_df[seq(1, length(ti_df), 12)], ti_l0_df=ti_l0_df[seq(1, length(ti_l0_df), 12)], ii_df=ii_df[seq(1, length(ii_df), 12)])) +
  geom_point(aes(ti_df,ti_l0_df))

R2_df_1 <- 1-sum((ti_df[seq(1, length(ti_df), 12)]-ti_l0_df[seq(1, length(ti_l0_df), 12)])^2)/sum((ti_df[seq(1, length(ti_l0_df), 12)]-mean(ti_df[seq(1, length(ti_l0_df), 12)]))^2)
R2_df_1
ti_df_1 <- ti_df[seq(1, length(ti_df), 12)]
ti_l0_df_1 <- ti_l0_df[seq(1, length(ti_df), 12)]
#Check l0 is the original data
plot(ti_l0_df_1, df$ti[df$time>=as.POSIXct(x = "2018-12-19 23:00:00 UTC", tz = "UTC") & df$time<=(val_dates[length(val_dates)] + days(1))])
max(ti_df_1 - df$ti[df$time>=as.POSIXct(x = "2018-12-19 23:00:00 UTC", tz = "UTC") & df$time<=(val_dates[length(val_dates)] + days(1))])

# plot(1:length(ti_df),ti_l0_df-ti_df)
# plot(ti_df,ti_l0_df-ti_df)


#### R2 of the second dataset
#Dates taken into account (all second dataset, except firts 10h)
forecast_dates_df2 <- df2$time[df2$time>=as.POSIXct(x = "2018-12-19 23:00:00 UTC", tz = "UTC")]

#Forecast and storage of the results
ti_df2 <- {}
ti_l0_df2 <- {}
ii_df2 <- c()
for (i in 1:length(forecast_dates_df2)) {
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    df = df2,#df,
    rows_to_filter = df$time %in% forecast_dates_df2,
    hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    ts_prediction = forecast_dates_df2[i]#NULL
  )
  
  ti_df2 <- c(ti_df2, predv$ti) #vector of the 12h real values in each iteration
  ti_l0_df2 <- c(ti_l0_df2, predv$ti_l0) #vector of the 12h forecasts in each iteration
  ii_df2 <- c(ii_df2,1:nrow(predv))
}
#Compute R2
R2_df2 <- 1-sum((ti_df2-ti_l0_df2)^2)/sum((ti_df2-mean(ti_df2))^2)
R2_df2

# plot(ti_df,ti_l0_df) + abline(a = 0, b = 1, col="red")
ggplot(data.frame(ti_df2=ti_df2,ti_l0_df2=ti_l0_df2,ii_df2=ii_df2))+geom_point(aes(ti_df2,ti_l0_df2)) + facet_wrap(~ii_df2)

ggplot(data.frame(ti_df2=ti_df2[seq(1, length(ti_df2), 12)], ti_l0_df2=ti_l0_df2[seq(1, length(ti_l0_df2), 12)], ii_df2=ii_df2[seq(1, length(ii_df2), 12)])) +
  geom_point(aes(ti_df2,ti_l0_df2))

R2_df2_1 <- 1-sum((ti_df2[seq(1, length(ti_df2), 12)]-ti_l0_df2[seq(1, length(ti_l0_df2), 12)])^2)/sum((ti_df2[seq(1, length(ti_l0_df2), 12)]-mean(ti_df2[seq(1, length(ti_l0_df2), 12)]))^2)
R2_df2_1
ti_df2_1 <- ti_df2[seq(1, length(ti_df2), 12)]
ti_l0_df2_1 <- ti_l0_df2[seq(1, length(ti_df2), 12)]
#Check l0 is the original data
plot(ti_df2_1[1:1000], df2$ti[df2$time>=as.POSIXct(x = "2018-12-19 23:00:00 UTC", tz = "UTC")][1:1000])
max(ti_df2_1[1:1000] - df2$ti[df2$time>=as.POSIXct(x = "2018-12-19 23:00:00 UTC", tz = "UTC")][1:1000])

# plot(ti_df2,ti_l0_df2) + abline(a = 0, b = 1, col="red")
ggplot(data.frame(ti_df2=ti_df2,ti_l0_df2=ti_l0_df2,ii_df2=ii_df2))+geom_point(aes(ti_df2,ti_l0_df2)) +
  facet_wrap(~ii_df2) + abline(a = 0, b = 1, col="red")
# 
# plot(1:length(ti_df2),abs(ti_l0_df2-ti_df2))

######################################################### SIMULATION INICIALIZATION #########################################################

setwd("~/annex71_st2_common_exercices")
# source("functions.R")

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

df_result_20s <- data.frame("time_to_predict_step" = {},
                            "time" = {},
                            "Tsup_optim" = {},
                            "HPs_optim" = {}, 
                            "Temp_aver" = {}, 
                            "hp_el" = {},
                            "cost" = {},
                            "ti_min" = {},
                            "ti_max" = {}
                            )

######################################################### LOOP Before MPC #########################################################

# Initialize the time to predict, for example: (took a date inside val_dates: (2019-01-23 : 2019-02-01))
time_to_predict <- as.POSIXct(x = "2018-12-19 19:00:00 UTC", tz = "UTC") #Time the MPC starts
time_to_predict_step <- as.POSIXct(x = "2018-12-19 14:00:00 UTC", tz = "UTC") #Start time of Modelica
delay <- as.numeric(difftime(time_to_predict, time_to_predict_step)) #Hours thet Modelica will run without MPC

#Set time to run Modelica (the same as the Python code)
time <- 30463200
step_size <- 20#3600
Tcost <- 0

#Set the first iteration valuies
Tsup <- 28
HPs <- 0
Taver <- 20

#Create the first iteration of results 
df_result <- rbind(df_result, data.frame("time_to_predict_step" = time_to_predict_step,
                                         "time" = time,
                                         "Tsup_optim" = Tsup,
                                         "HPs_optim" = HPs, 
                                         "Temp_aver" = Taver, 
                                         "hp_el" = 0,
                                         "cost" = 0,
                                         "Tcost" = Tcost,
                                         "price_position (h+1)" = hour(time_to_predict_step)+1,
                                         "price" = df_price$price[(hour(time_to_predict_step)+1)],
                                         "ti_min" = ti_min[(hour(time_to_predict_step)+1)],
                                         "ti_max" = ti_max[(hour(time_to_predict_step)+1)],
                                         "MPC_fitnessValue" = 0
                                          )
                    )

#Loop without using any MPC to run the iterations without lags
for (i in 1:(delay*3600/step_size)) { #1:(delay*3600/step_size)
  # print(time)
  #Save in the hourly data and update Tsupply and HP status 
  if ((time%%3600 == 0) & i!=1) {
    #Energy consumed in time to predict to have simulated temperature at time to predict +1
    df$hp_cons[df$time==time_to_predict_step - hours(1)] = sum(df_result_20s$hp_el[(i-(3600/step_size)):(i-1)])*(step_size/3600)
    #Inside temperature at time to predict +1 due to the effect of use the energy between time to predict and time to predict +1
    df$ti[df$time == time_to_predict_step] = sum(df_result_20s$Temp_aver[(i-(3600/step_size)):(i-1)])/(3600/step_size)
    
    Tsup <- df$hp_tset[df$time==time_to_predict_step]
    HPs <- df$hp_status[df$time==time_to_predict_step]
    
    df_result$hp_el[length(df_result$hp_el)] <- sum(df_result_20s$hp_el[(i-(3600/step_size)):(i-1)])*(step_size/3600)
    df_result$cost[length(df_result$cost)] <- sum(df_result_20s$cost[(i-(3600/step_size)):(i-1)])
    df_result$Tcost[length(df_result$Tcost)] <- Tcost
    
    df_result <- rbind(df_result, data.frame("time_to_predict_step" = time_to_predict_step,
                                             "time" = time,
                                             "Tsup_optim" = Tsup,
                                             "HPs_optim" = HPs, 
                                             "Temp_aver" = Taver, #sum(df_result_20s$Temp_aver[(i-(3600/step_size)):(i-1)])/(3600/step_size), 
                                             "hp_el" = 0, #sum(df_result_20s$hp_el[(i-(3600/step_size)):(i-1)])*(step_size/3600),
                                             "cost" = 0,
                                             "Tcost" = 0,
                                             "price_position (h+1)" = hour(time_to_predict_step)+1,
                                             "price" = df_price$price[(hour(time_to_predict_step)+1)],
                                             "ti_min" = ti_min[(hour(time_to_predict_step)+1)],
                                             "ti_max" = ti_max[(hour(time_to_predict_step)+1)],
                                             "MPC_fitnessValue" = 0))
  }
  
  #Modelica simulation
  step <- Iteration(fmu, vr_input1, vr_input2, vr_output1, vr_output2, time, step_size, Tsup, HPs)
  # Iteration return -> fmu, input1, input2, output1(Tav), output2(hp_el)
  
  #Extract data from the step simulation
  input1 <- step[[2]]
  input2 <- step[[3]]
  output1 <- step[[4]]
  output2 <- step[[5]]
  cost <- df_price$price[hour(time_to_predict_step)+1]/1000000*output2*(step_size/3600)
  Tcost = Tcost + cost
  # print(paste(i, output1, output2, Tcost, time))
  
  df_result_20s <- rbind(df_result_20s, data.frame("time_to_predict_step" = time_to_predict_step,
                                                   "time" = time,
                                                   "Tsup_optim" = Tsup,
                                                   "HPs_optim" = HPs, 
                                                   "Temp_aver" = Taver, #The internal temperature is the one from the previous calculation 
                                                   "hp_el" = output2,
                                                   "cost" = cost,
                                                   "ti_min" = ti_min[(hour(time_to_predict_step)+1)],
                                                   "ti_max" = ti_max[(hour(time_to_predict_step)+1)]))
  Taver <- output1 #the computed Taver is the internal temperature of the next iteration
  
  #Update time for next step
  time_to_predict_step <- as.POSIXct(x = time_to_predict_step + seconds(step_size), format = '%Y-%m-%d %H:%M:%S')
  time = time + step_size
}

# print(df_result)
#initialize fata frames to have a controll of what have it done the MPC
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
                          "Tset[12]" = {})

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
                          "Tave[12]" = {})

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
                          "Text[12]" = {})

df_costs <- data.frame("penalty" = {},
                       "iteration_price" = {})

######################################################### MPC LOOP #########################################################
max_time = 31676400 #Modelica max time
# n_steps =1
# max_time = time + step_size*(n_steps-1) + 10


while (time <= max_time) {
  
  #Save in the hourly data and update Tsupply and HP status with MPC
  if (time%%3600 == 0) {
    ti_min = ti_min_2days[(hour(time_to_predict_step)+1):(hour(time_to_predict_step)+12)]
    ti_max = ti_max_2days[(hour(time_to_predict_step)+1):(hour(time_to_predict_step)+12)]
    price = price_2days[(hour(time_to_predict_step)+1):(hour(time_to_predict_step)+12)]
    
    #Update the historical data with the Modelica data
    #Energy consumed in time to predict to have simulated temperature at time to predict +1
    df$hp_cons[df$time==time_to_predict_step - hours(1)] = 
      sum(df_result_20s$hp_el[(length(df_result_20s$hp_el)-(3600/step_size)):(length(df_result_20s$hp_el)-1)])*(step_size/3600)
    #Inside temperature at time to predict +1 due to the effect of use the energy between time to predict and time to predict +1
    df$ti[df$time == time_to_predict_step] = 
      sum(df_result_20s$Temp_aver[(length(df_result_20s$Temp_aver)-(3600/step_size)):(length(df_result_20s$Temp_aver)-1)])/(3600/step_size)
    
    # nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features)))
    # class_per_feature = mapply(function(i){i[['class']]},features)
    # nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features)
    # levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
    # names_per_feature = names(features)
    # time_to_predict = time_to_predict_step
    
    #Compute GA
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
      ))
    
    #Get the solution of the optimization
    params_hp_tset_24h_opt <- as.numeric(decodeValueFromBin(binary_representation = optimization_results_MPC@solution[1,],
                                                            class_per_feature = mapply(function(i){i[['class']]},features),
                                                            nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
                                                            levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
                                                            ))
    print(params_hp_tset_24h_opt)
    
    #Update the suggestion for the next iteration using the previous solution
    params_hp_tset_sugg = c(params_hp_tset_24h_opt[-1], NA)
    for (i in 1:length(params_hp_tset_sugg)) {
      if (is.na(params_hp_tset_sugg[i])) {
        params_hp_tset_sugg[i] <- min_hp_tset
      }
    }
    suggestions = decodeBinFromValue(values = params_hp_tset_sugg, class_per_feature = mapply(function(i){i[['class']]},features),
                                     nclasses_per_feature =  mapply(function(i){length(i[["levels"]])},features),
                                     levels_per_feature = lapply(function(i){i[["levels"]]}, X = features))
    
    #To check the results is computed the optimizer code with the solution and stored the results in dataframes
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
                                                 "Tset[12]" = params_hp_tset_24h_opt[12]))
    
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
                                                 "Tave[12]" = RETURN$predv.ti_l0[12]))
    
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
                                                  "Text[12]" = RETURN$predv.te_l0[12]))
    
    df_costs <- rbind(df_costs, data.frame("penalty" = penalty[1],
                                           "iteration_price" = iteration_price[1]))
    
    # print(df_sol_Text)
    # print(df_sol_Tave)
    
    #Update the Supply temperautre and the Heat Pump status
    Tsup_optim <- params_hp_tset_24h_opt[1]
    HPs_optim = 1.0
    if (is.na(Tsup_optim)) {
      Tsup_optim = 0.0
      HPs_optim = 0.0
    }
    
    #Update the costs of the previous iteration whitch takes into account the cost between t and t+1
    df_result$hp_el[length(df_result$hp_el)] <- 
      sum(df_result_20s$hp_el[(length(df_result_20s$hp_el)-(3600/step_size)):(length(df_result_20s$hp_el)-1)])*(step_size/3600)
    df_result$cost[length(df_result$cost)] <- 
      sum(df_result_20s$cost[(length(df_result_20s$cost)-(3600/step_size)):(length(df_result_20s$cost)-1)])
    df_result$Tcost[length(df_result$Tcost)] <- Tcost
    
    df_result <- rbind(df_result, data.frame("time_to_predict_step" = time_to_predict_step,
                                             "time" = time,
                                             "Tsup_optim" = Tsup_optim,
                                             "HPs_optim" = HPs_optim, 
                                             "Temp_aver" = Taver, #sum(df_result_20s$Temp_aver[(i-(3600/step_size)):(i-1)])/(3600/step_size), 
                                             "hp_el" = 0, #sum(df_result_20s$hp_el[(i-(3600/step_size)):(i-1)])*(step_size/3600),
                                             "cost" = 0,
                                             "Tcost" = 0,
                                             "price_position (h+1)" = hour(time_to_predict_step)+1,
                                             "price" = df_price$price[(hour(time_to_predict_step)+1)],
                                             "ti_min" = ti_min_2days[(hour(time_to_predict_step)+1)],
                                             "ti_max" = ti_max_2days[(hour(time_to_predict_step)+1)],
                                             "MPC_fitnessValue" = 0))
    print(paste(time_to_predict_step, Tsup_optim, HPs_optim, df_result$cost[length(df_result$cost)-1], 
                df_result$Tcost[length(df_result$Tcost)-1], time))
  }
  
  #Modelica simulation
  step <- Iteration(fmu, vr_input1, vr_input2, vr_output1, vr_output2, time, step_size, Tsup_optim, HPs_optim)
  # Iteration return -> fmu, input1, input2, output1(Tav), output2(hp_el)
  
  #Extract data from the step simulation
  input1 <- step[[2]]
  input2 <- step[[3]]
  output1 <- step[[4]]
  output2 <- step[[5]]
  cost <- df_price$price[hour(time_to_predict_step)+1]/1000000*output2*(step_size/3600)
  Tcost = Tcost + cost
  # print(paste(time_to_predict_step, output1, output2, Tcost, time))
  
  #Update the dataframe with the modelica results
  df_result_20s <- rbind(df_result_20s, data.frame("time_to_predict_step" = time_to_predict_step,
                                                   "time" = time,
                                                   "Tsup_optim" = Tsup,
                                                   "HPs_optim" = HPs, 
                                                   "Temp_aver" = Taver, #The internal temperature is the one from the previous calculation 
                                                   "hp_el" = output2,
                                                   "cost" = cost,
                                                   "ti_min" = ti_min_2days[(hour(time_to_predict_step)+1)],
                                                   "ti_max" = ti_max_2days[(hour(time_to_predict_step)+1)]))
  
  Taver <- output1 #the computed Taver is the internal temperature of the next iteration
  
  #Update time for next step
  time = time + step_size
  time_to_predict_step <- time_to_predict_step + seconds(step_size)
  
  # print(df_result)
}

Total_cost_BM <- df_result$Tcost[length(df_result$Tcost)-1]-df_result$Tcost[df_result$time_to_predict_step == as.POSIXct(x = "2018-12-19 18:00:00 UTC", tz = "UTC")]
Total_cost_BM

# END_fmu(fmu)

######################################################### Disconfort #########################################################
#In steps of 1h
df_disconfort <- df_result[df_result$time_to_predict_step >= as.POSIXct(x = "2018-12-19 19:00:00 UTC", tz = "UTC"),]
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
sum(df_disconfort$hp_el*df_disconfort$price/1000000)

#In steps of 20 seconds
df_disconfort_20s <- df_result_20s[df_result_20s$time_to_predict_step >= as.POSIXct(x = "2018-12-19 19:00:00 UTC", tz = "UTC"),]
disconfort_20s <- {}
for (i in 1:length(df_disconfort_20s$time_to_predict_step)) {
  if (df_disconfort_20s$ti_min[i] > df_disconfort_20s$Temp_aver[i]) {
    x <- (df_disconfort_20s$ti_min[i]-df_disconfort_20s$Temp_aver[i])*20/3600
    disconfort_20s[i] <- x #(-exp(lambda*x)/(x-delta_limit))-(1/delta_limit)
  } else {
    disconfort_20s[i] <- 0
  }
}
max(disconfort_20s)
Total_disconfort_20s <- sum(disconfort_20s)
hist(disconfort_20s)
disconfort <- disconfort_20s[disconfort_20s != 0]
hist(disconfort_20s)
sum(df_disconfort_20s$hp_el*price_2days[hour(df_disconfort_20s$time_to_predict_step)+1]/1000000)*20/3600
Total_cost_20s <- sum(df_disconfort_20s$cost)
sum(df_disconfort_20s$hp_el[length(df_disconfort_20s$hp_el)-180:length(df_disconfort_20s$hp_el)]*
      price_2days[hour(df_disconfort_20s$time_to_predict_step[length(df_disconfort_20s$hp_el)-180:length(df_disconfort_20s$hp_el)])+1]/1000000)*20/3600

######################################################### PLOTS #########################################################

df_plot <- df_result[, c("time_to_predict_step", "Tsup_optim", "HPs_optim", "Temp_aver", "hp_el", "cost", "price")]
df_plot <- melt(df_plot, "time_to_predict_step") 
df_plot$variable <- as.factor(df_plot$variable)

ggplot(df_plot) + 
  geom_line(aes(x = time_to_predict_step, y = value)) +
  facet_wrap(~as.factor(variable), ncol = 1, scales = "free_y")

ggplot() +
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$Temp_aver)) +
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$ti_min, color = "red")) + 
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$ti_max, color = "blue"))

# penalty <- zoo::rollapply(disconfort, width=12, align="left",partial=T,FUN=function(x){sum(x,na.rm=T)})


library(plotly)
ggplotly(
  ggplot(df) +
    aes(time,ti) + geom_line(),
  dynamicTicks = "x"
    )

ggplotly(
  ggplot(df) +
    aes(time,te) + geom_line(),
  dynamicTicks = "x"
)


for (i in 1:length(df_result_20s$time_to_predict_step)) {
  if (hour(df_result_20s$time_to_predict_step[i]) > 7) {
    df_result_20s$ti_min[i] <- 20
    df_result_20s$ti_max[i] <- 24
  }else{
    df_result_20s$ti_min[i] <- 18
    df_result_20s$ti_max[i] <- 22
  }
}


plot1 <- ggplot() + geom_line(aes(df$time[1:338], df$te[1:338]))
plot2 <- ggplot() + geom_line(aes(df$time, df$te))

plot_temperature <- ggplot() +
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$Temp_aver)) +
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$ti_min, color = "red")) + 
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$ti_max, color = "blue")) +
  geom_line(aes(x = df_result$time_to_predict_step, y = df$te[1:length(df_result$time_to_predict_step)], color = "green")) +
  geom_line(aes(x = df_result$time_to_predict_step, y = df_result$hp_el/100)) +
  theme(legend.position = "none")
plot_temperature

grid.arrange(plot_temperature, plot1, nrow =2)

ggplot(df) +
  geom_line(aes(x = df$time, y = df$ti)) 
  # geom_line(aes(x = df_result$time_to_predict_step, y = df_result$ti_min, color = "red")) + p


ggplotly(
  ggplot(df) +
    aes(time,ti) + geom_line(),
  dynamicTicks = "x"
)

ggplotly(
  ggplot(df) +
    aes(time,te) + geom_line(),
  dynamicTicks = "x"
)

df_plot <- df_result#[df_result$time_to_predict_step < as.POSIXct(x = "2018-12-26 00:00:00 UTC", tz = "UTC"),]
# ggplotly(
ggplot() +
  geom_line(aes(x = df_plot$time_to_predict_step, y = df_plot$Temp_aver)) +
  geom_line(aes(x = df_plot$time_to_predict_step, y = df_plot$ti_min, color = "red")) + 
  geom_line(aes(x = df_plot$time_to_predict_step, y = df_plot$ti_max, color = "blue")) +
  # geom_line(aes(x = df_plot$time_to_predict_step, y = df$te[1:length(df_plot$time_to_predict_step)], color = "green")) +
  # geom_line(aes(x = df_plot$time_to_predict_step, y = df_plot$hp_el/100)) +
  geom_point(aes(df_plot$time_to_predict_step,ifelse(df_plot$HPs_optim>0,df_plot$Temp_aver,NA)),col=3)+
  theme(legend.position = "none") +
  xlab("Date") + ylab("Temperature [ºC]")#,
# dynamicTicks = "x")

ggplot() + geom_line(aes(df$time,df$te)) +
  geom_vline(xintercept=c(as.POSIXct(x = "2018-12-25 00:00:00 UTC", tz = "UTC")), linetype=2, color = "red", size = 1) + 
  geom_vline(xintercept=c(as.POSIXct(x = "2019-01-03 00:00:00 UTC", tz = "UTC")), linetype=2, color = "blue", size = 1)

######################################################### Export Data to Matlab ###############################################

#Export R2 data
writeMat("Results_send_Arash/R2_data.mat",
         #Results in validation dates data frame 1
         ti_df = ti_df,
         ti_l0_df = ti_l0_df,
         ii_df = ii_df,
         ti_df_1 = ti_df_1,
         ti_l0_df_1 = ti_l0_df_1,
         R2_df = R2_df,
         R2_df_1 = R2_df_1,
         #Results in all data frame 2
         ti_df2 = ti_df2,
         ti_l0_df2 = ti_l0_df2,
         ii_df2 = ii_df2,
         ti_df2_1 = ti_df2_1,
         ti_l0_df2_1 = ti_l0_df2_1,
         R2_df2 = R2_df2,
         R2_df2_1 = R2_df2_1
)

#???Export MPC results
time_to_predict_step_20s <- as.character(df_disconfort_20s$time_to_predict_step)
time_to_predict_step_1h <- as.character(df_disconfort$time_to_predict_step)

writeMat("Results_send_Arash/MPC_data.mat", #Need:time(real), time modelica, Tin, hp_el, electric price in intervals of 20s and hourly
         time_to_predict_step_20s = time_to_predict_step_20s,
         time_to_predict_step_1h = time_to_predict_step_1h,
         time_20s = df_disconfort_20s$time,
         time_1h = df_disconfort$time,
         Taver_20s = df_disconfort_20s$Temp_aver,
         Taver_1h = df_disconfort$Temp_aver,
         ti_min_20s = df_disconfort_20s$ti_min,
         ti_min_1h = df_disconfort$ti_min,
         ti_max_20s = df_disconfort_20s$ti_max,
         ti_max_1h = df_disconfort$ti_max,
         Tsupply_20s = df_disconfort_20s$Tsup_optim,
         Tsupply_1h = df_disconfort$Tsup_optim,
         hp_status_20s = df_disconfort_20s$HPs_optim,
         hp_status_1h = df_disconfort$HPs_optim,
         hp_el_20s = df_disconfort_20s$hp_el,
         price_20s = df_disconfort_20s$price,
         cost_20s = df_disconfort_20s$cost,
         Total_discomfort = Total_disconfort_20s,
         Total_cost = Total_cost_20s
)


