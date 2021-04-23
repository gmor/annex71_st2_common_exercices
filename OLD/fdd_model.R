# Annex 71 Subtask 2, Common Exercise 3: Fault Detection and Diagnosis
source("functions_fdd.R")

# Libraries for mathematics
library(expm)
library(splines)
library(mgcv)

# Libraries for graphics
library(cowplot)
library(ggplot2)
library(gridExtra)
library(extrafont)
loadfonts()
# Libraries for data wrangling
library(padr)
library(lubridate)
library(parallel)
library(oce)
library(GA)
#library(zoo)
library(data.table)
#library(tidyr)
#library(scales)
#library(pracma)
library(readxl) 


#Data import
indoor_temp_fdd <- read_excel_allsheets("data/Twin_house_O5_FDD_CE3.xlsx")$`Indoor temperatures`
hp_fdd <- read_excel_allsheets("data/Twin_house_O5_FDD_CE3.xlsx")$`Heat pump`
heat_gains_fdd <- read_excel_allsheets("data/Twin_house_O5_FDD_CE3.xlsx")$`Internal heat gains`
weather_fdd <- read_excel_allsheets("data/Twin_house_O5_FDD_CE3.xlsx")$`Climate data`
weather_fdd[,c("sunAz","sunEl")] <- do.call(cbind,oce::sunAngle(weather_fdd$DATE,latitude = 47.874,longitude = 11.728))[,c("azimuth","altitude")]

fdd_hp_heaters <- hp_fdd$o5_HP_additional_heater_L1_elP + hp_fdd$o5_HP_additional_heater_L2_elP + 
  hp_fdd$o5_HP_additional_heater_L3_elP
fdd_hp_cons <- hp_fdd$o5_HP_compressor_elP

#Need to include lagged variables for fdd_hp_heaters and include them in the tsupply model, as well as interactions with
# the compressor consumption
ggplot() + geom_point(aes(1:length(fdd_hp_cons),fdd_hp_cons)) + geom_hline(yintercept = 80, col = 'red')
ggplot() + geom_histogram(aes(fdd_hp_cons), bins = 50) + geom_vline(xintercept  = 80, col = 'red')


fdd_ti <- (indoor_temp_fdd$o5_aroom_bath_110_AT*18 + indoor_temp_fdd$o5_aroom_bed_110_AT*29 + 
             indoor_temp_fdd$o5_aroom_child1_110_AT*62 + indoor_temp_fdd$o5_aroom_child2_110_AT*66 + 
             indoor_temp_fdd$o5_aroom_corridor_110_AT*14 + indoor_temp_fdd$o5_aroom_dining_110_AT*29 + 
             indoor_temp_fdd$o5_aroom_doorway_110_AT*15 + indoor_temp_fdd$o5_aroom_kitchen_110_AT*19 + 
             indoor_temp_fdd$o5_aroom_living_110_AT*87 + indoor_temp_fdd$o5_aroom_stairs_170_AT*23)/362

fdd_hg <- rowSums(heat_gains_fdd[,3:ncol(heat_gains_fdd)])

df_house_fdd <- data.frame("time"=hp_fdd$DATE,
                           hp_cons = fdd_hp_cons,
                           hp_heaters = fdd_hp_heaters,
                           tsupply = hp_fdd$o5_HeatPump_Tsup,
                           hp_tset = hp_fdd$o5_HeatPump_Tsup,
                           ti = fdd_ti,
                           hp_status = rep(1,nrow(hp_fdd)),
                           hg = fdd_hg)

df_house_fdd$hp_status <- ifelse(df_house_fdd$hp_cons >80,1,0)

df_weather_fdd <- data.frame(
  "time"=weather_fdd$DATE,
  "te"=weather_fdd$AmbientAirTemperature,
  "GHI"=weather_fdd$Radiation_Global,
  "BHI"=weather_fdd$Radiation_Global-weather_fdd$Radiation_Diffuse,
  "sunAz"=weather_fdd$sunAz,
  "sunEl"=weather_fdd$sunEl,
  "humidity"=weather_fdd$RelativeHumidity,
  "windSpeed"=weather_fdd$WindSpeed,
  "windBearing"=weather_fdd$WindDirection
)

df_fdd <- merge(df_house_fdd, df_weather_fdd)

grid.arrange(ggplot(df_fdd[df_fdd$time<"2019-02-07",]) + geom_line(aes(time,tsupply)),
             ggplot(df_fdd[df_fdd$time<"2019-02-07",]) + geom_line(aes(time,hp_cons)), 
             ggplot(df_fdd[df_fdd$time<"2019-02-07",]) + geom_line(aes(time,hp_status))
)

hour(df_fdd$time)
df_fdd$hour <- as.POSIXct(trunc(df_fdd$time, units = "hours"))

df_fdd <- data.frame(
  aggregate(df_fdd$hp_cons, list("t"=df_fdd$hour),FUN=sum),
  aggregate(df_fdd$hp_heaters, list("t"=df_fdd$hour),FUN=sum)$x,
  aggregate(df_fdd$tsupply,list("t"=df_fdd$hour),FUN=mean)$x,
  aggregate(df_fdd$hp_tset,list("t"=df_fdd$hour),FUN=mean)$x,
  aggregate(df_fdd$hg,list("t"=df_fdd$hour),FUN=sum)$x,
  aggregate(df_fdd$ti,list("t"=df_fdd$hour),FUN=mean)$x,
  aggregate(df_fdd$te,list("t"=df_fdd$hour),FUN=mean)$x,
  aggregate(df_fdd$humidity,list("t"=df_fdd$hour),FUN=mean)$x,
  aggregate(df_fdd$windSpeed,list("t"=df_fdd$hour),FUN=mean)$x,
  aggregate(df_fdd$windBearing,list("t"=df_fdd$hour),FUN=mean)$x,
  aggregate(df_fdd$sunAz,list("t"=df_fdd$hour),FUN=max)$x,
  aggregate(df_fdd$sunEl,list("t"=df_fdd$hour),FUN=max)$x,
  aggregate(df_fdd$GHI,list("t"=df_fdd$hour),FUN=mean)$x,
  aggregate(df_fdd$BHI,list("t"=df_fdd$hour),FUN=mean)$x,
  ifelse(aggregate(df_fdd$hp_cons,list("t"=df_fdd$hour),FUN=mean)$x>80,1,0)
)

colnames(df_fdd) = c("time","hp_cons","hp_heaters", "tsupply", "hp_tset", "hg", "ti", "te", "humidity",
                     "windSpeed", "windBearing", "sunAz","sunEl", "GHI","BHI","hp_status")

ggplot(reshape2::melt(df_fdd,"time")) + geom_line(aes(time,value)) + facet_wrap(~variable,ncol=1,scales="free_y")

#Analysis tsupply hp_cons releationship
grid.arrange(ggplot(df_fdd[df_fdd$time<"2019-02-07",]) + geom_line(aes(time,tsupply)),
             ggplot(df_fdd[df_fdd$time<"2019-02-07",]) + geom_line(aes(time,hp_cons)), 
             ggplot(df_fdd[df_fdd$time<"2019-02-07",]) + geom_line(aes(time,hp_status))
             )

# Training and validation datasets
all_dates <- sort(unique(as.Date(df_fdd$time,"Europe/Madrid")))
no_fault_dates <- all_dates[all_dates>as.Date("2019-02-01") & all_dates<as.Date("2019-03-01")]
train_dates <- sample(no_fault_dates,size = length(no_fault_dates)*0.9,replace = F)
val_dates <- no_fault_dates[!(no_fault_dates %in% train_dates)]

# Optimize the alpha values of the low pass filters
features <- list("alpha_te"=list(min=0,max=0.9,n=31,class="float"),
                 "alpha_BHI"=list(min=0,max=0,n=0,class="float"),
                 "alpha_GHI"=list(min=0,max=0,n=0,class="float"),
                 "alpha_ws"=list(min=0,max=0.9,n=15,class="float"),
                 "mod_hp_cons_ar"=list(min=1,max=6,n=5,class="int"),
                 "mod_hp_cons_lags_tsupply"=list(min=1,max=1,n=0,class="int"),
                 "mod_hp_cons_lags_te"=list(min=0,max=0,n=0,class="int"),
                 "mod_hp_cons_lags_humidity"=list(min=0,max=0,n=0,class="int"),
                 "mod_hp_cons_lags_hp_heaters"=list(min=0,max=3,n=3,class="int"),
                 "mod_tsupply_ar"=list(min=1,max=5,n=4,class="int"),
                 "mod_tsupply_lags_hp_cons"=list(min=0,max=3,n=3,class="int"),
                 "mod_tsupply_lags_dti"=list(min=0,max=0,n=0,class="int"),
                 "mod_tsupply_lags_hp_heaters"=list(min=0, max=6, n=6, class = "int"),
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
    df = df_fdd,
    train_dates = train_dates,
    val_dates = val_dates,
    popSize = 32,
    maxiter = 10,
    monitor = gaMonitor2,
    parallel = 8,
    elitism = 0.08,
    pmutation = 0.05)
)

params <- decodeValueFromBin(optimization_results@solution[1,],
                             mapply(function(i){i[['class']]},features), 
                             mapply(function(i){i[['n']]},features), 
                             mapply(function(i){i[['min']]},features), 
                             mapply(function(i){i[['max']]},features))
names(params) <- names(features)
print(params)

# Calculate the models with consumption as output (result_q) and indoor temperature as output (result_ti).
result_q <- calculate_model_q(params, df_fdd, train_dates, output="model")
result_ti <- calculate_model_ti(params, df_fdd, train_dates, output="model")
result_tsupply <- calculate_model_tsupply(params, df_fdd, train_dates, output="model")
mod_q <- result_q$mod
mod_ti <- result_ti$mod
mod_tsupply <- result_tsupply$mod
df_mod <- result_ti$df
cpgram(mod_q$model$hp_cons_l0-mod_q$fitted.values)
cpgram(mod_ti$model$ti_l0-mod_ti$fitted.values)
cpgram(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values)
summary(mod_tsupply)
summary(mod_q)
summary(mod_ti)

# Validation of the models in 24h predictions
predv <- prediction_scenario(
  mod_q = mod_q, 
  mod_ti = mod_ti,
  mod_tsupply = mod_tsupply,
  df = df_fdd,
  rows_to_filter = as.Date(df_fdd$time,"Europe/Madrid") %in% val_dates,
  hp_tset_24h = ifelse(df_fdd$hp_status==0,"NA", as.character(ceiling(df_fdd$hp_tset))),
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
