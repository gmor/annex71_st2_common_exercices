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
loadfonts()

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

############################################### MEASURING GOODNESS OF FIT #############################################
# Parameters to be calculated for the validation sample:

# https://stackoverflow.com/questions/40901445/function-to-calculate-r2-r-squared-in-r
# R squared can be a (but not the best) measure of "goodness of fit". But there is no justification that it can measure 
# the goodness of out-of-sample prediction. If you split your data into training and testing parts and fit a regression 
# model on the training one, you can get a valid R squared value on training part, but you can't legitimately compute an 
# R squared on the test part. Some people did this, but I don't agree with it.

# 1) R-squared or goodness of fit:

calculate_goodness <- function(y_actual, y_pred){
  y_mean = mean(y_actual) * rep(x = 1, times = length(y_actual))  
  num = sum((y_actual - y_pred)^2)
  den = sum((y_actual - y_mean)^2)
  R_squared = 1 - num/den
  R_squared_2 = (cor(x = y_actual, y = y_pred))^2
  R_squared_3 = summary(lm(y_pred~y_actual))$r.squared
  
  goodness_fit = 1 - sqrt(num/den)
  
  return(data.frame(R_squared = R_squared, 
                    R_squared_2 = R_squared_2, 
                    R_squared_3 = R_squared_3, 
                    goodness_fit = goodness_fit)
  )
}

# 2) RMSE or NRMSE 
calculate_error <- function(y_actual, y_pred){
  rmse = sqrt(sum((y_pred - y_actual)^2)/length(y_pred))
  nrmse = (sqrt(sum((y_pred - y_actual)^2)/length(y_pred))) / mean(y_actual)
  
  return(data.frame(rmse = rmse, 
                    nrmse = nrmse)
  )
}

# calculate_many_errors <- function(val_dates){
#   
#   for (i in 1:length(val_dates)) {
#     
#     i = 1
#     date = val_dates[i]
#     rows_to_filter = as.Date(predv$time,"Europe/Madrid") %in% val_dates[i]
#     
#     df_goodness <- calculate_goodness(y_actual = y_actual, y_pred = y_pred)
#     df_error <- calculate_error(y_actual = y_actual, y_pred = y_pred)
#     
#     print(df_goodness)
#     
#     df_day_aux <- data.frame(date = date, 
#                              R_squared = df_goodness$R_squared, 
#                              goodness_fit = df_goodness$goodness_fit, 
#                              rmse = df_error$rmse,
#                              nrmse = df_error$nrmse 
#     )
#     df_day <- rbind(df_day, df_day_aux)
#   }
# } 


# df_day <- data.frame(date = character(), 
#                      R_squared = numeric(),
#                      goodness_fit = numeric(), 
#                      R_squared_2 = numeric(),
#                      rmse = numeric(), 
#                      nrmse = numeric())


vector_hours_complete <- c(23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
# vector_hours <- c(23, 0, 1, 2, 3, 4, 5, 6, 7)

# vector_hours <- c(23, 0, 1, 2)
steps = c(2:12)

for (l in steps) {
  # l = steps[1] 
  vector_hours = vector_hours_complete[1:l]
  df_day <- data.frame(date = character(), 
                       R_squared = numeric())

  for (k in 1:length(val_dates)) {
   
    # k = 1
    val_date = val_dates[k]
  
    predv <- prediction_scenario(
      mod_q = mod_q, 
      mod_ti = mod_ti,
      mod_tsupply = mod_tsupply,
      df = df,
      rows_to_filter = (as.Date(df$time,"Europe/Madrid") %in% val_date & hour(df$time) %in% vector_hours) ,
      hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
      params = params,
      ts_prediction = NULL
    )
    
    # grid.arrange(
    #   ggplot(predv)+
    #     geom_line(aes(time,tsupply))+
    #     geom_line(aes(time,tsupply_l0),col=2)+
    #     geom_point(aes(time,ifelse(hp_status_l0>0,hp_tset_l0,NA)),col=3)+
    #     ylab("Supply temperature [ºC]")+
    #     facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") +
    #     scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
    #     theme_bw(),
    p <- ggplot(predv)+
        geom_line(aes(time,ti))+
        geom_line(aes(time,ti_l0),col=2)+
        ylab("Indoor temperature [ºC]")+
        facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") +
        scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
        theme_bw() 
    #   ggplot(predv)+
    #     geom_line(aes(time,hp_cons))+
    #     geom_line(aes(time,hp_cons_l0),col=2)+
    #     ylab("HP electricity [Wh]")+
    #     facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") +
    #     scale_x_datetime(date_minor_breaks = "2 hours" , date_labels = "%H:%M") +
    #     theme_bw(),
    #   ncol=1
    # )
    # ggsave(filename = paste0(val_date, "prediction.pdf"), plot = p)
  
    # df_input <- rbind(predv[, colnames(df_original)], df[(as.Date(df$time,"Europe/Madrid") %in% val_date & hour(df$time) %in% (0:23)[!(0:23 %in% vector_hours)]), ])
    
    y_actual = predv$ti
    y_pred = predv$ti_l0
    df_goodness <- calculate_goodness(y_actual = y_actual, y_pred = y_pred)
    # df_error <- calculate_error(y_actual = y_actual, y_pred = y_pred)
    
    # df_day_aux <- data.frame(date = val_date, 
    #                          R_squared = df_goodness$R_squared, 
    #                          goodness_fit = df_goodness$goodness_fit, 
    #                          R_squared_2 = df_goodness$R_squared_2,
    #                          rmse = df_error$rmse,
    #                          nrmse = df_error$nrmse 
    # )
    
    df_day_aux <- data.frame(date = val_date, 
                             R_squared_2 = df_goodness$R_squared_2)
    
    df_day <- rbind(df_day_aux, df_day)  
     
  }  

  print(l)
  print(mean(df_day$R_squared_2))

}





