# ¿IMPORTANT TODO! CHANGE 24HS TO 12HS?

# pulse secure:
# r0753014
# MPCsubex1

setwd("C:/Users/gerar/Nextcloud/Github/annex71_st2_common_exercices")
source("Paper_horizon_length/penalized-functions.R")

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
library(scales)

# Libraries for data wrangling
library(padr)
library(lubridate)
library(parallel)
library(oce)
library(GA)
library(data.table)
library(readxl) 
library(doParallel)
library(penalized)
library(purrr)
library(snow)
library(doSNOW)


# library to integrate Python
library(reticulate)

# #library ML
# library(autoxgboost)

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
                       "air_h"=rowSums(house[,grepl("_SUA_IHS_elP",colnames(house))]),
                       "air_s"=rowSums(house[,grepl("_SUA_fan_elP",colnames(house))]),
                       "air_e"=rowSums(house[,grepl("_EHA_fan_elP",colnames(house))]),
                       "vent"=rowSums(
                         data.frame((house$o5_Vent_child1_SUA_AT*house$o5_Vent_child1_SUA_VFR-house$o5_child1_AT*house$o5_Vent_child1_EHA_VFR),
                                    house$o5_Vent_child2_SUA_AT*house$o5_Vent_child2_SUA_VFR-house$o5_child2_AT*house$o5_Vent_child2_EHA_VFR,
                                    house$o5_Vent_living_SUA_AT*house$o5_Vent_living_SUA_VFR-(house$o5_living_AT+house$o5_bath_AT)*house$o5_Vent_bath_EHA_VFR
                        )))

df_weather <- data.frame(
  "time"=weather$DATE,
  "te"=weather$AmbientAirTemperature,
  "GHI"=weather$Radiation_Global,
  "BHI"=weather$Radiation_Global-weather$Radiation_Diffuse,
  "sunAz"=weather$sunAz,
  "sunEl"=weather$sunEl,
  "humidity"=weather$RelativeHumidity,
  "windSpeed"=weather$WindSpeed,
  "windBearing"=weather$WindDirection)

df_house$vent_status <- kmeans(df_house$vent, 3)$cluster
# df_house$vent <- house2$total_VFR[1:1053]
df <- merge(df_house,df_weather)
# saveRDS(df, file = "Paper_horizon_length/df.rds")

#take the new_df
df <- readRDS("Paper_horizon_length/df_new.rds")

# df <- df[-1,]

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
df_house2 <- df_house2[1:(length(df_house2$time)-1),]
df_house2$vent_status <- kmeans(df_house2$vent, 2)$cluster

df2 <- merge(df_house2,df_weather)


# ggplot(reshape2::melt(df,"time")) + geom_line(aes(time,value)) + facet_wrap(~variable,ncol=1,scales="free_y")


######################################################### TRAINING MODEL #########################################################

horizon <- 24
# Training and validation datasets
all_dates <- sort(unique(as.Date(df$time,"Europe/Madrid")))
eligible_dates <- all_dates[2:length(all_dates)]
# train_dates <- sample(eligible_dates,size = length(eligible_dates)*0.9,replace = F)
# val_dates <- eligible_dates[!(eligible_dates %in% train_dates)]

# train_dates <- eligible_dates[1:floor(length(eligible_dates)*0.75)]
# val_dates <- eligible_dates[!(eligible_dates %in% train_dates)]

train_dates_1 <- eligible_dates[eligible_dates<as.POSIXct(x = "2018-12-30", tz = "UTC") |  
                                  eligible_dates>as.POSIXct(x = "2019-01-12", tz = "UTC") & 
                                  eligible_dates<as.POSIXct(x = "2019-01-23", tz = "UTC")] #Excluiding the test dates
train_dates_2 <- eligible_dates[eligible_dates>as.POSIXct(x = "2019-01-22", tz = "UTC")]
# train_dates_2 <- train_dates[!(train_dates %in% train_dates_1)]
# train_dates <- sample(eligible_dates,round(length(eligible_dates)*0.75,0),replace = F)
val_dates <- eligible_dates[!(eligible_dates %in% train_dates_1) & !(eligible_dates %in% train_dates_2)] #Same as testing dates
# plot(train_dates_2,rep(1,length(train_dates_2)))
# ggplotly(ggplot() +geom_line(aes(x = df$time,y = df$te)),
#        dynamicTicks = "x")


# Optimize the alpha values of the low pass filters
features <- list("alpha_te"=list(min=0,max=0,n=0,class="float"),
                 "alpha_BHI"=list(min=0,max=0,n=0,class="float"),
                 "alpha_GHI"=list(min=0,max=0,n=0,class="float"),
                 "alpha_ws"=list(min=0,max=0,n=0,class="float"),
                 "mod_hp_cons_ar"=list(min=2,max=2,n=0,class="int"),
                 "mod_hp_cons_lags_tsupply"=list(min=0,max=0,n=0,class="int"),
                 "mod_hp_cons_lags_te"=list(min=0,max=0,n=0,class="int"),#2,2
                 "mod_hp_cons_lags_humidity"=list(min=0,max=0,n=0,class="int"),
                 "mod_hp_cons_lags_ti"=list(min=2,max=2,n=0,class="int"),
                 "mod_tsupply_ar"=list(min=1,max=3,n=2,class="int"),
                 "mod_tsupply_lags_hp_cons"=list(min=1,max=1,n=0,class="int"),#"mod_tsupply_lags_hp_cons"=list(min=0,max=3,n=3,class="int"),
                 "mod_tsupply_lags_dti"=list(min=0,max=0,n=0,class="int"),
                 "mod_tsupply_lags_te"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_ar"=list(min=1,max=5,n=4,class="int"),#"mod_ti_ar"=list(min=1,max=9,n=8,class="int"), #5,3
                 "mod_ti_lags_te"=list(min=1,max=3,n=2,class="int"),
                 "mod_ti_lags_dti"=list(min=1,max=3,n=2,class="int"),#"mod_ti_lags_dti"=list(min=2,max=9,n=7,class="int"),
                 "mod_ti_lags_hp_cons"=list(min=6,max=12,n=6,class="int"),
                 "mod_ti_lags_BHI"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_lags_GHI"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_infiltrations"=list(min=0,max=2,n=2,class="int"),
                 "mod_ti_lags_humidity"=list(min=0,max=0,n=0,class="int"),
                 "mod_ti_lags_ventilation"=list(min=1,max=1,n=0,class="int"),
                 "mod_ti_lags_hg"=list(min=0,max=2,n=2,class="int"),#"mod_ti_lags_hg"=list(min=0,max=7,n=7,class="int"),
                 "mod_ti_lags_air_h"=list(min=0,max=2,n=2,class="int"),
                 "sunAzimuth_nharmonics"=list(min=2,max=2,n=0,class="int"),
                 "windBearing_nharmonics"=list(min=2,max=2,n=0,class="int")
)

## Uncomment to run internally the GA
# nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){i[['n']]},features)))
# min_per_feature = mapply(function(i){i[['min']]},features)
# max_per_feature = mapply(function(i){i[['max']]},features)
# nclasses_per_feature = mapply(function(i){i[['n']]},features)
# class_per_feature = mapply(function(i){i[['class']]},features)
# names_per_feature = names(features)
# train_dates = train_dates_1
# set_seed = sample(10000:99000,size = 1)
# val_dates = train_dates_2

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
    train_dates = train_dates_1,##DATA TO RUN
    val_dates = train_dates_2,##DATA TO RUN
    horizon = horizon,
    popSize = 800,#32 100##MODEL
    maxiter = 20,#10 20##MODEL
    monitor = gaMonitor2,##MODEL
    set_seed = sample(10000:99000,size = 1),
    parallel = "snow", #change for windows  ##MODEL
    elitism = 0.05,#0.08 ##MODEL
    pmutation = 0.30)#0.05 ##MODEL
)
#X<-optimization_results@solution[1,]

params <- decodeValueFromBin(binary_representation = optimization_results@solution[1,],
                             class_per_feature = mapply(function(i){i[['class']]},features), 
                             nclasses_per_feature = mapply(function(i){i[['n']]},features),
                             min_per_feature = mapply(function(i){i[['min']]},features),
                             max_per_feature = mapply(function(i){i[['max']]},features))
names(params) <- names(features)


#Load the already optimized parameters
params <- readRDS("Paper_horizon_length/params_bo")
params["mod_ti_ar"]<-3 #2
params["mod_ti_lags_hg"]<-5 #4

#Select the dates to train the models
train_dates <- c(train_dates_1, train_dates_2)
# train_dates <- eligible_dates
# train_dates <- train_dates_1

# Calculate the models with consumption as output (result_q) and indoor temperature as output (result_ti).
result_q <- calculate_model_q(params, df, train_dates, output="model") #df
mod_q <- result_q$mod

result_cop <- calculate_model_cop(params, df, train_dates, output="model") #df
mod_cop <- result_cop$mod

result_ti <- calculate_model_ti(params, df, train_dates = train_dates, output="model") #df
mod_ti <- result_ti$mod

result_tsupply <- calculate_model_tsupply(params, df, train_dates, output="model") #df
mod_tsupply <- result_tsupply$mod

####################################### PENALTY lambdas #########################################################
source("Paper_horizon_length/Liada/penalized-functions.R")

penalties <- list("L1"=list(min=0,max=0.5,n=5,class="float"),
                  "L2"=list(min=0,max=0.5,n=5,class="float"))

# nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){i[['n']]},penalties)))
# min_per_penalty = mapply(function(i){i[['min']]},penalties)
# max_per_penalty = mapply(function(i){i[['max']]},penalties)
# nclasses_per_penalty = mapply(function(i){i[['n']]},penalties)
# class_per_penalty = mapply(function(i){i[['class']]},penalties)
# names_per_penalty = names(penalties)
# train_dates = train_dates_1
# formula = mod_ti@nuisance$formula

penalty_optimization <- suppressMessages(
  ga(
    type = "binary",##MODEL
    fitness = penalty_optimizer,##MODEL
    nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){i[['n']]},penalties))),##MODEL
    min_per_penalty = mapply(function(i){i[['min']]},penalties),##DATA TO RUN
    max_per_penalty = mapply(function(i){i[['max']]},penalties),##DATA TO RUN
    nclasses_per_penalty = mapply(function(i){i[['n']]},penalties),##DATA TO RUN
    class_per_penalty = mapply(function(i){i[['class']]},penalties),##DATA TO RUN
    names_per_penalty = names(penalties),##DATA TO RUN
    selection = gabin_tourSelection,##MODEL
    df = df,##DATA TO RUN
    train_dates = train_dates_1,##DATA TO RUN
    val_dates = val_dates,##DATA TO RUN
    formula = mod_ti@nuisance$formula,
    mod_q = mod_q,
      # mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    horizon = horizon,
    params = params,
    popSize = 10,#32 100##MODEL
    maxiter = 3,#10 20##MODEL
    monitor = gaMonitor2,##MODEL
    parallel = "snow", #change for windows  ##MODEL
    elitism = 0.08,#0.08 ##MODEL
    pmutation = 0.3)#0.05 ##MODEL
)

penalty <- decodeValueFromBin(binary_representation = penalty_optimization@solution[1,],
                              class_per_feature = mapply(function(i){i[['class']]},penalties), 
                              nclasses_per_feature = mapply(function(i){i[['n']]},penalties),
                              min_per_feature = mapply(function(i){i[['min']]},penalties),
                              max_per_feature = mapply(function(i){i[['max']]},penalties))
names(penalty) <- names(penalties)

result_ti_op <- calculate_model_ti(params, df, train_dates_1, output="model", penalty = penalty) #df
mod_ti_op <- result_ti$mod

L1 <- rep(c(0,0.1,0.2,0.3,0.4,0.5),6)
L2 <- {}
for (i in 1:6) {
  L2 <- c(L2, rep((i-1)/10,6))
}
penaltyy <- data.frame("L1" =L1, "L2" = L2, "score" = 0)

penaltyy <- all_penalties(
  penaltyy = penaltyy,
  mod_q = mod_q,
  mod_tsupply = mod_tsupply,
  df = df,
  val_dates = val_dates,
  params = params,
  horizon = horizon
)

# penalty_optim <- c(0.3,0.3)
penalty_optim <- c(penaltyy[penaltyy$score == max(penaltyy$score),"L1"],penaltyy[penaltyy$score == max(penaltyy$score),"L2"])
names(penalty_optim) <- c("L1", "L2")
result_ti_op <- calculate_model_ti(params, df, train_dates_1, output="model", penalty = penalty_optim) #df
mod_ti_op <- result_ti_op$mod

######################################## MODEL "VALIDATION" ##########################################

#Graphics to check if the ARX model fits
cpgram(mod_q$model$hp_cons_l0-mod_q$fitted.values)
cpgram(mod_ti@nuisance$df$ti_l0-mod_ti@fitted)#mod_ti@residuals)#(mod_ti$model$ti_l0-mod_ti$fitted.values)
cpgram(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values)
cpgram(mod_cop$model$hp_cop_l0-mod_cop$fitted.values)

acf(mod_q$model$hp_cons_l0-mod_q$fitted.values)
acf(mod_ti@nuisance$df$ti_l0-mod_ti@fitted,25)
acf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values)
acf(mod_cop$model$hp_cop_l0-mod_cop$fitted.values,50)

summary(mod_q)
summary(mod_ti)
summary(mod_tsupply)
summary(mod_cop)

ccf(mod_q$model$hp_cons_l0-mod_q$fitted.values,mod_q$model$hp_cons_l0)
ccf(mod_q$model$hp_cons_l0-mod_q$fitted.values,mod_q$model$ti_l1)

# ccf(mod_ti@residuals, mod_ti@nuisance$df$ti_l0, 50)
ccf(mod_ti@residuals, mod_ti@nuisance$df$hp_thermal_l0, 50)
ccf(mod_ti@residuals, mod_ti@nuisance$df$tsupply_l0)
ccf(mod_ti@residuals, mod_ti@nuisance$df$te_l0, 50) #ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$te_l0)
ccf(mod_ti@residuals, mod_ti@nuisance$df$hg_l0, 20) #ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$hg_l0)
ccf(mod_ti@residuals, mod_ti@nuisance$df$BHI_l0, 50) #ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$BHI_l0)
ccf(mod_ti@residuals, mod_ti@nuisance$df$vent_l0, 50) #ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$vent_l0)
# ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$ti_l0)
ccf(mod_ti@residuals, mod_ti@nuisance$df$hp_status_l1) #ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$`as.factor(hp_status_l1)`)
ccf(mod_ti@residuals, mod_ti@nuisance$df$infiltrations_l0, 20) #ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$infiltrations_l0)
# ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$)
ccf(mod_ti@residuals, mod_ti@nuisance$df$vent_status)
ccf(mod_ti@residuals, mod_ti@nuisance$df$air_h)

ccf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,mod_tsupply$model$tsupply_l0)
ccf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,mod_tsupply$model$`as.factor(hp_status_l0)`)
ccf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,mod_tsupply$model$hp_cons_l0)
ccf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,mod_tsupply$model$te_raw_l0)

ccf(mod_cop$model$hp_cop_l0-mod_cop$fitted.values, mod_cop$model$tsupply_l0)
ccf(mod_cop$model$hp_cop_l0-mod_cop$fitted.values, mod_cop$model$te_raw_l0)

# result_ti <- calculate_model_ti(params, df, train_dates = train_dates_1, output="model") #df
# mod_ti <- result_ti$mod

# Validation of the models in 24h predictions
predv <- prediction_scenario(
  mod_q = mod_q, 
  mod_ti = mod_ti,
  mod_tsupply = mod_tsupply,
  mod_cop = mod_cop,
  df = df, #df
  rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates,#val_dates,
  hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
  params = params,
  ts_prediction = NULL,
  horizon = horizon
)

grid.arrange(
  ggplot(predv)+
    geom_line(aes(time,tsupply))+
    geom_line(aes(time,tsupply_l0),col=2)+
    geom_point(aes(time,ifelse(as.numeric(hp_status_l0)>0,hp_tset_l0,NA)),col=3)+
    ylab("Supply temperature [ÂºC]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_breaks = "4 hours" , date_labels = "%H:%M") +
    theme_bw()+ theme(axis.text.x=element_text(angle = 60,hjust=1)),
  ggplot(predv)+
    geom_line(aes(time,ti))+
    geom_line(aes(time,ti_l0),col=2)+
    ylab("Indoor temperature [ÂºC]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_breaks = "4 hours" , date_labels = "%H:%M") +
    theme_bw()+ theme(axis.text.x=element_text(angle = 60,hjust=1)),
  ggplot(predv)+
    geom_line(aes(time,hp_cons))+
    geom_line(aes(time,hp_cons_l0),col=2)+
    ylab("HP electricity [Wh]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_breaks = "4 hours" , date_labels = "%H:%M") +
    theme_bw()+ theme(axis.text.x=element_text(angle = 60,hjust=1)),
  ncol=1
)

horizon <- 24
# Building free floating behaviour
df__ <- df_initaial
df__$hg <- 0
df__$air_h <- 0
df__$BHI <- 0
df__$hp_status <- 0
df__$hp_tset <- 0
df__$ti <- 15
predv_ff <- prediction_scenario(
  mod_q = mod_q, 
  mod_ti = mod_ti,
  mod_tsupply = mod_tsupply,
  mod_cop = mod_cop,
  df = df__, #df
  rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates,#val_dates,eligible_dates[-length(eligible_dates)]
  hp_tset_24h = rep(45,nrow(df)),
  params = params,
  ts_prediction = NULL,
  horizon = horizon,
  set_seed = sample(10000,size = 1)
)
grid.arrange(
  # ggplot(predv_ff)+
  #   geom_line(aes(time,tsupply))+
  #   geom_line(aes(time,tsupply_l0),col=2)+
  #   #geom_point(aes(time,ifelse(hp_status_l0>0,hp_tset_l0,NA)),col=3)+
  #   ylab("Supply temperature [ÂºC]")+
  #   facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
  #   scale_x_datetime(date_breaks = "4 hours" , date_labels = "%H:%M") +
  #   theme_bw()+ theme(axis.text.x=element_text(angle = 60,hjust=1)),
  ggplot(predv_ff)+
    geom_line(aes(time,ti))+
    geom_line(aes(time,ti_l0),col=2)+
    ylab("Indoor temperature [ÂºC]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") + 
    scale_x_datetime(date_breaks = "4 hours" , date_labels = "%H:%M") +
    theme_bw()+ theme(axis.text.x=element_text(angle = 60,hjust=1)),
  ggplot(predv_ff)+
    geom_line(aes(time,hp_cons))+
    geom_line(aes(time,hp_cons_l0),col=2)+
    ylab("HP electricity [Wh]")+
    facet_wrap(~as.factor(ts_prediction),nrow=1,scales="free_x") +
    scale_x_datetime(date_breaks = "4 hours" , date_labels = "%H:%M") +
    theme_bw()+ theme(axis.text.x=element_text(angle = 60,hjust=1)),
  ncol=1
)
# rm(df__)
# rm(predv_ff)

######################################################### Optimal horizon #########################################################

#Create a fake dataset in which is computed the time needed to reach a temperature (hot_temp)
#from a low temperature (initial_temp) with full power (hp_tset = 45ºC)

h1 <- 50 #lags before simulation (needed later for shifting)
h2 <- 100 #simulation iterations
initial_temp <- 15 #initial temperature
hot_temp <- 20 #target temperature
df_fake <- data.frame("time" = df$time[299:(299+h1+h2-1)],
                      "hp_cons" = c(rep(0, h1),rep(NA, h2)),
                      "hp_status" = c(rep(0, h1),rep(1, h2)),
                      "hp_tset" = c(rep(0, h1),rep(45, h2)),
                      "hp_cop" = c(rep(0, h1),rep(NA, h2)),
                      "hp_thermal" = c(rep(0, h1),rep(NA, h2)),
                      "tsupply" = c(rep(0, h1),rep(45, h2)),
                      "ti" = c(rep(initial_temp, h1),rep(NA, h2)),
                      "hg" = rep(0, h1+h2),
                      "te" = rep(-15, h1+h2),
                      "BHI" = rep(0, h1+h2),
                      "GHI" = rep(0, h1+h2),
                      "sunAz" = rep(20, h1+h2),
                      "windSpeed" = rep(0, h1+h2),
                      "vent" = rep(0, h1+h2),
                      "vent_status" = rep(0, h1+h2),
                      "humidity" = rep(0, h1+h2),
                      "windBearing" = rep(0, h1+h2),
                      "air_h" = rep(0, h1+h2),
                      "iteration" = c((1-h1):h2)
                      )
#iteration to compute the increase of temperature timestep by timestep
for (i in 1:(h2)) {
  df_fake_ <- tune_model_input(df_fake,params) #tune the df to have all the lags
  df_fake_ <- df_fake_[h1+i,] #Cut the df to the only iteration computed each loop
  df_fake$hp_status[h1+i] <- df_fake_$hp_status_l0
  df_fake$tsupply[h1+i] <- df_fake_$tsupply_l0 <- 45
  df_fake$hp_cons[h1+i] <- df_fake_$hp_cons_l0 <- predict(mod_q, df_fake_)
  df_fake$hp_cop[h1+i] <- df_fake_$hp_cop_l0 <- predict(mod_cop, df_fake_)
  df_fake$hp_thermal[h1+i] <- df_fake_$hp_thermal_l0 <- df_fake_$hp_cons_l0*df_fake_$hp_cop_l0
  df_fake$ti[h1+i] <- df_fake_$ti_l0 <- (model.matrix(mod_ti@nuisance$formula, 
                                                      df_fake_)[,colnames(model.matrix(mod_ti@nuisance$formula, df_fake_)) %in%
                                                                  names(coef(mod_ti))] %*% coef(mod_ti))
}
rm(df_fake_)

#is taken the instant before and after crossing and aproximate it to a line to get the exact time of crossing (to draw it in the plot)
a <- df_fake$ti[df_fake$ti>hot_temp][1] #temp before crossing
a[2] <- tail(df_fake$ti[df_fake$ti<hot_temp], 1) #temp after corssing
time_heating <- ((nrow(df_fake[df_fake$ti<hot_temp,]))*60 + round((hot_temp-a[2])/(a[1]-a[2])*60)) #time to reach the temperature (minutes)

#time of heating in hours (iterations needed)
hours_heating <- ceiling(time_heating/60)-h1 
hours_heating

#Heating curve plot
ggplotly(
  ggplot(df_fake[10:nrow(df_fake),]) +
    geom_line(aes(x = iteration, y = ti)) +
    geom_hline(yintercept = hot_temp, linetype="dashed", color = 2) +
    geom_point(aes(x = (df_fake$iteration[1] + (time_heating-60)/60), y = hot_temp), size = 5, color = 2, shape = 4, stroke = 1) +
    xlab("Time[hours]") + ylab("Ti[ºC]")
)

c1 <- 50 #lags before simulation
c2 <- 100 #sumulation iterations
initial_temp <- 20 #initial temperature
cold_temp <- 15 #Minimum temperature allowed night setback
df_fake_cold <- data.frame("time" = df$time[299:(299+c1+c2-1)],
                      "hp_cons" = c(rep(0, c1),rep(NA, c2)),
                      "hp_status" = c(rep(0, c1),rep(0, c2)),
                      "hp_tset" = c(rep(0, c1),rep(0, c2)),
                      "hp_cop" = c(rep(0, c1),rep(NA, c2)),
                      "hp_thermal" = c(rep(0, c1),rep(NA, c2)),
                      "tsupply" = c(rep(0, c1),rep(45, c2)),
                      "ti" = c(rep(initial_temp, c1),rep(NA, c2)),
                      "hg" = rep(0, c1+c2),
                      "te" = rep(-15, c1+c2),
                      "BHI" = rep(0, c1+c2),
                      "GHI" = rep(0, c1+c2),
                      "sunAz" = rep(20, c1+c2),
                      "windSpeed" = rep(0, c1+c2),
                      "vent" = rep(0, c1+c2),
                      "vent_status" = rep(0, c1+c2),
                      "humidity" = rep(0, c1+c2),
                      "windBearing" = rep(0, c1+c2),
                      "air_h" = rep(0, c1+c2),
                      "iteration" = c((1-c1):c2)
)

#iteration to compute the decrease of temperature timestep by timestep (free floating)
for (i in 1:(c2)) {
  df_fake_ <- tune_model_input(df_fake_cold,params)
  df_fake_ <- df_fake_[c1+i,]
  df_fake_cold$hp_status[c1+i] <- df_fake_$hp_status_l0 <- 0
  df_fake_cold$hp_cons[c1+i] <- df_fake_$hp_cons_l0 <- 0
  df_fake_cold$tsupply[c1+i] <- df_fake_$tsupply_l0 <- predict(mod_tsupply, df_fake_)
  df_fake_cold$hp_cop[c1+i] <- df_fake_$hp_cop_l0 <- 0
  df_fake_cold$hp_thermal[c1+i] <- df_fake_$hp_thermal_l0 <- df_fake_$hp_cons_l0*df_fake_$hp_cop_l0
  df_fake_cold$ti[c1+i] <- df_fake_$ti_l0 <- (model.matrix(mod_ti@nuisance$formula, 
                                                      df_fake_)[,colnames(model.matrix(mod_ti@nuisance$formula, df_fake_)) %in%
                                                                  names(coef(mod_ti))] %*% coef(mod_ti))
}
rm(df_fake_)

#is taken the instant before and after crossing and aproximate it to a line to get the exact time of crossing (to draw it in the plot)
a <- df_fake_cold$ti[df_fake_cold$ti<cold_temp][1] #temp before crossing
a[2] <- tail(df_fake_cold$ti[df_fake_cold$ti>cold_temp], 1) #temp after corssing
#time in minutes to reach the minimum allowed temperature
time_ff <- ((nrow(df_fake_cold[df_fake_cold$ti>cold_temp,]))*60 + ((1-(cold_temp-a[1])/(a[2]-a[1]))*60))

#time of free floating in hours to reach the minumum temp. (iterations needed)
hours_ff <- ceiling(time_ff/60)-c1
hours_ff

#Plot of the temperature decay
ggplotly(
  ggplot(df_fake_cold[10:nrow(df_fake_cold),]) +
    geom_line(aes(x = iteration, y = ti)) +
    geom_hline(yintercept = cold_temp, linetype="dashed", color = 2) +
    geom_point(aes(x = (df_fake_cold$iteration[1] + (time_ff-60)/60), y = cold_temp), size = 5, color = 2, shape = 4, stroke = 1) +
    xlab("Time[hours]") + ylab("Ti[ºC]")
)

#Once the free floating and the heating curve are generated now is time to compute the night setback horizon
#There are 3 possibilities
# 1.- Optimal horizon larger than setback (is taken the optimal horizon)
# 2.- The building reaches the lower temperature (is taken the time to heat + opt. horizon)
# 3.- The bulding dosn't reach the minimum temp (is taken the time between the line corssing and the end of setback + opt. horizon)


horizon_optim <- 7 #horizon from the ARX lags
setback <- 10 #time of setback
# The heating curve is moved in order to reach the target temp at the end of the  setback
df_fake_g <- df_fake[((h1+1) - setback + hours_heating):nrow(df_fake),]
df_fake_g$iteration <- c(1:nrow(df_fake_g))
#The free floating curve is moved to start at zero
df_fake_cold_g <- df_fake_cold[(c1+0):nrow(df_fake_cold),]

#Time between the curves cross and the end of setback
hours_heating_from_cooling <- setback - (df_fake_g$iteration[df_fake_cold_g$ti[1:(nrow(df_fake_g))]<df_fake_g$ti][1]-1-1) #-1 for counting the iteration before the crossing
                                                                                                                          #segond -1 for counting the "zero"
#Decision tree of whitch horizon to take in night setback
min_horizon <- max(horizon_optim, min(setback, horizon_optim+hours_heating, hours_heating_from_cooling + horizon_optim))
#Summary of the horizon decission parameters
paste("the free floating time is:", hours_ff)
paste("the heating time is:", hours_heating)
paste("the setback time is:", setback)
paste("the optimal horizon is:", horizon_optim)
paste("min horizon in night setback is:", min_horizon)

#Plot of the two curves to see it's interactions
ggplotly(
  ggplot() +
    geom_line(aes(x = df_fake_g$iteration, y = df_fake_g$ti), color = 2) +
    # geom_hline(yintercept = hot_temp, linetype="dashed", color = 2) +
    # geom_point(aes(x = ((-h1+1+setback-hours_heating) + (time_heating-60)/60), y = hot_temp), size = 5, color = 2, shape = 4, stroke = 1) +
    geom_line(aes(x = df_fake_cold_g$iteration, y = df_fake_cold_g$ti), color = 4, linetype = "dotted") +
    geom_line(aes(x = c(0, 0, setback-1, setback, setback, setback+1), y = c(hot_temp, cold_temp, cold_temp, cold_temp, hot_temp, hot_temp)),
              linetype = "dashed") +
    # geom_hline(yintercept = cold_temp, linetype="dashed", color = 4) +
    # geom_point(aes(x = ((-c1+1) + (time_ff-60)/60), y = cold_temp), size = 5, color = 4, shape = 4, stroke = 1) +
    # geom_vline(xintercept = setback, linetype = "dashed", show.legend = T) +
    xlab("Time[hours]") + xlim(0, setback+1) + ylab("Ti[ºC]") + ylim(cold_temp-0, hot_temp+0.5) 
    #theme(legend.box = "vertical", legend.position = "bottom") + labs(colour = color) +
    # theme(legend.text	= c("heateing curve", "free floating curve",))
)

#Figure for paper
l_size = 1.2
ggplot() +
  geom_line(aes(x = df_fake_g$iteration+0.4, y = df_fake_g$ti, color = "Heating", linetype = "Heating"), size = l_size) +
  geom_line(aes(x = df_fake_cold_g$iteration, y = df_fake_cold_g$ti, color = "Free floating", linetype = "Free floating"), size = l_size) +
  geom_line(aes(x = c(0, 0, setback-1, setback, setback, setback+1), y = c(hot_temp, cold_temp, cold_temp, cold_temp, hot_temp, hot_temp), 
                color = "Min. temp. setpoint", linetype = "Min. temp. setpoint"), size = l_size) +
  xlab("Time[hours]") + scale_x_continuous(breaks = seq(from = 0, to = 12, by = 2), limits = c(0, setback+1)) + # + xlim(0, setback+2)
  ylab(expression("T"^"i"~"[ºC]")) + ylim(cold_temp-0, hot_temp+0.3) + #ylab("Ti[ºC]")
  scale_color_manual(values = c("Free floating" = "blue", "Heating" = "red", "Min. temp. setpoint" = "grey50"), name = "A") +
  scale_linetype_manual(values = c("Free floating" = "dotted", "Heating" = "solid", "Min. temp. setpoint" = "dashed"), name = "A") +
  geom_segment(aes(x = 7.5, y = 18.85, xend = setback-0.1, yend = 18.85), size = 1.3, arrow = arrow(length = unit(0.25, "cm"), ends = "both")) +
  annotate("text", x=8.9, y=19.1, label= expression("h"["cross"]), size = 6) +
  # scale_linetype_manual(values = c("dotted", "solid", "dashed")) +
  theme_classic()+
  theme(legend.title = element_blank(), legend.position = c(0.65,0.25), text = element_text(size=14, colour = "black"),
        legend.key.width=unit(2, "line"), 
        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm"), size=14, color="black"),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm"), size=14, color="black")
        )
#legend.text = element_text(size = 13)


setback <- 20:1
horizon_optim <- 5
hours_heating <- 10
hours_heating_from_cooling <- 12
min_horizon <- {}
for (i in 1:length(setback)) {
  min_horizon[i] <- max(horizon_optim, min(setback[i], horizon_optim+hours_heating, hours_heating_from_cooling + horizon_optim))
}

ggplot() +
  geom_line(aes(x = 1:20, y = min_horizon, color = "hop", linetype = "hop"), size = (l_size)) +
  geom_line(aes(x = 1:20, y = setback, color = "setback", linetype = "setback"), size = l_size) +
  geom_line(aes(x = 1:20, y = rep(horizon_optim,20), color = "hlag", linetype = "hlag"), size = l_size) +
  geom_line(aes(x = 1:20, y = rep(horizon_optim+hours_heating,20), color = "hlag", linetype = "hlag"), size = l_size) +
  xlab("Time in setback[h]") + #scale_x_continuous(breaks = seq(from = 0, to = 12, by = 2), limits = c(0, setback+1)) + # + xlim(0, setback+2)
  ylab(expression("horizon[h]")) + #+ ylim(cold_temp-0, hot_temp+0.3) + #ylab("Ti[ºC]")
  scale_color_manual(values = c("hop" = "black", "setback" = "red", "hlag" = "grey50"), name = "A") +
  scale_linetype_manual(values = c("hop" = "solid", "setback" = "longdash", "hlag" = "dotdash"), name = "A") +
  annotate("text", x=5, y=6, label= expression("h"["lag"]), size = 6) +
  annotate("text", x=14, y=16, label= expression("min(h"["lag"]+"h"["heat"]~", h"["lag"]+"h"["cross"]~")"), size = 6) +
  theme_classic()+
  theme(legend.title = element_blank(), legend.position = c(0.85,0.55), text = element_text(size=14, colour = "black"),
        legend.key.width=unit(3, "line"), 
        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm"), size=14, color="black"),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm"), size=14, color="black")
  )







######################################################### Compute R2 of ti#########################################################

#### R2 of the first dataset
forecast_dates_df <- df$time[as.Date(df$time) %in% val_dates]
#Forecast and storage of the results
ti_df <- {}
ti_l0_df <- {}
tsupply_df <- {}
tsupply_l0_df <- {}
cop_df <- {}
cop_l0_df <- {}
hp_cons_df <- {}
hp_cons_l0_df <- {}
ii_df <- {}
for (i in 1:length(forecast_dates_df)) {
  predv <- prediction_scenario(
    mod_q = mod_q,
    mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    mod_cop = mod_cop,
    df = df,#df,
    rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% forecast_dates_df,
    hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    ts_prediction = forecast_dates_df[i],#NULL
    horizon = horizon
  )
  ti_df <- c(ti_df, predv$ti) #vector of the 12h real values in each iteration
  ti_l0_df <- c(ti_l0_df, predv$ti_l0) #vector of the 12h forecasts in each iteration
  tsupply_df <- c(tsupply_df, predv$tsupply)
  tsupply_l0_df <- c(tsupply_l0_df, predv$tsupply_l0)
  cop_df <- c(cop_df, predv$hp_cop)
  cop_l0_df <- c(cop_l0_df, predv$hp_cop_l0)
  hp_cons_df <- c(hp_cons_df, predv$hp_cons)
  hp_cons_l0_df <- c(hp_cons_l0_df, predv$hp_cons_l0)
  ii_df <- c(ii_df,1:nrow(predv))
}
#Compute R2
R2_df_ti <- 1-sum((ti_df-ti_l0_df)^2)/sum((ti_df-mean(ti_df))^2)
R2_df_ti
error_ti <- ti_df-ti_l0_df
max(abs(error_ti))
CVRMSE_ti <- sqrt(sum((ti_df-ti_l0_df)^2)/length(ti_df))/mean(ti_df)*100
CVRMSE_ti

R2_df_tsup <- 1-sum((tsupply_df-tsupply_l0_df)^2)/sum((tsupply_df-mean(tsupply_df))^2)
R2_df_tsup
error_tsup <- tsupply_df-tsupply_l0_df
max(abs(error_tsup))
CVRMSE_tsup <- sqrt(sum((tsupply_df-tsupply_l0_df)^2)/length(tsupply_df))/mean(tsupply_df)*100
CVRMSE_tsup

R2_df_cop <- 1-sum((cop_df-cop_l0_df)^2)/sum((cop_df-mean(cop_df))^2)
R2_df_cop
error_cop <- cop_df-cop_l0_df
max(abs(error_cop))
CVRMSE_cop <- sqrt(sum((cop_df-cop_l0_df)^2)/length(cop_df))/mean(cop_df)*100
CVRMSE_cop

R2_df_hp_cons <- 1-sum((hp_cons_df-hp_cons_l0_df)^2)/sum((hp_cons_df-mean(hp_cons_df))^2)
R2_df_hp_cons
error_hp_cons <- hp_cons_df-hp_cons_l0_df
max(abs(error_hp_cons))
CVRMSE_hp_cons <- sqrt(sum((hp_cons_df-hp_cons_l0_df)^2)/length(hp_cons_df))/mean(hp_cons_df)*100
CVRMSE_hp_cons


cop_df_MAPE <- cop_df
cop_df_MAPE[cop_df_MAPE==0] <- NA
cop_l0_df_MAPE <- cop_l0_df
cop_l0_df_MAPE[cop_l0_df_MAPE==0] <- NA
hp_cons_df_MAPE <- hp_cons_df
hp_cons_df_MAPE[hp_cons_df_MAPE==0] <- NA
hp_cons_l0_df_MAPE <- hp_cons_l0_df
hp_cons_l0_df_MAPE[hp_cons_l0_df_MAPE==0] <- NA
R2_df_ti_ <- {}
R2_df_tsup_ <- {}
R2_df_cop_ <- {}
R2_df_hp_cons_ <- {}
MAPE_df_ti_ <- {}
MAPE_df_tsup_ <- {}
MAPE_df_cop_ <- {}
MAPE_df_cons_ <- {}
CVRMSE_ti_ <- {}
CVRMSE_tsup_ <- {}
CVRMSE_cop_ <- {}
CVRMSE_hp_cons_ <- {}
for (i in 1:horizon) {
  forecast_horit <- i
  R2_df_ti_[i] <- 1-sum((ti_df[seq(forecast_horit, length(ti_df), horizon)]-ti_l0_df[seq(forecast_horit, length(ti_l0_df), horizon)])^2)/
    sum((ti_df[seq(forecast_horit, length(ti_l0_df), horizon)]-mean(ti_df[seq(forecast_horit, length(ti_l0_df), horizon)]))^2)
  R2_df_tsup_[i] <- 1-sum((tsupply_df[seq(forecast_horit, length(tsupply_df), horizon)]-tsupply_l0_df[seq(forecast_horit, length(tsupply_l0_df), horizon)])^2)/
    sum((tsupply_df[seq(forecast_horit, length(tsupply_l0_df), horizon)]-mean(tsupply_df[seq(forecast_horit, length(tsupply_l0_df), horizon)]))^2)
  R2_df_cop_[i] <- 1-sum((cop_df[seq(forecast_horit, length(cop_df), horizon)]-cop_l0_df[seq(forecast_horit, length(cop_l0_df), horizon)])^2)/
    sum((cop_df[seq(forecast_horit, length(cop_l0_df), horizon)]-mean(cop_df[seq(forecast_horit, length(cop_l0_df), horizon)]))^2)
  R2_df_hp_cons_[i] <- 1-sum((hp_cons_df[seq(forecast_horit, length(hp_cons_df), horizon)]-hp_cons_l0_df[seq(forecast_horit, length(hp_cons_l0_df), horizon)])^2)/
    sum((hp_cons_df[seq(forecast_horit, length(hp_cons_l0_df), horizon)]-mean(hp_cons_df[seq(forecast_horit, length(hp_cons_l0_df), horizon)]))^2)
  
  
  MAPE_df_ti_[i] <- sum(abs(ti_df[seq(forecast_horit, length(ti_df), horizon)]-ti_l0_df[seq(forecast_horit, length(ti_l0_df), horizon)])/
                          abs(ti_df[seq(forecast_horit, length(ti_df), horizon)]))/length(ti_df[seq(forecast_horit, length(ti_df), horizon)])
  MAPE_df_tsup_[i] <- sum(abs(tsupply_df[seq(forecast_horit, length(tsupply_df), horizon)]-tsupply_l0_df[seq(forecast_horit, length(tsupply_l0_df), horizon)])/
                            abs(tsupply_df[seq(forecast_horit, length(tsupply_df), horizon)]))/length(tsupply_df[seq(forecast_horit, length(tsupply_df), horizon)])
  
  MAPE_df_cop_[i] <- sum(abs(cop_df_MAPE[seq(forecast_horit, length(cop_df), horizon)]-cop_l0_df_MAPE[seq(forecast_horit, length(cop_l0_df), horizon)])/
                            abs(cop_df_MAPE[seq(forecast_horit, length(cop_df), horizon)]), na.rm = T)/sum(!(is.na(cop_df_MAPE[seq(forecast_horit, length(cop_df), horizon)])))
  MAPE_df_cons_[i] <- sum(abs(hp_cons_df_MAPE[seq(forecast_horit, length(hp_cons_df), horizon)]-hp_cons_l0_df_MAPE[seq(forecast_horit, length(hp_cons_l0_df), horizon)])/
                           abs(hp_cons_df_MAPE[seq(forecast_horit, length(hp_cons_df), horizon)]), na.rm = T)/sum(!(is.na(hp_cons_df_MAPE[seq(forecast_horit, length(hp_cons_df), horizon)])))
  
  sqrt(sum((hp_cons_df-hp_cons_l0_df)^2)/length(hp_cons_df))/mean(hp_cons_df)*100
  
  CVRMSE_ti_[i] <- sqrt(sum((ti_df[seq(forecast_horit, length(ti_df), horizon)]-ti_l0_df[seq(forecast_horit, length(ti_l0_df), horizon)])^2)/length(ti_df[seq(forecast_horit, length(ti_df), horizon)]))/mean(ti_df[seq(forecast_horit, length(ti_df), horizon)])*100
  CVRMSE_tsup_[i] <- sqrt(sum((tsupply_df[seq(forecast_horit, length(tsupply_df), horizon)]-tsupply_l0_df[seq(forecast_horit, length(tsupply_l0_df), horizon)])^2)/length(tsupply_df[seq(forecast_horit, length(tsupply_df), horizon)]))/mean(tsupply_df[seq(forecast_horit, length(tsupply_df), horizon)])*100
  CVRMSE_cop_[i] <- sqrt(sum((cop_df[seq(forecast_horit, length(cop_df), horizon)]-cop_l0_df[seq(forecast_horit, length(cop_l0_df), horizon)])^2)/length(cop_df[seq(forecast_horit, length(cop_df), horizon)]))/mean(cop_df[seq(forecast_horit, length(cop_df), horizon)])*100
  CVRMSE_hp_cons_[i] <- sqrt(sum((hp_cons_df[seq(forecast_horit, length(hp_cons_df), horizon)]-hp_cons_l0_df[seq(forecast_horit, length(hp_cons_l0_df), horizon)])^2)/length(hp_cons_df[seq(forecast_horit, length(hp_cons_df), horizon)]))/mean(hp_cons_df[seq(forecast_horit, length(hp_cons_df), horizon)])*100
}

R2_df_ti_
mean(R2_df_ti_)

R2_df_tsup_
mean(R2_df_tsup_)

R2_df_cop_
mean(R2_df_cop_)

R2_df_hp_cons_
mean(R2_df_hp_cons_)

MAPE_df_ti_
mean(MAPE_df_ti_)
sum(abs(ti_df-ti_l0_df)/abs(ti_df))/length(ti_df)
max(MAPE_df_ti_)

MAPE_df_tsup_
mean(MAPE_df_tsup_)
sum(abs(tsupply_df-tsupply_l0_df)/abs(tsupply_df))/length(tsupply_df)
max(MAPE_df_tsup_)

MAPE_df_cop_
mean(MAPE_df_cop_)
sum(abs(cop_df_MAPE-cop_l0_df_MAPE)/abs(cop_df_MAPE), na.rm = T)/sum(!(is.na(cop_df_MAPE)))
max(MAPE_df_cop_)


MAPE_df_cons_
mean(MAPE_df_cons_)
sum(abs(hp_cons_df_MAPE-hp_cons_l0_df_MAPE)/abs(hp_cons_df_MAPE), na.rm = T)/sum(!(is.na(hp_cons_df_MAPE)))
max(MAPE_df_cons_)

plot(MAPE_df_ti_*100)
plot(MAPE_df_tsup_*100)
plot(MAPE_df_cop_*100)
plot(MAPE_df_cons_*100)


CVRMSE_ti
CVRMSE_ti_
plot(CVRMSE_ti_)

CVRMSE_tsup
CVRMSE_tsup_
plot(CVRMSE_tsup_)

CVRMSE_cop
CVRMSE_cop_
plot(CVRMSE_cop_)

CVRMSE_hp_cons
CVRMSE_hp_cons_
plot(CVRMSE_hp_cons_)





val_dates

# ggplot(data.frame(ti_df=ti_df,ti_l0_df=ti_l0_df,ii_df=ii_df))+geom_point(aes(ti_df,ti_l0_df)) + facet_wrap(~ii_df)
# 
# iter <- 3
# ggplot(data.frame(ti_df=ti_df[seq(iter, length(ti_df), horizon)], ti_l0_df=ti_l0_df[seq(iter, length(ti_l0_df), horizon)], ii_df=ii_df[seq(iter, length(ii_df), horizon)])) +
#   geom_point(aes(ti_df,ti_l0_df))

ti_df_1 <- ti_df[seq(1, length(ti_df), horizon)]
ti_l0_df_1 <- ti_l0_df[seq(1, length(ti_df), horizon)]
#Check l0 is the original data
plot(ti_df_1, df$ti[df$time>=as.POSIXct(x = "2018-12-19 23:00:00 UTC", tz = "UTC") & df$time<=(val_dates[length(val_dates)] + days(1))])
max(ti_df_1 - df$ti[df$time>=as.POSIXct(x = "2018-12-19 23:00:00 UTC", tz = "UTC") & df$time<=(val_dates[length(val_dates)] + days(1))])

# plot(1:length(ti_df),ti_l0_df-ti_df)
# plot(ti_df,ti_l0_df-ti_df)

######################################################### SIMULATION INICIALIZATION #########################################################
# df_initaial <- df
df <- df_initaial
horizon <- 8

setwd("C:/Users/gerar/Nextcloud/Github/annex71_st2_common_exercices")
source("Paper_horizon_length/penalized-functions.R")

# The output should be the prediction of the optimum heat pump's status (ON/OFF) and set point temperature 
# for the next 24 hours (both parameters are saved in the vector: params_hp_tset_24h). 

# Import the "price" dataset
df_price <- read_excel_allsheets("data/electricity_price.xlsx")$Sheet1
colnames(df_price) <- c("daytime", "price")
df_price$daytime <- c(0:23)
price <- df_price$price

# source("C:/Users/gerar/Desktop/Config entrenament/df_866-df2900/functions.R")

# Initialize range for tset  
min_hp_tset <- min(df$hp_tset)
max_hp_tset <- max(df$hp_tset)# -3

# Initialize the limits in which the vector params_hp_tset_24h will live

features <- do.call(c,
                    list(
                      lapply(0:(horizon-1),
                             function(i){
                               list(levels = c("NA", as.character(seq(from = min_hp_tset, to = 34, by = 1)), 
                                               as.character(seq(from = 36, to = 40, by = 2)),
                                               as.character(seq(from = 41, to = max_hp_tset, by = 1))), class = "discrete")
                               })
                      ))

names(features) <- as.character(0:(horizon-1))

# Suggestion for the GA
params_hp_tset_24h = rep(x = "NA", times = horizon)#(x = min_hp_tset, times = horizon)
suggestions = decodeBinFromValue(values = params_hp_tset_24h, class_per_feature = mapply(function(i){i[['class']]},features), 
                                 nclasses_per_feature =  mapply(function(i){length(i[["levels"]])-1},features), 
                                 levels_per_feature = lapply(function(i){i[["levels"]]}, X = features))
# suggestions[8] <- 0

# Initialize the time to predict, for example: (took a date inside val_dates: (2019-01-23 : 2019-02-01))
time_to_predict <- as.POSIXct(x = "2018-12-29 10:00:00 UTC", tz = "UTC")#(x = "2018-12-19 23:00:00 UTC", tz = "UTC") #Time the MPC starts
time_to_predict_step <- as.POSIXct(x = "2018-12-19 14:00:00 UTC", tz = "UTC")#(x = "2018-12-19 14:00:00 UTC", tz = "UTC") #Start time of Modelica
delay <- as.numeric(difftime(time_to_predict, time_to_predict_step, units = "hours")) #Hours thet Modelica will run without MPC

#Set time to run Modelica (the same as the Python code)
time <- as.numeric(seconds(time_to_predict_step) - seconds(as.POSIXct(x = "2018-12-19 14:00:00 UTC", tz = "UTC")))+30463200 #30463200
step_size <- 600#20 #3600



#To use Python environment
use_condaenv()

py<-import("os")
py<-import("fmpy")
py<-import("sys")

source_python("C:/Users/gerar/PycharmProjects/untitled1/venv/Scripts/Run_Modelica.py")

#Load Modelica model
simulation = simulate_with_input(time)

fmu <- simulation[[1]]
vr_input1 <- simulation[[2]]
vr_input2 <- simulation[[3]]
vr_output1 <- simulation[[4]]
vr_output2 <- simulation[[5]]

######################################################### LOOP Before MPC #########################################################
# Constrains:
# max and min range (comfort bands) for ti defined based on the time of the day
# Thermal comfort band: 20-24 during the day (7:00 to 23:00)
#                       18-22 during the night (23:00 to 7:00)
ti_min = c(rep(x = 18, times = 7), rep(x = 20, times = 24-7)) 
ti_max = c(rep(x = 22, times = 7), rep(x = 24, times = 24-7)) 

#To take the forward X hours temperature boundaries/prices are needed 2 days
ti_min_3days = c(ti_min, ti_min, ti_min)
ti_max_3days = c(ti_max, ti_max, ti_max)
price_3days = c(price, price, price)

df_result <- data.frame({})

df_result_20s <- data.frame({})


#Set the first iteration valuies
Tsup_optim <- df$hp_tset[df$time==time_to_predict_step]
HPs_optim <- df$hp_status[df$time==time_to_predict_step]
Tcost <- 0

#Loop without using any MPC to run the iterations without lags
if (delay!=0) {
  for (i in 1:(delay*3600/step_size)) { #1:(delay*3600/step_size)
    # print(time)
    #Save in the hourly data and update Tsupply and HP status 
    if ((time%%3600 == 0) & i!=1) {
      #Energy consumed in time to predict to have simulated temperature at time to predict +1
      df$hp_cons[df$time==time_to_predict_step - hours(1)] = sum(df_result_20s$hp_el[(i-(3600/step_size)):(i-1)])*(step_size/3600)
      #Inside temperature at time to predict +1 due to the effect of use the energy between time to predict and time to predict +1
      df$ti[df$time == time_to_predict_step- hours(1)] = sum(df_result_20s$Temp_aver[(i-(3600/step_size)):(i-1)])/(3600/step_size)
      
      Tsup_optim <- df$hp_tset[df$time==time_to_predict_step]
      HPs_optim <- df$hp_status[df$time==time_to_predict_step]
      
      df_result <- rbind(df_result, data.frame("time_to_predict_step" = time_to_predict_step - hours(1),
                                               "time" = time - 3600,
                                               "Tsup_optim" = df$hp_tset[df$time==time_to_predict_step - hours(1)],
                                               "HPs_optim" = df$hp_status[df$time==time_to_predict_step - hours(1)], 
                                               "Temp_aver" = df$ti[df$time == time_to_predict_step - hours(1)],
                                               "hp_el" = df$hp_cons[df$time==time_to_predict_step - hours(1)],
                                               "cost" = sum(df_result_20s$cost[(i-(3600/step_size)):(i)]),
                                               "Tcost" = Tcost,
                                               "price" = df_price$price[(hour(time_to_predict_step - hours(1))+1)],
                                               "ti_min" = ti_min[(hour(time_to_predict_step - hours(1))+1)],
                                               "ti_max" = ti_max[(hour(time_to_predict_step - hours(1))+1)],
                                               "MPC_fitnessValue" = NA,
                                               "tin_Data_Driven" = NA,
                                               "hp_el_Data_Driven" = NA))
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
    # print(paste(i, output1, output2, Tcost, time))
    
    df_result_20s <- rbind(df_result_20s, data.frame("time_to_predict_step" = time_to_predict_step,
                                                     "time" = time,
                                                     "Tsup_optim" = Tsup_optim,
                                                     "HPs_optim" = HPs_optim, 
                                                     "Temp_aver" = output1, #The internal temperature is the one from the previous calculation 
                                                     "hp_el" = output2,
                                                     "cost" = cost,
                                                     "ti_min" = ti_min[(hour(time_to_predict_step)+1)],
                                                     "ti_max" = ti_max[(hour(time_to_predict_step)+1)]))
    
    #Update time for next step
    time_to_predict_step <- as.POSIXct(x = time_to_predict_step + seconds(step_size), format = '%Y-%m-%d %H:%M:%S')
    time = time + step_size
  }
}

#initialize the COP variable for the iterations in the main MPC loop
hp_cop <- df$hp_cop[df$time == time_to_predict_step - hours(1)]
hp_tsupply <- df$tsupply[df$time == time_to_predict_step - hours(1)]

# print(df_result)
#initialize fata frames to have a controll of what have it done the MPC
df_sol_time <- data.frame({})

df_sol_Tset <- data.frame({})

df_sol_Tave <- data.frame({})

df_sol_Text <- data.frame({})

df_costs <- data.frame({})

RETURN <-data.frame("predv.ti_l0" = NA,
                    "predv.hp_cons_l0" = NA)

setClass("GA", representation(fitnessValue = "numeric"))
optimization_results_MPC <- new("GA", "fitnessValue" = 0)

######################################################### MPC LOOP #########################################################
# max_time = 31676400 #Modelica max time
# n_steps =1
# max_time = time + step_size*(n_steps-1) + 10
max_time = as.numeric(seconds(as.POSIXct(x = "2019-01-13 01:00:00 UTC", tz = "UTC")) - #as.numeric(seconds(as.POSIXct(x = "2019-01-13 00:00:00 UTC", tz = "UTC"))
                        seconds(as.POSIXct(x = "2018-12-19 14:00:00 UTC", tz = "UTC"))) + 30463200

# time_to_predict_step <- as.POSIXct(x = "2019-01-02 22:00:00", tz = "UTC")

#Cluster for parallelization
# cl<-makeCluster(3,type="SOCK")#(###YOUR NUMBER OF CORES GOES HERE ###,type="SOCK")

while (time <= max_time) { #<=
  
  #Every hour update the historical data and run the GA
  if (time%%3600 == 0) {
    #Temp limits and price for each hour forecast
    ti_min = ti_min_3days[(hour(time_to_predict_step)+1):(hour(time_to_predict_step)+horizon)]
    ti_max = ti_max_3days[(hour(time_to_predict_step)+1):(hour(time_to_predict_step)+horizon)]
    price = price_3days[(hour(time_to_predict_step)+1):(hour(time_to_predict_step)+horizon)]
    
    #Update the historical data with the Modelica results applying the previous iteration
    df$hp_cons[df$time==as.POSIXct(time_to_predict_step) - hours(1)] = 
      mean(df_result_20s$hp_el[(length(df_result_20s$hp_el)-(3600/step_size)+1):(length(df_result_20s$hp_el))])
    df$ti[df$time==as.POSIXct(time_to_predict_step) - hours(1)] = 
      mean(df_result_20s$Temp_aver[(length(df_result_20s$Temp_aver)-(3600/step_size)+1):(length(df_result_20s$Temp_aver))])
    
    #Update df_result with all the data of the previous iteration
    df_result <- rbind(df_result, data.frame("time_to_predict_step" = as.POSIXct(time_to_predict_step) - hours(1),
                                             "time" = time-3600,
                                             "Tsup_optim" = df$hp_tset[df$time==as.POSIXct(time_to_predict_step) - hours(1)],
                                             "HPs_optim" = df$hp_status[df$time==as.POSIXct(time_to_predict_step) - hours(1)], 
                                             "Temp_aver" = df$ti[df$time==as.POSIXct(time_to_predict_step) - hours(1)], #sum(df_result_20s$Temp_aver[(i-(3600/step_size)):(i-1)])/(3600/step_size), 
                                             "hp_el" = df$hp_cons[df$time==as.POSIXct(time_to_predict_step) - hours(1)],
                                             "cost" = sum(df_result_20s$cost[(length(df_result_20s$cost)-(3600/step_size)+1):(length(df_result_20s$cost))]),
                                             "Tcost" = Tcost,
                                             "price" = price_3days[(hour(time_to_predict_step - hours(1))+1)],#price[1]
                                             "ti_min" = ti_min_3days[(hour(time_to_predict_step - hours(1))+1)],#ti_min[1],
                                             "ti_max" = ti_max_3days[(hour(time_to_predict_step - hours(1))+1)],#ti_max[1]
                                             "MPC_fitnessValue" = optimization_results_MPC@fitnessValue,
                                             "tin_Data_Driven" = RETURN$predv.ti_l0[1],
                                             "hp_el_Data_Driven" = RETURN$predv.hp_cons_l0[1]))
    #Summary of the previous iteration
    print(paste((time_to_predict_step) - hours(1), Tsup_optim, HPs_optim, df_result$cost[length(df_result$cost)], 
                df_result$Tcost[length(df_result$Tcost)], time))
    print(paste("ti_actual",df$ti[df$time == time_to_predict_step - hours(1)], " forecasted temp", RETURN$predv.ti_l0[1]))
    print(paste("hp_el", df$hp_cons[df$time == time_to_predict_step - hours(1)],"hp_el forecasted", RETURN$predv.hp_cons_l0[1]))
    
    ## Uncomment to run internally the GA
    # nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features)))
    # class_per_feature = mapply(function(i){i[['class']]},features)
    # nclasses_per_feature =  mapply(function(i){length(i[["levels"]])-1},features)
    # levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
    # names_per_feature = names(features)
    # time_to_predict = time_to_predict_step
    # params_hp_tset_24h <- c(rep("42",horizon))

    
    # system.time(
    #Compute GA
    optimization_results_MPC <- suppressMessages(
      ga(
        type = "binary",
        fitness = optimizer_MPC,
        nBits = sum(mapply(function(x) { nchar(toBin(length(x[["levels"]])-1)) },features)),
        class_per_feature = mapply(function(i){i[['class']]},features),
        nclasses_per_feature =  mapply(function(i){length(i[["levels"]])-1},features),
        levels_per_feature = lapply(function(i){i[["levels"]]}, X = features),
        names_per_feature = names(features),
        time_to_predict = as.POSIXct(time_to_predict_step),
        params = params,
        mod_q = mod_q,
        mod_ti = mod_ti,
        mod_tsupply = mod_tsupply,
        mod_cop = mod_cop,
        ti_min = ti_min,
        ti_max = ti_max,
        price = price,
        selection = gabin_tourSelection,
        mutation = gabin_raMutation,
        crossover = partial(bee_uCrossover, nclasses_per_feature =  mapply(function(i){length(i[["levels"]])-1},features)),
        df = df,
        horizon = horizon,
        suggestions = suggestions,
        keepBest = TRUE,
        popSize = 400, #64
        maxiter = 20, #20
        monitor = gaMonitor2,
        parallel = "snow", #cl "snow"
        elitism = 0.05,#0.08
        pcrossover = 0.8,
        pmutation = 0.20, #0.05
        maxFitness = -(sum(0)+(sum((price/1000000)*4000)))*(1+0)#if the fitness value is the cost function
                                                                #with zero consumption and zero penalty it stops the GA
      ))
    # )

    #Get the solution of the optimization
    params_hp_tset_24h_opt <- as.numeric(decodeValueFromBin(binary_representation = optimization_results_MPC@solution[1,],
                                                            class_per_feature = mapply(function(i){i[['class']]},features),
                                                            nclasses_per_feature = mapply(function(i){length(i[["levels"]])-1},features),
                                                            levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
                                                            ))
    print("Optimized tsupply:")
    print(params_hp_tset_24h_opt)
    
    #Update the suggestion for the next iteration using the previous solution
    params_hp_tset_sugg = c(params_hp_tset_24h_opt[-1], NA)
    for (i in 1:length(params_hp_tset_sugg)) {if (is.na(params_hp_tset_sugg[i])) {params_hp_tset_sugg[i] <- "NA"}}
    suggestions = decodeBinFromValue(values = params_hp_tset_sugg, class_per_feature = mapply(function(i){i[['class']]},features),
                                     nclasses_per_feature =  mapply(function(i){length(i[["levels"]])-1},features),
                                     levels_per_feature = lapply(function(i){i[["levels"]]}, X = features))
    
    ## Uncomment to run internally the optimizer MPC
    # X = optimization_results_MPC@solution[1,]
    # class_per_feature = mapply(function(i){i[['class']]},features)
    # nclasses_per_feature = mapply(function(i){length(i[["levels"]])-1},features)
    # names_per_feature = names(features)
    # levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
    # time_to_predict = time_to_predict_step
    # post_analysis=T
    
    #Compute the GA solution to take the forecasted results
    RETURN <- optimizer_MPC(X = optimization_results_MPC@solution[1,],
                            class_per_feature = mapply(function(i){i[['class']]},features),
                            nclasses_per_feature = mapply(function(i){length(i[["levels"]])-1},features),
                            names_per_feature = names(features),
                            levels_per_feature = lapply(function(i){i[["levels"]]}, X = features),
                            df = df,
                            mod_q = mod_q,
                            mod_ti = mod_ti,
                            mod_tsupply = mod_tsupply,
                            mod_cop = mod_cop,
                            ti_min = ti_min,
                            ti_max = ti_max,
                            price = price,
                            time_to_predict = time_to_predict_step,
                            params = params,
                            horizon = horizon,
                            post_analysis=T)
    
    penalty <- RETURN$penalty
    iteration_cost <- RETURN$iteration_cost
    df_sol_time <- rbind(df_sol_time, data.frame("time_to_predict_step" = time_to_predict_step))
    df_sol_Tset <- rbind(df_sol_Tset, data.frame(t(params_hp_tset_24h_opt)))
    df_sol_Tave <- rbind(df_sol_Tave, data.frame(t(RETURN$predv.ti_l0)))
    df_sol_Text <- rbind(df_sol_Text, data.frame(t(RETURN$predv.te_l0)))
    df_costs <- rbind(df_costs, data.frame("penalty" = penalty[1],
                                           "iteration_cost" = iteration_cost[1]))
    
    #Update the Supply temperautre and the Heat Pump status from the optimization
    Tsup_optim <- params_hp_tset_24h_opt[1]
    HPs_optim = 1.0
    if (is.na(Tsup_optim)) {
      Tsup_optim = 0.0
      HPs_optim = 0.0
    }
    
    #Update the historical data with the applied setpoints
    df$hp_status[df$time==as.POSIXct(time_to_predict_step)] <- HPs_optim
    df$hp_tset[df$time==as.POSIXct(time_to_predict_step)] <- Tsup_optim
    #Update the historical data with the forecasted COP and tsuppy (we don't have feedback from modelica of this values)
    df$hp_cop[df$time==as.POSIXct(time_to_predict_step)] <- RETURN$predv.hp_cop_l0[1]
    df$tsupply[df$time==as.POSIXct(time_to_predict_step)] <- RETURN$predv.tsupply_l0[1]
  }
  
  #Modelica simulation
  step <- Iteration(fmu, vr_input1, vr_input2, vr_output1, vr_output2, time, step_size, Tsup_optim, HPs_optim)
          #Iteration return -> fmu, input1, input2, output1(Tav), output2(hp_el)
  
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
                                                   "Tsup_optim" = Tsup_optim,
                                                   "HPs_optim" = HPs_optim, 
                                                   "Temp_aver" = output1, #The internal temperature is the one from the previous calculation 
                                                   "hp_el" = output2,
                                                   "cost" = cost,
                                                   "ti_min" = ti_min_3days[(hour(time_to_predict_step)+1)],
                                                   "ti_max" = ti_max_3days[(hour(time_to_predict_step)+1)]))
  
  #Update time for next step
  time = time + step_size
  time_to_predict_step <- time_to_predict_step + seconds(step_size)
  
  # print(df_result)
}
# stopCluster(cl)

for (i in 1:ncol(df_sol_Tave)) {
  names(df_sol_Tave)[i] <- sprintf("Tave-%02i",i)
  names(df_sol_Tset)[i] <- sprintf("Tset-%02i",i)
  names(df_sol_Text)[i] <- sprintf("Text-%02i",i)
}
df_sol <- data.frame(df_sol_time, df_sol_Tave, df_sol_Tset, df_sol_Text, df_costs)

Total_cost_BM <- df_result$Tcost[length(df_result$Tcost)-1]-df_result$Tcost[df_result$time_to_predict_step == as.POSIXct(x = "2018-12-19 18:00:00 UTC", tz = "UTC")]
Total_cost_BM

df_eval <- df_result[df_result$time_to_predict_step >= as.POSIXct(x = "2018-12-30 00:00:00 UTC", tz = "UTC") & df_result$time_to_predict_step <= as.POSIXct(x = "2019-01-12 23:00:00 UTC", tz = "UTC"),]
sum(df_eval$cost, na.rm = T)
# END_fmu(fmu)


plot(df_result$Temp_aver[1:length(df_result$Temp_aver)-1]-df_result$tin_Data_Driven[2:length(df_result$Temp_aver)])

######################################################### Disconfort #########################################################
#In steps of 1h
df_disconfort <- df_eval#df_result#[df_result$time_to_predict_step >= as.POSIXct(x = "2018-12-19 19:00:00 UTC", tz = "UTC"),]
disconfort <- {}
for (i in 1:length(df_disconfort$time_to_predict_step)) {
  if (df_disconfort$ti_min[i] > df_disconfort$Temp_aver[i]) {
    x <- (df_disconfort$ti_min[i]-df_disconfort$Temp_aver[i])
    disconfort[i] <- x #(-exp(lambda*x)/(x-delta_limit))-(1/delta_limit)
  } else {
    disconfort[i] <- 0
  }
}
df_eval$disconfort <- disconfort
df_eval$te <- df$te[df$time >= as.POSIXct(x = "2018-12-30 00:00:00 UTC", tz = "UTC") & df$time <= as.POSIXct(x = "2019-01-12 23:00:00 UTC", tz = "UTC")]

max(disconfort)
sum(disconfort)
hist(disconfort)
disconfort <- disconfort[disconfort != 0]
hist(disconfort, breaks = c(seq(from = 0, to = 0.16, by= 0.02)))#, breaks = c(seq(from = 0, to = 0.25, by= 0.02)))
sum(df_disconfort$hp_el*df_disconfort$price/1000000)

plot(df_eval$time_to_predict_step,df_eval$Temp_aver-df_eval$tin_Data_Driven)
cpgram(df_eval$Temp_aver-df_eval$tin_Data_Driven)
acf(df_eval$Temp_aver-df_eval$tin_Data_Driven)
df_fit <- df_eval[df_eval$MPC_fitnessValue < -2,]
plot(df_fit$time_to_predict_step, df_fit$MPC_fitnessValue)
cpgram(df_eval$hp_el-df_eval$hp_el_Data_Driven)
acf(df_eval$hp_el-df_eval$hp_el_Data_Driven)


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
sum(df_disconfort_20s$hp_el*price_3days[hour(df_disconfort_20s$time_to_predict_step)+1]/1000000)*20/3600
Total_cost_20s <- sum(df_disconfort_20s$cost)
sum(df_disconfort_20s$hp_el[length(df_disconfort_20s$hp_el)-180:length(df_disconfort_20s$hp_el)]*
      price_3days[hour(df_disconfort_20s$time_to_predict_step[length(df_disconfort_20s$hp_el)-180:length(df_disconfort_20s$hp_el)])+1]/1000000)*20/3600

######################################################### PLOTS #########################################################

df_plot <- df_result[, c("time_to_predict_step", "Tsup_optim", "HPs_optim", "Temp_aver", "hp_el", "cost", "price")]
df_plot <- melt(df_plot, "time_to_predict_step") 
df_plot$variable <- as.factor(df_plot$variable)

ggplot(df_plot) + 
  geom_line(aes(x = time_to_predict_step, y = value)) +
  facet_wrap(~as.factor(variable), ncol = 1, scales = "free_y")

ggplotly(
  ggplot() +
    geom_line(aes(x = df_eval$time_to_predict_step, y = df_eval$Temp_aver)) +
    geom_line(aes(x = df_eval$time_to_predict_step, y = df_eval$ti_min, color = "red")) +
    geom_line(aes(x = df_eval$time_to_predict_step, y = df_eval$ti_max, color = "blue")) +
    geom_line(aes(x = df_eval$time_to_predict_step, y = df_eval$price/3,  color = "green")) +
    geom_point(aes(x = df_eval$time_to_predict_step, y = df_eval$price/3)) +
    geom_point(aes(df_eval$time_to_predict_step, ifelse(df_eval$HPs_optim>0,df_eval$price/3,NA), color = "green")) +
    geom_line(aes(df_eval$time_to_predict_step, (df_eval$Tsup_optim/4)+15, color = "grey"))
    # geom_line(aes(df_eval$time_to_predict_step, (df_eval$cost*50)+15, color = "cyan"))
)

#Grafica Tin + boundaries
l_size = 1.2
ggplot(df_eval[df_eval$time_to_predict_step >= as.POSIXct(x = "2019-01-04 00:00:00 UTC", tz = "UTC") &
                 df_eval$time_to_predict_step <= as.POSIXct(x = "2019-01-07 00:00:00 UTC", tz = "UTC"),]) +
  geom_line(aes(x = time_to_predict_step, y = Temp_aver, color = "Tin", linetype = "Tin"), size = l_size) +
  geom_line(aes(x = time_to_predict_step, y = ti_min, color = "Tmin", linetype = "Tmin"), size = l_size) +
  geom_line(aes(x = time_to_predict_step, y = ti_max, color = "Tmax", linetype = "Tmax"), size = l_size) +
  xlab("Time[hours]") + 
  scale_x_datetime(labels = date_format("%m/%d %H:%M"), breaks = seq(from = as.POSIXct(x = "2019-01-04 00:00:00 UTC", tz = "UTC"),
                                                                     to = as.POSIXct(x = "2019-01-07 00:00:00 UTC", tz = "UTC"),
                                                                     by = 3600*6)) +
  ylab(expression("T"~"[ºC]")) +
  scale_color_manual(values = c("Tin" = "black", "Tmin" = "blue", "Tmax" = "red"), name = "A") +
  scale_linetype_manual(values = c("Tin" = "solid", "Tmin" = "dotted", "Tmax" = "dashed"), name = "A") +
    # scale_linetype_manual(values = c("dotted", "solid", "dashed")) +
  theme_classic()+
  theme(legend.title = element_blank(), legend.position = c(0.85,0.65), text = element_text(size=14, colour = "black"),
        legend.key.width=unit(2, "line"), 
        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm"), size=14, color="black", angle = 90),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 3), "mm"), size=14, color="black"),
        axis.text.y.right = element_text(margin = unit(c(t = 0, r = 3, b = 0, l = 3.5), "mm"), size=14, color="black"))

#Grafica Tin + boundaries + energy cost
ggplot(df_eval[df_eval$time_to_predict_step >= as.POSIXct(x = "2019-01-04 00:00:00 UTC", tz = "UTC") &
                 df_eval$time_to_predict_step <= as.POSIXct(x = "2019-01-07 00:00:00 UTC", tz = "UTC"),]) +
  geom_line(aes(x = time_to_predict_step, y = price/5 + 8, color = "Energy price", linetype = "Energy price"), size = l_size) +
  geom_line(aes(x = time_to_predict_step, y = ti_min, color = "Tmin", linetype = "Tmin"), size = l_size) +
  geom_line(aes(x = time_to_predict_step, y = ti_max, color = "Tmax", linetype = "Tmax"), size = l_size) +
  geom_line(aes(x = time_to_predict_step, y = Temp_aver, color = "Tin", linetype = "Tin"), size = l_size) +
  xlab("Time[hours]") + 
  scale_x_datetime(labels = date_format("%m/%d %H:%M"), breaks = seq(from = as.POSIXct(x = "2019-01-04 00:00:00 UTC", tz = "UTC"),
                                                                     to = as.POSIXct(x = "2019-01-07 00:00:00 UTC", tz = "UTC"),
                                                                     by = 3600*6)) +
  ylab(expression("T"~"[ºC]")) +
  scale_y_continuous(sec.axis = sec_axis(~ .*5 - (8*5), name = "Energy cost [Eur/MW]", breaks = seq(from=40,to=80,by=10), labels = seq(from=40,to=80,by=10)),
                     position = "left", guide = guide_axis(check.overlap = T)) +
  scale_color_manual(values = c("Tin" = "black", "Tmin" = "blue", "Tmax" = "red", "Energy price" = "grey"), name = "A") +
  scale_linetype_manual(values = c("Tin" = "solid", "Tmin" = "twodash", "Tmax" = "dashed", "Energy price" = "solid"), name = "A") +
  theme_classic()+
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        text = element_text(size=14, colour = "black"),
        legend.key.width=unit(3, "line"), 
        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm"), size=14, color="black", angle = 90),
        axis.text.y.left = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 3), "mm"), size=14, color="black"),
        axis.text.y.right = element_text(margin = unit(c(t = 0, r = 3, b = 0, l = 3.5), "mm"), size=14, color="black"))


#Electricity price + iteration electricity cost + consumption points
l_size = 1.2
ggplot(df_eval[df_eval$time_to_predict_step >= as.POSIXct(x = "2019-01-04 00:00:00 UTC", tz = "UTC") &
                 df_eval$time_to_predict_step <= as.POSIXct(x = "2019-01-07 00:00:00 UTC", tz = "UTC"),]) +
  geom_line(aes(x = time_to_predict_step, y = price,  color = "Electricity price"), size = l_size) +
  # geom_point(aes(x = time_to_predict_step, y = price, color = "price_point_b"), size = 3) +
  geom_point(aes(time_to_predict_step, ifelse(HPs_optim>0,price,NA), color = "Consumption point"), size = 3) +
  geom_line(aes(time_to_predict_step, (cost*150) + 40, color = "Iteration cost"), size = l_size) +
  xlab("Time") + 
  scale_x_datetime(labels = date_format("%m/%d %H:%M"), breaks = seq(from = as.POSIXct(x = "2019-01-04 00:00:00 UTC", tz = "UTC"),
                                                                     to = as.POSIXct(x = "2019-01-07 00:00:00 UTC", tz = "UTC"),
                                                                     by = 3600*6)) +
  ylab("Electricity price [Eur/MWh]") +
  scale_y_continuous(sec.axis = sec_axis(~ ./150 - 40/150, name = "Iteration cost [Eur]"), position = "left", guide = guide_axis(check.overlap = T)) +
  scale_color_manual(values = c("Electricity priece" = "cyan4", "price_point_b" = "blue", "Consumption point" = "red",
                                "Iteration cost" = "black"), name = "ABC") +
  
  theme_classic() +
  theme(legend.title = element_blank(), legend.position = "bottom", text = element_text(size=14, colour = "black"),
        legend.key.width=unit(2, "line"), 
        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm"), size=14, color="black", angle = 90),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 3), "mm"), size=14, color="black"),
        axis.text.y.right = element_text(margin = unit(c(t = 0, r = 3, b = 0, l = 3.5), "mm"), size=14, color="black"))




l_size = 1.2
ggplot() +
  geom_line(aes(x = df_fake_g$iteration+0.4, y = df_fake_g$ti, color = "Heating", linetype = "Heating"), size = l_size) +
  geom_line(aes(x = df_fake_cold_g$iteration, y = df_fake_cold_g$ti, color = "Free floating", linetype = "Free floating"), size = l_size) +
  geom_line(aes(x = c(0, 0, setback-1, setback, setback, setback+1), y = c(hot_temp, cold_temp, cold_temp, cold_temp, hot_temp, hot_temp), 
                color = "Min. temp. setpoint", linetype = "Min. temp. setpoint"), size = l_size) +
  xlab("Time[hours]") + scale_x_continuous(breaks = seq(from = 0, to = 12, by = 2), limits = c(0, setback+1)) + # + xlim(0, setback+2)
  ylab(expression("T"^"i"~"[ºC]")) + ylim(cold_temp-0, hot_temp+0.3) + #ylab("Ti[ºC]")
  scale_color_manual(values = c("Free floating" = "blue", "Heating" = "red", "Min. temp. setpoint" = "grey50"), name = "A") +
  scale_linetype_manual(values = c("Free floating" = "dotted", "Heating" = "solid", "Min. temp. setpoint" = "dashed"), name = "A") +
  geom_segment(aes(x = 7.5, y = 18.85, xend = setback-0.1, yend = 18.85), size = 1.3, arrow = arrow(length = unit(0.25, "cm"), ends = "both")) +
  annotate("text", x=8.9, y=19.1, label= expression("h"["cross"]), size = 6) +
  # scale_linetype_manual(values = c("dotted", "solid", "dashed")) +
  theme_classic()+
  theme(legend.title = element_blank(), legend.position = c(0.65,0.25), text = element_text(size=14, colour = "black"),
        legend.key.width=unit(2, "line"), 
        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin = unit(c(t = 3.5, r = 0, b = 0, l = 0), "mm"), size=14, color="black"),
        axis.text.y = element_text(margin = unit(c(t = 0, r = 3.5, b = 0, l = 0), "mm"), size=14, color="black")
  )

















cbPalette <- c("black", "red", "yellow")
ggplotly(
  ggplot() +
    geom_point(aes(x = df_eval$time_to_predict_step, y = df_eval$MPC_fitnessValue/1e8, color = "A")) +
    geom_point(aes(x = df_eval$time_to_predict_step, y = -df_eval$disconfort, color = "B")) +
    # geom_line(aes(x = df_eval$time_to_predict_step, y = (df_eval$Temp_aver-df_eval$tin_Data_Driven), color = "C")) +
    # geom_line(aes(x = df_eval$time_to_predict_step, y = (df_eval$te/10), color = "D")) +
    scale_colour_manual(values=cbPalette))

time_dis <- 0
for (i in 1:length(df_eval$disconfort)) {
 if (df_eval$disconfort[i]>0) {time_dis <- time_dis +1}
}
time_dis

cpgram(df_eval$Temp_aver-df_eval$tin_Data_Driven)
acf(df_eval$Temp_aver-df_eval$tin_Data_Driven)
cpgram(df_eval$hp_el-df_eval$hp_el_Data_Driven)
acf(df_eval$hp_el-df_eval$hp_el_Data_Driven)
# penalty <- zoo::rollapply(disconfort, width=12, align="left",partial=T,FUN=function(x){sum(x,na.rm=T)})
df_car <- df_eval[df_eval$price==77.8,]
df_car$hp_el
sum(df_car$hp_el,na.rm = T)

library(plotly)
ggplotly(
  ggplot(df) +
    aes(time,ti) + geom_line(),
  dynamicTicks = "x"
    )

ggplotly(
  ggplot(df) +
    aes(time,BHI) + geom_line(),
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
    aes(time,air_h) + geom_line(),
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
writeMat("Results_in_matlab/R2_data.mat",
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

writeMat("Results_in_matlab/MPC_data.mat", #Need:time(real), time modelica, Tin, hp_el, electric price in intervals of 20s and hourly
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


plot(df_eval$Temp_aver - df_eval$tin_Data_Driven)
plot(df_eval$time_to_predict_step[1:17], ifelse(df_eval$hp_el[1:17] == 0,0,(df_eval$hp_el[1:17] - df_eval$hp_el_Data_Driven[1:17])/df_eval$hp_el[1:17]))

