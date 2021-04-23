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
                        ))
                       # "vent"= weather$WindSpeed
)
# df_house$extra_heat <- df_house$hg + df_house$air_h

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
df_house$vent_status <- kmeans(df_house$vent, 3)$cluster
# df_house$vent <- house2$total_VFR[1:1053]
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

######################################################### SIMULATION INICIALIZATION #########################################################

setwd("C:/Users/gerar/Nextcloud/Github/annex71_st2_common_exercices")
source("Paper_horizon_length/penalized-functions.R")

# The output should be the prediction of the optimum heat pump's status (ON/OFF) and set point temperature 
# for the next 24 hours (both parameters are saved in the vector: params_hp_tset_24h). 

# Import the "price" dataset
df_price <- read_excel_allsheets("data/electricity_price.xlsx")$Sheet1
colnames(df_price) <- c("daytime", "price")
df_price$daytime <- c(0:23)
price <- df_price$price


# Initialize the limits in which the vector params_hp_tset_24h will live

# suggestions[8] <- 0
# Initialize the time to predict, for example: (took a date inside val_dates: (2019-01-23 : 2019-02-01))
time_to_predict <- as.POSIXct(x = "2019-01-13 00:00:00 UTC", tz = "UTC")#(x = "2018-12-19 23:00:00 UTC", tz = "UTC") #Time the MPC starts
time_to_predict_step <- as.POSIXct(x = "2018-12-29 00:00:00 UTC", tz = "UTC")#(x = "2018-12-19 14:00:00 UTC", tz = "UTC") #Start time of Modelica
delay <- as.numeric(difftime(time_to_predict, time_to_predict_step, units = "hours")) #Hours thet Modelica will run without MPC

#Set time to run Modelica (the same as the Python code)
time <- as.numeric(seconds(time_to_predict_step) - seconds(as.POSIXct(x = "2018-12-19 14:00:00 UTC", tz = "UTC")))+30463200 #30463200
step_size <- 20 #3600



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
Tsup <- 28
HPs <- 0
Taver <- 20
Tcost <- 0

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
if (delay!=0) {
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
      
      df_result$hp_el[length(df_result$hp_el)] <- sum(df_result_20s$hp_el[(i-(3600/step_size)+1):(i)])*(step_size/3600)
      df_result$cost[length(df_result$cost)] <- sum(df_result_20s$cost[(i-(3600/step_size)):(i)])
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
      print(time_to_predict_step)
    }
    
    # #Determine HPs
    # if (Taver<ti_min[(hour(time_to_predict_step)+1)]) {HPs <- 1} else {HPs <- 0}
    # 
    # # #Tsup is taken as a constant at 40ºC, timestep is low so it should work fine (for not overheat the house)
    # # Tsup <- 42
    # #Tsup is not a constant, depends linearly on the disconfort, id it's 0Kh its 28ºC and if it's 1Kh it's 42ºC
    # #the maximum temperature of Tsupply is 42ºC so avobe 1Kh is constant at 42ºC
    # if (HPs == 1) {
    #   Tsup = 32 + (42-32)*(ti_min[(hour(time_to_predict_step)+1)]-Taver)
    #   if (Tsup > 42) {Tsup <- 42}
    # } else {Tsup <- 0}
    
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
}


######################################################### MPC LOOP #########################################################
# max_time = 31676400 #Modelica max time
# n_steps =1
# max_time = time + step_size*(n_steps-1) + 10
max_time = as.numeric(seconds(as.POSIXct(x = "2019-01-13 00:00:00 UTC", tz = "UTC")) - #"2019-01-02 00:00:00 UTC"
                        seconds(as.POSIXct(x = "2018-12-19 14:00:00 UTC", tz = "UTC"))) + 30463200


Total_cost_BM <- df_result$Tcost[length(df_result$Tcost)-1]-df_result$Tcost[df_result$time_to_predict_step == as.POSIXct(x = "2018-12-19 18:00:00 UTC", tz = "UTC")]
Total_cost_BM
sum(df_result_20s$cost, na.rm = T)
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
df_disconfort_20s <- df_result_20s#[df_result_20s$time_to_predict_step >= as.POSIXct(x = "2018-12-19 19:00:00 UTC", tz = "UTC"),]
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
Total_disconfort_20s
hist(disconfort_20s)
disconfort <- disconfort_20s[disconfort_20s != 0]
hist(disconfort)
sum(df_disconfort_20s$hp_el*price[hour(df_disconfort_20s$time_to_predict_step)+1]/1000000)*20/3600
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
    geom_line(aes(x = df_result_20s$time_to_predict_step, y = df_result_20s$Temp_aver)) +
    geom_line(aes(x = df_result_20s$time_to_predict_step, y = df_result_20s$ti_min, color = "red")) +
    geom_line(aes(x = df_result_20s$time_to_predict_step, y = df_result_20s$ti_max, color = "blue")) +
    # geom_line(aes(x = df_result_20s$time_to_predict_step, y = df_result_20s$price/3,  color = "green")) +
    # geom_point(aes(x = df_result_20s$time_to_predict_step, y = df_result_20s$price/3)) +
    # geom_point(aes(df_result_20s$time_to_predict_step, ifelse(df_result_20s$HPs_optim>0,df_result_20s$price/3,NA), color = "green")) +
    geom_line(aes(df_result_20s$time_to_predict_step, (df_result_20s$Tsup_optim/4)+15, color = "grey"))
    # geom_line(aes(df_result_20s$time_to_predict_step, (df_result_20s$cost*50*100)+15, color = "cyan"))
)

# penalty <- zoo::rollapply(disconfort, width=12, align="left",partial=T,FUN=function(x){sum(x,na.rm=T)})
df_car <- df_result[df_result$price==77.8,]

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


