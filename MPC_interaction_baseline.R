
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
library(zoo)
library(R.matlab)

# library to integrate Python
library(reticulate)

######################################################### IMPORT DATA #########################################################

#Extract form the Excel file the inputs to send to Modelica 
# BM <- read_excel_allsheets("data/hystersis.xlsx")$Sheet1
BM <- read_excel_allsheets("data/hystersis2.5.xlsx")$Sheet1 #Last version of the setpoints form Matlab

# The output should be the prediction of the optimum heat pump's status (ON/OFF) and set point temperature 
# for the next 24 hours (both parameters are saved in the vector: params_hp_tset_24h). 

# Import the "price" dataset
df_price <- read_excel_allsheets("data/electricity_price.xlsx")$Sheet1
colnames(df_price) <- c("daytime", "price")
df_price$daytime <- c(0:23)
price <- df_price$price


# Constrains:
# max and min range (comfort bands) for ti defined based on the time of the day
# Thermal comfort band: 20-24 during the day (7:00 to 23:00)
#                       18-22 during the night (23:00 to 7:00)
# ti_min = c(rep(x = 18, times = 7), rep(x = 20, times = 24-7)) 
# ti_max = c(rep(x = 22, times = 7), rep(x = 24, times = 24-7)) 


# Initialize the time to predict, for example: (took a date inside val_dates: (2019-01-23 : 2019-02-01))
# time_to_predict <- as.POSIXct(x = "2018-12-19 19:00:00 UTC", tz = "UTC")
time_to_predict_step <- as.POSIXct(x = "2018-12-19 14:00:00 UTC", tz = "UTC")
# delay <- as.numeric(difftime(time_to_predict, time_to_predict_step))


#Use conda to run Python in R
use_condaenv()

#import Python libraries
py<-import("os")
py<-import("fmpy")
py<-import("sys")

#Load the Python functions to run Modelica
source_python("C:/Users/gerar/PycharmProjects/untitled1/venv/Scripts/Run_Modelica.py")

#Open the simulation and get the variables
simulation = simulate_with_input()

fmu <- simulation[[1]]
vr_input1 <- simulation[[2]]
vr_input2 <- simulation[[3]]
vr_output1 <- simulation[[4]]
vr_output2 <- simulation[[5]]

#Create the dataframe where the results will be stored
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
                        "ti_max" = {}
                        )

#Create the data frame with all the imputs of the modelica for each 20 secons and fill with the data
BM_20 <- data.frame("Time" = {},
                    "HPstatus" = {},
                     "Tsup" = {}
                     )

for (i in 1:(length(BM$Time)-1)) {
  BM_20 <- rbind(BM_20, data.frame("Time" = c(BM$Time[i],BM$Time[i]+20,BM$Time[i]+40),
                                       "HPstatus" = c(BM$u1[i], BM$u1[i], BM$u1[i]),
                                       "Tsup" = c(BM$u2[i], BM$u2[i], BM$u2[i])
                                        )
                  )
}

BM_20 <- rbind(BM_20, data.frame("Time" = BM$Time[length(BM$Time)],
                                 "HPstatus" = BM$u1[length(BM$Time)],
                                 "Tsup" = BM$u2[length(BM$Time)]
                                  )
               )

#Crate a data frame with the boundary conditions
df_ti_bond = data.frame("hour" = c(0:23), 
                       "ti_min" = c(rep(x = 18, times = 7), rep(x = 20, times = 24-7)),
                                  # c(20, rep(x = 18, times = 7), rep(x = 20, times = 24-7-1)),
                       "ti_max" = c(rep(x = 22, times = 7), rep(x = 24, times = 24-7))
                                  # c(24, rep(x = 22, times = 7), rep(x = 24, times = 24-7-1))
                       )

######################################################### Run Modleica Model #########################################################

time <- 30463200
step_size <- 20 #60
Tcost <- 0

Tin <- 20
# hp_el <- 0

for (i in 1:length(BM_20$Time)) {
  
  #do simulation with modelica
  step <- Iteration(fmu, vr_input1, vr_input2, vr_output1, vr_output2,
                    BM_20$Time[i], step_size, BM_20$Tsup[i], BM_20$HPstatus[i])
  # Iteration return -> fmu, input1, input2, output1(Tav), output2(hp_el)
  
  input1 <- step[[2]]
  input2 <- step[[3]]
  output1 <- step[[4]]
  output2 <- step[[5]]
  cost <- df_price$price[hour(time_to_predict_step)+1]/1000000*output2*(step_size/3600)
  Tcost = Tcost + cost
  print(paste(i, output1, output2, Tcost, time))
  
  df_result <- rbind(df_result, data.frame("time_to_predict_step" = time_to_predict_step,
                                           "time" = BM_20$Time[i],
                                           "Tsup_optim" = BM_20$Tsup[i],
                                           "HPs_optim" = BM_20$HPstatus[i], 
                                           "Temp_aver" = Tin, #The temperature from the model is the next iteration temperature
                                           "hp_el" = output2,
                                           "cost" = cost,
                                           "Tcost" = Tcost,
                                           "price_position (h+1)" = hour(time_to_predict_step)+1,
                                           "price" = df_price$price[(hour(time_to_predict_step)+1)],
                                           "ti_min" = df_ti_bond$ti_min[hour(time_to_predict_step)+1],
                                           "ti_max" = df_ti_bond$ti_max[hour(time_to_predict_step)+1]
                                            )
                    )
  
  Tin <- output1 #To save the computed temperature on the next iteration
  #hp_el <- output2 #To save the computed heat pump power on the next iteration
  
  #Next ietartion time
  time_to_predict_step <- as.POSIXct(x = time_to_predict_step + seconds(20), format = '%Y-%m-%d %H:%M:%S')
  time = time + step_size
}

# END_fmu(fmu)

#Total cost removing the first 6 hours
Total_cost_BM <- df_result$Tcost[length(df_result$Tcost)]-df_result$Tcost[df_result$time_to_predict_step == as.POSIXct(x = "2018-12-19 19:59:00 UTC", tz = "UTC")]
Total_cost_BM

#Print results
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


#Disconfort

df_disconfort <- df_result[df_result$time_to_predict_step > as.POSIXct(x = "2018-12-19 20:00:00 UTC", tz = "UTC"),]

disconfort <- {}
delta_limit <- 3
lambda <- 2
# x_disconfort <- {}
disconfort <- 0
for (i in 1:length(df_disconfort$time_to_predict_step)) {
  if (df_disconfort$ti_min[i] > df_disconfort$Temp_aver[i]) {
    x <- (df_disconfort$ti_min[i]-df_disconfort$Temp_aver[i])*20/3600
    disconfort[i] <- x #(-exp(lambda*x)/(x-delta_limit))-(1/delta_limit)
    # x_disconfort[i] <- x
  } else {
    disconfort[i] <- 0
    # x_disconfort[i] <- 0
  }
}
max(disconfort)
sum(disconfort)
disconfort <- disconfort[disconfort != 0]
hist(disconfort)



# penalty <- sum(zoo::rollapply(disconfort, width=60*12, align="left",partial=T,FUN=function(x){sum(x,na.rm=T)}))


#Load the results form Matlab for comparison
Tav_matlab <- readMat("Matlab Arash/Taver2.mat")
Ti_min_matlab <- readMat("Matlab Arash/Ti_min.mat")
Ti_min_raw_matlab <- readMat("Matlab Arash/Ti_min_raw.mat")


#Diference between baseline computed in modelica throught R and Matlab
diff_Taver <- df_result$Temp_aver-Tav_matlab[["Taver"]]
sum(diff_Taver)*20/3600


#Check that boundary conditions in R and Matlab matches
df_ti_min <- data.frame("Ti_min_MPC" = df_result$ti_min,
                        "Ti_min_matlab" = Ti_min_matlab[["Tlow"]],
                        "Ti_min_raw" = Ti_min_raw_matlab[["Tlow.raw"]]
                          )


#Compute the disconfort on the Matlab results
df_disconfort_matlab <- data.frame("Time" = df_result$time_to_predict_step,
                                "Temp_aver" = Tav_matlab[["Taver"]],
                                "Ti_min_matlab" = Ti_min_matlab[["Tlow"]]
                                )

df_disconfort_matlab <- df_disconfort_matlab[df_disconfort_matlab$Time > as.POSIXct(x = "2018-12-19 19:00:00 UTC", tz = "UTC"),]

disconfort_matlab <- 0
for (i in 1:length(df_disconfort_matlab$Time)) {
  if (df_disconfort$ti_min[i] > df_disconfort_matlab$Temp_aver[i]) {
    x <- (df_disconfort$ti_min[i]-df_disconfort_matlab$Temp_aver[i])*20/3600
    disconfort_matlab[i] <- x #(-exp(lambda*x)/(x-delta_limit))-(1/delta_limit)
    # x_disconfort[i] <- x
  } else {
    disconfort_matlab[i] <- 0
    # x_disconfort[i] <- 0
  }
}
sum(disconfort_matlab)

error <- df_result$Temp_aver - Tav_matlab[["Taver"]]
max(error)
sum(error*20/3600)


#electrical consumption taken into account results of each minute not in steps of 20 seconsa
elcon_cost <- 0
for (i in 1:length(df_disconfort$time)) {
  if ((i-1)%%3 ==0) {
    elcon_cost <- elcon_cost + df_disconfort$hp_el[i]*60/3600*df_disconfort$price[i]/1000000
  }
}
elcon_cost


#Send results to Matlab
writeMat("Matlab Arash/R_results.mat", time=df_result$time, 
         HPstatus=df_result$HPs_optim, 
         Tsup=df_result$Tsup_optim, 
         Taver=df_result$Temp_aver,
         hp_el=df_result$hp_el,
         price=df_result$price)

writeMat("Matlab Arash/R_results_cut.mat", time=df_disconfort$time, 
         HPstatus=df_disconfort$HPs_optim, 
         Tsup=df_disconfort$Tsup_optim, 
         Taver=df_disconfort$Temp_aver,
         hp_el=df_disconfort$hp_el,
         price=df_disconfort$price)

comparison <- data.frame("Time" = BM_20$Time,
                         "diff_Taver" = diff_Taver)

#Dinamic plot of the differens of temperature average between matlab and the integration of Modeica in R
library(plotly)
ggplotly(ggplot(comparison, aes(Time, diff_Taver)) + geom_point())
ggplot(comparison, aes(Time, diff_Taver)) + geom_point()


####cutted plot
plot_df01 <- df_result[df_result$time_to_predict_step >= as.POSIXct(x = "2018-12-20 00:00:00 UTC", tz = "UTC") &
                         df_result$time_to_predict_step <= as.POSIXct(x = "2018-12-21 00:00:00 UTC", tz = "UTC"),]

ggplot(plot_df01, aes(x = time_to_predict_step)) +
  geom_line(aes(y = price)) +
  geom_line(aes(y = hp_el/40, color = "blue"))
