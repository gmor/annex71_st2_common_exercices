#The ML is supposed to be trained to forecast the non-liniarity part of the model and then after the lm()/penlalty()  
#prediction forecast the non-liniarity part of the model and get make the correction on tin


# pulse secure:
# r0753014
# MPCsubex1

setwd("C:/Users/gerar/Nextcloud/Github/annex71_st2_common_exercices")
source("ML for non-linearity/functions_non-linear-ML.R")

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

# library to integrate Python
library(reticulate)

#library ML
library(autoxgboost)

######################################################### IMPORT DATA #########################################################
######################################################### TRAINING MODEL #########################################################

##After training the model

# #Test to simulate ti with ML
#     library(mlr)
#     library(mlrMBO)
#     library(autoxgboost)
#     library(xgboost)
#     library(zoo)
#     #dfr <- df[,!(colnames(df) %in% c("vent", "windSpeed", "windBearing", "humidity", "sunAz", "sunEl", "GHI"))]
#     # ti_mod <- as.list(rollmean(df["ti"], 4, align = "center", fill = "extend"))
#     df_mod <- df#[,!(colnames(df) %in% "ti")]
#     # df_mod["ti"] <- as.numeric(ti_mod)
#      
#     result_ti <- calculate_model_ti_AutoXGboost(params, df_mod, train_dates, output="model") #df
#     mod_ti <- result_ti$mod
#     summary(mod_ti$final.learner)



######################################################### Compute R2 of ti#########################################################


######################################################### MODEL THE NON-LINEARITY #####################################################
forecast_error_dates_df <- df$time[as.Date(df$time) %in% train_dates_1 | as.Date(df$time) %in% train_dates_2]
forecast_error_dates_df <- df$time[as.Date(df$time) %in% train_dates_2]

#Forecast and storage of the results
ti_df <- {}
ti_l0_df <- {}
ii_df <- c()
predv_df <- data.frame({})
for (i in 1:length(forecast_error_dates_df)) {
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    df = df,#df,
    rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% forecast_error_dates_df,
    hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    ts_prediction = forecast_error_dates_df[i],#NULL
    horizon = horizon
  )
  
  for (i in 1:length(predv_1)) {
    predv_1 <- predv[i,]
    predv_1 <- as.data.frame(c(predv_1, "error_1" = (predv_1$ti - predv_1$ti_l0)))
    predv_df <- rbind(predv_df, predv_1)
  }
  ti_df <- c(ti_df, predv$ti) #vector of the 12h real values in each iteration
  ti_l0_df <- c(ti_l0_df, predv$ti_l0) #vector of the 12h forecasts in each iteration
  ii_df <- c(ii_df,1:nrow(predv))
}

#Compute R2
R2_df <- 1-sum((ti_df-ti_l0_df)^2)/sum((ti_df-mean(ti_df))^2)
R2_df
error <- ti_df-ti_l0_df
max(abs(error))
max(abs(predv_df$error_1),na.rm = TRUE)
plot(predv_df$error_1)
hist(predv_df$error_1,breaks = 20)
# df <- df[,!grepl(c("time|ti_l0|^dti|^dtf|^dte|^te_raw"),colnames(df))]
df_train_nonlin <- subset(predv_df, select = -c(time,date,ti_l0,dti,dti,dtf,dte,te_raw,ts_prediction))
df_train_nonlin <- df_train_nonlin[!is.na(df_train_nonlin$error_1),]

trainTask <- makeRegrTask(data = df_train_nonlin, target = "error_1")

ctrl <- makeMBOControl()
mod_nonlin <- autoxgboost(trainTask,
                   control = ctrl,
                   measure = rmse,
                   #tune.threshold = FALSE,
                   # max.nrounds=1500,
                   mbo.learner = makeLearner(cl = "regr.bgpllm", predict.type = "se",
                                             par.vals = list()), #"regr.randomForest"
                   #"regr.crs" i "regr.bgpllm" sembla que funciona be pero no aconsegueixo reduir l'overfit amb iterations=5 iteracion i design.size=2
                   # par.set = autoxgbparset.mixed,
                   iterations = 5L
                   #build.final.model = T,
                   #design.size = 15L
                   )

library(xgboost)
mod_nonlin$final.learner
mod2 <- xgb.train(
  params=list(booster="gbtree", objective= "reg:linear",
              eta=0.0673,gamma=0.00811,max_depth=20,colsample_bytree=0.504,colsample_bylevel=0.808,lambda=0.00243,alpha=0.00276,subsample=0.91),
  data= xgb.DMatrix(as.matrix(df_train_nonlin[,!(colnames(df_train_nonlin)%in%c("error_1"))]),label=df_train_nonlin$error_1),
  nrounds=69,
  # eval_metrix="rmse"
)

mat <- xgb.importance(feature_names = colnames(df_train_nonlin),model=mod2)
xgb.plot.importance(importance_matrix = mat[1:20]) #first 20 variables

result_error <- mod_nonlin$final.model
summary(result_error)

error_1_p <- predict(mod_nonlin, predv_df[,colnames(df_train_nonlin)])$data$response
plot(predv_df$time,predv_df$error_1)
plot(predv_df$time,(predv_df$error_1-error_1_p))

sum(abs(predv_df$error_1))/length(predv_df$error_1)
sum(abs(predv_df$error_1-error_1_p))/length(predv_df$error_1)

#Forecast with nonlineatity
df_train_nonlin_colnames <- colnames(df_train_nonlin)
df_train_nonlin_colnames <- df_train_nonlin_colnames[-length(df_train_nonlin_colnames)]
df_train_nonlin <- subset(predv_df, select = -c(time,date,ti_l0,dti,dti,dtf,dte,te_raw,ts_prediction))


ML_forecast_error_dates_df <- df$time[as.Date(df$time) %in% val_dates]
ML_ti_df <- {}
ML_ti_l0_df <- {}
ML_ii_df <- c()
ML_predv_df <- data.frame({})
for (i in 1:length(ML_forecast_error_dates_df)) {
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    df = df,#df,
    rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% ML_forecast_error_dates_df,
    hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    ts_prediction = ML_forecast_error_dates_df[i],#NULL
    df_train_nonlin_colnames = df_train_nonlin_colnames,#NULL
    mod_nonlin = mod_nonlin,#NULL
    horizon = horizon
  )
  ML_predv_1 <- predv[1,]
  ML_predv_1 <- as.data.frame(c(ML_predv_1, "error_1" = (ML_predv_1$ti - ML_predv_1$ti_l0)))
  ML_predv_df <- rbind(ML_predv_df, ML_predv_1)
  ML_ti_df <- c(ML_ti_df, predv$ti) #vector of the 12h real values in each iteration
  ML_ti_l0_df <- c(ML_ti_l0_df, predv$ti_l0) #vector of the 12h forecasts in each iteration
  ML_ii_df <- c(ML_ii_df,1:nrow(predv))
}

ML_R2_df <- 1-sum((ML_ti_df-ML_ti_l0_df)^2)/sum((ML_ti_df-mean(ML_ti_df))^2)
ML_R2_df
error <- ti_df-ti_l0_df
max(abs(error))
max(abs(predv_df$error_1))
plot(predv_df$error_1)
hist(predv_df$error_1,breaks = 20)