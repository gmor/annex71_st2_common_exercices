#
# Load this file after the functions.R in order to modify the prediction scenario function
#

calculate_model_ti_AutoXGboost <- function(params, df, train_dates, output="aic"){
  
  #df_mod <- df
  #df<-df_mod
  df <- tune_model_input(df,params)

  df <- df[as.Date(df$time,"Europe/Madrid") %in% train_dates,]
  df <- df[complete.cases(df),]
  
  df <- df[,!grepl(c("time|ti_l0|^dti|^dtf|^dte|^te_raw"),colnames(df))]
  
  trainTask <- makeRegrTask(data = df, target = "ti")

  ctrl <- makeMBOControl(#resample.at = integer(0),
                         resample.desc = makeResampleDesc("Bootstrap", iter = 10L, predict = "test")#folds = 4, reps = 5), #makeResampleDesc("RepCV", folds = 4L, reps = 30L)
                         # final.method = "best.predicted",
                         )#final.evals = 3L
  ctrl <- makeMBOControl()
  ctrl <- setMBOControlTermination(ctrl, iters = 3)
  ctrl <- setMBOControlInfill(ctrl, crit = crit.se, opt = 'ea')
  library(autoxgboost)
  # autoxgbparset.mixed <- makeParamSet(makeIntegerParam("max_depth", lower = 3, upper = 20))
  # autoxgbparset.mixed = makeParamSet(
  #   makeDiscreteParam("booster", values = c("gbtree")),#, "gblinear", "dart")),
  #   makeDiscreteParam("sample_type", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
  #   makeDiscreteParam("normalize_type", values = c("tree", "forest"), requires = quote(booster == "dart")),
  #   makeNumericParam("rate_drop", lower = 0, upper = 1, requires = quote(booster == "dart")),
  #   makeNumericParam("skip_drop", lower = 0, upper = 1, requires = quote(booster == "dart")),
  #   makeLogicalParam("one_drop", requires = quote(booster == "dart")),
  #   makeDiscreteParam("grow_policy", values = c("depthwise", "lossguide")),
  #   makeIntegerParam("max_leaves", lower = 0, upper = 8, trafo = function(x) 2^x, requires = quote(grow_policy == "lossguide")),
  #   makeIntegerParam("max_bin", lower = 2L, upper = 9, trafo = function(x) 2^x),
  #   makeNumericParam("eta", lower = 0.01, upper = 0.2),
  #   makeNumericParam("gamma", lower = -7, upper = 6, trafo = function(x) 2^x),
  #   makeIntegerParam("max_depth", lower = 3, upper = 4),
  #   makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  #   makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
  #   makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
  #   makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
  #   makeNumericParam("subsample", lower = 0.5, upper = 1)
  # )
  
  mod <- autoxgboost(trainTask,
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
  mod$final.learner
  mod2 <- xgb.train(
    params=list(booster="gbtree", objective= "reg:squarederror",
                eta=0.092,gamma=0.0104,max_depth=5,colsample_bytree=0.749,colsample_bylevel=0.909,lambda=0.00119,alpha=0.0872,subsample=0.871),
    data= xgb.DMatrix(as.matrix(df[,!(colnames(df)%in%c("ti"))]),label=df$ti),
    nrounds=51,
    eval_metrix="rmse"
    )
  mat <- xgb.importance(feature_names = colnames(df),model=mod2)
  xgb.plot.importance(importance_matrix = mat[1:20]) #first 20 variables
  
  res <- as.data.frame(predict(mod,df))
  cor(res$truth,res$response)^2
  
  # task <- makeRegrTask(id = 'aa', data = df, target = "ti")
  # task.over = oversample(task, rate = 8)
  # task.under = undersample(task, rate = 1/8)
  # learner <- makeLearner(cl = "regr.crs", predict.type = "se")
  # learner <- makeBaggingWrapper(learner,
  #                               bw.iters = 10L,
  #                               bw.replace = TRUE,
  #                               bw.size,
  #                               bw.feats = 1
  #                             )
  # mod <- train(learner, task)
  
  mod$force_colnames <- colnames(df)
  
  # mod <- lm(formula, data=df)
  result_ti <- list("mod"=mod,"df"=df)
  if(output == "model"){
    return(list("mod"=mod,"df"=df))
  } else {
    if(!is.null(df_v)){
      df_v <- tune_model_input(df_v, params)
      return(-pracma::rmserr(df_v$ti_l0,predict(mod,df_v))$rmse)#-AIC(mod))#summary(mod)$r.sq)
    } else {
      return(-pracma::rmserr(mod$model$ti_l0,mod$fitted.value)$rmse)#-AIC(mod))#summary(mod)$r.sq)
    }
  }
}

prediction_scenario <- function(mod_q, mod_ti, mod_tsupply, df, rows_to_filter=NULL, hp_tset_24h, params, ts_prediction=NULL,
                                df_train_nonlin_colnames = NULL, mod_nonlin = NULL){
  
  df <- tune_model_input(df,params)
  if(!is.null(rows_to_filter) && sum(rows_to_filter,na.rm=T)>0){ 
    df <- df[rows_to_filter,]
    hp_tset_24h <- hp_tset_24h[rows_to_filter]
  }
  #df <- df[complete.cases(df),]
  
  df$date <- as.Date(df$time, tz="Europe/Berlin")

  # # CHANGED HERE:
  if(is.null(ts_prediction)){
    ts_prediction <- smartAgg(df,"date",function(x){x[sample(1:length(x),1)]},"time",catN = F)$time #min(x,na.rm=T)
  }
  
  # ts_prediction <- df$time[1]
  
  # Simulate by sets of 24h from the ts_prediction
  df <- lapply(ts_prediction,function(ts_){
    
    # Filter the initial dataset 
    # ts_ = ts_prediction[1]
    # df_ <- df[df$time>=ts_ & df$time<=(ts_+hours(23)),]
    df_ <- df[df$time>=ts_ & df$time<=(ts_+hours(11)),]
    df_$ts_prediction <- ts_
    
    # Assign the control variables of the scenario
    df_$hp_tset_l0 <- as.numeric(hp_tset_24h[df$time %in% df_$time])
    df_$hp_status_l0 <- ifelse(is.na(hp_tset_24h[df$time %in% df_$time]),0,1)
    
    # Iterate for each timestep (1 hour)
    for (i in 1:nrow(df_)){
      #i=12
      # Calculate the floor temperature and indoor temperature under free floating conditions
      df_$hp_cons_l0[i] <- 0
      tsupply_ff <- df_$tsupply_l0[i] <- predict(mod_tsupply, df_[i,])
      ti_ff <- df_$ti_l0[i] <- predict(mod_ti, df_[i,mod_ti$force_colnames])$data$response
      if (!is.null(mod_nonlin)) {
        ti_ff <- df_$ti_l0[i] <- df_$ti_l0[i]-predict(mod_nonlin, df_[i,df_train_nonlin_colnames])$data$response
      }
      # If the heat pump should be on, estimate the heat pump consumption and 
      # re-estimate the floor and indoor temperatures considering the heat input.
      if(df_$hp_status_l0[i]==1 && !is.na(tsupply_ff) && !is.na(ti_ff)){
        df_$tsupply_l0[i] <- df_$hp_tset_l0[i]
        df_$hp_cons_l0[i] <- predict(mod_q, df_[i,])
        if(df_$hp_cons_l0[i]>0){
          # df_$tsupply_l0[i] <- predict(mod_tsupply, df_[i,])
          df_$ti_l0[i] <- predict(mod_ti, df_[i,mod_ti$force_colnames])$data$response
          if (!is.null(mod_nonlin)) {
            ti_ff <- df_$ti_l0[i] <- df_$ti_l0[i]-predict(mod_nonlin, df_[i,df_train_nonlin_colnames])$data$response
          }
          # If heat pump consumption estimation is negative, then consider the indoor and 
          # floor temperatures estimated with free floating conditions
        } else {
          df_$hp_cons_l0[i] <- 0
          df_$hp_status_l0[i] <- 0
          df_$hp_tset_l0[i] <- NA
          df_$tsupply_l0[i] <- tsupply_ff
          df_$ti_l0[i] <- ti_ff
        }
      }
      
      # Reassign the ti calculated values to next timesteps lagged values
      for (l in 1:max(params[c("mod_tsupply_ar","mod_hp_cons_lags_tsupply","mod_ti_lags_dti")])){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("tsupply_l",l)] <- df_$tsupply_l0[i]}}, error=function(e){next})
      }
      for (l in 1:max(params[c("mod_hp_cons_ar","mod_tsupply_lags_hp_cons","mod_tsupply_ar","mod_ti_lags_dti")])){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("hp_cons_l",l)] <- df_$hp_cons_l0[i]}}, error=function(e){next})
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("hp_status_l",l)] <- df_$hp_status_l0[i]}}, error=function(e){next})
      }
      for (l in 1:max(params[c("mod_ti_ar","mod_ti_lags_infiltrations")])){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("ti_l",l)] <- df_$ti_l0[i]}}, error=function(e){next})
      }
      df$dti[i] <- df$ti_l0[i] - df$tsupply_l0[i]
      for (l in 1:max(c(params[c("mod_ti_lags_dti","mod_tsupply_lags_dti")]))){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("dti_l",l)] <- df_$dti_l0[i]}}, error=function(e){next})
      }
      df$dte_l0[i] <- (rowMeans(data.frame(df$ti_l1[i],df$ti_l0[i])) - rowMeans(data.frame(df$te_l1[i],df$te_l0[i])))
      df$infiltrations_l0[i] <- ifelse(df$dte_l0[i]>0,df$dte_l0[i],0) * df$windSpeed[i]
      for (l in 1:max(c(params[c("mod_ti_lags_infiltrations")]))){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("infiltrations_l",l)] <- df_$infiltrations_l0[i]}}, error=function(e){next})
      }
      df$dtf_l0[i] <- df$tsupply_l0[i] - df$te_l0[i]
      for (l in 1:max(c(params[c("mod_hp_cons_lags_tsupply")]))){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("dtf_l",l)] <- df_$dtf_l0[i]}}, error=function(e){next})
      }
    }
    return(df_)
  })
  
  df <- df  %>% plyr::ldply(rbind)
  # ggplot(df) + geom_line(aes(time,ti)) + geom_line(aes(time,ti_l0),col="red") +
  #   geom_line(aes(time,value_l0),col="red")+ geom_line(aes(time,value)) 
  return(df)
}
