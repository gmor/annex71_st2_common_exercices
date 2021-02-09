#The only modifications are the "mod_nonlin" in the variables and the two conditionals inside the forecast loop
#The ML is supposed to be trained to forecast the non-liniarity part of the model and then after the lm()/penlalty()  
#prediction forecast the non-liniarity part of the model and get make the correction on tin

prediction_scenario <- function(mod_q, mod_ti, mod_tsupply, df, rows_to_filter=NULL, hp_tset_24h, params, horizon, ts_prediction=NULL,
                                df_train_nonlin_colnames = NULL, mod_nonlin = NULL){
  # df <- merge(df_house,df_weather)
  df <- tune_model_input(df,params)
  if(!is.null(rows_to_filter) && sum(rows_to_filter,na.rm=T)>0){ 
    df <- df[rows_to_filter,]
    hp_tset_24h <- hp_tset_24h[rows_to_filter] # hp_tset_24h <- hp_tset_24h_saved[rows_to_filter]
  }
  #df <- df[complete.cases(df),]
  
  df$date <- as.Date(df$time, tz="Europe/Berlin")

  # # CHANGED HERE:
  if(is.null(ts_prediction)){
    ts_prediction <- smartAgg(df,"date",function(x){x[sample(1:length(x),1)]},"time",catN = F)$time #min(x,na.rm=T)
    if (horizon == 24) {
      #ts_prediction <- ts_prediction[-length(ts_prediction)]
      ts_prediction[length(ts_prediction)] <- as.POSIXct(x = sprintf("%i-%02i-%02i 23:00:00 UTC", 
                                                                     year(ts_prediction[length(ts_prediction)]), 
                                                                     month(ts_prediction[length(ts_prediction)]), 
                                                                     day(ts_prediction[length(ts_prediction)])-1), 
                                                         tz = "UTC")
    }
  }
  
  # ts_prediction <- df$time[1]
  
  # Simulate by sets of 24h from the ts_prediction
  df <- lapply(ts_prediction,function(ts_){
    
    # Filter the initial dataset 
    # ts_ = ts_prediction[1]
    # df_ <- df[df$time>=ts_ & df$time<=(ts_+hours(23)),]
    df_ <- df[df$time>=ts_ & df$time<=(ts_+hours(horizon-1)),]
    df_$ts_prediction <- ts_
    
    # Assign the control variables of the scenario
    df_$hp_tset_l0 <- as.numeric(hp_tset_24h[df$time %in% df_$time])
    df_$hp_status_l0 <- ifelse(is.na(hp_tset_24h[df$time %in% df_$time]),0,1)
    
    # Iterate for each timestep (1 hour)
    for (i in 1:nrow(df_)){
      
      #i=12 i=nrow(df_) i=21
      # Calculate the floor temperature and indoor temperature under free floating conditions
      df_$hp_cons_l0[i] <- 0
      tsupply_ff <- df_$tsupply_l0[i] <- predict(mod_tsupply, df_[i,])
      
      ti_ff <- df_$ti_l0[i] <- predict(mod_ti, df_[i,])
      
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
          df_$ti_l0[i] <- predict(mod_ti, df_[i,])
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