read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Low pass filter function
lp_vector<- function(x, a1) {
  ## Make a 1'st order low pass filter as (5.3) p.46 in the HAN report.
  y <- numeric(length(x))
  ## First value in x is the init value
  y[1] <- x[1]
  ## 
  for (i in 2:length(x)) {
    if (is.na(y[i - 1])) {
      y[i] <- x[i]
    } else {
      y[i] <- a1 * y[i - 1] + (1 - a1) * x[i]
    }
  }
  ## Return (afterwards the init value y[1], must be handled)
  return(y)
}

tune_model_input <- function(df,params){
  
  # Low pass filtering of the indoor and outdoor temperatures
  df$te_raw <- df$te
  df$te <- lp_vector(df$te,params["alpha_te"])
  df$GHI <- lp_vector(df$GHI,params["alpha_GHI"])
  df$BHI <- lp_vector(df$BHI,params["alpha_BHI"])
  df$windSpeed <- lp_vector(df$windSpeed,params["alpha_ws"])
  df$hp_thermal <- df$hp_cons * df$hp_cop
  
  # Lag the input columns and weather tranformations
  # HP consumption
  for (l in 0:max(params[c("mod_tsupply_lags_hp_cons","mod_hp_cons_ar","mod_tsupply_ar","mod_ti_lags_dti","mod_ti_lags_hp_cons")])){
    df[,paste0("hp_cons_l",l)] <- dplyr::lag(df[,"hp_cons"],l)
    df[,paste0("hp_thermal_l",l)] <- dplyr::lag(df[,"hp_thermal"],l)
    df[,paste0("hp_status_l",l)] <- dplyr::lag(df[,"hp_status"],l)
    df[,paste0("hp_cop_l",l)] <- dplyr::lag(df[,"hp_cop"],l)
  }
  df[,"hp_tset_l0"] <- df[,"hp_tset"]
  for (l in 0:max(params[c("mod_hp_cons_lags_tsupply","mod_ti_lags_dti","mod_tsupply_ar")])){
    df[,paste0("tsupply_l",l)] <- dplyr::lag(df[,"tsupply"],l)
  }
  # Weather and indoor comfort
  for (l in 0:max(c(1,params[c("mod_hp_cons_ar","mod_hp_cons_lags_te","mod_hp_cons_lags_humidity","mod_ti_lags_te","mod_hp_cons_lags_tsupply",
                               "mod_ti_lags_infiltrations","mod_tsupply_lags_hp_cons","mod_tsupply_lags_te")]))){
    df[,paste0("te_l",l)] <- dplyr::lag(df[,"te"],l)
    df[,paste0("te_raw_l",l)] <- dplyr::lag(df[,"te_raw"],l)
  }
  df$dtf <- df$tsupply - df$te
  for (l in 0:max(c(1,params[c("mod_hp_cons_lags_tsupply")]))){
    df[,paste0("dtf_l",l)] <- dplyr::lag(df[,"dtf"],l)
  }
  df$dti <- df$tsupply - df$ti
  for (l in 0:max(c(1,params[c("mod_ti_ar","mod_ti_lags_dti","mod_ti_lags_infiltrations","mod_hp_cons_lags_ti")]))){
    df[,paste0("ti_l",l)] <- dplyr::lag(df[,"ti"],l)
  }
  for (l in 0:max(c(params[c("mod_ti_lags_dti","mod_tsupply_lags_dti")]))){
    df[,paste0("dti_l",l)] <- dplyr::lag(df[,"dti"],l)
  }
  for (l in 0:params["mod_ti_lags_hg"]){
    df[,paste0("hg_l",l)] <- dplyr::lag(df[,"hg"],l)
  }
  for (l in 0:params["mod_ti_lags_GHI"]){
    df[,paste0("GHI_l",l)] <- dplyr::lag(df[,"GHI"],l)
  }
  for (l in 0:params["mod_ti_lags_BHI"]){
    df[,paste0("BHI_l",l)] <- dplyr::lag(df[,"BHI"],l)
  }
  for (l in 0:params["mod_ti_lags_ventilation"]){
    df[,paste0("vent_l",l)] <- dplyr::lag(df[,"vent"],l)
    df[,paste0("vent_status_l",l)] <- dplyr::lag(df[,"vent_status"],l)
    # df[,paste0("air_h_l",l)] <- dplyr::lag(df[,"air_h"],l)
    # df[,paste0("air_e_l",l)] <- dplyr::lag(df[,"air_e"],l)
    # df[,paste0("air_s_l",l)] <- dplyr::lag(df[,"air_s"],l)
  }
  for (l in 0:max(params[c("mod_hp_cons_ar","mod_ti_lags_humidity","mod_hp_cons_lags_te",
                           "mod_hp_cons_lags_humidity","mod_tsupply_lags_hp_cons")])){
    df[,paste0("humidity_l",l)] <- dplyr::lag(df[,"humidity"],l)
  }
  df$dte <- (rowMeans(data.frame(df$ti_l1)) - rowMeans(data.frame(df$te_l1,df$te_l0)))
  df$infiltrations <- -ifelse(df$dte>0,df$dte,0) * df$windSpeed
  # for (l in 0:params["lags_dte"]){
  #   df[,paste0("dte_l",l)] <- dplyr::lag(df[,"dte"],l)
  # }
  for (l in 0:params["mod_ti_lags_infiltrations"]){
    df[,paste0("infiltrations_l",l)] <- dplyr::lag(df[,"infiltrations"],l)
  }
  
  # Calculate the sunazimuth and windbearing fourier series
  if(params["sunAzimuth_nharmonics"]>0){
    df <- add_fourier_series_sunazimuth(df, sunAzimuth_nharmonics = params["sunAzimuth_nharmonics"], min_solarElevation=-1)
  }
  for(m in colnames(df)[grepl("^sunAzimuth_fs_",colnames(df))]){
    for (l in 0:max(params[c("mod_ti_lags_GHI","mod_ti_lags_BHI")])){
      df[,gsub("fs",paste0("fs_l",l),m)] <- dplyr::lag(df[,m],l)
    }
  }
  if(params["windBearing_nharmonics"]>0){
    df <- add_fourier_series_windbearing(df, windBearing_nharmonics= params["windBearing_nharmonics"])
  }
  for(m in colnames(df)[grepl("^windBearing_fs_",colnames(df))]){
    for (l in 0:params["mod_ti_lags_infiltrations"]){
      df[,gsub("fs",paste0("fs_l",l),m)] <- dplyr::lag(df[,m],l)
    }
  }
  for (l in 0:max(c(params[c("mod_ti_lags_air_h")]))) {
    df[,paste0("air_h_l",l)] <- dplyr::lag(df[,"air_h"],l)
  }
  
  return(df)
}

fs <- function(X, nharmonics, pair_function=T) {
  out <- as.list(do.call(c, lapply(1:nharmonics, function(i) {
    if(pair_function==T){
      val <- list(sin(i * X * 2 * pi), cos(i * X * 2 * pi))
      names(val) <- paste0(c("sin_", "cos_"), i)
      return(val)
    } else {
      val <- list(sin(i * X * 2 * pi))
      names(val) <- paste0(c("sin_"), i)
      return(val)
    }
  })))
  # out <- c(out, rep(1, length(out[[1]])))
  return(do.call(cbind,out))
}

add_fourier_series_sunazimuth <- function(df, sunAzimuth_nharmonics, min_solarElevation=10) {
  # Fourier series of the sunAzimuth
  
  # yearly_sun <- sunAngle(seq(as.POSIXct("2019-01-01 00:00:00"),as.POSIXct("2019-12-31 23:59:59"),by="hour"),
  #                        latitude = unique(df[, "latitude"]), longitude = unique(df[, "longitude"]))
  # min_max_azimuth <- c("min"=min(yearly_sun$azimuth[yearly_sun$altitude>min_solarElevation]),
  #                      "max"=max(yearly_sun$azimuth[yearly_sun$altitude>min_solarElevation]))
  # sunAzimuth_01 <- normalize_range_int(ifelse(df$sunAzimuth==0,NA,df$sunAzimuth),0,1,specs = min_max_azimuth)
  
  sunAzimuth_01 <- normalize_range_int(df$sunAz,-0.5,0.5,specs = c("min"=0,"max"=360))
  sunAzimuth_01 <- ifelse(sunAzimuth_01>0,sunAzimuth_01-0.5,sunAzimuth_01+0.5)
  
  sunAzimuth_fs <- as.data.frame(fs(sunAzimuth_01, nharmonics=sunAzimuth_nharmonics,pair_function = F))
  colnames(sunAzimuth_fs) <- paste0("sunAzimuth_fs_",colnames(sunAzimuth_fs))
  sunAzimuth_fs[is.na(sunAzimuth_fs)] <- 0
  df <- cbind(df,sunAzimuth_fs)
  
  return(df)
  
}

add_fourier_series_windbearing <- function(df, windBearing_nharmonics) {
  
  # Fourier series of the windBearing
  windBearing_01 <- normalize_range_int(df$windBearing,-0.5,0.5,specs = c("min"=0,"max"=360))
  #windBearing_01 <- normalize_range_int(0:360,-0.5,0.5,specs = c("min"=0,"max"=360))
  windBearing_01 <- ifelse(windBearing_01>0,windBearing_01-0.5,windBearing_01+0.5)
  
  windBearing_fs <- as.data.frame(fs(windBearing_01, nharmonics=windBearing_nharmonics,pair_function = F))
  colnames(windBearing_fs) <- paste0("windBearing_fs_",colnames(windBearing_fs))
  df <- cbind(df,windBearing_fs)
  
  return(df)
  
}

normalize_range_int <- function(df,inf,sup,specs=NULL,threshold_for_min=NULL){
  if(sup>inf){
    
    if(!is.null(ncol(df))){
      if (is.null(specs)){
        specs <- mapply(function(i){c("min"=min(df[,i],na.rm=T),
                                      "max"=max(df[,i],na.rm=T))},1:ncol(df))
      }
      result <- list(
        "norm" = mapply(
          function(i){
            x <- df[,i]
            if (specs["min",i]==specs["max",i]){
              rep(inf,length(x))
            } else {
              r <- ((x-specs["min",i])/(specs["max",i]-specs["min",i]))*(sup-inf)+inf
              if(!is.null(threshold_for_min)){
                r <- ifelse(x>=threshold_for_min,r,inf)
              }
              return(r)
            }
          },
          1:ncol(df)
        ),
        "specs" = specs
      )
      if(!is.matrix(result$norm)){
        result$norm <- t(result$norm)
        colnames(result$norm) <- colnames(df)
      }
      return(result)
    } else{
      x <- df
      if (is.null(specs)){
        specs <- c("min" = min(x,na.rm=T), "max" = max(x,na.rm=T))
      }
      if (specs["min"]==specs["max"]){
        rep(inf,length(x))
      } else {
        r <- ((x-specs["min"])/(specs["max"]-specs["min"]))*(sup-inf)+inf
        if(!is.null(threshold_for_min)){
          r <- ifelse(x>=threshold_for_min,r,inf)
        }
      }
      return(r)
    }
  }
}


calculate_model_ti <- function(params, df, train_dates, output="aic", penalty = NULL){

  #df <- merge(df_house,df_weather)
  df <- tune_model_input(df,params)
  df_v <- NULL
  
  df <- df[as.Date(df$time,"Europe/Madrid") %in% train_dates,]
  df <- df[complete.cases(df),]
  
  # # Formula definition. Base formula + GHI and windSpeed terms #ti_l0~
  # formula <-  as.formula(sprintf("~ 
  #                                0 + %s",
  #                                paste(
  #                                  paste0("ti_l",1:params["mod_ti_ar"],"",collapse=" + "),#paste0("ti_l",1:params["mod_ti_ar"],"",collapse=" + "),
  #                                  #paste0(mapply(function(x){sprintf("(ti_l%s-te_l%s)",x,x)},1:params["mod_ti_ar2"]),collapse=" + "), #"bs(ti_l%s-te_l%s, degree=2)"
  #                                  # paste0(mapply(function(x){sprintf("bs(ti_l%s,degree=2)",x)},1:params["mod_ti_ar2"]),collapse=" + "),
  #                                  paste0(mapply(function(x){sprintf("tsupply_l%s:as.factor(hp_status_l%s)",x,x)},1:params["mod_ti_lags_dti"]),collapse=" + "),
  #                                  #paste0("tsupply_l",0:(params["mod_ti_lags_dti"]),collapse=" + "),
  #                                  paste0("te_l",0:(params["mod_ti_lags_te"]),"",collapse=" + "),
  #                                  paste0("hg_l",0:(params["mod_ti_lags_hg"]),"",collapse=" + "),
  #                                  paste0("BHI_l",0:(params["mod_ti_lags_BHI"]),":fs(sunAz/360,2)",collapse=" + "),
  #                                  # paste0("vent_l",0:(params["mod_ti_lags_ventilation"]),"",collapse=" + "),
  #                                  sep=" + "
  #                                )))
  
  # Formula definition. Base formula + GHI and windSpeed terms #ti_l0~
  #params_ <- params
  formula <-  as.formula(sprintf("~ 
                                 0 + %s",
                                 paste(
                                   paste0("ti_l",1:params["mod_ti_ar"],"",collapse=" + "),#paste0("ti_l",1:params["mod_ti_ar"],"",collapse=" + "),
                                   #paste0(mapply(function(x){sprintf("(ti_l%s-te_l%s)",x,x)},1:params["mod_ti_ar2"]),collapse=" + "), #"bs(ti_l%s-te_l%s, degree=2)"
                                   #paste0(mapply(function(x){sprintf("bs(hp_cons_l%s,degree=2,knots=4)",x)},1:params["mod_ti_lags_hp_cons"]),collapse=" + "),
                                   paste0("hp_thermal_l",0:(params["mod_ti_lags_hp_cons"]),"",collapse=" + "),
                                   #paste0("bs(tsupply_l",1:(params["mod_ti_lags_dti"]),",degree=2,knots=3)",collapse=" + "),
                                   #paste0("tsupply_l",0:(params["mod_ti_lags_dti"]),collapse=" + "),
                                   paste0("te_l",0:(params["mod_ti_lags_te"]),"",collapse=" + "),
                                   # paste0(mapply(function(x){sprintf("tsupply_l%s:as.factor(hp_status_l%s)",x,x)},1:params["mod_ti_lags_dti"]),collapse=" + "),
                                   #paste0("hp_cons_l",0:(params["mod_ti_lags_dti"]),":as.factor(hp_status_l0)",collapse=" + "),
                                   # paste0("bs(hg_l",0:(params["mod_ti_lags_hg"]),",degree=2,knots=3)",collapse=" + "),
                                   ## no available ## paste0("hg_l",0:(params["mod_ti_lags_hg"]),"",collapse=" + "),
                                   #paste0("GHI_l",0:(params["mod_ti_lags_GHI"]),":fs(sunAz/360,2)",collapse=" + "), #
                                   paste0("BHI_l",0:(params["mod_ti_lags_BHI"]),"",collapse=" + "), #
                                   paste0("BHI_l",0:(params["mod_ti_lags_BHI"]),":fs(sunAz/360,2)",collapse=" + "),
                                   # paste0("infiltrations_l",0:(params["mod_ti_lags_infiltrations"]),"",collapse=" + "),
                                   # paste0(mapply(function(x){sprintf("vent_l%s:as.factor(vent_status_l%s)",x,x)},0:params["mod_ti_lags_ventilation"]),"",collapse=" + "),
                                   # paste0("vent_l",0:(params["mod_ti_lags_ventilation"]),"",collapse=" + "),
                                   # paste0("air_h_l",0:(params["mod_ti_lags_air_h"]),"",collapse=" + "),
                                   sep=" + "
                                 )))
  
  
  
  
  # formula2 <-  as.formula(sprintf("ti_l0~
  #                                0 + %s",
  #                                paste(
  #                                  paste0("ti_l",1:params["mod_ti_ar"],"",collapse=" + "),#paste0("ti_l",1:params["mod_ti_ar"],"",collapse=" + "),
  #                                  #paste0(mapply(function(x){sprintf("(ti_l%s-te_l%s)",x,x)},1:params["mod_ti_ar2"]),collapse=" + "), #"bs(ti_l%s-te_l%s, degree=2)"
  #                                  #paste0(mapply(function(x){sprintf("bs(hp_cons_l%s,degree=2,knots=4)",x)},1:params["mod_ti_lags_hp_cons"]),collapse=" + "),
  #                                  #paste0("s(hp_cons_l",0:(params["mod_ti_lags_hp_cons"]),",k=2)",collapse=" + "),
  #                                  paste0("s(hp_thermal_l",0:(params["mod_ti_lags_dti"]),",k=2)",collapse=" + "),
  #                                  #paste0("tsupply_l",0:(params["mod_ti_lags_dti"]),collapse=" + "),
  #                                  paste0("te_l",0:(params["mod_ti_lags_te"]),"",collapse=" + "),
  #                                  #paste0("hp_cons_l",0:(params["mod_ti_lags_dti"]),":as.factor(hp_status_l0)",collapse=" + "),
  #                                  #paste0("s(hg_l",0:(params["mod_ti_lags_hg"]),",k=2)",collapse=" + "),
  #                                  #paste0("GHI_l",0:(params["mod_ti_lags_BHI"]),":fs(sunAz/360,2)",collapse=" + "), #
  #                                  #paste0("s(infiltrations_l",0:(params["mod_ti_lags_infiltrations"]),",k=2)",collapse=" + "),
  #                                  #paste0(mapply(function(x){sprintf("vent_l%s:as.factor(vent_status_l%s)",x,x)},0:params["mod_ti_lags_ventilation"]),"",collapse=" + "),
  #                                  sep=" + "
  #                                )))
  # mod2 <- gam(formula2,data = df)
  # par(mar=c(2,4,2,2))
  # summary(mod2)
  # plot.gam(mod2,scale = 0, pages=1,residuals = T, all.terms = T)
  # plot(mod2$fitted.values,type="l"); lines(mod2$model$ti_l0, col="red")
  
  # Define the sunAzimuth fourier series terms and add the GHI terms to the formula
  # if(params["mod_ti_solar_gains"]==1){
  #   formula <- update.formula(formula,paste0(". ~ . +",paste0("BHI_l",0:params["mod_ti_lags_BHI"],"",collapse=" + ")))
  # } else if (params["mod_ti_solar_gains"]==2){
  #   for (i in 0:params["mod_ti_lags_BHI"]){
  #     sunAzimuth_fs_terms <- colnames(df)[grepl(paste0("^sunAzimuth_fs_l",i),colnames(df))]
  #     solar_features <- lapply(sunAzimuth_fs_terms,function(x){paste0("BHI_l",i,":",x)})
  #     formula <- update.formula(formula,paste0(". ~ . + ",do.call(paste,list(solar_features,collapse=" + "))))
  #   }
  # }
  # # Define the windBearing fourier series terms and add the infiltration terms to the formula
  # if(params["mod_ti_infiltrations"]==1){
  #   formula <- update.formula(formula,paste0(". ~ . +",paste0("infiltrations_l",0:params["mod_ti_lags_infiltrations"],"",collapse=" + ")))
  # } else if (params["mod_ti_infiltrations"]==2){
  #   for (i in 0:params["mod_ti_lags_infiltrations"]){#(params[3]-1)){#0){#
  #     windBearing_fs_terms <- colnames(df)[grepl(paste0("^windBearing_fs_l",i),colnames(df))]
  #     infiltrations_features <- lapply(windBearing_fs_terms,function(x){paste0("infiltrations_l",i,":",x)})
  #     formula <- update.formula(formula,paste0(". ~ . + ",do.call(paste,list(infiltrations_features,collapse=" + "))))
  #   }
  # }
  
  # formula2 <- update.formula(formula, ti_l0~.)
  # mod2 <- lm(formula2, data=df)
  # summary(mod2)
  # acf(mod2$residuals)

  # mod <- penalized(response = df$ti_l0, penalized = model.matrix(formula, df), unpenalized = ~ 0, lambda1 = 0,lambda2 = 0)
  # penalty <- NULL
  x <- model.matrix(formula, df)
  y <- df$ti_l0
  if (is.null(penalty)) {
    mod <- penalized(response = y, penalized = x, unpenalized = ~ 0, lambda1 = 0,lambda2 = 0) 
                     #positive = grepl("tsupply|hp_cons|hg|infiltrations",colnames(x)))
  } else {
    mod <- penalized(response = y, penalized = x, unpenalized = ~ 0, lambda1 = penalty["L1"],lambda2 = penalty["L2"])
  }
  
  # plot_irf(linear_coefficients=coef(mod), impulse_lags=24, pattern_ar_coefficients = "^ti_l",
  #                    pattern_seasonality = NULL, title=bquote(Phi["k<0"]^"h"*"=0;"~Phi["k">="0"]^"h"*"=1"),
  #                    name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
  #                    y_label=bquote(Delta*Phi^"h"["k"]^"h"))
  # plot_irf(linear_coefficients=coef(mod), impulse_lags=24, pattern_ar_coefficients = "^hp_cons_l",
  #                  pattern_seasonality = "hp_status_l1", title=bquote(Psi["k<0"]*"=0;"~Psi["k">="0"]*"=1"),
  #                  name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
  #                  y_label=bquote(Delta*Phi^"h"["k"]^"h"), exhogenous_coeff=T)
  
  mod@nuisance$formula <- formula
  mod@nuisance$df <- df
  # plot(mod@fitted,type="l"); lines(df$ti_l0, col="red")
  # acf(mod@fitted - df[,"ti"])
  
  if(output == "model"){
    return(list("mod"=mod,"df"=df))
  } else {
    if(!is.null(df_v)){
      df_v <- tune_model_input(df_v, params)
      return(-pracma::rmserr(df_v$ti_l0,predict(mod,df_v))$rmse)#-AIC(mod))#summary(mod)$r.sq)
    } else {
      return(-pracma::rmserr(df$ti_l0,mod@fitted)$rmse)#-AIC(mod))#summary(mod)$r.sq)
    }
  }
}

calculate_model_q <- function(params, df, train_dates, output="aic"){
  
  
  df <- tune_model_input(df,params)
  
  
  df <- df[as.Date(df$time,"Europe/Madrid") %in% train_dates,]
  df <- df[complete.cases(df),]
  
  # Formula definition. Base formula + GHI and windSpeed terms
  formula <- as.formula(sprintf("hp_cons_l0 ~ 
      1 + %s",
        paste(
          paste0("bs(tsupply_l",0:params["mod_hp_cons_lags_tsupply"],",degree=2,knots=4)",collapse=" + "),
          #paste0(mapply(function(x){sprintf("bs(tsupply_l%s):bs(ti_l%s)",x,x)},1:1),collapse=" + "),
          paste0("bs(te_raw_l",0:params["mod_hp_cons_lags_te"],",degree=2,knots=4)",collapse=" + "),
          # paste0(mapply(function(x){sprintf("hp_cons_l%s:humidity_l%s",x,x)},1:params["mod_hp_cons_ar"]),collapse=" + "),
          # paste0(mapply(function(x){sprintf("hp_cons_l%s:te_l%s",x,x)},1:params["mod_hp_cons_ar"]),collapse=" + "),
          #paste0("hp_cons_l",1:params["mod_hp_cons_ar"],"",collapse=" + "),
          # paste0("te_l",0:params["mod_hp_cons_lags_te"],"",collapse=" + "),
          # paste0("bs(dtf_l",0:params["mod_hp_cons_lags_tsupply"],")",collapse=" + "),
          # paste0("bs(tsupply_l",0:params["mod_hp_cons_lags_tsupply"],")",collapse=" + "),
          paste0("ti_l",1:params["mod_hp_cons_lags_ti"],"",collapse=" + "),
          # paste0("humidity_l",0:params["mod_hp_cons_lags_humidity"],"",collapse=" + ")
          #paste0(mapply(function(x){sprintf("bs(te_l%s,knots=1,degree=2):bs(humidity_l%s,knots=1,degree=2)",x,x)},0:max(params[c("mod_hp_cons_lags_te","mod_hp_cons_lags_humidity")])),collapse=" + ")
        sep=" + ")))
  
  mod <- lm(formula, data=df[df$hp_status>0,])
  
  if(output == "model"){
    return(list("mod"=mod,"df"=df))
  } else {
    if(!is.null(df_v)){
      df_v <- tune_model_input(df_v, params)
      return(-pracma::rmserr(df_v$hp_cons_l0,predict(mod,df_v))$rmse)#-AIC(mod))#summary(mod)$r.sq)
    } else {
      return(-pracma::rmserr(mod$model$hp_cons_l0,mod$fitted.value)$rmse)#-AIC(mod))#summary(mod)$r.sq)
    }
  }
}

calculate_model_cop <- function(params, df, train_dates, output="aic"){
  
  # df <- merge(df_house,df_weather)
  df <- tune_model_input(df,params)
  
  
  df <- df[as.Date(df$time,"Europe/Madrid") %in% train_dates,]
  df <- df[complete.cases(df),]
  
  # Formula definition. Base formula + GHI and windSpeed terms
  formula <- as.formula(sprintf("hp_cop_l0 ~ 
      0 + %s",
      paste(
        paste0("bs(tsupply_l0,degree=2,knots=4)",collapse=" + "),
        paste0("bs(te_raw_l0,degree=2,knots=4)",collapse=" + "),
        # paste0("bs(te_raw_l1,degree=2,knots=4)",collapse=" + "),
        "tsupply_l0:te_raw_l0",
        sep=" + ")))
  
  mod <- lm(formula, data=df[df$hp_status>0,])

  if(output == "model"){
    return(list("mod"=mod,"df"=df))
  } else {
    if(!is.null(df_v)){
      df_v <- tune_model_input(df_v, params)
      return(-pracma::rmserr(df_v$hp_cop_l0,predict(mod,df_v))$rmse)#-AIC(mod))#summary(mod)$r.sq)
    } else {
      return(-pracma::rmserr(mod$model$hp_cop_l0,mod$fitted.value)$rmse)#-AIC(mod))#summary(mod)$r.sq)
    }
  }
}

calculate_model_tsupply <- function(params, df, train_dates, output="aic"){
  
  df <- tune_model_input(df,params)
  
  df <- df[as.Date(df$time,"Europe/Madrid") %in% train_dates,]
  df <- df[complete.cases(df),]
  
  # Formula definition. Base formula + GHI and windSpeed terms
  formula <- as.formula(sprintf("tsupply_l0 ~ 
      0 + %s",
      paste(
        # paste0(mapply(function(x){sprintf("bs(tsupply_l%s,degree=2)*as.factor(hp_status_l%s)",x,x-1)},1:params["mod_tsupply_ar"]),collapse=" + "),
        paste0(mapply(function(x){sprintf("tsupply_l%s:as.factor(hp_status_l%s)",x,x-1)},1:params["mod_tsupply_ar"]),collapse=" + "),
        paste0(mapply(function(x){sprintf("hp_cons_l%s:te_raw_l%s",x,x)},0:params["mod_tsupply_lags_hp_cons"]),collapse=" + "),
        paste0(mapply(function(x){sprintf("bs(hp_cons_l%s, degree=2, knots=4)",x)},0:params["mod_tsupply_lags_hp_cons"]),collapse=" + "),
        paste0(mapply(function(x){sprintf("te_l%s",x)},0:params["mod_tsupply_lags_te"]),collapse=" + "),
        #paste0("te_l",0:params["lags_te"],"",collapse=" + "),
        #paste0("humidity_l",0:params["lags_humidity"],"",collapse=" + "),
        #paste0("hg_l",0:params["lags_hg"],"",collapse=" + "),
                    #paste0("ti_l",1:max(1,params["lags_ti"]),"",collapse=" + ")
        sep=" + "
      )))
  
  # Define the sunAzimuth fourier series terms and add the GHI terms to the formula
  # for (i in 0:params["lags_GHI"]){
  #   sunAzimuth_fs_terms <- colnames(df)[grepl(paste0("^sunAzimuth_fs_l",i),colnames(df))]
  #   solar_features <- lapply(sunAzimuth_fs_terms,function(x){paste0("GHI_l",i,":",x)})
  #   formula <- update.formula(formula,paste0(". ~ . + ",do.call(paste,list(solar_features,collapse=" + "))))
  # }
  
  mod <- lm(formula, data=df)
  
  if(output == "model"){
    return(list("mod"=mod,"df"=df))
  } else {
    if(!is.null(df_v)){
      df_v <- tune_model_input(df_v, params)
      return(-pracma::rmserr(df_v$hp_cons_l0,predict(mod,df_v))$rmse)#-AIC(mod))#summary(mod)$r.sq)
    } else {
      return(-pracma::rmserr(mod$model$hp_cons_l0,mod$fitted.value)$rmse)#-AIC(mod))#summary(mod)$r.sq)
    }
  }
}


prediction_scenario <- function(mod_q, mod_ti, mod_tsupply, mod_cop, df, rows_to_filter=NULL, hp_tset_24h, params, horizon, ts_prediction=NULL, set_seed=214515){
  # df <- merge(df_house,df_weather)
  # df_bo <- df
  # df <- df_bo
  df <- tune_model_input(df,params)
  if(!is.null(rows_to_filter) && sum(rows_to_filter,na.rm=T)>0){ 
    df <- df[rows_to_filter,]
    hp_tset_24h <- hp_tset_24h[rows_to_filter] # hp_tset_24h <- hp_tset_24h_saved[rows_to_filter]
  }
  #df <- df[complete.cases(df),]
  
  df$date <- as.Date(df$time, tz="Europe/Berlin")

  # # CHANGED HERE:
  if(is.null(ts_prediction)){
    set.seed(set_seed)
    ts_prediction <- smartAgg(df,"date",function(x){x[sample(1:length(x),1)]},"time",catN = F)$time #min(x,na.rm=T)
    if (horizon == 24) {
      #ts_prediction <- ts_prediction[-length(ts_prediction)]
      ts_prediction[length(ts_prediction)] <- (as.POSIXct(x = sprintf("%i-%02i-%02i 23:00:00 UTC", 
                                                                      year(ts_prediction[length(ts_prediction)]), 
                                                                      month(ts_prediction[length(ts_prediction)]), 
                                                                      day(ts_prediction[length(ts_prediction)])), tz = "UTC")-days(1))
    }
  }
  # hp_tset_24h <- c(rep("42", horizon))
  # ts_prediction <- df$time[1]
  
  # Simulate by sets of 24h from the ts_prediction
  df <- lapply(ts_prediction,function(ts_){
    
    # Filter the initial dataset 
    # ts_ = ts_prediction[1]
    # df_ <- df[df$time>=ts_ & df$time<=(ts_+hours(23)),]
    df_ <- df[df$time>=(ts_) & df$time<=(ts_+hours(horizon-1)),]
    df_$ts_prediction <- ts_
    
    # Assign the control variables of the scenario
    df_$hp_tset_l0 <- as.numeric(hp_tset_24h[df$time %in% df_$time])
    df_$hp_status_l0 <- factor(ifelse(is.na(hp_tset_24h[df$time %in% df_$time]),0,1),levels=c(0,1),labels=c(0,1))
    for(k in 1:max(as.numeric(gsub("hp_status_l","",colnames(df_)[grepl("hp_status_l",colnames(df_))])))){
      df_[,paste0("hp_status_l",k)] <- factor(df_[,paste0("hp_status_l",k)],levels=c(0,1),labels=c(0,1))
    }
    
    # # Force the available levels for ventilatio
    # tryCatch({
    #   df_$vent_status_l0 <- factor(df_$vent_status_l0,labels=1:3,levels=1:3)
    #   df_$vent_status_l1<- factor(df_$vent_status_l1,labels=1:3,levels=1:3)
    #   df_$vent_status_l2<- factor(df_$vent_status_l2,labels=1:3,levels=1:3)
    #   df_$vent_status_l3<- factor(df_$vent_status_l3,labels=1:3,levels=1:3)
    #   df_$vent_status_l4<- factor(df_$vent_status_l4,labels=1:3,levels=1:3)
    # },error=function(e){return(-1000)})

    # Iterate for each timestep (1 hour)
    for (i in 1:nrow(df_)){
      #i=1 i=nrow(df_) i=21
      # Calculate the floor temperature and indoor temperature under free floating conditions
      df_$hp_cons_l0[i] <- 0
      tsupply_ff <- df_$tsupply_l0[i] <- predict(mod_tsupply, df_[i,])
      df_$hp_cop_l0[i] <- 0
      df_$hp_thermal_l0[i] <- df_$hp_cons_l0[i]*df_$hp_cop_l0[i]
      ti_ff <- df_$ti_l0[i] <- (model.matrix(mod_ti@nuisance$formula, df_)[,colnames(model.matrix(mod_ti@nuisance$formula, df_)) %in% names(coef(mod_ti))] %*% coef(mod_ti))[i]#predict(mod_ti, df_[i,])

      # If the heat pump should be on, estimate the heat pump consumption and 
      # re-estimate the floor and indoor temperatures considering the heat input.
      if(df_$hp_status_l0[i]==1 && !is.na(tsupply_ff) && !is.na(ti_ff)){
        
        df_$tsupply_l0[i] <- df_$hp_tset_l0[i]
        df_$hp_cons_l0[i] <- predict(mod_q, df_[i,])
        df_$hp_cop_l0[i] <- predict(mod_cop, df_[i,])
        df_$hp_thermal_l0[i] <- df_$hp_cons_l0[i]*df_$hp_cop_l0[i]
        
        if(df_$hp_thermal_l0[i]>0){
          # df_$tsupply_l0[i] <- predict(mod_tsupply, df_[i,])
          df_$ti_l0[i] <- (model.matrix(mod_ti@nuisance$formula, df_)[,colnames(model.matrix(mod_ti@nuisance$formula, df_)) %in% names(coef(mod_ti))] %*% coef(mod_ti))[i]#predict(mod_ti, df_[i,])
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
      for (l in 1:max(params[c("mod_hp_cons_ar","mod_tsupply_lags_hp_cons","mod_tsupply_ar","mod_ti_lags_dti","mod_ti_lags_hp_cons")])){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("hp_cons_l",l)] <- df_$hp_cons_l0[i]}}, error=function(e){next})
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("hp_cop_l",l)] <- df_$hp_cop_l0[i]}}, error=function(e){next})
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("hp_thermal_l",l)] <- df_$hp_thermal_l0[i]}}, error=function(e){next})
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("hp_status_l",l)] <- df_$hp_status_l0[i]}}, error=function(e){next})
      }
      for (l in 1:max(params[c("mod_ti_ar","mod_ti_lags_infiltrations")])){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("ti_l",l)] <- df_$ti_l0[i]}}, error=function(e){next})
      }
      df_$dti[i] <- df_$ti_l0[i] - df_$tsupply_l0[i]
      for (l in 1:max(c(params[c("mod_ti_lags_dti","mod_tsupply_lags_dti")]))){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("dti_l",l)] <- df_$dti_l0[i]}}, error=function(e){next})
      }
      df_$dte_l0[i] <- (rowMeans(data.frame(df_$ti_l1[i],df_$ti_l0[i])) - rowMeans(data.frame(df_$te_l1[i],df_$te_l0[i])))
      df_$infiltrations_l0[i] <- -ifelse(df_$dte_l0[i]>0,df_$dte_l0[i],0) * df_$windSpeed[i]
      for (l in 1:max(c(params[c("mod_ti_lags_infiltrations")]))){
        tryCatch({if((i+l)<=nrow(df_)){df_[i+l,paste0("infiltrations_l",l)] <- df_$infiltrations_l0[i]}}, error=function(e){next})
      }
      df_$dtf_l0[i] <- df_$tsupply_l0[i] - df_$te_l0[i]
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

###
# Genetic Algorithm
###

decodeValueFromBin <- function(binary_representation, class_per_feature, nclasses_per_feature, 
                               levels_per_feature = NULL, min_per_feature = NULL, max_per_feature = NULL){
  
  bitOrders <- mapply(function(x) { nchar(toBin(x)) }, nclasses_per_feature)
  #binary_representation <- X
  binary_representation <- split(binary_representation, rep.int(seq.int(bitOrders), times = bitOrders))
  orders <- sapply(binary_representation, function(x) { binary2decimal(gray2binary(x)) })
  orders <- mapply(function(x){min(orders[x],nclasses_per_feature[x])},1:length(orders))
  orders <- mapply(
    function(x){
      switch(class_per_feature[x],
             "discrete"= levels_per_feature[[x]][orders[x]+1],
             "int"= floor(seq(min_per_feature[x],max_per_feature[x],
                              by=if(nclasses_per_feature[x]>0){
                                (max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])
                              }else{1}
             )[orders[x]+1]),
             "float"= seq(min_per_feature[x],max_per_feature[x],
                          by=if(nclasses_per_feature[x]>0){
                            (max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])
                          }else{1})[orders[x]+1]
      )
    }
    ,1:length(orders))
  return(unname(orders))
}

decodeBinFromValue <- function(values, class_per_feature, nclasses_per_feature,
                               levels_per_feature = NULL, min_per_feature = NULL, max_per_feature = NULL){
  # values=c(0,400)
  # class_per_feature=c("int","int")
  # nclasses_per_feature=c(4,5)
  # min_per_feature=c(0,0)
  # max_per_feature=c(400,500)
  #
  
  values <- mapply(
    function(x){
      
      switch(class_per_feature[x],
             "discrete"= which(levels_per_feature[[x]] %in% values[x])-1,
             "int"= which(seq(min_per_feature[x],max_per_feature[x],
                              by=(max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])) %in% values[x])-1,
             "float"= which(seq(min_per_feature[x],max_per_feature[x],
                                by=(max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])) %in% values[x])-1
      )
    }
    ,1:length(values))
  
  bitOrders <- mapply(function(x) { nchar(toBin(x)) }, nclasses_per_feature)
  binary_representation <- unlist(c(sapply(1:length(values), FUN=function(x) { binary2gray(decimal2binary(values[x],bitOrders[x])) })))
  
  return(binary_representation)
}

toBin<-function(x){ as.integer(paste(rev( as.integer(intToBits(x))),collapse="")) }

gaMonitor2 <- function (object, digits = getOption("digits"), ...)
{
  fitness <- na.exclude(object@fitness)
  cat(paste("GA | Iter =", object@iter, " | Mean =", format(mean(fitness),
                                                            digits = digits), " | Best =", format(max(fitness),
                                                                                                  digits = digits), "\n"))
  flush.console()
}

bee_uCrossover <- function(object, parents, nclasses_per_feature)
{
  parents <- object@population[parents,,drop = FALSE]
  u <- unlist(lapply(nclasses_per_feature,function(i)rep(runif(1),nchar(toBin(i)))))
  children <- parents
  children[1:2, u > 0.5] <- children[2:1, u > 0.5]
  out <- list(children = children, fitness = rep(NA,2))  
  return(out)
}

smartAgg <- function(df, by, ..., catN=T, printAgg=F) {
  args <- list(...)
  dt <- as.data.table(df)
  
  ## organizing agg Methods and variable names into 2 separate lists
  aggMethod <- list()
  vars <- list()
  j<-1
  for(i in seq(1,length(args),2)) {
    aggMethod[[j]] <- args[[i]]
    vars[[j]] <- args[[i+1]]
    if(class(vars[[j]]) %in% c('integer', 'numeric')) vars[[j]] <- names(df)[vars[[j]]]
    j<-j+1
  }
  
  ## creat line to exec
  k<-0
  varL <- vector()
  for(j in 1:length(aggMethod)){
    for(i in 1:length(vars[[j]])){
      if(vars[[j]][i] %in% names(df)){
        if(class(aggMethod[[j]])=='function') {
          afun <- paste0('af',j)
          assign(afun, aggMethod[[j]])
          laf2 <- as.list(formals(get(afun)))
          laf2[which(lapply(laf2, nchar)==0)] <- vars[[j]][i] 
          rhstmp <- paste(unlist(lapply(seq_along(laf2), function(y,n,i) paste0(n[[i]], '=', y[[i]]), n=names(laf2), y=laf2)), collapse=',')
          tmp <- paste(vars[[j]][i], '=', afun, '(', rhstmp, ')', sep='') # anonymous functions
        } else {         
          tmp <- paste(vars[[j]][i], '=', aggMethod[[j]], '(', vars[[j]][i], ')', sep='') #non-anonymous functions
        }
        k <- k+1
        varL[k] <- tmp
      } else {print(paste('WARNING: ', vars[[j]][i], ' not in dataframe', sep=''))}
    }
  }
  varL <- paste(varL, collapse=', ')
  if(catN==T) varL <- paste(varL, ',countPerBin=length(', vars[[1]][1], ')', sep='')  
  
  ## actually creating aggregation command and executing it
  line2exec <- paste('dtAgg <- dt[,list(', varL, '), by=list(', paste(by,collapse=','), ')]', sep='')
  if(printAgg==T) print(line2exec)
  eval(parse(text=line2exec))
  dfAgg <- data.frame(dtAgg)
  
  return(dfAgg)
}

rmserr <- function (x, y, summary = FALSE){
  if (!is.numeric(x) || !is.numeric(y)) 
    stop("Arguments 'x' and 'y' must be numeric vectors.")
  if (length(x) != length(y)) 
    stop("Vectors 'x' and ' y' must have the same length.")
  n <- length(x)
  mae <- sum(abs(y - x),na.rm=T)/n
  mae_f <- formatC(mae, digits = 4, format = "f")
  mse <- sum((y - x)^2,na.rm=T)/n
  mse_f <- formatC(mse, digits = 4, format = "f")
  rmse <- sqrt(sum((y - x)^2,na.rm=T)/n)
  rmse_f <- formatC(rmse, digits = 4, format = "f")
  mape <- sum(abs((y - x)/x),na.rm=T)/n
  mape_f <- formatC(mape, digits = 4, format = "f")
  nmse <- sum((y - x)^2,na.rm=T)/sum((x - mean(x))^2,na.rm=T)
  nmse_f <- formatC(nmse, digits = 4, format = "f")
  rstd <- sqrt(sum((y - x)^2)/n)/mean(x)
  rstd_f <- formatC(rstd, digits = 4, format = "f")
  if (summary) {
    cat("-- Error Terms --------------------------------------------------\n")
    cat(" MAE:  ", mae_f, "  \t- mean absolute error (in range [", 
        range(x), "])\n")
    cat(" MSE:  ", mse_f, "  \t- mean squared error (the variance?!)\n")
    cat(" RMSE: ", rmse_f, "  \t- root mean squared error (std. dev.)\n")
    cat(" MAPE: ", mape_f, "  \t- mean absolute percentage error\n")
    cat(" LMSE: ", nmse_f, "  \t- normalized mean squared error\n")
    cat(" rSTD: ", rstd_f, "  \t- relative standard deviation (", 
        mean(x), ")\n")
    cat("-----------------------------------------------------------------\n")
  }
  R <- list(mae = mae, mse = mse, rmse = rmse, mape = mape, 
            nmse = nmse, rstd = rstd)
  if (summary) {
    invisible(R)
  }
  else {
    return(R)
  }
}

#function to take the optimal models parameters used in a GA
optimizer_model_parameters <- function(X, class_per_feature, nclasses_per_feature, min_per_feature, 
                                       max_per_feature, names_per_feature, df, train_dates, val_dates, horizon, set_seed){
  #X=sample(c(0,1),nBits,replace=T)
  #params <- readRDS("params_nice")
  
  params <- decodeValueFromBin(X, class_per_feature, nclasses_per_feature, min_per_feature = min_per_feature, 
                               max_per_feature = max_per_feature)
  names(params) <- names_per_feature
  
  # Training of the models
  #tryCatch({
    mod_q <- calculate_model_q(params, df, train_dates, output="model")$mod
    mod_cop <- calculate_model_cop(params, df, train_dates, output="model")$mod
    mod_tsupply <- calculate_model_tsupply(params, df, train_dates, output="model")$mod
    mod_ti <- calculate_model_ti(params, df, train_dates, output="model")$mod
  #},error=function(e){return(-1000)})
  
  # rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates
  # hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset)
  # ts_prediction = NULL

  # Validation of the models in 24h predictions
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    mod_cop = mod_cop,
    df = df,
    rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates,
    hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    ts_prediction = NULL,
    set_seed = set_seed,
    horizon = horizon
  )
  
  # Accuracy indoor temperature and consumption
  q_diff <- rmserr(predv$hp_cons[is.finite(predv$hp_cons) & predv$hp_cons>0],
                   predv$hp_cons_l0[is.finite(predv$hp_cons) & predv$hp_cons>0])$rmse
  ti_diff <- rmserr(predv$ti[is.finite(predv$ti)],predv$ti_l0[is.finite(predv$ti)])$rmse
  tsupply_diff <- rmserr(predv$tsupply[is.finite(predv$tsupply)],predv$tsupply_l0[is.finite(predv$tsupply)])$rmse
  
  # # Autocorrelations
  # a <- acf(mod_ti@nuisance$df$ti_l0-mod_ti@fitted,plot = F) #mod_ti@fitted #mod_ti$model$ti_l0-mod_ti$fitted.values
  # inc_ti <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
  # a <- acf(mod_q$model$hp_cons_l0-mod_q$fitted.values,plot = F)
  # inc_q <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
  # a <- acf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,plot = T)
  # inc_tsupply <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
  #
  # # Impulse responses
  # mod_ti_te <- sum(abs(plot_irf(mod_ti$coefficients,24,"^te_l",plot = F,exhogenous_coeff = T)$value)>10)
  # mod_ti_value <- sum(plot_irf(mod_ti$coefficients,24,"^value_l",plot = F,exhogenous_coeff = T)$value<(-0.05))
  # mod_q_dte  <- sum(abs(plot_irf(mod_q$coefficients,24,"^dte_l",plot = F,exhogenous_coeff = T)$value)>10)
  # mod_q_dti <- sum(abs(plot_irf(mod_q$coefficients,24,"^dti_l",plot = F,exhogenous_coeff = T)$value)>10)
  # mod_q_te <- sum(abs(plot_irf(mod_q$coefficients,24,"^te_l",plot = F,exhogenous_coeff = T)$value)>10)
  
  # # Percentage pvalue < 0.05 to all variables
  # pval_q <- 1-(sum(summary(mod_q)$coef[,4]<=0.05) / nrow(summary(mod_q)$coef))
  # pval_ti <- 1-(sum(summary(mod_ti)$coef[,4]<=0.05) / nrow(summary(mod_ti)$coef))
  # pval_tsupply <- 1-(sum(summary(mod_tsupply)$coef[,4]<=0.05) / nrow(summary(mod_tsupply)$coef))
  # plot(predv$time,predv$ti_l0, col="red",type="l");lines(predv$time,predv$ti,col="black")
  # print(ti_diff)
  score <- -ti_diff
    #- q_total_diff*inc_q - ti_diff*inc_ti - (mod_ti_te+mod_ti_value+mod_q_dte+mod_q_dti+mod_q_te)# inc_ti * inc_q
    # -(ti_diff*inc_ti*10*(1+params["mod_ti_ar"]*0.1) +
    #     q_diff*inc_q*4*(1+params["mod_hp_cons_ar"]*0.1) + 
    #     tsupply_diff*inc_tsupply*6*(1+params["mod_tsupply_ar"]*0.1) )# * (
          #(sum(pval_q,pval_ti,pval_tsupply)/3) #mean(pval_q,pval_ti,pval_tsupply)   )
  
  if (is.finite(score)){
    return(score)#-weighted.mean(,c(0.6,0.6,0.4)))
  } else {return(-10000000000000)}
}

#To run the MPC
optimizer_MPC <- function(X, class_per_feature, nclasses_per_feature, names_per_feature, levels_per_feature, 
                          df, mod_q, mod_ti, mod_tsupply, mod_cop, ti_min, ti_max, price, time_to_predict, params, horizon, weight, post_analysis = F){
  
  #X=sample(c(0,1),nBits,replace=T)
  
  params_hp_tset_24h <- decodeValueFromBin(X, class_per_feature, nclasses_per_feature, levels_per_feature = levels_per_feature)
  params_hp_tset_24h <- as.numeric(params_hp_tset_24h)
  # names(params_hp_tset_24h) <- names_per_feature
  
  # rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% as.Date(time_to_predict)
  # time_to_predict_24h = seq(from = time_to_predict, to = time_to_predict + hours(23), by = "hours")
  time_to_predict_24h = seq(from = time_to_predict, to = time_to_predict + hours(horizon-1), by = "hours")
  rows_to_filter = df$time %in% time_to_predict_24h
  hp_tset_24h = numeric(nrow(df))
  hp_tset_24h[rows_to_filter] <- params_hp_tset_24h
  
  # ts_prediction = time_to_predict
  # the predicted variables will be tagged as "_0" 
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_cop = mod_cop,
    mod_tsupply = mod_tsupply,
    df = df,
    rows_to_filter = rows_to_filter,
    hp_tset_24h = hp_tset_24h, #ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    horizon = horizon,
    ts_prediction = time_to_predict #min(df$time)
  )
  
  # Simetrical penalty is proposed (same penalty if the temperature bound in violated over or under)
  delta <- pmin((predv$ti_l0 - ti_min), (ti_max - predv$ti_l0))
  delta[delta>0] <- 0
  delta <- abs(delta)
  
  if (any(delta!=0)) {
    # Exponential parameter lambda
    lambda <- 0.90 #0.75 #0.7 #0.6 #0.2 #0.25 #0.35 #2

    # (Both parameters should be hardcoded outside, during trial session are going to stay here)
    
    # The soft restrictions have been implemented in two ways:
    # 1) Exponential growth of penalty 
    # 
    # exp_delta <- exp(lambda*delta)
    # penalty <- sum(exp_delta[exp_delta>1])
    # 
    # 
    # if (any(delta > delta_limit)){
    #   score = 5000000
    #   return(-score)
    # }
    
    # 2) Exponential growth of penalty with homographic function to include the asymptote in the delta_limit
    delta_penalty <- {}
    delta_limit = 4
    lambda <- 1#0.90 #0.75 #0.7 #0.6 #0.2 #0.25 #0.35 #2
    for (i in 1:length(delta)) {
      if ((delta[i] > 0)&(delta[i] < delta_limit)) {
        delta_penalty[i] <- penalty_function(delta[i], delta_limit = delta_limit, lambda)
      }else if (delta[i] >= delta_limit) {
        delta_penalty[i] <- abs(delta[i])^2*5000
      }else{
        delta_penalty[i] <- 0
      }
    }
    # delta_penalty[delta_penalty[i]<0] <- 50000
    
    # 3) Linear growth of penalty
    # delta_penalty <- 
    
    penalty <- delta_penalty*500000000
  } else {
    penalty <- 0
  }
  
  # Cost function: sum over 24 hours
  iteration_cost <- (price/1000000)*predv$hp_cons_l0
  score <- (sum(iteration_cost*weight)+sum((price*weight/1000000)*4000))*(1+sum(penalty*weight)) # (sum(iteration_cost) + 1) * (penalty + 1)
  #Arash score <- sum(iteration_cost) + 0.030*(sum(delta))
  
  if(post_analysis==T){
    return(data.frame("predv" = predv,
                      "penalty" = penalty,
                      "iteration_cost" = iteration_cost
    ))
  } else {
    return(-score)
  }
  
}

penalty_function <- function(x, delta_limit, lambda){
    y = (-exp(lambda*x)/(x-delta_limit))-(1/delta_limit)
  return(y)
}

######################################## Penalty-lambdas Optimization #########################################

penalty_optimizer <- function(X, min_per_penalty, max_per_penalty, nclasses_per_penalty, class_per_penalty, names_per_penalty, 
                              df, train_dates, val_dates, formula, mod_q, mod_tsupply, horizon, params) {
  #X=sample(c(0,1),nBits,replace=T)
  
  penalties <- decodeValueFromBin(X, class_per_penalty, nclasses_per_penalty, min_per_feature = min_per_penalty, 
                               max_per_feature = max_per_penalty)
  names(penalties) <- names_per_penalty
  
  #df is tunned to match the formula and is cutted to use only the train dates
  df_mod <- tune_model_input(df,params)
  df_mod <- df_mod[as.Date(df_mod$time,"Europe/Madrid") %in% train_dates,]
  df_mod <- df_mod[complete.cases(df_mod),]
  
  mod_ti <- penalized(response = df_mod$ti_l0, penalized = model.matrix(formula, df_mod), unpenalized = ~ 0,
                      lambda1 = penalties["L1"],lambda2 = penalties["L2"])#lambda1 = penalties["L1"],lambda2 = penalties["L2"]
  mod_ti@nuisance$formula <- formula
  mod_ti@nuisance$ti_l0 <- df_mod$ti_l0
  
  # rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates
  # hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset)
  # ts_prediction = NULL
  # mod_nonlin = NULL
  # hp_tset_24h_saved<-hp_tset_24h
  
  # Validation of the models in 24h predictions
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    df = df,
    rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates,
    hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    ts_prediction = NULL,
    horizon = horizon
  )
  
  # Accuracy indoor temperature and consumption
  q_diff <- rmserr(predv$hp_cons[is.finite(predv$hp_cons) & predv$hp_cons>0],
                   predv$hp_cons_l0[is.finite(predv$hp_cons) & predv$hp_cons>0])$rmse
  ti_diff <- rmserr(predv$ti[is.finite(predv$ti)],predv$ti_l0[is.finite(predv$ti)])$rmse
  tsupply_diff <- rmserr(predv$tsupply[is.finite(predv$tsupply)],predv$tsupply_l0[is.finite(predv$tsupply)])$rmse
  
  # # Autocorrelations
  a <- acf(mod_ti@nuisance$ti_l0-mod_ti@fitted,plot = F) #mod_ti@fitted #mod_ti$model$ti_l0-mod_ti$fitted.values
  inc_ti <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
  a <- acf(mod_q$model$hp_cons_l0-mod_q$fitted.values,plot = F)
  inc_q <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
  a <- acf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,plot = T)
  inc_tsupply <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))

  score <- #- q_total_diff*inc_q - ti_diff*inc_ti - (mod_ti_te+mod_ti_value+mod_q_dte+mod_q_dti+mod_q_te)# inc_ti * inc_q
    -(ti_diff*inc_ti*10*(1+params["mod_ti_ar"]*0.1) +
        q_diff*inc_q*4*(1+params["mod_hp_cons_ar"]*0.1) + 
        tsupply_diff*inc_tsupply*6*(1+params["mod_tsupply_ar"]*0.1) )# * (
  #(sum(pval_q,pval_ti,pval_tsupply)/3) #mean(pval_q,pval_ti,pval_tsupply)   )
  
  if (is.finite(score)){
    return(score)#-weighted.mean(,c(0.6,0.6,0.4)))
  } else {return(-10000000000000)}
}


all_penalties <- function(penaltyy, mod_q, mod_tsupply, df, val_dates, params, horizon){
  
  # library(foreach)
  # # library(doSNOW)
  # # library(Rmpi)
  # #setup parallel backend to use many processors
  # cores=detectCores()
  # cl <- makeCluster(cores[1]-1) #not to overload your computer
  # registerDoParallel(cl)
  # # registerDoSNOW(cl)
  # cl <- parallel::makeCluster(2)
  # doParallel::registerDoParallel(cl)
  
  for (i in 1:length(penaltyy$L1)) { #foreach(i=1:length(penaltyy$L1), .combine= 'c', .packages = "penalized") %dopar% { #
    print(i)
    penalty <- c(penaltyy[i,"L1"], penaltyy[i,"L2"])
    names(penalty) <- c("L1", "L2")
    result_ti <- calculate_model_ti(params, df, train_dates_1, output="model", penalty = penalty)
    mod_ti <- result_ti$mod
    
    predv <- prediction_scenario(
      mod_q = mod_q, 
      mod_ti = mod_ti,
      mod_tsupply = mod_tsupply,
      df = df,
      rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates,
      hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
      params = params,
      ts_prediction = NULL,
      horizon = horizon
    )
    
    # Accuracy indoor temperature and consumption
    q_diff <- rmserr(predv$hp_cons[is.finite(predv$hp_cons) & predv$hp_cons>0],
                     predv$hp_cons_l0[is.finite(predv$hp_cons) & predv$hp_cons>0])$rmse
    ti_diff <- rmserr(predv$ti[is.finite(predv$ti)],predv$ti_l0[is.finite(predv$ti)])$rmse
    tsupply_diff <- rmserr(predv$tsupply[is.finite(predv$tsupply)],predv$tsupply_l0[is.finite(predv$tsupply)])$rmse
    
    # # Autocorrelations
    a <- acf(mod_ti@nuisance$df$ti_l0-mod_ti@fitted,plot = F) #mod_ti@fitted #mod_ti$model$ti_l0-mod_ti$fitted.values
    inc_ti <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
    a <- acf(mod_q$model$hp_cons_l0-mod_q$fitted.values,plot = F)
    inc_q <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
    a <- acf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,plot = T)
    inc_tsupply <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
    
    score <- #- q_total_diff*inc_q - ti_diff*inc_ti - (mod_ti_te+mod_ti_value+mod_q_dte+mod_q_dti+mod_q_te)# inc_ti * inc_q
      -(ti_diff*inc_ti*10*(1+params["mod_ti_ar"]*0.1) +
          q_diff*inc_q*4*(1+params["mod_hp_cons_ar"]*0.1) + 
          tsupply_diff*inc_tsupply*6*(1+params["mod_tsupply_ar"]*0.1) )
    penaltyy[i,"score"] <- score
  }
  # parallel::stopCluster(cl)
  # stopCluster(cl)
  return(penaltyy)
}
