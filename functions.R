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

# Function to decompose time in multiple columns (For grouping purposes)

add_decomposition_time <- function(contractId_measures, time_column, tz="UTC"){
  contractId_measures$dayweek <- as.factor(strftime(contractId_measures[,time_column],"%u",tz=tz))
  contractId_measures$dayweek_int <- as.integer(as.character(contractId_measures$dayweek))
  contractId_measures$dayhour <- as.factor(data.table::hour(contractId_measures[,time_column]))
  contractId_measures$dayhour_int <- as.integer(as.character(contractId_measures$dayhour))
  contractId_measures$daypart <- as.factor(contractId_measures$dayhour_int)
  levels(contractId_measures$daypart) <- list(
    "night"=c(23,0,1,2,3,4),
    "morning"=5:11,
    "noon"=12:14,
    "afternoon"=15:19,
    "evening"=20:22
  )
  contractId_measures$month <- as.factor(data.table::month(contractId_measures[,time_column]))
  contractId_measures$month_int <- as.integer(as.character(contractId_measures$month))
  contractId_measures$season <- as.factor(contractId_measures$month_int)
  levels(contractId_measures$season) <- list(
    "winter"=c(12,1,2,3),
    "summer"=c(6,7,8,9),
    "midterm"=c(4,5,10,11)
  )
  return(contractId_measures)
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
  
  # Lag the input columns and weather tranformations
    # HP consumption
  for (l in 0:max(params[c("mod_tsupply_lags_hp_cons","mod_hp_cons_ar","mod_tsupply_ar","mod_ti_lags_dti")])){
    df[,paste0("hp_cons_l",l)] <- dplyr::lag(df[,"hp_cons"],l)
    df[,paste0("hp_status_l",l)] <- dplyr::lag(df[,"hp_status"],l)
    df[,paste0("hp_cop_l",l)] <- dplyr::lag(df[,"hp_cop"],l)
  }
  df[,"hp_tset_l0"] <- df[,"hp_tset"]
  for (l in 0:max(params[c("mod_hp_cons_lags_tsupply","mod_ti_lags_dti","mod_tsupply_ar")])){
    df[,paste0("tsupply_l",l)] <- dplyr::lag(df[,"tsupply"],l)
  }
    # Weather and indoor comfort
  for (l in 0:max(c(1,params[c("mod_hp_cons_ar","mod_hp_cons_lags_te","mod_hp_cons_lags_humidity","mod_ti_lags_te","mod_hp_cons_lags_tsupply",
                               "mod_ti_lags_infiltrations","mod_tsupply_lags_hp_cons")]))){
    df[,paste0("te_l",l)] <- dplyr::lag(df[,"te"],l)
    df[,paste0("te_raw_l",l)] <- dplyr::lag(df[,"te_raw"],l)
  }
  df$dtf <- df$tsupply - df$te
  for (l in 0:max(c(1,params[c("mod_hp_cons_lags_tsupply")]))){
    df[,paste0("dtf_l",l)] <- dplyr::lag(df[,"dtf"],l)
  }
  df$dti <- df$tsupply - df$ti
  for (l in 0:max(c(1,params[c("mod_ti_ar","mod_ti_lags_dti","mod_ti_lags_infiltrations")]))){
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
    # df[,paste0("air_h_l",l)] <- dplyr::lag(df[,"air_h"],l)
    # df[,paste0("air_e_l",l)] <- dplyr::lag(df[,"air_e"],l)
    # df[,paste0("air_s_l",l)] <- dplyr::lag(df[,"air_s"],l)
  }
  for (l in 0:max(params[c("mod_hp_cons_ar","mod_ti_lags_humidity","mod_hp_cons_lags_te",
                           "mod_hp_cons_lags_humidity","mod_tsupply_lags_hp_cons")])){
    df[,paste0("humidity_l",l)] <- dplyr::lag(df[,"humidity"],l)
  }
  df$dte <- (rowMeans(data.frame(df$ti_l1,df$ti_l0)) - rowMeans(data.frame(df$te_l1,df$te_l0)))
  df$infiltrations <- ifelse(df$dte>0,df$dte,0) * df$windSpeed
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
  
  return(df)
}

tune_model_input_fdd <- function(df,params){
  
  # Low pass filtering of the indoor and outdoor temperatures
  df$te_raw <- df$te
  df$te <- lp_vector(df$te,params["alpha_te"])
  df$GHI <- lp_vector(df$GHI,params["alpha_GHI"])
  df$BHI <- lp_vector(df$BHI,params["alpha_BHI"])
  df$windSpeed <- lp_vector(df$windSpeed,params["alpha_ws"])
  
  # Lag the input columns and weather tranformations
  # HP consumption
  for (l in 0:max(params[c("mod_tfloor_lags_hp_cons","mod_hp_cons_ar","mod_tfloor_ar")])){
    df[,paste0("hp_cons_l",l)] <- dplyr::lag(df[,"hp_cons"],l)
    df[,paste0("hp_status_l",l)] <- dplyr::lag(df[,"hp_status"],l)
  }
  for (l in 0:max(params[c("mod_hp_cons_lags_tfloor","mod_ti_lags_dti","mod_tfloor_ar")])){
    df[,paste0("tfloor_l",l)] <- dplyr::lag(df[,"tfloor"],l)
  }
  # Weather and indoor comfort
  for (l in 0:max(c(1,params[c("mod_hp_cons_ar","mod_hp_cons_lags_te","mod_hp_cons_lags_humidity","mod_ti_lags_te","mod_hp_cons_lags_tfloor",
                               "mod_ti_lags_infiltrations","mod_tfloor_lags_hp_cons")]))){
    df[,paste0("te_l",l)] <- dplyr::lag(df[,"te"],l)
    df[,paste0("te_raw_l",l)] <- dplyr::lag(df[,"te_raw"],l)
  }
  df$dtf <- df$tfloor - df$te
  for (l in 0:max(c(1,params[c("mod_hp_cons_lags_tfloor")]))){
    df[,paste0("dtf_l",l)] <- dplyr::lag(df[,"dtf"],l)
  }
  df$dti <- df$tfloor - df$ti
  for (l in 0:max(c(1,params[c("mod_ti_ar","mod_ti_lags_dti","mod_ti_lags_infiltrations")]))){
    df[,paste0("ti_l",l)] <- dplyr::lag(df[,"ti"],l)
  }
  for (l in 0:max(c(params[c("mod_ti_lags_dti","mod_tfloor_lags_dti")]))){
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
  for (l in 0:max(params[c("mod_hp_cons_ar","mod_ti_lags_humidity","mod_hp_cons_lags_te",
                           "mod_hp_cons_lags_humidity","mod_tfloor_lags_hp_cons")])){
    df[,paste0("humidity_l",l)] <- dplyr::lag(df[,"humidity"],l)
  }
  df$dte <- (rowMeans(data.frame(df$ti_l1,df$ti_l0)) - rowMeans(data.frame(df$te_l1,df$te_l0)))
  df$infiltrations <- ifelse(df$dte>0,df$dte,0) * df$windSpeed
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
  
  return(df)
}


fs <- function(X, nharmonics, pair_function=T) {
  do.call("c", lapply(1:nharmonics, function(i) {
    if(pair_function==T){
      val <- list(sin(i * X * 2 * pi), cos(i * X * 2 * pi))
      names(val) <- paste0(c("sin_", "cos_"), i)
      return(val)
    } else {
      val <- list(sin(i * X * 2 * pi))
      names(val) <- paste0(c("sin_"), i)
      return(val)
    }
  }))
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

plot_linear_GHI <- function(result,GHI_filter){
  #GHI_filter="GHI_c_occ|GHI_h_occ"
  
  sum_linear_df <- na.omit(data.frame("coefficients"=(result$mod$coefficients)))
  sum_linear_df$names <- rownames(sum_linear_df)
  
  GHI_matrix <- sum_linear_df[grepl(GHI_filter,sum_linear_df$names),]
  
  if (nrow(GHI_matrix)==1){
    
    coeffs <- GHI_matrix$coefficients
    Isol<-0:1100
    
    GHI_matrix <- data.frame(
      "temp"=Isol,
      "value"=Isol*coeffs,
      stringsAsFactors = F)
    
    if(result$mod$terms[[2]]=="value_l0"){
      g <- ggplot(GHI_matrix) + geom_line(aes(temp,value)) +
        ylab(bquote(Delta*Phi^"h"*"[kWh]")) + 
        xlab(bquote("I"^"sol"*"[W/m"^"2"*"]"))  + theme_bw() +
        theme(text= element_text(size=14, family="CM Roman"),axis.text.x=element_text(angle=0, hjust=1),
              legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman")) +
        ggtitle(bquote("I"^"sol"~"response"))
    } else {
      g <- ggplot(GHI_matrix) + geom_line(aes(temp,value)) +
        ylab(bquote(Delta*"T"^"i"*"["*degree*"C]")) + 
        xlab(bquote("I"^"sol"*"[W/m"^"2"*"]"))  + theme_bw() +
        theme(text= element_text(size=14, family="CM Roman"),axis.text.x=element_text(angle=0, hjust=1),
              legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman")) +
        ggtitle(bquote("I"^"sol"~"response"))
    }    
    
    return(g)
  }
  
  # Check if filtered df row's are equal to 0 
  is_empty <- nrow(GHI_matrix)
  if (is_empty == 0){
    return(NULL)
  }
  
  names <- strsplit(GHI_matrix$names,":")
  GHI_matrix$type <- ifelse(grepl("GHI_hvac_l0",rownames(GHI_matrix)),"GHI_q","GHI_t")
  lab_names <- list('GHI_t'="Temperature output",'GHI_q'="Consumption output")
  l_df <- do.call(rbind,lapply(unique(GHI_matrix$type),function(x){
    #x = "GHI_t"
    fs_df <- result$df[,(grepl(sprintf("%s|sunAz|sunEl",x),colnames(result$df)) & !grepl(sprintf("_l",x),colnames(result$df)))]
    #fs_df <- fs_df[!duplicated(fs_df$sunAzimuth_raw),]
    coeffs_v <- GHI_matrix[GHI_matrix$type==x,"coefficients"]
    uniqs <- fs_df[,"sunAz"]
    uniqs_elev <- fs_df[,"sunEl"]
    
    fs_df <- as.matrix(fs_df[,!(colnames(fs_df)%in%c(x,"sunEl","sunAz"))]) %*% diag(coeffs_v)#* fs_df[,x]
    p_df <- data.frame("sunAzimuth"=uniqs, "sunElevation"=uniqs_elev, "value"=rowSums(fs_df), "type"=x)
    p_df$value <- ifelse(p_df$sunElevation>0,p_df$value,NA)
    p_df
  }))
  
  levels(l_df$type) <- unlist(lab_names)[levels(l_df$type)]
  
  # #Faceted plot for heating and cooling GHI
  # g <- ggplot(l_df) + geom_line(aes(sunAzimuth, value)) + 
  #   geom_area(aes(sunAzimuth,ifelse(value>0,value,0)),fill="red", alpha=0.5) +
  #   geom_area(aes(sunAzimuth,ifelse(value<0,value,0)),fill="blue",alpha=0.5) +  
  #   facet_wrap(~type,ncol=1,strip.position = "right") + 
  #   ggtitle("Azimuth - GHI response") + theme_bw() + ylab(bquote(Delta*Phi^"h"))+ xlim(c(0,360)) +
  #   theme(axis.text.x=element_text(angle=0, hjust=1),text= element_text(size=14, family="CM Roman"),axis.title.x = element_blank(),
  #         legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman"))
  
  # Plot
  if("Consumption output" %in% levels(l_df$type)){
    max_val <- max(l_df$value,na.rm=T)
    g_h <- ggplot(l_df) + geom_line(aes(sunAzimuth, value)) + 
      geom_ribbon(aes(x=sunAzimuth,ymin=value,ymax=max_val),fill="red", alpha=0.5) +
      ggtitle(bquote("I"^"sol"*"response")) + theme_bw() + ylab(bquote(Delta*Phi^"h"*"[kWh] / "*"I"^"sol"*"[W/m"^"2"*"]"))+ xlim(c(0,360)) +
      xlab(bquote("sun azimuth ["*degree*"]")) +
      theme(axis.text.x=element_text(angle=0, hjust=1),text= element_text(size=14, family="CM Roman"),
            legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman"))
    g <- g_h
  }
  if("Temperature output" %in% levels(l_df$type)){
    min_val <- min(l_df$value,na.rm=T)
    g_c <- ggplot(l_df) + geom_line(aes(sunAzimuth, value)) +
      geom_ribbon(aes(x=sunAzimuth,ymin=min_val,ymax=value),fill="red", alpha=0.5) +
      ggtitle(bquote("I"^"sol"*"response")) + theme_bw() + ylab(bquote(Delta*"T"^"i"*"["*degree*"C] / "*"I"^"sol"*"[W/m"^"2"*"]")) + xlim(c(0,360)) +
      xlab(bquote("sun azimuth ["*degree*"]")) +
      theme(axis.text.x=element_text(angle=0, hjust=1),text= element_text(size=14, family="CM Roman"),
            legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman"))
    g <- g_c
  }
  
  return(g)
}
plot_linear_renovations <- function(result){
  
  sum_linear_df <- na.omit(data.frame("coefficients"=(result$mod$coefficients)))
  sum_linear_df$names <- rownames(sum_linear_df)
  
  wind_matrix <- sum_linear_df[grepl("dte_l0",sum_linear_df$names),]
  
  # Check if filtered df row's are equal to 0
  if (nrow(wind_matrix) < 2){
    return(NULL)
  }
  
  fs_df <- result$df[,grepl("dayhour_int",colnames(result$df)) | grepl(sprintf("dayhour_f"),colnames(result$df))]
  coeffs_v <- wind_matrix[,"coefficients"]
  uniqs <- fs_df[,"dayhour_int"]
  fs_df <- as.matrix(fs_df[,!(colnames(fs_df)%in%c("dayhour_int","dayhour_f"))]) %*% diag(coeffs_v)#* fs_df[,x]
  p_df <- data.frame("windBearing"=uniqs, "value"=rowSums(fs_df), "type"="infiltrations")
  
  if(result$mod$terms[[2]]=="value_l0"){
    min_val <- min(p_df$value,na.rm=T)
    g <- ggplot(p_df,aes(windBearing, value)) + geom_line() +
      geom_ribbon(aes(x=windBearing,ymin=min_val,ymax=value),fill="red", alpha=0.5) +
      ggtitle(bquote("W"^"s"*"·"*Psi~"response")) + theme_bw() + ylab(bquote(Delta*Phi^"h"*"[kWh]"*"/(W"["s"]*"[m/s]·"*Psi*"["*degree*"C])")) + xlim(c(0,24)) +
      xlab(bquote("wind direction ["*degree*"]")) +      
      theme(axis.text.x=element_text(angle=0, hjust=1),text= element_text(size=14, family="CM Roman"),
            legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman"))
  } else {
    max_val <- max(p_df$value,na.rm=T)
    g <- ggplot(p_df,aes(windBearing, value)) + geom_line() +
      geom_ribbon(aes(x=windBearing,ymin=value,ymax=max_val),fill="red", alpha=0.5) +
      ggtitle("W"^"s"*"·"*Psi~"response") + theme_bw() + ylab(bquote(Delta*"T"^"i"*"["*degree*"C]"*"/(W"["s"]*"[m/s]·"*Psi*"["*degree*"C])")) + xlim(c(0,24)) +
      xlab(bquote("wind direction ["*degree*"]")) +
      theme(axis.text.x=element_text(angle=0, hjust=1),text= element_text(size=14, family="CM Roman"),
            legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman"))
  }
  return(g)
}

plot_linear_infiltrations <- function(result){
  
  sum_linear_df <- na.omit(data.frame("coefficients"=(result$mod$coefficients)))
  sum_linear_df$names <- rownames(sum_linear_df)
  
  wind_matrix <- sum_linear_df[grepl("infiltrations_l0",sum_linear_df$names),]
  
  if (nrow(wind_matrix)==1){
    
    coeffs <- wind_matrix$coefficients
    windtemp<-0:50
    
    windtemp_matrix <- data.frame(
      "temp"=windtemp,
      "value"=windtemp*coeffs,
      stringsAsFactors = F)
    
    if(result$mod$terms[[2]]=="value_l0"){
      g <- ggplot(windtemp_matrix) + geom_line(aes(temp,value)) +
        ylab(bquote(Delta*Phi^"h"*"[kWh]")) + 
        xlab(bquote("W"^"s"*"[m/s]·"*Psi*"["*degree*"C]"))  + theme_bw() +
        theme(text= element_text(size=14, family="CM Roman"),axis.text.x=element_text(angle=0, hjust=1),
              legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman")) +
        ggtitle(bquote("W"^"s"*"·"*Psi~"response"))
    } else {
      g <- ggplot(windtemp_matrix) + geom_line(aes(temp,value)) +
        ylab(bquote(Delta*"T"^"i"*"["*degree*"C]")) + 
        xlab(bquote("W"^"s"*"[m/s]·"*Psi*"["*degree*"C]"))  + theme_bw() +
        theme(text= element_text(size=14, family="CM Roman"),axis.text.x=element_text(angle=0, hjust=1),
              legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman")) +
        ggtitle(bquote("W"^"s"*"·"*Psi~"response"))
    }    
    
    return(g)
  }
  
  # Check if filtered df row's are equal to 0 
  is_empty <- nrow(wind_matrix)
  if (is_empty == 0){
    return(NULL)
  }
  
  fs_df <- result$df[,grepl("windBearing",colnames(result$df)) & !grepl(sprintf("_l","windSpeed"),colnames(result$df))]
  coeffs_v <- wind_matrix[,"coefficients"]
  uniqs <- fs_df[,"windBearing"]
  fs_df <- as.matrix(fs_df[,!(colnames(fs_df)%in%c("windBearing","windBearing_f"))]) %*% diag(coeffs_v)#* fs_df[,x]
  p_df <- data.frame("windBearing"=uniqs, "value"=rowSums(fs_df), "type"="infiltrations")
  
  if(result$mod$terms[[2]]=="value_l0"){
    min_val <- min(p_df$value,na.rm=T)
    g <- ggplot(p_df,aes(windBearing, value)) + geom_line() +
      geom_ribbon(aes(x=windBearing,ymin=min_val,ymax=value),fill="red", alpha=0.5) +
      ggtitle(bquote("W"^"s"*"·"*Psi~"response")) + theme_bw() + ylab(bquote(Delta*Phi^"h"*"[kWh]"*"/(W"["s"]*"[m/s]·"*Psi*"["*degree*"C])")) + xlim(c(0,360)) +
      xlab(bquote("wind direction ["*degree*"]")) +      
      theme(axis.text.x=element_text(angle=0, hjust=1),text= element_text(size=14, family="CM Roman"),
            legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman"))
  } else {
    max_val <- max(p_df$value,na.rm=T)
    g <- ggplot(p_df,aes(windBearing, value)) + geom_line() +
      geom_ribbon(aes(x=windBearing,ymin=value,ymax=max_val),fill="red", alpha=0.5) +
      ggtitle("W"^"s"*"·"*Psi~"response") + theme_bw() + ylab(bquote(Delta*"T"^"i"*"["*degree*"C]"*"/(W"["s"]*"[m/s]·"*Psi*"["*degree*"C])")) + xlim(c(0,360)) +
      xlab(bquote("wind direction ["*degree*"]")) +
      theme(axis.text.x=element_text(angle=0, hjust=1),text= element_text(size=14, family="CM Roman"),
            legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman"))
  }
  return(g)
}

plot_linear_dte <- function(mod){
  
  # Check if output variable of the model is consumption
  if(mod$terms[[2]]!="value_l0"){
    return(NULL)
  }
  
  sum_linear_df <- na.omit(data.frame("coefficients"=(mod$coefficients)))
  sum_linear_df$names <- rownames(sum_linear_df)
  temp_matrix <- sum_linear_df[grepl("dte_l0",sum_linear_df$names),]
  
  # Check if filtered df row's are equal to 0 
  is_empty <- nrow(temp_matrix)
  if (is_empty == 0){
    return(NULL)
  }
  
  coeffs <- temp_matrix$coefficients
  temp<-0:20
  
  limits_y <- c(-5,10)
  
  temp_matrix <- data.frame(
    "temp"=temp,
    "value"=temp*coeffs,
    stringsAsFactors = F)
  
  g <- ggplot(temp_matrix) + geom_line(aes(temp,value)) +
    ylab(bquote(Delta*Phi^"h"*"[kWh]")) + 
    xlab(bquote(Psi*"["*degree*"C]"))  + theme_bw() +
    ylim(limits_y) + 
    theme(text= element_text(size=14, family="CM Roman"),axis.text.x=element_text(angle=0, hjust=1),
          legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman")) +
    ggtitle(bquote(Psi~" response"))
  
  return(g)
}

plot_linear_dti <- function(mod){
  
  # Check if output variable of the model is consumption
  if(mod$terms[[2]]!="value_l0"){
    return(NULL)
  }
  
  sum_linear_df <- na.omit(data.frame("coefficients"=(mod$coefficients)))
  sum_linear_df$names <- rownames(sum_linear_df)
  temp_matrix <- sum_linear_df[grepl("dti_l0",sum_linear_df$names),]
  
  # Check if filtered df row's are equal to 0 
  is_empty <- nrow(temp_matrix)
  if (is_empty == 0){
    return(NULL)
  }
  
  coeffs <- temp_matrix$coefficients
  temp<-0:6
  
  limits_y <- c(-5,10)
  
  temp_matrix <- data.frame(
    "temp"=temp,
    "value"=temp*coeffs,
    stringsAsFactors = F)
  
  g <- ggplot(temp_matrix) + geom_line(aes(temp,value)) +
    ylab(bquote(Delta*Phi^"h"*"[kWh]")) + 
    xlab(bquote(Theta*"["*degree*"C]"))  + theme_bw() +
    ylim(limits_y) + 
    theme(text= element_text(size=14, family="CM Roman"),axis.text.x=element_text(angle=0, hjust=1),
          legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman")) +
    ggtitle(bquote(Theta~" response"))
  
  return(g)
}

plot_linear_te <- function(mod){
  
  # Check if output variable of the model is consumption
  if(mod$terms[[2]]!="value_l0"){
    return(NULL)
  }
  
  sum_linear_df <- na.omit(data.frame("coefficients"=(mod$coefficients)))
  sum_linear_df$names <- rownames(sum_linear_df)
  temp_matrix <- sum_linear_df[grepl("^te_l0",sum_linear_df$names),]
  
  # Check if filtered df row's are equal to 0 
  is_empty <- nrow(temp_matrix)
  if (is_empty == 0){
    return(NULL)
  }
  
  coeffs <- temp_matrix$coefficients
  temp<-0:20
  
  limits_y <- c(-5,10)
  
  temp_matrix <- data.frame(
    "temp"=temp,
    "value"=temp*coeffs,
    stringsAsFactors = F)
  
  g <- ggplot(temp_matrix) + geom_line(aes(temp,value)) +
    ylab(bquote(Delta*Phi^"h"*"[kWh]")) + 
    xlab(bquote("T"^"e"*"["*degree*"C]"))  + theme_bw() +
    ylim(limits_y) + 
    theme(text= element_text(size=14, family="CM Roman"),axis.text.x=element_text(angle=0, hjust=1),
          legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman")) +
    ggtitle(bquote("T"^"e"~" response"))
  
  return(g)
}

plot_linear_ti <- function(mod){
  
  # Check if output variable of the model is consumption
  if(mod$terms[[2]]!="value_l0"){
    return(NULL)
  }
  
  sum_linear_df <- na.omit(data.frame("coefficients"=(mod$coefficients)))
  sum_linear_df$names <- rownames(sum_linear_df)
  temp_matrix <- sum_linear_df[grepl("^ti_l0",sum_linear_df$names),]
  
  # Check if filtered df row's are equal to 0 
  is_empty <- nrow(temp_matrix)
  if (is_empty == 0){
    return(NULL)
  }
  
  coeffs <- temp_matrix$coefficients
  temp<-15:25
  
  limits_y <- c(-5,10)
  
  temp_matrix <- data.frame(
    "temp"=temp,
    "value"=temp*coeffs,
    stringsAsFactors = F)
  
  g <- ggplot(temp_matrix) + geom_line(aes(temp,value)) +
    ylab(bquote(Delta*Phi^"h"*"[kWh]")) + 
    xlab(bquote("T"^"i"*"["*degree*"C]"))  + theme_bw() +
    ylim(limits_y) + 
    theme(text= element_text(size=14, family="CM Roman"),axis.text.x=element_text(angle=0, hjust=1),
          legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman")) +
    ggtitle(bquote("T"^"i"~" response"))
  
  return(g)
}

printlm <- function(mod, df, value_column, value_repr, ncol, irf_objects=NULL){
  
  plot_list <- list(
    #"dte"= plot_linear_dte(mod),
    #"dti"= plot_linear_dti(mod),
    #"te_l0"= plot_linear_te(mod),
    #"ti_l0"= plot_linear_ti(mod),
    "GHI"= plot_linear_GHI(result = list("mod"=mod,"df"=df),GHI_filter="GHI_l0|GHI_hvac_l0"),
    "renovations"= plot_linear_renovations(result = list("mod"=mod,"df"=df)),
    "infiltrations"= plot_linear_infiltrations(result = list("mod"=mod,"df"=df))
  )
  if(!is.null(irf_objects)){
    plot_list <- modifyList(plot_list, irf_objects)
  }
  plot_list <- plot_list[!vapply(plot_list, is.null, logical(1))]
  print(plot_grid(plotlist=plot_list,ncol = ncol,align = c("h","v")))

}

calculate_model_ti <- function(params, df, train_dates, output="aic"){
  
  
  df <- tune_model_input(df,params)
  
  df <- df[as.Date(df$time,"Europe/Madrid") %in% train_dates,]
  df <- df[complete.cases(df),]
  
  # Formula definition. Base formula + GHI and windSpeed terms
  formula <-  as.formula(sprintf("ti_l0 ~ 
                                 0 + %s + %s + %s + %s",
                                 paste0("ti_l",1:params["mod_ti_ar"],"",collapse=" + "),
                                 paste0(mapply(function(x){sprintf("tsupply_l%s:as.factor(hp_status_l%s)",x,x)},0:params["mod_ti_lags_dti"]),collapse=" + "),
                                 #paste0("tsupply_l",0:(params["mod_ti_lags_dti"]),collapse=" + "),
                                 paste0("te_l",0:(params["mod_ti_lags_te"]),"",collapse=" + "),
                                 paste0("hg_l",0:(params["mod_ti_lags_hg"]),"",collapse=" + ")
                                 #paste0("vent_l",0:(params["mod_ti_lags_ventilation"]),"",collapse=" + ")
  ))
  
  # Define the sunAzimuth fourier series terms and add the GHI terms to the formula
  if(params["mod_ti_solar_gains"]==1){
    formula <- update.formula(formula,paste0(". ~ . +",paste0("BHI_l",0:params["mod_ti_lags_BHI"],"",collapse=" + ")))
  } else if (params["mod_ti_solar_gains"]==2){
    for (i in 0:params["mod_ti_lags_BHI"]){
      sunAzimuth_fs_terms <- colnames(df)[grepl(paste0("^sunAzimuth_fs_l",i),colnames(df))]
      solar_features <- lapply(sunAzimuth_fs_terms,function(x){paste0("BHI_l",i,":",x)})
      formula <- update.formula(formula,paste0(". ~ . + ",do.call(paste,list(solar_features,collapse=" + "))))
    }
  }
  # Define the windBearing fourier series terms and add the infiltration terms to the formula
  if(params["mod_ti_infiltrations"]==1){
    formula <- update.formula(formula,paste0(". ~ . +",paste0("infiltrations_l",0:params["mod_ti_lags_infiltrations"],"",collapse=" + ")))
  } else if (params["mod_ti_infiltrations"]==2){
    for (i in 0:params["mod_ti_lags_infiltrations"]){#(params[3]-1)){#0){#
      windBearing_fs_terms <- colnames(df)[grepl(paste0("^windBearing_fs_l",i),colnames(df))]
      infiltrations_features <- lapply(windBearing_fs_terms,function(x){paste0("infiltrations_l",i,":",x)})
      formula <- update.formula(formula,paste0(". ~ . + ",do.call(paste,list(infiltrations_features,collapse=" + "))))
    }
  }
  
  mod <- lm(formula, data=df)
  
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

calculate_model_q <- function(params, df, train_dates, output="aic"){
  

  df <- tune_model_input(df,params)
  
  
  df <- df[as.Date(df$time,"Europe/Madrid") %in% train_dates,]
  df <- df[complete.cases(df),]
  
  # Formula definition. Base formula + GHI and windSpeed terms
  formula <- as.formula(sprintf("hp_cons_l0 ~ 
      1 + %s + %s + %s",
      paste0("bs(tsupply_l",0:params["mod_hp_cons_lags_tsupply"],")",collapse=" + "),
      paste0(mapply(function(x){sprintf("bs(tsupply_l%s):bs(ti_l%s)",x,x)},1:1),collapse=" + "),
      paste0("te_raw_l",0:params["mod_hp_cons_lags_te"],"",collapse=" + ")
      # paste0(mapply(function(x){sprintf("hp_cons_l%s:humidity_l%s",x,x)},1:params["mod_hp_cons_ar"]),collapse=" + "),
      # paste0(mapply(function(x){sprintf("hp_cons_l%s:te_l%s",x,x)},1:params["mod_hp_cons_ar"]),collapse=" + "),
      # paste0("hp_cons_l",1:params["mod_hp_cons_ar"],"",collapse=" + "),
      # paste0("te_l",0:params["mod_hp_cons_lags_te"],"",collapse=" + "),
      # paste0("bs(dtf_l",0:params["mod_hp_cons_lags_tsupply"],")",collapse=" + "),
      # paste0("bs(tsupply_l",0:params["mod_hp_cons_lags_tsupply"],")",collapse=" + "),
      # #paste0("ti_l",0:params["lags_dti"],"",collapse=" + "),
      # paste0("humidity_l",0:params["mod_hp_cons_lags_humidity"],"",collapse=" + ")
      #paste0(mapply(function(x){sprintf("bs(te_l%s,knots=1,degree=2):bs(humidity_l%s,knots=1,degree=2)",x,x)},0:max(params[c("mod_hp_cons_lags_te","mod_hp_cons_lags_humidity")])),collapse=" + ")
  ))
  
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

calculate_model_tsupply <- function(params, df, train_dates, output="aic"){
  
 
  df <- tune_model_input(df,params)
  
  df <- df[as.Date(df$time,"Europe/Madrid") %in% train_dates,]
  df <- df[complete.cases(df),]
  
  # Formula definition. Base formula + GHI and windSpeed terms
  formula <- as.formula(sprintf("tsupply_l0 ~ 
      0 + %s + %s",
      # paste0(mapply(function(x){sprintf("bs(tsupply_l%s,degree=2)*as.factor(hp_status_l%s)",x,x-1)},1:params["mod_tsupply_ar"]),collapse=" + "),
      paste0(mapply(function(x){sprintf("bs(tsupply_l%s)*as.factor(hp_status_l%s)",x,x-1)},1:params["mod_tsupply_ar"]),collapse=" + "),
      paste0(mapply(function(x){sprintf("hp_cons_l%s*te_raw_l%s",x,x)},0:params["mod_tsupply_lags_hp_cons"]),collapse=" + ")
      #paste0("te_l",0:params["lags_te"],"",collapse=" + "),
      #paste0("humidity_l",0:params["lags_humidity"],"",collapse=" + "),
      #paste0("hg_l",0:params["lags_hg"],"",collapse=" + "),
      #paste0("ti_l",1:max(1,params["lags_ti"]),"",collapse=" + ")
  ))
  
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

plot_residuals <- function(mod=NULL, value_column=NULL, value_repr, residuals =NULL){
  if(is.null(residuals)){
    residuals <- mod$model[,value_column] - mod$fitted.values
  }
  layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE))
  cex=2
  par(cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex, family = "CM Roman",mar=c(4.5,6,2,2))
  acf(residuals,main="",lag.max = 24)
  title("", line=0.7)
  pacf(residuals,ylim=c(-1,1),main="",lag.max = 23)
  title("", line=0.7)
  cpgram(residuals, main="")
  title("Correlogram", line=0.7)
  #qqnorm(residuals[residuals >quantile(residuals,0.05,na.rm=T) & residuals < quantile(residuals,0.95,na.rm=T)],main = "")
  #title("Q-Q plot", line=0.7)
  #qqline(residuals[residuals >quantile(residuals,0.05,na.rm=T) & residuals < quantile(residuals,0.95,na.rm=T)])
  # hist(residuals,breaks = 50,prob=T,xlim=c(mean(residuals)-4*sd(residuals),mean(residuals)+4*sd(residuals)),
  #      ylim=c(0,max(density(residuals)$y)))
  # lines(density(residuals),col=2)
  plot(residuals, type="l",ylim=c(mean(residuals)-5*sd(residuals),mean(residuals)+5*sd(residuals)),
       cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex, ylab=value_repr)
  # title(paste0("Structure: ",str_val," \n n/m order: ",floor(mod$n),"/",floor(mod$m), " - alphaTemp = ",round(mod$lpf_temperature_coeff,2), 
  #              "\n balanceTemp = ",round(mod$tbal,2)," ºC"), line=0.7)
}





printgam <- function (mod, value_column, value_repr, irf_objects, ncol=2, ask = TRUE, pages = 1,...){

  labeller_ <- function(lab){
    if(grepl("dayhour",lab)){
      lab <-gsub("_f_"," ", lab)
      lab <- gsub("dayhour","h", lab)
      lab <- gsub(" s"," in seasonality ", lab)
      return(lab)
    }
    if(grepl("occupancy_tf",lab)){
      lab <- gsub("as.factor(as.logical(occupancy_tf))","occupancy", lab)
      lab <- gsub("occupancy_tf_","occupancy ", lab)
      lab <- gsub("s"," in seasonality ", lab)
      return(lab)
    }
    if(grepl("dayweek",lab)){
      lab <-gsub("_f_"," ", lab)
      lab <- gsub("dayweek","d", lab)
      lab <- gsub(" s"," in seasonality ", lab)
      return(lab)
    }
    if(grepl("value_lag_",lab)){
      lab <- gsub("_f_"," ",lab)
      lab <- gsub("value_","",lab)
      lab <- paste(lab,"-lagged log(C)")
      return(lab)
    }
    return(
      switch(lab,
             `status` = "status", 
             `s(x)` = value_repr,
             `sunEl` = bquote("H"["t"]^"sol"),
             `sunAz` = bquote("A"["t"]^"sol"),
             `te_l0`=bquote("T"["t"]^"e,lp"),
             `ti_l1`=bquote("T"["t-1"]^"i"),
             `ti_l2`=bquote("T"["t-2"]^"i"),
             `ti_l3`=bquote("T"["t-3"]^"i"),
             `ti_l4`=bquote("T"["t-4"]^"i"),
             `value_l0`=bquote(Phi["t"]^"h"),
             `value_l1`=bquote(Phi["t-1"]^"h"),
             `value_l2`=bquote(Phi["t-2"]^"h"),
             `value_l3`=bquote(Phi["t-3"]^"h"),
             `value_l4`=bquote(Phi["t-4"]^"h"),
             `dti`=bquote(Theta["t"]),
             `dte`=bquote(Psi["t"]),
             `sunAzimuth`="Sun azimuth", 
             lab)
    )
  }
  xbreaks_ <- function(.l){
    if(grepl("dayhour",.l$ggObj$labels$x)){
      .l$data$fit <- .l$data$fit[as.character(.l$data$fit$x)!="NA",]
      .l<-.l + scale_x_discrete(breaks=c("00","12","23"),labels=c("00","12","23"))
    } else if(grepl("dayweek",.l$ggObj$labels$x)){
      .l$data$fit <- .l$data$fit[as.character(.l$data$fit$x)!="NA",]
      .l<-.l + scale_x_discrete(breaks=c("01","04","07"),labels=c("Mon","Thu","Sun"))
    }
    return(.l)
  }
  
  .addDefaultLayers <- function(.l) {
    .cl <- paste(.l$type, collapse = "")
    .l <- switch(.cl, fs1D = .l + l_fitLine() + theme(legend.position = "none"), 
                 `1D` = .l + l_fitLine() + l_ciLine() + ylab(labeller_("s(x)")) + xlab(labeller_(.l$ggObj$labels$x)),
                 `2D` = .l + l_fitRaster() + l_fitContour() + guides(fill=guide_colorbar(title = labeller_("s(x)"))) + 
                   xlab(labeller_(.l$ggObj$labels$x)) + ylab(labeller_(.l$ggObj$labels$y)), 
                 MD = .l + l_fitRaster() + l_fitContour(), 
                 MDslice = .l + l_fitRaster() + l_fitContour(), 
                 sos0 = .l + l_fitRaster() + l_fitContour(), 
                 sos1 = .l + l_fitRaster() + l_fitContour(), 
                 randomEffect = .l + l_fitLine() + l_ciLine() + l_points(), 
                 MultiRandomEffect = .l + l_fitLine() + l_ciLine() + l_points(), 
                 mrf = .l + l_poly(), 
                 PtermNumeric = .l + l_fitLine() + l_ciLine() + ylab(labeller_("s(x)")) + xlab(labeller_(.l$ggObj$labels$x)), 
                 PtermFactor = xbreaks_(.l) + l_ciBar() + l_fitPoints() + ylab(labeller_("s(x)")) + xlab(labeller_(.l$ggObj$labels$x)), 
                 PtermLogical = .l + l_ciBar() + l_fitPoints(), Multi1D = .l + 
                   l_fitLine(), Multi2D = .l + l_fitRaster() + 
                   l_fitContour(), MultiPtermNumeric = .l + l_ciBar() + 
                   l_fitPoints(), MultiPtermFactor = .l + l_ciBar() + 
                   l_fitPoints())
    .l <- .l + theme_bw() + theme(text= element_text(size=14, family="CM Roman"),
                                  legend.title = element_text(size=12),
                                  legend.text = element_text(size=12),
                                  axis.text = element_text(size=14, family="CM Roman"),
                                  plot.title = element_blank())
    return(.l)
  }
   
  b = getViz(mod)
  x = plot(b, allTerms=T)
  
  # Delete some of the resultant plots (In this case the ones with status0)
  x$plots <- lapply(1:length(x$plots), function(.kk) 
    if(grepl("status0",x$plots[[.kk]]$ggObj$labels$y)==F){
      if(grepl("status0",x$plots[[.kk]]$ggObj$labels$title)==F || length(x$plots[[.kk]]$ggObj$labels$title)==0 ){
        x$plots[[.kk]]
      }
    }
    )
  x$plots <- Filter(Negate(is.null), x$plots)
  
  # Generate the cleaned gList
  tmp <- lapply(1:length(x$plots), function(.kk) 
    if(!is.null(irf_objects)){
      if(sum(grepl(paste0(names(irf_objects),collapse="|"),x$plots[[.kk]]$ggObj$labels$x))==1){
        NULL
      } else {
        .addDefaultLayers(x$plots[[.kk]])$ggObj
      }
    } else {
      .addDefaultLayers(x$plots[[.kk]])$ggObj
    })
  
  # delete the autoregressive atomic plots
  list.condition <- !vapply(tmp, is.null, logical(1))
  tmp <- tmp[list.condition]
  
  # add the impulse response (the complete autoregressive description in a single plot)
  if(!is.null(irf_objects)){
    for(i in 1:length(irf_objects)){
      tmp[[length(tmp)+1]] <- irf_objects[[i]]
    }
  }
  
  return(grid.arrange(grobs = tmp, ncol = ncol))
}

plot_results <- function(mod, plot_file, value_column, value_repr, value_repr_residuals, df, height_coeffs=7,
                         width_coeffs=7, ncol_coeffs=2){
  
  # Print the plots
  if(!is.null(plot_file)){
    
    df_case_train <- df
    
    # Initialize the PDF if needed
    if (!("pdf" %in% names(dev.list()))){ pdf(sprintf(plot_file,"inputs"),width = 6,height = 10) }
    mod_df <- data.frame(
      "time"=df_case_train$time,
      "BHI"=df_case_train$BHI,
      "ti"=df_case_train$ti_l0,
      "te"=df_case_train$te_l0,
      "tsupply"=df_case_train$tsupply_l0,
      "hp_cons"=df_case_train$hp_cons_l0,
      "infiltrations"=df_case_train$infiltrations,
      "vent"=df_case_train$vent,
      "hg"=df_case_train$hg
    )
    print(
      ggplot(reshape2::melt(pad(mod_df),id.vars=c("time"))) + 
        geom_line(aes(time,as.numeric(value),col=variable,group=1)) + 
        facet_wrap(~variable,nrow=8,scales="free_y") + theme_bw() +
        ylab("") + scale_color_brewer(palette = "Dark2",
          name = "", labels = c(
            bquote("I"^"sol"~"[W/"*"m"^"2"*"]"),
            bquote("T"^"i"~"["*degree*"C]"),
            bquote("T"^"e"~"["*degree*"C]"),
            bquote("T"^"f"~"["*degree*"C]"),
            bquote(Phi^"h"~"[kWh]"),
            bquote("V"^"i"~"[m/s·"*degree*"C]"),
            bquote("V"^"a"~"[m"^"3"*"/h·"*degree*"C]"),
            bquote("G"^"h"~"[W]"))
          )+
        theme(text= element_text(size=15, family="CM Roman"),
              axis.text = element_text(size=15, family="CM Roman"),
              plot.title = element_blank(), legend.position="bottom",
              strip.background = element_blank(),
              strip.text.x = element_blank())
    )
    dev.off()
    embed_fonts(sprintf(plot_file,"inputs"), outfile=sprintf(plot_file,"inputs"))
    
    # Plot IRF
    irf_objects <- NULL
    if(value_column=="ti_l0"){
      if(sum(grepl("ti_l1",names(mod$coefficients)))>0){
        irf_objects <- list(
          "ti_l"=plot_irf(linear_coefficients=mod$coefficients, impulse_lags=168, pattern_ar_coefficients = "ti_l",
                   pattern_seasonality = NULL, title=bquote("T"["k<0"]^"i"*"=0;"~"T"["k">="0"]^"i"*"=1"),
                   name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
                   y_label=bquote(Delta*"T"["k"]^"i"*"["*degree*"C]")),
          "te_l"=plot_irf(linear_coefficients=mod$coefficients, impulse_lags=24, pattern_ar_coefficients = "^te_l",
                   pattern_seasonality = NULL, title=bquote("T"["k<0"]^"e,lp"*"=0;"~"T"["k">="0"]^"e,lp"*"=1"),
                   name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
                   y_label=bquote(Delta*"T"["k"]^"i"*"["*degree*"C]"), exhogenous_coeff=T),
          "te4_l"=plot_irf(linear_coefficients=mod$coefficients, impulse_lags=24, pattern_ar_coefficients = "te4_l",
                          pattern_seasonality = NULL, title=bquote("T"["k<0"]^"e,lp"^4*"=0;"~"T"["k">="0"]^"e,lp"^4*"=1"),
                          name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
                          y_label=bquote(Delta*"T"["k"]^"i"*"["*degree*"C]"), exhogenous_coeff=T),
          "value_l"=plot_irf(linear_coefficients=mod$coefficients, impulse_lags=24, pattern_ar_coefficients = "value_l",
                   pattern_seasonality = NULL, title=bquote(Phi["k<0"]^"h"*"=0;"~Phi["k">="0"]^"h"*"=1"),
                   name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
                   y_label=bquote(Delta*"T"["k"]^"i"*"["*degree*"C]"), exhogenous_coeff=T)
        )
      }
    } else if (value_column=="value_l0"){
        irf_objects <- list(
          "value_l"=plot_irf(linear_coefficients=mod$coefficients, impulse_lags=24, pattern_ar_coefficients = "value_l",
               pattern_seasonality = "status", title=bquote(Phi["k<0"]^"h"*"=0;"~Phi["k">="0"]^"h"*"=1"),
               name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
               y_label=bquote(Delta*Phi^"h"["k"]^"h")),
          "dte_l"=plot_irf(linear_coefficients=mod$coefficients, impulse_lags=24, pattern_ar_coefficients = "^dte_l",
                          pattern_seasonality = NULL, title=bquote(Psi["k<0"]*"=0;"~Psi["k">="0"]*"=1"),
                          name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
                          y_label=bquote(Delta*Phi^"h"["k"]^"h"), exhogenous_coeff=T),
          "dti_l"=plot_irf(linear_coefficients=mod$coefficients, impulse_lags=24, pattern_ar_coefficients = "dti_l",
                           pattern_seasonality = NULL, title=bquote(Theta["k<0"]*"=0;"~Theta["k">="0"]*"=1"),
                           name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
                           y_label=bquote(Delta*Phi^"h"["k"]^"h"), exhogenous_coeff=T),
          "te_l"=plot_irf(linear_coefficients=mod$coefficients, impulse_lags=24, pattern_ar_coefficients = "^te_l",
                           pattern_seasonality = NULL, title=bquote("T"["k<0"]^"e,lp"^4*"=0;"~"T"["k">="0"]^"e,lp"^4*"=1"),
                           name_seasonality_factor=bquote(Phi['t']^'h'*'>0'),
                           y_label=bquote(Delta*Phi^"h"["k"]^"h"), exhogenous_coeff=T)
        )
      
    }
    
    # Model characterization features
    if (!("pdf" %in% names(dev.list()))){ pdf(sprintf(plot_file,"coeffs"),width = width_coeffs,height = height_coeffs) }
    printlm(mod = mod, df= df, value_column = value_column, value_repr = value_repr, ncol=ncol_coeffs,
            irf_objects =  irf_objects)
    # printgam(mod = mod, value_column = value_column, value_repr = value_repr, ncol=ncol_coeffs,
    #          irf_objects = irf_objects)
    dev.off()
    embed_fonts(sprintf(plot_file,"coeffs"), outfile=sprintf(plot_file,"coeffs"))
    
    # Residuals plots -- White noise test (ACF, PACF, cumulative periodogram, ts plot)
    if (!("pdf" %in% names(dev.list()))){ pdf(sprintf(plot_file,"residuals"),width = 7,height = 5) }
    plot_residuals(mod, value_column, value_repr_residuals)
    dev.off()
    embed_fonts(sprintf(plot_file,"residuals"), outfile=sprintf(plot_file,"residuals"))
  }
}

predict_multiple_setpoints <- function(contractId, mod_q, mod_ti, temp_off, temp_comfort, params, df, plot, multiple_setpoints){
  results_multiple_setpoints <- lapply(FUN=function(setpoint){
    simulation_mode <- if(setpoint[["name"]]=="real"){F}else{T}
    # Predict the setpoint
    if("temp_vector" %in% names(setpoint)){
      predv <- prediction_setpoint(
        mod_q, mod_ti, 0, 0, 0, df=df, hysteresis = 0, ts_vector = setpoint[["temp_vector"]], 
        max_lags = max(params[c("ar_ti","lags_q","lags_te","ar_q","lags_infiltrations","lags_Isol","lags_dti")]),
        simulation_mode = simulation_mode
      )
    } else {
      predv <- prediction_setpoint(
        mod_q, mod_ti, setpoint[["temp_status_trigger"]], setpoint[["temp_off"]], setpoint[["temp_comfort"]], df=df, hysteresis = 0, 
        max_lags = max(params[c("ar_ti","lags_q","lags_te","ar_q","lags_infiltrations","lags_Isol","lags_dti")]),
        simulation_mode = simulation_mode
      )
    }
    predv <- predv[is.finite(predv$time)&!is.na(predv$value),]
    
    # Plot if needed
    if(plot==T){ 
      plot_prediction_setpoint(predv,
                               plot_file = sprintf("results/%s_baxi_simulation_%s.pdf",contractId,setpoint[["name"]]),
                               validation_case = setpoint[["name"]]=="real"
      )
    }
    
    # Savings estimation
    act <- sum(predv[,"value"],na.rm=T)
    pred <- sum(predv[,"value_l0"],na.rm=T)
    savings <- round(ifelse(act==0, 0, ((pred - act) / act) * 100),2)
    return(list(
      "summary"=c(setpoint[["name"]], act, pred, savings),
      "df"=predv
    ))
  },multiple_setpoints)
  names(results_multiple_setpoints) <- c(mapply(function(i){multiple_setpoints[[i]][["name"]]},1:length(multiple_setpoints)))
  return(
    list(
      "summary"=mapply(function(i){results_multiple_setpoints[[i]][["summary"]]},1:length(results_multiple_setpoints)),
      "df"=lapply(1:length(results_multiple_setpoints),function(i){
        data.frame("setpoint_name"=names(results_multiple_setpoints)[i],results_multiple_setpoints[[i]][["df"]])
        }) %>% plyr::ldply(rbind)
    )
  )
}

plot_prediction_setpoint <- function(predv, plot_file, validation_case=F){
  predv <- predv[is.finite(predv$time),]
  rmse_ti <- rmserr(predv[,"ti"],predv[,"ti_l0"])$rmse
  rmse_q <- rmserr(predv$value,predv$value_l0)$rmse
  
  predv <- pad(predv)
  
  # Savings estimation
  act <- sum(predv[,"value"],na.rm=T)
  pred <- sum(predv[,"value_l0"],na.rm=T)
  savings <- round(ifelse(act==0, 0, ((pred - act) / act) * 100),2)
  if(validation_case==T){
    txt_savings <- bquote(scriptstyle(sum(widehat(Phi^"h"))*"="*.(round(pred,0))~"kWh"~"(RMSE "*
                            widehat(Phi^"h")*"= "*.(round(rmse_q,2))*" kWh,  RMSE "*widehat("T"^"i")*"= "*
                            .(round(rmse_ti,2))*degree*"C)"))
  } else {
    txt_savings <- ifelse(savings>0,sprintf(" (Savings: +%s%%)",as.character(savings)),sprintf(" (Savings: %s%%)",as.character(savings)))
    txt_savings <- bquote(scriptstyle(sum(widehat(Phi^"h"))*"="*.(round(pred,0))~"kWh"~.(txt_savings)))
  }
  txt_act <- bquote(scriptstyle(sum(Phi^"h")*"="*.(round(act,0))~"kWh"))
  
  df_value <- reshape2::melt(predv[,c("time",
                                        #"value",
                                        "value_l0")],"time")
  qs_virtual <- ggplot(df_value) + geom_line(aes(time,value,color=variable), size=0.3) +
    scale_color_manual(values=c(
      #"black",
      "blue"
      ), name = bquote(""), labels = c(
      #bquote(Phi^"h"),
      bquote(widehat(Phi^"h"))
    )) + theme_bw() +
    theme(text= element_text(size=15, family="CM Roman"),
          axis.text = element_text(size=15, family="CM Roman"),
          axis.text.x = element_blank(),legend.direction = "vertical",legend.justification="left",
          axis.title.x = element_blank(), legend.text.align = 0, legend.position="right",
          strip.background = element_blank(),legend.title = element_blank(),
          strip.text.x = element_blank()) +
    ylim(c(0,max(c(predv$value,predv$value_l0),na.rm = T))) +
    ylab("kWh") +if(validation_case==T){
      ggtitle(bquote(scriptstyle(bold("Forecasting ("*"T"^"s"*"="*"T"^"s,sim"*")   "))~.(txt_savings)))
    } else {
      ggtitle(bquote(scriptstyle(bold("Forecasted (Simulated "*"T"^"s,sim"*")   "))*.(txt_savings)))
    }
  df_value <- reshape2::melt(predv[,c("time","value")],"time")
  qs_real <- ggplot(df_value) + geom_line(aes(time,value,color=variable), size=0.3) +
    ggtitle(bquote(scriptstyle(bold("Measured data   ")*.(txt_act)))) +
    scale_color_manual(values=c("black"),name = bquote(""), labels = c(
      bquote(Phi^"h"))) + theme_bw() +
    theme(text= element_text(size=15, family="CM Roman"),
          axis.text = element_text(size=15, family="CM Roman"),
          axis.text.x = element_blank(),legend.direction = "vertical",legend.justification="left" ,
          axis.title.x = element_blank(),legend.text.align = 0, legend.position="right",
          strip.background = element_blank(),legend.title = element_blank(),
          strip.text.x = element_blank()) + 
    ylim(c(0,max(c(predv$value,predv$value_l0),na.rm = T))) +
    ylab("kWh")
  
  df_temperatures <- reshape2::melt(predv[,c("time","ts_l0","ti")],"time")
  df_temperatures2 <- reshape2::melt(predv[,c("time","ts","ti_l0")],"time")
  #df_temperatures$setpoint <- grepl("ts",df_temperatures$variable)
  min_temp <- min(c(df_temperatures$value,df_temperatures2$value),na.rm=T)
  max_temp <- max(c(df_temperatures$value,df_temperatures2$value),na.rm=T)
  temperatures_real <- ggplot(df_temperatures) + geom_line(aes(time,value, col=variable),size=0.3) +
    theme_bw() + ylim(c(min_temp,max_temp)) +
    scale_color_manual(values=c("azure4","black"),
                       name = bquote(""), labels = c(
      bquote("T"^"s"),
      bquote("T"^"i"))
    ) +
    theme(text= element_text(size=15, family="CM Roman"), legend.position="right",
          axis.text = element_text(size=15, family="CM Roman"),axis.text.x = element_blank(),
          plot.title = element_blank(), axis.title.x = element_blank(),
          strip.background = element_blank(),legend.direction = "vertical",legend.justification="left",
          legend.title = element_blank(),legend.text.align = 0,
          strip.text.x = element_blank()) +
    ylab(bquote(degree*"C"))
  df_temperatures <- df_temperatures2
  temperatures_virtual <- ggplot(df_temperatures) + geom_line(aes(time,value,col=variable),size=0.3) +
    theme_bw() + ylim(c(min_temp,max_temp)) +
    scale_color_manual(values = c("azure4",
                                  #"black",
                                  "blue"), name = bquote(""), labels = c(
      bquote("T"^"s,sim"),
      #bquote("T"^"i"),
      bquote(widehat("T"^"i")))
    ) +
    theme(text= element_text(size=15, family="CM Roman"),
          axis.text = element_text(size=15, family="CM Roman"),
          plot.title = element_blank(), legend.position="right", legend.direction = "vertical",legend.justification="left",
          legend.title = element_blank(),legend.text.align = 0,
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    ylab(bquote(degree*"C"))
  pdf(plot_file,height=6,width=9)
  print(cowplot::plot_grid(qs_real,temperatures_real,qs_virtual,temperatures_virtual,ncol = 1,align = "v",
                     rel_heights = c(0.95,0.8,0.95,1.1)))
  dev.off()
  #grid.arrange(grobs=list(),heights=c(1,0.8,1.0))
  embed_fonts(plot_file,outfile = plot_file)
}


plot_irf <- function(linear_coefficients, impulse_lags, pattern_ar_coefficients, pattern_seasonality = NULL, title="",
                     name_seasonality_factor="seasonality", y_label=bquote(Phi^"h"), exhogenous_coeff=F, plot=T){
  # Impulse response function
  # Take a look https://www3.nd.edu/~esims1/arp_companion.pdf
  #pattern_ar_coefficients_ <- "(ti_l)"
  pattern_ar_coefficients_ <- paste0("(",paste(pattern_ar_coefficients,collapse="|"),")")
  lags_coeffs <- linear_coefficients[grepl(pattern_ar_coefficients_,names(linear_coefficients))]
  if (length(lags_coeffs) == 0) {
    return(NULL)
  }
  
  if (length(lags_coeffs)>1 & sum(grepl(":",names(lags_coeffs)))>0 & !is.null(pattern_seasonality)){
    lags <- data.frame(t(mapply(function(i){ c(gsub(pattern_ar_coefficients_,"",i[grepl(pattern_ar_coefficients_,i)]),
                                               i[grepl(pattern_seasonality,i)]) },strsplit(names(lags_coeffs),":"))),
                       lags_coeffs, stringsAsFactors = F)
  } else {
    lags <- data.frame(t(mapply(function(i){ c(gsub(pattern_ar_coefficients_,"",i[grepl(pattern_ar_coefficients_,i)]),
                                               "1") },strsplit(names(lags_coeffs),":"))),
                       lags_coeffs, stringsAsFactors = F)
  }
  colnames(lags) <- c("lags","seasonality","value")
  
  if(length(pattern_ar_coefficients)>1){
    lags$terms <- mapply(function(i){
      j = 1
      while(grepl(pattern_ar_coefficients[j],rownames(lags)[i])==F){
        j = j+1 
      }
      pattern_ar_coefficients[j]
    },1:nrow(lags))
    lags_ <- lags[lags$terms %in% pattern_ar_coefficients,]
    lags <- aggregate(data.frame("value"=lags_$value),by=list("seasonality"=lags_$seasonality,"lags"=lags_$lags),FUN=sum)
  }
  
  # ## Time constants
  # for (s in unique(lags$seasonality)){
  # roots <- polyroot(rev(c(1,-lags$value[lags$seasonality==s][1:15])))
  # plot(density(1 * -1/log(abs(roots))),main=s)
  # }
  
  lags$lags <- as.numeric(lags$lags)
  lags<-lags[order(lags$lags),]
  o <- max(lags$lags)
  irf_all = data.frame()
  horizon_lags <- 0:max(o,impulse_lags)
  for (j in unique(lags$seasonality)){
    #j="1"
    lags_s <- lags[lags$seasonality==j,]
    lags_s <- lags_s[order(lags_s$lags),"value"]
    if(exhogenous_coeff==T){
      # The IRF(0) is the shock of the variable in study to the output variable at time 0.
      impulse_values <- lags_s[1]
      # If the regressive order is bigger than 1, then calculate the rest of the impulse response
      if(length(lags_s)>1){
        lags_s <- lags_s[2:length(lags_s)]
        # Add the input and lagged input
        idf <- data.frame(h=rep(impulse_values,length(horizon_lags)))
        for(k in 1:length(lags_s)){
          idf[,paste0("h_",k)] <- dplyr::lag(idf$h,k)
        }
        idf[is.na(idf)]<-0
        if(ncol(idf)==2){
          idf <- data.frame("h_1"=idf[-1,-1])
        } else {
          idf <- idf[-1,-1]
        }
        idf[,"h_0"] <- 0
        # Calculate the impulse response after order 0.
        for(k in horizon_lags[-1]){
          idf[k,"h_0"] <- sum(lags_s * idf[k,1:length(lags_s)])
          for(l in 1:length(lags_s)){
            if((k+l)<=nrow(idf)){
              idf[k+l,paste0("h_",l)] <- idf[k,"h_0"]
            }
          }
        }
        impulse_values <- c(impulse_values, idf$h_0)
      } else {
        impulse_values <- c(impulse_values, rep(0,length(horizon_lags[-1])))
      }
      
    } else {
      # Impulse response of the autoregressive variable
      impulse_matrix <- matrix(c(lags_s),byrow=T,nrow = 1)
      if(length(lags_s)>1){
        impulse_matrix <- rbind(impulse_matrix, diag(length(lags_s))[-length(lags_s),])
      }
      impulse_values <- mapply(
        function(horizon){
          (impulse_matrix %^% horizon)[1,1]
        },horizon_lags)
    }
    irf_all <- rbind(irf_all, data.frame("lags"=horizon_lags,"seasonality"=as.character(j),"value"=impulse_values))
  }
  if(plot==F){
    return(irf_all)
  } else {
    return(
      if(length(unique(irf_all$seasonality))==1){
        ggplot(irf_all) + geom_hline(aes(yintercept=0),linetype="dashed") + geom_line(aes(x = lags, y = value, col=seasonality)) +
          #stat_smooth(aes(x = lags, y = value, col=seasonality, group=seasonality),method = "gam",formula= y~s(x,fx=T,k=impulse_lags+1),se=F) +
          ggtitle(title) +theme_bw() + ylab(y_label) + xlab("k")+
          geom_label(aes(max(lags),max(value),label=deparse(title)),parse=T,size=4, family="CM Roman", vjust = "inward", hjust = "inward") +
          theme(legend.position = "none", text= element_text(size=12, family="CM Roman"),
                legend.title = element_text(size=10), axis.text = element_text(size=14, family="CM Roman"),
                plot.title = element_blank()) + guides(col=guide_legend(ncol=2))
      } else {
        ggplot(irf_all) + geom_hline(aes(yintercept=0),linetype="dashed") + geom_line(aes(x = lags, y = value, col=seasonality)) +
          #stat_smooth(aes(x = lags, y = value, col=seasonality, group=seasonality),method = "gam",formula= y~s(x,fx=T,k=impulse_lags+1),se=F) +
          ggtitle(title) +theme_bw() + ylab(y_label) + xlab("k")+
          geom_label(aes(max(lags),max(value),label=deparse(title)),parse=T,size=4, family="CM Roman", vjust = "inward", hjust = "inward") +
          theme(legend.position = "bottom", text= element_text(size=14, family="CM Roman"),
                legend.title = element_text(size=10), legend.text = element_text(size=12), axis.text = element_text(size=14, family="CM Roman"),
                plot.title = element_blank(),legend.direction = "horizontal") + guides(col=guide_legend(ncol=2)) +
          scale_color_discrete(name_seasonality_factor, labels=c("0"="False","1"="True"))
      }
    )
  }
}

prediction_scenario <- function(mod_q, mod_ti, mod_tsupply, df, rows_to_filter=NULL, hp_tset_24h, params, ts_prediction=NULL, n_steps_prediction = 23){
  
  df <- tune_model_input(df,params)
  if(!is.null(rows_to_filter) && sum(rows_to_filter,na.rm=T)>0){ 
    df <- df[rows_to_filter,]
    hp_tset_24h <- hp_tset_24h[rows_to_filter]
  }
  #df <- df[complete.cases(df),]
  
  df$date <- as.Date(df$time, tz="Europe/Berlin")
  
  if(is.null(ts_prediction)){
    ts_prediction <- smartAgg(df,"date",function(x){min(x,na.rm=T)},"time",catN = F)$time
  }
  
  # Simulate by sets of 24h from the ts_prediction
  df <- lapply(ts_prediction,function(ts_){
    
    # Filter the initial dataset 
    # ts_=ts_prediction[1]
    df_ <- df[df$time>=ts_ & df$time<=(ts_+hours(n_steps_prediction)),]
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
      ti_ff <- df_$ti_l0[i] <- predict(mod_ti, df_[i,])
      # If the heat pump should be on, estimate the heat pump consumption and 
      # re-estimate the floor and indoor temperatures considering the heat input.
      if(df_$hp_status_l0[i]==1 && !is.na(tsupply_ff) && !is.na(ti_ff)){
        df_$tsupply_l0[i] <- df_$hp_tset_l0[i]
        df_$hp_cons_l0[i] <- predict(mod_q, df_[i,])
        if(df_$hp_cons_l0[i]>0){
          # df_$tsupply_l0[i] <- predict(mod_tsupply, df_[i,])
          df_$ti_l0[i] <- predict(mod_ti, df_[i,])
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

# WORKING HERE WITH SEVERAL PROBLEMS:

prediction_scenario_different_steps <- function(mod_q, mod_ti, mod_tsupply, df, rows_to_filter=NULL, hp_tset_24h, params, ts_prediction=NULL, n_steps_prediction = 23){
  
  df <- tune_model_input(df,params)
  if(!is.null(rows_to_filter) && sum(rows_to_filter,na.rm=T)>0){ 
    df <- df[rows_to_filter,]
    hp_tset_24h <- hp_tset_24h[rows_to_filter]
  }
  #df <- df[complete.cases(df),]
  
  df$date <- as.Date(df$time, tz="Europe/Berlin")
  
  if(is.null(ts_prediction)){
    ts_prediction_initial <- smartAgg(df,"date",function(x){min(x,na.rm=T)},"time",catN = F)$time
  }

  for (hour in (1:(24/n_steps_prediction)) - 1) {
    
    # hour = ((1:(24/n_steps_prediction)) - 1)[1]
    ts_prediction <- ts_prediction_initial + 3600*hour
    ts_prediction <- format(ts_prediction,"%Y-%m-%d %H:%M:%S %Z")  
    browser()
    # Simulate by sets of 24h from the ts_prediction
    # df <- lapply(ts_prediction,function(ts_){
    
    df <- lapply(ts_prediction,function(ts_){
      
      # Filter the initial dataset 
      # ts_=ts_prediction[1]
      df_ <- df[df$time>=ts_ & df$time<=(as.POSIXct(ts_, tz = "UTC")+hours(n_steps_prediction)),]
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
        ti_ff <- df_$ti_l0[i] <- predict(mod_ti, df_[i,])
        # If the heat pump should be on, estimate the heat pump consumption and 
        # re-estimate the floor and indoor temperatures considering the heat input.
        if(df_$hp_status_l0[i]==1 && !is.na(tsupply_ff) && !is.na(ti_ff)){
          df_$tsupply_l0[i] <- df_$hp_tset_l0[i]
          df_$hp_cons_l0[i] <- predict(mod_q, df_[i,])
          if(df_$hp_cons_l0[i]>0){
            # df_$tsupply_l0[i] <- predict(mod_tsupply, df_[i,])
            df_$ti_l0[i] <- predict(mod_ti, df_[i,])
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
  }

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

optimizer_model_parameters <- function(X, class_per_feature, nclasses_per_feature, min_per_feature, 
                                       max_per_feature, names_per_feature, df, train_dates, val_dates){
    #X=sample(c(0,1),nBits,replace=T)

    params <- decodeValueFromBin(X, class_per_feature, nclasses_per_feature, min_per_feature = min_per_feature, 
                                 max_per_feature = max_per_feature)
    names(params) <- names_per_feature
    
    # Training of the models
    tryCatch({
      mod_q <- calculate_model_q(params, df, train_dates, output="model")$mod
      mod_tsupply <- calculate_model_tsupply(params, df, train_dates, output="model")$mod
      mod_ti <- calculate_model_ti(params, df, train_dates, output="model")$mod
    },error=function(e){return(-1000)})
    
    # Validation of the models in 24h predictions
    predv <- prediction_scenario(
      mod_q = mod_q, 
      mod_ti = mod_ti,
      mod_tsupply = mod_tsupply,
      df = df,
      rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% val_dates,
      hp_tset_24h = ifelse(df$hp_status==0,NA,df$hp_tset),
      params = params,
      ts_prediction = NULL
    )
    
    # Accuracy indoor temperature and consumption
    q_diff <- rmserr(predv$hp_cons[is.finite(predv$hp_cons) & predv$hp_cons>0],
                             predv$hp_cons_l0[is.finite(predv$hp_cons) & predv$hp_cons>0])$rmse
    ti_diff <- rmserr(predv$ti[is.finite(predv$ti)],predv$ti_l0[is.finite(predv$ti)])$rmse
    tsupply_diff <- rmserr(predv$tsupply[is.finite(predv$tsupply)],predv$tsupply_l0[is.finite(predv$tsupply)])$rmse
    
    # # Autocorrelations
    a <- acf(mod_ti$model$ti_l0-mod_ti$fitted.values,plot = F)
    inc_ti <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
    a <- acf(mod_q$model$hp_cons_l0-mod_q$fitted.values,plot = F)
    inc_q <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
    a <- acf(mod_tsupply$model$tsupply_l0-mod_tsupply$fitted.values,plot = T)
    inc_tsupply <- 1 + sum(abs(as.numeric(a$acf)[abs(as.numeric(a$acf)[2:length(a$acf)])>qnorm((1 + 0.95)/2)/sqrt(a$n.used)])-qnorm((1 + 0.95)/2)/sqrt(a$n.used))
    #
    # # Impulse responses
    # mod_ti_te <- sum(abs(plot_irf(mod_ti$coefficients,24,"^te_l",plot = F,exhogenous_coeff = T)$value)>10)
    # mod_ti_value <- sum(plot_irf(mod_ti$coefficients,24,"^value_l",plot = F,exhogenous_coeff = T)$value<(-0.05))
    # mod_q_dte  <- sum(abs(plot_irf(mod_q$coefficients,24,"^dte_l",plot = F,exhogenous_coeff = T)$value)>10)
    # mod_q_dti <- sum(abs(plot_irf(mod_q$coefficients,24,"^dti_l",plot = F,exhogenous_coeff = T)$value)>10)
    # mod_q_te <- sum(abs(plot_irf(mod_q$coefficients,24,"^te_l",plot = F,exhogenous_coeff = T)$value)>10)
    
    # Percentage pvalue < 0.05 to all variables
    pval_q <- 1-(sum(summary(mod_q)$coef[,4]<=0.05) / nrow(summary(mod_q)$coef))
    pval_ti <- 1-(sum(summary(mod_ti)$coef[,4]<=0.05) / nrow(summary(mod_ti)$coef))
    pval_tsupply <- 1-(sum(summary(mod_tsupply)$coef[,4]<=0.05) / nrow(summary(mod_tsupply)$coef))
    
    score <- #- q_total_diff*inc_q - ti_diff*inc_ti - (mod_ti_te+mod_ti_value+mod_q_dte+mod_q_dti+mod_q_te)# inc_ti * inc_q
      -(ti_diff*inc_ti*10*(1+params["mod_ti_ar"]*0.1) +
      q_diff*inc_q*(1+params["mod_hp_cons_ar"]*0.1) + 
      tsupply_diff*inc_tsupply*2*(1+params["mod_tsupply_ar"]*0.1) ) * (
        mean(pval_q,pval_ti,pval_tsupply)
      )
    if (is.finite(score)){
      return(score)#-weighted.mean(,c(0.6,0.6,0.4)))
    } else {return(-10000000000000)}
}

optimizer_MPC <- function(X, class_per_feature, nclasses_per_feature, names_per_feature, levels_per_feature, 
                          df, mod_q, mod_ti, mod_tsupply, ti_min, ti_max, df_price, time_to_predict, params){
  
  #X=sample(c(0,1),nBits,replace=T)
  
  params_hp_tset_24h <- decodeValueFromBin(X, class_per_feature, nclasses_per_feature, levels_per_feature = levels_per_feature)
  params_hp_tset_24h <- as.numeric(params_hp_tset_24h)
  # names(params_hp_tset_24h) <- names_per_feature
  
  rows_to_filter = as.Date(df$time,"Europe/Madrid") %in% as.Date(time_to_predict)
  hp_tset_24h = numeric(nrow(df))
  hp_tset_24h[rows_to_filter] <- params_hp_tset_24h
  
  # the predicted variables will be tagged as "_0" 
  predv <- prediction_scenario(
    mod_q = mod_q, 
    mod_ti = mod_ti,
    mod_tsupply = mod_tsupply,
    df = df,
    rows_to_filter = rows_to_filter,
    hp_tset_24h = hp_tset_24h, #ifelse(df$hp_status==0,NA,df$hp_tset),
    params = params,
    ts_prediction = NULL
  )
  
  # Simetrical penalty is proposed (same penalty if the temperature bound in violated over or under)
  delta <- pmin((predv$ti_l0 - ti_min), (ti_max - predv$ti_l0))
  delta[delta>0] <- 0
  delta <- abs(delta)
  
  if (any(delta!=0)) {
    # Exponential parameter lambda
    lambda <- 2
    # Maximum delta allowed for each hour 
    delta_limit <- 3
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
    delta_penalty <- penalty_function(delta, delta_limit = 1.2, lamda)
    delta_penalty[delta_penalty<0] <- 500000
    
    # 3) Linear growth of penalty
    # delta_penalty <- 
    # 
    penalty <- sum(delta_penalty)
  }
  
  # Cost function: sum over 24 hours
  # check units (in price is euro/MWh and consumption Wh)??
  score <- sum(df_price$price*predv$hp_cons_l0) + penalty

  return(-score)
}

penalty_function <- function(x, delta_limit, lambda){
  y = -exp(x)/(x-delta_limit)
  return(y)
}