source("functions.R")

# Libraries for mathematics
library(expm)

# Libraries for graphics
library(cowplot)
library(ggplot2)
library(gridExtra)
library(extrafont)
loadfonts()
# Libraries for data wrangling
#library(padr)
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

# Libraries for ETL's
#library(jsonlite)  
#library(mongolite)

house <- read_excel_allsheets("data/Twin_house_O5_exp1_60minR1.xlsx")$Sheet1
weather <- read_excel_allsheets("data/Twin_house_weather_exp1_60min_compensated.xlsx")$`Weather_Station_IBP_2018-11-01_`
weather[,c("sunAz","sunEl")] <- do.call(cbind,oce::sunAngle(weather$DATE,latitude = 47.874,longitude = 11.728))[,c("azimuth","altitude")]
  
df <- data.frame("time"=house$DATE,
                 "hp_cons"=house$hp_el_cons,
                 "hp_status"=house$`hp_status_command (u1)`,
                 "hp_tset"=house$`hp_supply_temp_command(u2)`,
                 "hp_cop"=house$COP,
                 "tfloor"=house$HeatPump_actual_Tsup,
                 "ti"=house$`Building's representative Temperature of the current time step(volume-averaged)`
)

df_weather <- data.frame(
                 "time"=weather$DATE,
                 "te"=weather$AmbientAirTemperature,
                 "GHI"=weather$Radiation_Global,
                 "BHI"=weather$Radiation_Global-weather$Radiation_Diffuse,
                 "sunAz"=weather$sunAz,
                 "humidity"=weather$RelativeHumidity,
                 "windSpeed"=weather$WindSpeed,
                 "windBearing"=weather$WindDirection
)

df <- merge(df,df_weather)

ggplot(reshape2::melt(df,"time")) + geom_line(aes(time,value)) + facet_wrap(~variable,ncol=1,scales="free_y")

# Training and validation datasets
train_p <- sample(unique(as.Date(df$time,"Europe/Madrid")),size = length(unique(as.Date(df$time,"Europe/Madrid")))*0.8,replace = F)
val_p <- unique(as.Date(df$time,"Europe/Madrid"))[!(unique(as.Date(df$time,"Europe/Madrid")) %in% train_p)]
df_t <- df[as.Date(df$time,"Europe/Madrid") %in% train_p,]
df_t <- df_t[order(df_t$time),]
df_v <- df[as.Date(df$time,"Europe/Madrid") %in% val_p,]
df_v <- df_v[order(df_v$time),]
  
# Optimize the alpha values of the low pass filters
features <- list("alpha_te"=list(min=0,max=0.9,n=31,class="float"),
                 "alpha_BHI"=list(min=0,max=0.9,n=15,class="float"),
                 "alpha_GHI"=list(min=0,max=0.9,n=15,class="float"),
                 "alpha_ws"=list(min=0,max=0.9,n=15,class="float"),
                 "ar_hp_cons"=list(min=0,max=6,n=6,class="int"),
                 "ar_tfloor"=list(min=0,max=6,n=6,class="int"),
                 "ar_ti"=list(min=0,max=6,n=6,class="int"),
                 "lags_te"=list(min=0,max=4,n=4,class="int"),
                 "lags_ti"=list(min=0,max=4,n=4,class="int"),
                 "lags_tfloor"=list(min=0,max=4,n=4,class="int"),
                 "lags_GHI"=list(min=0,max=0,n=0,class="int"),
                 "lags_BHI"=list(min=0,max=0,n=0,class="int"),
                 "lags_humidity"=list(min=0,max=3,n=3,class="int"),
                 "lags_infiltrations"=list(min=0,max=0,n=0,class="int"),
                 "lags_hp_cons"=list(min=0,max=12,n=12,class="int"),
                 "sunAzimuth_nharmonics"=list(min=2,max=7,n=5,class="int"),
                 "windBearing_nharmonics"=list(min=2,max=7,n=5,class="int")
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
    df_t = df_t,
    df_v = df_v,
    popSize = 32,
    maxiter = 20,
    monitor = gaMonitor2,
    parallel = 8,
    elitism = 0.1,
    pmutation = 0.02)
)
  
params <- decodeValueFromBin(optimization_results@solution[1,],
                             class_per_feature, nclasses_per_feature, min_per_feature, 
                             max_per_feature)
names(params) <- names(features)
print(params)

# Calculate the models with consumption as output (result_q) and indoor temperature as output (result_ti).
result_q <- calculate_model_q(params=params,df=df,output="model")
result_ti <- calculate_model_ti(params=params,df=df_,output="model")
mod_q <- result_q$mod
mod_ti <- result_ti$mod
df_mod <- result_ti$df
    
    # plot_crossvalidations <- function(){
    #   for(){
    #     
    #   }
    # }
    # ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$DNI_l0,ylim=c(-0.4,0.4))
    # ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$te_l0,ylim=c(-0.4,0.4))
    # ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$te4_l0,ylim=c(-0.4,0.4))
    # ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$infiltrations_l0,ylim=c(-0.4,0.4))
    # ccf(mod_ti$model$ti_l0-mod_ti$fitted.values,mod_ti$model$value_l0,ylim=c(-0.4,0.4))
    # 
    # acf(mod_q$model$value_l0-mod_q$fitted.values)
    # cpgram(mod_q$model$value_l0-mod_q$fitted.values)
    # ccf(mod_q$model$value_l0-mod_q$fitted.values,mod_q$model$DNI_l0,ylim=c(-0.4,0.4))
    # ccf(mod_q$model$value_l0-mod_q$fitted.values,mod_q$model$te_l0,ylim=c(-0.4,0.4))
    # ccf(mod_q$model$value_l0-mod_q$fitted.values,mod_q$model$infiltrations_l0,ylim=c(-0.4,0.4))
    # ccf(mod_q$model$value_l0-mod_q$fitted.values,mod_q$model$ti_l0,ylim=c(-0.4,0.4))
    # ccf(mod_q$model$value_l0-mod_q$fitted.values,mod_q$model$dte_l0,ylim=c(-0.4,0.4))
    # ccf(mod_q$model$value_l0-mod_q$fitted.values,mod_q$model$dti_l0,ylim=c(-0.4,0.4))
    
    # SH models plots
    plot_results(mod = mod_ti, plot_file = sprintf("results/%s_baxi_output_ti_%s.pdf",contractId,"%s"),
                 value_column="ti_l0",value_repr=expression(Delta~"T"["t"]^"i"),
                 value_repr_residuals=expression("T"["t"]^"i"-widehat("T"["t"]^"i")),
                 df = df_mod, height_coeffs=7, width_coeffs=8.5, ncol_coeffs=3)
    plot_results(mod = mod_q, plot_file = sprintf("results/%s_baxi_output_q_%s.pdf",contractId,"%s"),
                 value_column="value_l0",value_repr=expression(Delta~Phi["t"]^"h"),
                 value_repr_residuals=expression(Phi["t"]^"h"-widehat(Phi["t"]^"h")),
                 df = df_mod, height_coeffs=7, width_coeffs=8.5, ncol_coeffs=3)
    
    # SH setpoint scenarios
    temp_off <- round(median(df$ts[df$value==0],na.rm=T)/0.5,0)*0.5
    temp_comfort <- round(median(df$ts[df$value>0],na.rm=T)/0.5,0)*0.5
    temp_status_trigger <- mean(as.numeric(names(sort(table(df_mod$ts_l0),T))[1:2]))
    results_multiple_setpoints <- predict_multiple_setpoints(
      contractId,mod_q, mod_ti, temp_off, temp_comfort, params, df=df_mod[as.numeric(df_mod$ym) <= 201905,],plot=T,
      multiple_setpoints = list(
        list("name"="real","temp_vector"=df_mod[as.numeric(df_mod$ym) <= 201905,"ts"]),
        list("name"="real-1","temp_vector"=df_mod[as.numeric(df_mod$ym) <= 201905,"ts"]-1),
        list("name"="real-2","temp_vector"=df_mod[as.numeric(df_mod$ym) <= 201905,"ts"]-2),
        list("name"="20_20","temp_status_trigger"=temp_status_trigger, "temp_comfort"=20, "temp_off"=20),
        list("name"="21_21","temp_status_trigger"=temp_status_trigger, "temp_comfort"=21, "temp_off"=21),
        list("name"="22_22","temp_status_trigger"=temp_status_trigger, "temp_comfort"=22, "temp_off"=22),
        list("name"="20_17","temp_status_trigger"=temp_status_trigger, "temp_comfort"=20, "temp_off"=17),
        list("name"="20_19","temp_status_trigger"=temp_status_trigger, "temp_comfort"=20, "temp_off"=19),
        list("name"="21_17","temp_status_trigger"=temp_status_trigger, "temp_comfort"=21, "temp_off"=17),
        list("name"="21_19","temp_status_trigger"=temp_status_trigger, "temp_comfort"=21, "temp_off"=19),
        list("name"="22_17","temp_status_trigger"=temp_status_trigger, "temp_comfort"=22, "temp_off"=17),
        list("name"="22_19","temp_status_trigger"=temp_status_trigger, "temp_comfort"=22, "temp_off"=19),
        list("name"="free_floating","temp_status_trigger"=temp_status_trigger, "temp_comfort"=10, "temp_off"=10))
    )
    
    # ggplot(results_multiple_setpoints$df[results_multiple_setpoints$df$setpoint_name=="20_20",]) +
    #   geom_line(aes(time,dte_l0))
    # colnames(results_multiple_setpoints$df)
    # Accuracy test and model coefficients
    val_q <- results_multiple_setpoints$df[results_multiple_setpoints$df$setpoint_name=="real",c("time","value","value_l0")]
    val_ti <- results_multiple_setpoints$df[results_multiple_setpoints$df$setpoint_name=="real",c("time","ti","ti_l0")]
    
    write.csv(broom::tidy(mod_ti),sprintf("results/%s_mod_ti_summary_coefficients.csv",contractId),row.names = F)
    write.csv(
      cbind(
        broom::glance(mod_ti),
        as.data.frame(t(unlist(rmserr(mod_ti$fitted.values,mod_ti$model$ti_l0)))) %>% dplyr::rename_all(function(x) paste0("train_", x)),
        as.data.frame(t(unlist(rmserr(val_ti$ti,val_ti$ti_l0)))) %>% dplyr::rename_all(function(x) paste0("val_", x)),
        t(params)
      ),
      sprintf("results/%s_mod_ti_summary_accuracy.csv",contractId),row.names = F
    )
    
    write.csv(broom::tidy(mod_q),sprintf("results/%s_mod_q_summary_coefficients.csv",contractId),row.names = F)
    write.csv(
      cbind(
        broom::glance(mod_q),
        as.data.frame(t(unlist(rmserr(mod_q$fitted.values,mod_q$model$value_l0)))) %>% dplyr::rename_all(function(x) paste0("train_", x)),
        as.data.frame(t(unlist(rmserr(val_q$value,val_q$value_l0)))) %>% dplyr::rename_all(function(x) paste0("val_", x)),
        t(params)
      ),
      sprintf("results/%s_mod_q_summary_accuracy.csv",contractId),row.names = F
    )
    
    colnames(val_ti) <- c("time","real","predicted")
    colnames(val_q) <- c("time","real","predicted")
    pvalti <- ggplot(reshape2::melt(pad(val_ti),"time")) + geom_line(aes(time,value,col=variable),alpha=0.8) + theme_bw() + 
      theme(text= element_text(size=15, family="CM Roman"),
            axis.text = element_text(size=15, family="CM Roman"),legend.direction = "vertical",legend.justification="left",
            axis.title.x = element_blank(), legend.text.align = 0, legend.position="right",
            strip.background = element_blank(),legend.title = element_blank(),
            strip.text.x = element_blank()) + 
      scale_color_manual(values=c("black","red"), name = bquote(""), labels = c(bquote("T"^"i"),bquote(widehat("T"^"i")))) +
      ylab(bquote(degree*"C"))
    pvalq <- ggplot(reshape2::melt(pad(val_q),"time")) + geom_line(aes(time,value,col=variable),alpha=0.8) + theme_bw() + 
      theme(text= element_text(size=15, family="CM Roman"),
            axis.text = element_text(size=15, family="CM Roman"),legend.direction = "vertical",legend.justification="left",
            axis.title.x = element_blank(), legend.text.align = 0, legend.position="right",
            strip.background = element_blank(),legend.title = element_blank(),
            strip.text.x = element_blank()) + 
      scale_color_manual(values=c("black","red"), name = bquote(""), labels = c(bquote(Phi^"h"),bquote(widehat(Phi^"h")))) +
      ylab(bquote("kWh"))
    pdf(sprintf("results/%s_accuracy_validation.pdf",contractId),width=7,height = 3)
    print(plot_grid(plotlist = list(pvalti,pvalq),ncol = 1,align = T))
    dev.off()
    embed_fonts(sprintf("results/%s_accuracy_validation.pdf",contractId),outfile=sprintf("results/%s_accuracy_validation.pdf",contractId))
    
    # Results
    result <- data.frame(contractId,t(results_multiple_setpoints[["summary"]]),
                         temp_off,temp_comfort,
                         t(unlist(rmserr(mod_q$model$value_l0,mod_q$fitted.values))),
                         t(unlist(rmserr(mod_ti$model$ti_l0,mod_ti$fitted.values))))
    colnames(result) <-
      c("contractId","case","q_real","q_pred","savings","temp_off","temp_comfort",
        "q_train_mae","q_train_mse","q_train_rmse","q_train_mape","q_train_nmse","q_train_rstd",
        "ti_train_mae","ti_train_mse","ti_train_rmse","ti_train_mape","ti_train_nmse","ti_train_rstd")
    
    # # Real-predicted model comparison
    # predv <- results_multiple_setpoints$df[results_multiple_setpoints$df$setpoint_name=="real",]
    # predv_melted <- pad(reshape2::melt(results_multiple_setpoints$df[,c("time","ti_l0","setpoint_name")],c("time","setpoint_name")))
    # temp_scenarios <- ggplot(predv_melted) + geom_line(aes(time,value)) + facet_wrap(~setpoint_name,ncol=1,strip.position = "right") +
    #   theme_bw()
    # predv_melted <- pad(reshape2::melt(results_multiple_setpoints$df[,c("time","value_l0","setpoint_name")],c("time","setpoint_name")))
    # value_scenarios <- ggplot(predv_melted) + geom_line(aes(time,value)) + facet_wrap(~setpoint_name,ncol=1,strip.position = "right") +
    #   theme_bw()
    # plot_grid(plotlist = list(temp_scenarios,value_scenarios))
    
    # # plot_residuals(value_repr = "Temperature",residuals = predv$ti-predv$ti_l0)
    # plotly::ggplotly(ggplot(predv) + geom_line(aes(time,ti)) + geom_line(aes(time,ti_l0),col="red"))
    # # plot_residuals(value_repr = "Consumption",residuals = predv$value-predv$value_l0)
    # plotly::ggplotly(ggplot(predv) + geom_line(aes(time,value)) + geom_line(aes(time,value_l0),col="red"))
    # 
    # # Plot the real value and the predicted value by the model
    # df_mod$pred <- ifelse(df_mod$status==1,predict(mod_q,df_mod),0)
    # df_mod <- pad(df_mod)
    # ggplot(df_mod)+
    #   geom_line(aes(time,value_l0),group=1)+  geom_line(aes(time,pred),col=2,group=1) + theme_bw()
    # df_mod$pred <- predict(mod_ti,df_mod)
    # ggplot(df_mod)+
    #   geom_line(aes(time,ti),group=1)+  geom_line(aes(time,pred),col=2,group=1) + theme_bw()
  }
  return(result)
  },error=function(e){return(NULL)})
})

write.csv(do.call(rbind,all_contracts_results),"results/savings.csv",row.names = F)
