1.2e+7/24

lambda <- 0.5
delta_limit <- 2

delta <- seq(from = 0, to = 1, by = 0.01)
# y = (-exp(lambda*x)/(x-delta_limit))-(1/delta_limit)
delta_penalty <- {}
for (i in 1:length(delta)) {
  if ((delta[i] > 0)&(delta[i] < delta_limit)) {
    delta_penalty[i] <- (-exp(lambda*delta[i]^2)/(delta[i]^2-delta_limit))-(1/delta_limit)
  }else if (delta[i] >= delta_limit) {
    delta_penalty[i] <- abs(delta[i])^2*500
  }else{
    delta_penalty[i] <- 0
  }
}
plot(delta,delta_penalty)
max(y)
delta[401]
14170386/24
plot(df$time,df)

sum(df_result$cost, na.rm = T)

(-exp(0.50*0.5)/(0.5-4))-(1/4)

plot(df$hp_cons,df$tsupply, col=df$hp_status)
