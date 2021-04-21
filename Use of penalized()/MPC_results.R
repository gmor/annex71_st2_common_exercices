library(ggplot2)
library(plotly)

Results_MPC_V1 <- data.frame(
  "horizon" = c(6, 12, 24),
  "cost" = c(14.33, 12.82, 12.77),
  "discomfort" = c(22.38, 9.29, 7.26)
)

Results_MPC_V2 <- data.frame(
  "horizon" = c(6, 12, 24),
  "cost" = c(15.34, 14.13, 14.08),
  "discomfort" = c(22.38, 2.99, 1.89)
)


Results_MPC_hard <- data.frame(
  "horizon" = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  "cost" = c(16.07, 14.80, 13.68, 12.99, 12.46, 12.33, 12.35, 12.12, 12.13, 12.28, 12.13),
  "discomfort" = c(13.73, 1.99, 0.34, 0.21, 0.25, 0.44, 0.26,  0.24, 0.61, 0.40, 0.42)
)


Results_MPC_down24h <- data.frame(
  "horizon" = c(10, 15, 20, 25),
  "cost" = c(1.97, 1.76, 1.63, 1.59),
  "discomfort" = c(0, 0, 0, 0)
)

Results_MPC_down48h <- data.frame(
  "horizon" = c(10, 15, 20, 25),
  "cost" = c(3.606, 3.378, 3.301, 3.242),
  "discomfort" = c(0.289, 0.058, 0.011, 0.048)
)

plot(Results_MPC_hard$horizon, Results_MPC_hard$cost)
plot(Results_MPC_hard$horizon, Results_MPC_hard$discomfort)


# ggplotly(
ggplot(Results_MPC_hard[-10,]) + #Results_MPC_hard[1:nrow(Results_MPC_hard),]
  geom_point(aes(horizon,cost), color = "red", shape = 19, size = 3) +
  geom_line(aes(horizon,cost), color = "red", size = 1.25) +
  xlab("Horizon length[h]") + ylab("Cost[Eur]") +
  # geom_point(aes(horizon,discomfort+12), color = "blue", shape = 22, size =1) +
  # geom_line(aes(horizon,discomfort), color = "blue") +
  # scale_y_continuous(sec.axis = sec_axis(trans = ~.-12, name = "Discomfort [Kh]", breaks = seq(from = 0, to = 14, by = 0.5))) +
  scale_x_continuous(breaks=seq(2,12,2)) +
  # theme_bw() 
  theme_classic(base_size = 15)
# )


# improvement <-{}
# for (i in 2:nrow(Results_MPC_hard)) {
#   improvement[i-1] <- (Results_MPC_hard$cost[-10][i-1]-Results_MPC_hard$cost[-10][i])/Results_MPC_hard$cost[-10][i-1]
# }
# improvement*100
# solutions_space <- c(16^2, 16^3, 16^4, 16^5, 16^6, 16^7, 16^8, 16^9, 16^10, 16^11, 16^12)
# # solutions_space <- log10(solutions_space)
# Results_MPC_hard_plot <- Results_MPC_hard[-10,]
# 
# convertion <- function(x) (log(x)-11)/5
# convertion_in <- function(x) (log((x/5)))+11
# 
# ggplot() + #Results_MPC_hard[1:nrow(Results_MPC_hard),]
#   geom_point(aes(Results_MPC_hard_plot$horizon, 10^((Results_MPC_hard_plot$cost)), color = "cost"), shape = 19, size = 3) +
#   geom_line(aes(Results_MPC_hard_plot$horizon, 10^(Results_MPC_hard_plot$cost), color = "cost"), size = 1.25) +
#   geom_line(aes(x = 2:12, y = (solutions_space), color = "Solutions space")) +
#   xlab("Horizon length[h]") + ylab("Dimension of solution space [-]") +
#   scale_y_log10(position = "right",  labels = trans_format("log10", math_format(10^.x)),
#     sec.axis = sec_axis(trans = convertion_in,#"log10",#trans_new("log10_12", transform = convertion, inverse = convertion_in),
#                         name = "Cost[Eur]",
#                         breaks = seq(12:16)*10,
#                         labels = as.character((seq(12:16)+11)),
#                         guide = guide_axis(position = "left"))) +
#   scale_x_continuous(breaks=seq(2,12,2)) +
#   # theme_bw() 
#   theme_classic(base_size = 15)



# ggplotly(
ggplot(Results_MPC_down48h) + #Results_MPC_hard[1:nrow(Results_MPC_hard),]
  geom_point(aes(horizon,cost), color = "red", shape = 12, size = 1) +
  geom_line(aes(horizon,cost), color = "red") +
  xlab("Horizon [h]") + ylab("Cost [Eur]") +
  # geom_point(aes(horizon,discomfort+12), color = "blue", shape = 22, size =1) +
  # geom_line(aes(horizon,discomfort), color = "blue") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~.-12, name = "Discomfort [Kh]")) +
  theme_bw() + scale_x_continuous(breaks=seq(5,25,5))#breaks=seq(2,12,2)

# )

error<-{}
for (i in 2:nrow(Results_MPC_hard)) {
  error[i-1] <- Results_MPC_hard$cost[i]-Results_MPC_hard$cost[i-1]
}
plot(2:nrow(Results_MPC_hard), abs(error))

error














fit <- lm(Results_MPC_hard$cost[2:11] ~ (exp(-(Results_MPC_hard$horizon[2:11]))))
fited <- coef(fit)[1]+coef(fit)[2]*exp(-(1:11))
ggplot()+
  geom_point(aes(Results_MPC_hard$horizon,Results_MPC_hard$cost), color = "red", shape = 12, size = 1) +
  geom_line(aes(Results_MPC_hard$horizon,fited))

exp(-2+log(52.44))
exp(-2)*52.44

exp(2/5)

exp(-+log(52.44))

plot(Results_MPC_hard$horizon, Results_MPC_hard$cost)

plot(log10(16^(1:12)))

16^12
16^24


500*40
250*30
