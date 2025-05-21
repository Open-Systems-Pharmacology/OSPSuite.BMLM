x = seq(1,10)
minX = 0
maxX = 11

dt <- data.table()

for (M in seq(0.5,10.5,by = 1/3)){
  for (S in 10^seq(0,1,by = 0.25)){

    dt <- rbind(dt,
                data.table(
                  M = M,
                  S = S,
                  pLB = pnorm(q = minX,mean = M,sd = S),
                  pUB = pnorm(q = maxX,mean = M,sd = S),
                  pX = sum(dnorm(x = x,mean = M,sd = S)),
                  logLikelihood = sum(dnorm(x = x,mean = M,sd = S,log = TRUE))
                )
    )
  }
}

dt[,truncationOffset := (pUB -  pLB)]
dt[,logTruncationOffset := log(truncationOffset)]
dt[,logLikelihood_corrected := logLikelihood - length(x)*logTruncationOffset]

dtMelted <- melt(dt, id.vars = c("M", "S"), measure.vars = c("logLikelihood_corrected", "logTruncationOffset", "logLikelihood"),
                  variable.name = "Metric", value.name = "Value")

ggplot(dtMelted, aes(x = M, y = Value, color = as.factor(S), group = S)) +
  geom_line() +
  facet_wrap(~ Metric, scales = "free") +
  scale_fill_manual(values = c("pX" = "blue", "logTruncationOffset" = "green", "logLikelihood" = "red")) +
  labs(x = 'Mean',
       y = "Value",
       color = "SD") +
  theme_minimal() +
  theme(legend.position = 'top')

