require(TSA)
require(astsa)
require(forecast)
require(fpp2)
require(ggplot2)
library(readr)
SerieStorica <- read_csv("C:/Users/lory0/Desktop/Università/Serie Storiche/SerieStorica.csv")
dati <- SerieStorica[242:482, 11]
serie <- ts(dati, start = c(2003,2), frequency = 12)

#pdf("C:/Users/lory0/Desktop/Università/Serie Storiche/Rplot.pdf")
autoplot(serie) +
  labs(x = "Years", y = "")
#dev.off()

#pdf("C:/Users/lory0/Desktop/Università/Serie Storiche/Rplotacf.pdf")
ggAcf(serie) + labs(title = "")
#dev.off()

autocorr <- ggAcf(serie)
p_autocorr <- ggPacf(serie)
fit_serie <- auto.arima(serie)
delta <- diff(serie)

#pdf("C:/Users/lory0/Desktop/Università/Serie Storiche/Rplotdelta.pdf")
autoplot(delta) +
  labs(x = "Years", y = "")
#dev.off()

#pdf("C:/Users/lory0/Desktop/Università/Serie Storiche/Rplotacfdelta.pdf")
acf2(delta, main = "")
#dev.off()

autocorr_d <- ggAcf(delta)
p_autocorr_d <- ggPacf(delta)
fit_delta <- auto.arima(delta)
autoplot(diff(serie))+
  labs(title ="Indice di produzione manufatturiera", x ="Years", y = "diff(manufact)")


#Diagnostica

checkresiduals(fit_serie)

tsdiag(fit_delta, gof.lag=12)


predict(fit_delta, n.ahead=10)
pp <- predict(fit_serie, n.ahead=10) 
class(fit_delta) 
pp

#pdf("C:/Users/lory0/Desktop/Università/Serie Storiche/Rplotprevisione.pdf")
prev <- fit_serie %>% forecast(h=10) %>% autoplot + labs(title = "", x = "Years", y = "")
prev
#dev.off()


autoplot(serie)
ts.plot(serie, pp$pred, pp$pred+2*pp$se, pp$pred-2*pp$se,
        gpars=list(xlab="Year", ylab="log(passengers)", 
                   lty=c(1:4)), col=c("black", "blue", "darkgreen", "orange"))
        

#Diagnostica previsioni
#pdf("C:/Users/lory0/Desktop/Università/Serie Storiche/Rplotresidui.pdf")
sarima(serie, 0, 1, 2) # MA(2)
#dev.off()

sarima.for(serie, n.ahead=10, p=0, d=1, q=2) # previsione 10 passi in avanti


set <- window(serie, start=c(2003,2))
plot.ts(set, xlab="years", ylab="")
abline(v=c(2019,3), col="orange")
text(2014,125,labels=c("Train set"), cex=1.2, col="red")
text(2021, 125,labels=c("Test set"), cex=1.2, col="blue")
# il train set è l'80% del periodo (T) -> 16 anni e 1 mese
train <- window(serie, end=c(2019,3))
test <- window(serie, start=c(2019,4))
# stimo modello arima
fit.arima <- Arima(train, order=c(0,1,2))
fit.arima
checkresiduals(fit.arima)


a1 <- fit.arima %>% forecast(h = 1) %>%
  accuracy(serie)
a1[,c("RMSE","MAE","MAPE")] # as.data.frame(a2) %>% select(RMSE , MAE , MAPE , MASE)

#pdf("C:/Users/lory0/Desktop/Università/Serie Storiche/Rplottest.pdf")
autoplot(train, series="Training set") +
  autolayer(test, series="Test set") + labs(x = "Years", y="")
#dev.off()

