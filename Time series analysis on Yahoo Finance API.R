
install.packages('pdfetch')
library(pdfetch) 
freq.data= '1d'


out= pdfetch_YAHOO(identifiers='AAPL', fields = c("open", "high", "low", "volume", "close"),from='2010-01-01',interval = freq.data)

stockdata_d = data.frame(out)

head(stockdata_d)
tail(stockdata_d)
t=stockdata_d$AAPL.close



#a)

taapl <- ts(t)
tss=diff(taapl)
plot(tss)
tss=diff(log(taapl))

plot(tss)
acf(tss)
pacf(tss)



#b)

manual.fit<-arima(taapl,c(1,1,1))
manual.fit
auto.fit<-auto.arima(taapl,seasonal=F)
auto.fit<-auto.arima(taapl,seasonal=T)
auto.fit

#c) AR=p=1,MA=q=1

#d)

auto.fcast<-forecast(auto.fit,h=10)
plot(auto.fcast)

