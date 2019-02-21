
data<-read.csv("Desktop/GSPC2019.csv")
data$logreturn<-0
data$logreturn<-c(NA,diff(log(data$Adj.Close)))
View(data)
data1<-read.csv("Desktop/IXIC2019.csv")
data1$returns<-c(NA,diff(log(data1$Adj.Close)))
test_model<-lm(data1$returns~data$logreturn)
View(data1)
t<-predict(test_model,interval="prediction")
predicted<-0
fitted<-t[,1]
for(i in data$Adj.Close){
  predicted<- exp(fitted[i]+log(data$Adj.Close[i]))
}
predicted
#[1] 2432.314 2555.589 2555.205 2581.810 2588.552 2600.538 2596.934 2579.754 2618.512
lower_int<-0
lower<-t[,2]
for(i in data$Adj.Close){
  lower_int<- exp(lower[i]+log(data$Adj.Close[i]))
}
lower
#2             3             4             5             6             7 
#-0.0396738866  0.0348349790  0.0022013192  0.0055394201 -0.0014558093 -0.0009256001 
#8             9            10 
#-0.0068535855 -0.0134609360  0.0068072307 
upper_int<-0
upper<-t[,2]
for(i in data$Adj.Close){
  upper_int<- exp(upper[i]+log(data$Adj.Close[i]))
}
upper
#2             3             4             5             6             7 
#-0.0232292533  0.0512774365  0.0160917798  0.0194982961  0.0124123267  0.0129423906 
#8             9            10 
#0.0070800117  0.0007050906  0.0208041785 
plot(data$Adj.Close, type="l", col="blue", ylab="Price", main="Price", xlab = "Days")
lines(lower_int,col="red")
lines(predicted,col="green",lwd=2)
legend(x = "bottomright", legend = c("Actual", "Predicted","P-Interval"), fill = 1:6, ncol = 2, lwd = 2, col = c("blue", "green","red"))