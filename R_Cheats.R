#Normal Curve over Histogram 
hist(*data, breaks = 30, freq=FALSE)
curve(dnorm(x, mean = *data.mean, sd = *data.sd), add = TRUE)


#Plotting Residuals 
resid <- *fit1$residuals
sd.resid <- sd(resid)
plot (*data, resid, ylab="Residuals", pch=20)
abline (sd.resid,0,lty=2)
abline(0,0)
abline (-sd.resid,0,lty=2)