#Normal Curve over Histogram 
hist(*data, breaks = 30, freq=FALSE)
curve(dnorm(x, mean = *data.mean, sd = *data.sd), add = TRUE)