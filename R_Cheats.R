#adding dta

#Normal Curve over Histogram 
hist(*data, breaks = 30, freq=FALSE)
curve(dnorm(x, mean = *data.mean, sd = *data.sd), add = TRUE)

#Jittered Plot
plot(jitter(*x, factor=.5), jitter(*y, factor=.5), pch=20)


#Plotting Residuals 
resid <- *fit1$residuals
sd.resid <- sd(resid)
plot (*data, resid, ylab="Residuals", pch=20)
abline (sd.resid,0,lty=2)
abline(0,0)
abline (-sd.resid,0,lty=2)

#Removing Column from Data Table: 
tablename[,columnname:=NULL]
tablename[,c('columnname','colname2'):=NULL]

#Function to split 168 Hour Week into Day/Night 1/0. b1, b2 are the breaks between start/end
day_night <- function(test_hr,b1,b2){
  
  test_hr<-ifelse(test_hr==1,1.1,test_hr)
  for(i in 0:6){
    test_hr<-ifelse(test_hr> (b1+24*i) & test_hr< (b2+24*i), 1 ,test_hr)
  }
  test_hr<-ifelse(test_hr==1,1,0)
  return(test_hr)
}


#Function to divide 168 Wrk Week into 3 sections per day
#b1, b2, b3's are breaks in a day
multi_divide <- function(test_hr,b1,b2,b3){
  test_hr<-ifelse(test_hr==1,1.1,test_hr)
  test_hr<-ifelse(test_hr==2,2.1,test_hr)
  for(i in 0:6){
    test_hr<-ifelse(test_hr> (b1+24*i) & test_hr<= (b2+24*i), 1 ,test_hr)
    test_hr<-ifelse(test_hr> (b2+24*i) & test_hr<= (b3+24*i), 2 ,test_hr)
  }
  test_hr<-ifelse(test_hr!=1 & test_hr!=2,0,test_hr)
  return(test_hr)
}

#Replace all unique values with Ints
for(i in 1:length(unique(col))){
  dt[col==unique(col[i]),col:=as.character(i)]
}



