#Problem: I have a dataset of events that is captured based on the hour of the week. i.e. 4 am on sunday= 4, 4 am on Monday = 28 etc. I want to analyze this data on a daily basis. For instance, all of the events that happen between 8 and 10 am each day. So, I have built a function that returns a dichotomous value for the given range for an ordered list. 

#Function accepts an ordered list of integers between 0:168 representing the hours of a week and a range (b1 and b2) for the desired periods of a 24 hour day. 
#b1 and b2 are the end breaks of the sections of the 24 hour day that are desired. b1=8 and b2=10 will return all all values of 9, 33, 57...etc., all others 0. 

#The fast efficient way: 
getTest_hr <- function(weekHours, startTime, stopTime) {
  as.integer((weekHours %% 24)  %in% seq(startTime, stopTime))
}

#The 'FUN' way:
two_break <- function(test_hr,b1,b2){
  
  test_hr<-ifelse(test_hr==1,1.1,test_hr)
  for(i in 0:6){
    test_hr<-ifelse(test_hr> (b1+24*i) & test_hr< (b2+24*i), 1 ,test_hr)
  }
  test_hr<-ifelse(test_hr==1,1,0)
  return(test_hr)
}

#Same as above, but this time there three breaks that can be categorized into: 0,1,2


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
