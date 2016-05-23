#Function accepts a vector of integers between 0:168 as well as a range (b1 and b2) for the desired periods of the 24 hour day. 
#b1 and b2 are unincluded sections of the 24 hour day that are desired. b1=8 and b2=10 will return all all values of 9 as 1, all others 0. 
two_break <- function(test_hr,b1,b2){
  
  test_hr<-ifelse(test_hr==1,1.1,test_hr)
  for(i in 0:6){
    test_hr<-ifelse(test_hr> (b1+24*i) & test_hr< (b2+24*i), 1 ,test_hr)
  }
  test_hr<-ifelse(test_hr==1,1,0)
  return(test_hr)
}


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
