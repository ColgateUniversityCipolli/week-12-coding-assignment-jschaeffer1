library(tidyverse)
library(VGAM)
#Question 1

#Part a
t_val20 = qt(0.95, df=19)
t_val20

#Part b
t_val30 = qt(0.95, df=29)
t_val30

#Part c
R=10000
err20 = 0
err30 = 0

for(i in 1:R){
  set.seed(7272+i)
  x20 = rlaplace(20,0,4) #Calculating sample
  t20 = mean(x20) / (sd(x20)/sqrt(20)) #Calculating t20
  if (t20>t_val20) #If type1 error, add 1
    err20=err20+1
  
  x30 = rlaplace(30,0,4) #Calculating sample
  t30 = mean(x30) / (sd(x30)/sqrt(30)) #Calculating t30
  if (t30>t_val30)
    err30=err30+1
}
#Calculating percentage of t error
perc_err20 = err20/R
perc_err30= err30/R

perc_err20
perc_err30


############################
#####    QUESTION 2    #####
############################
n=15 #Initializing
R=10000
mean1 = 10/12
mean2 = 2/12
mean3 = 10/20
err1_left=0
err2_left=0
err3_left=0

err1_right=0
err2_right=0
err3_right=0

err1_two=0
err2_two=0
err3_two=0
for(i in 1:R){
  set.seed(7272+i)
  sample1 = rbeta(n, 10, 2) #Calculating samples
  sample2 = rbeta(n, 2, 10)
  sample3 = rbeta(n, 10, 10)
  
  #Calculating t values
  t1 = (mean(sample1)-mean1)/(sd(sample1)/sqrt(n))
  t2 = (mean(sample2)-mean2)/(sd(sample2)/sqrt(n))
  t3 = (mean(sample3)-mean3)/(sd(sample3)/sqrt(n))
  #Checking if type 1 error occurred for left test
  if (t1<qt(0.05, df=14))
    err1_left=err1_left+1
  if (t2<qt(0.05, df=14))
    err2_left=err2_left+1
  if (t3<qt(0.05, df=14))
    err3_left=err3_left+1
  
  
  #Checking if type 1 error occurred for right test
  if (t1>qt(0.95, df=14))
    err1_right=err1_right+1
  if (t2>qt(0.95, df=14))
    err2_right=err2_right+1
  if (t3>qt(0.95, df=14))
    err3_right=err3_right+1
  
  
  #Checking if type 1 error occurred for two tailed test
  if (abs(t1)>qt(0.975, df=14))
    err1_two=err1_two+1
  if (abs(t2)>qt(0.975, df=14))
    err2_two=err2_two+1
  if (abs(t3)>qt(0.975, df=14))
    err3_two=err3_two+1

}

err1_left/R
err2_left/R
err3_left/R

err1_right/R
err2_right/R
err3_right/R

err1_two/R
err2_two/R
err3_two/R





