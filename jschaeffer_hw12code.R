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
R=1000
err20 = 0
err30 = 0

for(i in 1:R){
  set.seed(7272+i) #Ensuring same sample generation for 20 and 30
  x20 = rlaplace(20,0,4) #Calculating sample
  t20 = mean(x20) / (sd(x20)/sqrt(20)) #Calculating t20
  if (t20>tval20) #If type1 error, add 1
    err20=err20+1
  
  x30 = rlaplace(30,0,4) #Calculating sample
  t30 = mean(x30) / (sd(x30)/sqrt(30)) #Calculating t30
  if (t30>tval30)
    err30=err30+1
}

test = rlaplace(20,0,4)