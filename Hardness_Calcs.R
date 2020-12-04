####### HV Calculations v1.1 ########

library(tidyverse)

data <- as_tibble(read.csv("Hardness_Tests.csv"))

####### Section 1 ########

one.dat <- data %>%
  select(1,4) %>%
  filter(Indent < 51)

####### Section 2 ########

two.dat <- data %>%
  select(1,4) %>%
  filter(Indent < 101 & Indent > 50)

####### Section 3 ########

three.dat <- data %>%
  select(1,4) %>%
  filter(Indent > 100)

####### Data Manipulation ########

a <- one.dat$Hardness
b <- two.dat$Hardness
c <- three.dat$Hardness

# # Fusion Zone Comparison
# 
# n.f = length(f) # number of samples in region six
# 
# n.g = length(g) # number of samples in region seven
# 
# x_bar.f = (1/n.f)*sum(f) # average hardness in region six
# 
# x_bar.g = (1/n.g)*sum(g) # average hardness in region seven
# 
# s_x2.f <- (1/(n.f-1))*sum((f - x_bar.f)^2) # this is the sample variance of the data set
# 
# s_x.f <- sqrt(s_x2.f) # this is the standard deviation of the data set
# 
# s_x2.g <- (1/(n.g-1))*sum((g - x_bar.g)^2) # this is the sample variance of the data set
# 
# s_x.g <- sqrt(s_x2.g) # this is the standard deviation of the data set
# 
# f.part <- ((s_x.f^2)/n.f)
# 
# g.part <- ((s_x.g^2)/n.g)
# 
# nu <- (f.part + g.part) /
#   (((f.part^2)/(n.f-1)) + ((g.part^2)/(n.g-1)))# degrees of freedom
# 
# new_nu <- round(nu)
# 
# t_crit = 1.963
# 
# t_stat = (x_bar.f - x_bar.g) /
#   sqrt(f.part + g.part)
# 
# if(t_stat > t_crit | t_stat < (-1)*t_crit) {
#   print("rejects the null hypothesis")
# } else print("failed to reject the null hypothesis")
# 
# results1 <- tibble(n.f,n.g,x_bar.f,x_bar.g,s_x.f,s_x.g,nu,new_nu,t_crit,t_stat)

# Average hardness of each area

n.a = length(a) # number of samples in region six
n.b = length(b) # number of samples in region seven
n.c = length(c) # number of samples in region six

x_bar.a = (1/n.a)*sum(a) # average hardness in region six
x_bar.b = (1/n.b)*sum(b) # average hardness in region six
x_bar.c = (1/n.c)*sum(c) # average hardness in region seven

s_x2.a <- (1/(n.a-1))*sum((a - x_bar.a)^2) # this is the sample variance of the data set
s_x2.b <- (1/(n.b-1))*sum((b - x_bar.b)^2) # this is the sample variance of the data set
s_x2.c <- (1/(n.c-1))*sum((c - x_bar.c)^2) # this is the sample variance of the data set

s_x.a <- sqrt(s_x2.a) # this is the standard deviation of the data set
s_x.b <- sqrt(s_x2.b) # this is the standard deviation of the data set
s_x.c <- sqrt(s_x2.c) # this is the standard deviation of the data set

Sx_bar.a <- s_x.a / sqrt(n.a) # standard deviation of means
Sx_bar.b <- s_x.b / sqrt(n.b) # standard deviation of means
Sx_bar.c <- s_x.c / sqrt(n.c) # standard deviation of means

HV <- c(x_bar.a,x_bar.b,x_bar.c)
Sx_bar <- c(Sx_bar.a,Sx_bar.b,Sx_bar.c)
N <- c(n.a,n.b,n.c)

results2 <- tibble(HV,Sx_bar,N)

# write.csv(results1, file = "FZ_HV_Comp.csv")
write.csv(results2, file = "Avg_HV.csv")