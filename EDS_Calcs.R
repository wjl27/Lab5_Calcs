######## Base EDS Data ########

rm(list=ls())

library(tidyverse)

data <- as_tibble(read.csv("Overall_Base_alloy_EDS.csv"))

Ni.data <- data %>%
  select(1,2) %>%
  filter(Element == "Ni")

Al.data <- data %>%
  select(1,2) %>%
  filter(Element == "Al")

Co.data <- data %>%
  select(1,2) %>%
  filter(Element == "Co")

Cr.data <- data %>%
  select(1,2) %>%
  filter(Element == "Cr")

Ni.dat <- Ni.data$Wt.
Al.dat <- Al.data$Wt.
Co.dat <- Co.data$Wt.
Cr.dat <- Cr.data$Wt.

n.Ni = length(Ni.dat) # number of samples
n.Al = length(Al.dat) # number of samples
n.Co = length(Co.dat) # number of samples
n.Cr = length(Cr.dat) # number of samples

x_bar.a = (1/n.Ni)*sum(Ni.dat) # average hardness
x_bar.b = (1/n.Al)*sum(Al.dat) # average hardness
x_bar.c = (1/n.Co)*sum(Co.dat) # average hardness
x_bar.d = (1/n.Cr)*sum(Cr.dat) # average hardness

s_x2.a <- (1/(n.Ni-1))*sum((Ni.dat - x_bar.a)^2) # this is the sample variance of the data set
s_x2.b <- (1/(n.Al-1))*sum((Al.dat - x_bar.b)^2) # this is the sample variance of the data set
s_x2.c <- (1/(n.Co-1))*sum((Co.dat - x_bar.c)^2) # this is the sample variance of the data set
s_x2.d <- (1/(n.Cr-1))*sum((Cr.dat - x_bar.d)^2) # this is the sample variance of the data set

s_x.a <- sqrt(s_x2.a) # this is the standard deviation of the data set
s_x.b <- sqrt(s_x2.b) # this is the standard deviation of the data set
s_x.c <- sqrt(s_x2.c) # this is the standard deviation of the data set
s_x.d <- sqrt(s_x2.d) # this is the standard deviation of the data set

Sx_bar.a <- s_x.a / sqrt(n.Ni) # standard deviation of means
Sx_bar.b <- s_x.b / sqrt(n.Al) # standard deviation of means
Sx_bar.c <- s_x.c / sqrt(n.Co) # standard deviation of means
Sx_bar.d <- s_x.d / sqrt(n.Cr) # standard deviation of means

El <- c("Ni","Al","Co","Cr")
Wt.Prct <- c(x_bar.a,x_bar.b,x_bar.c,x_bar.d)
Sx_bar <- c(Sx_bar.a,Sx_bar.b,Sx_bar.c,Sx_bar.d)
N <- c(n.Ni,n.Al,n.Co,n.Cr)

results2 <- tibble(El,Wt.Prct,Sx_bar,N)
write.csv(results2, file = "Avg_Base_EDS.csv")