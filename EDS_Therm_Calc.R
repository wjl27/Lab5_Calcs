######## Thermal Barrier Coating EDS Data ########

rm(list=ls())

library(tidyverse)

data <- as_tibble(read.csv("OverAll_Therm_Coating_EDS.csv"))

C.data <- data %>%
  select(1,2) %>%
  filter(Element == "C")

O.data <- data %>%
  select(1,2) %>%
  filter(Element == "O")

Zr.data <- data %>%
  select(1,2) %>%
  filter(Element == "Zr")

Y.data <- data %>%
  select(1,2) %>%
  filter(Element == "Y")

C.dat <- C.data$Wt.
O.dat <- O.data$Wt.
Zr.dat <- Zr.data$Wt.
Y.dat <- Y.data$Wt.

n.C = length(C.dat) # number of samples
n.O = length(O.dat) # number of samples
n.Zr = length(Zr.dat) # number of samples
n.Y = length(Y.dat) # number of samples

x_bar.a = (1/n.C)*sum(C.dat) # average hardness
x_bar.b = (1/n.O)*sum(O.dat) # average hardness
x_bar.c = (1/n.Zr)*sum(Zr.dat) # average hardness
x_bar.d = (1/n.Y)*sum(Y.dat) # average hardness

s_x2.a <- (1/(n.C-1))*sum((C.dat - x_bar.a)^2) # this is the sample variance of the data set
s_x2.b <- (1/(n.O-1))*sum((O.dat - x_bar.b)^2) # this is the sample variance of the data set
s_x2.c <- (1/(n.Zr-1))*sum((Zr.dat - x_bar.c)^2) # this is the sample variance of the data set
s_x2.d <- (1/(n.Y-1))*sum((Y.dat - x_bar.d)^2) # this is the sample variance of the data set

s_x.a <- sqrt(s_x2.a) # this is the standard deviation of the data set
s_x.b <- sqrt(s_x2.b) # this is the standard deviation of the data set
s_x.c <- sqrt(s_x2.c) # this is the standard deviation of the data set
s_x.d <- sqrt(s_x2.d) # this is the standard deviation of the data set

Sx_bar.a <- s_x.a / sqrt(n.C) # standard deviation of means
Sx_bar.b <- s_x.b / sqrt(n.O) # standard deviation of means
Sx_bar.c <- s_x.c / sqrt(n.Zr) # standard deviation of means
Sx_bar.d <- s_x.d / sqrt(n.Y) # standard deviation of means

El <- c("C","O","Zr","Y")
Wt.Prct <- c(x_bar.a,x_bar.b,x_bar.c,x_bar.d)
Sx_bar <- c(Sx_bar.a,Sx_bar.b,Sx_bar.c,Sx_bar.d)
N <- c(n.C,n.O,n.Zr,n.Y)

results2 <- tibble(El,Wt.Prct,Sx_bar,N)
write.csv(results2, file = "Avg_Thermal_Coating_EDS.csv")