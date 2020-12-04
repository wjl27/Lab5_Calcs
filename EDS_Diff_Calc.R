######## Diffusion Coating EDS Data ########

rm(list=ls())

library(tidyverse)

data <- as_tibble(read.csv("OverAll_Diff_Coating_EDS.csv"))

C.data <- data %>%
  select(1,2) %>%
  filter(Element == "C")

Co.data <- data %>%
  select(1,2) %>%
  filter(Element == "Co")

Ni.data <- data %>%
  select(1,2) %>%
  filter(Element == "Ni")

Pt.data <- data %>%
  select(1,2) %>%
  filter(Element == "Pt")

Al.data <- data %>%
  select(1,2) %>%
  filter(Element == "Al")

Cr.data <- data %>%
  select(1,2) %>%
  filter(Element == "Cr")

C.dat <- C.data$Wt.
Co.dat <- Co.data$Wt.
Ni.dat <- Ni.data$Wt.
Pt.dat <- Pt.data$Wt.
Al.dat <- Al.data$Wt.
Cr.dat <- Cr.data$Wt.

n.C = length(C.dat) # number of samples
n.Co = length(Co.dat) # number of samples
n.Ni = length(Ni.dat) # number of samples
n.Pt = length(Pt.dat) # number of samples
n.Al = length(Al.dat) # number of samples
n.Cr = length(Cr.dat) # number of samples

x_bar.a = (1/n.C)*sum(C.dat) # average hardness
x_bar.b = (1/n.Co)*sum(Co.dat) # average hardness
x_bar.c = (1/n.Ni)*sum(Ni.dat) # average hardness
x_bar.d = (1/n.Pt)*sum(Pt.dat) # average hardness
x_bar.e = (1/n.Al)*sum(Al.dat) # average hardness
x_bar.f = (1/n.Cr)*sum(Cr.dat) # average hardness

s_x2.a <- (1/(n.C-1))*sum((C.dat - x_bar.a)^2) # this is the sample variance of the data set
s_x2.b <- (1/(n.Co-1))*sum((Co.dat - x_bar.b)^2) # this is the sample variance of the data set
s_x2.c <- (1/(n.Ni-1))*sum((Ni.dat - x_bar.c)^2) # this is the sample variance of the data set
s_x2.d <- (1/(n.Pt-1))*sum((Pt.dat - x_bar.d)^2) # this is the sample variance of the data set
s_x2.e <- (1/(n.Al-1))*sum((Al.dat - x_bar.e)^2) # this is the sample variance of the data set
s_x2.f <- (1/(n.Cr-1))*sum((Cr.dat - x_bar.f)^2) # this is the sample variance of the data set

s_x.a <- sqrt(s_x2.a) # this is the standard deviation of the data set
s_x.b <- sqrt(s_x2.b) # this is the standard deviation of the data set
s_x.c <- sqrt(s_x2.c) # this is the standard deviation of the data set
s_x.d <- sqrt(s_x2.d) # this is the standard deviation of the data set
s_x.e <- sqrt(s_x2.e) # this is the standard deviation of the data set
s_x.f <- sqrt(s_x2.f) # this is the standard deviation of the data set

Sx_bar.a <- s_x.a / sqrt(n.C) # standard deviation of means
Sx_bar.b <- s_x.b / sqrt(n.Co) # standard deviation of means
Sx_bar.c <- s_x.c / sqrt(n.Ni) # standard deviation of means
Sx_bar.d <- s_x.d / sqrt(n.Pt) # standard deviation of means
Sx_bar.e <- s_x.e / sqrt(n.Al) # standard deviation of means
Sx_bar.f <- s_x.f / sqrt(n.Cr) # standard deviation of means

El <- c("C","Co","Ni","Pt","Al","Cr")
Wt.Prct <- c(x_bar.a,x_bar.b,x_bar.c,x_bar.d,x_bar.e,x_bar.f)
Sx_bar <- c(Sx_bar.a,Sx_bar.b,Sx_bar.c,Sx_bar.d,Sx_bar.e,Sx_bar.f)
N <- c(n.C,n.Co,n.Ni,n.Pt,n.Al,n.Cr)

results2 <- tibble(El,Wt.Prct,Sx_bar,N)
write.csv(results2, file = "Avg_Diffusion_Coating_EDS.csv")