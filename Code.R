install.packages("stargazer")
install.packages("glmulti")
install.packages("ggpubr")
install.packages("corrplot")
library(stargazer)
library(glmulti)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(corrplot)
rm(list=ls())
load("QM2018_data_essay.Rdata")

data <- data[3:13]                  
data <- data[,c(5,1:4,6:11)]
data$interaction <- data$mobilecoverage * data$lpopdensity

summary(data)
stargazer(data)

model_test <- glm(reported ~ mobilecoverage, data = data, family =
                   "binomial"(link = "logit"))
summary(model_test)

model1 <- glm(reported ~ mobilecoverage + ldistcity + ltotalkia + propfriendlydeaths, data = data, family =
                 "binomial"(link = "probit"))

model2 <- glm(reported ~ mobilecoverage + lpopdensity + ltotalkia + propfriendlydeaths, data = data, family =
                 "binomial"(link = "logit"))

summary(model2)

stargazer(model_test, model1, model2)


model_3 <- glm(reported ~ mobilecoverage + popdensity + totalkia + propfriendlydeaths
                   + interaction, data = data)
summary(model_3)

stargazer(model_3)

#### Histograms ####
a <- ggplot(data=data, aes(data$reported)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Reported Cases", y="Count")

b <- ggplot(data=data, aes(data$mobilecoverage)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Mobile Coverage", y="Count")

c <- ggplot(data=data, aes(data$disttown)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Remoteness from Town", y="Count")

d <- ggplot(data=data, aes(data$distcity)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Remoteness from City", y="Count")

e <- ggplot(data=data, aes(data$popdensity)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Population Density", y="Count")

f <- ggplot(data=data, aes(data$totalkia)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Total Number of Casualties", y="Count")

g <- ggplot(data=data, aes(data$propfriendlydeaths)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Share of Western Casualties", y="Count")

h <- ggplot(data=data, aes(data$ltotalkia)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Logged Total Number of Casualties", y="Count")

i <- ggplot(data=data, aes(data$ldisttown)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Logged Remoteness from Town", y="Count")

j <- ggplot(data=data, aes(data$ldistcity)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Logged Remoteness from City", y="Count")

k <- ggplot(data=data, aes(data$lpopdensity)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Logged Population Density", y="Count")

l <- ggplot(data=data, aes(data$interaction)) + 
  geom_histogram(aes(y =..density..),
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(x="Mobile Coverage * Pop. Density (log)", y="Count")

pdf(file="histograms.pdf")
grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l + rremove("x.text"), 
             ncol = 3, nrow = 4)
dev.off()

#### Correlogram ####
corrplot.mixed(model2, lower.col = "black", number.cex = .7)
