
getwd()
setwd("C:/Users/Jesse/Desktop/R files")
# a ###############################
dataset <- read.csv("singapore.economy.csv",header = TRUE)
fix(dataset)
dataset
dim(dataset)

# b #########################################
dataset2 <- na.omit(dataset)
fix(dataset2)
dim(dataset2)

# c ###############################################
attach(dataset2)
plot(time,gdp,xlab="Time",ylab="GDP (%)",main="Singapore GDP growth")

# d ###################################################
gdp1= dataset2[period=="1","gdp"]
mean(gdp1)
var(gdp1)
sd(gdp1)

gdp2= dataset2[period=="2","gdp"]
mean(gdp2)
var(gdp2)
sd(gdp2)

gdp3= dataset2[period=="3","gdp"]
mean(gdp3)
var(gdp3)
sd(gdp3)

############################################
mean<- c(mean(gdp1),mean(gdp2),mean(gdp3))
sd<- c(sd(gdp1),sd(gdp2),sd(gdp3))
period<- c(1,2,3)
stat.table<- data.frame(period,mean,sd)
stat.table

# e ##########################################
pairs(dataset2[,3:10])

# f simple linear regression #################
slr.GDP= lm(gdp~exp)
slr.GDP
summary(slr.GDP)

# g multiple regression model #################
mlr.GDP= lm(gdp~exp+epg+hpr+oil+gdpus+crd)
mlr.GDP
summary(mlr.GDP)

# h ##########################################
quantile(gdp,probs = 0.05)
state = rep(0, length(dataset2$gdp))
## 0 = crisis 
## 1 = normal
state[gdp <= quantile(gdp,probs = 0.05)] = 0
state[gdp > quantile(gdp,probs = 0.05)] = 1
dataset2 = data.frame(dataset2, state)
##################
train = (time < 2007.917)
dataset2.2007 = dataset2[!train, ]
glm.fit = glm(state ~ bci, data = dataset2, family = binomial, subset = train)
glm.probs = predict(glm.fit, dataset2.2007, type = "response")
glm.pred = rep("crisis", length(glm.probs))
glm.pred[glm.probs > 0.5] = "normal"
gdp.2007 = gdp[!train]
table(glm.pred, gdp.2007)


