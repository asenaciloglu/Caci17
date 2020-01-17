#setup -----
setwd(("~/Caci17/SWP4"))
#install.packages('mlogit')
library(mlogit)
library(data.table)

data.cbc<-read.csv("cbc_data.csv")

#---- Upload SWP3 data with clusters ----
bluetooth <- read.csv("new_indiv.csv")
bluetooth1 <- bluetooth[bluetooth$clusters == "1", ]
id1 <- bluetooth1$id
bluetooth2 <- bluetooth[bluetooth$clusters == "2", ]
id2 <- bluetooth2$id
bluetooth3 <- bluetooth[bluetooth$clusters == "3", ]
id3 <- bluetooth3$id
bluetooth4 <- bluetooth[bluetooth$clusters == "4", ]
id4 <- bluetooth4$id

# select cluster members from cbc data

data.cbc_1 <- data.cbc[data.cbc$id %in% id1,]
data.cbc_2 <- data.cbc[data.cbc$id %in% id2,]
data.cbc_3 <- data.cbc[data.cbc$id %in% id3,]
data.cbc_4 <- data.cbc[data.cbc$id %in% id4,]


#---- Conjoint Analysis for the complete data----
data.cbc$price<-data.cbc$price/100 # niye abi ne alaka

# adding dummies for ommited variables
data.cbc$weight4 <- ifelse(data.cbc$weight1 == 0 & data.cbc$weight2 == 0 & 
                             data.cbc$weight3 == 0, 1, 0) # 700 grams
data.cbc$sound4 <- ifelse(data.cbc$sound1 == 0 & data.cbc$sound2 == 0 & 
                            data.cbc$sound3 == 0, 1, 0) # 5.0 stars
data.cbc$battery5 <- ifelse(data.cbc$battery1 == 0 & data.cbc$battery2 == 0 & 
                              data.cbc$battery3 == 0 & data.cbc$battery4 == 0, 1, 0)


head(data.cbc,8)
str(data.cbc)

data_ml_bluetooth <- mlogit.data(data = data.cbc, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl_bluetooth = mlogit(choice ~ -1 + none + price +
                         battery2 + battery3 + battery4 + battery5
                       + weight2 + weight3 + weight4 +
                         sound2 + sound3 + sound4, data = data_ml_bluetooth)
summary(mnl_bluetooth)


predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}


#MarketSimulation <- read.csv("marketsimulation.csv")
#MarketSimulation$price<-MarketSimulation$price/100
#head(MarketSimulation,12)


deneme <- predict.mnl(mnl_bluetooth,data.cbc[data.cbc$id == 6 ,])



#---- conjoint for cluster 1----
data.cbc_1$price<-data.cbc_1$price/100 # niye abi ne alaka

# adding dummies for ommited variables
data.cbc_1$weight4 <- ifelse(data.cbc_1$weight1 == 0 & data.cbc_1$weight2 == 0 & 
                             data.cbc_1$weight3 == 0, 1, 0) # 700 grams
data.cbc_1$sound4 <- ifelse(data.cbc_1$sound1 == 0 & data.cbc_1$sound2 == 0 & 
                            data.cbc_1$sound3 == 0, 1, 0) # 5.0 stars
data.cbc_1$battery5 <- ifelse(data.cbc_1$battery1 == 0 & data.cbc_1$battery2 == 0 & 
                              data.cbc_1$battery3 == 0 & data.cbc_1$battery4 == 0, 1, 0)


head(data.cbc_1,8)
str(data.cbc_1)

data_ml_bluetooth <- mlogit.data(data = data.cbc_1, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl_bluetooth = mlogit(choice ~ -1 + none + price +
                         battery2 + battery3 + battery4 + battery5
                       + weight2 + weight3 + weight4 +
                         sound2 + sound3 + sound4, data = data_ml_bluetooth)
summary(mnl_bluetooth)


predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}


#MarketSimulation <- read.csv("marketsimulation.csv")
#MarketSimulation$price<-MarketSimulation$price/100
#head(MarketSimulation,12)


deneme <- predict.mnl(mnl_bluetooth,data.cbc_1[data.cbc_1$id == 6 ,])


#---- conjoint for cluster 2----
data.cbc_2$price<-data.cbc_2$price/100 # niye abi ne alaka

# adding dummies for ommited variables
data.cbc_2$weight4 <- ifelse(data.cbc_2$weight1 == 0 & data.cbc_2$weight2 == 0 & 
                             data.cbc_2$weight3 == 0, 1, 0) # 700 grams
data.cbc_2$sound4 <- ifelse(data.cbc_2$sound1 == 0 & data.cbc_2$sound2 == 0 & 
                            data.cbc_2$sound3 == 0, 1, 0) # 5.0 stars
data.cbc_2$battery5 <- ifelse(data.cbc_2$battery1 == 0 & data.cbc_2$battery2 == 0 & 
                              data.cbc_2$battery3 == 0 & data.cbc_2$battery4 == 0, 1, 0)


head(data.cbc_2,8)
str(data.cbc_2)

data_ml_bluetooth <- mlogit.data(data = data.cbc_2, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl_bluetooth = mlogit(choice ~ -1 + none + price +
                         battery2 + battery3 + battery4 + battery5
                       + weight2 + weight3 + weight4 +
                         sound2 + sound3 + sound4, data = data_ml_bluetooth)
summary(mnl_bluetooth)


predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}


#MarketSimulation <- read.csv("marketsimulation.csv")
#MarketSimulation$price<-MarketSimulation$price/100
#head(MarketSimulation,12)


deneme <- predict.mnl(mnl_bluetooth,data.cbc_2[data.cbc_2$id == 6 ,])



#---- conjoint for cluster 3----
data.cbc_3<-read.csv("cbc_data.csv")
data.cbc_3$price<-data.cbc_3$price/100 # niye abi ne alaka

# adding dummies for ommited variables
data.cbc_3$weight4 <- ifelse(data.cbc_3$weight1 == 0 & data.cbc_3$weight2 == 0 & 
                             data.cbc_3$weight3 == 0, 1, 0) # 700 grams
data.cbc_3$sound4 <- ifelse(data.cbc_3$sound1 == 0 & data.cbc_3$sound2 == 0 & 
                            data.cbc_3$sound3 == 0, 1, 0) # 5.0 stars
data.cbc_3$battery5 <- ifelse(data.cbc_3$battery1 == 0 & data.cbc_3$battery2 == 0 & 
                              data.cbc_3$battery3 == 0 & data.cbc_3$battery4 == 0, 1, 0)


head(data.cbc_3,8)
str(data.cbc_3)

data_ml_bluetooth <- mlogit.data(data = data.cbc_3, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl_bluetooth = mlogit(choice ~ -1 + none + price +
                         battery2 + battery3 + battery4 + battery5
                       + weight2 + weight3 + weight4 +
                         sound2 + sound3 + sound4, data = data_ml_bluetooth)
summary(mnl_bluetooth)


predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}


#MarketSimulation <- read.csv("marketsimulation.csv")
#MarketSimulation$price<-MarketSimulation$price/100
#head(MarketSimulation,12)


deneme <- predict.mnl(mnl_bluetooth,data.cbc_3[data.cbc_3$id == 6 ,])


#---- conjoint for cluster 4----
data.cbc_4<-read.csv("cbc_data.csv")
data.cbc_4$price<-data.cbc_4$price/100 # niye abi ne alaka

# adding dummies for ommited variables
data.cbc_4$weight4 <- ifelse(data.cbc_4$weight1 == 0 & data.cbc_4$weight2 == 0 & 
                               data.cbc_4$weight3 == 0, 1, 0) # 700 grams
data.cbc_4$sound4 <- ifelse(data.cbc_4$sound1 == 0 & data.cbc_4$sound2 == 0 & 
                              data.cbc_4$sound3 == 0, 1, 0) # 5.0 stars
data.cbc_4$battery5 <- ifelse(data.cbc_4$battery1 == 0 & data.cbc_4$battery2 == 0 & 
                                data.cbc_4$battery3 == 0 & data.cbc_4$battery4 == 0, 1, 0)


head(data.cbc_4,8)
str(data.cbc_4)

data_ml_bluetooth <- mlogit.data(data = data.cbc_4, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl_bluetooth = mlogit(choice ~ -1 + none + price +
                         battery2 + battery3 + battery4 + battery5
                       + weight2 + weight3 + weight4 +
                         sound2 + sound3 + sound4, data = data_ml_bluetooth)
summary(mnl_bluetooth)


predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}


#MarketSimulation <- read.csv("marketsimulation.csv")
#MarketSimulation$price<-MarketSimulation$price/100
#head(MarketSimulation,12)


deneme <- predict.mnl(mnl_bluetooth,data.cbc_4[data.cbc_4$id == 6 ,])
