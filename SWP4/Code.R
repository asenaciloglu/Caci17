# essalatu
setwd(("~/Caci17/SWP4"))
# install.packages('mlogit')
library(mlogit)
library(data.table)

data.cbc<-read.csv("cbc_data.csv")
data.cbc$price<-data.cbc$price/100 # niye abi ne alaka

# adding dummies for ommited variables
data.cbc$weight4 <- ifelse(data.cbc$weight1 == 0 & data.cbc$weight2 == 0 & 
                             data.cbc$weight3 == 0, 1, 0) # 700 grams
data.cbc$sound4 <- ifelse(data.cbc$sound1 == 0 & data.cbc$sound2 == 0 & 
                             data.cbc$sound3 == 0, 1, 0) # 5.0 stars

head(data.cbc,8)
str(data.cbc)

data_ml_bluetooth <- mlogit.data(data.cbc, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl_bluetooth = mlogit(choice ~ -1 + none + price + battery1 + battery2 + battery3 + battery4
                       + weight1 + weight2 + weight3 +
                         sound1 + sound2 + sound3, data = data_ml_bluetooth)
summary(mnl_bluetooth)


predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}


MarketSimulation <- read.csv("marketsimulation.csv")
MarketSimulation$price<-MarketSimulation$price/100
head(MarketSimulation,12)


predict.mnl(mnl_bluetooth,MarketSimulation)
