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


# deneme <- predict.mnl(mnl_bluetooth,data.cbc[data.cbc$id == 6 ,])

#---- conjoint for cluster 1----
data.cbc_1$price <- data.cbc_1$price/100 # niye abi ne alaka

# adding dummies for ommited variables
data.cbc_1$weight4 <- ifelse(data.cbc_1$weight1 == 0 & data.cbc_1$weight2 == 0 & 
                             data.cbc_1$weight3 == 0, 1, 0) # 700 grams
data.cbc_1$sound4 <- ifelse(data.cbc_1$sound1 == 0 & data.cbc_1$sound2 == 0 & 
                            data.cbc_1$sound3 == 0, 1, 0) # 5.0 stars
data.cbc_1$battery5 <- ifelse(data.cbc_1$battery1 == 0 & data.cbc_1$battery2 == 0 & 
                              data.cbc_1$battery3 == 0 & data.cbc_1$battery4 == 0, 1, 0)


head(data.cbc_1,8)
str(data.cbc_1)

data1_ml_bluetooth <- mlogit.data(data = data.cbc_1, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt") # uygun data frame haline getir

mnl1_bluetooth = mlogit(choice ~ -1 + none + price +
                         battery2 + battery3 + battery4 + battery5
                       + weight2 + weight3 + weight4 +
                         sound2 + sound3 + sound4, data = data1_ml_bluetooth)# buyuk data train
summary(mnl1_bluetooth)


predict.mnl <- function(model , data ) {
  data.model <- model.matrix(
    update(model$formula, 0 ~ .),
    data = data )
  utility <- data.model %*% model$coef
  share <- exp( utility )/sum (exp ( utility ))
  cbind (share , data )
}


deneme <- predict.mnl(mnl1_bluetooth, data.cbc_1[data.cbc_1$id == 9 ,])

# tüm cluster icin uygulayip kisi bazinda max cekiyorum
# deneme1 <- predict.mnl(mnl_bluetooth,data.cbc_1)
# offer_1 <- as.data.table(deneme1)[,.SD[which.max(share)],by=id]

# cluster daki her abi/abla ustunden donup max share ini alıp offer1 df sine eklemece
offer1 = data.frame()

for (x in unique(data.cbc_1$id)) {
  predictionCase <- predict.mnl(mnl_bluetooth, data.cbc_1[data.cbc_1$id == x ,])
  offer1 <- rbind(offer1, as.vector(predictionCase[which.max(predictionCase$share),]))
}

cols = as.vector(colnames(data.cbc))[6:19] # price tan... son attribute a

offer1$AttributeCombined <- apply(offer1[, cols] ,1,paste , collapse = '') # urun kodu

offer1$count <- ave(as.numeric(offer1$id), offer1$AttributeCombined, FUN = length)

offer1_fave <- offer1[which.max(offer1$count),]
offer1$fave <-NULL


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

data2_ml_bluetooth <- mlogit.data(data = data.cbc_2, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl2_bluetooth = mlogit(choice ~ -1 + none + price +
                         battery2 + battery3 + battery4 + battery5
                       + weight2 + weight3 + weight4 +
                         sound2 + sound3 + sound4, data = data2_ml_bluetooth)


offer2 = data.frame()

for (x in unique(data.cbc_2$id)) {
  predictionCase <- predict.mnl(mnl_bluetooth, data.cbc_2[data.cbc_2$id == x ,])
  offer2 <- rbind(offer2, as.vector(predictionCase[which.max(predictionCase$share),]))
}

offer2$AttributeCombined <- apply(offer2[, cols] ,1,paste , collapse = '') # urun kodu

offer2$count <- ave(as.numeric(offer2$id), offer2$AttributeCombined, FUN = length)

offer2_fave <- offer2[which.max(offer2$count),]

#---- conjoint for cluster 3----
data.cbc_3$price<-data.cbc_3$price/100 # niye abi ne alaka

# adding dummies for ommited variables
data.cbc_3$weight4 <- ifelse(data.cbc_3$weight1 == 0 & data.cbc_3$weight2 == 0 & 
                             data.cbc_3$weight3 == 0, 1, 0) # 700 grams
data.cbc_3$sound4 <- ifelse(data.cbc_3$sound1 == 0 & data.cbc_3$sound2 == 0 & 
                            data.cbc_3$sound3 == 0, 1, 0) # 5.0 stars
data.cbc_3$battery5 <- ifelse(data.cbc_3$battery1 == 0 & data.cbc_3$battery2 == 0 & 
                              data.cbc_3$battery3 == 0 & data.cbc_3$battery4 == 0, 1, 0)


data3_ml_bluetooth <- mlogit.data(data = data.cbc_3, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl3_bluetooth = mlogit(choice ~ -1 + none + price +
                         battery2 + battery3 + battery4 + battery5
                       + weight2 + weight3 + weight4 +
                         sound2 + sound3 + sound4, data = data3_ml_bluetooth)

offer3 = data.frame()

for (x in unique(data.cbc_3$id)) {
  predictionCase <- predict.mnl(mnl3_bluetooth, data.cbc_3[data.cbc_3$id == x ,])
  offer3 <- rbind(offer3, as.vector(predictionCase[which.max(predictionCase$share),]))
}

offer3$AttributeCombined <- apply(offer3[, cols] ,1,paste , collapse = '') # urun kodu

offer3$count <- ave(as.numeric(offer3$id), offer3$AttributeCombined, FUN = length)

offer3_fave <- offer3[which.max(offer3$count),]
offer3_fave

#---- conjoint for cluster 4----
data.cbc_4$price<-data.cbc_4$price/100 # niye abi ne alaka

# adding dummies for ommited variables
data.cbc_4$weight4 <- ifelse(data.cbc_4$weight1 == 0 & data.cbc_4$weight2 == 0 & 
                               data.cbc_4$weight3 == 0, 1, 0) # 700 grams
data.cbc_4$sound4 <- ifelse(data.cbc_4$sound1 == 0 & data.cbc_4$sound2 == 0 & 
                              data.cbc_4$sound3 == 0, 1, 0) # 5.0 stars
data.cbc_4$battery5 <- ifelse(data.cbc_4$battery1 == 0 & data.cbc_4$battery2 == 0 & 
                                data.cbc_4$battery3 == 0 & data.cbc_4$battery4 == 0, 1, 0)

data4_ml_bluetooth <- mlogit.data(data = data.cbc_4, choice = "choice", shape = "long",
                                 id.var = "id", alt.var = "alt")

mnl4_bluetooth = mlogit(choice ~ -1 + none + price +
                         battery2 + battery3 + battery4 + battery5
                       + weight2 + weight3 + weight4 +
                         sound2 + sound3 + sound4, data = data4_ml_bluetooth)

offer4 = data.frame()

for (x in unique(data.cbc_4$id)) {
  predictionCase <- predict.mnl(mnl4_bluetooth, data.cbc_4[data.cbc_4$id == x ,])
  offer4 <- rbind(offer4, as.vector(predictionCase[which.max(predictionCase$share),]))
}

offer4$AttributeCombined <- apply(offer4[, cols] ,1,paste , collapse = '') # urun kodu

offer4$count <- ave(as.numeric(offer4$id), offer4$AttributeCombined, FUN = length)

offer4_fave <- offer4[which.max(offer4$count),]
offer4_fave


faves <- rbind(offer1_fave, offer2_fave, offer3_fave, offer4_fave)
faves$cluster <- c(1,2,3,4)
faves$clusterRespondents <- c(151, 185, 60, 179)
faves$percentage <- c(0.10, 0.09, 0.11, 0.11)
