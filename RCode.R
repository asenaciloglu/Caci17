getwd()
setwd("~/Caci17")
data <- read.csv("QuestionaireData_CityTrips.csv")
head(data)
library(dplyr)
install.packages("R.utils")
library(R.utils)
library(MASS)
library(psych)


cities = c("Prague","Geneva","Paris","Stockholm","Brussels","London","Amsterdam",
           "Athens","Riga","Budapest","Dublin","Lisbon","Istanbul","Vienna","Rome","Barcelona",
           "Madrid","Berlin","Krakow","StPetersburg")
data_att <- data %>% dplyr:: select(Sample, ID, contains("Att"))
data_att

data_att$Sample <- as.factor(data_att$Sample)

prag <- data_att[data_att$Sample %in% c("1", "4", "5"), ]
prag <- prag %>% dplyr:: select(contains("Prague"))
prag <- cbind(City = "Prague", prag)
head(prag)

geneva <- data_att[data_att$Sample %in% c("1", "2", "3"), ]
geneva <- geneva %>% dplyr:: select(contains("Geneva"))
geneva <- cbind(City = "Geneva", geneva)
head(geneva)

paris <- data_att[data_att$Sample %in% c("1", "3", "4"), ]
paris <- paris %>% dplyr:: select(contains("Paris"))
paris <- cbind(City = "Paris", paris)
head(paris)

stockholm <- data_att[data_att$Sample %in% c("1", "2", "3"), ]
stockholm <- stockholm %>% dplyr:: select(contains("Stockholm"))
stockholm <- cbind(City = "Stockholm", stockholm)
head(stockholm)  

brussels <- data_att[data_att$Sample %in% c("1", "5", "6"), ]
brussels <- brussels %>% dplyr:: select(contains("Brussels"))
brussels <- cbind(City = "Brussels", brussels)
head(brussels)  

london <- data_att[data_att$Sample %in% c("1", "2", "3"), ]
london <- london %>% dplyr:: select(contains("London"))
london <- cbind(City = "London", london)
head(london) 

amsterdam <- data_att[data_att$Sample %in% c("2", "3", "8"), ]
amsterdam <- amsterdam %>% dplyr:: select(contains("Amsterdam"))
amsterdam <- cbind(City = "Amsterdam", amsterdam)
head(amsterdam) 

athens <- data_att[data_att$Sample %in% c("2", "3", "6"), ]
athens <- athens %>% dplyr:: select(contains("Athens"))
athens <- cbind(City = "Athens", athens)
head(athens) 

riga <- data_att[data_att$Sample %in% c("2", "4", "5"), ]
riga <- riga %>% dplyr:: select(contains("Riga"))
riga <- cbind(City = "Riga", riga)
head(riga)

budapest <- data_att[data_att$Sample %in% c("4", "9", "10"), ]
budapest <- budapest %>% dplyr:: select(contains("Budapest"))
budapest <- cbind(City = "Budapest", budapest)
head(budapest)

dublin <- data_att[data_att$Sample %in% c("4", "7", "10"), ]
dublin <- dublin %>% dplyr:: select(contains("Dublin"))
dublin <- cbind(City = "Dublin", dublin)
head(dublin)

lisbon <- data_att[data_att$Sample %in% c("4", "7", "9"), ]
lisbon <- lisbon %>% dplyr:: select(contains("Lisbon"))
lisbon <- cbind(City = "Lisbon", lisbon)
head(lisbon)

istanbul <- data_att[data_att$Sample %in% c("5", "8", "10"), ]
istanbul <- istanbul %>% dplyr:: select(contains("Istanbul"))
istanbul <- cbind(City = "Istanbul", istanbul)
head(istanbul)

vienna <- data_att[data_att$Sample %in% c("5", "8", "10"), ]
vienna <- vienna %>% dplyr:: select(contains("Vienna"))
vienna <- cbind(City = "Vienna", vienna)
head(vienna)

rome <- data_att[data_att$Sample %in% c("5", "7", "9"), ]
rome <- rome %>% dplyr:: select(contains("Rome"))
rome <- cbind(City = "Rome", rome)
head(rome)

barcelona <- data_att[data_att$Sample %in% c("6", "7", "10"), ]
barcelona <- barcelona %>% dplyr:: select(contains("Barcelona"))
barcelona <- cbind(City = "Barcelona", barcelona)
head(barcelona)

madrid <- data_att[data_att$Sample %in% c("6", "8", "9"), ]
madrid <- madrid %>% dplyr:: select(contains("Madrid"))
madrid <- cbind(City = "Madrid", madrid)
head(madrid)

berlin <- data_att[data_att$Sample %in% c("6", "7", "8"), ]
berlin <- berlin %>% dplyr:: select(contains("Berlin"))
berlin <- cbind(City = "Berlin", berlin)
head(berlin)

krakow <- data_att[data_att$Sample %in% c("6", "7", "9"), ]
krakow <- krakow %>% dplyr:: select(contains("Krakow"))
krakow <- cbind(City = "Krakow", krakow)
head(krakow)

st.petersburg <- data_att[data_att$Sample %in% c("8", "9", "10"), ]
st.petersburg <- st.petersburg %>% dplyr:: select(contains("StPetersburg"))
st.petersburg <- cbind(City = "StPetersburg", st.petersburg)
head(st.petersburg)

liste <- list(prag, geneva, paris, stockholm, amsterdam, st.petersburg, barcelona, budapest, brussels, berlin, 
              istanbul, athens, dublin, krakow, rome, london, riga, madrid, vienna, lisbon)

colnames <- sub('.*_', '', colnames(prag))
colnames  
colnames(prag) <- colnames
colnames(geneva) <- colnames
colnames(paris) <- colnames
colnames(stockholm) <- colnames
colnames(brussels) <- colnames
colnames(london) <- colnames
colnames(amsterdam) <- colnames
colnames(athens) <- colnames
colnames(riga) <- colnames
colnames(budapest) <- colnames
colnames(dublin) <- colnames
colnames(lisbon) <- colnames
colnames(istanbul) <- colnames
colnames(vienna) <- colnames
colnames(rome) <- colnames
colnames(barcelona) <- colnames
colnames(berlin) <- colnames
colnames(krakow) <- colnames
colnames(st.petersburg) <- colnames
colnames(madrid) <- colnames

data_comb <- rbind(prag, geneva, paris, stockholm, brussels, london, amsterdam, athens, riga, budapest,
                  dublin, lisbon, istanbul, vienna, rome, barcelona, berlin, krakow, st.petersburg, madrid) 

data_comb
colnames(data_comb) <- c("Group.1", "Friendly", "Historical", "Affordable", "Trendy", "VibrantNightlife", 
                        "Delicious Food", "Transportation", "Shopping", "Cultural Events", "Museums", 
                        "Clean", "Green", "International", "Too Touristic", "Fun", "Noisy", 
                        "Romantic", "Safe", "Beautiful", "English Speaker")


data_agg <-aggregate(data_comb[,-c(1)], by=list(data_comb$City), mean, na.rm=TRUE)
data_agg
dist.df <- dist(data_agg[,-c(1)])
dist.df
fit <- cmdscale(dist.df, k = 2)
fit

x <- fit[,1]
y <- fit[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS", 
     pch = 19, ylim = c(-5.5, 5.5), xlim = c(-5.5, 5.5))
text(x, y, labels = data_agg$Group.1, cex = 1, pos = 4)
abline(h = 0, v = 0, col = "grey")

summary(lm(data_agg[,2]~-1+fit))

print("asena")

# Property fitting
data_agg$x <- x
data_agg$y <- y
data_agg


head(data_agg)

profit <- lm(cbind(Friendly, Historical, Affordable, Trendy, VibrantNightlife, 
                   `Delicious Food`, Transportation, Shopping, `Cultural Events`, Museums, 
                   Clean, Green, International, `Too Touristic`, Fun, Noisy, 
                   Romantic, Safe, Beautiful, `English Speaker`)
             ~ -1 +  x + y, data = data_agg)
summary(profit)
coef(profit)
str(profit)
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS", 
     pch = 19, ylim = c(-5.5, 5.5), xlim = c(-5.5, 5.5))
text(x, y, labels = data_agg$Group.1, cex = 0.75, pos = 4)
abline(h = 0, v = 0, col = "grey")
arrows(x0 = c(0, 0, 0), y0 = c(0, 0, 0), 
       x1 = coef(profit)[1, ]*10, y1 = coef(profit)[2, ]*10, col = 2, lwd = 1)
text(t(coef(profit)*10), colnames(coef(profit)*10), cex=0.5, col = 1, pos = 4)

column_names = c("Friendly", "Historical", "Affordable", "Trendy", "Vibrant Nightlife", 
                 "Delicious Food", "Transportation", "Shopping", "Cultural Events", "Museums", 
                 "Clean", "Green", "International", "Too Touristic", "Fun", "Noisy", "Romantic", 
                 "Safe", "Beautiful", "English-Speaker")
column_names

#Property Fitting with ideal points
data_agg$q <- x*x+y*y

profit_q <- lm(cbind(Friendly, Historical, Affordable, Trendy, `Vibrant Nightlife`, 
                   `Delicious Food`, Transportation, Shopping, `Cultural Events`, Museums, 
                   Clean, Green, International, `Too Touristic`, Fun, Noisy, 
                   Romantic, Safe, Beautiful, `English Speaker`)
             ~ 1 +  x + y + q, data = data_agg)
summary(profit_q)

coef<-coef(profit_q)
coef
coef[2,]<-coef[2,]/(-2*coef[4,])
coef[3,]<-coef[3,]/(-2*coef[4,])

coef
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS", 
     pch = 19, ylim = c(-15, 15), xlim = c(-15, 15))
text(x, y, labels = data_agg$Group.1, cex = 0.75, pos = 4)
abline(h = 0, v = 0, col = "grey")

points(x = coef[2, ], y = coef[3, ], col = 3)
text(x= coef[2, ], y= coef[3, ], labels = colnames(coef), cex = 0.75, pos = 4,col=3)

#PCA disaggregated dosyasindan
#PCA
data_comb <- na.omit(data_comb)
eigen(cor(data_comb[,2:21]))$values
plot(eigen(cor(data_comb[,2:21]))$values)
data_comb
a.pca<-principal(data_comb[,2:21],nfactors=4, rotate ="varimax")
a.pca
head(a.pca$scores)
head(data_comb)
aggregate(a.pca$scores, by=list(data_comb$Group.1), mean, na.rm=TRUE) # compute aggregate component score for cities
t(a.pca$scores)%*%a.pca$scores/(nrow(data_comb[,2:21])-1) # show taht factors are uncorrelated

# factor analysis
a.fa<-fa(data_comb[,2:21],method=mle,scores='tenBerge', nfactors=4, rotate ="varimax")
a.fa
aggregate(a.fa$scores, by=list(data_comb$Group.1),mean, na.rm=TRUE) # compute aggregate component score for cities
t(a.fa$scores)%*%a.fa$scores/(nrow(attribute[,2:21])-1) # show that factors are uncorrelated

#COMPARE RESULTS WITH 2-DIMENSIONAL MDS
a.fa<-fa(data_comb[,2:21],method=mle,scores='tenBerge', nfactors=2, rotate ="varimax")
a.fa
scores<-aggregate(a.fa$scores, by=list(data_comb$Group.1),mean, na.rm=TRUE)
x <- scores[,2]
y <- scores[,3]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS", 
     pch = 19, ylim = c(-2, 2), xlim = c(-2, 2))
text(x, y, labels = scores[,1], cex = 1, pos = 4)
abline(h = 0, v = 0, col = "grey")



