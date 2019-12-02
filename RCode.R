getwd()
setwd("~/Caci17")
data <- read.csv("QuestionaireData_CityTrips.csv")
head(data)
install.packages("R.utils")
install.packages("corrplot")
install.packages("GPArotation")
library(dplyr)
library(R.utils)
library(MASS)
library(psych)
library(corrplot)
library(GPArotation)

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
colnames(data_comb) <- c("Group.1", "Friendly", "Historical", "Affordable", "Trendy", "Vibrant Nightlife", 
                        "Delicious Food", "Transportation", "Shopping", "Cultural Events", "Museums", 
                        "Clean", "Green", "International", "Too Touristic", "Fun", "Noisy", 
                        "Romantic", "Safe", "Beautiful", "English Speaker")


data_agg <-aggregate(data_comb[,-c(1)], by=list(data_comb$Group.1), mean, na.rm=TRUE)
data_agg
dist.df <- dist(data_agg[,-c(1)])
dist.df

dst <- data.matrix(dist.df)
dim <- ncol(dst)

# distance/similarity matrix grafik cizme
# Open a pdf file
# pdf("distanceMatrix.pdf") # bu kodu run ettikten sonra kapatmassan, grafik gozukmez R'de sadece dosyaya kaydeder
# eger grafigi gormek istiyosan bu kodu run etme!

# Create a plot
image(1:dim, 1:dim, dst, axes = FALSE, xlab="", ylab="", 
      col = gray.colors(12, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))

axis(1, 1:dim, data_agg[1:20,1], cex.axis = 0.5, las=3)
axis(2, 1:dim, data_agg[1:20,1], cex.axis = 0.5, las=1)
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", dst), cex=0.6)

# Close the pdf file
# dev.off() 

fit <- cmdscale(dist.df, k = 2) #izdusum
fit

x <- fit[,1]
y <- fit[,2]
#pdf("metrics_MDS.pdf")
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS", 
     pch = 20, ylim = c(-5.5, 5.5), xlim = c(-5.5, 5.5))
text(x, y, labels = data_agg$Group.1, cex = 0.5, pos = 3)
abline(h = 0, v = 0, col = "grey")
#dev.off()

summary(lm(data_agg[,2]~-1+fit))

print("asena")

# Property fitting
data_agg$x <- x
data_agg$y <- y
data_agg


head(data_agg)

profit <- lm(cbind(Friendly, Historical, Affordable, Trendy, `Vibrant Nightlife`, 
                   `Delicious Food`, Transportation, Shopping, `Cultural Events`, Museums, 
                   Clean, Green, International, `Too Touristic`, Fun, Noisy, 
                   Romantic, Safe, Beautiful, `English Speaker`)
             ~ -1 +  x + y, data = data_agg)
summary(profit)
coef(profit)
str(profit)

#pdf("Property_Fitting_xy.pdf")
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS", 
     pch = 19, ylim = c(-5.5, 5.5), xlim = c(-5.5, 5.5))
text(x, y, labels = data_agg$Group.1, cex = 0.5, pos = 3)
abline(h = 0, v = 0, col = "grey")
arrows(x0 = c(0, 0, 0), y0 = c(0, 0, 0), 
       x1 = coef(profit)[1, ]*10, y1 = coef(profit)[2, ]*10, col = 2, lwd = 1)
text(t(coef(profit)*10), colnames(coef(profit)*10), cex=0.4, col = 2, pos = c(2,2))
#dev.off()

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

# pdf("Property_Fitting_xy_q.pdf")
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Property  MDS", 
     pch = "*", ylim = c(-10.5, 11), xlim = c(-11, 11), col = "green")
text(x, y, labels = data_agg$Group.1, cex = 0.4, pos = 3, col = 3)
abline(h = 0, v = 0, col = "grey")
points(x = coef[2, ], y = coef[3, ], col = 1)
text(x= coef[2, ], y = coef[3, ], labels = colnames(coef), cex = 0.5, pos = 2,col=1)
# dev.off()

### PCA'ye gecmeden property fittingde yapmadiklarimiz var.


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

#Di??er doasyadan (TV stations vard??)

corrplot(corr=cor(data_comb[,2:21],use ="complete.obs"), method ="ellipse")
eigen(cor(data_comb[,2:21]))$values
plot(eigen(cor(data_comb[,2:21]))$values)

# START: Princial component analysis with singular value decomposition, no rotation
a.svd<-svd(cor(data_comb[,2:21]))
a.svd
d<-diag(a.svd$d)
d
a<-a.svd$u[,1:3]%*%sqrt(d[1:3,1:3])
a

a.svd$u[,1:3]%*%d[1:3,1:3]%*%t(a.svd$u[,1:3]) # Recomputed correlation based on first 3 principal components
cor(data_comb[,2:21])
diag(t(a.svd$u)%*%a.svd$u%*%d%*%t(a.svd$u)%*%a.svd$u)


# START: Princial component analysis with r-function prcomp, no rotation
data.scale<-apply(data_comb[,2:21],2,scale)
pr<-prcomp(data.scale)
?prcomp
pr
str(pr)
t(pr$sdev*t(pr$rotation)) # labeled as rotated but it is not rotated
# START: Princial component analysis with r-function prcomp, no rotation

# PCA and Factor analysis with easy to do R-FUNCTIONS
library(psych)
a.pca<-principal(data_comb[,2:21],nfactors=3, rotate ="none")
str(a.pca)
a.pca
a.pca<-principal(data_comb[,2:21],nfactors=3, rotate ="varimax")
a.pca
head(a.pca$score)
nrow(a.pca$score)
t(a.pca$scores)%*%a.pca$scores/(nrow(data_comb[,2:21])-1) # demonstrate that the principal components are uncorrelated after rotation
aggregate(a.pca$scores, by=list(data_comb$Group.1),mean, na.rm=TRUE)

summary(lm( Friendly ~ Historical+ Affordable+ Trendy+ VibrantNightlife+ 
                   `Delicious Food`+ Transportation+ Shopping+ `Cultural Events`+ Museums+ 
                   Clean+ Green+ International + `Too Touristic`+ Fun+ Noisy+ 
                   Romantic+ Safe+ Beautiful+ `English Speaker`, data=data_comb))

# factor analysis
?fa
a.fa<-fa(data_comb[,2:21],method=ml, SMC=TRUE, scores='regression',nfactors=3, rotate ="varimax")
a.fa<-fa(data_comb[,2:21],nfactors=3,rotate='varimax') # default setting
a.fa


# calculate factor score for objects
str(a.pca)
a.pca$scores
aggregate(a.fa$scores, by=list(data_comb$Group.1),mean, na.rm=TRUE)

# demonstrate that the factors after rotation are uncorrelated
a.fa.rot <-fa(data_comb[,2:21], scores='tenBerge',SMC=TRUE, fm='pa', nfactors =3,rotate ="varimax")
a.fa.rot
t(a.fa.rot$scores)%*%a.fa.rot$scores/(nrow(data_comb[,2:21])-1)
a.fa.rot$rot.mat
t(a.fa.rot$rot.mat)%*%a.fa.rot$rot.mat
?fa

# Oblique rotation
library(GPArotation)

a.fa.oblique<-fa(data_comb[,2:21],nfactors=3, scores='tenBerge', rotate ="oblimin")
a.fa.oblique
a.fa.oblique$Structure # correlation between variables and factors
a.fa.oblique$loadings # pattern (association) between variables and factors

cor(data_comb[,2:21])
a.fa$Structure
a.fa$Structure%*%t(a.fa$Structure)
a.fa$loadings%*%t(a.fa$loadings)
t(a.fa$scores)%*%a.fa$scores/nrow(data_comb[,2:21]-1)

a.pca.oblirot <-principal(data_comb[,2:21], nfactors =3,rotate ="oblimin")
a.pca.oblirot
a.pca.oblirot$loadings
a.pca.oblirot$Structure
a.z <- apply(data_comb[,2:21],2,scale)
str(a.z)
t(a.z)%*%a.z/(nrow(data_comb[,2:21])-1)

pattern<-solve(t(a.pca.oblirot$scores)%*%a.pca.oblirot$scores/(nrow(data_comb[,2:21])-1))%*%t(a.pca.oblirot$scores)%*%a.z/(nrow(data_comb[,2:21])-1)
structure<-t(a.pca.oblirot$scores)%*%a.pca.oblirot$scores%*%pattern/(nrow(data_comb[,2:21])-1)
structure
pattern
t(a.pca.oblirot$loadings)
t(a.pca.oblirot$Structure)
t(a.pca.oblirot$scores)%*%a.pca.oblirot$scores/(nrow(data_comb[,2:21])-1)



#### burda bitirdik

# START: Principal Component Analysis of aggregated data with SVD, no rotation 
attribute.agg.svd<-svd(cor(attribute.agg.eval.df[,2:15]))
l<-attribute.agg.svd$u*(sqrt(attribute.agg.svd$d))
l
attribute.agg.svd$d
attribute.agg.svd$u
attribute.agg.svd$u*t(sqrt(attribute.agg.svd$d))%x%matrix(1,nrow=14,ncol=1)
# END: Principal Component Analysis of aggregated data with SVD, no rotation 

eigen(cor(attribute.agg.eval.df[,2:15]))$values


a.pca<-principal(attribute.agg.eval.df[,2:15],nfactors=2, rotate ="varimax")
a.pca
head(attribute.agg.eval.df)
aggregate(a.pca$scores, by=list(attribute.agg.eval.df$Group.1),mean, na.rm=TRUE)
a.pca$scores

r1<-qr(cor(attribute.agg.eval.df[,2:15]))
r2<-qr(cor(attribute.eval.df[,3:16]))
r1$rank
r2$rank



