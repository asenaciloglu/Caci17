getwd()
# setwd("/Users/edanurkahvecioglu/Desktop/3.Donem/CACII/SPW1")
data <- read.csv("QuestionaireData_CityTrips_csv.csv")
head(data)
library(dplyr)

Aleyk??m selam

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
lapply(liste, setNames, nm = colnames)

print(colnames)