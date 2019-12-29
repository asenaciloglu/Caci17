getwd()
setwd("~/Caci17/SWP3")

bluetooth <- read.csv("indivData.csv")


###---- Brand Awarenes ----

#bakalim kac kisi biliyormus
head <- c("Anker", "Bose", "JBL", "Philips", "Sony", "UE", "HarmanKardon", "Beats", "None", "Observations", "0", "1", "2", "3", "4", "5", "6", "7", "8", "Avr")

counts_general <- as.vector(rowSums(bluetooth[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
#let's make a new column with counts general
bluetooth$brand_awareness <- as.factor(counts_general)
plot(bluetooth$brand_awareness, xlab="# Known Brands by Respondents", ylab = "Frequency", ylim=c(0,120), main = "Brand Awareness", col = "rosybrown3", cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5, cex.names = 1.5)

general_sums <- c(sum(bluetooth$BrandAwareness_Anker), 
              sum(bluetooth$BrandAwareness_Bose),
              sum(bluetooth$BrandAwareness_JBL),
              sum(bluetooth$BrandAwareness_Philips),
              sum(bluetooth$BrandAwareness_Sony),
              sum(bluetooth$BrandAwareness_UE),
              sum(bluetooth$BrandAwareness_HarmanKardon),
              sum(bluetooth$BrandAwareness_Beats),
              sum(bluetooth$BrandAwareness_None),
              nrow(bluetooth),
              sum(counts_general == "0"),
              sum(counts_general == "1"),
              sum(counts_general == "2"),
              sum(counts_general == "3"),
              sum(counts_general == "4"),
              sum(counts_general == "5"),
              sum(counts_general == "6"),
              sum(counts_general == "7"),
              sum(counts_general == "8"),
              mean(counts_general)) #there is one person did not respond this question!

#hicbirini bilmiyorum diyen ama yine de baska markayi biliyorum diye isaretleyen olmus mu- olmamis baktim.

#cinsiyete gore ayiralim
female <- bluetooth[bluetooth$GenderLabel == "female", ]
male <- bluetooth[bluetooth$GenderLabel == "male", ]
no_gender_info <- bluetooth[bluetooth$GenderLabel == "Prefer not to answer", ]

counts_female <- as.vector(rowSums(female[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
female_sums <- c(sum(female$BrandAwareness_Anker), 
                  sum(female$BrandAwareness_Bose),
                  sum(female$BrandAwareness_JBL),
                  sum(female$BrandAwareness_Philips),
                  sum(female$BrandAwareness_Sony),
                  sum(female$BrandAwareness_UE),
                  sum(female$BrandAwareness_HarmanKardon),
                  sum(female$BrandAwareness_Beats),
                  sum(female$BrandAwareness_None),
                  nrow(female),
                  sum(counts_female == "0"),
                  sum(counts_female == "1"),
                  sum(counts_female == "2"),
                  sum(counts_female == "3"),
                  sum(counts_female == "4"),
                  sum(counts_female == "5"),
                  sum(counts_female == "6"),
                  sum(counts_female == "7"),
                  sum(counts_female == "8"),
                  mean(counts_female))

counts_male <- as.vector(rowSums(male[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
male_sums <- c(sum(male$BrandAwareness_Anker), 
                  sum(male$BrandAwareness_Bose),
                  sum(male$BrandAwareness_JBL),
                  sum(male$BrandAwareness_Philips),
                  sum(male$BrandAwareness_Sony),
                  sum(male$BrandAwareness_UE),
                  sum(male$BrandAwareness_HarmanKardon),
                  sum(male$BrandAwareness_Beats),
                  sum(male$BrandAwareness_None),
                  nrow(male),
                  sum(counts_male == "0"),
                  sum(counts_male == "1"),
                  sum(counts_male == "2"),
                  sum(counts_male == "3"),
                  sum(counts_male == "4"),
                  sum(counts_male == "5"),
                  sum(counts_male == "6"),
                  sum(counts_male == "7"),
                  sum(counts_male == "8"),
                  mean(counts_male))

counts_gender <- as.vector(rowSums(no_gender_info[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
no_gender_sums <- c(sum(no_gender_info$BrandAwareness_Anker), 
                  sum(no_gender_info$BrandAwareness_Bose),
                  sum(no_gender_info$BrandAwareness_JBL),
                  sum(no_gender_info$BrandAwareness_Philips),
                  sum(no_gender_info$BrandAwareness_Sony),
                  sum(no_gender_info$BrandAwareness_UE),
                  sum(no_gender_info$BrandAwareness_HarmanKardon),
                  sum(no_gender_info$BrandAwareness_Beats),
                  sum(no_gender_info$BrandAwareness_None),
                  nrow(no_gender_info),
                  sum(counts_gender == "0"),
                  sum(counts_gender == "1"),
                  sum(counts_gender == "2"),
                  sum(counts_gender == "3"),
                  sum(counts_gender == "4"),
                  sum(counts_gender == "5"),
                  sum(counts_gender == "6"),
                  sum(counts_gender == "7"),
                  sum(counts_gender == "8"),
                  mean(counts_gender))
#by their occupation
employed <- bluetooth[bluetooth$OccupationLabel == "Employed",]
retired <- bluetooth[bluetooth$OccupationLabel == "Retired",]
selfemployed <- bluetooth[bluetooth$OccupationLabel == "Self-employed",]
student <- bluetooth[bluetooth$OccupationLabel == "Student",]
unemployed <- bluetooth[bluetooth$OccupationLabel == "Unemployed",]

counts_employed <- as.vector(rowSums(employed[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
employed_sums <- c(sum(employed$BrandAwareness_Anker), 
                 sum(employed$BrandAwareness_Bose),
                 sum(employed$BrandAwareness_JBL),
                 sum(employed$BrandAwareness_Philips),
                 sum(employed$BrandAwareness_Sony),
                 sum(employed$BrandAwareness_UE),
                 sum(employed$BrandAwareness_HarmanKardon),
                 sum(employed$BrandAwareness_Beats),
                 sum(employed$BrandAwareness_None),
                 nrow(employed),
                 sum(counts_employed == "0"),
                 sum(counts_employed == "1"),
                 sum(counts_employed == "2"),
                 sum(counts_employed == "3"),
                 sum(counts_employed == "4"),
                 sum(counts_employed == "5"),
                 sum(counts_employed == "6"),
                 sum(counts_employed == "7"),
                 sum(counts_employed == "8"),
                 mean(counts_employed))

counts_retired <- as.vector(rowSums(retired[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
retired_sums <- c(sum(retired$BrandAwareness_Anker), 
               sum(retired$BrandAwareness_Bose),
               sum(retired$BrandAwareness_JBL),
               sum(retired$BrandAwareness_Philips),
               sum(retired$BrandAwareness_Sony),
               sum(retired$BrandAwareness_UE),
               sum(retired$BrandAwareness_HarmanKardon),
               sum(retired$BrandAwareness_Beats),
               sum(retired$BrandAwareness_None),
               nrow(retired),
               sum(counts_retired == "0"),
               sum(counts_retired == "1"),
               sum(counts_retired == "2"),
               sum(counts_retired == "3"),
               sum(counts_retired == "4"),
               sum(counts_retired == "5"),
               sum(counts_retired == "6"),
               sum(counts_retired == "7"),
               sum(counts_retired == "8"),
               mean(counts_retired))

counts_selfemployed <- as.vector(rowSums(selfemployed[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
selfemployed_sums <- c(sum(selfemployed$BrandAwareness_Anker), 
                    sum(selfemployed$BrandAwareness_Bose),
                    sum(selfemployed$BrandAwareness_JBL),
                    sum(selfemployed$BrandAwareness_Philips),
                    sum(selfemployed$BrandAwareness_Sony),
                    sum(selfemployed$BrandAwareness_UE),
                    sum(selfemployed$BrandAwareness_HarmanKardon),
                    sum(selfemployed$BrandAwareness_Beats),
                    sum(selfemployed$BrandAwareness_None),
                    nrow(selfemployed),
                    sum(counts_selfemployed == "0"),
                    sum(counts_selfemployed == "1"),
                    sum(counts_selfemployed == "2"),
                    sum(counts_selfemployed == "3"),
                    sum(counts_selfemployed == "4"),
                    sum(counts_selfemployed == "5"),
                    sum(counts_selfemployed == "6"),
                    sum(counts_selfemployed == "7"),
                    sum(counts_selfemployed == "8"),
                    mean(counts_selfemployed))

counts_student <- as.vector(rowSums(student[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
student_sums <- c(sum(student$BrandAwareness_Anker), 
                      sum(student$BrandAwareness_Bose),
                      sum(student$BrandAwareness_JBL),
                      sum(student$BrandAwareness_Philips),
                      sum(student$BrandAwareness_Sony),
                      sum(student$BrandAwareness_UE),
                      sum(student$BrandAwareness_HarmanKardon),
                      sum(student$BrandAwareness_Beats),
                      sum(student$BrandAwareness_None),
                      nrow(student),
                      sum(counts_student == "0"),
                      sum(counts_student == "1"),
                      sum(counts_student == "2"),
                      sum(counts_student == "3"),
                      sum(counts_student == "4"),
                      sum(counts_student == "5"),
                      sum(counts_student == "6"),
                      sum(counts_student == "7"),
                      sum(counts_student == "8"),
                      mean(counts_student))

counts_unemployed <- as.vector(rowSums(unemployed[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
unemployed_sums <- c(sum(unemployed$BrandAwareness_Anker), 
                       sum(unemployed$BrandAwareness_Bose),
                       sum(unemployed$BrandAwareness_JBL),
                       sum(unemployed$BrandAwareness_Philips),
                       sum(unemployed$BrandAwareness_Sony),
                       sum(unemployed$BrandAwareness_UE),
                       sum(unemployed$BrandAwareness_HarmanKardon),
                       sum(unemployed$BrandAwareness_Beats),
                       sum(unemployed$BrandAwareness_None),
                       nrow(unemployed),
                       sum(counts_unemployed == "0"),
                       sum(counts_unemployed == "1"),
                       sum(counts_unemployed == "2"),
                       sum(counts_unemployed == "3"),
                       sum(counts_unemployed == "4"),
                       sum(counts_unemployed == "5"),
                       sum(counts_unemployed == "6"),
                       sum(counts_unemployed == "7"),
                       sum(counts_unemployed == "8"),
                       mean(counts_unemployed))

#by income
levels(bluetooth$IncomeLabel)
#we have 8 levels, 1 to 7 is going in an increasing scale, while 8 is not specified 
bluetooth$Income <- as.factor(bluetooth$Income)
income1 <- bluetooth[bluetooth$Income == "1",]
income2 <- bluetooth[bluetooth$Income == "2",]
income3 <- bluetooth[bluetooth$Income == "3",]
income4 <- bluetooth[bluetooth$Income == "4",]
income5 <- bluetooth[bluetooth$Income == "5",]
income6 <- bluetooth[bluetooth$Income == "6",]
income7 <- bluetooth[bluetooth$Income == "7",]
income8 <- bluetooth[bluetooth$Income == "8",]


counts_income1 <- as.vector(rowSums(income1[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
income1_sums <- c(sum(income1$BrandAwareness_Anker), 
                     sum(income1$BrandAwareness_Bose),
                     sum(income1$BrandAwareness_JBL),
                     sum(income1$BrandAwareness_Philips),
                     sum(income1$BrandAwareness_Sony),
                     sum(income1$BrandAwareness_UE),
                     sum(income1$BrandAwareness_HarmanKardon),
                     sum(income1$BrandAwareness_Beats),
                     sum(income1$BrandAwareness_None),
                     nrow(income1),
                     sum(counts_income1 == "0"),
                     sum(counts_income1 == "1"),
                     sum(counts_income1 == "2"),
                     sum(counts_income1 == "3"),
                     sum(counts_income1 == "4"),
                     sum(counts_income1 == "5"),
                     sum(counts_income1 == "6"),
                     sum(counts_income1 == "7"),
                     sum(counts_income1 == "8"),
                     mean(counts_income1))

counts_income2 <- as.vector(rowSums(income2[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
income2_sums <- c(sum(income2$BrandAwareness_Anker), 
                     sum(income2$BrandAwareness_Bose),
                     sum(income2$BrandAwareness_JBL),
                     sum(income2$BrandAwareness_Philips),
                     sum(income2$BrandAwareness_Sony),
                     sum(income2$BrandAwareness_UE),
                     sum(income2$BrandAwareness_HarmanKardon),
                     sum(income2$BrandAwareness_Beats),
                     sum(income2$BrandAwareness_None),
                     nrow(income2),
                     sum(counts_income2 == "0"),
                     sum(counts_income2 == "1"),
                     sum(counts_income2 == "2"),
                     sum(counts_income2 == "3"),
                     sum(counts_income2 == "4"),
                     sum(counts_income2 == "5"),
                     sum(counts_income2 == "6"),
                     sum(counts_income2 == "7"),
                     sum(counts_income2 == "8"),
                     mean(counts_income2))


counts_income3 <- as.vector(rowSums(income3[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
income3_sums <- c(sum(income3$BrandAwareness_Anker), 
                     sum(income3$BrandAwareness_Bose),
                     sum(income3$BrandAwareness_JBL),
                     sum(income3$BrandAwareness_Philips),
                     sum(income3$BrandAwareness_Sony),
                     sum(income3$BrandAwareness_UE),
                     sum(income3$BrandAwareness_HarmanKardon),
                     sum(income3$BrandAwareness_Beats),
                     sum(income3$BrandAwareness_None),
                     nrow(income3),
                     sum(counts_income3 == "0"),
                     sum(counts_income3 == "1"),
                     sum(counts_income3 == "2"),
                     sum(counts_income3 == "3"),
                     sum(counts_income3 == "4"),
                     sum(counts_income3 == "5"),
                     sum(counts_income3 == "6"),
                     sum(counts_income3 == "7"),
                     sum(counts_income3 == "8"),
                     mean(counts_income3))

counts_income4 <- as.vector(rowSums(income4[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
income4_sums <- c(sum(income4$BrandAwareness_Anker), 
                     sum(income4$BrandAwareness_Bose),
                     sum(income4$BrandAwareness_JBL),
                     sum(income4$BrandAwareness_Philips),
                     sum(income4$BrandAwareness_Sony),
                     sum(income4$BrandAwareness_UE),
                     sum(income4$BrandAwareness_HarmanKardon),
                     sum(income4$BrandAwareness_Beats),
                     sum(income4$BrandAwareness_None),
                     nrow(income4),
                     sum(counts_income4 == "0"),
                     sum(counts_income4 == "1"),
                     sum(counts_income4 == "2"),
                     sum(counts_income4 == "3"),
                     sum(counts_income4 == "4"),
                     sum(counts_income4 == "5"),
                     sum(counts_income4 == "6"),
                     sum(counts_income4 == "7"),
                     sum(counts_income4 == "8"),
                     mean(counts_income4))

counts_income5 <- as.vector(rowSums(income5[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
income5_sums <- c(sum(income5$BrandAwareness_Anker), 
                     sum(income5$BrandAwareness_Bose),
                     sum(income5$BrandAwareness_JBL),
                     sum(income5$BrandAwareness_Philips),
                     sum(income5$BrandAwareness_Sony),
                     sum(income5$BrandAwareness_UE),
                     sum(income5$BrandAwareness_HarmanKardon),
                     sum(income5$BrandAwareness_Beats),
                     sum(income5$BrandAwareness_None),
                     nrow(income5),
                     sum(counts_income5 == "0"),
                     sum(counts_income5 == "1"),
                     sum(counts_income5 == "2"),
                     sum(counts_income5 == "3"),
                     sum(counts_income5 == "4"),
                     sum(counts_income5 == "5"),
                     sum(counts_income5 == "6"),
                     sum(counts_income5 == "7"),
                     sum(counts_income5 == "8"),
                     mean(counts_income5))

counts_income6 <- as.vector(rowSums(income6[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
income6_sums <- c(sum(income6$BrandAwareness_Anker), 
                     sum(income6$BrandAwareness_Bose),
                     sum(income6$BrandAwareness_JBL),
                     sum(income6$BrandAwareness_Philips),
                     sum(income6$BrandAwareness_Sony),
                     sum(income6$BrandAwareness_UE),
                     sum(income6$BrandAwareness_HarmanKardon),
                     sum(income6$BrandAwareness_Beats),
                     sum(income6$BrandAwareness_None),
                     nrow(income6),
                     sum(counts_income6 == "0"),
                     sum(counts_income6 == "1"),
                     sum(counts_income6 == "2"),
                     sum(counts_income6 == "3"),
                     sum(counts_income6 == "4"),
                     sum(counts_income6 == "5"),
                     sum(counts_income6 == "6"),
                     sum(counts_income6 == "7"),
                     sum(counts_income6 == "8"),
                     mean(counts_income6))

counts_income7 <- as.vector(rowSums(income7[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
income7_sums <- c(sum(income7$BrandAwareness_Anker), 
                     sum(income7$BrandAwareness_Bose),
                     sum(income7$BrandAwareness_JBL),
                     sum(income7$BrandAwareness_Philips),
                     sum(income7$BrandAwareness_Sony),
                     sum(income7$BrandAwareness_UE),
                     sum(income7$BrandAwareness_HarmanKardon),
                     sum(income7$BrandAwareness_Beats),
                     sum(income7$BrandAwareness_None),
                     nrow(income7),
                     sum(counts_income7 == "0"),
                     sum(counts_income7 == "1"),
                     sum(counts_income7 == "2"),
                     sum(counts_income7 == "3"),
                     sum(counts_income7 == "4"),
                     sum(counts_income7 == "5"),
                     sum(counts_income7 == "6"),
                     sum(counts_income7 == "7"),
                     sum(counts_income7 == "8"),
                     mean(counts_income7))

counts_income8 <- as.vector(rowSums(income8[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
income8_sums <- c(sum(income8$BrandAwareness_Anker), 
                     sum(income8$BrandAwareness_Bose),
                     sum(income8$BrandAwareness_JBL),
                     sum(income8$BrandAwareness_Philips),
                     sum(income8$BrandAwareness_Sony),
                     sum(income8$BrandAwareness_UE),
                     sum(income8$BrandAwareness_HarmanKardon),
                     sum(income8$BrandAwareness_Beats),
                     sum(income8$BrandAwareness_None),
                     nrow(income8),
                     sum(counts_income8 == "0"),
                     sum(counts_income8 == "1"),
                     sum(counts_income8 == "2"),
                     sum(counts_income8 == "3"),
                     sum(counts_income8 == "4"),
                     sum(counts_income8 == "5"),
                     sum(counts_income8 == "6"),
                     sum(counts_income8 == "7"),
                     sum(counts_income8 == "8"),
                     mean(counts_income8))

total_matrix <- rbind(general_sums,female_sums, male_sums, no_gender_sums ,
           employed_sums, retired_sums, selfemployed_sums, student_sums, unemployed_sums,
           income1_sums, income2_sums, income3_sums, income4_sums, income5_sums, income6_sums, income7_sums, income8_sums)
colnames(total_matrix) <- head
total_matrix

#write.csv(total_matrix, "file.csv")


### RELATIVE IMPORTANT FACTORS ANALYSIS - 0rki

# genel data da insanlar neyi onemsemis, en fazla degerler vs....
relImportanceEveryone <- cbind(summary(bluetooth$RelImp_battery), summary(bluetooth$RelImp_price),
                       summary(bluetooth$RelImp_weight), summary(bluetooth$RelImp_sound) )
colnames(relImportance) <- (cbind('Battery', 'Price', 'Weight', 'Sound'))
# sonra bu aynisini farkli yas-income-bilgi vs. gruplari icin yapilsin.

mean(x = sort(x = bluetooth$RelImp_weight)[-c(1, length(x = bluetooth$RelImp_weight))])

# TO-DO: otomatize edilebilir regex kullanilarak, kafam basmadi suan
# for (attribute in regexpr('RelImp_*', colnames(bluetooth))) {
#   hist(bluetooth$attribute, freq = FALSE)
#   lines(density(bluetooth$attribute))
# }

# Open a png file
png("RelativeImportanceDists.png")

par(mfrow=c(2, 2)) # aslinda 2ye 2lik bir grafik template i aciyor

hist(bluetooth$RelImp_battery, freq = FALSE, main = 'Battery' , xlab = 'Relative Importance')
lines(density(bluetooth$RelImp_battery))

hist(bluetooth$RelImp_price, freq = FALSE, main = 'Price', xlab = 'Relative Importance')
lines(density(bluetooth$RelImp_price))

hist(bluetooth$RelImp_weight, freq = FALSE, main = 'Weight', xlab = 'Relative Importance')
lines(density(bluetooth$RelImp_weight))

hist(bluetooth$RelImp_sound, freq = FALSE, main = 'Sound', xlab = 'Relative Importance')
lines(density(bluetooth$RelImp_sound))

mtext('Distribution of Relative Importances', outer = TRUE, cex = 1.5)

# Close the png file
dev.off() 

### ---- Other Descriptive Analytics (Kodlarimiz karismasin diye section actim buraya) ----

#code code