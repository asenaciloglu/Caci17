getwd()
setwd("~/Caci17/SWP3")

bluetooth <- read.csv("indivData.csv")


###---- Brand Awarenes ----

#bakalim kac kisi biliyormus
head <- c("Anker", "Bose", "JBL", "Philips", "Sony", "UE", "HarmanKardon", "Beats", "None", "Observations", "0", "1", "2", "3", "4", "5", "6", "7", "8")

counts_general <- as.vector(rowSums(bluetooth[, c(4, 5, 6, 7, 8, 9, 10, 11)]))
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
              sum(counts_general == "8")) #there is one person did not respond this question!

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
                  sum(counts_female == "8"))

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
                  sum(counts_male == "8"))

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
                  sum(counts_gender == "8"))
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
                 sum(counts_employed == "8"))

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
               sum(counts_retired == "8"))

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
                    sum(counts_selfemployed == "8"))

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
                      sum(counts_student == "8"))

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
                       sum(counts_unemployed == "8"))



x <- rbind(general_sums,female_sums, male_sums, no_gender_sums ,employed_sums, retired_sums, selfemployed_sums, student_sums, unemployed_sums)
colnames(x) <- head
x
