getwd()
setwd("~/Caci17/SWP3")
bluetooth <- read.csv("indivData.csv")

nrow(bluetooth)
summary(bluetooth)
str(bluetooth)


###---- Brand Awarenes ----

#bakalim kac kisi biliyormus
head <- c("Anker", "Bose", "JBL", "Philips", "Sony", "UE", "HarmanKardon", "Beats", "None", "# Observations")
general_sums <- c(sum(bluetooth$BrandAwareness_Anker), 
              sum(bluetooth$BrandAwareness_Bose),
              sum(bluetooth$BrandAwareness_JBL),
              sum(bluetooth$BrandAwareness_Philips),
              sum(bluetooth$BrandAwareness_Sony),
              sum(bluetooth$BrandAwareness_UE),
              sum(bluetooth$BrandAwareness_HarmanKardon),
              sum(bluetooth$BrandAwareness_Beats),
              sum(bluetooth$BrandAwareness_None),
              nrow(bluetooth))

#hicbirini bilmiyorum diyen ama yine de baska markayi biliyorum diye isaretleyen olmus mu- olmamis baktim.

#cinsiyete gore ayiralim
female <- bluetooth[bluetooth$GenderLabel == "female", ]
male <- bluetooth[bluetooth$GenderLabel == "male", ]
no_gender_info <- bluetooth[bluetooth$GenderLabel == "Prefer not to answer", ]
female_sums <- c(sum(female$BrandAwareness_Anker), 
                  sum(female$BrandAwareness_Bose),
                  sum(female$BrandAwareness_JBL),
                  sum(female$BrandAwareness_Philips),
                  sum(female$BrandAwareness_Sony),
                  sum(female$BrandAwareness_UE),
                  sum(female$BrandAwareness_HarmanKardon),
                  sum(female$BrandAwareness_Beats),
                  sum(female$BrandAwareness_None),
                  nrow(female))
male_sums <- c(sum(male$BrandAwareness_Anker), 
                  sum(male$BrandAwareness_Bose),
                  sum(male$BrandAwareness_JBL),
                  sum(male$BrandAwareness_Philips),
                  sum(male$BrandAwareness_Sony),
                  sum(male$BrandAwareness_UE),
                  sum(male$BrandAwareness_HarmanKardon),
                  sum(male$BrandAwareness_Beats),
                  sum(male$BrandAwareness_None),
                  nrow(male))
no_gender_sums <- c(sum(no_gender_info$BrandAwareness_Anker), 
                  sum(no_gender_info$BrandAwareness_Bose),
                  sum(no_gender_info$BrandAwareness_JBL),
                  sum(no_gender_info$BrandAwareness_Philips),
                  sum(no_gender_info$BrandAwareness_Sony),
                  sum(no_gender_info$BrandAwareness_UE),
                  sum(no_gender_info$BrandAwareness_HarmanKardon),
                  sum(no_gender_info$BrandAwareness_Beats),
                  sum(no_gender_info$BrandAwareness_None),
                  nrow(no_gender_info))

#by their occupation
levels(bluetooth$OccupationLabel)
employed <- bluetooth[bluetooth$OccupationLabel == "Employed",]
retired <- bluetooth[bluetooth$OccupationLabel == "Retired",]
selfemployed <- bluetooth[bluetooth$OccupationLabel == "Self-employed",]
student <- bluetooth[bluetooth$OccupationLabel == "Student",]
unemployed <- bluetooth[bluetooth$OccupationLabel == "Unemployed",]

employed_sums <- c(sum(employed$BrandAwareness_Anker), 
                 sum(employed$BrandAwareness_Bose),
                 sum(employed$BrandAwareness_JBL),
                 sum(employed$BrandAwareness_Philips),
                 sum(employed$BrandAwareness_Sony),
                 sum(employed$BrandAwareness_UE),
                 sum(employed$BrandAwareness_HarmanKardon),
                 sum(employed$BrandAwareness_Beats),
                 sum(employed$BrandAwareness_None),
                 nrow(employed))
retired_sums <- c(sum(retired$BrandAwareness_Anker), 
               sum(retired$BrandAwareness_Bose),
               sum(retired$BrandAwareness_JBL),
               sum(retired$BrandAwareness_Philips),
               sum(retired$BrandAwareness_Sony),
               sum(retired$BrandAwareness_UE),
               sum(retired$BrandAwareness_HarmanKardon),
               sum(retired$BrandAwareness_Beats),
               sum(retired$BrandAwareness_None),
               nrow(retired))
selfemployed_sums <- c(sum(selfemployed$BrandAwareness_Anker), 
                    sum(selfemployed$BrandAwareness_Bose),
                    sum(selfemployed$BrandAwareness_JBL),
                    sum(selfemployed$BrandAwareness_Philips),
                    sum(selfemployed$BrandAwareness_Sony),
                    sum(selfemployed$BrandAwareness_UE),
                    sum(selfemployed$BrandAwareness_HarmanKardon),
                    sum(selfemployed$BrandAwareness_Beats),
                    sum(selfemployed$BrandAwareness_None),
                    nrow(selfemployed))
student_sums <- c(sum(student$BrandAwareness_Anker), 
                      sum(student$BrandAwareness_Bose),
                      sum(student$BrandAwareness_JBL),
                      sum(student$BrandAwareness_Philips),
                      sum(student$BrandAwareness_Sony),
                      sum(student$BrandAwareness_UE),
                      sum(student$BrandAwareness_HarmanKardon),
                      sum(student$BrandAwareness_Beats),
                      sum(student$BrandAwareness_None),
                      nrow(student))
unemployed_sums <- c(sum(unemployed$BrandAwareness_Anker), 
                       sum(unemployed$BrandAwareness_Bose),
                       sum(unemployed$BrandAwareness_JBL),
                       sum(unemployed$BrandAwareness_Philips),
                       sum(unemployed$BrandAwareness_Sony),
                       sum(unemployed$BrandAwareness_UE),
                       sum(unemployed$BrandAwareness_HarmanKardon),
                       sum(unemployed$BrandAwareness_Beats),
                       sum(unemployed$BrandAwareness_None),
                       nrow(unemployed))

x <- rbind(general_sums,female_sums, male_sums, no_gender_sums ,employed_sums, retired_sums, selfemployed_sums, student_sums, unemployed_sums)
colnames(x) <- head
x
#female knows more than two
summed <- rowSums(female[, c(4, 5, 6, 7, 8, 9, 10, 11)])
table(summed)
