library(psych)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)

#Loading data
data = read.csv('/Users/maidamuhtar/Documents/Lakarprogrammet/Kurs 8/OCT_Aurora_Tegaderm_Data File ver.2_Paired t-test T vs noT.csv', sep=';')
datasites = read.csv('/Users/maidamuhtar/Documents/Lakarprogrammet/Kurs 8/OCT_Aurora_Tegaderm_Data File ver.5_Paired t-test sites.csv', sep=';')
datadiff = read.csv('/Users/maidamuhtar/Documents/Lakarprogrammet/Kurs 8/OCT_Aurora_Tegaderm_Data File ver.6_Differences.csv', sep=';')
datadiff = filter(datadiff, !is.na(Participant))

#Making certain variable values into factors 
data$Participant = factor(data$Participant, levels=c(1,2,3,4,5,6), labels=c('P001','P002','P003','P004','P005','P006'))
data$Site = factor(data$Site, levels=c(1,2,3), labels=c('Forearm','Hypothenar/thenar','Finger Pad'))
data$Tegaderm = factor(data$Tegaderm, levels=c(0,1), labels=c('No','Yes'))

datasites$Participant = factor(datasites$Participant, levels=c(1,2,3,4,5,6), labels=c('P001','P002','P003','P004','P005','P006'))
datasites$Site = factor(datasites$Site, levels=c(1,2,3), labels=c('Forearm','Hypothenar/thenar','Finger Pad'))

datadiff$Site = factor(datadiff$Site, levels=c(1,2,3), labels=c('Forearm','Hypothenar/thenar','Finger Pad'))
datadiffFA = (filter(datadiff, Site == 'Forearm')) 
datadiffHTR = (filter(datadiff, Site == 'Hypothenar/thenar'))
datadiffFP = (filter(datadiff, Site == 'Finger Pad'))

#Checking the data
describe(data)
data[1,]
plot(data$Tegaderm, data$Mx_Ind)

#Separating the sites by creating new variables to be used for paired t-tests
dataFA = (filter(data, Site == 'Forearm')) 
dataFA = arrange(dataFA, 'Participant','Site','Tegaderm','Observation_number')
ggplot(dataFA, aes(x=Tegaderm, y=Mx_Ind)) +
  geom_point()

dataHTR = (filter(data, Site == 'Hypothenar/thenar'))
dataHTR = arrange(dataHTR, 'Participant','Site','Tegaderm','Observation_number')
ggplot(dataHTR, aes(x=Tegaderm, y=Mx_Ind)) +
  geom_point()

dataFP = (filter(data, Site == 'Finger Pad'))
dataFP = arrange(dataFP, 'Participant','Site','Tegaderm','Observation_number')
ggplot(dataHTR, aes(x=Tegaderm, y=Mx_Ind)) +
  geom_point()

#Paired t-test to compare NoTegaderm and YesTegaderm for all variables separated by site. 
with(dataFA, t.test(Mx_Ind[Tegaderm == 'Yes'], Mx_Ind[Tegaderm == 'No'], paired = TRUE, var.equal = TRUE))

with(dataFA, t.test(X10pc_D[Tegaderm == 'Yes'], X10pc_D[Tegaderm == 'No'], paired = TRUE, var.equal = TRUE))

with(dataHTR, t.test(Mx_Ind[Tegaderm == 'Yes'], Mx_Ind[Tegaderm == 'No'], paired = TRUE, var.equal = TRUE))

with(dataHTR, t.test(X50pc_P[Tegaderm == 'Yes'], X50pc_P[Tegaderm == 'No'], paired = TRUE, var.equal = TRUE))

with(dataFP, t.test(Mx_Ind[Tegaderm == 'Yes'], Mx_Ind[Tegaderm == 'No'], paired = TRUE, var.equal = TRUE))

with(dataFP, t.test(X10pc_D[Tegaderm == 'Yes'], X10pc_D[Tegaderm == 'No'], paired = TRUE, var.equal = TRUE))



#Paired t-test to compare difference in Max_Ind between sites 
with(datasites, t.test(nteg.teg[Site == 'Forearm'], nteg.teg[Site == 'Hypothenar/thenar'], paired = TRUE, var.equal = TRUE))

with(datasites, t.test(nteg.teg[Site == 'Forearm'], nteg.teg[Site == 'Finger Pad'], paired = TRUE, var.equal = TRUE))

with(datasites, t.test(nteg.teg[Site == 'Hypothenar/thenar'], nteg.teg[Site == 'Finger Pad'], paired = TRUE, var.equal = TRUE))


# Random 
with(datadiff, t.test(Mx_IndDiff[Site == 'Forearm'], Mx_IndDiff[Site == 'Finger Pad'], paired = TRUE, var.equal = TRUE))
with(datadiff, t.test(X10pc_D[Site == 'Forearm'], X10pc_D[Site == 'Hypothenar/thenar'], paired = TRUE, var.equal = TRUE))
with(datadiff, t.test(X10pc_D[Site == 'Finger Pad'], X10pc_D[Site == 'Hypothenar/thenar'], paired = TRUE, var.equal = TRUE))
FAT <- filter(dataFA, Tegaderm == 'No')
shapiro.test(FAT$Mx_Ind)
describe(datadiffFA$Mx_IndDiff)



