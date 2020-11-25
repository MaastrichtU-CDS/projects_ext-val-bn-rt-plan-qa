######################
#BN_RTplan data check#
######################

# read excel file
install.packages("readxl")
library("readxl")
Data <- read_excel('PATH/to/the/file/Data.xlsx')

# Evaluate variables and remove empty columns and create factor variables.
colnames(Data)
str(Data)
Data$Researchnumber <- as.factor(Data$Researchnumber)
Data$PBD <- as.factor(Data$PBD)
Data$AnatomicTumorLoc <- NULL
Data$Diagnose <- as.factor(Data$Diagnose)
Data$cT <- as.factor(Data$cT)
Data$cN <- as.factor(Data$cN)
Data$cM <- as.factor(Data$cM)
Data$TStage <- NULL
Data$NStage <- NULL
Data$MStage <- NULL
Data$TreatmentIntent <-as.factor(Data$TreatmentIntent)
Data$Indicatie <- as.factor(Data$Indicatie)
Data$`Zorgplan (Protocol)` <- as.factor(Data$`Zorgplan (Protocol)`)
Data$RxRadiationType <- as.factor(Data$RxRadiationType)
Data$MachineId <- as.factor(Data$MachineId)
Data$PlanTechnique <- as.factor(Data$PlanTechnique)
Data$Bolus <- as.factor(Data$Bolus)
Data$BeamEnergy <- as.factor(Data$BeamEnergy)
Data$Orientation <- as.factor(Data$Orientation)
Data$Tolerance <- as.factor(Data$Tolerance)
Data$NumberOfRxs <-as.factor(Data$NumberOfRxs)
Data$SetupDevice1 <- NULL
Data$SetupDevice2 <- NULL
Data$SetupDevice3 <- NULL
Data$SetupDevice4 <- NULL

# we have 2 variables which indicate treatmentintent, treatmentintent and indication, see how they related
library(ggplot2)
ggplot(Data, aes(x = TreatmentIntent, fill = factor(Indicatie))) +
  geom_bar() +
  xlab("TreatmentIntent") +
  ylab("Total Count") +
  labs(fill = "Indicatie") 
# treatmentintent does not seem correct, almost all patients are recorded as Curative, as they are not. Indication seems more correct (has a mix between palliative and radical(which is same as curative).
# we have another variable to verify, the Zorplan (protocol) variable, palliative procols (often) start with a P- P for palliative, 

# clear TreatmentIntent since it is wrong anyway
Data$TreatmentIntent <-NULL

#check columns again
colnames(Data)

#check Zorplan (protocol)
library(stringr)
Protocol <- Data[which(str_detect(Data$`Zorgplan (Protocol)`, "P-")),]
Protocol[1:5,]

# make a variable presence of "P-"
Data$Palliative <- str_detect(Data$`Zorgplan (Protocol)`, "P-")
Data$Palliative <- as.factor(Data$Palliative)

#check columns again
colnames(Data)

# compare created variable with indicatie variable from HIX
library(ggplot2)
ggplot(Data, aes(x = Palliative, fill = factor(Indicatie))) +
  geom_bar() +
  xlab("Palliative") +
  ylab("Total Count") +
  labs(fill = "Indicatie")




# the protocol based variable palliative is in agreement with indication, however solves a lot of NA in the indication variable.
# The string search makes a mistake for two protocolnames: R-M-LY-S-BP-5x7 AND R-M-LY-S-OP-3x10 which should have been radical. 
# Lets make a new variable based on indicatie and added with Palliative
Data$Treatment_Intent <- ifelse(Data$Indicatie == "Radicaal", "Radicaal", ifelse(Data$Indicatie == "Palliatief", "Palliatief", ifelse(Data$Indicatie == NA, Data$Palliative)))
str(Data)

#Delete indicatie
Data$Indicatie <- NULL
#Delete palliative
Data$Palliative <- NULL
#Check Collimator_Angle
Data$CollimatorAngle
#check CollimatorAngle
Data$CollimatorAngle <- as.double(Data$CollimatorAngle)
boxplot(Data$CollimatorAngle)

#check NumberOfRxs wrong all the values are 0
#check Dose per fraction 
Data$DosePerFraction
#check Total fractions
Data$TotalFractions


#check dose per fraction with total fractions
Data$PTVDoseCheck <- Data$DosePerFraction * Data$TotalFractions
Data$PTVDoseCheck

Data$PTVDoseCheck1 <- Data$PTVDoseCheck - Data$PTVDoseRx
Data$PTVDoseCheck1
Data$PTVDoseCheck1 <-NULL
Data$PTVDoseCheck <-NULL


#check RxRadiationType
Data$RxRadiationType
library(ggplot2)
ggplot(Data, aes(x = RxRadiationType)) +
  geom_bar() +
  xlab("RxRadiationType") +
  ylab("Total Count") 

#check PlanTechnique
Data$PlanTechnique
library(ggplot2)
ggplot(Data, aes(x = PlanTechnique)) +
  geom_bar() +
  xlab("PlanTechnique") +
  ylab("Total Count") 

#check TableAngle
Data$TableAngle
boxplot(Data$TableAngle)

#check NumberOfBeams
Data$NumberOfBeams
boxplot(Data$NumberOfBeams)

#check wedge
Data$Wedge
boxplot(Data$Wedge)

#check ControlPoints
Data$ControlPoints
boxplot(Data$ControlPoints)

#check SSD
Data$SSD
boxplot(Data$SSD)

#check GantryAngle
Data$GantryAngle
boxplot(Data$GantryAngle)



#check BeamEnergy
Data$BeamEnergy
Data$BeamEnergy <- as.double(Data$BeamEnergy)
boxplot(Data$BeamEnergy)

#check Orientation
Data$Orientation
Data$Orientation <- as.character(Data$Orientation)
library(ggplot2)
ggplot(Data, aes(x = Orientation)) +
  geom_bar() +
  xlab("Orientation") +
  ylab("Total Count") 

#check CouchLat
Data$CouchLat
boxplot(Data$CouchLat)

#check CouchLong
Data$CouchLong
boxplot(Data$CouchLong)

#check CouchVert
Data$CouchVert
boxplot(Data$CouchVert)

#check Tolerance
Data$Tolerance
library(ggplot2)
ggplot(Data, aes(x = Tolerance)) +
  geom_bar() +
  xlab("Tolerance") +
  ylab("Total Count") 


#Data check
str(Data)
#check missing values
sum(is.na(Data))

#check how many missing values we have
colSums(is.na(Data))

#check missing values in Treatment_Intent
sum(is.na(Data$Treatment_Intent))

#Print the dataset in a new excel called Data_clean
install.packages("openxlsx") 
library(openxlsx)

write.xlsx(Data, "PATH/to/the/file/Data_clean_new.xlsx")

#Now let's clean the NAs
complete.cases(Data)
na_vec <- which(complete.cases(Data))
na_vec
na_vec <- which(!complete.cases(Data))
Data_no_na <- Data[-na_vec,]
Data_no_na

#Print the dataset in a new excel without NA values called Data_clean
install.packages("openxlsx") 
library(openxlsx)

write.xlsx(Data_no_na, "PATH/to/the/file/Data_clean_no_na_new.xlsx")

