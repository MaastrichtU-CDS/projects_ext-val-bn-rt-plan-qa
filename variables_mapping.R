##############################################
############BN_RTplan QA######################
##########Mapping values script###############
##############################################

# read excel file
install.packages("readxl")
library("readxl")
Data <- read_excel('C:/Users/pkale/Desktop/qart_new/Data.xlsx')

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

#check missing values
sum(is.na(Data))

#check how many missing values we have
colSums(is.na(Data))

#create a dataframe with the NAs total missing values of each column
na_count <-sapply(Data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)


#############################################
####   Rx_Radiation_Type ####################
#############################################
#create a new column indicating the radiation type
Data$BeamEnergyRadiationType = Data$RxRadiationType

#check the new variable
table(Data$BeamEnergyRadiationType)
class(Data$BeamEnergyRadiationType)

#change it to character
Data$BeamEnergyRadiationType <- as.character(as.factor(Data$BeamEnergyRadiationType))

#Let's now change the values of the dataframe according to the Luk's values
library(plyr)
revalue(Data$BeamEnergyRadiationType, c("0P" = "Protons")) -> Data$BeamEnergyRadiationType
revalue(Data$BeamEnergyRadiationType, c("10X" = "x10")) -> Data$BeamEnergyRadiationType
revalue(Data$BeamEnergyRadiationType, c("12E" = "e12")) -> Data$BeamEnergyRadiationType
revalue(Data$BeamEnergyRadiationType, c("15E" = "e15")) -> Data$BeamEnergyRadiationType
revalue(Data$BeamEnergyRadiationType, c("6E" = "e06")) -> Data$BeamEnergyRadiationType
revalue(Data$BeamEnergyRadiationType, c("6X" = "x06")) -> Data$BeamEnergyRadiationType

#remove proton therapy patients and electrons 9E
Data <- filter(Data,BeamEnergyRadiationType != "Protons")

#rename the column according to Luk
names(Data)[names(Data) == "BeamEnergyRadiationType"] <- "Rx_Radiation_Type"

#delete the column of radiation type we dont need
Data$RxRadiationType <- NULL

#############################################
####   Beam energy       ####################
#############################################

#let's check the variable Beam Energy
table(Data$BeamEnergy)
class(Data$BeamEnergy)
Data$BeamEnergy <- as.character(as.numeric(Data$BeamEnergy))

Data$BeamEnergy <- revalue(as.character(Data$BeamEnergy), c("6" = "15") )
Data$BeamEnergy <- revalue(as.character(Data$BeamEnergy), c("2" = "6"))                                                 
Data$BeamEnergy <- revalue(as.character(Data$BeamEnergy), c("4" = "10"))  
Data$BeamEnergy <- revalue(as.character(Data$BeamEnergy), c("5" = "12"))  

#change the column from character to numeric
Data$BeamEnergy <- as.numeric(as.character(Data$BeamEnergy))

#rename the column according to Luk
names(Data)[names(Data) == "BeamEnergy"] <- "Beam_Energy"

#############################################
####   Bolus ################################
#############################################

#check the bolus column now
table(Data$Bolus)
class(Data$Bolus)

#change the column from factor to charater
Data$Bolus <- as.character(as.factor(Data$Bolus))

#Let's now change the values of the dataframe according to the Luk's values
revalue(Data$Bolus, c("#MULTIVALUE" = "*custom")) -> Data$Bolus
revalue(Data$Bolus, c("Y" = "*custom")) -> Data$Bolus
revalue(Data$Bolus, c("N" = "NONE")) -> Data$Bolus


#############################################
#### Collimator angle #######################
#############################################

#check the collimator angle
table(Data$CollimatorAngle)
#round the values
Data$CollimatorAngle <- round(Data$CollimatorAngle) # Round off the column t

#check Luks data collimator angle
table(LukData$Collimator_Angle)
#check luks variable statistics
summary(LukData$Collimator_Angle)

#there are no values to exclude outside the min and max so we have to round the values to the nearest

#transform variables to characters for revalue
Data$CollimatorAngle  <- as.character(as.numeric(Data$CollimatorAngle ))

#revalue the values to the nearest luks value
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("39" = "40") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("48" = "49") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("53" = "54") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("59" = "60") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("62" = "61") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("64" = "65") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("66" = "67") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("69" = "70") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("109" = "110") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("119" = "120") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("124" = "125") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("137" = "138") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("143" = "145") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("144" = "145") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("147" = "148") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("152" = "153") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("159" = "160") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("161" = "162") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("163" = "165") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("201" = "202") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("218" = "220") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("228" = "230") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("251" = "250") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("269" = "270") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("274" = "275") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("289" = "290") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("292" = "293") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("294" = "295") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("299" = "300") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("319" = "320") )
Data$CollimatorAngle <- revalue(as.character(Data$CollimatorAngle), c("326" = "327") )

#transform again 
Data$CollimatorAngle  <- as.numeric(as.character(Data$CollimatorAngle ))
#change column name
names(Data)[names(Data) == "CollimatorAngle"] <- "Collimator_Angle"


#############################################
#### Control points##########################
#############################################

#check the variable
class(Data$ControlPoints)
table(Data$ControlPoints)

#check Luk's value
table(LukData$Control_Points)
summary(LukData$Control_Points)

#we have one value (0) outside the range so we should transform it to the minimum=1
#transform it to character
Data$ControlPoints  <- as.character(as.numeric(Data$ControlPoints ))

#round the value to the minimum
Data$ControlPoints <- revalue(as.character(Data$ControlPoints), c("0" = "1") )
Data$ControlPoints <- revalue(as.character(Data$ControlPoints), c("178" = "179") )

#change it to numeric values 
Data$ControlPoints  <- as.numeric(as.character(Data$ControlPoints ))

#rename
names(Data)[names(Data) == "ControlPoints"] <- "Control_Points"

#############################################
#### clinical T    ##########################
#############################################

#check variable
table(Data$cT)

#change it to character
Data$cT <- as.character(as.factor(Data$cT))

revalue(Data$cT, c("0" = "T0")) -> Data$cT
revalue(Data$cT, c("1" = "T1")) -> Data$cT
revalue(Data$cT, c("1a" = "T1a")) -> Data$cT
revalue(Data$cT, c("1a2" = "T1a2")) -> Data$cT
revalue(Data$cT, c("1b" = "T1b")) -> Data$cT
revalue(Data$cT, c("1b1" = "T1b1")) -> Data$cT
revalue(Data$cT, c("1b2" = "T1b2")) -> Data$cT
revalue(Data$cT, c("1c" = "T1c")) -> Data$cT
revalue(Data$cT, c("1mi" = "T1mi")) -> Data$cT
revalue(Data$cT, c("2" = "T2")) -> Data$cT
revalue(Data$cT, c("2a" = "T2a")) -> Data$cT
revalue(Data$cT, c("2b" = "T2b")) -> Data$cT
revalue(Data$cT, c("2c" = "T2c")) -> Data$cT
revalue(Data$cT, c("3" = "T3")) -> Data$cT
revalue(Data$cT, c("3a" = "T3a")) -> Data$cT
revalue(Data$cT, c("3b" = "T3b")) -> Data$cT
revalue(Data$cT, c("3c" = "T3NOS")) -> Data$cT
revalue(Data$cT, c("4" = "T4")) -> Data$cT
revalue(Data$cT, c("4a" = "T4a")) -> Data$cT
revalue(Data$cT, c("4b" = "T4b")) -> Data$cT
revalue(Data$cT, c("4d" = "T4d")) -> Data$cT
revalue(Data$cT, c("is" = "Tis(")) -> Data$cT
revalue(Data$cT, c("IS" = "Tis(")) -> Data$cT
revalue(Data$cT, c("is (dcis)" = "Tis(")) -> Data$cT
revalue(Data$cT, c("is (lcis)" = "Tis(")) -> Data$cT
revalue(Data$cT, c("is (Paget)" = "Tis(")) -> Data$cT
revalue(Data$cT, c("n.i." = "NULL")) -> Data$cT
revalue(Data$cT, c("X" = "TX")) -> Data$cT

#rename the column
names(Data)[names(Data) == "cT"] <- "T_stage"

#############################################
#### clinical n     ##############
#############################################

#check variable
table(Data$cN)
class(Data$cN)
#check Luk's variable
table(LukData$N_Stage)

#change it to character
Data$cN <- as.character(as.factor(Data$cN))

revalue(Data$cN, c("0" = "N0")) -> Data$cN
revalue(Data$cN, c("1" = "N1")) -> Data$cN
revalue(Data$cN, c("1a" = "N1a")) -> Data$cN
revalue(Data$cN, c("1b" = "N1b")) -> Data$cN
revalue(Data$cN, c("1mi" = "N1mi")) -> Data$cN
revalue(Data$cN, c("2" = "N2")) -> Data$cN
revalue(Data$cN, c("2a" = "N2a")) -> Data$cN
revalue(Data$cN, c("2b" = "N2b")) -> Data$cN
revalue(Data$cN, c("2c" = "N2c")) -> Data$cN
revalue(Data$cN, c("3" = "N3")) -> Data$cN
revalue(Data$cN, c("3a" = "N3a")) -> Data$cN
revalue(Data$cN, c("3b" = "N3b")) -> Data$cN
revalue(Data$cN, c("3c" = "N3c")) -> Data$cN
revalue(Data$cN, c("n.i." = "NULL")) -> Data$cN
revalue(Data$cN, c("X" = "NX")) -> Data$cN

#rename the column
names(Data)[names(Data) == "cN"] <- "N_stage"


#############################################
#### clinical m     #########################
#############################################

#check variable
table(Data$cM)
class(Data$cM)

#check Luk's variable
table(LukData$M_Stage)

#change it to character
Data$cM <- as.character(as.factor(Data$cM))

#change the values
revalue(Data$cM, c("0" = "M0")) -> Data$cM
revalue(Data$cM, c("1" = "M1")) -> Data$cM
revalue(Data$cM, c("1a" = "M1a")) -> Data$cM
revalue(Data$cM, c("1b" = "M1b")) -> Data$cM
revalue(Data$cM, c("1c" = "M1c")) -> Data$cM
revalue(Data$cM, c("n.i." = "NULL")) -> Data$cM
names(Data)[names(Data) == "cM"] <- "M_stage"


#############################################
####  Table Angle     #######################
#############################################

#check the variable
class(Data$TableAngle)
table(Data$TableAngle)
Data$TableAngle<- round(Data$TableAngle)

#check Luk's variable
table(LukData$Table_Angle)
summary(LukData$Table_Angle)

#change it to character
Data$TableAngle <- as.character(as.numeric(Data$TableAngle))


#change the value 359 to the maximum
revalue(Data$TableAngle, c("359" = "358")) -> Data$TableAngle
revalue(Data$TableAngle, c("61" = "62")) -> Data$TableAngle
revalue(Data$TableAngle, c("339" = "340")) -> Data$TableAngle

#change the variable to numeric
Data$TableAngle <- as.numeric(as.character(Data$TableAngle))

names(Data)[names(Data) == "TableAngle"] <- "Table_Angle"



#############################################
####  SSD     ###############################
#############################################

#check variable
table(Data$SSD)
Data$SSD<- round(Data$SSD)

Data$SSD <- as.numeric(as.character(Data$SSD ))
Data$SSD <- as.character(as.numeric(Data$SSD ))

summary(LukData$SSD)
require(plyr)
require(dplyr)

Data$SSD <- revalue(as.character(Data$SSD), c("112" = "115") )
Data$SSD <- revalue(as.character(Data$SSD), c("349" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("351" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("352" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("353" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("354" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("355" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("356" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("357" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("358" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("359" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("360" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("361" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("362" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("363" = "350") )
Data$SSD <- revalue(as.character(Data$SSD), c("365" = "350") )

Data$SSD <- as.numeric(as.character(Data$SSD ))





#############################################
####   Gantry Angle #########################
#############################################
table(Data$GantryAngle)
Data$GantryAngle<- round(Data$GantryAngle)

summary(LukData$Gantry_Angle)
Data$GantryAngle <- as.character(as.numeric(Data$GantryAngle))

Data$GantryAngle <- revalue(as.character(Data$GantryAngle), c("99" = "100") )
Data$GantryAngle <- revalue(as.character(Data$GantryAngle), c("101" = "102") )
Data$GantryAngle <- revalue(as.character(Data$GantryAngle), c("181" = "182") )
Data$GantryAngle <- revalue(as.character(Data$GantryAngle), c("189" = "190") )
Data$GantryAngle <- revalue(as.character(Data$GantryAngle), c("246" = "247") )
Data$GantryAngle <- revalue(as.character(Data$GantryAngle), c("251" = "252") )
Data$GantryAngle <- revalue(as.character(Data$GantryAngle), c("259" = "260") )
Data$GantryAngle <- revalue(as.character(Data$GantryAngle), c("261" = "262") )
Data$GantryAngle <- revalue(as.character(Data$GantryAngle), c("341" = "342") )

Data$GantryAngle <- as.numeric(as.character(Data$GantryAngle))

names(Data)[names(Data) == "GantryAngle"] <- "Gantry_Angle"


#############################################
#### wedges        ##############
#############################################
table(Data$Wedge)


#############################################
####  tolerance          ####################
#############################################
table(Data$Tolerance)

Data$Tolerance_Table <- ifelse(Data$Rx_Radiation_Type =="x10","!Photon",
                        ifelse(Data$Rx_Radiation_Type =="x06","!Photon" , 
                        ifelse(Data$Rx_Radiation_Type =="e12","!Electron",
                        ifelse(Data$Rx_Radiation_Type =="e15","!Electron",
                        ifelse(Data$Rx_Radiation_Type =="e06","!Electron")))))
Data$Tolerance_Table <- as.character(as.factor(Data$Tolerance_Table))

#############################################
####  treatment intent ######################
#############################################
table(Data$Treatment_Intent)
class(Data$Treatment_Intent)
revalue(Data$Treatment_Intent, c("Palliatief" = "Palliative(other)")) -> Data$Treatment_Intent
revalue(Data$Treatment_Intent, c("Radicaal" = "Curative(adjuvant)")) -> Data$Treatment_Intent



#############################################
#### orientation     ########################
#############################################
table(Data$Orientation)
table(LukData$Orientation)
revalue(Data$Orientation, c("Head First-Supine" = "HeadIn,Supine")) -> Data$Orientation
revalue(Data$Orientation, c("Head First-Decubitus Right" = "HeadIn,RecumbentRight")) -> Data$Orientation

#############################################
#### plan technique     ##############
#############################################
table(Data$PlanTechnique)
revalue(Data$PlanTechnique, c("ARC" = "VMAT")) -> Data$PlanTechnique
revalue(Data$PlanTechnique, c("STATIC" = "IMRT")) -> Data$PlanTechnique
revalue(Data$PlanTechnique, c("MODULAT_SCANNING" = "IMRT")) -> Data$PlanTechnique
names(Data)[names(Data) == "PlanTechnique"] <- "Plan_Technique"

#############################################
####  Total Fractions    ####################
#############################################
table(Data$TotalFractions)
Data$TotalFractions <- as.character(as.numeric(Data$TotalFractions))
revalue(Data$TotalFractions, c("34" = "35")) -> Data$TotalFractions
Data$TotalFractions <- as.numeric(as.character(Data$TotalFractions))
names(Data)[names(Data) == "TotalFractions"] <- "Total_Fractions"


#############################################
####  Dose per fraction    ##################
#############################################
table(Data$DosePerFraction)
Data$DosePerFraction <- Data$DosePerFraction *100 #transform it to cGy
Data$DosePerFraction<- round(Data$DosePerFraction)

Data$DosePerFraction <- as.character(as.numeric(Data$DosePerFraction))
revalue(Data$DosePerFraction, c("203" = "205")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("255" = "257")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("312.5" = "320")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("312" = "320")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("367" = "370")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("440" = "450")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("520" = "550")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("540" = "550")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("570" = "578")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("725" = "720")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("1400" = "1500")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("2100" = "2000")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("2400" = "2000")) -> Data$DosePerFraction
revalue(Data$DosePerFraction, c("2600" = "2000")) -> Data$DosePerFraction

Data$DosePerFraction <- as.numeric(as.character(Data$DosePerFraction))
names(Data)[names(Data) == "DosePerFraction"] <- "Dose_Per_Fraction"

#############################################
####  PTV DOSE RX           #################
#############################################
table(Data$PTVDoseRx)
Data$PTVDoseRx <-Data$PTVDoseRx *100 #transform it to cGy
Data$PTVDoseRx <- as.character(as.numeric(Data$PTVDoseRx))

revalue(Data$PTVDoseRx, c("235" = "232")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("267" = "266")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("534" = "532")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("801" = "800")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("850" = "848")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("1068" = "1071")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("1140" = "1150")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("1335" = "1350")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("1560" = "1548")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("1602" = "1600")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("1710" = "1700")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("1869" = "1840")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("1880" = "1900")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("1925" = "1900")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("2080" = "2100")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("2136" = "2134")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("2350" = "2340")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("2403" = "2400")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("2568" = "2500")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("2585" = "2600")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("2670" = "2650")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("2937" = "2900")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("3025" = "3000")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("3204" = "3200")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("3360" = "3375")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("3471" = "3500")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("3525" = "3500")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("3625" = "3604")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("3738" = "3750")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("3760" = "3750")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("4230" = "4240")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("4465" = "4500")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("4539" = "4522")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("4669" = "4680")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("4675" = "4680")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("4806" = "4800")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5073" = "5088")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5170" = "5200")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5225" = "5250")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5280" = "5250")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5340" = "5320")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5505" = "5500")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5607" = "5600")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5624" = "5625")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5640" = "5624")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5775" = "5780")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5850" = "5805")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5874" = "5805")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5875" = "5805")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("5992" = "6000")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6050" = "6020")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6110" = "6216")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6141" = "6216")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6160" = "6216")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6200" = "6216")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6210" = "6216")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6345" = "6360")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6440" = "6450")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6580" = "6572")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6720" = "6750")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("6800" = "6840")) -> Data$PTVDoseRx
revalue(Data$PTVDoseRx, c("7700" = "7740")) -> Data$PTVDoseRx

Data$PTVDoseRx <- as.numeric(as.character(Data$PTVDoseRx))
names(Data)[names(Data) == "PTVDoseRx"] <- "PTV_Dose_Rx"



#############################################
####  Number of beams       #################
#############################################
table(Data$NumberOfBeams)
table(LukData$Number_of_beams)

names(Data)[names(Data) == "NumberOfBeams"] <- "Number_of_beams"

table(Data$Number_of_beams)

#############################################
####   Couch Lat ############################
#############################################
table(Data$CouchLat)
Data$CouchLat <- round(Data$CouchLat)
summary(LukData$Couch_Lat)
summary(Data$CouchLat)
names(Data)[names(Data) == "CouchLat"] <- "Couch_Lat"


#############################################
####   Couch Long ###########################
#############################################
table(Data$CouchLong)
Data$CouchLong <- round(Data$CouchLong)
summary(Data$CouchLong)
summary(LukData$Couch_Long)

Data$CouchLong <- as.character(as.numeric(Data$CouchLong))

#max 100 in luk values so these values have to be deleted
revalue(Data$CouchLong, c("101" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("102" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("103" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("104" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("105" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("106" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("107" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("108" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("109" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("110" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("111" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("112" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("113" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("114" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("115" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("116" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("117" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("118" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("119" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("120" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("121" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("122" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("123" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("124" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("125" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("126" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("127" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("128" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("129" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("130" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("131" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("132" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("133" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("134" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("135" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("136" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("137" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("138" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("139" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("140" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("141" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("142" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("143" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("144" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("145" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("146" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("147" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("148" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("149" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("150" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("151" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("152" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("153" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("154" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("155" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("156" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("157" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("158" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("159" = "100")) -> Data$CouchLong
revalue(Data$CouchLong, c("160" = "100")) -> Data$CouchLong
Data$CouchLong <- as.numeric(as.character(Data$CouchLong))


names(Data)[names(Data) == "CouchLong"] <- "Couch_Long"


#############################################
####   Couch Vert ###########################
#############################################
table(Data$CouchVert)
Data$CouchVert <- round(Data$CouchVert)
summary(Data$CouchVert)
summary(LukData$Couch_Vert)
Data$CouchVert <- as.character(as.numeric(Data$CouchVert))

revalue(Data$CouchVert, c("-27" = "-25")) -> Data$CouchVert
revalue(Data$CouchVert, c("-26" = "-25")) -> Data$CouchVert
revalue(Data$CouchVert, c("-23" = "-24")) -> Data$CouchVert
revalue(Data$CouchVert, c("-22" = "-21")) -> Data$CouchVert
revalue(Data$CouchVert, c("-66" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-65" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-64" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-63" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-62" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-61" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-60" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-59" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-58" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-57" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-56" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-55" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-54" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-53" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-52" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-51" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-44" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-43" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-42" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-41" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-40" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-38" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-37" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-36" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-34" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-33" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-32" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-31" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-29" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-28" = "-35")) -> Data$CouchVert
revalue(Data$CouchVert, c("-4" = "-3")) -> Data$CouchVert


Data$CouchVert <- as.numeric(as.character(Data$CouchVert))


names(Data)[names(Data) == "CouchVert"] <- "Couch_Vert"



#############################################
#### anatomic tumour location   ##############
#############################################
table(Data$Diagnose)
Data$Diagnose <- as.character(as.factor(Data$Diagnose))
class(Data$Diagnose)
#revalue the variable
revalue(Data$Diagnose, c("malignant neoplasm of lung" = "LUNG")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant melanoma of ear" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of esophagus" = "ESOPHAGUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of breast" = "BREASTFEMALE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of prostate" = "PROSTATEGLAND")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of tongue" = "TONGUE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of skin of face" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of parotid gland" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of mouth" = "MOUTHFLOOR")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of skin" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of larynx" = "LARYNX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of oropharynx" = "OROPHARYNX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of nasal cavity" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of bladder" = "BLADDER")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of brains" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of thyroid" = "THYROIDGLAND")) -> Data$Diagnose
revalue(Data$Diagnose, c("metastasis" = "METASTASIS/")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of hypopharynx" = "HYPOPHARYNX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of floor of mouth" = "MOUTHFLOOR")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of supraglottis" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of base of tongue" = "TONGUE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of nasopharynx" = "NASOPHARYNX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of tonsil" = "TONSIL")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of kidney" = "KIDNEY")) -> Data$Diagnose
revalue(Data$Diagnose, c("diffuse large cell non-Hodgkin's lymphoma" = "LYMPHOMA(NHL)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of posterior wall of the oropharynx" = "OROPHARYNX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of skin of upper limb" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("follicular non-Hodgkin's lymphoma" = "LYMPHOMA(NHL)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasms of submandibular" = "SALIVARYGLAND")) -> Data$Diagnose
revalue(Data$Diagnose, c("extranodal marginal zone B-cell lymphoma of mucosa-associated lymphoid tissue" = "LYMPHOMA(NHL)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of vallecula epiglottica" = "OROPHARYNX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant melanoma of the face" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of skin of neck" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of maxillary sinus" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("lymphomatoid palpulose" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of larynx" = "LARYNX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of upper gum" = "MOUTHFLOOR")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissues of the head and neck" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("plasmocytoom solitary bone" = "BONE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of major salivary glands" = "SALIVARYGLAND")) -> Data$Diagnose
revalue(Data$Diagnose, c("prostate" = "PROSTATEGLAND")) -> Data$Diagnose
revalue(Data$Diagnose, c("tongue base" = "TONGUE")) -> Data$Diagnose
revalue(Data$Diagnose, c("kidney" = "KIDNEY")) -> Data$Diagnose
revalue(Data$Diagnose, c("rectum" = "RECTUM")) -> Data$Diagnose
revalue(Data$Diagnose, c("thymus, anterior mediastinum" = "THORAX")) -> Data$Diagnose
revalue(Data$Diagnose, c("lung, bronchus" = "LUNG")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of rectum" = "RECTUM")) -> Data$Diagnose
revalue(Data$Diagnose, c("lung, upper lobe" = "LUNG")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissue" = "SOFTTISSUES")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of sigmoid colon" = "COLON")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of ascending colon" = "COLON")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of pancreas" = "PANCREAS")) -> Data$Diagnose
revalue(Data$Diagnose, c("meningioma" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("myelosarcoom" = "BONE")) -> Data$Diagnose
revalue(Data$Diagnose, c("hepatocellular carcinoma" = "LIVER")) -> Data$Diagnose
revalue(Data$Diagnose, c("non-Hodgkin's lymphoma" = "LYMPHOMA(NHL)")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of brains" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of appendix" = "INTESTINALTRACT")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant melanoma of upper limb" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("neurinoma" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of rectum in transition from sigmoid" = "RECTUM")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of stomach" = "STOMACH")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of breast" = "BREASTFEMALE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant melanoma of neck" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("glioblastoma" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of olfactory nerve" = "NASOPHARYNX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of thymus" = "THORAX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of vulva" = "URINARYSYSTEM")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of opinions" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("intraductal carcinoma in situ of breast" = "BREASTFEMALE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of uterus" = "UTERUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant melanoma of lower limb" = "LIMB")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant melanoma of skin" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of esophagus" = "ESOPHAGUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissues of pelvic" = "PELVIS")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of ovary" = "OVARY")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of endocervix" = "CERVIX")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of vagina" = "URINARYSYSTEM")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of fallopian tube" = "URINARYSYSTEM")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of colon" = "COLON")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of skin of lower limb" = "LIMB")) -> Data$Diagnose
revalue(Data$Diagnose, c("T-cell lymphoma" = "LYMPHOMA(NHL)")) -> Data$Diagnose
revalue(Data$Diagnose, c("non-small cell lung cancer" = "LUNG")) -> Data$Diagnose
revalue(Data$Diagnose, c("primary cutaneous anaplastic large T-cell lymphoma, CD30 positive" = "LYMPHOMA(NHL)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of renal pelvis" = "PELVIS")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of anus" = "ANUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of sinus ethmoidalis" = "SINUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("primarily diffuse large B-cell lymphoma by central nervous system" = "NERVOUSSYSTEM")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of testis" = "TESTIS")) -> Data$Diagnose
revalue(Data$Diagnose, c("lobular carcinoma in situ of breast" = "BREASTFEMALE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of duodenum" = "PANCREAS")) -> Data$Diagnose
revalue(Data$Diagnose, c("arthrosis" = "SOFTTISSUES")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of descending colon" = "COLON")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissues of lower limb" = "LIMB")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of pituitary" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Epilepsy: Epilepsy surgery and cortical motor stimulation @DBC" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of gastric fundus" = "STOMACH")) -> Data$Diagnose
revalue(Data$Diagnose, c("diagnostics still underway @DBC" = "UNKNOWNPRIMARY")) -> Data$Diagnose
revalue(Data$Diagnose, c("Pleural Mesothelioma" = "PLEURA")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of gallbladder" = "BLADDER")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of sinus" = "SINUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of cranial nerve" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of small intestine" = "SMALLINTESTINE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of transverse colon" = "COLON")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of eye" = "EYE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of biliary" = "LIVER")) -> Data$Diagnose
revalue(Data$Diagnose, c("nodular lymphocyte-rich Hodgkin's lymphoma" = "HODGKINS")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of exocervix" = "CERVIX")) -> Data$Diagnose
revalue(Data$Diagnose, c("low-grade glioma of brains" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("intrahepatic bile duct carcinoma" = "LIVER")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of cerebral opinions" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of cecum" = "ABDOMEN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of eyelid" = "EYE")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of liver" = "LIVER")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of body of pancreas" = "PANCREAS")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissue of upper limb" = "LIMB")) -> Data$Diagnose
revalue(Data$Diagnose, c("diffuse non-Hodgkin's lymphoma" = "LYMPHOMA(NHL)")) -> Data$Diagnose
revalue(Data$Diagnose, c("diffuse small cell non-Hodgkin's lymphoma" = "LYMPHOMA(NHL)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Primary malignant neoplasm of cerebrum" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of bone" = "BONE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Acute myeloid leukemia FLT3 mutation" = "LEUKEMIANOS")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of glans penis" = "PENIS")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissue of the abdomen" = "ABDOMEN")) -> Data$Diagnose
revalue(Data$Diagnose, c("heterotopic ossification" = "BONE")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of the bladder" = "BLADDER")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of brains, supratentorial" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("oligodendroglioma" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("keloid scar" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Ewing's sarcoma of bone" = "BONE")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of skin of trunk" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissues of the back" = "SOFTTISSUES")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of endocervix" = "CERVIX")) -> Data$Diagnose
revalue(Data$Diagnose, c("impairment of saliva" = "SALIVARYGLAND")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of middle ear" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("kaposi's sarcoma of soft tissue" = "SOFTTISSUES")) -> Data$Diagnose
revalue(Data$Diagnose, c("Other and unspecified gastroenteritis of infectious origin" = "STOMACH")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of ear" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("urothelial cell carcinoma of kidney" = "KIDNEY")) -> Data$Diagnose
revalue(Data$Diagnose, c("Carcinoma with osteoclast-like giant cells" = "PANCREAS")) -> Data$Diagnose
revalue(Data$Diagnose, c("chronic myelomonocytic leukemia" = "LEUKEMIANOS")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of cauda of pancreas" = "PANCREAS")) -> Data$Diagnose
revalue(Data$Diagnose, c("Kaposi's sarcoma" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of major duodenal papilla" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("plasmacelmyeloom" = "HEMATOPOIETIC")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of ductus craniopharyngeus" = "HEAD/FACE/NECK")) -> Data$Diagnose
revalue(Data$Diagnose, c("invasive malignant neoplasm of bladder" = "BLADDER")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom NNE" = "BREASTFEMALE")) -> Data$Diagnose
revalue(Data$Diagnose, c("corpus uteri" = "UTERUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("brains, temporal lobe" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom, medial upper quadrant" = "BREASTFEMALE")) -> Data$Diagnose
revalue(Data$Diagnose, c("bronchus / lung NNE" = "LUNG")) -> Data$Diagnose
revalue(Data$Diagnose, c("esophagus, thoracic" = "ESOPHAGUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("lung, lower lobe" = "LUNG")) -> Data$Diagnose
revalue(Data$Diagnose, c("Other var. (Eg.prim.cutaan CD30 grootcell.Tcel.lymf)" = "UNKNOWNPRIMARY")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom, central" = "BREASTFEMALE")) -> Data$Diagnose
revalue(Data$Diagnose, c("thymus" = "THORAX")) -> Data$Diagnose
revalue(Data$Diagnose, c("uterus NNE" = "UTERUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant bone marrow tumor NNE" = "BONE")) -> Data$Diagnose
revalue(Data$Diagnose, c("metastases with unknown primary tumor" = "METASTASIS/")) -> Data$Diagnose
revalue(Data$Diagnose, c("plasmocytoom NNE" = "HEMATOPOIETIC")) -> Data$Diagnose
revalue(Data$Diagnose, c("skin, lower extremities" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("esophagus, abdominal part" = "ESOPHAGUS")) -> Data$Diagnose
revalue(Data$Diagnose, c("cervix uteri NNE" = "CERVIX")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom, medial quadrant" = "BREASTFEMALE")) -> Data$Diagnose
revalue(Data$Diagnose, c("Hodgkin lymphoma (lymphocyte rich form)" = "HODGKINS")) -> Data$Diagnose
revalue(Data$Diagnose, c("ovary" = "OVARY")) -> Data$Diagnose
revalue(Data$Diagnose, c("soft tissue, lower limb / Hip" = "LIMB")) -> Data$Diagnose
revalue(Data$Diagnose, c("Other localizations brains" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("brains, parietal lobe" = "BRAIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("colon, sigmoid" = "COLON")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom, axillary spur" = "BREASTFEMALE")) -> Data$Diagnose
revalue(Data$Diagnose, c("primary cutaneous diffuse large B-cell lymphoma of bone" = "BONE")) -> Data$Diagnose
revalue(Data$Diagnose, c("transition rectum / sigmoid" = "RECTUM")) -> Data$Diagnose
revalue(Data$Diagnose, c("melanoma of the skin, skull / neck / nape" = "SKIN")) -> Data$Diagnose
revalue(Data$Diagnose, c("reticulosarcoma" = "BONE")) -> Data$Diagnose
revalue(Data$Diagnose, c("paranasal sinuses, frontal sinus" = "NASOPHARYNX")) -> Data$Diagnose

#rename the column
names(Data)[names(Data) == "Diagnose"] <- "Anatomic_tumor_loc"


#Number of Rxs
table(Data$NumberOfRxs)
counts<-as.data.frame(table(Data$PBD))
Data$rxs_valuecount <- counts$Freq[match(Data$PBD, counts$Var1)]


#Take the final df for the variables
resultDF <- Data[rowSums(is.na(Data[ , 0:ncol(Data)])) < ncol(Data), ]



#export is as a csv

write.csv(resultDF, "C:/Users/pkale/Desktop/2ndapproachfinalCSV12611RXS.csv", row.names = FALSE)

#check the colnames of the df
colnames(resultDF)




