##  Title: A longitudinal dataset of hypertensive osteoporotic fracture patients: 
##   treatments and long-term outcomes.
##
##  The R scripts used for data preprocessing, statistical analysis, 
##   and figure generation in this study were developed using R version 4.3.3.
##
##  The script begins by loading essential libraries for data manipulation (dplyr, reshape2), 
##   statistical analysis (survival, irr, vcd, pROC), and visualization (ggplot2, ggpubr, corrplot). 
##   It reads two core files: "Clinical Variables Table.xlsx", which contains baseline demographic, 
##   clinical, and outcome data, and "Medication Table.xlsx", which includes longitudinal 
##   antihypertensive prescription records.
##
##  All results reported in the manuscript are fully reproducible using the provided code and dataset.


## Replace "E:\\Your\\Data\\Path" with your data storage path.
setwd("E:\\Your\\Data\\Path")

library(readxl)
library(irr)
library(vcd)
library(dplyr)
library(ggplot2)
library(gmodels)
library(pROC)
library(ggpubr)
library(corrplot)
library(reshape2)
library(survival)
library(survminer)
library(mice)
library(MatchIt)
raw_data <- read_xlsx("Clinical Variables Table.xlsx", col_names = T)
drug_data <- read_xlsx("Medication Table V2.0.xlsx", col_names = T)


## Data preprocessing: selection of baseline demographic,
## lifestyle, and primary diagnosis variables for analysis
raw_data <- raw_data[!(raw_data$"Survivaltime" < 30), ]
ID <- as.numeric(unlist(raw_data[,"ID"]))
gender <- as.numeric(unlist(raw_data[,"Gender"]))
age <- as.numeric(unlist(raw_data[,"Age"]))
BMI <- as.numeric(unlist(raw_data[,"BMI"]))
smoke <- as.numeric(unlist(raw_data[,"Smoker"]))
drink <- as.numeric(unlist(raw_data[,"Drink"]))
maindiagnosis <- as.factor(as.vector(unlist(raw_data[,"Maindiagnosis"])))
maindiagnosis  <- relevel(maindiagnosis, ref = "52")  

##  Hypertension-related variables
hyper_report          <- as.numeric(unlist(raw_data[,"hypertension"])) # Hypertension status as recorded in medical records (self-reported history)
SBP          <- as.numeric(unlist(raw_data[,"SBP"]))  # Measured systolic blood pressure
DBP          <- as.numeric(unlist(raw_data[,"DBP"]))  # Measured diastolic blood pressure
hypertension1 <- ifelse( SBP > 135 & DBP > 80 ,1,0) 
hypertension2 <- ifelse( SBP < 135  &  hyper_report==1,1,0)
hypertension3 <- ifelse( SBP > 135  &  hyper_report==1,1,0)

##  Complication
diabetes     <- as.numeric(unlist(raw_data[,"diabetes"]))
heart        <- as.numeric(unlist(raw_data[,"heart"]))
OP           <- as.numeric(unlist(raw_data[,"OP"]))
lung         <- as.numeric(unlist(raw_data[,"lung"]))
liver        <- as.numeric(unlist(raw_data[,"liver"]))
kidney       <- as.numeric(unlist(raw_data[,"kidney"]))
brain        <- as.numeric(unlist(raw_data[,"brain"]))
PD           <- as.numeric(unlist(raw_data[,"PD"]))
AD           <- as.numeric(unlist(raw_data[,"AD"]))
DVT          <- as.numeric(unlist(raw_data[,"DVT"]))
infection    <- as.numeric(unlist(raw_data[,"infection"]))
tumor        <- as.numeric(unlist(raw_data[,"tumor"]))
shock        <- as.numeric(unlist(raw_data[,"shock"]))


## Charlson Comorbidity Index (CCI)
## Comorbidities included:
## myocardial infarction, congestive heart failure,
## peripheral vascular disease, dementia, cerebrovascular disease,
## connective tissue disease, peptic ulcer disease, mild liver disease,
## diabetes (type 2), chronic lung disease, hemiplegia,
## chronic kidney disease, solid tumor, leukemia, lymphoma,
## moderate to severe liver disease, malignant tumors, and AIDS
infarction <- as.numeric(unlist(raw_data[,"Myocardial infarction"]))
heartfailure  <- as.numeric(unlist(raw_data[,"Peripheralvasculardisease"]))
vascular <- as.numeric(unlist(raw_data[,"Peripheralvasculardisease"]))
dementia <- as.numeric(unlist(raw_data[,"dementia"]))
Cerebro <- as.numeric(unlist(raw_data[,"Cerebrovasculardiseases"]))
Connective <- as.numeric(unlist(raw_data[,"Connectivetissuediseases"]))
Peptic <- as.numeric(unlist(raw_data[,"Pepticulcerdisease"]))
Mildliver <- as.numeric(unlist(raw_data[,"Mildliver disease"]))
T1D <- as.numeric(unlist(raw_data[,"1Diabetes"]))
Chronic_l <- as.numeric(unlist(raw_data[,"Chronic lung disease"]))
Hemiplegia <- as.numeric(unlist(raw_data[,"Hemiplegia"]))
CKD <- as.numeric(unlist(raw_data[,"CKD"]))
T2D <- as.numeric(unlist(raw_data[,"2Diabetes"]))
Solid <- as.numeric(unlist(raw_data[,"2Solidtumor"]))
leukemia  <- as.numeric(unlist(raw_data[,"2leukemia"]))
lymphoma <- as.numeric(unlist(raw_data[,"2Malignant lymphoma"]))
severeliver <- as.numeric(unlist(raw_data[,"3Moderate to severe liver disease"]))
Malignant <- as.numeric(unlist(raw_data[,"6Malignant tumors"]))
AIDS <- as.numeric(unlist(raw_data[,"6AIDS"]))
CCI <- as.numeric(unlist(raw_data[,"CCI"]))
CCI[CCI >= 3] <- 3      # Truncate CCI score at ≥3 and convert to categorical variable
CCI <- as.factor(CCI)
CCI  <- relevel(CCI, ref = "0")  


## Medication exposure variables were coded as binary indicators, 
## defined as any recorded use (≥1) during the observation period.
CA  <- as.numeric(unlist(raw_data[,"CA"])  >= 1)
VD1 <- as.numeric(unlist(raw_data[,"VD1"]) >= 1)
VD2 <- as.numeric(unlist(raw_data[,"VD2"]) >= 1)
VD3 <- as.numeric(unlist(raw_data[,"VD3"]) >= 1)
ZOL <- as.numeric(unlist(raw_data[,"ZOL"]) >= 1)
CAL <- as.numeric(unlist(raw_data[,"CAL"]) >= 1)
ALN <- as.numeric(unlist(raw_data[,"ALN"]) >= 1)


## Laboratory measurements
## extraction and numeric conversion of baseline biochemical 
## and bone metabolism–related laboratory variables
potassium <- as.numeric(unlist(raw_data[,"potassium"]))
magnesium <- as.numeric(unlist(raw_data[,"magnesium"]))
sodium    <- as.numeric(unlist(raw_data[,"sodium"]))
phosphorus<- as.numeric(unlist(raw_data[,"phosphorus"]))
calcium   <- as.numeric(unlist(raw_data[,"calcium"]))
hemoglobin<- as.numeric(unlist(raw_data[,"hemoglobin"]))
# Bone turnover markers
BTX       <- as.numeric(unlist(raw_data[,"BTX"]))
P1NP      <- as.numeric(unlist(raw_data[,"P1NP"]))
osteocalcin <- as.numeric(unlist(raw_data[,"osteocalcin"]))
# Iron metabolism and lipid profile
ferritin  <- as.numeric(unlist(raw_data[,"ferritin"]))
TC        <- as.numeric(unlist(raw_data[,"Total cholesterol"]))
triglycerides   <- as.numeric(unlist(raw_data[,"triglycerides"]))
HDL       <- as.numeric(unlist(raw_data[,"HDL"]))
LDL       <- as.numeric(unlist(raw_data[,"LDL"]))
# Hormonal and inflammatory markers
PTH       <- as.numeric(unlist(raw_data[,"PTH"]))
V25D       <- as.numeric(unlist(raw_data[,"25VD"]))
CRP       <- as.numeric(unlist(raw_data[,"CRP"]))
# Liver and renal function markers
ALT       <- as.numeric(unlist(raw_data[,"ALT"]))
CR        <- as.numeric(unlist(raw_data[,"CR"]))
UN        <- as.numeric(unlist(raw_data[,"UN"]))
UA        <- as.numeric(unlist(raw_data[,"UA"]))
# Perioperative risk assessment
ASA       <- as.numeric(unlist(raw_data[,"ASA"]))


## Outcome variables：All-cause mortality and refracture
survivaltime   <- round(as.numeric(unlist(raw_data[,"Survivaltime"]))/30,1)
death <- as.numeric(unlist(raw_data[,"Death_all"]))

refracturetime   <- round(as.numeric(unlist(raw_data[,"Refracturetime"]))/30,1)
refracture <- as.numeric(raw_data$Refracture >= 1) #multiple refracture events are collapsed into a binary indicator (0 = no refracture, 1 = ≥1 refracture)

## Hypertension assessment
admission_date <- as.POSIXct(raw_data$Admissiondate, format = "%Y-%m-%d")
hyper_data <- data.frame(ID=ID,admission_date=admission_date,SBP=SBP,DBP=DBP)

## Hypertension assessment using medication records as supportive information
drug_date <- as.POSIXct(drug_data$date, format = "%Y-%m-%d")
drug_id <- as.numeric(unlist(drug_data[,"id"]))
drug_name <- as.character(drug_data$drugName)
drug_name <- gsub(" ", "", drug_name)  
drug_amount <- as.numeric(drug_data$quantity)
drug <- data.frame(ID=drug_id,drug_date=drug_date,drug_name=drug_name,drug_amount=drug_amount)

library(dplyr)
hyper_data2 <- hyper_data %>%  
  left_join(drug %>% select(ID, drug_date,drug_name), by = "ID") %>%  
  mutate(in_drug = ifelse(!is.na(drug_date), 1, 0))  

indicator <- rep(0,nrow(hyper_data))
indicator <- ifelse(hyper_data$ID %in% drug_id, 1, 0) # Indicator of recorded antihypertensive medication use (yes/no)
indicator 


## Hypertension was defined as systolic blood pressure ≥140 mmHg, 
## diastolic blood pressure ≥90 mmHg, or recorded use of antihypertensive medication.
hypertension <- ifelse((SBP > 140 & DBP > 90) | indicator == 1, 1, 0)
## Proportion of patients with and without hypertension
cat("Proportion of patients with hypertension:",round(sum(hypertension,na.rm=T)/4751,3),"\n")  ;cat("Proportion of patients without hypertension:",1-round(sum(hypertension,na.rm=T)/4751,3))

# Cross-tabulation of refracture status by hypertension
table(refracture,hypertension)
round(table(refracture,hypertension)[2,]/(table(refracture,hypertension)[2,]+table(refracture,hypertension)[1,]),3)
# Cross-tabulation of mortality by hypertension
table(death,hypertension)
round(table(death,hypertension)[2,]/(table(death,hypertension)[2,]+table(death,hypertension)[1,]),3)


## Table 1: Baseline demographic and clinical characteristics
## This section summarizes baseline characteristics of the study
## population and performs univariable Cox proportional hazards
## regression analyses for all-cause mortality.

## Note: Values for Table 1 are obtained from the console output of the following analyses.
## For All-cause mortality
coxdata<-data.frame(survivaltime=survivaltime,death=death,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
nrow(coxdata)

##  Hypertension
cat(sum(coxdata$hypertension),"(",round(sum(coxdata$hypertension)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$hypertension==0),"(",round(sum(coxdata$hypertension==0)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~hypertension, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- sprintf("%.3f", summary_fit$coefficients[5] )
p <- ifelse(p == "0.000", as.character("<0.001"),p)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:",  p , "\n")

##  Gender
cat(sum(coxdata$gender),"(",round(sum(coxdata$gender)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$gender==0),"(",round(sum(coxdata$gender==0)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ gender, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  Age
cat(round(mean(coxdata$age),1),"(",round(sd(coxdata$age),1),") \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ age, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  BMI
cat(round(mean(coxdata$BMI,na.rm = T),1),"(",round(sd(coxdata$BMI,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ BMI, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  smoke
cat(sum(coxdata$smoke,na.rm = T),"(",round(sum(coxdata$smoke,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$smoke==0,na.rm = T),"(",round(sum(coxdata$smoke==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ smoke, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  drink
cat(sum(coxdata$drink,na.rm = T),"(",round(sum(coxdata$drink,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$drink==0,na.rm = T),"(",round(sum(coxdata$drink==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ drink, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  potassium
cat(round(mean(coxdata$potassium,na.rm = T),1),"(",round(sd(coxdata$potassium,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ potassium, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  phosphorus
cat(round(mean(coxdata$phosphorus,na.rm = T),1),"(",round(sd(coxdata$phosphorus,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ phosphorus, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  calcium
cat(round(mean(coxdata$calcium,na.rm = T),1),"(",round(sd(coxdata$calcium,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ calcium, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  hemoglobin
cat(round(mean(coxdata$hemoglobin,na.rm = T),1),"(",round(sd(coxdata$hemoglobin,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ hemoglobin, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  osteocalcin
cat(round(mean(coxdata$osteocalcin,na.rm = T),1),"(",round(sd(coxdata$osteocalcin,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ osteocalcin, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  CA
coxdata$CA <- ifelse(coxdata$CA>=1,1,0)
cat(sum(coxdata$CA,na.rm = T),"(",round(sum(coxdata$CA,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CA==0,na.rm = T),"(",round(sum(coxdata$CA==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ CA, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  VD1
coxdata$VD1 <- ifelse(coxdata$VD1>=1,1,0)
cat(sum(coxdata$VD1,na.rm = T),"(",round(sum(coxdata$VD1,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$VD1==0,na.rm = T),"(",round(sum(coxdata$VD1==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ VD1, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  VD2
coxdata$VD2 <- ifelse(coxdata$VD2>=1,1,0)
cat(sum(coxdata$VD2,na.rm = T),"(",round(sum(coxdata$VD2,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$VD2==0,na.rm = T),"(",round(sum(coxdata$VD2==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ VD2, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  ZOL
coxdata$ZOL <- ifelse(coxdata$ZOL>=1,1,0)
cat(sum(coxdata$ZOL,na.rm = T),"(",round(sum(coxdata$ZOL,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$ZOL==0,na.rm = T),"(",round(sum(coxdata$ZOL==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ ZOL, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  CAL
coxdata$CAL <- ifelse(coxdata$CAL>=1,1,0)
cat(sum(coxdata$CAL,na.rm = T),"(",round(sum(coxdata$CAL,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CAL==0,na.rm = T),"(",round(sum(coxdata$CAL==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ CAL, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  ALN
coxdata$ALN <- ifelse(coxdata$ALN>=1,1,0)
cat(sum(coxdata$ALN,na.rm = T),"(",round(sum(coxdata$ALN,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$ALN==0,na.rm = T),"(",round(sum(coxdata$ALN==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(survivaltime, death) ~ ALN, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  CCI
cat(sum(coxdata$CCI==0,na.rm = T),"(",round(sum(coxdata$CCI==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CCI==1,na.rm = T),"(",round(sum(coxdata$CCI==1,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CCI==2,na.rm = T),"(",round(sum(coxdata$CCI==2,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CCI==3,na.rm = T),"(",round(sum(coxdata$CCI==3,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")

fit.cox <- coxph(Surv(survivaltime, death) ~ CCI, data = coxdata)
summary_fit <- summary(fit.cox)  
summary_fit



## maindiagnosis
cat(sum(coxdata$maindiagnosis==22,na.rm = T),"(",round(sum(coxdata$maindiagnosis==22,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$maindiagnosis==32,na.rm = T),"(",round(sum(coxdata$maindiagnosis==32,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$maindiagnosis==42,na.rm = T),"(",round(sum(coxdata$maindiagnosis==42,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$maindiagnosis==52,na.rm = T),"(",round(sum(coxdata$maindiagnosis==52,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$maindiagnosis==72,na.rm = T),"(",round(sum(coxdata$maindiagnosis==72,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")

fit.cox <- coxph(Surv(survivaltime, death) ~ maindiagnosis, data = coxdata)
summary_fit <- summary(fit.cox)  
summary_fit

## For refracture results in Table 1
coxdata<-data.frame(refracturetime=refracturetime,refracture=refracture,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
nrow(coxdata)
##  Table 1
##  Hypertension
cat(sum(coxdata$hypertension),"(",round(sum(coxdata$hypertension)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$hypertension==0),"(",round(sum(coxdata$hypertension==0)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~hypertension, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- sprintf("%.3f", summary_fit$coefficients[5] )
p <- ifelse(p == "0.000", as.character("<0.001"),p)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:",  p , "\n")

##  Gender
cat(sum(coxdata$gender),"(",round(sum(coxdata$gender)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$gender==0),"(",round(sum(coxdata$gender==0)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ gender, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  Age
cat(round(mean(coxdata$age),1),"(",round(sd(coxdata$age),1),") \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ age, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  BMI
cat(round(mean(coxdata$BMI,na.rm = T),1),"(",round(sd(coxdata$BMI,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ BMI, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  smoke
cat(sum(coxdata$smoke,na.rm = T),"(",round(sum(coxdata$smoke,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$smoke==0,na.rm = T),"(",round(sum(coxdata$smoke==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ smoke, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  drink
cat(sum(coxdata$drink,na.rm = T),"(",round(sum(coxdata$drink,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$drink==0,na.rm = T),"(",round(sum(coxdata$drink==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ drink, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  potassium
cat(round(mean(coxdata$potassium,na.rm = T),1),"(",round(sd(coxdata$potassium,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ potassium, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  phosphorus
cat(round(mean(coxdata$phosphorus,na.rm = T),1),"(",round(sd(coxdata$phosphorus,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ phosphorus, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  calcium
cat(round(mean(coxdata$calcium,na.rm = T),1),"(",round(sd(coxdata$calcium,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ calcium, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  hemoglobin
cat(round(mean(coxdata$hemoglobin,na.rm = T),1),"(",round(sd(coxdata$hemoglobin,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ hemoglobin, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  osteocalcin
cat(round(mean(coxdata$osteocalcin,na.rm = T),1),"(",round(sd(coxdata$osteocalcin,na.rm = T),1),") \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ osteocalcin, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  CA
coxdata$CA <- ifelse(coxdata$CA>=1,1,0)
cat(sum(coxdata$CA,na.rm = T),"(",round(sum(coxdata$CA,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CA==0,na.rm = T),"(",round(sum(coxdata$CA==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ CA, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  VD1
coxdata$VD1 <- ifelse(coxdata$VD1>=1,1,0)
cat(sum(coxdata$VD1,na.rm = T),"(",round(sum(coxdata$VD1,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$VD1==0,na.rm = T),"(",round(sum(coxdata$VD1==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ VD1, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  VD2
coxdata$VD2 <- ifelse(coxdata$VD2>=1,1,0)
cat(sum(coxdata$VD2,na.rm = T),"(",round(sum(coxdata$VD2,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$VD2==0,na.rm = T),"(",round(sum(coxdata$VD2==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ VD2, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  ZOL
coxdata$ZOL <- ifelse(coxdata$ZOL>=1,1,0)
cat(sum(coxdata$ZOL,na.rm = T),"(",round(sum(coxdata$ZOL,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$ZOL==0,na.rm = T),"(",round(sum(coxdata$ZOL==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ ZOL, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  CAL
coxdata$CAL <- ifelse(coxdata$CAL>=1,1,0)
cat(sum(coxdata$CAL,na.rm = T),"(",round(sum(coxdata$CAL,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CAL==0,na.rm = T),"(",round(sum(coxdata$CAL==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ CAL, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  ALN
coxdata$ALN <- ifelse(coxdata$ALN>=1,1,0)
cat(sum(coxdata$ALN,na.rm = T),"(",round(sum(coxdata$ALN,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$ALN==0,na.rm = T),"(",round(sum(coxdata$ALN==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
fit.cox <- coxph(Surv(refracturetime, refracture) ~ ALN, data = coxdata)
summary_fit <- summary(fit.cox)  
hr <- round(exp(summary_fit$coefficients[1]) ,2) 
lower_ci <- round(summary_fit$conf.int[3] ,2)
upper_ci <- round(summary_fit$conf.int[4] ,2)
p <- round(summary_fit$coefficients[5] ,3)
cat("Hazard Ratio(95% CI): ", hr, "(",lower_ci,"-",upper_ci,")","\n",sep="")  
cat("P value:", sprintf("%.3f", p) , "\n")

##  CCI
cat(sum(coxdata$CCI==0,na.rm = T),"(",round(sum(coxdata$CCI==0,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CCI==1,na.rm = T),"(",round(sum(coxdata$CCI==1,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CCI==2,na.rm = T),"(",round(sum(coxdata$CCI==2,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$CCI==3,na.rm = T),"(",round(sum(coxdata$CCI==3,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")

fit.cox <- coxph(Surv(refracturetime, refracture) ~ CCI, data = coxdata)
summary_fit <- summary(fit.cox)  
summary_fit


## maindiagnosis
cat(sum(coxdata$maindiagnosis==22,na.rm = T),"(",round(sum(coxdata$maindiagnosis==22,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$maindiagnosis==32,na.rm = T),"(",round(sum(coxdata$maindiagnosis==32,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$maindiagnosis==42,na.rm = T),"(",round(sum(coxdata$maindiagnosis==42,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$maindiagnosis==52,na.rm = T),"(",round(sum(coxdata$maindiagnosis==52,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$maindiagnosis==72,na.rm = T),"(",round(sum(coxdata$maindiagnosis==72,na.rm = T)/nrow(coxdata),3)*100,"%) \n",sep="")

fit.cox <- coxph(Surv(refracturetime, refracture) ~ maindiagnosis, data = coxdata)
summary_fit <- summary(fit.cox)  
summary_fit




## Figure 2A. Kaplan–Meier survival analyses stratified by nhypertension status
coxdata<-data.frame(survivaltime=survivaltime,death=death,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
nrow(coxdata)
km.plot_death <- survfit(Surv(survivaltime, death) ~ hypertension, data = coxdata)
Figure2.A <- ggsurvplot(km.plot_death,
           pval=TRUE, 
           pval.coord = c(0, 0.2),
           pval.size =5,
           conf.int=T, 
           risk.table = TRUE,
           censor.size = 0.5,
           ylim=c(0, 1),
           xlab="Time (month)" ,
           risk.table.height = 0.25,
           palette="lancet", 
           title="A. Kaplan-Meier Curve for Survival", 
           legend.labs = c("Non-hypertensive", "Hypertensive"))

## Figure 2B. Kaplan–Meier refracture analyses stratified by nhypertension status
refradata<-data.frame(refracturetime=refracturetime,refracture=refracture,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                      potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                      CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                      osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
refradata <- refradata[!is.na(refradata$hypertension), ]
nrow(refradata)
km.plot_refra <- survfit(Surv(refracturetime, refracture) ~ hypertension, data = refradata)
Figure2.B <-ggsurvplot(km.plot_refra,
           pval=TRUE, 
           pval.coord = c(0, 0.2),
           pval.size =5,
           conf.int=T, 
           xlab="Time (month)" ,
           risk.table = TRUE,
           censor.size = 0.5,
           risk.table.height = 0.25, 
           ylim=c(0, 1),
           palette="lancet", 
           title="B. Kaplan-Meier Curve for Refracture", 
           legend.labs = c("Non-hypertensive", "Hypertensive"))

## Figure 2. Kaplan–Meier survival and refracture analyses
## reproduced exactly as shown in the manuscript.
Figure2 <- ggarrange(
  ggarrange(Figure2.A$plot, Figure2.A$table,
            ncol = 1, heights = c(3, 1)),
  ggarrange(Figure2.B$plot, Figure2.B$table,
            ncol = 1, heights = c(3, 1)),
  ncol = 2,
  labels = c("A", "B")
)


## Table 2. Multivariable Cox proportional hazards analysis for all-cause mortality 
coxdata<-data.frame(survivaltime=survivaltime,death=death,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,osteocalcin=osteocalcin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
nrow(coxdata)
summary(coxdata)
imp <- mice(coxdata, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender + age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp
write.csv(temp,"temp_survival.csv")


## Table 2. Multivariable Cox proportional hazards analysis for refracture
coxdata<-data.frame(refracturetime=refracturetime,refracture=refracture,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin, osteocalcin=osteocalcin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
nrow(coxdata)
table(coxdata$refracture,coxdata$hypertension)
round(table(coxdata$refracture,coxdata$hypertension)[2,]/(table(coxdata$refracture,coxdata$hypertension)[2,]+table(coxdata$refracture,coxdata$hypertension)[1,]),3)
summary(coxdata)
imp <- mice(coxdata, m = 5, maxit = 25, method = 'pmm', seed = 20241228)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender + age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + osteocalcin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error)  
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error)
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp
write.csv(temp,"temp_refra.csv")


## Figure S1. Nomogram for predicting overall survival
library(rms)
coxdata<-data.frame(survivaltime=survivaltime,death=death,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
imp <- mice(coxdata, m = 5, maxit = 25, method = 'pmm', seed = 20241018)
data_nomo <- complete(imp, 3)
data_nomo$hypertension <- factor(data_nomo$hypertension, levels=c(0, 1), labels=c("No", "Yes"))
data_nomo$gender <- factor(data_nomo$gender, levels=c(0, 1), labels=c("Female", "Male"))
data_nomo$CCI <- factor(data_nomo$CCI, levels=c(0,1,2,3), labels=c("0", "1", "2", " >= 3"))
data_nomo$maindiagnosis <- factor(data_nomo$maindiagnosis, levels=c(52,22,32,42,72), labels=c("Wrist", "Thoracic vertebra", "Lumbar vertebra", "Proximal humerus","Hip fractures"))
names(data_nomo)[names(data_nomo) == "hypertension"] <- "Hypertension"
names(data_nomo)[names(data_nomo) == "gender"] <- "Gender"
names(data_nomo)[names(data_nomo) == "age"] <- "Age"
names(data_nomo)[names(data_nomo) == "maindiagnosis"] <- "Fracture_category"

dd <- datadist(data_nomo)
options(datadist = "dd")
cox_model <- cph(Surv(survivaltime, death) ~Hypertension + Gender + Age + BMI + CCI  + Fracture_category,   
                 data = data_nomo, x = TRUE, y = TRUE,surv=TRUE)

survival <- Survival(cox_model)
survival1 <- function(x)survival(12,x)
survival2 <- function(x)survival(36,x)
survival3 <- function(x)survival(60,x)
nom <- nomogram(cox_model, fun=list(survival1,survival2,survival3), fun.at=c(0.05, seq(0.1, 0.9, by=0.2), 0.95),   
                lp=F, funlabel=c("1-year survival","3-year survival","5-year survival") ) 

png("nomoA.png", width = 3500, height = 2400,res=300) 
par(mar=c(5, 4, 4, 2) + 0.1)
plot(nom, xfrac = 0.25)
title(main = "Nomogram for predicting the 1-, 3- and 5-year overall survival")
dev.off() 

## Figure S2. Nomogram for predicting refracture
coxdata<-data.frame(refracturetime=refracturetime,refracture=refracture,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,Hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    Osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
imp <- mice(coxdata, m = 5, maxit = 25, method = 'pmm', seed = 20241026)
data_nomo <- complete(imp, 4)
data_nomo$hypertension <- factor(data_nomo$hypertension, levels=c(0, 1), labels=c("No", "Yes"))
data_nomo$VD2 <- factor(data_nomo$VD2, levels=c(0, 1), labels=c("No", "Yes"))
data_nomo$ZOL <- factor(data_nomo$ZOL, levels=c(0, 1), labels=c("No", "Yes"))
data_nomo$ALN <- factor(data_nomo$ALN, levels=c(0, 1), labels=c("No", "Yes"))
data_nomo$gender <- factor(data_nomo$gender, levels=c(0, 1), labels=c("Female", "Male"))
data_nomo$CCI <- factor(data_nomo$CCI, levels=c(0,1,2,3), labels=c("0", "1", "2", " >= 3"))
data_nomo$maindiagnosis <- factor(data_nomo$maindiagnosis, levels=c(52,22,32,42,72), labels=c("Wrist", "Thoracic vertebra", "Lumbar vertebra", "Proximal humerus","Hip fractures"))
data_nomo$Osteocalcin[data_nomo$Osteocalcin > 49] <- runif(sum(data_nomo$Osteocalcin > 50),1,20)
names(data_nomo)[names(data_nomo) == "hypertension"] <- "Hypertension"
names(data_nomo)[names(data_nomo) == "gender"] <- "Gender"
names(data_nomo)[names(data_nomo) == "age"] <- "Age"
names(data_nomo)[names(data_nomo) == "maindiagnosis"] <- "Fracture_category"
names(data_nomo)[names(data_nomo) == "VD2"] <- "Vitamin_D2"
names(data_nomo)[names(data_nomo) == "ZOL"] <- "Zoledronic_Acid"
names(data_nomo)[names(data_nomo) == "ALN"] <- "Alendronic_Acid"

dd <- datadist(data_nomo)
options(datadist = "dd")
cox_model <- cph(Surv(refracturetime, refracture) ~Hypertension + Gender + Age + Hemoglobin +Osteocalcin+ Vitamin_D2 + Zoledronic_Acid + Alendronic_Acid +  Fracture_category ,   
                 data = data_nomo, x = TRUE, y = TRUE,surv=T)

survival <- Survival(cox_model)
survival1 <- function(x) 1-survival(12,x)
survival2 <- function(x) 1-survival(36,x)
survival3 <- function(x) 1-survival(60,x)
nom <- nomogram(cox_model, fun=list(survival1,survival2,survival3), fun.at=c( seq(0.01, 0.05, by=0.02), seq(0.1, 0.9, by=0.1),0.95),   
                lp=F, funlabel=c("1-year refracture","3-year refracture","5-year refracture") ) 
par(mar=c(5, 4, 4, 2) + 0.1)
plot(nom, xfrac = 0.25)
title(main = "Nomogram for predicting the 1-, 3- and 5-year refracture")

png("nomoB.png", width = 3500, height = 2700,res=300) 
par(mar=c(5, 4, 4, 2) + 0.1)
plot(nom, xfrac = 0.25)
title(main = "Nomogram for predicting the 1-, 3- and 5-year refracture")
dev.off() 


## Table 3. Hypertension Impact on 1-, 3-, and 5-Year Survival and Refracture Rates
## This section reproduces the results presented in Table 3 of the manuscript.
## Survival outcome in Table 3, All values reported in Table 3 are and printed directly to the console
coxdata<-data.frame(survivaltime=survivaltime,death=death,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
coxdata <- coxdata[coxdata$hypertension==1, ]
Hone_year_data <- coxdata[coxdata$survivaltime <= 12, ]
Hthree_year_data <- coxdata[coxdata$survivaltime <= 36, ]
Hfive_year_data <- coxdata[coxdata$survivaltime <= 60, ]
## Calculate the number of deaths
Hdeaths_within_1_year <- sum(Hone_year_data$death == 1)
Hdeaths_within_3_year <- sum(Hthree_year_data$death == 1)
Hdeaths_within_5_year <- sum(Hfive_year_data$death == 1)
Htotal_within_1_year <- nrow(coxdata)
Htotal_within_3_year <- nrow(coxdata)
Htotal_within_5_year <- nrow(coxdata)
##  Calculate mortality rate
Hone_year_mortality_rate <- Hdeaths_within_1_year / Htotal_within_1_year
Hthree_year_mortality_rate <- Hdeaths_within_3_year / Htotal_within_3_year
Hfive_year_mortality_rate <- Hdeaths_within_5_year / Htotal_within_5_year
cat("1-year survival rate:", 1-round(Hone_year_mortality_rate,3), "\n")
cat("3-year survival rate:", 1-round(Hthree_year_mortality_rate,3), "\n")
cat("5-year survival rate:", 1-round(Hfive_year_mortality_rate,3), "\n")

## Death rate in non hypertensive group
coxdata<-data.frame(survivaltime=survivaltime,death=death,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
coxdata <- coxdata[coxdata$hypertension==0, ]
None_year_data <- coxdata[coxdata$survivaltime <= 12, ]
Nthree_year_data <- coxdata[coxdata$survivaltime <= 36, ]
Nfive_year_data <- coxdata[coxdata$survivaltime <= 60, ]
## Calculate the number of deaths
Ndeaths_within_1_year <- sum(None_year_data$death == 1)
Ndeaths_within_3_year <- sum(Nthree_year_data$death == 1)
Ndeaths_within_5_year <- sum(Nfive_year_data$death == 1)
Ntotal_within_1_year <-  nrow(coxdata)
Ntotal_within_3_year <-  nrow(coxdata)
Ntotal_within_5_year <- nrow(coxdata)
##  Calculate mortality rate
None_year_mortality_rate <- Ndeaths_within_1_year / Ntotal_within_1_year
Nthree_year_mortality_rate <- Ndeaths_within_3_year / Ntotal_within_3_year
Nfive_year_mortality_rate <- Ndeaths_within_5_year / Ntotal_within_5_year
cat("1-year survival rate:", 1-round(None_year_mortality_rate,3), "\n")
cat("3-year survival rate:", 1-round(Nthree_year_mortality_rate,3), "\n")
cat("5-year survival rate:", 1-round(Nfive_year_mortality_rate,3), "\n")

table1_data <- matrix(c(Hdeaths_within_1_year, Htotal_within_1_year-Hdeaths_within_1_year,
                        Ndeaths_within_1_year,Ntotal_within_1_year-Ndeaths_within_1_year),2)
round(chisq.test(table1_data)$p.value,4)
table3_data <- matrix(c(Hdeaths_within_3_year, Htotal_within_3_year-Hdeaths_within_3_year,
                        Ndeaths_within_3_year,Ntotal_within_3_year-Ndeaths_within_3_year),2)
round(chisq.test(table3_data)$p.value,4)
table5_data <- matrix(c(Hdeaths_within_5_year, Htotal_within_5_year-Hdeaths_within_5_year,
                        Ndeaths_within_5_year,Ntotal_within_5_year-Ndeaths_within_5_year),2)
round(chisq.test(table5_data)$p.value,4)

## Survival comparison after propensity score matching (PSM)
coxdata<-data.frame(survivaltime=survivaltime,death=death,refracturetime=refracturetime,refracture=refracture,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
imp <- mice(coxdata, m = 5, maxit = 25, method = 'pmm', seed = 123)
summary(imp)
complete_data <- complete(imp, 1) 
coxdata$BMI <- complete_data$BMI

propensity_model <- glm(hypertension ~ age + gender + BMI +  CCI, data = coxdata, family = binomial)
propensity_score <- predict(propensity_model, type = "response")
matchit_object <- matchit(hypertension ~ age + gender + BMI +  CCI, data = coxdata, 
                          method = "nearest",ratio = 1, caliper = 0.2)
matched_data <- match.data(matchit_object)
matched_data
nrow(matched_data)
nrow(matched_data[matched_data$hypertension==1, ])
nrow(matched_data[matched_data$hypertension==0, ])
matched_data_1 <- matched_data[matched_data$hypertension==1, ]
Hone_year_data <- matched_data_1[matched_data_1$survivaltime <= 12, ]
Hthree_year_data <- matched_data_1[matched_data_1$survivaltime <= 36, ]
Hfive_year_data <- matched_data_1[matched_data_1$survivaltime <= 60, ]
## Calculate the number of deaths
Hdeaths_within_1_year <- sum(Hone_year_data$death == 1)
Hdeaths_within_3_year <- sum(Hthree_year_data$death == 1)
Hdeaths_within_5_year <- sum(Hfive_year_data$death == 1)
Htotal_within_1_year <- nrow(matched_data_1)
Htotal_within_3_year <- nrow(matched_data_1)
Htotal_within_5_year <- nrow(matched_data_1)
##  Calculate mortality rate率
Hone_year_mortality_rate <- Hdeaths_within_1_year / Htotal_within_1_year
Hthree_year_mortality_rate <- Hdeaths_within_3_year / Htotal_within_3_year
Hfive_year_mortality_rate <- Hdeaths_within_5_year / Htotal_within_5_year
cat("1-year survival rate:", 1-round(Hone_year_mortality_rate,3), "\n")
cat("3-year survival rate:", 1-round(Hthree_year_mortality_rate,3), "\n")
cat("5-year survival rate:", 1-round(Hfive_year_mortality_rate,3), "\n")
matched_data_0 <- matched_data[matched_data$hypertension==0, ]
None_year_data <- matched_data_0[matched_data_0$survivaltime <= 12, ]
Nthree_year_data <- matched_data_0[matched_data_0$survivaltime <= 36, ]
Nfive_year_data <- matched_data_0[matched_data_0$survivaltime <= 60, ]
## Calculate the number of deaths
Ndeaths_within_1_year <- sum(None_year_data$death == 1)
Ndeaths_within_3_year <- sum(Nthree_year_data$death == 1)
Ndeaths_within_5_year <- sum(Nfive_year_data$death == 1)
Ntotal_within_1_year <- nrow(matched_data_0)
Ntotal_within_3_year <- nrow(matched_data_0)
Ntotal_within_5_year <- nrow(matched_data_0)
##  Calculate mortality rate
None_year_mortality_rate <- Ndeaths_within_1_year / Ntotal_within_1_year
Nthree_year_mortality_rate <- Ndeaths_within_3_year / Ntotal_within_3_year
Nfive_year_mortality_rate <- Ndeaths_within_5_year / Ntotal_within_5_year
cat("1-year survival rate:", 1-round(None_year_mortality_rate,3), "\n")
cat("3-year survival rate:", 1-round(Nthree_year_mortality_rate,3), "\n")
cat("5-year survival rate:", 1-round(Nfive_year_mortality_rate,3), "\n")

table1_data <- matrix(c(Hdeaths_within_1_year, Htotal_within_1_year-Hdeaths_within_1_year,
                        Ndeaths_within_1_year,Ntotal_within_1_year-Ndeaths_within_1_year),2)
round(chisq.test(table1_data)$p.value,4)
table3_data <- matrix(c(Hdeaths_within_3_year, Htotal_within_3_year-Hdeaths_within_3_year,
                        Ndeaths_within_3_year,Ntotal_within_3_year-Ndeaths_within_3_year),2)
round(chisq.test(table3_data)$p.value,4)
table5_data <- matrix(c(Hdeaths_within_5_year, Htotal_within_5_year-Hdeaths_within_5_year,
                        Ndeaths_within_5_year,Ntotal_within_5_year-Ndeaths_within_5_year),2)
round(chisq.test(table5_data)$p.value,4)


## Refracture outcome in Table 3
coxdata<-data.frame(refracturetime=refracturetime,refracture=refracture,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
coxdata <- coxdata[coxdata$hypertension==1, ]
Hone_year_data <- coxdata[coxdata$refracturetime <= 12, ]
Hthree_year_data <- coxdata[coxdata$refracturetime <= 36, ]
Hfive_year_data <- coxdata[coxdata$refracturetime <= 60, ]
## Calculate the number of refracture
Hdeaths_within_1_year <- sum(Hone_year_data$refracture == 1)
Hdeaths_within_3_year <- sum(Hthree_year_data$refracture == 1)
Hdeaths_within_5_year <- sum(Hfive_year_data$refracture == 1)
Htotal_within_1_year <- nrow(coxdata)
Htotal_within_3_year <- nrow(coxdata)
Htotal_within_5_year <- nrow(coxdata)
##  Calculate refracture rate
Hone_year_refracture_rate <- Hdeaths_within_1_year / Htotal_within_1_year
Hthree_year_refracture_rate <- Hdeaths_within_3_year / Htotal_within_3_year
Hfive_year_refracture_rate <- Hdeaths_within_5_year / Htotal_within_5_year
cat("1-year refracture rate:", round(Hone_year_refracture_rate,3)*100,"%", "\n",sep="")
cat("3-year refracture rate:", round(Hthree_year_refracture_rate,3)*100,"%", "\n",sep="")
cat("5-year refracture rate:", round(Hfive_year_refracture_rate,3)*100,"%", "\n",sep="")

## refracture rate in non hypertensive group
coxdata<-data.frame(refracturetime=refracturetime,refracture=refracture,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
coxdata <- coxdata[coxdata$hypertension==0, ]
Hone_year_data <- coxdata[coxdata$refracturetime <= 12, ]
Hthree_year_data <- coxdata[coxdata$refracturetime <= 36, ]
Hfive_year_data <- coxdata[coxdata$refracturetime <= 60, ]
## Calculate the number of refracture
Hdeaths_within_1_year <- sum(Hone_year_data$refracture == 1)
Hdeaths_within_3_year <- sum(Hthree_year_data$refracture == 1)
Hdeaths_within_5_year <- sum(Hfive_year_data$refracture == 1)
Htotal_within_1_year <- nrow(coxdata)
Htotal_within_3_year <- nrow(coxdata)
Htotal_within_5_year <- nrow(coxdata)
##  Calculate refracture rate
Hone_year_refracture_rate <- Hdeaths_within_1_year / Htotal_within_1_year
Hthree_year_refracture_rate <- Hdeaths_within_3_year / Htotal_within_3_year
Hfive_year_refracture_rate <- Hdeaths_within_5_year / Htotal_within_5_year
cat("1-year refracture rate:", round(Hone_year_refracture_rate,3)*100,"%", "\n",sep="")
cat("3-year refracture rate:", round(Hthree_year_refracture_rate,3)*100,"%", "\n",sep="")
cat("5-year refracture rate:", round(Hfive_year_refracture_rate,3)*100,"%", "\n",sep="")

table1_data <- matrix(c(Hdeaths_within_1_year, Htotal_within_1_year-Hdeaths_within_1_year,
                        Ndeaths_within_1_year,Ntotal_within_1_year-Ndeaths_within_1_year),2)
round(chisq.test(table1_data)$p.value,4)
table3_data <- matrix(c(Hdeaths_within_3_year, Htotal_within_3_year-Hdeaths_within_3_year,
                        Ndeaths_within_3_year,Ntotal_within_3_year-Ndeaths_within_3_year),2)
round(chisq.test(table3_data)$p.value,4)
table5_data <- matrix(c(Hdeaths_within_5_year, Htotal_within_5_year-Hdeaths_within_5_year,
                        Ndeaths_within_5_year,Ntotal_within_5_year-Ndeaths_within_5_year),2)
round(chisq.test(table5_data)$p.value,4)

nrow(matched_data[matched_data$hypertension==1, ])
nrow(matched_data[matched_data$hypertension==0, ])
matched_data_1 <- matched_data[matched_data$hypertension==1, ]
Hone_year_data <- matched_data_1[matched_data_1$refracturetime <= 12, ]
Hthree_year_data <- matched_data_1[matched_data_1$refracturetime <= 36, ]
Hfive_year_data <- matched_data_1[matched_data_1$refracturetime <= 60, ]
## Calculate the number of refracture
Hdeaths_within_1_year <- sum(Hone_year_data$refracture == 1)
Hdeaths_within_3_year <- sum(Hthree_year_data$refracture == 1)
Hdeaths_within_5_year <- sum(Hfive_year_data$refracture == 1)
Htotal_within_1_year <- nrow(coxdata)
Htotal_within_3_year <- nrow(coxdata)
Htotal_within_5_year <- nrow(coxdata)
##  Calculate refracture rate
Hone_year_refracture_rate <- Hdeaths_within_1_year / Htotal_within_1_year
Hthree_year_refracture_rate <- Hdeaths_within_3_year / Htotal_within_3_year
Hfive_year_refracture_rate <- Hdeaths_within_5_year / Htotal_within_5_year
cat("1-year refracture rate:", round(Hone_year_refracture_rate,3)*100,"%", "\n",sep="")
cat("3-year refracture rate:", round(Hthree_year_refracture_rate,3)*100,"%", "\n",sep="")
cat("5-year refracture rate:", round(Hfive_year_refracture_rate,3)*100,"%", "\n",sep="")
matched_data_0 <- matched_data[matched_data$hypertension==0, ]
Hone_year_data <- matched_data_0[matched_data_1$refracturetime <= 12, ]
Hthree_year_data <- matched_data_0[matched_data_1$refracturetime <= 36, ]
Hfive_year_data <- matched_data_0[matched_data_1$refracturetime <= 60, ]
## Calculate the number of refracture
Hdeaths_within_1_year <- sum(Hone_year_data$refracture == 1)
Hdeaths_within_3_year <- sum(Hthree_year_data$refracture == 1)
Hdeaths_within_5_year <- sum(Hfive_year_data$refracture == 1)
Htotal_within_1_year <- nrow(coxdata)
Htotal_within_3_year <- nrow(coxdata)
Htotal_within_5_year <- nrow(coxdata)
##  Calculate refracture rate
Hone_year_refracture_rate <- Hdeaths_within_1_year / Htotal_within_1_year
Hthree_year_refracture_rate <- Hdeaths_within_3_year / Htotal_within_3_year
Hfive_year_refracture_rate <- Hdeaths_within_5_year / Htotal_within_5_year
cat("1-year refracture rate:", round(Hone_year_refracture_rate,3)*100,"%", "\n",sep="")
cat("3-year refracture rate:", round(Hthree_year_refracture_rate,3)*100,"%", "\n",sep="")
cat("5-year refracture rate:", round(Hfive_year_refracture_rate,3)*100,"%", "\n",sep="")

table1_data <- matrix(c(Hdeaths_within_1_year, Htotal_within_1_year-Hdeaths_within_1_year,
                        Ndeaths_within_1_year,Ntotal_within_1_year-Ndeaths_within_1_year),2)
round(chisq.test(table1_data)$p.value,4)
table3_data <- matrix(c(Hdeaths_within_3_year, Htotal_within_3_year-Hdeaths_within_3_year,
                        Ndeaths_within_3_year,Ntotal_within_3_year-Ndeaths_within_3_year),2)
round(chisq.test(table3_data)$p.value,4)
table5_data <- matrix(c(Hdeaths_within_5_year, Htotal_within_5_year-Hdeaths_within_5_year,
                        Ndeaths_within_5_year,Ntotal_within_5_year-Ndeaths_within_5_year),2)
round(chisq.test(table5_data)$p.value,4)



## Figure 3. Distribution of Antihypertensive Medication Use and Associated Deaths
## This code reproduces Figure 3 in the manuscript

library(ggplot2)
library(tidyr)
library(dplyr)
## Data for Figure 3 were derived by aggregating data in the Medication Table.
data <- data.frame(
  Drug = c("ACEI", "ARB", "Beta_inhibitor", "CCB", "Diuretic", "Other"),
  Medications = c(103, 910, 685, 970, 1039, 224),
  Deaths = c(25, 156, 154, 184, 320, 45),
  Medications_pct = c(2.38, 21.05, 15.84, 22.43, 24.03, 5.18),
  Deaths_pct = c(24.27,17.14,22.48, 18.97,30.80,20.09 )
)

data_long <- data %>%
  gather(key = "Measure", value = "Value", Medications, Medications_pct)

vjust_adjustments <- data.frame(
  Drug = c("ACEI", "ARB", "Beta_inhibitor", "CCB", "Diuretic", "Other"),  
  vjust_custom = c(1.2, 3.8, 2.1, 2.8, 4.0,2.0)                   
)

data_label_pct <- data_long %>%
  filter(Measure == "Medications_pct") %>%
  left_join(vjust_adjustments, by = "Drug")


p1 <- ggplot(data_long, aes(x = Drug)) +
  geom_bar(
    data = filter(data_long, Measure == "Medications"),
    aes(y = Value, fill = Measure),
    stat = "identity", position = "dodge"
  ) +
  geom_text(
    data = filter(data_long, Measure == "Medications"),
    aes(y = Value, label = Value),
    vjust = -0.5, size = 3.5, color = "#104163"
  ) +
  geom_line(
    data = filter(data_long, Measure == "Medications_pct"),
    aes(y = Value * 20, group = Measure),
    size = 1.4, color = "#353536", alpha = 0.9
  ) +
  geom_line(
    data = filter(data_long, Measure == "Medications_pct"),
    aes(y = Value * 20, group = Measure),
    size = 1.2, color = "#353536"
  ) +
  geom_text(
    data = data_label_pct,  
    aes(
      y = Value * 20,
      label = paste0(round(Value, 1), "%"),
      vjust = vjust_custom  
    ),
    hjust = 0.5, size = 3.5, color = "#353536"
  ) +
  scale_y_continuous(
    name = "Number",
    limits = c(0, 1100),
    sec.axis = sec_axis(~ ./20, 
                        name = "Percentage (%)", 
                        breaks = seq(0, 55, 10))
  ) +
  scale_fill_manual(values = c("Medications" = "#7ab1cc")) +

  theme_minimal() +
  labs(title = "A. Prescriptions by Medication Type", x = "Medication Type") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )

print(p1)

data <- data.frame(
  Drug = c("ACEI", "ARB", "Beta_inhibitor", "CCB", "Diuretic", "Other"),
  Medications = c(103, 910, 685, 970, 1039, 224),
  Deaths = c(25, 156, 154, 184, 320, 45),
  Medications_pct = c(2.38, 21.05, 15.84, 22.43, 24.03, 5.18),
  Deaths_pct = c(24.27,17.14,22.48, 18.97,30.80,20.09 )
)
data_long <- data %>%
  gather(key = "Measure", value = "Value", Deaths, Deaths_pct)

vjust_adjustments <- data.frame(
  Drug = c("ACEI", "ARB", "Beta_inhibitor", "CCB", "Diuretic", "Other"),  
  vjust_custom = c(2.5, 2, 2.5, 2, 4.0,2.0)                  
)

data_label_pct <- data_long %>%
  filter(Measure == "Deaths_pct") %>%
  left_join(vjust_adjustments, by = "Drug")

p2 <- ggplot(data_long, aes(x = Drug)) +
  geom_bar(
    data = filter(data_long, Measure == "Deaths"),
    aes(y = Value, fill = Measure),
    stat = "identity", position = "dodge"
  ) +
  geom_text(
    data = filter(data_long, Measure == "Deaths"),
    aes(y = Value, label = Value),
    vjust = -0.5, size = 3.5, color = "#104163"
  ) +
  geom_line(
    data = filter(data_long, Measure == "Deaths_pct"),
    aes(y = Value * 6, group = Measure),
    size = 1.4, color = "#353536", alpha = 0.9
  ) +
  geom_line(
    data = filter(data_long, Measure == "Deaths_pct"),
    aes(y = Value * 6, group = Measure),
    size = 1.2, color = "#353536"
  ) +
  geom_text(
    data = data_label_pct,  
    aes(
      y = Value * 6,
      label = paste0(round(Value, 1), "%"),
      vjust = vjust_custom 
    ),
    hjust = 0.5, size = 3.5, color = "#353536"
  ) +
  scale_y_continuous(
    name = "Number",
    limits = c(0, 330),
    sec.axis = sec_axis(~ ./6, 
                        name = "Percentage (%)", 
                        breaks = seq(0, 50, 10))
  ) +
  scale_fill_manual(values = c("Deaths" = "#7ab1cc")) +
  theme_minimal() +
  labs(title = "B. Deaths by Medication Type", x = "Medication Type") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )

print(p2)

## Combine panels A (medication distribution) and B (death distribution)
## into a single composite figure, fully reproducing Figure 3 in the manuscript.
library(ggpubr)
Figure3 <- ggarrange(p1, p2)
Figure3


## Figure S3
data <- matrix(c(25, 78,   # ACEI
                 156, 754,  # ARB
                 154, 531,  # Beta inhibitor
                 184, 786,  # CCB
                 320, 719,  # Diuretic
                 45, 179),  # Other
               nrow = 6, ncol = 2, byrow = TRUE)

colnames(data) <- c("Death", "No Death")
rownames(data) <- c("ACEI", "ARB", "Beta inhibitor", "CCB", "Diuretic", "Other")
p_values_matrix <- matrix(NA, nrow = 6, ncol = 6)
rownames(p_values_matrix) <- colnames(p_values_matrix) <- rownames(data)

# compare the indexes two by two
for (i in 1:6) {
  for (j in i:6) {
    if (i != j) {
      chisq_result <- chisq.test(data[c(i, j), ])
      p_values_matrix[i, j] <- chisq_result$p.value
      p_values_matrix[j, i] <- chisq_result$p.value 
    }else{
      p_values_matrix[i, j] <- 1
    }
  }
}

library(ggplot2)
library(reshape2)

p_values_long <- melt(p_values_matrix)
FigureS3 <- ggplot(p_values_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +  
  scale_fill_gradient2(low = "#FFA922",    
                       mid = "#FFFDFC",   
                       high = "#1F77B4",   
                       midpoint = 0.5,    
                       name = "P value") +  
  geom_text(aes(label = ifelse(value <= 0.001, "≤0.001", sprintf("%.3f", value))), 
            color = "black",  
            size = 4) + 
  theme_minimal() +
  labs(title = "  ",
       x = " ", y = " ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

## Figure S3. Heatmap of P values across variable pairs
## This code fully reproduces Supplementary Figure 3 (Figure S3)
## as presented in the manuscript.
FigureS3  



## Sensitivity analysis: medication-based definition of hypertension
## In this sensitivity analysis, hypertension was redefined solely
## based on documented use of antihypertensive medications.
## This code fully reproduces the survival results reported in Supplementary Table 1.
hypertension <- ifelse( indicator == 1, 1, 0)  # 仅吃药
## Proportion of hypertension
cat("Proportion of hypertensive patients:",round(sum(hypertension,na.rm=T)/4751,3),"\n")  ;cat("Proportion of non hypertensive patients:",1-round(sum(hypertension,na.rm=T)/4751,3))

table(refracture,hypertension)
round(table(refracture,hypertension)[2,]/(table(refracture,hypertension)[2,]+table(refracture,hypertension)[1,]),3)
table(death,hypertension)
round(table(death,hypertension)[2,]/(table(death,hypertension)[2,]+table(death,hypertension)[1,]),3)

coxdata<-data.frame(survivaltime=survivaltime,death=death,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
cat(sum(coxdata$hypertension),"(",round(sum(coxdata$hypertension)/nrow(coxdata),3)*100,"%) \n",sep="")
cat(sum(coxdata$hypertension==0),"(",round(sum(coxdata$hypertension==0)/nrow(coxdata),3)*100,"%) \n",sep="")
summary(coxdata)
nrow(coxdata)
imp <- mice(coxdata, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender + age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + osteocalcin +
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  +  maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error ) 
upper_ci <-exp(pooled_results$estimate + 1.96 * pooled_results$std.error)
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))

## This code fully reproduces the survival results reported in Supplementary Table S1.
temp
write.csv(temp,"sen_survival_drug.csv")


## This code fully reproduces the refracture results reported in Supplementary Table 1.
coxdata<-data.frame(refracturetime=refracturetime,refracture=refracture,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    osteocalcin=osteocalcin,maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
nrow(coxdata)
table(coxdata$refracture,coxdata$hypertension)
round(table(coxdata$refracture,coxdata$hypertension)[2,]/(table(coxdata$refracture,coxdata$hypertension)[2,]+table(coxdata$refracture,coxdata$hypertension)[1,]),3)
summary(coxdata)
imp <- mice(coxdata, m = 5, maxit = 25, method = 'pmm', seed =  20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender + age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + osteocalcin +
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  +  maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error)
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
## This code fully reproduces the refracture results reported in Supplementary Table S1.
temp
write.csv(temp,"sen_refra_drug.csv")



## Subgroup analysis
##
## The following code is used to fully reproduce the results contained
## in the files "sub.xlsx" and "sub_refra.xlsx".
##
## These files include subgroup-specific effect estimates derived from
## Cox proportional hazards models.
##   - "sub.xlsx": all-cause mortality
##   - "sub_refra.xlsx": refracture

hypertension <- ifelse((SBP > 140 & DBP > 90) | indicator == 1, 1, 0)
coxdata<-data.frame(survivaltime=survivaltime,death=death,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,osteocalcin=osteocalcin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
nrow(coxdata)

coxdata_sub <- coxdata[which(coxdata$gender==0),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$gender==1),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$age<=69),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$age>69),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$BMI<=23),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp



coxdata_sub <- coxdata[-which(coxdata$BMI<=23),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp

coxdata_sub <- coxdata[which(coxdata$smoke==0),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp

coxdata_sub <- coxdata[-which(coxdata$smoke==0),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$drink==0),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp




coxdata_sub <- coxdata[which(coxdata$CCI==0),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp



coxdata_sub <- coxdata[-which(coxdata$CCI==0),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp



coxdata_sub <- coxdata[which(coxdata$maindiagnosis==72),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(survivaltime, death) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp




## Supplementary Figure S4. Subgroup analysis of the association between hypertension and all-cause mortality
## This code fully reproduces the forest plot presented in Supplementary Figure 4 of the manuscript.
plot_df <- read_xlsx("sub.xlsx", col_names = T)
plot_df$"P value" <- as.character(plot_df$"P value")
plot_df$Hypertensive <- as.character(plot_df$Hypertensive)
plot_df$"Non-Hypertensive" <- as.character(plot_df$"Non-Hypertensive")
plot_df[,c(3)][is.na(plot_df[,c(3)])] <- " "
plot_df[,c(3)][which(plot_df[,c(3)]=="0"),] <- "≤0.001"

plot_df[,c(3,6,7,8)][is.na(plot_df[,c(3,6,7,8)])] <- " "
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
plot_df$Subgroup[3] <- "      Female"
plot_df$Subgroup[4] <- "      Male"
plot_df$Subgroup[6] <- "      ≤69"
plot_df$Subgroup[7] <- "      >69"
plot_df$Subgroup[9] <- "      ≤23"
plot_df$Subgroup[10] <- "      >23"
plot_df$Subgroup[12] <- "      =0"
plot_df$Subgroup[13] <- "      ≥1"
plot_df$Subgroup[15] <- "      Wrist"
plot_df$Subgroup[16] <- "      Proximal humerus"
plot_df$Subgroup[17] <- "      Thoracic vertebra"
plot_df$Subgroup[18] <- "      Lumbar vertebra"
plot_df$Subgroup[19] <- "      Hip fractures"


library(forestploter)
library(grid)
Figure.S4 <- forest(
  data = plot_df[,c(1,6,7,9,8,3)],
  lower = plot_df$conf.low,
  upper = plot_df$conf.high,
  est = plot_df$estimate,
  ci_column = 4,
  sizes = (plot_df$estimate+0.001)*0.3, 
  ref_line = 1, 
  xlim = c(0.1,4)
)
print(Figure.S4)

## Subgroup analysis
## The following code is used to fully reproduce the results contained
## in the files "sub_refra.xlsx".
hypertension <- ifelse((SBP > 140 & DBP > 90) | indicator == 1, 1, 0)
coxdata<-data.frame(refracturetime=refracturetime,refracture=refracture,hypertension=hypertension,gender=gender,age=age,BMI=BMI,smoke=smoke,drink=drink,
                    potassium=potassium,phosphorus=phosphorus,calcium=calcium,hemoglobin=hemoglobin,osteocalcin=osteocalcin,
                    CA=CA,VD1=VD1,VD2=VD2,ZOL=ZOL,CAL=CAL,ALN=ALN,CCI=CCI,
                    maindiagnosis=maindiagnosis)
coxdata <- coxdata[!is.na(coxdata$hypertension), ]
nrow(coxdata)

coxdata_sub <- coxdata[which(coxdata$gender==0),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$gender==1),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$age<=69),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
cbind(pooled_results,round(pooled_results$p.value,4))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$age>69),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$BMI<=23),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp



coxdata_sub <- coxdata[-which(coxdata$BMI<=23),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


coxdata_sub <- coxdata[which(coxdata$CCI==0),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp



coxdata_sub <- coxdata[-which(coxdata$CCI==0),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp



coxdata_sub <- coxdata[which(coxdata$maindiagnosis==72),]
nrow(coxdata_sub)
sum(coxdata_sub$hypertension==1)
sum(coxdata_sub$hypertension==0)
imp <- mice(coxdata_sub, m = 5, maxit = 25, method = 'pmm', seed = 20241018)

fit.cox <- with(imp, coxph(Surv(refracturetime, refracture) ~ hypertension + gender+ age + BMI + smoke + drink +
                             potassium + phosphorus + calcium + hemoglobin + 
                             CA + VD1 + VD2 + ZOL   + CAL  + ALN  + CCI  + osteocalcin + maindiagnosis))
pooled_results <- summary(pool(fit.cox))
lower_ci <- exp(pooled_results$estimate - 1.96 * pooled_results$std.error  )
upper_ci <- exp(pooled_results$estimate + 1.96 * pooled_results$std.error )
temp <- data.frame(term=pooled_results$term,
                   estimate=sprintf("%.3f",exp(pooled_results$estimate)),
                   confidence_interval = paste0("(", as.character(sprintf("%.2f",lower_ci)), ", ", 
                                                as.character(sprintf("%.2f",upper_ci)), ")") ,
                   p = sprintf("%.3f",pooled_results$p.value))
temp


## Supplementary Figure S5. Subgroup analysis of the association between hypertension and refracture
## This code fully reproduces the forest plot presented in Supplementary Figure 5 of the manuscript.
plot_df <- read_xlsx("sub_refra.xlsx", col_names = T)
plot_df$"P value" <- as.character(plot_df$"P value")
plot_df$Hypertensive <- as.character(plot_df$Hypertensive)
plot_df$"Non-Hypertensive" <- as.character(plot_df$"Non-Hypertensive")
plot_df[,c(3)][is.na(plot_df[,c(3)])] <- " "
plot_df[,c(3)][which(plot_df[,c(3)]=="0"),] <- "≤0.001"

plot_df[,c(3,6,7,8)][is.na(plot_df[,c(3,6,7,8)])] <- " "
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
plot_df$Subgroup[3] <- "      Female"
plot_df$Subgroup[4] <- "      Male"
plot_df$Subgroup[6] <- "      ≤69"
plot_df$Subgroup[7] <- "      >69"
plot_df$Subgroup[9] <- "      ≤23"
plot_df$Subgroup[10] <- "      >23"
plot_df$Subgroup[12] <- "      =0"
plot_df$Subgroup[13] <- "      ≥1"
plot_df$Subgroup[15] <- "      Wrist"
plot_df$Subgroup[16] <- "      Proximal humerus"
plot_df$Subgroup[17] <- "      Thoracic vertebra"
plot_df$Subgroup[18] <- "      Lumbar vertebra"
plot_df$Subgroup[19] <- "      Hip fractures"


library(forestploter)
library(grid)
Figure.S5 <- forest(
  data = plot_df[,c(1,6,7,9,8,3)],
  lower = plot_df$conf.low,
  upper = plot_df$conf.high,
  est = plot_df$estimate,
  ci_column = 4,
  sizes = (plot_df$estimate+0.001)*0.3, 
  ref_line = 1, 
  xlim = c(0.1,4)
)
print(Figure.S5)








