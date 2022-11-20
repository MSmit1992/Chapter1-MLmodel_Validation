##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Select Behaviours                  ###
##########################################

### READ R LIBRARIES
library(tidyverse); library(qwraps2); library(caret); library(mlbench); library(plot3D); library(data.table)
library(psych)

### LOAD DATA
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/Processed_data/"
setwd(paste0(start_path,"Preparation"))
load("Compl_data.RDATA")
str(dfCompl)
levels(dfCompl$Behaviour)
table(dfCompl$Behaviour)


################################################################################################################ BEHAVIOUR SELECTION
##########################################
### STEP 1 - FIRST BEHAVIOUR SELECTION ###
##########################################
table(dfCompl$Behaviour)

dfCompl <- dfCompl[!grepl("Active_Rolling",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Active_Running",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Inactive_Lying.Sleeping",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Maintenance_Nutrition.Drinking",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Other_ActigraphOff",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Other_Social.Human",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Other_Start",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Inactive_Lying.Down",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Inactive_Sitting.Down",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Inactive_Sitting.Up",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Inactive_Standing.Up",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Active_Playfight.Fighting",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Active_Playfight.Playing",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Other_Other",dfCompl$Behaviour),]

dfCompl <- droplevels(dfCompl)


################################################################################################################### MERGE BEHAVIOURS
#################################
### STEP 2 - MERGE BEHAVIOURS ###
#################################

### BEHAVIOUR 1 - 14
levels(dfCompl$Behaviour)
dfCompl$Behaviour1 <- dfCompl$Behaviour
levels(dfCompl$Behaviour1) <- c("Climbing","Jumping","Jumping","Rubbing","Trotting","Walking","Lying","Lying","Sitting",
                                "Standing","Grooming","Littering","Digging","Littering","Littering","Eating","Scratching","Shaking","Shaking","Allogroom")


### SAVE COMPLETE DATAFRAME
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))
save(dfCompl, file= "Compl_data.RDATA")


###################################################################################################################### DATA SPLITING
###############################
### STEP 3 - DATA SPLITTING ###
###############################
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
load("Compl_data.RDATA")

### SPLIT COLLAR AND HARNESS DATA
df.collar <- dfCompl %>% filter(Position == "Collar")
df.harness <- dfCompl %>% filter(Position == "Harness")

### MAKE SELECTION OF MOVING WINDOW FOR ODBA
# See Statistics R Script
### REMOVE ABUNDANT COLUMNS
df.collar <- df.collar %>% select(-ODBA15_Mean,-ODBA45_Mean,-ODBA60_Mean,-ODBA90_Mean)
df.collar <- rename(df.collar, ODBA=ODBA30_Mean)
df.harness <- df.harness %>% select(-ODBA15_Mean,-ODBA45_Mean,-ODBA60_Mean,-ODBA90_Mean)
df.harness <- rename(df.harness, ODBA=ODBA30_Mean)

### SAVE COLLAR AND HARNESS DATASET
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))
save(df.collar, file="df.collar.RDATA")
save(df.harness, file="df.harness.RDATA")

############################################################################################################# DOWNSIZING BEHAVIOURS
######################################
### STEP 4 - DOWNSIZING BEHAVIOURS ###
######################################

### COLLAR
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))
load("df.collar.RDATA")

table(df.collar$Behaviour1)

dt <- data.table(df.collar)
dt <- dt[-sample(which(Behaviour1=="Lying"),49209)]
dt <- dt[-sample(which(Behaviour1=="Sitting"),28395)]
dt <- dt[-sample(which(Behaviour1=="Standing"),904)]
dt <- dt[-sample(which(Behaviour1=="Grooming"),4772)]

df <- as.data.frame(dt)

df <- df %>% select(-Position, -Timestamp, -Behaviour)
df <- rename(df, Behaviour=Behaviour1)

df.collar <- df
save(df.collar, file="Collar_B1.RDATA")

### HARNESS
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))
load("df.harness.RDATA")

table(df.harness$Behaviour1)

dt <- data.table(df.harness)
dt <- dt[-sample(which(Behaviour1=="Lying"),49209)]
dt <- dt[-sample(which(Behaviour1=="Sitting"),28395)]
dt <- dt[-sample(which(Behaviour1=="Standing"),904)]
dt <- dt[-sample(which(Behaviour1=="Grooming"),4772)]

df <- as.data.frame(dt)

df <- df %>% select(-Position, -Timestamp, -Behaviour)
df <- rename(df, Behaviour=Behaviour1)

df.harness <- df
save(df.harness, file="Harness_B1.RDATA")


####################################################################################################################### BUILD MODELS
#############################
### STEP 5 - BUILD MODELS ###
#############################

#Step 5.1: Build RF and SOMs, see respective R Scripts
#Step 5.2: Merge/delete behaviours based on confusion matrix (as in Step 2 above)
# Repeat these steps until you have a model that you are happy with


###############################
### STEP 5.2 - RF - ROUND 1 ###
###############################
### BEHAVIOUR 2 - 8
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))

### COLLAR
load("Collar_B1.RDATA")
df <- df.collar

levels(df$Behaviour)
levels(df$Behaviour) <- c("Climbing","Jumping","Rubbing","Active","Active","Lying","Sitting","Standing","Grooming","Littering","Digging",
                                 "Eating","Scratching","Shaking","Grooming")

df <- df[!grepl("Climbing",df$Behaviour),]
df <- df[!grepl("Jumping",df$Behaviour),]
df <- df[!grepl("Rubbing",df$Behaviour),]
df <- df[!grepl("Digging",df$Behaviour),]
df <- df[!grepl("Shaking",df$Behaviour),]
df <- droplevels(df)

levels(df$Behaviour)
table(df$Behaviour)

df.collar <- df
levels(df.collar$Behaviour)
save(df.collar, file="Collar_B2_RF.RDATA")

### HARNESS
load("Harness_B1.RDATA")
df <- df.harness

levels(df$Behaviour)
levels(df$Behaviour) <- c("Climbing","Jumping","Rubbing","Active","Active","Lying","Sitting","Standing","Grooming","Littering","Digging",
                          "Eating","Scratching","Shaking","Grooming")

df <- df[!grepl("Climbing",df$Behaviour),]
df <- df[!grepl("Jumping",df$Behaviour),]
df <- df[!grepl("Rubbing",df$Behaviour),]
df <- df[!grepl("Digging",df$Behaviour),]
df <- df[!grepl("Shaking",df$Behaviour),]
df <- droplevels(df)

levels(df$Behaviour)
table(df$Behaviour)

df.harness <- df
save(df.harness, file="Harness_B2_RF.RDATA")




###############################
### STEP 5.2 - RF - ROUND 2 ###
###############################
### BEHAVIOUR 3 - 6
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))

### COLLAR
load("Collar_B2_RF.RDATA")
df <- df.collar

levels(df$Behaviour)
levels(df$Behaviour) <- c("Active","Lying","Sitting","Standing","Grooming","Littering","Eating","Grooming")

df <- df[!grepl("Littering",df$Behaviour),]
df <- droplevels(df)

levels(df$Behaviour)
table(df$Behaviour)

df.collar <- df
levels(df.collar$Behaviour)
save(df.collar, file="Collar_B3_RF.RDATA")

### HARNESS
load("Harness_B2_RF.RDATA")
df <- df.harness

levels(df$Behaviour)
levels(df$Behaviour) <- c("Active","Lying","Sitting","Standing","Grooming","Littering","Eating","Grooming")

df <- df[!grepl("Littering",df$Behaviour),]
df <- droplevels(df)

levels(df$Behaviour)
table(df$Behaviour)

df.harness <- df
save(df.harness, file="Harness_B3_RF.RDATA")

###############################
### STEP 5.2 - RF - ROUND 3 ###
###############################
### BEHAVIOUR 4 - 3
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))

### COLLAR
load("Collar_B3_RF.RDATA")
df <- df.collar

levels(df$Behaviour)
levels(df$Behaviour) <- c("Active","Inactive","Inactive","Inactive","Maintenance","Maintenance")

levels(df$Behaviour)
table(df$Behaviour)

df.collar <- df
levels(df.collar$Behaviour)
save(df.collar, file="Collar_B4_RF.RDATA")

### HARNESS
load("Harness_B3_RF.RDATA")
df <- df.harness

levels(df$Behaviour)
levels(df$Behaviour) <- c("Active","Inactive","Inactive","Inactive","Maintenance","Maintenance")

levels(df$Behaviour)
table(df$Behaviour)

df.harness <- df
save(df.harness, file="Harness_B4_RF.RDATA")


###############################
### STEP 5.2 - RF - ROUND 4 ###
###############################
### BEHAVIOUR 5 - 3
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))

### COLLAR
load("Collar_B4.RDATA")
df <- df.collar

table(df$Behaviour)

dt <- data.table(df.collar)
dt <- dt[-sample(which(Behaviour=="Inactive"),16000)]
dt <- dt[-sample(which(Behaviour=="Maintenance"),9444)]

df <- as.data.frame(dt)
table(df$Behaviour)

df.collar <- df
levels(df.collar$Behaviour)
save(df.collar, file="Collar_B5.RDATA")

### HARNESS
load("Harness_B4.RDATA")
table(df$Behaviour)

dt <- data.table(df.harness)
dt <- dt[-sample(which(Behaviour=="Inactive"),16000)]
dt <- dt[-sample(which(Behaviour=="Maintenance"),9444)]

df <- as.data.frame(dt)
table(df$Behaviour)

df.harness <- df
levels(df.harness$Behaviour)
save(df.harness, file="Harness_B5.RDATA")

################################
### STEP 5.2 - SOM - ROUND 1 ###
################################
### BEHAVIOUR 2 - 10
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))

### COLLAR
load("Collar_B1.RDATA")
df <- df.collar

levels(df$Behaviour)

df <- df[!grepl("Climbing",df$Behaviour),]
df <- df[!grepl("Jumping",df$Behaviour),]
df <- df[!grepl("Digging",df$Behaviour),]
df <- df[!grepl("Shaking",df$Behaviour),]
df <- df[!grepl("Allogroom",df$Behaviour),]
df <- droplevels(df)

levels(df$Behaviour)
table(df$Behaviour)

df.collar <- df
levels(df.collar$Behaviour)
save(df.collar, file="Collar_B2_SOM.RDATA")

### HARNESS
load("Harness_B1.RDATA")
df <- df.harness

levels(df$Behaviour)

df <- df[!grepl("Climbing",df$Behaviour),]
df <- df[!grepl("Jumping",df$Behaviour),]
df <- df[!grepl("Digging",df$Behaviour),]
df <- df[!grepl("Shaking",df$Behaviour),]
df <- df[!grepl("Allogroom",df$Behaviour),]
df <- droplevels(df)

levels(df$Behaviour)
table(df$Behaviour)

df.harness <- df
save(df.harness, file="Harness_B2_SOM.RDATA")




