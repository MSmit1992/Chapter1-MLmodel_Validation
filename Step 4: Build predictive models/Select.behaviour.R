##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Prepare datasets                   ###
##########################################

### READ R LIBRARIES
library(tidyverse); library(qwraps2); library(caret); library(mlbench); library(psych); library(plot3D)

### LOAD DATA
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
load("Compl_data.RDATA")
str(dfCompl)
levels(dfCompl$Behaviour)
table(dfCompl$Behaviour)

########################################################################################################## FIRST BEHAVIOUR SELECTION
##########################################
### STEP 1 - FIRST BEHAVIOUR SELECTION ###
##########################################

dfCompl <- dfCompl[!grepl("Active_Rolling",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Active_Running",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Inactive_Lying.Sleeping",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Maintenance_Nutrition.Drinking",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Other_ActigraphOff",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Other_Social.Human",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Other_Start",dfCompl$Behaviour),]
dfCompl <- dfCompl[!grepl("Other_Outofsight",dfCompl$Behaviour),]
dfCompl <- droplevels(dfCompl)


################################################################################################################### MERGE BEHAVIOURS
#################################
### STEP 2 - MERGE BEHAVIOURS ###
#################################

### BEHAVIOUR1
levels(dfCompl$Behaviour)
dfCompl$Behaviour1 <- dfCompl$Behaviour
levels(dfCompl$Behaviour1) <- c("Climbing","Jumping","Jumping","Playfight","Playfight","Rubbing","Trotting","Walking","Resting",
                                "LyingDown","Resting","SittingDown","Sitting","SittingUp","Standing","StandingUp","Grooming",
                                "Littering","Digging","Littering","Littering","Eating","Scratching","Shake","Shake","Other",
                                "Allogroom")
levels(dfCompl$Behaviour1)
table(dfCompl$Behaviour1)

### BEHAVIOUR2
levels(dfCompl$Behaviour1)
dfCompl$Behaviour2 <- dfCompl$Behaviour1
levels(dfCompl$Behaviour2) <- c("Other","Other","Other","Other","Trotting","Walking","Resting","LyingDown","SittingDown","Sitting",
                                "SittingUp","Standing","StandingUp","Grooming","Littering","Digging","Eating","Scratching",
                                "Other","Other","Other")
levels(dfCompl$Behaviour2)
table(dfCompl$Behaviour2)

### BEHAVIOUR3
levels(dfCompl$Behaviour2)
dfCompl$Behaviour3 <- dfCompl$Behaviour2
levels(dfCompl$Behaviour3) <- c("Other", "Trotting", "Walking", "Resting", "Resting", "Sitting", "Sitting", "Sitting", "Standing", 
                                "Standing", "Grooming", "Littering", "Digging", "Eating", "Scratching")
levels(dfCompl$Behaviour3)
table(dfCompl$Behaviour3)

### BEHAVIOUR4
dfCompl$Behaviour4 <- dfCompl$Behaviour3
levels(dfCompl$Behaviour4) <- c("Other", "Trotting", "Walking", "Resting", "Sitting", "Standing", 
                                "Grooming", "Littering", "Other", "Eating", "Scratching")
levels(dfCompl$Behaviour4)
table(dfCompl$Behaviour4)

### BEHAVIOUR5
dfCompl$Behaviour5 <- dfCompl$Behaviour4
levels(dfCompl$Behaviour5) <- c("Other", "Active", "Active", "Resting", "Sitting", "Standing", 
                                "Grooming", "Littering", "Eating", "Scratching")
levels(dfCompl$Behaviour5)
table(dfCompl$Behaviour5)

### Behaviour6
dfCompl$Behaviour6 <- dfCompl$Behaviour5
levels(dfCompl$Behaviour6) <- c("Other", "Active", "Inactive", "Inactive", "Inactive", "Maintenance", 
                                "Maintenance", "Maintenance", "Maintenance")
levels(dfCompl$Behaviour6)
table(dfCompl$Behaviour6)

### SAVE COMPLETE DATAFRAME
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
save(dfCompl, file= "Compl_data.RDATA")


###################################################################################################################### DATA SPLITING
###############################
### STEP 3 - DATA SPLITTING ###
###############################
start_path <- "C:/Users/200179481/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
load("Compl_data.RDATA")

### SPLIT COLLAR AND HARNESS DATA
df.collar <- dfCompl %>% filter(Position == "Collar")
df.harness <- dfCompl %>% filter(Position == "Harness")

### SAVE COLLAR AND HARNESS DATASET
start_path <- "C:/Users/200179481/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
save(df.collar, file="df.collar.RDATA")
save(df.harness, file="df.harness.RDATA")


############################################################################################################# CREATE SUB DATAFRAMES
######################################
### STEP 4 - CREATE SUB DATAFRAMES ###
######################################
# Subset data to features we wish to keep/use
# Required input: - Collar dataset
#                 - Harness dataset

start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))

### COLLAR
load("df.collar.RDATA")
df <- df.collar %>% select(Cat_id,Pen,Status,Behaviour,Behaviour1,Behaviour2,Behaviour3,Behaviour4, Behaviour5, Behaviour6, everything())

# Create seperate dataframe for Behaviour1
Collar_B1 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour2, -Behaviour3, -Behaviour4, -Behaviour5, -Behaviour6)
str(Collar_B1)
head(Collar_B1)
save(Collar_B1, file="Collar_B1.RDATA")

# Create seperate dataframe for Behaviour2
Collar_B2 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour3, -Behaviour4, -Behaviour5, -Behaviour6)
str(Collar_B2)
head(Collar_B2)
save(Collar_B2, file="Collar_B2.RDATA")

# Create seperate dataframe for Behaviour3
Collar_B3 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour2, -Behaviour4, -Behaviour5, -Behaviour6)
head(Collar_B3)
save(Collar_B3, file="Collar_B3.RDATA")

# Create seperate dataframe for Behaviour4
Collar_B4 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour2, -Behaviour3, -Behaviour5, -Behaviour6)
head(Collar_B4)
save(Collar_B4, file="Collar_B4.RDATA")

# Create seperate dataframe for Behaviour5
Collar_B5 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour2, -Behaviour3, -Behaviour4, -Behaviour6)
head(Collar_B5)
save(Collar_B5, file="Collar_B5.RDATA")

# Create seperate dataframe for Behaviour6
Collar_B6 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour2, -Behaviour3, -Behaviour4, -Behaviour5)
head(Collar_B6)
save(Collar_B6, file="Collar_B6.RDATA")


### HARNESS
load("df.harness.RDATA")
df <- df.harness %>% select(Cat_id,Pen,Status,Behaviour,Behaviour1,Behaviour2,Behaviour3,Behaviour4, Behaviour5, Behaviour6, everything())

# Create seperate dataframe for Behaviour1
Harness_B1 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour2, -Behaviour3, -Behaviour4, -Behaviour5, -Behaviour6)
str(Harness_B1)
head(Harness_B1)
save(Harness_B1, file="Harness_B1.RDATA")

# Create seperate dataframe for Behaviour2
Harness_B2 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour3, -Behaviour4, -Behaviour5, -Behaviour6)
str(Harness_B2)
head(Harness_B2)
save(Harness_B2, file="Harness_B2.RDATA")

# Create seperate dataframe for Behaviour3
Harness_B3 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour2, -Behaviour4, -Behaviour5, -Behaviour6)
head(Harness_B3)
save(Harness_B3, file="Harness_B3.RDATA")

# Create seperate dataframe for Behaviour4
Harness_B4 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour2, -Behaviour3, -Behaviour5, -Behaviour6)
head(Harness_B4)
save(Harness_B4, file="Harness_B4.RDATA")

# Create seperate dataframe for Behaviour5
Harness_B5 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour2, -Behaviour3, -Behaviour4, -Behaviour6)
head(Harness_B5)
save(Harness_B5, file="Harness_B5.RDATA")

# Create seperate dataframe for Behaviour6
Harness_B6 <- df %>% select(-Position, -Timestamp, -Pen, -Status, -Behaviour, -Behaviour1, -Behaviour2, -Behaviour3, -Behaviour4, -Behaviour5)
head(Harness_B6)
save(Harness_B6, file="Harness_B6.RDATA")

