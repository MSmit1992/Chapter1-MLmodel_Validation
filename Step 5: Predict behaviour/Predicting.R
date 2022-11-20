##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Predicting behaviour               ###
##########################################

### READ R LIBRARIES
library(plyr);library(dplyr);library(lubridate);library(readxl)
library(tidyverse); library(caret); library(randomForest); library(kohonen)


################################################################################################################# PREPARATION META DATA
#######################################
### STEP 1 -  PREPARATION META DATA ###
#######################################
# Required input: - Meta data prediction
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"meta_data"))
meta <- read.csv("Predict_Meta.csv")
head(meta)
str(meta)

### ADD DATE TO COLUMNS OB_START AND OB_END
df <- meta
df <- df %>%
  mutate(Ob_Start = paste("2021/06/30",df$Ob_Start, sep=" "),Ob_End = paste("2021/07/07",df$Ob_End, sep=" "))

### CHANGE VARIABLE TYPES
# Change Cat_id from chr to factor
df$Cat_id <- as.factor(df$Cat_id)
# Change Ob_Start and Ob_End from chr to POSIXct
df$Ob_Start<-as.POSIXct(df$Ob_Start,format="%Y/%m/%d %H:%M:%S")
df$Ob_End<-as.POSIXct(df$Ob_End,format="%Y/%m/%d %H:%M:%S")

### SAVE META DATAFRAME
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/Processed_data/"
setwd(paste0(start_path,"Predictions"))
meta <- df
save(meta, file="pred_meta.RDATA")


######################################################################################################## MERGE META AND ACCELEROMETER DATA
#######################################################
### STEP 2 - MERGE META DATA AND ACCELEROMETER DATA ###
#######################################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/Processed_data/"
setwd(paste0(start_path,"Predictions"))
load("pred_meta.RDATA")

start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))
load("accel_data.RDATA")

### SET TIMEZONE TO UTC
tz(meta$Ob_Start)
tz(meta$Ob_End)
tz(longaccl$Timestamp)
tz(meta$Ob_Start) <- "UTC"
tz(meta$Ob_End) <- "UTC"

### MERGE META DATA WITH ACCELEROMETER DATA
dfAcclMeta <- longaccl %>% left_join(meta, by = 'Cat_id')
head(dfAcclMeta)
tail(dfAcclMeta)
str(dfAcclMeta)

### SELECT ACCELERATION DATA THAT FALL BETWEEN START AND END TIME
df <- dfAcclMeta %>% group_by(Cat_id) %>%
  filter(Timestamp >= Ob_Start & Timestamp <= Ob_End)
head(df)
tail(df)
str(df)
dim(df)

### REMOVE START AND END TIME COLUMS
df <- df %>% select(-Ob_Start,-Ob_End)

### SAVE ACCLMETA DATAFRAME
dfAcclMeta <- df
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
save(dfAcclMeta, file= "Pred_dataset.RDATA")

###################################################################################################################### DATA SPLITING
###############################
### STEP 3 - DATA SPLITTING ###
###############################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
load("Pred_dataset.RDATA")

### SPLIT COLLAR AND HARNESS DATA
pred.collar <- dfAcclMeta %>% filter(Position == "Collar")
pred.harness <- dfAcclMeta %>% filter(Position == "Harness")

### REMOVE POSITION & RENAME ODBA
pred.collar <- pred.collar %>% select (-Position, -ODBA15_Mean,-ODBA45_Mean, -ODBA60_Mean, -ODBA90_Mean)
pred.collar <- rename (pred.collar, ODBA=ODBA30_Mean)
pred.harness <- pred.harness %>% select (-Position, -ODBA15_Mean,-ODBA45_Mean, -ODBA60_Mean, -ODBA90_Mean)
pred.harness <- rename (pred.harness, ODBA=ODBA30_Mean)


### SAFE DATAFRAMES
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
save(pred.collar, file="predict.collar.RDATA")
save(pred.harness, file="predict.harness.RDATA")

################################################################################################################### BEHAVIOR PREDICTION
##########################################
### STEP 4 - BEHAVIOUR PREDICTION - RF ###
##########################################

############################################################################################## RF COLLAR
####################################
### RANDOM FOREST MODEL - COLLAR ###
####################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
load("predict.collar.RDATA")

start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))

### Behaviour 1
load("Model_RF_collar1.RDATA")
pred.collar$C.RF1 <- predict(model.collar,pred.collar,type="raw")
rm(model.collar)

### Behaviour 2
load("Model_RF_collar2.RDATA")
pred.collar$C.RF2 <- predict(model.collar,pred.collar,type="raw")

### Behaviour 3
load("Model_RF_collar3.RDATA")
pred.collar$C.RF3 <- predict(model.collar,pred.collar,type="raw")

### Behaviour 4
load("Model_RF_collar4.RDATA")
pred.collar$C.RF4 <- predict(model.collar,pred.collar,type="raw")

### Behaviour 5
load("Model_RF_collar5.RDATA")
pred.collar$C.RF5 <- predict(model.collar,pred.collar,type="raw")

### SAFE PRED.COLLAR
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
save(pred.collar, file="predict.collar.RDATA")

rm(list=ls())

############################################################################################# SOM COLLAR
#####################################
### Self Organizing Maps - COLLAR ###
#####################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
load("predict.collar.RDATA")

start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))

### Behaviour 1
load("Model_SOM_collar1.RDATA")
dat <- pred.collar[,3:34]

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.collar$C.SOM1 <- ssom.pred$predictions$Activity
rm(ssom)


### Behaviour 2
load("Model_SOM_collar2.RDATA")
dat <- pred.collar[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.collar$C.SOM2 <- ssom.pred$predictions$Activity
rm(ssom)

### Behaviour 3
load("Model_SOM_collar3.RDATA")
dat <- pred.collar[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.collar$C.SOM3 <- ssom.pred$predictions$Activity
rm(ssom)

### Behaviour 4
load("Model_SOM_collar4.RDATA")
dat <- pred.collar[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.collar$C.SOM4 <- ssom.pred$predictions$Activity

### Behaviour 5
load("Model_SOM_collar5.RDATA")
dat <- pred.collar[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.collar$C.SOM5 <- ssom.pred$predictions$Activity


### SAFE PRED.COLLAR
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
save(pred.collar, file="predict.collar.RDATA")

rm(list=ls())

############################################################################################## RF HARNESS
#####################################
### RANDOM FOREST MODEL - HARNESS ###
#####################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
load("predict.harness.RDATA")

start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))

### Behaviour 1
load("Model_RF_harness1.RDATA")
pred.harness$H.RF1 <- predict(model.harness,pred.harness,type="raw")

### Behaviour 2
load("Model_RF_harness2.RDATA")
pred.harness$H.RF2 <- predict(model.harness,pred.harness,type="raw")

### Behaviour 3
load("Model_RF_harness3.RDATA")
pred.harness$H.RF3 <- predict(model.harness,pred.harness,type="raw")

### Behaviour 4
load("Model_RF_harness4.RDATA")
pred.harness$H.RF4 <- predict(model.harness,pred.harness,type="raw")

### Behaviour 5
load("Model_RF_harness5.RDATA")
pred.harness$H.RF5 <- predict(model.harness,pred.harness,type="raw")

### SAFE PRED.HARNESS
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
save(pred.harness, file="predict.harness.RDATA")

rm(list=ls())

############################################################################################# SOM HARNESS
#####################################
### Self Organizing Maps - HARNESS ###
#####################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
load("predict.harness.RDATA") #Load dataset containing predictor variables Harness

start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))

### Behaviour 1
load("Model_SOM_harness1.RDATA")
dat <- pred.harness[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.harness$H.SOM1 <- ssom.pred$predictions$Activity
rm(ssom)


### Behaviour 2
load("Model_SOM_harness2.RDATA")
dat <- pred.harness[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.harness$H.SOM2 <- ssom.pred$predictions$Activity
rm(ssom)

### Behaviour 3
load("Model_SOM_harness3.RDATA")
dat <- pred.harness[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.harness$H.SOM3 <- ssom.pred$predictions$Activity
rm(ssom)

### Behaviour 4
load("Model_SOM_harness4.RDATA")
dat <- pred.harness[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.harness$H.SOM4 <- ssom.pred$predictions$Activity
rm(ssom)

### Behaviour 5
load("Model_SOM_harness5.RDATA")
dat <- pred.harness[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.harness$H.SOM5 <- ssom.pred$predictions$Activity
rm(ssom)

### SAFE PRED.HARNESS
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
save(pred.harness, file="predict.harness.RDATA")

############################################
### STEP 4 - BEHAVIOUR PREDICTION - SOMS ###
############################################
#####################################
### Self Organizing Maps - COLLAR ###
#####################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
load("predict.collar.RDATA")

start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Model_SOM"))

### Behaviour 2
load("Model_SOM_collar2.RDATA")
dat <- pred.collar[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.collar$C.SOM6 <- ssom.pred$predictions$Activity

### SAFE PRED.COLLAR
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
save(pred.collar, file="predict.collar.RDATA")

rm(list=ls())

############################################################################################## RF HARNESS
#####################################
### Self Organizing Maps - HARNESS ###
#####################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
load("predict.harness.RDATA") #Load dataset containing predictor variables Harness

start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Model_SOM"))

### Behaviour 2
load("Model_SOM_harness2.RDATA")
dat <- pred.harness[,3:34] # Change

trSamp <- function(x){
  d <- dat %>% select(X_Mean:Cor_YZ) # predictor variable range
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.harness$H.SOM6 <- ssom.pred$predictions$Activity

### SAFE PRED.HARNESS
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Predictions"))
save(pred.harness, file="predict.harness.RDATA")

rm(list=ls())