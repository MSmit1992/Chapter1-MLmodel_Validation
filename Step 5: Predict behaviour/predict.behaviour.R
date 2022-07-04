##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Prepare datasets                   ###
##########################################

### READ R LIBRARIES
library(plyr);library(dplyr);library(lubridate);library(readxl)
library(tidyverse); library(caret); library(randomForest); library(kohonen)


################################################################################################################# PREPARATION META DATA
#######################################
### STEP 1 -  PREPARATION META DATA ###
#######################################
# Required input:   - Meta data
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"meta_data"))
meta <- read.csv("Predict_Meta.csv")
head(meta)
str(meta)

### ADD DATE TO COLUMNS OB_START AND OB_END
df <- meta
df <- df %>%
  mutate(Ob_Start = paste("2021/06/30",df$Ob_Start, sep=" "),Ob_End = paste("2021/07/07",df$Ob_End, sep=" "))

### CHANGE VARIABLE TYPES
df <- meta
# Change Cat_id from chr to factor
df$Cat_id <- as.factor(df$Cat_id)
# Change Ob_Start and Ob_End from chr to POSIXct
df$Ob_Start<-as.POSIXct(df$Ob_Start,format="%Y/%m/%d %H:%M:%S")
df$Ob_End<-as.POSIXct(df$Ob_End,format="%Y/%m/%d %H:%M:%S")

start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
meta <- df
save(meta, file="meta_data.RDATA")

### SAVE META DATAFRAME
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
meta <- df
save(meta, file="pred_meta.RDATA")


######################################################################################################## MERGE META AND ACCELEROMETER DATA
#######################################################
### STEP 2 - MERGE META DATA AND ACCELEROMETER DATA ###
#######################################################
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
load("pred_meta.RDATA")
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
save(dfAcclMeta, file= "Pred_dataset.RDATA")


###################################################################################################################### DATA SPLITING
###############################
### STEP 3 - DATA SPLITTING ###
###############################
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
load("Pred_dataset.RDATA")

### SPLIT COLLAR AND HARNESS DATA
pred.collar <- dfAcclMeta %>% filter(Position == "Collar")
pred.harness <- dfAcclMeta %>% filter(Position == "Harness")

start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))
save(pred.collar, file="predict.collar.RDATA")
save(pred.harness, file="predict.harness.RDATA")

################################################################################################################### BEHAVIOR PREDICTION
#####################################
### STEP 4 - BEHAVIOUR PREDICTION ###
#####################################

############################################################################################## RANDOM FOREST MODEL
###########################
### RANDOM FOREST MODEL ###
###########################
start_path <- "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/"
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))

### COLLAR
load("predict.collar.RDATA") #Load dataset containing predictor variables
load("Model_RF_collar5.RDATA")#Load predictive model

pred.collar$predRF <- predict(model.collar,pred.collar,type="raw")
save(pred.collar, file="predict.collar.RDATA")


### HARNESS
load("predict.harness.RDATA") #Load datast containing predictor variables
load("Model_RF_harness5.RDATA") #Load predictive model

pred.harness$predRF <- predict(model.harness,pred.harness,type="raw")
save(pred.harness, file="predict.collar.RDATA")


############################################################################################## SOMs MODEL
##################
### SOMs MODEL ###
##################
start_path <- "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/"
start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))

### COLLAR
load("Model_SOM_collar5.RDATA") #Load predictive model
plot(ssom)

load("predict.collar.RDATA") #Load datast containing predictor variables

dat <- pred.collar[,4:38]

trSamp <- function(x){
   d <- dat %>% select(X_Mean:ODBA)
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.collar$predSOM <- ssom.pred$predictions$Activity

save(pred.collar, file="predict.collar.RDATA")

### HARNESS
load("Model_SOM_harness5.RDATA") #Load predictive model
plot(ssom)

load("predict.collar.RDATA") #Load datast containing predictor variables

dat <- pred.harness[,4:38]

trSamp <- function(x){
  d <- dat %>% select(X_Mean:ODBA)
  act <- as.factor(x$Activity)
  out <- list(measurements = as.matrix(d))
  return(out)
}

dat2 <- trSamp(dat)

ssom.pred <- predict(ssom, newdata=dat2$measurements,whatmap=1)
pred.harness$predSOM <- ssom.pred$predictions$Activity

save(pred.collar, file="predict.harness.RDATA")

