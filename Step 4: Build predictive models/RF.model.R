##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Random Forest models               ###
##########################################

### READ R LIBRARIES
library(tidyverse); library(qwraps2); library(caret); library(randomForest); library(mlbench); library(psych); library(plot3D)


####################################################################################################################### MODEL BUILDING
#################################
### STEP 5.1 - MODEL BUILDING ###
#################################
# Required input: - Collar datasets
#                 - Harness datasets
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Preparation"))

###################
### BEHAVIOUR 1 ###
###################

### COLLAR
load("Collar_B1.RDATA")

index.C <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.collartrain <- df.collar[index.C,]
df.collartest <- df.collar[-index.C,]
str(df.collartrain)

model.collar <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)

# Confusion matrix
confusionMatrix(preds, df.collartest$Behaviour)

# Model evaluation
Importance <- varImp(model.collar, scale = FALSE)
print(Importance)
plot(Importance)

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.collar, file="Model_RF_collar1.rds")
save(model.collar, file="Model_RF_collar1.RDATA")


### HARNESS
load("Harness_B1.RDATA")

index.C <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.harnesstrain <- df.harness[index.C,]
df.harnesstest <- df.harness[-index.C,]
str(df.harnesstrain)

model.harness <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)

# Confusion matrix
confusionMatrix(preds, df.harnesstest$Behaviour)

# Model evaluation
Importance <- varImp(model.harness, scale = FALSE)
print(Importance)
plot(Importance)

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.harness, file="Model_RF_harness1.rds")
save(model.harness, file="Model_RF_harness1.RDATA")


###################
### BEHAVIOUR 2 ###
###################

### COLLAR
load("Collar_B2.RDATA")

index.C <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.collartrain <- df.collar[index.C,]
df.collartest <- df.collar[-index.C,]
str(df.collartrain)

model.collar <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)

# Confusion matrix
confusionMatrix(preds, df.collartest$Behaviour)

# Model evaluation
Importance <- varImp(model.collar, scale = FALSE)
print(Importance)
plot(Importance)

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.collar, file="Model_RF_collar2.rds")
save(model.collar, file="Model_RF_collar2.RDATA")


### HARNESS
load("Harness_B2_RF.RDATA")

index.C <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.harnesstrain <- df.harness[index.C,]
df.harnesstest <- df.harness[-index.C,]
str(df.harnesstrain)

model.harness <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)

# Confusion matrix
confusionMatrix(preds, df.harnesstest$Behaviour)

# Model evaluation
Importance <- varImp(model.harness, scale = FALSE)
print(Importance)
plot(Importance)

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.harness, file="Model_RF_harness2.rds")
save(model.harness, file="Model_RF_harness2.RDATA")


##################################################################################################################### CROSS VALIDATION
###################
### BEHAVIOUR 3 ###
###################

### COLLAR
load("Collar_B3.RDATA")

index.C <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.collartrain <- df.collar[index.C,]
df.collartest <- df.collar[-index.C,]
str(df.collartrain)

model.collar <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)

# Confusion matrix
confusionMatrix(preds, df.collartest$Behaviour)

# Model evaluation
Importance <- varImp(model.collar, scale = FALSE)
print(Importance)
plot(Importance)

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.collar, file="Model_RF_collar3.rds")
save(model.collar, file="Model_RF_collar3.RDATA")


### HARNESS
load("Harness_B3_RF.RDATA")

index.C <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.harnesstrain <- df.harness[index.C,]
df.harnesstest <- df.harness[-index.C,]
str(df.harnesstrain)

model.harness <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)

# Confusion matrix
confusionMatrix(preds, df.harnesstest$Behaviour)

# Model evaluation
Importance <- varImp(model.harness, scale = FALSE)
print(Importance)
plot(Importance)

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.harness, file="Model_RF_harness3.rds")
save(model.harness, file="Model_RF_harness3.RDATA")


###################
### BEHAVIOUR 4 ###
###################

### COLLAR
load("Collar_B4.RDATA")

index.C <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.collartrain <- df.collar[index.C,]
df.collartest <- df.collar[-index.C,]
str(df.collartrain)

model.collar <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)

# Confusion matrix
confusionMatrix(preds, df.collartest$Behaviour)

# Model evaluation
Importance <- varImp(model.collar, scale = FALSE)
print(Importance)
plot(Importance)

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.collar, file="Model_RF_collar4.rds")
save(model.collar, file="Model_RF_collar4.RDATA")


### HARNESS
load("Harness_B4_RF.RDATA")

index.C <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.harnesstrain <- df.harness[index.C,]
df.harnesstest <- df.harness[-index.C,]
str(df.harnesstrain)

model.harness <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)

# Confusion matrix
confusionMatrix(preds, df.harnesstest$Behaviour)

# Model evaluation
Importance <- varImp(model.harness, scale = FALSE)
print(Importance)
plot(Importance)

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.harness, file="Model_RF_harness4.rds")
save(model.harness, file="Model_RF_harness4.RDATA")


###################
### BEHAVIOUR 5 ###
###################

### COLLAR
load("Collar_B5.RDATA")

index.C <- createDataPartition(df.collar$Behaviour, p = 0.70, list = FALSE)
df.collartrain <- df.collar[index.C,]
df.collartest <- df.collar[-index.C,]
str(df.collartrain)

model.collar <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)

# Confusion matrix
confusionMatrix(preds, df.collartest$Behaviour)

# Model evaluation
Importance <- varImp(model.collar, scale = FALSE)
print(Importance)
plot(Importance)

# Safe model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.collar, file="Model_RF_collar5.rds")
save(model.collar, file="Model_RF_collar5.RDATA")


### HARNESS
load("Harness_B5.RDATA")

index.C <- createDataPartition(df.harness$Behaviour, p = 0.70, list = FALSE)
df.harnesstrain <- df.harness[index.C,]
df.harnesstest <- df.harness[-index.C,]
str(df.harnesstrain)

model.harness <- train(Behaviour ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)

# Confusion matrix
confusionMatrix(preds, df.harnesstest$Behaviour)

# Model evaluation
Importance <- varImp(model.harness, scale = FALSE)
print(Importance)
plot(Importance)

# Safe Model
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))
save(model.harness, file="Model_RF_harness5.rds")
save(model.harness, file="Model_RF_harness5.RDATA")



#################################
### STEP 6 - CROSS VALIDATION ###
#################################
start_path <- "C:/Users/msmit1/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/processed_data/"
setwd(paste0(start_path,"Models"))

### COLLAR
load("Collar_B1.RDATA")

Cat <- unique(Collar_B6$Cat_id)
accuracy <- c()
error.rate <- c()

for(i in Cat) {
  train <- Collar_B6[!Collar_B6$Cat_id==i,]
  train_rf <- train(Behaviour6 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                      Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                      VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                    data=train, method="rf", trControl=trainControl(method ="none"))
  test <- Collar_B6[Collar_B6$Cat_id==i,]
  pred <- predict(train_rf,newdata=test)
  result<-print(train_rf)
  tmp<-data.frame(truth=test$Behaviour6, pred = pred)
  accuracy<-c(accuracy,sum(tmp$truth == tmp$pred)/nrow(tmp))
  error.rate<-c(error.rate,sum(!tmp$truth == tmp$pred)/nrow(tmp))
}

round(accuracy,4)
round(error.rate,4)

### HARNESS
load("Harness_B1.RDATA")

Cat <- unique(Harness_B6$Cat_id)
accuracy <- c()
error.rate <- c()

for(i in Cat) {
  train <- Harness_B6[!Harness_B6$Cat_id==i,]
  train_rf <- train(Behaviour6 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                      Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                      VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                    data=train, method="rf", trControl=trainControl(method ="none"))
  test <- Harness_B6[Harness_B6$Cat_id==i,]
  pred <- predict(train_rf,newdata=test)
  result<-print(train_rf)
  tmp<-data.frame(truth=test$Behaviour6, pred = pred)
  accuracy<-c(accuracy,sum(tmp$truth == tmp$pred)/nrow(tmp))
  error.rate<-c(error.rate,sum(!tmp$truth == tmp$pred)/nrow(tmp))
}

round(accuracy,4)
