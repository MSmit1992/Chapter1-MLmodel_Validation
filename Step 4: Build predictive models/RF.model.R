##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Prepare datasets                   ###
##########################################

### READ R LIBRARIES
library(tidyverse); library(qwraps2); library(caret); library(randomForest); library(mlbench); library(psych); library(plot3D)


####################################################################################################################### DATA SPLITTING
#####################################################################
### Step 5 - DATA SPLITTING - CREATE TRAINING AND TESTING DATESET ###
#####################################################################
# Required input: - Collar datasets
#                 - Harness datasets

start_path <- "C:/Users/20017948/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
setwd(paste0(start_path,"processed_data"))

load("Collar_B1.RDATA")
load("Collar_B2.RDATA")
load("Collar_B3.RDATA")
load("Collar_B4.RDATA")
load("Collar_B5.RDATA")
load("Collar_B6.RDATA")

load("Harness_B1.RDATA")
load("Harness_B2.RDATA")
load("Harness_B3.RDATA")
load("Harness_B4.RDATA")
load("Harness_B5.RDATA")
load("Harness_B6.RDATA")

### COLLAR
index.C <- createDataPartition(Collar_B1$Behaviour1, p = 0.70, list = FALSE)
df.collartrain <- Collar_B1[index.C,]
df.collartest <- Collar_B1[-index.C,]
str(df.collartrain)

index.C <- createDataPartition(Collar_B2$Behaviour2, p = 0.70, list = FALSE)
df.collartrain <- Collar_B2[index.C,]
df.collartest <- Collar_B2[-index.C,]
str(df.collartrain)

index.C <- createDataPartition(Collar_B3$Behaviour3, p = 0.70, list = FALSE)
df.collartrain <- Collar_B3[index.C,]
df.collartest <- Collar_B3[-index.C,]
str(df.collartrain)

index.C <- createDataPartition(Collar_B4$Behaviour4, p = 0.70, list = FALSE)
df.collartrain <- Collar_B4[index.C,]
df.collartest <- Collar_B4[-index.C,]
str(df.collartrain)

index.C <- createDataPartition(Collar_B5$Behaviour5, p = 0.70, list = FALSE)
df.collartrain <- Collar_B5[index.C,]
df.collartest <- Collar_B5[-index.C,]
str(df.collartrain)

index.C <- createDataPartition(Collar_B6$Behaviour6, p = 0.70, list = FALSE)
df.collartrain <- Collar_B6[index.C,]
df.collartest <- Collar_B6[-index.C,]
str(df.collartrain)


### HARNESS
index.C <- createDataPartition(Harness_B1$Behaviour1, p = 0.70, list = FALSE)
df.harnesstrain <- Harness_B1[index.C,]
df.harnesstest <- Harness_B1[-index.C,]
str(df.harnesstrain)

index.C <- createDataPartition(Harness_B2$Behaviour2, p = 0.70, list = FALSE)
df.harnesstrain <- Harness_B2[index.C,]
df.harnesstest <- Harness_B2[-index.C,]
str(df.harnesstrain)

index.C <- createDataPartition(Harness_B3$Behaviour3, p = 0.70, list = FALSE)
df.harnesstrain <- Harness_B3[index.C,]
df.harnesstest <- Harness_B3[-index.C,]
str(df.harnesstrain)

index.C <- createDataPartition(Harness_B4$Behaviour4, p = 0.70, list = FALSE)
df.harnesstrain <- Harness_B4[index.C,]
df.harnesstest <- Harness_B4[-index.C,]
str(df.harnesstrain)

index.C <- createDataPartition(Harness_B5$Behaviour5, p = 0.70, list = FALSE)
df.harnesstrain <- Harness_B5[index.C,]
df.harnesstest <- Harness_B5[-index.C,]
str(df.harnesstrain)

index.C <- createDataPartition(Harness_B6$Behaviour6, p = 0.70, list = FALSE)
df.harnesstrain <- Harness_B6[index.C,]
df.harnesstest <- Harness_B6[-index.C,]
str(df.harnesstrain)


####################################################################################################################### MODEL BUILDING
###############################
### STEP 6 - MODEL BUILDING ###
###############################

### COLLAR
model.collar <- train(Behaviour1 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)
save(model.collar, file="Model_RF_collar1.rds")
save(model.collar, file="Model_RF_collar1.RDATA")

model.collar <- train(Behaviour2 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)
save(model.collar, file="Model_RF_collar2.rds")
save(model.collar, file="Model_RF_collar2.RDATA")

model.collar <- train(Behaviour3 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)
save(model.collar, file="Model_RF_collar3.rds")
save(model.collar, file="Model_RF_collar3.RDATA")

model.collar <- train(Behaviour4 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)
save(model.collar, file="Model_RF_collar4.rds")
save(model.collar, file="Model_RF_collar4.RDATA")

model.collar <- train(Behaviour5 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)
save(model.collar, file="Model_RF_collar5.rds")
save(model.collar, file="Model_RF_collar5.RDATA")

model.collar <- train(Behaviour6 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                        Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                        VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                      data=df.collartrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.collar,df.collartest)
save(model.collar, file="Model_RF_collar6.rds")
save(model.collar, file="Model_RF_collar6.RDATA")


### HARNESS
# No pre-processing
model.harness <- train(Behaviour1 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)
save(model.harness, file="Model_RF_harness1.rds")
save(model.harness, file="Model_RF_harness1.RDATA")

model.harness <- train(Behaviour2 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)
save(model.harness, file="Model_RF_harness2.rds")
save(model.harness, file="Model_RF_harness2.RDATA")

model.harness <- train(Behaviour3 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)
save(model.harness, file="Model_RF_harness3.rds")
save(model.harness, file="Model_RF_harness3.RDATA")

model.harness <- train(Behaviour4 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)
save(model.harness, file="Model_RF_harness4.rds")
save(model.harness, file="Model_RF_harness4.RDATA")

model.harness <- train(Behaviour5 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
save(model.harness, file="Model_RF_harness5.rds")
save(model.harness, file="Model_RF_harness5.RDATA")

model.harness <- train(Behaviour6 ~ X_Mean + X_Min + X_Max + X_Sum + X_sd + X_Skew + X_Kurt + Y_Mean + Y_Min + Y_Max + Y_Sum + Y_sd + 
                         Y_Skew + Y_Kurt + Z_Mean + Z_Min + Z_Max + Z_Sum + Z_sd + Z_Skew + Z_Kurt + VM_Mean + VM_Min + VM_Max + 
                         VM_Sum + VM_sd + VM_Skew + VM_Kurt + Cor_XY + Cor_XZ + Cor_YZ + X_DBA + Y_DBA + Z_DBA + ODBA,
                       data=df.harnesstrain, method="rf", trControl=trainControl(method ="none"))
preds <- predict(model.harness,df.harnesstest)
save(model.harness, file="Model_RF_harness6.rds")
save(model.harness, file="Model_RF_harness6.RDATA")


##################################################################################################################### MODEL EVAULATION
#################################
### STEP 7 - MODEL EVALUATION ###
#################################

### PREDICTOR IMPORTANCE
Importance <- varImp(model.collar, scale = FALSE)
Importance <- varImp(model.collar, scale = TRUE)
print(Importance)
plot(Importance)

Importance <- varImp(model.harness, scale = FALSE)
print(Importance)
plot(Importance)

### COLLAR CONFUSION MATRIX
confusionMatrix(preds, df.collartest$Behaviour1)
confusionMatrix(preds, df.collartest$Behaviour2)
confusionMatrix(preds, df.collartest$Behaviour3)
confusionMatrix(preds, df.collartest$Behaviour4)
confusionMatrix(preds, df.collartest$Behaviour5)
confusionMatrix(preds, df.collartest$Behaviour6)

### HARNESS CONFUSION MATRIX
confusionMatrix(preds, df.harnesstest$Behaviour1)
confusionMatrix(preds, df.harnesstest$Behaviour2)
confusionMatrix(preds, df.harnesstest$Behaviour3)
confusionMatrix(preds, df.harnesstest$Behaviour4)
confusionMatrix(preds, df.harnesstest$Behaviour5)
confusionMatrix(preds, df.harnesstest$Behaviour6)




