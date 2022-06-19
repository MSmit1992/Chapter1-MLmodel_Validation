##########################################
### CAT BEHAVIOUR PROJECT              ###
### ActiGraph validation behaviour ID  ###
### Self Organizing Maps (SOM)         ###
##########################################
# rm(list=ls())

### BASIC INFORMATION
# SOM is a type of Artificial Neural Network (ANN)
# Can be trained supervised and unsupervised (with already labelled data = supervised)


### READ R LIBRARIES
library(tidyverse);library(kohonen);library(data.table);library(caret);library(inlmisc)

### ################################################################################################################ DATA PREPARATION
########################
### DATA PREPARATION ###
########################
# Required input: - Collar dataset: Collar_B2 & Collar_B3 & Collar_B4
#                 - Harness dataset: Harness_B2 & Harness_B3 & Harness_B4
start_path <- "/Volumes/One Touch/PhD_Massey_Study1/Study1_072021_DataAnalysisR/Data/behaviour_data/"
start_path <- "C:/Users/200179481/OneDrive - Massey University/2.2 Study 1 - Validation ActiGraph/Data/MichelleSmit/Data/behaviour_data/"
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



##################################################################################################################### DATA SPLITTING
#####################################################################
### Step 5 - DATA SPLITTING - CREATE TRAINING AND TESTING DATESET ###
#####################################################################

### COLLAR
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 3:37]
  act <- as.factor(x$Behaviour7[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(Collar_B7$Behaviour7, p = 0.70, list = FALSE)
df.train <- trsamp(Collar_B7, n=ind)
df.test <- trsamp(Collar_B7, n=-ind)

### HARNESS
trsamp <- function(x, n=1:nrow(x)) {
  d <- x[n, 3:37]
  act <- as.factor(x$Behaviour7[n])
  out <- list(measurements = as.matrix(d),Activity=act)
  return(out)
}
ind <- createDataPartition(Harness_B7$Behaviour7, p = 0.70, list = FALSE)
df.train <- trsamp(Harness_B7, n=ind)
df.test <- trsamp(Harness_B7, n=-ind)


###################################################################################################################### MODEL BUILDING
###############################
### STEP 6 - MODEL BUILDING ###
###############################
# Required input: - Full dataframes collar & harness 
#                 - Splitted dataframes collar: df.collartrain & df.collartest
#                 - Splitted dataframes harness: df.harnesstrain & df.harnesstest
# Hexagonal:  - Behaviour 1 has 21 behaviours, 21x4 = 84 (10x9)
#             - Behaviour 2 has 15 behaviours, 15x4 = 60 (8x8)
#             - Behaviour 3 has 11 behaviours, 11x4 = 44 (7x7)
#             - Behaviour 4 has 10 behaviours, 10x4 = 50 (7x6)
#             - Behaviour 5 has 9 behaviours, 9x4 = 36 (6x6)
#             - Behaviour 6 has 4 behaviours, 4x4 = 16 (4x4)

# Behaviour 1
ssom <- supersom(df.train,grid=somgrid(10,9,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_collar1.rds")
save(ssom, file="Model_SOM_collar1.RDATA")

ssom <- supersom(df.train,grid=somgrid(10,9,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_harness1.rds")
save(ssom, file="Model_SOM_harness1.RDATA")

# Behaviour 2
ssom <- supersom(df.train,grid=somgrid(8,8,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_collar2.rds")
save(ssom, file="Model_SOM_collar2.RDATA")

ssom <- supersom(df.train,grid=somgrid(8,8,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_harness2.rds")
save(ssom, file="Model_SOM_harness2.RDATA")

# Behaviour 3
ssom <- supersom(df.train,grid=somgrid(7,7,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_collar3.rds")
save(ssom, file="Model_SOM_collar3.RDATA")

ssom <- supersom(df.train,grid=somgrid(7,7,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_harness3.rds")
save(ssom, file="Model_SOM_harness3.RDATA")

# Behaviour 4
ssom <- supersom(df.train,grid=somgrid(7,6,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_collar4.rds")
save(ssom, file="Model_SOM_collar4.RDATA")

ssom <- supersom(df.train,grid=somgrid(7,6,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_harness4.rds")
save(ssom, file="Model_SOM_harness4.RDATA")

# Behaviour 5
ssom <- supersom(df.train,grid=somgrid(6,6,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_collar5.rds")
save(ssom, file="Model_SOM_collar5.RDATA")

ssom <- supersom(df.train,grid=somgrid(6,6,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_harness5.rds")
save(ssom, file="Model_SOM_harness5.RDATA")

# Behaviour 6
ssom <- supersom(df.train,grid=somgrid(4,4,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_collar6.rds")
save(ssom, file="Model_SOM_collar6.RDATA")

ssom <- supersom(df.train,grid=somgrid(4,4,"hexagonal"))
ssom.pred <- predict(ssom, df.test)
save(ssom, file="Model_SOM_harness6.rds")
save(ssom, file="Model_SOM_harness6.RDATA")


################################################################################################################### MODEL EVAULATION
#################################
### Step 7 - MODEL EVALUATION ###
#################################

### CONFUSION MATRIX
# Behaviour 1
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour1 = df.test$Activity)
ptab

# Behaviour 2
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour2 = df.test$Activity)
ptab

# Behaviour 3
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour3 = df.test$Activity)
ptab

# Behaviour 4
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour4 = df.test$Activity)
ptab

# Behaviour 5
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour5 = df.test$Activity)
ptab

# Behaviour 6
ptab <- table(predictions = ssom.pred$predictions$Activity, Behaviour6 = df.test$Activity)
ptab


### SENSITIVITY, PRECISION, SPECIFICITY, ACCURACY
# Sensitivity = how often the behaviour is correctly identified by the model
# Precision = How often a model is right when it predicts a behaviour
# Specificity = How often the absence of the behaviour is correctly identified by the model
# Accuracy = How often the model is right
true_positives  <- diag(ptab)
false_positives <- rowSums(ptab) - true_positives
false_negatives <- colSums(ptab) - true_positives
true_negatives  <- sum(ptab) - true_positives - false_positives - false_negatives

SENS<-c(true_positives/(true_positives+false_negatives))
PREC<-c(true_positives/(true_positives+false_positives))
SPEC<-c(true_negatives/(true_negatives+false_positives))
ACCU<-c((true_positives+true_negatives)/(true_positives+true_negatives+false_positives+false_negatives))

SENS
PREC
SPEC
ACCU

######################################################################################################################### VIZUALIZING
###################
### VISUALIZING ###
###################
# Figure (codes): further apart, the more different, the closer the together, the more similar the behaviours are
# Figure (codes): Size of triangle/arrow indicates how often a particular behaviour is represented by that element
# Figure (codes): Full triangle of one colour: determined with an overall 100% accuracy, large sample size behaviours
# Figure (codes): Clustered shapes of different colours: False positive or false negative, small sample size behaviours
plot(ssom, type="codes", codeRendering = "segments", shape="straight")
som.hc <- cutree(hclust(object.distances(ssom,"codes")),15)
add.cluster.boundaries(ssom, som.hc) 

plot(ssom, type="mapping", shape="straight") #The lower the number of empty cells, the better the SOM
plot
plot(ssom, type="count", shape="straight") #Number of observations assigned to node, grey = no observations
plot


# Heat maps, shows us the values of the specific predictor for the specific node. Blue is lower values of predictor, red is higher values of predictors
colnames(getCodes(ssom,1))
cols_out <- inlmisc::GetColors(35, scheme="sunset")
cols_out[1:35] # 11 = number of colnames
yb<-colorRampPalette(cols_out[1:35])
     

# Number between [] is the number of the specific classifier (gotten from colnames)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,1], main=colnames(getCodes(ssom,1))[1], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,2], main=colnames(getCodes(ssom,1))[2], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,3], main=colnames(getCodes(ssom,1))[3], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,4], main=colnames(getCodes(ssom,1))[4], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,5], main=colnames(getCodes(ssom,1))[5], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,6], main=colnames(getCodes(ssom,1))[6], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,7], main=colnames(getCodes(ssom,1))[7], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,8], main=colnames(getCodes(ssom,1))[8], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,9], main=colnames(getCodes(ssom,1))[9], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,10], main=colnames(getCodes(ssom,1))[10], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,11], main=colnames(getCodes(ssom,1))[11], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,12], main=colnames(getCodes(ssom,1))[12], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,13], main=colnames(getCodes(ssom,1))[13], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,14], main=colnames(getCodes(ssom,1))[14], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,15], main=colnames(getCodes(ssom,1))[15], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,16], main=colnames(getCodes(ssom,1))[16], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,17], main=colnames(getCodes(ssom,1))[17], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,18], main=colnames(getCodes(ssom,1))[18], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,19], main=colnames(getCodes(ssom,1))[19], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,20], main=colnames(getCodes(ssom,1))[20], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,21], main=colnames(getCodes(ssom,1))[21], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,22], main=colnames(getCodes(ssom,1))[22], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,23], main=colnames(getCodes(ssom,1))[23], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,24], main=colnames(getCodes(ssom,1))[24], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,25], main=colnames(getCodes(ssom,1))[25], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,26], main=colnames(getCodes(ssom,1))[26], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,27], main=colnames(getCodes(ssom,1))[27], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,28], main=colnames(getCodes(ssom,1))[28], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,29], main=colnames(getCodes(ssom,1))[29], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,30], main=colnames(getCodes(ssom,1))[30], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,31], main=colnames(getCodes(ssom,1))[31], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,32], main=colnames(getCodes(ssom,1))[32], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,33], main=colnames(getCodes(ssom,1))[33], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,34], main=colnames(getCodes(ssom,1))[34], palette.name=yb)
plot(ssom, shape='straight',type="property", property=getCodes(ssom,1)[,35], main=colnames(getCodes(ssom,1))[35], palette.name=yb)


