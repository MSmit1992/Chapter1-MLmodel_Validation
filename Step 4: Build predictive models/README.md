This folder contains:
- R Code for Behaviour selection (Select.behaviour.R) - Step 1-4
- R Code for Random forest models (RF.model.R) - Step 5.1
- R Code for Self-Organizing Maps models (SOMs.model.R) - Step 5.1

# Prepare datasets
In step 3, all data was merged into a big dataset. The next step is to prepare the data and create dataframes that can be used to build the models.

# Steps
Steps 2 to 5 are repeated multiple times (see step 2 on merging behaviours for more information)
## (1) Remove behaviours not observed
Not all behaviours were observed in the videorecordings. Behaviours that were not observed and where cats were out of sight, were removed from the dataset. 
## (2) Merge behaviours
Multiple predictive models were built for both the collar and harness mounted ActiGraph. For each model, the overall accuracy and estimated Kappa’s coefficient were evaluated and for each behaviour the sensitivity, specificity, precision and accuracy were evaluated. Behaviours with a small amount of datapoints, were removed from or clustered with a behaviour as it was often misclassified as. A new model was then built, and the overall accuracy and estimated Kappa’s coefficient were compared to the previous model. Behaviours were removed or clustered until the overall accuracy and Kappa’s coefficient no longer improved.

In this step you see all the behaviour selections that occured.
## (3) Data splitting 1
Each cat wore two accelerometers: one attached to a collar and one attached to a harness. Here we split the complete dataframe in two dataframes:
1. Collar dataframe, containing the predictor variables from the collar-mounted accelerometer
2. Harness dataframe, containg the predictor variables from the harness-mounted accelerometer
## (4) Create sub dataframes
For every behaviour group (explained in step 2), a seperate dataframe was created to be able to easily build the models
Abundant columns were removed.
## (5) Data splitting 2
To build and assess the performance of a model, the dataset was divided into a training dataset to build the model, and a test dataset to test the model.
## (6) Model building
Using the training and test datasets, the model is built.
## (7) Model evaluation
A confusion matrix was made to be able to describe the performance of the predictive model
