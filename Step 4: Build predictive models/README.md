This folder contains:
- R Code for Behaviour selection (Select.behaviour.R)
- R Code for Random forest models (predict.behaviourRF.R)
- R Code for Self-Organizing Maps models (predict.behaviourSOMS.R)
- R Data files for each behaviour group a model was built (Folder 'R Dataframes')

# Prepare datasets
In step 3, all data was merged into a big dataset. The next step is to prepare the data and create dataframes that can be used to build the models.

# Steps
Steps 2 to 
## (1) Remove behaviours not observed
Not all behaviours were observed in the videorecordings. Behaviours that were not observed and where cats were out of sight, were removed from the dataset. 
## (2) Merge behaviours
Multiple predictive models were built for both the collar and harness mounted ActiGraph. For each model, the overall accuracy and estimated Kappa’s coefficient were evaluated and for each behaviour the sensitivity, specificity, precision and accuracy were evaluated. Behaviours with low values were clustered with the behaviour the model often misclassified it as, if the behaviours belong to the same category (other, active, inactive, maintenance), or to the category ‘other’ if misclassified as a behaviour belonging to another category. A new model was then built, and the overall accuracy and estimated Kappa’s coefficient were compared to the previous model. Behaviours were clustered until the overall accuracy and Kappa’s coefficient no longer improved.

In this step you see all the behaviour selections that occured.
## (3) Data splitting
Each cat wore two accelerometers: one attached to a collar and one attached to a harness. Here we split the complete dataframe in two dataframes:
1. Collar dataframe, containing the predictor variables from the collar-mounted accelerometer
2. Harness dataframe, containg the predictor variables from the harness-mounted accelerometer
## (4) Create a seperate dataframe for every new behaviour group

