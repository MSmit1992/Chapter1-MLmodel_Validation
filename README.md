
# Cat_Accelerometry
Buiding predictive models to predict cat behaviour using accelerometers

*A lot of the code used for the Self-Organizing Maps was published on GitHub by [cclemente](https://github.com/cclemente) in the repository [Animal_accelerometry](https://github.com/cclemente/Animal_accelerometry.git).*

# Step 1: Data collection
For this study, two accelerometers were attached to domestic cats:
- Ventrally attached to a collar
- Attached to a harness, with the activity monitor resting on the left shoulder blade

While wearing the accelerometers, cats were videorecorded. Videorecordings were used to score the behaviour of cats.

[ActiGraph wGT3X-BT](https://actigraphcorp.com/actigraph-wgt3x-bt/) accelerometers were used.

# [Step 2: Feature engineering](https://github.com/MSmit1992/Cat_Accelerometry/tree/main/Step%202:%20Feature%20engineering)
Acceleration data was sampled at a frequency of 30 Hz (raw data). A total of 32 predictor variabes were derived from the raw x,y,z accelerometer data and summarized into 1 second epochs.

# [Step 3: Data preparation](https://github.com/MSmit1992/Cat_Accelerometry/tree/main/Step%203:%20Data%20preparation)
Following step 2, you now have a file for each cat containing the 32 predictor variables. The next step is to merge this data with scored behaviour (annotated) data.

# [Step 4: Build predictive models](https://github.com/MSmit1992/Cat_Accelerometry/tree/main/Step%204:%20Build%20predictive%20models)
Following step 4, you now have the datasets you can use to build the predictive models.
In this study, two Machine Learning techniques were used to build predictive models:
- Random Forest
- Self-Organizing maps

# [Step 5: Use models to predict behaviour](https://github.com/MSmit1992/Cat_Accelerometry/tree/main/Step%205:%20Predict%20behaviour)
With the models built in step 4, you can now use the model to predict the behaviour using the 32 predictor variables derived from the raw accelerometer data

# [Step 6: Use models to predict behaviour](https://github.com/MSmit1992/Cat_Accelerometry/blob/main/Step%206:%20Compare%20Models)
The models were compared for the proportions of the behaviours they predicted. This was done by comparing proportional graphs and by a Dirichlet test.

# [Rater reliability](https://github.com/MSmit1992/Cat_Accelerometry/tree/main/Rater%20reliability)

