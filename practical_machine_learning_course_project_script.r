### Load necessary packages
pckgs = c('dplyr', 'ggplot2', 'caret')
installed_pckgs <- pckgs %in% rownames(installed.packages())
if (any(installed_pckgs == FALSE)) {
  install.packages(pckgs[!installed_pckgs])
}
invisible(lapply(pckgs, library, character.only = TRUE))

### Get and filter data
training = read.csv('pml-training.csv')
testing = read.csv('pml-testing.csv')

training = training %>% 
  select(grep(names(training), 
              pattern = '^accel', #all columns that start with 'accel'
              value = TRUE),
         classe) #get actual column names instead of index

testing = testing %>% 
  select(grep(names(testing), 
              pattern = '^accel', #all columns that start with 'accel'
              value = TRUE), 
         problem_id) #get actual column names instead of index

### Create actual training data and quiz data
set.seed(123)
inTrain = createDataPartition(y = training$classe, p = 0.7, list = FALSE)
train_set = training[inTrain, ]
quiz_set = training[-inTrain, ]

x = train_set[, -13]
y = train_set$classe

### Train models
trControl = trainControl(method = 'repeatedcv', number = 3, repeats = 3)

set.seed(12129)
modrf = train(x, y, method = 'rf', trControl = trControl)
modknn = train(x, y, method = 'knn', trControl = trControl)
modgbm = train(x, y, method = 'gbm', trControl = trControl)

### Save models
saveRDS(modrf, file = 'modrf.rds')
saveRDS(modknn, file = 'modknn.rds')
saveRDS(modgbm, file = 'modgbm.rds')




