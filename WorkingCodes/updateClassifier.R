## Updates pre-existing classifier if new ground thruth data is available
# not in a user friendly format
# performs, but needs more work

iGroundTruthData <- 3
iStormData <- 2
groundTruthData <- readRDS(paste('./DataSets/dataValues', iGroundTruthData, '.RData', sep = ''))
stormData <- readRDS(paste('./DataSets/dataValues', iStormData, '.RData', sep = ''))

# choose attributes to be used for prediction
# turbineDataAttributes <- names(groundTruthData)
# predictorAttributes <- turbineDataAttributes[turbineDataAttributes != 'turbineClass']
# # predictorAttributes <- predictors(fsModel)

trainData <- groundTruthData[predictorAttributes]
trainClass <- groundTruthData[['turbineClass']]

testData <- stormData[predictorAttributes]
testClass <- stormData[['turbineClass']]


###############################################################################
###############################################################################
###############################################################################


# Plot classifier results based on old classifier model for the new prediction (test) data set
testDataPredictions <- getClassPredictions(classifierFit, testData)
testDataPerf <- lapply(testDataPredictions, function(x) pROC::roc(testClass, x))

plotROCs(testDataPerf, legendStr = 'Classifier Models', titleStr1 = '(old) Classifier Models', titleStr2 = 'New Test')



###############################################################################
###############################################################################
###############################################################################

# Train classifiers on only the new ground truth data
classifierFitNew <- classifyTurbines(trainData, trainClass, testData, testClass,
                                     predictorAttributes,
                                     parallelFlag = T,
                                     # resamplingMethod = 'adaptiveCV',
                                     # preProcessStr = c('spatialSign'),
                                     # preProcessStr = c('scale', 'center'),
                                     # preProcessStr = c('range'),
                                     # classifier = c('svmRadialWeights')
                                     classifierNames = c('svmRadial', 'nnet', 'avNNet', 'rf', 'pls', 'svmPoly')
                                     # 'rbf', 'xgbTree', 'treebag', 'ORFsvm', 'rf')
                                     # classifier = c('AdaBag', 'rf')
                                     , numCores = 4)

# Plot classifier results
testDataPredictions <- getClassPredictions(classifierFitNew, testData)
testDataPerf <- lapply(testDataPredictions, function(x) pROC::roc(testClass, x))

plotROCs(testDataPerf, legendStr = 'Classifier Models', titleStr1 = '(new) Classifier Models',
         titleStr2 = 'New Test')

###############################################################################
###############################################################################
###############################################################################

# Combine new ground truth Data with old classifier model

groundTruthPrediction <- getClassPredictions(classifierFit, trainData)
# groundTruthROC <- lapply(groundTruthPrediction, function(x) pROC::roc(trainClass, x))
# weights <- as.vector(unlist(lapply(groundTruthROC, function(x) x$auc)))

# trainData <- cbind(trainData, as.matrix(groundTruthPrediction) %*% weights/sum(weights))
trainData <- cbind(trainData, groundTruthPrediction)
# names(trainData)[length(trainData)] <- 'oldClassifierPred'
predictorAttributes <- names(trainData)

testDataPrediction <- getClassPredictions(classifierFit, testData)
# testDataROC <- lapply(testDataPrediction, function(x) pROC::roc(trainClass, x))
# weights <- as.vector(unlist(lapply(testDataROC, function(x) x$auc)))

# testData <- cbind(testData, as.matrix(testDataPrediction) %*% weights/sum(weights))
testData <- cbind(testData, testDataPrediction)
# names(testData)[length(testData)] <- 'oldClassifierPred'

classifierFitMod <- classifyTurbines(trainData, trainClass, testData, testClass,
                                     predictorAttributes,
                                     parallelFlag = T,
                                     # resamplingMethod = 'adaptiveCV',
                                     # preProcessStr = c('spatialSign'),
                                     # preProcessStr = c('scale', 'center'),
                                     # preProcessStr = c('range'),
                                     # classifier = c('svmRadialWeights')
                                     classifierNames = c('svmRadial', 'nnet', 'avNNet', 'rf', 'pls', 'svmPoly')
                                     # 'rbf', 'xgbTree', 'treebag', 'ORFsvm', 'rf')
                                     # classifier = c('AdaBag', 'rf')
                                     , numCores = 4
                                     , oldClassifierModels = classifierFit)


# Plot modified classifier results
testDataPredictions <- getClassPredictions(classifierFitMod, testData)
testDataPerf <- lapply(testDataPredictions, function(x) pROC::roc(testClass, x))

plotROCs(testDataPerf, legendStr = 'Classifier Models', titleStr1 = '(updated) Classifier Models',
         titleStr2 = 'New Test')
