## Main file for performing Feature Selection

#### Read pre-generated datasets

iDataSet1 <- 1
iDataSet2 <- 2
dataValues1 <- readRDS(paste('./DataSets/dataValues', iDataSet1, '.RData', sep = ''))
dataValues2 <- readRDS(paste('./DataSets/dataValues', iDataSet2, '.RData', sep = ''))


#### Choose predictor attributes
turbineDataAttributes <- names(dataValues1)
predictorAttributes <- turbineDataAttributes[turbineDataAttributes != 'turbineClass']

# check for zero variance variables and remove those
nearZeroVarAttributes = nearZeroVar(dataValues1[predictorAttributes], saveMetrics = T)
predictorAttributes = predictorAttributes[nearZeroVarAttributes$percentUnique >= 20]

# check for high correlation and remove them
corVals <- cor(dataValues1[predictorAttributes])
highCorIndx <- findCorrelation(corVals, cutoff = 0.9)
predictorAttributes <- predictorAttributes[-highCorIndx]

#### Get the Train and Testing Datasets
dataSet1 <- dataValues1[predictorAttributes]
dataClass1 <- dataValues1[['turbineClass']]

dataSet2 <- dataValues2[predictorAttributes]
dataClass2 <- dataValues2[['turbineClass']]


trainData <- dataSet1
trainClass <- dataClass1
testData <- dataSet2
testClass <- dataClass2


predictorAttributesTrainPerf <- predictorAttributeROC(trainData, trainClass, names(trainData), dataStr = 'Train')
plotROCs(predictorAttributesTrainPerf, legendStr = 'Predictor Attributes', titleStr1 = 'Train')

predictorAttributesTestPerf <- predictorAttributeROC(testData, testClass, names(testData), dataStr = 'Test')
plotROCs(predictorAttributesTestPerf, legendStr = 'Predictor Attributes')

fsModel <- getOptimalFeatures(trainData, trainClass, testData, testClass
                   # , fsAlgo = 'rfe'
                   , fsAlgo = 'GA'
                   # , fsAlgo = 'GA'
                   , parallelFlag = T
                   # , rfeFunc = rfFuncs
                   # , rfeFunc = lmFuncs
                   # , rfeFunc = treebagFuncs
                   # , rfeFunc = nbFuncs
                   # , caretMethod = 'lda'
                   , caretMethod = 'pls'
                   # , tuneGrid = expand.grid(mtry = 1:length(names(trainData)))
                   # , tuneGrid = expand.grid(sigma = 1, C = 2^(-3:4))
                   # , preProcessStr = 'BoxCox'
                   # , preProcessStr = c('range', 'nzv')
                   )

# saveRDS(fsModel, './WorkSpaceData/svmRadialCostSA.RData')