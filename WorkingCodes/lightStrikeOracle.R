# this is the ORACLE function for training classifiers based on
# damage done by known lightning storm on a turbine farm
# predicts damage to be done by new lightning storm
# if existing classifier model is passed then only make predictions : doesn't retrain the classifier

lightStrikeOracle <- function(TurbineData, # turbine data set
                              LightningDataOld, # lightning data with damage known
                              groundTruthDataClass = NULL, # ground truth data for LightningDataOld
                              LightningDataNew, # lightning data for which prediction is to be done
                              peakAmpThresh = runif(1, 30, 50), # peak amplitude threshold for generating attributes
                              distThresh = runif(1, 0.3, 0.6), # distance threshold for generating attributes
                              fsOption = 0, # feature selection option
                              modelFit = NULL) # pre-existing classifier model
{

    peakAmpThreshGnd <- runif(1, 30, 50) # threshold on peak amplitude for generating ground truth data
    distThreshGnd <- runif(1, 0.3, 0.6) # threshold on turbine attractive radius for generating ground truth data

    # generate attributes for the turbines based on the lightning storm
    dataValuesNew <- generateClassifierAttributes(LightningDataNew, TurbineData, NULL, peakAmpThresh = peakAmpThresh,
                                                  distThresh = distThresh, peakAmpThreshGnd = peakAmpThreshGnd,
                                                  distThreshGnd = distThreshGnd)
    # for new Lightning Data we do not know ground truth : so simulate it

    if (is.null(modelFit)) # if no classifier model is provided generate a new classifier model
    {
        dataValuesOld <- generateClassifierAttributes(LightningDataOld, TurbineData, groundTruthDataClass,
                                                      peakAmpThresh = peakAmpThresh,
                                                      distThresh = distThresh, peakAmpThreshGnd = peakAmpThreshGnd,
                                                      distThreshGnd = distThreshGnd)
        # for old Lightning Data pass ground truth data if provided, or else will be simulated

        # dataValuesOld <- readRDS(paste('./DataSets/dataValues', 1, '.RData', sep = ''))
        # dataValuesNew <- readRDS(paste('./DataSets/dataValues', 2, '.RData', sep = ''))

        turbineDataAttributes <- names(dataValuesOld)

        # get attribute names to be used for training
        predictorAttributes <- turbineDataAttributes[turbineDataAttributes != 'turbineClass']

        # get ROC values for training and testing set based on chosesn attributes
        predictorAttributesTrainPerf <- predictorAttributeROC(dataValuesOld[predictorAttributes], dataValuesOld[['turbineClass']]
                                                              , predictorAttributes, dataStr = 'Train')

        predictorAttributesTestPerf <- predictorAttributeROC(dataValuesNew[predictorAttributes], dataValuesNew[['turbineClass']]
                                                             , predictorAttributes, dataStr = 'Test')

        #### Select a subset of features

        if (fsOption == 1)
        {
            #### Choose predictor attributes if corresponding ROC value is >= 0.55
            predictorAttributesTrainROC <- lapply(predictorAttributesTrainPerf, function(x) x$auc)
            predictorAttributes <- names(which(predictorAttributesTrainROC >= 0.55))
            # predictorAttributes = c('strikeProbAmpSum', 'strikeOverLapAreaAmpMax', 'strikeBinomProb', 'strikeProximity')


            # #### Remove predictor attributes with >= 0.95 correlation
            # trainData <- dataValues1[predictorAttributes]
            # corVals <- cor(trainData)
            # highCorIdx <- findCorrelation(corVals, 0.95)
            # predictorAttributes <- predictorAttributes[-highCorIdx]

        } else if (fsOption == 2) {

            # RFE model : pre-generated
            fsModel <- readRDS('./WorkSpaceData/svmRadialCostSA.RData')
            predictorAttributes <- predictors(fsModel$fit)
            # predictorAttributes <- c(predictors(fsModel), 'strikeBinomProb')

        }

        #### Get the Train and Testing Datasets

        trainData <- dataValuesOld[predictorAttributes]
        trainClass<- dataValuesOld[['turbineClass']]

        testData <- dataValuesNew[predictorAttributes]
        testClass <- dataValuesNew[['turbineClass']]

        #### Perform Classification
        classifierFit <- classifyTurbines(trainData = trainData, trainClass = trainClass, testData = testData,
                                          testClass = testClass, predictorAttributes, parallelFlag = T,
                                          # resamplingMethod = 'adaptiveCV',
                                          # preProcessStr = c('spatialSign'),
                                          # preProcessStr = c('BoxCox'),
                                          # preProcessStr = c('YeoJohnson'),
                                          # preProcessStr = c('nzv', 'range'),
                                          # preProcessStr = c('scale', 'center'),
                                          # preProcessStr = c('range'),
                                          # classifier = c('ORFsvm')
                                          # preProcessFlag = T,
                                          classifierNames = c('svmRadial', 'nnet', 'avNNet', 'rf', 'pls')
                                          # 'svmRadialWeights', 'svmRadialCost')
                                          # 'rbf', 'xgbTree', 'treebag', 'ORFsvm')
                                          # classifier = c('AdaBag', 'rf')
                                          # , numCores = 0)
                                          , numCores = detectCores()/2)

        # Get classifier results
        testDataPredictions <- getClassPredictions(classifierFit, testData)
        testDataPerf <- lapply(testDataPredictions, function(x) pROC::roc(testClass, x))

        #### Perform Ensemble Classifier
        classifierNamesSorted <- names(sort(unlist(lapply(testDataPerf, function(x) x$auc)), decreasing = T))
        bestClassifierIndx <- which(names(classifierFit) == classifierNamesSorted[1])

        results <- resamples(classifierFit)
        modelCorrVals <- modelCor(results)

        # pick the one with best ROC, and the one which is least corelated with it
        ensClassifierNames <- c(classifierNamesSorted[1],
                                    names(sort(modelCorrVals[-bestClassifierIndx, bestClassifierIndx], decreasing = F)[1]))

        useOrigPredictorsFlag <- F
        ensClassifierFit <- getEnsembleClassifier(trainData = trainData, trainClass = trainClass, testData = testData,
                                                  testClass = testClass,
                                                  classifierModels = classifierFit[ensClassifierNames],
                                                  classifierName = 'pls',
                                                  # preProcessStr = c('center', 'scale'),
                                                  useOrigPredictors = useOrigPredictorsFlag)

        newTestData <- getEnsembleClassifierAttributes(testData, classifierFit[ensClassifierNames],
                                                       useOrigPredictors = useOrigPredictorsFlag)


        testDataPredictionsEns <- getClassPredictions(ensClassifierFit, newTestData)
        testDataPerfEns <- lapply(testDataPredictionsEns, function(x) pROC::roc(testClass, x))
        names(testDataPerfEns) <- paste('ENS(',names(testDataPerfEns),')', sep = '')


        #### Plot ROC curves
        plotROCs(c(testDataPerf, testDataPerfEns),
                 legendStr = 'Classifier Models', titleStr1 = 'Classifier Models', titleStr2 = 'Test')


        sortedROC <- sort(unlist(lapply(c(testDataPerf, testDataPerfEns), function(x) x$auc)), decreasing = T)
        print(sortedROC)

        ####  get the best classifier based on ROC curve
        bestClassifierName <- names(sortedROC)[1]

        # return the best classifier and corresponding prediction
        if (grepl('ENS', bestClassifierName))
        {
            modelFit <- ensClassifierFit
            predProbs <- testDataPredictionsEns
        } else {
            modelFit <- classifierFit[[bestClassifierName]]
            predProbs <- testDataPredictions[[bestClassifierName]]
        }

    } else {

        # return prediction if classifier model is passed
        predictorAttributes <- predictors(modelFit)
        predProbs <- predict(modelFit, dataValuesNew[predictorAttributes], type = 'prob')[,2]
    }

    return(list(predProbs = predProbs, modelFit = modelFit, originalClassLabels = testClass))
}