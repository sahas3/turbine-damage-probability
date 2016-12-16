# Main function for classifying different combinations of training and testing data sets

fsOption <- 0 # flag for selecting features
# 0 : no feature selection
# 1 : choose features with ROC >= 0.55
# 2 : choose features from a pre-existing model generated using CARET (RFE/GA/SA) method

plotFlag <- F # flag for plotting ROC curves for predictor attributes (all and selected)
ensFlag <- F # flag for performing ensemble classification

## Set up parallel proessing
numCores <- 10 # number of cores to be used for Parallel Processing
clusters <- makeCluster(numCores)
registerDoParallel(clusters)

## Read pre-generated datasets
dataSetCombinations <- matrix(c(1, 2, 2, 1, 3, 4, 4, 3, 5, 6, 6, 5, 7, 8, 8, 7, 9, 10, 10, 9), nrow = 2)
# dataSetCombinations <- matrix(c(1, 2), nrow = 2)

rocValues <- foreach(iDataSet = 1:1, .packages = packageNames, .combine = cbind) %do% {

    itrainData <- dataSetCombinations[1, iDataSet]
    itestData <- dataSetCombinations[2, iDataSet]
    dataValues1 <- readRDS(paste('./DataSets/dataValues', itrainData, '.RData', sep = ''))
    dataValues2 <- readRDS(paste('./DataSets/dataValues', itestData, '.RData', sep = ''))


    # choose attributes to be used for prediction
    turbineDataAttributes <- names(dataValues1)
    predictorAttributes <- turbineDataAttributes[turbineDataAttributes != 'turbineClass']
    # predictorAttributes <- predictorAttributes[7]
    # predictorAttributes <- c("strikeOverLapAreaSum", "strikeOverLapAreaAmpSum", "strikeCounter", "strikeProximity",
    # "strikeBinomProb")


    # roc values for attributes for the training set
    predictorAttributesTrainPerf <- predictorAttributeROC(dataValues1[predictorAttributes], dataValues1[['turbineClass']]
                                                          , predictorAttributes, dataStr = 'Train')

    # roc values for attributes for the testing set
    predictorAttributesTestPerf <- predictorAttributeROC(dataValues2[predictorAttributes], dataValues2[['turbineClass']]
                                                         , predictorAttributes, dataStr = 'Test')
    if (plotFlag)
    {
        # plot ROC curves for all predictor attributes
        plotROCs(predictorAttributesTrainPerf, legendStr = 'Predictor Attributes', titleStr1 = 'All Attributes',
                 titleStr2 = 'Train')
        plotROCs(predictorAttributesTestPerf, legendStr = 'Predictor Attributes', titleStr1 = 'All Attributes')
    }


    #### Select a subset of features
    titleStr <- 'Classifier Models (all attributes)'
    if (fsOption == 1)
    {
        #### Choose predictor attributes if corresponding ROC value is >= 0.55
        predictorAttributesTrainROC <- lapply(predictorAttributesTrainPerf, function(x) x$auc)
        predictorAttributes <- names(which(predictorAttributesTrainROC >= 0.55))
        # predictorAttributes = c('strikeProbAmpSum', 'strikeOverLapAreaAmpMax', 'strikeBinomProb', 'strikeProximity')

        titleStr <- 'Classifier Models (selected attributes)'
        # #### Remove predictor attributes with >= 0.95 correlation
        # trainData <- dataValues1[predictorAttributes]
        # corVals <- cor(trainData)
        # highCorIdx <- findCorrelation(corVals, 0.95)
        # predictorAttributes <- predictorAttributes[-highCorIdx]

    } else if (fsOption == 2) {

        # RFE model : pre-generated
        # fsModel <- readRDS('./WorkSpaceData/svmRadialCostSA.RData')
        fsModel <- readRDS('./WorkSpaceData/nnetFSv2.RData')
        predictorAttributes <- predictors(fsModel$fit)
        # predictorAttributes <- c(predictors(fsModel), 'strikeBinomProb')
        titleStr <- 'Classifier Models (selected attributes)'
    }

    #### Get the Train and Testing Datasets

    trainData <- dataValues1[predictorAttributes]
    trainClass<- dataValues1[['turbineClass']]

    testData <- dataValues2[predictorAttributes]
    testClass <- dataValues2[['turbineClass']]

    #### Plot the ROC curves for selected predictor attributes

    # if (plotFlag)
    # {
    #
    #     predictorAttributesTrainPerf <- predictorAttributeROC(trainData, trainClass, predictorAttributes, dataStr = 'Train')
    #     plotROCs(predictorAttributesTrainPerf, legendStr = 'Predictor Attributes', titleStr1 = 'Selected Attributes',
    #              titleStr2 = 'Train')
    #
    #     predictorAttributesTestPerf <- predictorAttributeROC(testData, testClass, predictorAttributes, dataStr = 'Test')
    #     plotROCs(predictorAttributesTestPerf, legendStr = 'Predictor Attributes', titleStr1 = 'Selected Attributes')
    # }

    #### Perform Classification
    classifierFit <- classifyTurbines(trainData = trainData, trainClass = trainClass, testData = testData,
                                      testClass = testClass, predictorAttributes, parallelFlag = T,
                                      # resamplingMethod = 'adaptiveCV',
                                      # preProcessStr = c('spatialSign'),
                                      # preProcessStr = c('BoxCox'),
                                      # preProcessStr = c('YeoJohnson'),
                                      # preProcessStr = c('nzv', 'range'),
                                      preProcessStr = c('scale', 'center'),
                                      # preProcessStr = c('range'),
                                      # classifier = c('ORFsvm')
                                      # preProcessFlag = T,
                                      classifierNames = c('svmRadial', 'nnet', 'avNNet', 'rf', 'pls', 'svmPoly')
                                                      # 'svmRadialWeights', 'svmRadialCost')
                                      # 'rbf', 'xgbTree', 'treebag', 'ORFsvm')
                                      # classifier = c('AdaBag', 'rf')
                                      # , numCores = 0)
                                      , numCores = 0)

    ## Plot classifier results

    # get probability predictions
    testDataPredictions <- getClassPredictions(classifierFit, testData)
    # get ROC values
    testDataPerf <- lapply(testDataPredictions, function(x) pROC::roc(testClass, x))

    # plot ROC curves
    plotROCs(testDataPerf, legendStr = 'Classifier Models', titleStr1 = titleStr,
             titleStr2 = 'Test')

    results <- resamples(classifierFit)
    print(summary(diff(results, metric = 'ROC')))


    if (ensFlag)
    {
        #### Perform Ensemble Classifier
        classifierNamesSorted <- names(sort(unlist(lapply(testDataPerf, function(x) x$auc)), decreasing = T))
        bestClassifierIndx <- which(names(classifierFit) == classifierNamesSorted[1])
        # ensClassifierNamesBest <- classifierNamesSorted[1:2]
        ensClassifierNames <- c('avNNet', 'svmRadial') # always uses this combination of classifiers to ensemble

        modelCorrVals <- modelCor(results)
        # # first element of correlation matrix is always 1, so include that directly for the best classifier model,
        # # for the rest pick models for which correlation with the first model picked is less than 0.25

        # pick the one with best ROC, and the one which is least corelated with it
        ensClassifierNamesBest <- c(classifierNamesSorted[1],
                                    names(sort(modelCorrVals[-bestClassifierIndx, bestClassifierIndx], decreasing = T)[1]))
                                # names(which(modelCorrVals[-bestClassifierIndx, bestClassifierIndx] <= 0.25)))

        useOrigPredictorsFlag <- F
        ensClassifierFitBest <- getEnsembleClassifier(trainData = trainData, trainClass = trainClass, testData = testData,
                                                  testClass = testClass,
                                                  classifierModels = classifierFit[ensClassifierNamesBest],
                                                  classifierName = 'pls',
                                                  # preProcessStr = c('center', 'scale'),
                                                  useOrigPredictors = useOrigPredictorsFlag)


        ensClassifierFit <- getEnsembleClassifier(trainData = trainData, trainClass = trainClass, testData = testData,
                                                      testClass = testClass,
                                                      classifierModels = classifierFit[ensClassifierNames],
                                                      classifierName = 'pls',
                                                      # preProcessStr = c('center', 'scale'),
                                                      useOrigPredictors = useOrigPredictorsFlag)

        ## Plot Ensemble Classifier Results

        # modify the test sets to include proper attributes : see getEnsembleClassifier
        newTestDataBest <- getEnsembleClassifierAttributes(testData, classifierFit[ensClassifierNamesBest],
                                                       useOrigPredictors = useOrigPredictorsFlag)

        newTestData <- getEnsembleClassifierAttributes(testData, classifierFit[ensClassifierNames],
                                                           useOrigPredictors = useOrigPredictorsFlag)

        testDataPredictionsEns <- getClassPredictions(ensClassifierFitBest, newTestDataBest)
        testDataPerfEnsBest <- lapply(testDataPredictionsEns, function(x) pROC::roc(testClass, x))
        names(testDataPerfEnsBest) <- paste('ENS_BEST(',names(testDataPerfEnsBest),')', sep = '')

        testDataPredictionsEns <- getClassPredictions(ensClassifierFit, newTestData)
        testDataPerfEns <- lapply(testDataPredictionsEns, function(x) pROC::roc(testClass, x))
        names(testDataPerfEns) <- paste('ENS(',names(testDataPerfEns),')', sep = '')

        # plotROCs(c(testDataPerf[unique(c(ensClassifierNamesBest, ensClassifierNames))], testDataPerfEnsBest, testDataPerfEns),
        #          legendStr = 'Classifier Models', titleStr1 = 'Ensemble Classifier Model', titleStr2 = 'Test')

        plotROCs(c(testDataPerf[ensClassifierNamesBest], testDataPerfEnsBest),
                 legendStr = 'Classifier Models', titleStr1 = 'Ensemble Classifier Model', titleStr2 = 'Test')

        plotROCs(c(testDataPerf[ensClassifierNames], testDataPerfEns),
                 legendStr = 'Classifier Models', titleStr1 = 'Ensemble Classifier Model', titleStr2 = 'Test')

    } else {
        testDataPerfEns <- NULL
        testDataPerfEnsBest <- NULL
    }

    ## gather all roc values together
    allROCVals <- unlist(lapply(c(predictorAttributesTestPerf, testDataPerf, testDataPerfEns, testDataPerfEnsBest),
                                function(x) x$auc))
    allROCVals
}

print(rocValues)

stopCluster(clusters)
registerDoSEQ()

# ggsave(file = "./ProjectReport/Figures/predictorROC.png", width = 16, height = 9, type = "cairo-png")

# saveRDS(rocValues, './Results/rocValuesAllPredictorsScaledCenteredV2.RData')