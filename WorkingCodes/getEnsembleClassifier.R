# generate ensemble of existing classifier models

getEnsembleClassifier <- function(trainData, trainClass, testData, testClass,
                                  classifierModels, # pre-existing classifier models
                                  classifierName = 'glm',
                                  # classifier to perform ensemble : can be a vector string of different classifier models as well
                                  preProcessStr = NULL, # pre-processing string
                                  useOrigPredictors = F # flag to use attributes for the pre-existing classifier models
                                  )
{

    # if useOrigPredictors is 'false' then only use predictions from the existing classifier models for ensembling,
    # otherwise append predictions to the original train and test data set for ensembling
    newTrainData <- getEnsembleClassifierAttributes(trainData, classifierModels, useOrigPredictors)
    newTestData <- getEnsembleClassifierAttributes(testData, classifierModels, useOrigPredictors)

    preProcessFlag <- ifelse (is.null(preProcessStr), F, T)

    # perform ensemble classification
    ensClassifierFit <- classifyTurbines(newTrainData, trainClass, newTestData,
                                       testClass, names(newTrainData), parallelFlag = T,
                                       preProcessStr = preProcessStr,
                                       preProcessFlag = preProcessFlag,
                                       # resamplingMethod = 'adaptiveCV',
                                       # preProcessStr = c('spatialSign'),
                                       # preProcessStr = c('scale', 'center'),
                                       # preProcessStr = c('range'),
                                       classifierNames = classifierName
                                       , numCores = 4)

    return(ensClassifierFit)

}