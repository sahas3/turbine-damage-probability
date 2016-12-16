# generates data set to be used for ensemble classification

getEnsembleClassifierAttributes <- function(attrValues, # attributes used to generate the existing classifier models
                                            classifierModels, # exsiting classifier models
                                            useOrigPredictors = F # flag to use attrValues for ensembling
                                            )
{
    # get predictions based on existing classifier models
    dataProbs <- getClassPredictions(classifierModels, attrValues)

    if (!useOrigPredictors)
    {
        # only use predictions for ensembling
        return(dataProbs)

    } else {

        # append predictions with attributes and use the whole set for ensembling
        return(cbind(attrValues, dataProbs))

    }
}