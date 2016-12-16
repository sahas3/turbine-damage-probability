# generates predictions for a list of classifier models

getClassPredictions <- function(modelList, # list of classifier models
                                attrValues, # attribute values passed to the classifier models
                                type = 'prob' # type of prediction : 'prob' for probability, 'raw' for class label
                                )
{
    classPredictions <- lapply(modelList, function(x) predict(x,
        # newdata = ifelse(is.null(predictors(x)),
        #              attrValues[, names(x$trainingData)[-length(names(x$trainingData))]],
        #              attrValues[, predictors(x)]),
        newdata = attrValues,
        type=type))

    if (type == 'prob') {classPredictions <- lapply(classPredictions, function(x) x[["B"]])} # x[, 1])}
    # return the 'probability of damaged

    classPredictions <- data.frame(classPredictions)
    return(classPredictions)

}