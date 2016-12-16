# computes the ROC values for turbine attributes

predictorAttributeROC <- function(attrValues, # turbine attributes
                                  classValues, # class labels for computing ROC
                                  predictorAttributes, # set of attribute values to compute ROC
                                  dataStr = 'Test')
{
    cat('\nArea under ROC curves for the', toupper(dataStr), 'dataset :', '\n')

    # colAUC returns max(AUC, 1-AUC)
    # https://stat.ethz.ch/pipermail/r-help/2005-September/079513.html
    # print(colAUC(attrValues[predictorAttributes], classValues)) # prints results in a good fashion (tabular)

    rocVals <- lapply(attrValues[predictorAttributes], function(x) pROC::roc(classValues, x))
    print(lapply(rocVals, function(x) x$auc))
    return(rocVals)
    # computes senstitivity, specificity to be used later, to plot ROC curves
}