# plots the ROC values for a list of performance results

plotROCs <- function(perfValues,
                     # list of performance results with columns : specificities, sensitivitites, auc
                     # generated using pROC::roc() function
                     legendStr = 'Classifier Models',
                     titleStr1 = 'Classifier Models',
                     titleStr2 = 'Test')
{
    # combine all specificities into a single data frame
    specificities <- melt(lapply(perfValues, function(x) x$specificities), id.vars = NULL)
    names(specificities) <- c('value', 'modelName')

    # combine all sensitivities into a single data frame
    sensitivities <- melt(lapply(perfValues, function(x) x$sensitivities), id.vars = NULL)
    names(sensitivities) <- c('value', 'modelName')

    # combine all auroc (area under ROC) into a single data frame
    aucVals <- melt(lapply(perfValues, function(x) x$auc), measure.vars = NULL)
    names(aucVals) <- c('value', 'modelName')
    aucVals <- aucVals[order(aucVals['value']),] # reorder auc values in an ascending manner

    modelLevels <- factor(specificities[['modelName']], aucVals[['modelName']])

    modelNames <- levels(modelLevels)
    numModels <- length(modelNames)

    # create legend texts to incorporate AUC values by rounding them to the third decimal place
    labelStr <- ''
    for (iClassifier in 1:numModels)
    {
        labelStr <- c(labelStr, paste(modelNames[iClassifier], ':', round(aucVals[iClassifier, 1]*1000)/1000))
    }

    hfig <- ggplot() + geom_abline(data = NULL, aes(slope = 1, intercept = 0)) +
        geom_path(data = NULL, inherit.aes = F, aes(1 - specificities[['value']], sensitivities[['value']],
                                                    col = modelLevels), size = 1.1) +
        coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
        xlab('False Positive Rate') + ylab('True Positive Rate') +
        ggtitle(paste('ROC curves for', titleStr1, 'for', titleStr2, 'dataset')) +
        scale_colour_discrete(name = legendStr, labels = labelStr[-1]) +
        theme(legend.position = 'right', legend.title = element_text(size = 20), legend.text = element_text(size = 20),
              plot.title = element_text(size = 20, hjust = 0.5), axis.title = element_text(size = 20),
              axis.text = element_text(size = 20), legend.direction = 'vertical')

    print(hfig)
}