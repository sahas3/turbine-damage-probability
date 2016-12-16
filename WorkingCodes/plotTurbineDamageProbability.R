plotTurbineDamageProbability <- function(TurbineData, predProbs, groundTruthClass)
{
    # # nLevels = 5 # diff(range(predProbss))/2
    # nLevels = round(diff(range(predProbs))/0.1)
    # hfigTurbineProbs <- ggplot(TurbineData, aes(x = lon, y = lat, colour = groundTruthClass,
    #                                              size = predProbs, fill = predProbs)) +
    #     geom_point(shape = 21, stroke = 0.75) +
    #     scale_size_continuous(range = c(1, 2*nLevels), guide = guide_legend(title = 'Probability of Damage')) +
    #     # scale_size_discrete(range = c(0, 1), guide = guide_legend(title = legendStr)) +
    #     scale_fill_continuous(low='green', high = 'red', guide = guide_legend(title = 'Probability of Damage')) +
    #     scale_color_manual(values = c('red', 'green'), labels = c('Damaged', 'Good')) +
    #     labs(x = 'Longitude', y = 'Latitude', # size = 'Probability of Damage', fill = 'Probability of Damage',
    #          colour = 'Ground Truth') +
    #     ggtitle('Probability of Wind Turbines being damaged by a Lightning Storm') +
    #     theme(legend.position = 'bottom', legend.title = element_text(size = 20), legend.text = element_text(size = 20),
    #           plot.title = element_text(size = 20, hjust = 0.5), axis.title = element_text(size = 20),
    #           axis.text = element_text(size = 20), legend.box = 'vertical', legend.key.size = unit(0.5, 'cm'))
    #
    # print(hfigTurbineProbs)
    # return(hfigTurbineProbs)

    colfunc <- colorRampPalette(c("green", "red"))

    rangeProbs <- diff(range(predProbs))
    stepSz <- round(rangeProbs*1000000/5)/1000000
    breaksSeq <- seq(max(min(predProbs)-stepSz, 0), min(max(predProbs)+stepSz, 1), stepSz)
    breaksSeq <- seq(min(predProbs)-stepSz, max(predProbs)+stepSz, stepSz)

    # limit probability within 0 to 1
    breaksSeq[breaksSeq < 0] <- 0
    breaksSeq[breaksSeq > 1] <- 1
    breaksSeq <- unique(breaksSeq)
    # contour plots with region colored using discrete levels of strikeProb
    predProbLevels <- cut(predProbs, breaks = breaksSeq)

    # Sort the segments in ascending order
    breaks <- levels(unique(predProbLevels))
    nLevels <- round(1.5*length(breaks))
   hfigTurbineProbs <- ggplot(TurbineData, aes(x = lon, y = lat, colour = groundTruthClass, size = predProbLevels,
                                                   fill = predProbLevels)) +
        geom_point(shape = 21, stroke = 1.25) +
        scale_size_discrete(range = c(1, nLevels), guide = guide_legend(title = 'Probability of Damage')) +
        scale_fill_manual(values = colfunc(length(breaks)), guide = guide_legend(title = 'Probability of Damage'), breaks = breaks, labels = breaks) +
    scale_color_manual(values = c('red', 'green'), labels = c('Damaged', 'Good')) +
        labs(x = 'Longitude', y = 'Latitude', # size = 'Probability of Damage', fill = 'Probability of Damage',
             colour = 'Ground Truth') +
        ggtitle('Probability of Wind Turbines being damaged by a Lightning Storm') +
        theme(legend.position = 'right', legend.title = element_text(size = 20), legend.text = element_text(size = 20),
              plot.title = element_text(size = 20, hjust = 0.5), axis.title = element_text(size = 20),
              axis.text = element_text(size = 20)) #, #legend.box = 'vertical', legend.key.size = unit(0.5, 'cm'))

    print(hfigTurbineProbs)
}