plotTurbineAttributes <- function(TurbineData, # turbine data set : data frame with columns : lon, lat, color
                                  attrValues, # attribute values for turbines
                                  titleStr, legendStr,
                                  showGndTruth = T, # if gorund truth is to be plotted
                                  nLevels = 5 # levels for representing attibute value
                                  )
{
    # plots turbine attributes named 'attrValues'

    hfigTurbineFeature <- ggplot(TurbineData, aes(x = lon, y = lat, z = attrValues)) +
        geom_point(aes(size = attrValues, col = attrValues), show.legend = T) +
        scale_size_continuous(range = c(1, nLevels), guide = guide_legend(title = legendStr)) +
        # scale_size_discrete(range = c(0, 1), guide = guide_legend(title = legendStr)) +
        scale_color_continuous(low='green', high = 'red', guide = guide_legend(title = legendStr)) +
        ggtitle(titleStr) +
        theme(legend.position = 'bottom', legend.title = element_text(size = 20), legend.text = element_text(size = 20),
              plot.title = element_text(size = 20, hjust = 0.5), axis.title = element_text(size = 20),
              axis.text = element_text(size = 20)) +
        xlab('Longitude') + ylab('Latitude')

    if (showGndTruth)
    {
        hfigTurbineFeature <- hfigTurbineFeature +
            geom_point(inherit.aes = F, data = TurbineData, aes(lon , lat), col = TurbineData$color, pch = 21, size = 6)
    }

    print(hfigTurbineFeature)
    return(hfigTurbineFeature)
}