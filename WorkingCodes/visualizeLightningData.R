# plots lightning data for visualizing

# get indices for plotting Lightning Data
indxPoints <- 2:11

# Get data points for plotting ellipses for each Lightning Strike
ellipseData <- getEllipseData(cbind(LightningData$x[indxPoints], LightningData$y[indxPoints]),
                              cbind(LightningData$majorRad[indxPoints], LightningData$minorRad[indxPoints]),
                              LightningData$angle[indxPoints]);

# get latitude-longitude from cartesian co-ordinates
tempPos <- cartesian2LatLon(ellipseData$x*1000, ellipseData$y*1000); # input to this function has to be in meters
ellipseData$lon <- tempPos[,1];
ellipseData$lat <- tempPos[,2];

# plot the ellipses, with the size of the centers scaled by amplitude
hfig <- ggplot() + geom_path(data = ellipseData, aes(lon, lat, group = grp), color = 'blue') + # ellipses
    geom_point(data = LightningData[indxPoints, ], aes(lon, lat, size = peakAmpAbs),
               color = 'black', show.legend = T) + # centers
    scale_size_continuous(name = 'Peak Absolute Amplitude', range = c(1,8)) +
    # scale_fill_continuous(name = 'Peak Absolute Amplitude', low = "plum1", high = "purple4") +
    ggtitle('Visual Representation of Lightning Strike Data') +
    theme(legend.position = 'bottom', legend.title = element_text(size = 20), legend.text = element_text(size = 20),
          plot.title = element_text(size = 20, hjust = 0.5), axis.title = element_text(size = 20),
          axis.text = element_text(size = 20)) +
    labs(x = 'Longitude', y = 'Latitude')

print(hfig)

ggsave(hfig, file = "./Figures/LightDataVisualization.png", width = 16, height = 9, type = "cairo-png")
# ggsave(filename = "./Figures/LightDataVisualization.eps", device = 'eps')

