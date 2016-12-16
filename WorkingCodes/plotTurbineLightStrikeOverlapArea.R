# representing plot for showing computation of overlap area between lightning strike ellipse and attractive region of a turbine

StormData <- preProcessLightningData(
    lightStrikeSimulator(TurbineData, LightningData, numStrikes = 500, generateNewSimulation = F))

lightStrikeCenters <- cbind(StormData$x, StormData$y)
lightStrikeCentersLonLat <- cbind(StormData$lon, StormData$lat)
lightStrikeRadius <- cbind(StormData$majorRad, StormData$minorRad)


turbineCenters <- cbind(TurbineData$x, TurbineData$y)
turbineCentersLonLat <- cbind(TurbineData$lon, TurbineData$lat)

turbineRadius <- rep(0.45, 2)

iLightStrike = 20
turbineInEllipseIdx = which(isPointInEllipse(turbineCenters, lightStrikeCenters[iLightStrike,],
                                       lightStrikeRadius[iLightStrike, ], StormData$angle[iLightStrike]))

iTurbine = turbineInEllipseIdx[1]


# find range in longitude and latitude to create a 2D grid
numPoints <- 200
lonRange <- range(rbind(transpose(turbineCentersLonLat[,1]), transpose(lightStrikeCentersLonLat[,1]))) + c(-0.05, 0.05)
latRange <- range(rbind(transpose(turbineCentersLonLat[,2]), transpose(lightStrikeCentersLonLat[,2]))) + c(-0.05, 0.05)

lonGrid <- seq(lonRange[1], lonRange[2], length.out = numPoints)
latGrid <- seq(latRange[1], latRange[2], length.out = numPoints)

lonlatPoints <- data.matrix(expand.grid(lonGrid, latGrid))
xyPoints <- latLon2Cartesian(lonlatPoints[,1], lonlatPoints[,2])


pointsInStrikeEllipse = which(isPointInEllipse(xyPoints, lightStrikeCenters[iLightStrike,],
                                               lightStrikeRadius[iLightStrike, ], StormData$angle[iLightStrike]))
pointsInTurbineRadius = which(isPointInEllipse(xyPoints, turbineCenters[iTurbine,], turbineRadius, 0))

plotDataLon = rbind(lightStrikeCentersLonLat[iLightStrike, 1], turbineCentersLonLat[iTurbine, 1],
                    transpose(lonlatPoints[pointsInStrikeEllipse, 1]), transpose(lonlatPoints[pointsInTurbineRadius, 1]))
plotDataLat = rbind(lightStrikeCentersLonLat[iLightStrike, 2], turbineCentersLonLat[iTurbine, 2],
                    transpose(lonlatPoints[pointsInStrikeEllipse, 2]), transpose(lonlatPoints[pointsInTurbineRadius, 2]))
plotDataStr = c('LightStrikeCenter', 'TurbineCenter', rep('LightStrikeEllipse', length(lonlatPoints[pointsInStrikeEllipse, 1])),
                rep('TurbineRadius', length(lonlatPoints[pointsInTurbineRadius, 1])))


plotDataFrame = data.frame(x = plotDataLon, y = plotDataLat, type = plotDataStr)
hfigOverLapArea = ggplot(data = NULL, aes(lonlatPoints[,1], lonlatPoints[,2])) + geom_point(size = 0.1) +
    geom_point(inherit.aes = F, data = plotDataFrame, aes(x, y, col = type, shape = type), size = 3) +
    scale_shape_manual(name = '', values = c("LightStrikeCenter" = 16, "TurbineCenter" = 15, "LightStrikeEllipse" = 1,
    "TurbineRadius" = 3), labels = c('Light Strike Center', 'Light Strike Ellipse', 'Turbine Center',
    'Turbine Attraction Region')) +
    scale_color_manual(name = '', values = c("LightStrikeCenter" = 'blue', "TurbineCenter" = 'red', "LightStrikeEllipse" = 'green',
    "TurbineRadius" = 'violet'), labels = c('Light Strike Center', 'Light Strike Ellipse', 'Turbine Center',
    'Turbine Attraction Region')) + xlab('Longitude') + ylab('Latitude') +
    ggtitle('Area of Overlap between Lightning Strike Ellipse and Attractive Region around a Turbine') +
    theme(legend.position = 'bottom', legend.title = element_text(size = 20), legend.text = element_text(size = 20),
          plot.title = element_text(size = 20, hjust = 0.5), axis.title = element_text(size = 20),
          axis.text = element_text(size = 20))


lightStrikeEllipseData <- getEllipseData(lightStrikeCenters[iLightStrike, ], lightStrikeRadius[iLightStrike, ],
                              StormData$angle[iLightStrike])
tempPos <- cartesian2LatLon(lightStrikeEllipseData$x*1000, lightStrikeEllipseData$y*1000); # input to this function has to be in meters
lightStrikeEllipseData$lon <- tempPos[,1];
lightStrikeEllipseData$lat <- tempPos[,2];

turbineCircleData <- getEllipseData(turbineCenters[iTurbine, ], turbineRadius, 0)
tempPos <- cartesian2LatLon(turbineCircleData$x*1000, turbineCircleData$y*1000); # input to this function has to be in meters
turbineCircleData$lon <- tempPos[,1];
turbineCircleData$lat <- tempPos[,2];

hfigOverLapArea = hfigOverLapArea +
    geom_path(data = turbineCircleData, aes(lon, lat, group = grp), color = 'magenta') +
    geom_path(data = lightStrikeEllipseData, aes(lon, lat, group = grp), color = 'green')

hfigOverLapArea <- hfigOverLapArea + coord_fixed()
hfigOverLapArea <- hfigOverLapArea + coord_cartesian(xlim = c(-100.68, -100.645), ylim = c(32.5525, 32.59))
print(hfigOverLapArea)

ggsave(hfigOverLapArea, file = "Figures/TurbineOverlapArea.png", width = 16, height = 9, type = "cairo-png")
# ggsave(hfigOverLapArea, file = "Figures/TurbineOverlapArea.eps")




# plotDataX = rbind(lightStrikeCenters[iLightStrike, 1], turbineCenters[iTurbine, 1], transpose(xyPoints[pointsInStrikeEllipse, 1]),
#                   transpose(xyPoints[pointsInTurbineRadius, 1]))
# plotDataY = rbind(lightStrikeCenters[iLightStrike, 2], turbineCenters[iTurbine, 2], transpose(xyPoints[pointsInStrikeEllipse, 2]),
#                   transpose(xyPoints[pointsInTurbineRadius, 2]))
# plotDataStr = c('LightStrikeCenter', 'TurbineCenter', rep('LightStrikeEllipse', length(xyPoints[pointsInStrikeEllipse, 1])),
#               rep('TurbineRadius', length(xyPoints[pointsInTurbineRadius, 1])))
#
# plotDataFrame = data.frame(x = plotDataX, y = plotDataY, type = plotDataStr)
# ggplot(data = NULL, aes(xyPoints[,1], xyPoints[,2])) + geom_point(size = 0.1) +
# geom_point(inherit.aes = F, data = plotDataFrame, aes(x, y, col = type, shape = type), size = 3) +
#     scale_shape_manual(name = '', values =
#     c("LightStrikeCenter" = 16, "TurbineCenter" = 15, "LightStrikeEllipse" = 1, "TurbineRadius" = 3),
#     labels = c('Light Strike Center', 'Light Strike Ellipse', 'Turbine Center', 'Turbine Attraction Region')) +
#     scale_color_manual(name = '', values = c("LightStrikeCenter" = 'blue', "TurbineCenter" = 'red', "LightStrikeEllipse" = 'green',
#                                   "TurbineRadius" = 'violet'),
#     labels = c('Light Strike Center', 'Light Strike Ellipse', 'Turbine Center', 'Turbine Attraction Region')) +
#     xlab('x-position') + ylab('y-position')