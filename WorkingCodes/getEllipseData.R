# get points for drawing ellipses given center, semi major and minor axes and orientation

getEllipseData <- function(center, radius, angle, npts = 63)
{
    theta <- seq(0, 2*pi, length.out = npts)

    nEllipses <- length(angle);

    if (nEllipses == 1)
    {
        radius = matrix(radius, ncol = 2)
        center = matrix(center, ncol = 2)
    }

    ellipseData <- data.frame()
    for (iEllipse in 1:nEllipses)
    {
        # parametric values for plotting ellipses
        xData <- radius[iEllipse, 1]*cos(theta)*cos(angle[iEllipse]) - radius[iEllipse, 2]*sin(theta)*
            sin(angle[iEllipse]) + center[iEllipse, 1];
        yData <- radius[iEllipse, 1]*cos(theta)*sin(angle[iEllipse]) + radius[iEllipse, 2]*sin(theta)*
            cos(angle[iEllipse]) + center[iEllipse, 2];
        ellipseData <- rbind(ellipseData, data.frame(x=xData, y=yData, dataType = 'b', grp = iEllipse));
    }

    return(ellipseData);
}