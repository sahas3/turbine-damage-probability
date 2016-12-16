estimateBivariateGaussianParams <- function(ellipseRadius, ellipseAngle, ellipseConfidence = 0.99)
{
    # estimates covariance matrix for the bivariate gaussian distribution of location errors

    # chiSqVal = qchisq(ellipseConfidence, df = 2);
    # eigVals = ellipseRadius/sqrt(chiSqVal)
    #
    # tempVal1 = sum(eigVals)
    # tempVal2 = cos(ellipseAngle)*sin(ellipseAngle)*(-diff(eigVals))
    #
    # covMat = ginv(matrix(c(tempVal1, tempVal2, tempVal2, tempVal1), ncol = 2))

    nEllipses = length(ellipseAngle)
    ellipseParams = matrix(0, nEllipses, 6)

    if (nEllipses == 1)
    {
        ellipseRadius = matrix(ellipseRadius, nrow = 1, ncol = 2)
    }

    for (iEllipse in 1:nEllipses)
    {
        rotMat = matrix(c(cos(ellipseAngle[iEllipse]), -sin(ellipseAngle[iEllipse]),
                          sin(ellipseAngle[iEllipse]), cos(ellipseAngle[iEllipse])), ncol = 2)
        # covMat = rotMat %*% diag(sort(ellipseRadius[iEllipse,], decreasing = T)) %*% t(rotMat)
        covMat = t(rotMat) %*% diag(ellipseRadius[iEllipse,]^2) %*% rotMat
        ellipseParams[iEllipse, 1:4] = c(covMat)


        ## recompute ellipse angle to verify estimation

        eigenValues = eigen(covMat)
        angle = atan2(eigenValues$vectors[2,1], eigenValues$vectors[1,1])

        # wrap angle between 0 to 2*pi
        angle = wrapToPi(angle, pi) # ifelse(angle < 0, angle + pi, angle)

        ellipseParams[iEllipse, 5] = angle

        diffAngle = abs(angle - ellipseAngle[iEllipse])

        # check if estimation is true or not
        ellipseParams[iEllipse, 6] = as.numeric((diffAngle < 1e-10) | (abs(diffAngle - pi) < 1e-10))

        # (sum(as.numeric(eigenValues$values == sort(ellipseRadius[iEllipse,], decreasing = T))) == 2 &
        #                                     abs(angle - ellipseAngle[iEllipse]) < 1e-6)

    }

    return(ellipseParams)

}
