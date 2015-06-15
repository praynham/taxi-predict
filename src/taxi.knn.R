# TAXI KNN - Build and run a kNN prediction model on taxi-trip data sets

# Parameters:
#    train - training data frame of taxi trips
#    test - testing data frame of taxi trips to be predicted
#    k=10 - see taxi.knn.trial
#    weights=NULL - see taxi.knn.trial
#    scale=T - see taxi.knn.trial
#
# Returns:
#    Array of predictions:
#    - one row per test trip
#    - columns: predicted longitude, predicted latitude,
#        error in longitude prediction, error in latitude prediction,
#        actual longitude, actual latitude.
#    Prediction error is a measure of how many standard deviations
#    the prediction is from the actual trip.

taxi.knn <- function( train, test, k=10, weights=NULL, scale=T ) {
  #
  # Create array to hold the results
  #
  predicts <- matrix( ncol=10, nrow=nrow(test),
      dimnames=list( NULL, c(
        'trip.id', 'intvl', 'lon.pred','lat.pred','pred.range',
        'lon.dev','lat.dev','dist.error','lon.act','lat.act') ))
  #
  # Standard deviation of the destination locations
  # 
  sd.lon <- sd( train[,ncol(train)-1])
  sd.lat <- sd( train[,ncol(train)])
  #
  # For each test trip ...
  #
  for (ix in 1:nrow(test)) {
    #
    # Calculate the trip's predicted destination
    #
    prediction <- taxi.knn.trial(
        train, test[ix,],
        k=k, weights=weights, scale=scale )
    pred.locat <- prediction[c(1,2)]
    pred.deviat <- prediction[c(3,4)]
    #
    # Compare the predicted destination against the actual destination
    #
    predicts[ix,1] <- test[ix,'trip.id']
    predicts[ix,2] <- test[ix,'snap.time']
    act.locat <- c( test[ix,ncol(test)-1], test[ix,ncol(test)] )
    predicts[ix,3] <- pred.locat[1]
    predicts[ix,4] <- pred.locat[2]
    predicts[ix,5] <- haver.dist( pred.locat, pred.locat+pred.deviat ) / 1000
    deltas <- act.locat - pred.locat
    predicts[ix,6] <- sd.lon
    predicts[ix,7] <- sd.lat
    predicts[ix,8] <- haver.dist( act.locat, pred.locat ) / 1000
    predicts[ix,9] <- test[ix,"lon.00"]
    predicts[ix,10] <- test[ix,"lat.00"]
  }
  #
  # All done, return our predictions and discrepancies
  #
  predicts
}

# TAXI KNN TRIAL - Do a KNN prediction for a taxi in progress

# Parameters:
#    train - training data frame of taxi trips
#    trial - data frame of one taxi trip, to be predicted
#    k=10 - number of nearest neighbours to consider
#    weights=NULL - vector of numbers, one per column of train, specifying
#        relative importance of the column (dimension) for measuring
#        distances to neighbours. 0 = ignore this column. if NULL,
#        then columns day.hour, lon.start, and lat.start are 1, all else 0.
#    scale=T - T => scale each dimension to inverse of standard deviation
#
# Returns:
#    Predicted destination, and estimated error (1SD) of prediction as two-element 
#    vector (longitude, latitude, lon.error, lat.error)

taxi.knn.trial <- function( train, trial, k=10, weights=NULL, scale=T ) {
  #
  # Validate caller's arguments
  #
  k <- min( k, nrow(train) )
  if (is.null(weights)) {
    weights <- rep( 0, ncol(train) )
    weights[c(8,13,14)] <- 1
  }
  #
  # Adjust longitude weights, so that they have same weight as latitudes
  # when measured in distance (km), so that we calc good euclidian dists
  #
  lon.adjustment <- cos(41.155*pi/180)
  lon.cols <- grep( "lon", names(train) )
  weights[lon.cols] <- weights[lon.cols] * lon.adjustment
  #
  # Calculate distance of each training point from the trial point...
  #
  gaps <- rep( 0, nrow(train) )
  for (icol in 1:ncol(train)) {
    if (weights[icol] != 0) {
      #
      # For each dimension (data attribute), caculate euclidian distance,
      # then scale up by weight, and normalize by standard deviation
      #
      deltas <- train[,icol] - trial[1,icol]
      weight <- weights[icol]
      stddev <- 1
      if (scale) {
        stddev <- sd(train[,icol])
        if (stddev == 0) stddev <- 1
      }
      gaps <- gaps + (deltas*weight/stddev) ^ 2
    }
  }
  #
  # Find the k nearest points (smallest gap) to the trial point
  #
  nearests <- order( gaps, decreasing=F )[1:k]
  #
  # Calculate the mean of the target locations of the k nearest points.
  # Apply a weighting, so that targets of closer points count for more
  # than targets of further points.
  #
  closeness <- 1 / (1 + gaps[nearests])
  target.lons <- train[nearests,ncol(train)-1]
  target.lats <- train[nearests,ncol(train)]
  lon <- sum(target.lons*closeness) / sum(closeness)
  lat <- sum(target.lats*closeness) / sum(closeness)
  sd.lon <- sd(target.lons)
  sd.lat <- sd(target.lats)
  #
  # Resulting mean location is our prediction of the trial's target location.
  # return the prediction, and the standard deviation of all the neighbours
  #
  c( lon, lat, sd.lon, sd.lat )
}

# HAVER DIST - return precise distance (m) between two points (lon/lan deg)
#
# HaverDist is accurate for all pairs of points because it accounts for
# Earth's spherical shape. The distance is caculated along a great circle
# that connects the two points.

haver.dist <- function( point1, point2 ) {
  radius <- 6371000   # radius of earth in metres
  lon1 <- point1[1] * pi/180   # convert degrees to radians
  lat1 <- point1[2] * pi/180
  lon2 <- point2[1] * pi/180
  lat2 <- point2[2] * pi/180
  haver <- haver.sin(lat1-lat2) + cos(lat1)*cos(lat2)*haver.sin(lon1-lon2)
    # angle between two points relative to center of earth
  dist <- 2*radius*asin(sqrt(haver))   # length of great arc    
  dist
}

# HAVER SIN - "Half versed sign" trig function

haver.sin <- function( angl ) {
  sin(angl/2)^2
}    
