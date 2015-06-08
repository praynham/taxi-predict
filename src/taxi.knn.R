# TAXI KNN - Build and run a kNN prediction model on taxi-trip data sets
#
# Parameters:
#    train - training data frame of taxi trips
#    test - testing data frame of taxi trips to be predicted
#    k=10 - see taxi.knn.trial
#    weights=NULL - see taxi.knn.trial
#    scale=T - see taxi.knn.trial3
#
# Returns:
#    Array of predictions:
#    - one row per test trip
#    - columns: predicted longitude, predicted latitude,
#        error in longitude prediction, error in latitude prediction.
#    Prediction error is a measure of how many standard deviations
#    the prediction is from the actual trip.

taxi.knn <- function( train, test, k=10, weights=NULL, scale=T ) {
  #
  # Create array to hold the results
  #
  predicts <- matrix( ncol=5, nrow=nrow(test),
      dimnames=list( NULL, c('lon.pred','lat.pred','lon.error','lat.error','dist.error') ))
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
    pred.locat <- taxi.knn.trial(
        train, test[ix,],
        k=k, weights=weights, scale=scale )
    #
    # Compare the predicted destination against the actual destination
    #
    act.locat <- c( test[ix,ncol(test)-1], test[ix,ncol(test)] )
    predicts[ix,1] <- pred.locat[1]
    predicts[ix,2] <- pred.locat[2]
    deltas <- act.locat - pred.locat
    predicts[ix,3] <- deltas[1] / sd.lon
    predicts[ix,4] <- deltas[2] / sd.lat
    predicts[ix,5] <- sqrt(( deltas[1]^2 + deltas[2]^2 ) / (sd.lon^2 + sd.lat^2)) 
  }
  #
  # All done, return our predictions and discrepancies
  #
  predicts
}

# TAXI KNN TRIAL - Do a KNN prediction for a taxi in progress
#
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
#    Predicted destination, two-element vector (longitude, latitude)

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
  target.lons <- train[nearests,ncol(train)-1]
  target.lats <- train[nearests,ncol(train)]
  closeness <- 1 / (1 + gaps[nearests])
  lon <- sum(target.lons*closeness) / sum(closeness)
  lat <- sum(target.lats*closeness) / sum(closeness)
  #
  # Resulting mean location is our prediction of the trial's target location
  #
  c(lon,lat)
}

# SAMPLE RUN
#
# > weights
# [1] 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 1 1 1 1 1 0 0
# > taxi.knn( taxi.1k, taxi.5, weights=weights, k=10 )
#       lon.pred lat.pred   lon.error   lat.error dist.error
# [1,] -8.621335 41.15879 -0.46034186  0.01763746  0.3363483
# [2,] -8.682316 41.20539 -0.50274517 -0.30175245  0.4210296
# [3,] -8.614824 41.16283 -0.38101323  0.06518933  0.2817511
# [4,] -8.616557 41.16551  0.29407289 -0.51678523  0.4132604
# [5,] -8.623494 41.13386  0.09716505 -0.13389747  0.1157727
# > 
