# Run a test on taxi data - predict a trip destination and graph the results - R

# Load training data - 2.5min snapshots of 100K trip records
#
source <- "D:\\CKME 136\\Taxi\\sample 100K.csv"
taxi.100k <- read.csv( source, header=T )
names(taxi.100k) <- tolower( sub( "_", ".", names(taxi.100k) ))

# Load test data - 0.25min snapsnots of 10 trip records
#
source <- "D:\\CKME 136\\Taxi\\sample 10x.csv"
taxi.10x <- read.csv( source, header=T )
names(taxi.10x) <- tolower( sub( "_", ".", names(taxi.10x) ))

# Create weighting vector
#
weights <- rep( 0, ncol(taxi.100k) )
names(weights) <- names(taxi.100k)
weights["lon.start"] <- 5.0
weights["lat.start"] <- 5.0
weights["lon.3p"] <- 2.0
weights["lat.3p"] <- 2.0
weights["lon.2p"] <- 3.0
weights["lat.2p"] <- 3.0
weights["lon.1p"] <- 4.5
weights["lat.1p"] <- 4.5
weights["lon.00"] <- 6.75
weights["lat.00"] <- 6.75

# Run the kNN model
#
model <- taxi.knn( taxi.100k, taxi.10x, k=30, weights=weights )

# Extract one taxi trip for graphing
#
model.13 <- data.frame( model[model[,'trip.id']==130111,] )

# Plot a graph of prediction quality over the duration of the trip
#
x <- model.13$intvl   # Minutes into trip
y <- model.13$dist.error   # Standard-deviation of applicable training destinations
z <- model.13$pred.range * 1.605 / sqrt(30)   # 90% Confidence interval
plot ( x, z1, col="blue", type="l", lty=1, ylim=c(0,7), xaxp=c(0,14,7),
  main="Porto Taxi Trip 13011", 
  xlab="Time since departure (minutes)",
  ylab="Distance (km)")
lines (x, y,col="red", lty=1)
lines (x, z,col="blue", lty=2)
legend( "topright", 
  c("90% obs. range", "90% confidence","prediction error"), 
  lty=c(1,2,1), col=c("blue","blue","red"), text.width=2.25)

# Plot the trip on a Google map.
# The circle scales are accurate when the map is displayed at 640 x 640 pixels.
#
# At start of trip (interval minute = (index-1)/4 after start)
#
plot.trip( porto, model.13b, 1, point.scale=2, circle.scale=0.30 ) # with CI=90%
plot.trip( porto, model.13b, 1, point.scale=2, circle.scale=1.02 ) # with SD=1
#
# At two minutes into trip
#
plot.trip( porto, model.13b, 9, point.scale=2, circle.scale=0.30 ) # with CI=90%
plot.trip( porto, model.13b, 9, point.scale=2, circle.scale=1.02 ) # with SD=1
#
# At end of trip (interval minute = (-index-1)/4 before end)
#
plot.trip( porto, model.13b, -1, point.scale=2, circle.scale=0.30 ) # with CI=90%
plot.trip( porto, model.13b, -1, point.scale=2, circle.scale=1.02 ) # with SD=1
