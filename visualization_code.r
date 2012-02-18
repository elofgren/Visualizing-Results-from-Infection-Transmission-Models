###########################################################
# Setting Global Variables, Modifying Data Sets as Needed #
###########################################################
#Importing data sets into R
epidemic_timeseries <- read.csv("epidemic_timeseries.csv", sep=",", header= F)

timeseries_matrix <- as.matrix(epidemic_timeseries)
max_length <- 22 # Number of observations in epidemic_timeseries - 1
realizations <- 1000 # How many realizations of the model were run

#Create a new vector with the total number of cases for each epidemic
epidemic_sizes <- colSums(timeseries_matrix, na.rm = TRUE)
median_size<- median(epidemic_sizes)
upper_size <- quantile(epidemic_sizes, probs = c(0.975), na.rm=TRUE)
lower_size <- quantile(epidemic_sizes, probs = c(0.025), na.rm=TRUE)

#Create a long dataset with all epidemics in a single column with a timestamp
stack_series <- stack(epidemic_timeseries)
timestamp <- data.frame(rep(seq(1,23),realizations))
long_series<- cbind(stack_series,timestamp)
long_series<- long_series[,-2] #delete extraneous column

#Find the median and pointwise 2.5th and 97.5th quantiles for each simulated timestep
median_line <- apply(epidemic_timeseries, 1, median, na.rm = TRUE)
pointwise_CI <- apply(epidemic_timeseries, 1, quantile, probs = c(0.025, 0.975),  na.rm = TRUE)

#Select possible realizations representing the median, 2.5th and 97.5th percentiles of epidemic size
#Draw 4 of each
median_candidates <- which(epidemic_sizes == median_size)
median_sample <- sample(median_candidates, 4)
median_realizations<- epidemic_timeseries[median_sample]

upper_candidates <- which(epidemic_sizes == upper_size)
upper_sample <- sample(upper_candidates, 4)
upper_realizations<- epidemic_timeseries[upper_sample]

lower_candidates <- which(epidemic_sizes == lower_size)
lower_sample <- sample(lower_candidates, 4)
lower_realizations<- epidemic_timeseries[lower_sample]

############
# Figure 2 #
############
#Histogram of Final Epidemic Sizes
pdf('epidemic_histogram.pdf')
hist(epidemic_sizes, breaks = 20, col = "grey", xlab = "Final Epidemic Size", ylab = "Number of Realizations", main="")
dev.off()

#############
# Figure 3A #
#############
#Plot four realizations each of the median, 2.5th and 97.5th percentiles of epidemic sizes
pdf('final_size_intervals.pdf')
plot(0:max_length, rep(NA,max_length+1),main="", xlab="Time",ylab="Infections", ylim=c(0,20))
for (i in 1:4){
	lines(0:max_length,median_realizations[,i], lwd = 2, col = "black")
	lines(0:max_length,upper_realizations[,i], lwd = 1, col = "black", lty = 3)
	lines(0:max_length,lower_realizations[,i], lwd = 1, col = "black", lty = 3)
}
dev.off()

#############
# Figure 3B #
#############
#Plot the pointwise median, 2.5th and 97.5th percentiles of the simulations
pdf('pointwise_intervals.pdf')
plot(0:max_length, rep(NA,max_length+1), main="", xlab="Time",ylab="Infections",ylim=c(0,20))
lines(0:max_length,median_line,lwd=2,col="black") #Plot Median Line
lines(0:max_length,pointwise_CI[1,], lwd=1, col="black", lty = 3) #Plot Lower Interval
lines(0:max_length,pointwise_CI[2,], lwd=1, col="black", lty=3) #Plot Upper Interval
dev.off()

#############
# Figure 3C #
#############
#Plot of each run individually, with transparency set to 70% transparent
pdf('overplotted_lines.pdf')
plot(0:max_length, rep(NA,max_length+1),main="",xlab="Time", ylab="Infections", ylim=c(0,20))
for (i in 1:realizations) {
  lines(0:max_length, timeseries_matrix [ ,i],lwd=2,col=rgb(100,100,100,30,maxColorValue=255))
}
dev.off()

#############
# Figure 3D #
#############
#Smooth scatterplot of the epidemic data, with 128 bins, and plotting of the individual points turned off
pdf('smoothed.pdf')
smoothScatter(long_series[,2],long_series[,1],nbin=128,nrpoints=0,main="", xlab="Time",ylab="Infections", ylim=c(0,20))
dev.off()

#########################
# Supplemental Figure 1 #
#########################
# Smooth greyscale scatterplot similar to Figure 3D, with the addition of a single overlayed trajectory of interest
# In this example, the trajectory plotted is one of the candidate "median" realizations
pdf('smooth_with_trajectory.pdf')
smoothScatter(long_series[,2],long_series[,1],nbin=128,nrpoints=0,main="", xlab="Time",ylab="Infections", ylim=c(0,20),colramp=colorRampPalette(RColorBrewer::brewer.pal(9,"Greys")))
single_trajectory <- epidemic_timeseries[sample(median_candidates, 1)]
lines(single_trajectory, lwd = 3, col = "red")
dev.off()