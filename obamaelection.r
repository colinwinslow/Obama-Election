# Adapted for ISTA 116 at the University of Arizona's School of Information Science
# from a script written for this article:
# http://simplystatistics.org/post/34635539704/on-weather-forecasts-nate-silver-and-the

# This is a highly simplified simulation that does not account for the electoral college.

# Set the estimated percent for Obama
# based on the average of several national polls
proportionObama = 0.505

# Set the standard deviation
# this measures the variability between the different polls.
sdObama = 0.01

# Function to simulate a single election
simulateElection <- function(prop,sd){
  return(rnorm(1,mean=prop,sd=sd))
}

# Simulate the percent Obama in 1000 elections
simulatedProportionObama = replicate(1000, simulateElection(proportionObama,sdObama))

# Calculate the percent of times Obama wins
percentObamaWin = mean(simulatedPercentObama > 0.5)
percentObamaWin