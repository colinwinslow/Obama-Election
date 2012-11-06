# in this exercise, we're going to calculate the expectation for Romney's electoral votes
# in a bunch of "swing states" and also Arizona. 


# these are the votes we are considering "settled"; we're not including them in the model, just adding them in at the end.
rsafe = 180 #'safe' electoral votes for Romney
osafe = 229 #'safe' electoral votes for Obama

# number of electoral college votes for swing states
electoralVotes = c(11,9,29,15,4,18,20,13,10)
names(electoralVotes) = c("AZ","CO","FL","NC","NH","OH","PA","VA","WI")

#### FIRST we need to figure out how likely Romney is to actually win each state. 
#### This isn't as simple as just looking at his polling average; a slight lead in
#### the polls actually translate into a significant lead in chance of winning, and
#### the next few lines of code model that.


#here are some state by state polls I found. They could totally be wrong, though. 

romneyPercent = c(
  0.535,  # Arizona
  .487,   # Colorado
  0.50,   # Florida
  0.51,   # North Carolina
  .479,   # New Hampshire
  0.483,  # Ohio
  .466,   # Pennyslvania
  0.49,   # Virginia
  0.475   # Wisconsin
  )

simulateVote = function(n){
  # returns TRUE when Romney wins
  # This is actually a really inefficient way of doing this; we could use what we know about bernoulli
  # random variables to avoid having to run all these simulations. But running simulations is more 
  # satisfying, right?
  result = sample(c("Romney","Obama"),size=1000,replace=T,prob = c(romneyPercent[n],1-romneyPercent[n]))
  return(length(result[result=='Romney'])>500)
}

simulateElections = function(n){
  #runs 1000 state elections and returns the proportion that Romney wins
  tf = replicate(2000,simulateVote(n))
  return(length(tf[tf==T])/2000)
  
}

romneyChanceOfWin = c(
  simulateElections(1),
  simulateElections(2),
  simulateElections(3),
  simulateElections(4),
  simulateElections(5),
  simulateElections(6),
  simulateElections(7),
  simulateElections(8),
  simulateElections(9)
  )
names(romneyChanceOfWin) = c("AZ","CO","FL","NC","NH","OH","PA","VA","WI")

print(romneyChanceOfWin)


#### Now that we have a reasonable idea how likely Romney is to win each state,
#### we can use that information to calculate the expected Electoral votes he
#### will earn from the states we're modeling.


romneyExpectation = sum(electoralVotes * romneyChanceOfWin)

# according to our model, if we could run the election over and over under the same circumstances,
# we'd expect Romney to average the following number of total electoral votes.
rsafe + romneyExpectation

# At the moment, things don't look very good for Romney. However, what if the polls have a bias? 
# What if Romney is actually doing better in Ohio than the polls would seem to indicate?
# Try playing with the numbers a bit... a few percent here and there could turn the tide of the election.


