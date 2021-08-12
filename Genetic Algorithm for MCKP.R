########################################################################################
#                    Genetic Algorithm code by Lt Col Cox
#                 0-1 Multi-Constraint Knapsack Problem (MCKP)
#                     Code follows outline found here:
#             http://www.micsymposium.org/mics_2004/Hristake.pdf
########################################################################################

#Pull in MCKP data from excel (Value, Weight, Volume), then calculate number of items in data and assign it to datasize
library(readxl)
MCKP_Data <- read_excel("C:\\Users\\treyp\\OneDrive\\Documents\\OPER 623 - Heuristics\\MCKP Lab Data.xlsx", col_names = FALSE, skip = 1)
MCKP<-as.matrix(MCKP_Data)
datasize <- dim(MCKP)[1]

#Define constraints - These would need to be added/modified to match instance data
max.weight <- 55     #55 lbs is a nice pack weight
max.vol <- 4000   #4000 cubic inches is my max pack capacity

#Generate 'popsize' number of Random Parents.  
popsize <- 30 #user tunable parameter for population size
population<-matrix(rbinom(n=datasize*popsize, size=1, prob=0.5),ncol = datasize, byrow = TRUE) 

#Create a function to be called in main body of code that calculates the objective function for each chromosome (i.e Solution)
temp.value <-c(1:popsize)
value <- function(matrix){
  for (i in 1:popsize) {temp.value[i] = sum(matrix[i, ]*MCKP[, 1])}
  return(temp.value)
}

##Create a function to be called in main body of code that calculates the weight of each chromosome
temp.weight <-c(1:popsize)
weight <- function(matrix){
  for (i in 1:popsize) {temp.weight[i] = sum(matrix[i, ]*MCKP[, 2])}
  return(temp.weight)
}

#Create a volume checking function
temp.vol <-c(1:popsize)
volume <- function(matrix){
  for (i in 1:popsize) {temp.vol[i] = sum(matrix[i, ]*MCKP[, 3])}
  return(temp.vol)
}

#Create weight feasibility function, this takes an infeasible solution and forces random 1s to 0s until each solution is feasible
feasible.weight <-function(vector, matrix){
  for (i in 1:popsize){
    while(vector[i] > max.weight) {
      indx=sample(1:datasize, size=1)  
      if(matrix[i, indx]==1) {matrix[i, indx]=0}
      vector[i] = sum(matrix[i, ]*MCKP[, 2])
    }
  }
  return(matrix)
}

#Create volume feasibility function, this takes an infeasible solution and forces random 1s to 0s until each solution is feasible
feasible.vol <-function(vector, matrix){
  for (i in 1:popsize){
    while(vector[i] > max.vol) {
      indx=sample(1:datasize, size=1)  
      if(matrix[i, indx]==1) {matrix[i, indx]=0}
      vector[i] = sum(matrix[i, ]*MCKP[, 3])
    }
  }
  return(matrix)
}

#Initialize some variables used in loop below
stop=0
count=0
graphvector<-matrix()
mutate <- .01


#GA LOOP 
#This loop is the meat of the Genetic Algorithm
# OUTLINE
#1) Calculates fitness, weight and volume for each solution, if solutions are not feasible it forces feasibility of population
#2) Probabalistically chooses two solutions to be parents (better solutions have higher odds of being choosen)
#3) Randomly generates a crossover point and creates a child
#4) Mutate the child, this is necessary to add some variability back into the system else you converge to local optima very quickly 
#5) Compares child versus worst member of population and replaces it if better
while(stop == 0) {
  #1) Calculate objective values, weights and volumes
  pop.value  <- value(population);
  pop.weight <- weight(population);
  pop.vol    <- volume(population);
  
  #Force Feasibility by randomly bit flipping a 1 to a 0 until feasible
  population <- feasible.weight(pop.weight, population);
  population <- feasible.vol(pop.vol, population);
  
  #After forcing feasibility need to recheck objective values, weights and volumes
  pop.value  <- value(population);
  pop.weight <- weight(population);
  pop.vol    <- volume(population);
  
  #check all vectors pairwise for equal fitness, stop when 90% of population have same fitness 
  match <- 0;  # Initalize count of chromozone match, when 45/50 chromosones match stop algorithm
  for(i in 1:(popsize-1)) {
    for (j in (i+1):popsize) {
      if (pop.value[i]==pop.value[j]) {match = match + 1}}};
  
  
  #2) Choose Parents.  Code block sorts population according to fitness, the creates a step function so that higher fitness 
  #members are more likely to be choosen, chooses a mom and dad accordingly.  Then randomly chooses a chrossover point
  #and creates a child
  ndx <- order(pop.value, decreasing=TRUE)[1:popsize]; #create an index of chromosones in desending order of value (want to choose more frequently from top then bottom)
  #Create integer sector cutoff values.  i.e divide population into integer sized groups, group 1 is highest quality, group 4 is lowest quality 
  group1 <- round(popsize*.1,0); #Top 10% of candidates
  group2 <- round(popsize*0.25,0) + group1; #Next best 25% of candidates
  group3 <- round(popsize*0.50,0) + group2; #Next best 50% of candidates
  group4 <- popsize; #Bottom 15% of candidates  
  
  #Get "Dads" index: First draw a random number (1,100) to determine which group to pick from then randomly draw from that group
  dad.sector <- sample(1:100,1); #Draw random number from 1 to 100
  if (dad.sector <= 400) {dad = sample(1:group1,1)}; #30% chance we grab from group 1
  if (dad.sector > 40 & dad.sector <=70) {dad = sample((group1+1):group2,1)}; #50% chance we grab from group 2
  if (dad.sector > 70 & dad.sector <=95) {dad = sample((group2+1):group3,1)}; #15% chance we grab from group 3
  if (dad.sector > 95 & dad.sector <=100) {dad = sample((group3+1):group4,1)}; #5% chance we grab from group 4
  #with step propability get "Moms" index
  #mom.sector <- sample(1:100,1); #create a random number telling you what sector to look for 'Mom' in
  ##if (mom.sector <= 30) {mom = sample(1:group1,1)}; #30% chance we grab from group 1
  #if (mom.sector > 30 & mom.sector <=80) {mom = sample((group1+1):group2,1)}; #50% chance we grab from group 2
  #if (mom.sector > 80 & mom.sector <=95) {mom = sample((group2+1):group3,1)}; #15% chance we grab from group 3
  #if (mom.sector > 95 & mom.sector <=100) {mom = sample((group3+1):group4,1)}; #5% chance we grab from group 4
  mom = sample((group3+1):group4,1)
  
  #3) Use these parent indexes and a random crossover point to create a child
  
     lower <- round(datasize*.25);
     mid<-round(datasize*.5)
  upper <- round(datasize*.75);
  crossover1 <- round(runif(1, min=lower, max=mid),0); #choose a random crossover point between 1/3 datasize and 2/3 datasize (i.e in middle of chromosomes)
  crossover2<-round(runif(1,min=mid+.1, max=upper))
   dads.ndx <- ndx[dad];
  moms.ndx <- ndx[mom];
  child <- c(population[dads.ndx,1:crossover1],population[moms.ndx,(crossover1+1):crossover2],population[dads.ndx,(crossover2+1):datasize]); 
  

    
  
  
  #4 Mutate child to avoid system converging to stagnant population
  for (i in 1:datasize) {
    if(sample(1:100,1) <= mutate*100){
      if(child[i]==1) {child[i]<-0}
      if(child[i]==0) {child[i]<-1}
    }
  };
  #need to check child's fitness values, force feasibility and then recheck child's fitness values
  #Calculate objective values, weights and volumes
  child.value  <- sum(child*MCKP[, 1]);
  child.weight <- sum(child*MCKP[, 2]);
  child.vol    <- sum(child*MCKP[, 3]);
  
  #Force Weight Feasibility by randomly bit flipping a 1 to a 0 until feasible
  while(child.weight > max.weight) {
    indx=sample(1:datasize, size=1);  
    if(child[indx]==1) {child[indx]=0};
    child.weight = sum(child*MCKP[, 2])
  };
  
  #Force Volume Feasibility by randomly bit flipping a 1 to a 0 until feasible
  while(child.vol > max.vol) {
    indx=sample(1:datasize, size=1) ; 
    if(child[indx]==1) {child[indx]=0};
    child.vol = sum(child*MCKP[, 3])
  };
  
  #After forcing feasibility need to recheck objective values, weights and volumes
  child.value  <- sum(child*MCKP[, 1]);
  child.weight <- sum(child*MCKP[, 2]);
  child.vol    <- sum(child*MCKP[, 3]);
  
  
  #5) If Child is better than worst member of population replace worst member of population with child
  worst.ndx <- ndx[popsize] ;
  if (pop.value[worst.ndx] < child.value) {population[worst.ndx, ] <- child}; 

  #Determine if you've met stopping criteria
  count=count+1;
  if(count > 10000){stop = 1};
  #maxmatch <- round(popsize*.9,0);
  #if(match > maxmatch){stop = 1};
  
  #Output Progress
  graphvector<-c(graphvector, pop.value[ndx[1]])
  
}
  plot(graphvector)
  #print(c("Child Value", child.value, "Best value:", pop.value[ndx[1]], "worst value:", pop.value[ndx[popsize]] ));

  pop.value[ndx[1]]