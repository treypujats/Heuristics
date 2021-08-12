
# Initialize x solutions --------------------------------------------------

#test_ndx=
test_names=c("eil51","ts225","pr1002","gr120","rat195","Bays29","Berlin52","Cho130","KroA100","pcb442","pr76","pr76","gr48","pma343")
#optimal_dist=c(426,126643,259045,6942,2323,2020,7542,6110,21282,50778,108159,5046,1368)
#optimal_distance=optimal_dist[test_ndx]

library(readxl)
#Lab_Data <- read_excel("OPER 623 - Heuristics/TSP Data-1.xlsx", 
 #   sheet = "eil51", col_names = FALSE)
Lab_Data<-TSP_Data_1
#Read the data in as a matrix than can be manipulated
Data<-as.matrix.data.frame(Lab_Data)

#Count the number of rows since it is a n by n matrix with n cities
n<-nrow(Data)

current_best_cost<-Inf




#Also recommend the below code for getting percent error

#(best_distance-optimal_distance)*100/optimal_distance

xvalue<-vector()
yvalue<-vector()
  





#Choose starting location
max_iterations<-175
#Intialize variables
counter<-1


top_percentage<-.2
x=5 #number of initial solutions
population<-matrix(0L, nrow = 1, ncol = n)
top_index_cutoff <- round(x*top_percentage)

# Greedy Algorithm --------------------------------------------------------

for (i in 1:x) {
  tour<-sample(n,n,replace = FALSE)
  population<-rbind(population,tour)
}

population<-population[-c(1),]
population<-cbind(population,0L)

while (max_iterations>counter) {
  


for (i in 1:x) {
test_cost=0
  for (k in 1:n-1){
    test_cost = sum(Data[population[i,k],population[i,k+1]] , test_cost);   #find the cost of potential new tour.
  } 
  test_cost = sum(Data[population[i,n],population[i,1]] , test_cost)
  population[i,n+1]<-test_cost
  
}

colnames(population)[n+1]<-"TourCost"
population <- population[order(-population[,n+1], decreasing = TRUE),]


chosen_top_tour<-sample(top_index_cutoff,1)


for (q in 1:top_index_cutoff) {
  
  super_test_tour = population[q,1:n]
  
i=0
while (i < n-1){   #While i=0...size-1 
  i<-i+1; #increment i
  j<-i+1; #initalize j, note this means inner loop is always starting at i+1
  
  #inner j loop
  while (j < n){ #While j=i+2...size 
    test_tour = replace(super_test_tour, i:j, rev(super_test_tour[i:j]));
    k=1;
    test_cost = 0;
    for (k in 1:n-1){
      test_cost = sum(Data[test_tour[k],test_tour[k+1]] , test_cost);
    } 
    test_cost = sum(Data[test_tour[n],super_test_tour[1]] , test_cost)
  
    if (test_cost < population[q,n+1]) {
    population[q,1:n] = test_tour;
    best_cost = test_cost;
    #j=n
    #i=n
    
    }
    j<-j+1; #increment j
  }
}
  
}

for (i in 1:top_index_cutoff) {
  test_cost=0
  for (k in 1:n-1){
    test_cost = sum(Data[population[i,k],population[i,k+1]] , test_cost);   #find the cost of potential new tour.
  } 
  test_cost = sum(Data[population[i,n],population[i,1]] , test_cost)
  population[i,n+1]<-test_cost
  
}


# Update Bad Solutions ----------------------------------------------------

index_bad_solutions<-top_index_cutoff+1

crossover_to_offspring=function(gene1, gene2, co_point) {
  offspring1=matrix()
  offspring1=gene1[1:co_point]
  #offspring2=gene2[1:co_point]
  j=co_point
  #k=j
  for(i in 1:n){ 
    if ((gene2[i]%in%offspring1)==FALSE){
      j=j+1 #keeps track of index of next empty in offspring
      offspring1[j]=gene2[i]
    }
    # if (gene1[i]%in%offspring1=FALSE){
    #   k=k+1 #keeps track of index of next empty in offspring
    #   offspring2[k]=gene1[i]
    # }
  }
  return(offspring1)
}

pop_before_crossover<-population
oldcosts<-sort(population[,n+1])

for (i in index_bad_solutions:x) {
  Good_Parent<-population[sample(top_index_cutoff,1),1:n]
  Bad_Parent<-population[i,1:n]
  Crossover_Point<-sample(n,1)
  child<-crossover_to_offspring(Good_Parent, Bad_Parent, Crossover_Point)
  population[i,1:n]<-child
}


for (i in index_bad_solutions:x) {
  test_cost=0
  for (k in 1:n-1){
    test_cost = sum(Data[population[i,k],population[i,k+1]] , test_cost);   #find the cost of potential new tour.
  } 
  test_cost = sum(Data[population[i,n],population[i,1]] , test_cost)
  population[i,n+1]<-test_cost
  
}



population <- population[order(-population[,n+1], decreasing = TRUE),]
iteration_best_cost<-population[1,n+1]
iteration_best_tour<-population[1,1:n]

if (iteration_best_cost<current_best_cost) {
  best_tour<-iteration_best_tour
  current_best_cost<-iteration_best_cost
}
counter<-counter+1
print(counter)
print(current_best_cost)
yvalue<-rbind(yvalue,current_best_cost)
xvalue<-rbind(xvalue,counter)

}

current_best_cost

plot(xvalue,yvalue)
