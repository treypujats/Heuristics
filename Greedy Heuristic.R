rm(list=ls())

library(readxl)
arcs <- read_excel("OPER 623 - Heuristics/Lab 1 Data.xlsx")

n<-nrow(arcs)

visited<-rep(0,n)
arcstoured=matrix(0,nrow = n,ncol = n)

num_visited=0
total_cost=0
k=0
z=0

while (num_visited<2*n) {
  nearest_cost=Inf
  for (i in n) {
    for (j in n) {
      if (i!=j && arcs(i,j)<nearest_cost && arcstoured[i,j]!=1 && visited[i]<2 && visited[j]<2) {
        nearest_cost=arcs(i,j)
        k=i
        z=j
      }
    }
  }
total_cost=total_cost+nearest_cost
visited[k]=visited[k]+1
visited[z]=visited[z]+1
num_visited=sum(visited)
arcstoured[k,z]=1
  
if (num_visited<n) {
  visited[k]!= visited[z]
  }
}
arcstoured
