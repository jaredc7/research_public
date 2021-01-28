library(tidyverse)

setwd("C:/Users/jca305/sfuvault/jaredChiu/R-Scripts")
firmcode <- read_csv("CleanFirm.csv")
jsrfinal <- read_csv("new_final.csv") %>% 
  select(-X1) # %>% 
  left_join(firmcode,by=c("MemberID" = "memberid")) %>% 
  select(JSR,id,periodindex)


library(igraph)
graph_list <- list()
pairwisecolab <- data.frame()
for(i in 2:63){
  
  current_period <- jsrfinal %>% filter(periodindex == i) %>% 
    select(JSR,MemberID) 
  
  past_period <- jsrfinal %>% filter(periodindex == i - 1) %>% 
    select(JSR,MemberID) 
  
  twomode <- current_period %>%  filter(MemberID %in% past_period$MemberID)
  
  two_mode_network <- graph.incidence(twomode)
  one_mode_networks <- bipartite.projection(two_mode_network)
  n = dim(one_mode_networks)
  pairwisecolab[i-1,] <- ((n*n)-n)/2
  
 
}

i=2
current_period <- jsrfinal %>% filter(periodindex == i) %>% 
  select(JSR,MemberID) 

past_period <- jsrfinal %>% filter(periodindex == i - 1) %>% 
  select(JSR,MemberID) 

twomode <- current_period %>%  filter(MemberID %in% past_period$MemberID)

two_mode_network <- graph.incidence(twomode)
one_mode_networks <- bipartite.projection(two_mode_network)
n = dim(one_mode_networks$proj1)
pairwisecolab[i-1,] <- ((n*n)-n)/2


jsrquarter_finish$lastdt









# i=2
# 
# library(reshape2)
# 
# links <- table(melt(twomode, id.var="JSR"))
# 
# head(links)
# current_period <- jsrfinal %>% filter(periodindex == i)
# past_period <- jsrfinal %>% filter(periodindex == i - 1)
# 
# twomode <- current_period %>%  filter(MemberID %in% past_period$MemberID) %>%
#   select(JSR,MemberID) %>%   distinct(JSR,MemberID)
# 
# two_mode_network <- graph.incidence(links)
# 
# one_mode_networks <- bipartite.projection(two_mode_network)
# 
# t <- one_mode_networks$proj1
# 
# 
# 
# 
# graph_list[[i-1]] <- one_mode_networks
# 
# dim(graph_list[[1]])




##############
#WORKING CODE#
##############

new_final <- read.csv("new_final.csv")

period <- new_final %>% group_by(yq) %>% 
  select(yq,periodindex) %>% 
  distinct(yq,.keep_all = T)

quarter_jsr <- quarter_finish %>% 
  left_join(period, by = c('yq_end' = 'yq')) %>% 
  filter(!is.na(periodindex))


library(Matrix)
library(igraph)

output = list()
for(i in 1:nrow(quarter_jsr)){
  perioddata = quarter_jsr[i,]
  
  if(perioddata$periodindex != 1){
    periodjsr = perioddata$JSR
    periodcurrent = perioddata$periodindex
    
    periodprior = periodcurrent - 1
    
    aff_current <- new_final %>% 
      filter(periodindex == periodcurrent) %>% 
      distinct(JSR)
    
    aff <- new_final %>% 
      filter(periodindex == periodprior &
               JSR%in%aff_current$JSR)
    
    A <- spMatrix(nrow=length(unique(aff$MemberID)),
                  ncol = length(unique(aff$JSR)),
                  i = as.numeric(factor(aff$MemberID)),
                  j= as.numeric(factor(aff$JSR)),
                  x = rep(1, length(as.numeric(aff$MemberID))))
    
    row.names(A) <- levels(factor(aff$MemberID))
    colnames(A) <- levels(factor(aff$JSR))
    Arow <- A %*% t(A)
    
    statusdeg_1 <- sum(Arow[,periodjsr])
    #statusbet <- 
    
    
     g <- graph_from_adjacency_matrix(Arow)
     statusdeg = centr_degree(g,mode ="all")$centralization
    # statusbet= centr_betw(g)$centralization
    # statusclo = centr_clo(g,mode='all')$centralization
    #
    Arow[upper.tri(Arow,diag=F)]<-0
    pair_collab = sum(Arow)
    
    dt <- data.frame(periodjsr,pair_collab,statusdeg,statusdeg_1)#,statusbet,statusclo)
    output[[i]] <- dt
  }
  
}

plot(g)

output <- bind_rows(output)
Arow

i = 1

perioddata = quarter_jsr[i,]
periodjsr = perioddata$JSR
periodcurrent = perioddata$periodindex

periodprior = periodcurrent - 1

aff_current <- new_final %>% 
  filter(periodindex == periodcurrent) %>% 
  distinct(JSR)

aff <- new_final %>% 
  filter(periodindex == periodprior &
           JSR%in%aff_current$JSR) %>% 
  select(JSR,MemberID)

graph_from_i


as.symmetricAdjacencymatrix(aff)

A <- spMatrix(nrow=length(unique(aff$MemberID)),
              ncol = length(unique(aff$JSR)),
              i = as.numeric(factor(aff$MemberID)),
              j= as.numeric(factor(aff$JSR)),
              x = rep(1, length(as.numeric(aff$MemberID))))

row.names(A) <- levels(factor(aff$MemberID))
colnames(A) <- levels(factor(aff$JSR))
Arow <- A %*% t(A)
g <- graph_from_adjacency_matrix(Arow)
degree(g,loops =F)


