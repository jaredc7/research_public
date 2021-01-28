
library(tidyverse)

#Firm-JSR Affiliation 
new_final <- read.csv("new_final_v1.csv") %>% 
  filter(JSR == 1 | JSR == 15)

#Ordered1 <- read.csv('Ordered1.csv')

#JSR Start and Stop 
quarter_finish <- read.csv('quarter_finish.csv')

# Arranging yq with a period index
period <- new_final %>% 
  group_by(yq) %>% 
  select(yq,periodindex) %>% 
  distinct(yq,.keep_all = T) %>% 
  transform(yq = as.numeric(yq)) %>% 
  arrange(yq)


jsr <- new_final %>% 
  group_by(JSR) %>% 
  distinct(JSR)

#Adding period index bsed on start and end yq
quarter_jsr <- quarter_finish %>% 
  left_join(period, by = c('yq_start'  = 'yq')) %>% 
  left_join(period, by = c('yq_end'='yq')) %>% 
  rename('index_start' = periodindex.x,
         'index_end' = periodindex.y)
  


# #%>% 
#   mutate(
#   periodindex1 = case_when(
#     yq_end == 20093 ~ 33, # Old 27
#     yq_end == 20094 ~ 34,
#     yq_end == 20183 ~ 61,
#     yq_end == 20193 ~ 61,
#     yq_end == 20211 ~ 61,
#     yq_end == 20201 ~ 61,
#     TRUE ~ as.numeric(periodindex)
#   )) %>% 
#   select(-periodindex) %>% 
#   rename(periodindex = periodindex1)
# 
# 
# quarter_finish %>% 
#   anti_join(jsr,by="JSR")


  
#filter(!is.na(periodindex))# & JSR %in% jsr)
# 
# df <- new_final %>% inner_join(quarter_jsr, by ="JSR") %>% 
#   select(JSR,MemberID,periodindex.x,unique_id) %>% 
#   rename(periodindex = periodindex.x) %>% 
#   arrange(periodindex)
# 
# library(igraph)
# library(Matrix)
# library(tidyverse)
# 
# quarter_jsr %>% 
#   distinct(periodindex) %>% 
#   arrange(periodindex)
# 
# output = list()
#   
# for(i in 1:nrow(quarter_jsr)){
# 
#   ## Period Index Data
#   perioddata = quarter_jsr[i,] 
#   
#   if(perioddata$periodindex != 1) {
#     ## Period Specific Data
#     periodjsr = perioddata$JSR
#     periodcurrent = perioddata$periodindex
#     
#     # Exactly 1 period after
#     #periodprior = periodcurrent - 1
#     
#     # One period available before the current period 
#     periodprior <- max(quarter_jsr$periodindex[quarter_jsr$periodindex < periodcurrent])
#     # find out which value that is
#     #periodprior <- which(quarter_jsr$periodindex == maxless)
#     
#     
#     ## New Data Set
#     member_current <- df %>% 
#       filter(periodindex == periodcurrent & 
#                JSR == periodjsr ) %>% 
#       distinct(MemberID)
#     
#     aff <- df %>% 
#       filter(periodindex <= periodprior &
#                MemberID %in% member_current$MemberID) %>% 
#       distinct(unique_id,.keep_all=T)
#     
#     ## Incidence Matrix
#     A <- spMatrix(nrow=length(unique(aff$MemberID)),
#                   ncol = length(unique(aff$JSR)),
#                   i = as.numeric(factor(aff$MemberID)),
#                   j= as.numeric(factor(aff$JSR)),
#                   x = rep(1, length(as.numeric(aff$MemberID))))
#     
#     row.names(A) <- levels(factor(aff$MemberID))
#     colnames(A) <- levels(factor(aff$JSR))
#     
#     ## Adjacency Matrix
#     Arow <- A %*% t(A)
#     
#     ## Finding Pairwise Collaborations (Top Triangle)
#     Arow[upper.tri(Arow,diag=F)]<-0
#     pair_collab = sum(Arow)
#     
#     # expgroupsize <- df  %>%
#     #   filter(periodindex == periodcurrent &
#     #            JSR == periodjsr) %>%
#     #   group_by(JSR) %>%
#     #   summarise(count = n()) %>% 
#     #   select(count)
#     
#     ## Network Mesasures
#     
#     aff1 <- df %>% 
#       filter(periodindex <= periodprior) %>% 
#       distinct(unique_id,.keep_all=T)
#     
#     A1 <- spMatrix(nrow=length(unique(aff1$MemberID)),
#                   ncol = length(unique(aff1$JSR)),
#                   i = as.numeric(factor(aff1$MemberID)),
#                   j= as.numeric(factor(aff1$JSR)),
#                   x = rep(1, length(as.numeric(aff1$MemberID))))
#     
#     row.names(A1) <- levels(factor(aff1$MemberID))
#     colnames(A1) <- levels(factor(aff1$JSR))
#     
#     ## Adjacency Matrix
#     Arow1 <- A1 %*% t(A1)
#   
#     ## Calculating Graph Metric
#     g1 <- graph_from_adjacency_matrix(Arow1)
#     statusdeg = sum(centr_degree(g1)$res)$centralization
#     statusbet = sum(centr_betw(g1)$res)
#     statusclo = sum(centr_clo(g1)$res)
#     
#     
#     dt <- data.frame(periodjsr,pair_collab,statusdeg,statusbet,statusclo,periodcurrent) #,expgroupsize)
#     output[[i]] <- dt
#     
#   }
# }
#   
# output <- bind_rows(output)
# warnings()
#   
# rm(i)
# 
# graph(g1)
# 
# statusdeg = sum(centr_degree(g1)$centralization)
# 


###############
###Re-Format###
###############


install.packages('igraph')
library(igraph)
library('Matrix')
library(tidyverse)

output <- list()
for(i in 1:nrow(quarter_finish)){
  
  jsr <- quarter_finish[i,]
  
  jsr_num <- jsr$JSR
  start <- jsr$yq_start
  end <- jsr$yq_end
  release <- jsr$finalrelease
  
  dt <- new_final %>% 
    filter(JSR == jsr_num & yq <= end & yq >= start) %>% 
    distinct(yq,unique_id,.keep_all = TRUE) %>% 
    select(JSR,MemberID,yq) 
  
  
  # QA <- new_final %>%
  #   filter(JSR == jsr_num) %>%
  #   mutate(after = ifelse(yq > end,1,0),
  #          before = ifelse(yq < start,1,0))

  # dist <- new_final %>% mutate (id = paste0(yq,unique_id)) %>% distinct(id)
  # 
  # print(dist)
  
  output[[i]] <- dt %>% mutate(start = start, end = end)
}

df <- bind_rows(output)

periods <- df %>% distinct(yq) %>% left_join(period,by="yq") %>% arrange(periodindex)


# Calculating Network Measures

list <- list()
for(j in 1:nrow(periods)){
  
  j = 1 
  
  list_new <- list()
  
  period_yq <- periods[j,1]
  period_index <- periods[j,2]
  
  # Choose the number of quarters to capture
  num_quarters = 0
  
  diff <- period_index - num_quarters
  
  # Does not calculate measures if the past quarters are not available
  if(diff > 0 ){
  # Building a edgelist
  edgelist <- df %>% filter(yq == period_yq) %>% 
    select(MemberID,JSR) 
  
  # Selecting Time Frame in num of quarters from j'th period
  past_yq <- periods[j-num_quarters,1]
  pastedgelist <- df %>% filter(yq >= past_yq & yq < period_yq) %>% select(MemberID,JSR)
  
  edgelist <- rbind(edgelist,pastedgelist)
  
  
  # Edgelist needs to include all affiliations 
  # prior to the current period, including those for completed JSR since the beginning of the JCP Community
  # Include flexibility the timeframe of previous network included 
  
  # Building the affiliation matrix 
  A <- spMatrix(nrow=length(unique(edgelist$MemberID)),
                ncol=length(unique(edgelist$JSR)),
                i = as.numeric(factor(edgelist$MemberID)),
                j = as.numeric(factor(edgelist$JSR)),
                x = rep(1, length(as.numeric(edgelist$MemberID))) )
  row.names(A) <- levels(factor(edgelist$MemberID))
  colnames(A) <- levels(factor(edgelist$JSR))
  

  #Adjacency Matrix of Firm-Firm (Value Matrix)
  Arow <- A %*% t(A)
  
  #Binarize -  1 = 1 or more, 0 - no firm-firm tie
  #Arow_bin <- as.matrix((Arow > 0) + 0)
  
  g <- graph_from_adjacency_matrix(Arow,mode="upper",diag=FALSE)
  
  plot.igraph(g)
  
  all_member <- edgelist %>% distinct(MemberID) %>% arrange(MemberID)

  # Finding measures specific to an individual firm
  for(k in 1:nrow(all_member)){
    
    
    member <- as.character(all_member[k,])
      
    #Measures are calculated using a Valued firm-firm matrix
    statusdegree = centr_degree(g)$res[k]
    statusbet    = centr_betw(g)$res[k]
    statusclo    = centr_clo(g)$res[k]
    
    output_1 <- data.frame(member,period_yq,statusdegree,statusbet,statusclo)
    list_new[[k]] <- output_1
  }

  d <- bind_rows(list_new)
  
  d_ordered <- d %>% 
    transform(member = as.numeric(member)) %>% 
    arrange(member) %>% 
    rename(MemberID = member) %>% 
    select(-MemberID,-period_yq) %>% 
    as.matrix()
  
  # A is original sparse affiliation firm-jsr matrix
  # t(d_ordered) is the transposed ordered firm-firm matrix
  #Network by JSR for each period
  matrix <- t(d_ordered) %*% A 
  
  #JSR by Network 
  t_matrix <- (t(matrix)) 
  
  #Transforms above matrix into a data frame
  t_dataframe <- as.data.frame(as.matrix((t_matrix)))%>% 
    rownames_to_column("JSR") %>% 
    mutate(period = period_yq)
 
   # Loop to determine if certain firms were a part of the JSR in the current period
  # Also to find the unique number of firms for that specific JSR 
  list_1 <- list()
  for(l in 1:nrow(t_dataframe)){
    
    jsr_in = t_dataframe[l,1]
    
    edge <- edgelist %>%
      filter(JSR == jsr_in)
    
    uniq <- unique(edge$MemberID)
  
    final <- t_dataframe %>%
      filter(JSR == jsr_in) %>%
      mutate(sunmicro = ifelse(11 %in% edge$MemberID,1,0), #Copy for additional firms
             num_firms = length(uniq))
    
    list_1[[l]] <- final
    
  }

  e <- bind_rows(list_1)      
          
  
  list[[j]] <- e
  }
}

dt <- bind_rows(list)

# list_1 <- list()
# 
# for(l in 1:nrow(t_dataframe)){
# 
#   jsr_in = t_dataframe[l,1]
# 
#   edge <- edgelist %>%
#     filter(JSR == jsr_in)
# 
#   final <- t_dataframe %>%
#     filter(JSR == jsr_in) %>%
#     mutate(sunmicro = ifelse(11 %in% all_member,1,0))
# 
#   list_1[[l]] <- final
# 
# }

# Reordering
dt_ordered <- dt %>% 
  transform(JSR = as.numeric(JSR),
            period = as.integer(period)) %>% 
  arrange(JSR,period)



# Determining Which period is final release

output_2 <- list()
for(i in 1:nrow(quarter_finish)){
  
  jsr <- quarter_finish[i,]
  
  jsr_num <- jsr$JSR
  start <- jsr$yq_start
  end <- jsr$yq_end
  release <- jsr$finalrelease
  
  test <- dt_ordered %>% 
    filter(JSR == jsr_num) %>% 
    #distinct(yq,unique_id,.keep_all = TRUE) %>% 
    mutate(finalrelease = ifelse(period == end,release,0))
  
  output_2[[i]] <- test %>% mutate(start = start, end = end)
  
}


hold <- bind_rows(output_2)

release_check <- hold %>% 
  filter(finalrelease == 1) %>% 
  left_join(quarter_jsr, by = "JSR") %>% 
  filter(period == yq_end)

#258 JSR's Captured
length(unique(hold$JSR))

write.csv(hold, "JSR_network_measure.csv")


