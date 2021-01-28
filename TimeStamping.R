install.packages("lubridat")
install.packages("tidyverse")
install.packages(("data.table"))

library(lubridate)
library(tidyverse)
library(data.table)




CleanAssocFirm <- read_csv("CleanAssocFirm.csv") %>% 
  rename('MemberID' = memberid)

#Joining Dates
CleanAssocFirm$date <- as.Date(with(CleanAssocFirm, paste(y, m, d,sep="-")), "%Y-%m-%d")


#Removing Misc. data point and columns
CleanAssocFirm <- as.data.frame(CleanAssocFirm) %>% filter(X1 != 44675) %>% select(-c(m,y,d,gckey,crunchbase))

#Checking for increasing trend of JSRs
JSRInfo <- CleanAssocFirm %>% 
  group_by(date) %>% 
  summarize(count = n())

#write.csv(JSRInfo,file='JSRInfo.csv')

ggplot(JSRInfo, aes(x=date,y=cumsum(count))) + geom_line() + labs(y= "Cumulative Count of JSRs", x = "Year")

#Number of Snapshots/year

snapshots <- CleanAssocFirm %>% 
  group_by(y,m) %>% 
  summarize(jsr = n_distinct(JSR))

snap03 <- CleanAssocFirm %>% 
  filter(y==2003) %>% 
  filter(JSR %in% c(1:10)) %>% 
  mutate(
    snapperiod = as.factor(ifelse(m == 2,"1",ifelse(m==4,"2",ifelse(m==6,"3",ifelse(m==8,"4",ifelse(m==10,"5","6")))))),
    JSR = as.factor(JSR)
    ) %>% 
  group_by(JSR,snapperiod) %>% 
  summarise(count=n_distinct(MemberID))
ggplot(snap03,aes(snapperiod,JSR,size=count)) + geom_point()


xtabs(count~JSR+snapperiod,data=snap03)

# Random Sample of JSR's

set.seed(1234)
jj <- sample((min(CleanAssocFirm$JSR):max(CleanAssocFirm$JSR)),10))

JSR106 <- CleanAssocFirm %>% filter(CleanAssocFirm$JR == jj[1])

#Sorting By Date 
OrderedCleanFirm <- CleanAssocFirm[order(CleanAssocFirm$date),]

OrderedCleanFirm$date <- as.Date(OrderedCleanFirm$date)

#Creating a List of Periods for every half a year

jsrperiod <- function(numofdays){

  numofdays <- numofdays #number of days
date.df <- as.data.frame(seq(min(OrderedCleanFirm$date),max(OrderedCleanFirm$date),
                             by=numofdays))
names(date.df) <- c("start") 
date.df <- date.df %>% mutate(end = date.df$start)

#Function to shift start dates up by 1 period to make end dates and for the 
#last period use system time as the end date

shift <- function (x,n) {
  c(x[-(seq(n))],rep(Sys.Date(),n))}

#Applying shift function and adding an interval of the two as another column 
date.df$end <- shift(date.df$end,1)
date.df$period <- interval(date.df$start,date.df$end)

#Mapping intervals to dates

interval <- data.table(Interval = date.df$period)
findPeriodIndex <- function(x) {
  interval[,which(int_start(interval$Interval) <= x & x < 
                    int_end(interval$Interval))]
}

dt <- data.table(OrderedCleanFirm,x = OrderedCleanFirm$date)
dt$Periodindex <- paste("", sapply(dt$x, findPeriodIndex), sep = "")
dt <- dt %>% select(-c(x))

return(dt)
}

ptm <- proc.time()
dt <- jsrperiod(92)
proc.time() - ptm

write.csv(dt,"c:/Users/jca305/sfuvault/jaredChiu/R-Scripts/dt.csv")



JSRStageStartFinish <- read_csv("JSRStageStartFinish.csv", 
                                col_types = 
                                  cols(Start = 
                                         col_date(format = "%Y-%m-%d"), 
                                       Finish = 
                                         col_date(format = "%Y-%m-%d")))


 
new_dt <- dt %>% 
  merge(JSRStageStartFinish,by.x = c("JSR","date"),
        by.y=c("JSR","Start"),all.x=T) %>% 
  filter(JSR == 1)
  

num_jsr <- dt %>%  
  group_by(Periodindex) %>%
  distinct(JSR,MemberID) %>% 
  summarise(count = n()) 
  

x <- CleanAssocFirm %>% 
  mutate(
    y = year(date),
    m = month(date)
  )# %>% 
  group_by(y,m) %>% 
  distinct(JSR,MemberID) %>% 
  mutate(
    d = 1
  ) 

x$date <- as.Date(with(x, paste(y, m, d,sep="-")), "%Y-%m-%d") 

quarters <-quarter(CleanAssocFirm$date)

summary(quarters)

period<- cbind(CleanAssocFirm,quarters) %>% 
  mutate(yq = paste(year(date),quarters,sep="_")) %>% 
  distinct(yq,JSR,MemberID) 





period_1 <- period %>%  group_by(yq) %>% summarise(n())


#setdiff((period %>% filter(yq == paste(2002,4,sep="_")))$unique_id,
#        (period %>% filter(yq == paste(2003,1,sep="_")))$unique_id)

p_2002 <- period %>% filter(yq == paste(2002 + 1,4,sep="_")) 

p_2003 <- period %>% filter(yq == paste(2003,1,sep="_")) 

anti<- anti_join(p_2002,p_2003,by="unique_id") 

m <- p_2003$unique_id

np_2003 <- rbind(p_2003,anti) %>% 
  mutate(
    yq = "2003_1"
  )

final <- data.frame()

for (i in 1:17){
  for (g in 1:4){
    
    #a = paste(2000+i,g,sep="_")
    
    b = paste(2001+i,g,sep="_")
    
    #ab = paste(2000+i+1,g+1,sep="_")
    
    pattern <- "_4$"
    
    if(grepl(pattern,b)){
      
      a1 = paste(2002+i-1,4,sep="_")
      b1 = paste(2002+i,1,sep="_")
      
      c1 = period %>% filter(yq == a1)
      d1 = period %>% filter(yq == b1)
      
      e1 = anti_join(c1,d1,by="unique_id")
      
      f1 = rbind(d1,e1) %>% 
        mutate(
          yq = b1
        )
      
      g1 = rbind(f1,c1)
      
      final = rbind(final,g1)
   
    } else {
      a2 =  paste(2002+i,g,sep="_")
      b2 =  paste(2002+i,g+1,sep="_")
      
      c = period %>% filter(yq == a2)
      d = period %>% filter(yq == b2)
      
      e = anti_join(c,d,by="unique_id")
      
      f = rbind(d,e) %>% 
        mutate(
          yq = b
        )
      final = rbind(final,f)
    }
  }
}
   





# a = period %>% filter(yq == "2002_4")
# b = period %>% filter(yq == "2003_1")
# c = anti_join(a,b,by="unique_id")
# d = rbind(b,c) %>% 
#   mutate(yq = "2003_1")
# e = rbind(a,d)
# a1 = e %>% filter(yq == "2003_1")
# b1 = period %>% filter(yq == "2003_2")
# c1 = anti_join(a1,b1,by="unique_id")
# d1 = rbind(b1,c1) %>% 
#   mutate(yq = "2003_2")
# e = rbind(e,d1)
# 
# 
# for (i in 1:16){
#   for(g in 1:4){
#     
#     y = g+1
#     
#     if(y!=5){
#       
#       yq  = paste(2002+i,g,sep="_")
#       yq1 = paste(2002+i,g+1,sep="_")
#       
#       a1 = e %>% filter(yq == yq)
#       b1 = period %>% filter(yq == yq1)
#       
#       c1 = anti_join(a1,b1,by = "unique_id")
#       d1 = rbind(b1,c1) %>% 
#         mutate(yq = yq1,
#                copy = 1)
#       
#       e = rbind(e,d1)
#       
#     } else{
#       
#       yq2 = paste(2003+i-1,4,sep = "_")
#       yq3 = paste(2003+i,1,sep = "_")
#       
#       a2 = e %>% filter(yq == yq2)
#       b2 = period %>% filter(yq == yq3)
#       
#       c2 = anti_join(a2,b2, by = "unique_id")
#       d2 = rbind(c2,b2) %>% 
#         mutate(yq = yq3,
#                copy = 1)
#       
#       e = rbind(e,d2)
# 
#     }
#   }
# }
# 


for (i in 1:61){
  if(i = 1) {
    a = Ordered %>%  filter(periodindex == i) %>% 
      mutate(unique_id = paste(JSR,MemberID,"_"))
    b = Ordered %>%  filter(periodinex == i+1 )%>% 
      mutate(unique_id = paste(JSR,MemberID,"_"))
    
    c = anti_join(a,b, by = "unique_id")
    d = rbind(c,b) %>% 
      mutate(periodindex = i+1,
             copy = 1)
    
    e = rbind(e,d)
    
  } else { 
    a = e %>%  filter(periodindex == i) %>% 
    mutate(unique_id = paste(JSR,MemberID,"_"))
    b = Ordered %>%  filter(periodinex == i+1)%>% 
    mutate(unique_id = paste(JSR,MemberID,"_"))
  
  c = anti_join(a,b, by = "unique_id")
  d = rbind(c,b) %>% 
    mutate(periodindex = i+1,
           copy = 1)
  
  e = rbind(e,d)
  }
  
}


# FOR LOOP BY PERIOD INDEX 

f <- e %>% unique(unique_id,yq) 

#write.csv(f,"final_period.csv")

dt <- read_csv("final_period.csv") 


Ordered$periodindex <- cumsum(!duplicated(Ordered[3]))

Ordered <- period[order(period$yq),]

pairs <- str_split_fixed(Ordered$unique_id, "_", 2) %>% 
  rename(JSR = V1,
         MemberID = V2)


g <- e %>% group_by(yq) %>% summarise(count = n())

split()

period %>% group_by(yq) %>% summarise(n())


rm(a,a1,b,b1,b2,a2,c2,d2,c,c1,d,d1,y,yq1,yq,yq2,yq3)
# i = 3
# g = 1
# x= period
# 
# a = paste(2000+i,g,sep="_")
# b = paste(2000+i,g+1,sep="_")
# 
# c = x %>% filter(yq == a)
# d = x %>% filter(yq == b)
# 
# e = anti_join(c,d,by="unique_id")
# 
# f = rbind(d,e) %>% 
#   mutate(
#     yq = b
#   )
# 
# final = rbind(final,f)

#Notes
# Create unique non duplicated list of jsr firm pairings for each period
# find JSR's which exist from period 1 which are not in period 2 if > 0 then problem

###############
#WORKING CODE##
###############

library(tidyverse)
library(lubridate)

CleanAssocFirm <- read_csv("CleanAssocFirm.csv") %>% 
  rename('MemberID' = memberid)

#Joining Dates
CleanAssocFirm$date <- as.Date(with(CleanAssocFirm, paste(y, m, d,sep="-")), "%Y-%m-%d")


#Removing Misc. data point and columns
CleanAssocFirm <- as.data.frame(CleanAssocFirm) %>% filter(X1 != 44675) %>% select(-c(m,y,d,gckey,crunchbase))


quarters <- quarter(CleanAssocFirm$date)

ordered1 <- cbind(CleanAssocFirm,quarters) %>% 
  mutate(yq = as.numeric(paste(year(date),quarters,sep=""))) %>% 
  distinct(yq,JSR,MemberID) %>% 
  arrange(yq) %>% 
  mutate(periodindex = cumsum(!duplicated(yq)),
         unique_id   = paste(JSR,MemberID,sep="_"),
         copy = 0)

write.csv(Ordered1, 'Ordered1.csv')

# t <- Ordered1 %>% 
#   group_by(periodindex) %>% 
#   count()

e = data.frame()

## WORK ON DOCUMENTATION

for (i in 1:67){
  #if(i == 1){
    
    ## 
    a = ordered1 %>%  filter(periodindex == i) 
    b = ordered1 %>%  filter(periodindex == i+1)

    c = anti_join(a,b, by = "unique_id") %>%
      mutate(copy = 1)
    d = rbind(c,b) %>%
      mutate(periodindex = i+1)
    e = rbind(e,a)
    e = rbind(e,d)

  # } else{
  #   a1 = e %>%  filter(periodindex == i) 
  #   b1 = Ordered1 %>%  filter(periodindex == i+1)
  #   
  #   c1 = anti_join(a1,b1, by = "unique_id")%>% 
  #     mutate(copy = 1)
  #   d1 = rbind(c1,b1) %>% 
  #     mutate(periodindex = i+1)
  #   
  #   e = rbind(e,a1)
  #   e = rbind(e,d1)
  # }
}

final = e %>% select(JSR,MemberID,periodindex,copy,yq,unique_id) %>% 
  transform(yq = as.numeric(yq))

write.csv(final,"dt1.csv")
#
#From Nilesh's Stata Output
finished_jsr <- read_csv("C:/Users/jca305/sfuvault/jaredChiu/Clean Data Sets/finished_jsr.csv", 
                         col_types = cols(firstdt = col_date(format = "%m/%d/%Y"), 
                                          lastdt = col_date(format = "%m/%d/%Y"))) %>% 
  rename(JSR = jsr) %>% 
  as.data.frame()

quarter_finish <- finished_jsr %>% 
  filter(is.na(drop)) %>% 
  mutate(yq_start = paste0(year(`firstdt`),quarter(`firstdt`)),
         yq_end = paste0(year(`lastdt`),quarter(`lastdt`))) %>% 
  transform(yq_end = as.numeric(yq_end),yq_start = as.numeric(yq_start))


quarter_finish <- quarter_finish %>% mutate(yq_end = replace_na(yq_end, 20211))

write.csv(quarter_finish,'quarter_finish.csv')
# left_join(quarter_finish,jsrfinal, by ="yq")
# 
# final_1 <- final %>% merge(quarter_finish,by.x = c("JSR","yq"),
#                          by.y=c("JSR","yq"),all.x=T)



#Filters out completed JSR's based on finished_jsr's lastdt variable 

# for(i in 1:nrow(final)){
#   
#   row_final = final[i,]
#   quarter_final = quarter_finish %>% filter(JSR == row_final$JSR)
#   
#   if(row_final$yq <= quarter_final$yq_end && row_final$yq >= quarter_final$yq_start){
#     new_final = rbind(new_final,row_final)
#   }
# }



# final <- read_csv("dt1.csv")
# new_final = data.frame()
# unmatched = data.frame()
# for(i in 1:nrow(final)){
#   
#   row_final = final[i,]
#   quarter_final = quarter_finish %>% filter(JSR == row_final$JSR)
#   
#   if(row_final$JSR %in% quarter_finish$JSR && 
#      row_final$yq <= quarter_final$yq_end # && 
#      #row_final$yq >= quarter_final$yq_start
#      ){
#     new_final = rbind(new_final,row_final)
#   } 
#     else{unmatched = rbind(unmatched,row_final)}
# }


###########################
## New Final Affiliation ##
###########################

new_final <- list()
for(i in 1:nrow(quarter_finish)){
  

  jsrdata <- quarter_finish[i,]
  jsr <- jsrdata$JSR
  yqend <-  jsrdata$yq_end
  #yqstart <- jsrdata$yq_start
  
  finaldata <- final %>% 
    filter(JSR == jsr &
          yq <= yqend) 
  
  new_final[[i]] <- finaldata
  
}



new_final <- bind_rows(new_final)

jsr_1 = filter(new_final_v1,JSR == 1)

new_final %>% group_by(JSR) %>% summarise(n())

write.csv(new_final,'new_final_v1.csv')




# row_final = final[2,]
# quarter_final = quarter_finish %>% filter(JSR == row_final$JSR)
# 
# if(row_final$yq <= quarter_final$yq_end && row_final$yq >= quarter_final$yq_start){
#   new_final = rbind(new_final,row_final)
# }
# 
# 

# 
# final_list <- list()
# for(i in 1:nrow(quarter_finish)){
#   
#   jsr_final = quarter_finish[i,]$JSR
#   
#   yq_final = quarter_finish[i,]$yq
#   
#   rows <- which(final$JSR != jsr_final & final$yq > yq_final)
#   
#   new_final <- final[-rows,]
#   
#   final_list[[i]]<- new_final 
#   
# }
# 
# 
# 
# 
# test <- bind_rows(final_list)
# 
# test %>% 
#   group_by(periodindex) %>% 
#   count()
# jsrfinal %>% 
#   group_by(periodindex) %>% 
#   count()
