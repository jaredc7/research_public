
library(tidyverse)
library(readxl)
library(lubridate)
#install.packages("fuzzyjoin")
library(fuzzyjoin)


# Steps:
# Load data
# Rename Column Names by dates
# Remove non-firm data (ie. Stages)
# Transform data into single cell observations with JSR and Date

#load data
JSR_prior <- read_excel("C:/Users/jca305/sfuvault/Research - Nilesh/JSR Expert group main.xlsx")

for (col in 1:ncol(JSR_prior)){
  colnames(JSR_prior)[col] <-  gsub("[^0-9]", "", colnames(JSR_prior)[col])
}

list <- list()
final_list <- list()

for(date in 2:ncol(JSR_prior)){
  time <- str_sub(colnames(JSR_prior)[date],1,8)
  for(firm in 1:nrow(JSR_prior)){
    
    jsr <- JSR_prior[firm,1]
    
    
    if(grepl(JSR_prior[firm,date],pattern="JSR" )){}
    else{
      firms <- strsplit(as.character(JSR_prior[firm,date]), "; ")
      firm_unlist <- data.frame(firms = unlist(firms))
    
    
      final <- cbind(jsr,firm_unlist,time)
      list[[firm]] <- final}
  }
  final_list[[date]] <- list
}

# final_list[[2]]

association <- bind_rows(final_list) %>% 
  rename(JSR = Var.1,
         date= time) %>% 
  mutate(new_date = as.Date(date,'%Y%m%d')) %>% 
  mutate(m = month(new_date),
         y = year(new_date),
         d = day(new_date)) %>% 
  select(-new_date,-date)


CleanFirm <- read_csv("CleanFirm.csv") %>% 
  mutate(name = tolower(name))

CleanAssocFirm_prior <- association %>% 
  filter(!is.na(firms)) %>% 
  mutate(firms = tolower(firms)) %>% 
  mutate(firms = gsub(" ", "", firms, fixed = TRUE))%>% 
  
  mutate(firms = gsub('.*Bull,SmartCards&Terminals.*',"Bull SmartCards&Terminals",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*agilent.*',"agilenttechnologies",firms,fixed = TRUE)) %>%
  mutate(firms = gsub('.*bahwancybertektechnologiesinc..*',"bahwancybertekprivatelimited",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*electronicdatasystems.*',"electronicdatasystems(eds)",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*fujitsulimited.*',"fujistuamerica",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*ipunity.*',"ipunityglenayre",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*ddicorporation.*',"kddicorporation",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*lutristecnologies.*',"lutristechnologies",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*microfocus.*',"microfocusltd",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*mitre.*',"mitrecorporation",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*necsoftwaredesign.*',"neccorporation",firms,perl = TRUE)) %>%
  mutate(firms = gsub('.*neweraofnetworks(neon).*',"neonsystems",firms,perl = TRUE)) %>%

  mutate(firms = gsub(',inc.',"",firms,fixed=TRUE)) %>%
  mutate(firms = gsub(',ltd',"",firms,fixed=TRUE)) %>%
  mutate(firms = gsub(',llc',"",firms,fixed=TRUE)) %>% 
  left_join(CleanFirm,by=c(firms = "name")) %>% 
  filter(!is.na(memberid))

name_changes_2 <- read_excel("name_changes_2.xlsx")
for(i in 1:nrow(CleanAssocFirm_prior)){
  
  if(CleanAssocFirm_prior[i,]$firms %in% name_changes_2$Firm_old){
    
    new_data <- name_changes_2 %>% filter(Firm_old == CleanAssocFirm_prior[i,]$firms)
    
    CleanAssocFirm_prior[i,] <- CleanAssocFirm_prior[i,] %>% 
      mutate(memberid = new_data$memberid,
             firms = new_data$Firm_new)
  }
  else{}
}

# Append prior 2001-2002 to Clean Assoc Firm
CleanAssocFirm <- read_csv("CleanAssocFirm.csv") %>% 
  select(-X1)

add_CleanAssocFirm_prior <- CleanAssocFirm_prior %>% 
  select(JSR,m,y,d,memberid,gckey,crunchbase)

CleanAssocFirm_new <- CleanAssocFirm %>% 
  rbind(add_CleanAssocFirm_prior)

# Write new Clean Assoc Firm Data Set
write.csv(CleanAssocFirm_new,"CleanAssocFirm.csv")

# From Firm List.R
# Finding out of seq'ed memberid
Missingid <- data.frame(rep(1:nrow(CleanFirm))[!(rep(1:nrow(CleanFirm)) %in%  unique(CleanFirm$memberid))]) #out of seq's id's
names(Missingid) <- c("memberid")
Missingid <- mutate(Missingid, index = 1:nrow(Missingid))

# Giving new member ID's to the unmatched firms from 2002 - 2001
NoMemberID<- unmatched
NoMemberID<- select(NoMemberID,-(memberid))
NoMemberID <- mutate(NoMemberID, index = 1:nrow(NoMemberID))
NoMemberID <- right_join(Missingid, NoMemberID, by = "index", all.y=TRUE)
# Adding the new members to the cleanfirm.csv
NoMemberID <- select(NoMemberID,-(index))
NoMemberID <- NoMemberID[c('name','memberid','gckey','crunchbase','class')]
CleanFirm_New <- CleanFirm %>% 
  select(-X1) %>% 
  rbind(NoMemberID)

# Writing cleaned CleanFirm.csv
#write.csv(CleanFirm_New,"CleanFirm.csv")

#Not Matched - should be 0 after the above code from line 99-118
unmatched <- CleanAssocFirm_prior %>% 
  filter(is.na(memberid)) %>% 
  distinct(firms) %>% 
  filter(!grepl(",",firms)) %>% 
  regex_left_join(CleanFirm,by=c(firms="name")) %>% 
  filter(!grepl("maintenance",firms))
  
  


