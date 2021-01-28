 #Data Import
CleanClassification <- read_excel("expertgrps_Sept242019 V1.xlsx",sheet = "Clean Classification", col_types = c("text","numeric", "skip", "skip"))
CompanyDirty <- read_excel("expertgrps_Sept242019 V1.xlsx", sheet = "Companies", col_types = c("text", "numeric", "numeric", "text", "blank","blank", "blank", "blank", "blank","blank"))

#Subsetting
Company <- filter(CleanClassification,`Comp/Ind`==1)

#FOR COMPANY ONLY
#Merging Company Information 
names(Company) <- c("name","class")
NewCompany <- merge(x=CompanyDirty,y=Company,by="name",all.y=TRUE)
#Missing Member id
Missingid <- data.frame(rep(1:nrow(NewCompany))[!(rep(1:nrow(NewCompany)) %in%  unique(NewCompany$memberid))]) #out of seq's id's
names(Missingid) <- c("memberid")
Missingid <- mutate(Missingid, index = 1:nrow(Missingid))

#Companies with no member id 
NoMemberID<- NewCompany %>% filter(is.na(memberid))
NoMemberID<- select(NoMemberID,-(memberid))
NoMemberID <- mutate(NoMemberID, index = 1:nrow(NoMemberID))

#Adding Missing Member id 
NoMemberID <- right_join(Missingid, NoMemberID, by = "index", all.y=TRUE)

#Merging Missing with Full List
NoMemberID <- select(NoMemberID,-(index))
NoMemberID <- NoMemberID[c('name','memberid','gckey','crunchbase','class')]
NewCompany <- NewCompany[!is.na(NewCompany$memberid),]
NewCompanyClean <- rbind(NoMemberID,NewCompany)



#write.csv(NewCompanyClean,file="CleanFirm.csv")

EGCompany <- EGnew %>% filter(name %in% NewCompanyClean$name)
EGCompany <- merge(EGCompany,NewCompanyClean,by='name',all.x=TRUE)
EGCompany <- EGCompany %>% select(-c(name,class))

names(EGCompany) <- c("JSR","m","y","d","memberid","gckey","crunchbase")
#write.csv(EGCompany,file='CleanAssocFirm.csv')

library(tidyverse)

firmcode_dup <- firmcode %>% 
  mutate(entityid = memberid) 
