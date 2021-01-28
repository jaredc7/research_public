#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('shiny')

#mis2: #C:/Users/nsara/sfuvault/Research/jsr/data_Analysis/dataCollection/jaredChiu/R-Scripts
# #C:/Users/nsara/sfuvault/Research/jsr/data_Analysis/dataCollection/jaredChiu/R-Scripts

#setwd("C:/Users/nsara/sfuvault/Research/jsr/data_Analysis/dataCollection/jaredChiu/R-Scripts")
library(shiny)
library(networkD3)
library(tidyverse)

##Preset unique firm identifiers 


firmcode <- read_csv("../CleanFirm.csv") %>% 
    mutate(code = substr(name,1,5)) %>% 
    arrange(memberid) %>% 
    mutate(entityid = row_number(),
           match = ifelse(memberid == entityid,1,0),
           id = paste0(code,entityid))

##Back-end static data set - to be cleaned
dt <- read_csv("../new_final_v1.csv")


#### Server ####
server <- function(input, output) {
    
    ## Force Network Visualization
    output$force <- renderForceNetwork({
        
        ## Period Selection from UI side
        p <- input$period
        
        p = 1
        ##Firm Selection from UI side (exclude)
        f <- input$firmout
        
        ##JSR selection from UI side (exclude)
        j <- input$jsrout
        
        
        ##Building proper data set for network visualization
        networkData <- dt %>% 
            select(JSR,MemberID,periodindex) %>% 
            rename(memberid = MemberID) %>%
            
            ## Filtering for specific period
            filter(periodindex == p) %>% 
            left_join(firmcode,by = 'memberid') %>% 
            select(JSR,id) %>% 
            rename(src = JSR,
                   target = id) %>% 
            distinct(src,target)
        

        src <- as.character(networkData$src)
        target <- networkData$target
        
        networkData1 <- data.frame(src, target, stringsAsFactors = FALSE)
        
        nodes <- data.frame(name = unique(c(src, target)), stringsAsFactors = FALSE)
        
        nodes$id <- 0:(nrow(nodes) - 1)
        
        ## make a grouping variable that will match to colours
        nodes$group <- ifelse(nodes$name %in% src, "JSR", "Firms")
        
    
        ## building edge data frame for association between firms and jsr's
        edges <- networkData1 %>%
            left_join(nodes, by = c("src" = "name")) %>%
            select(-src) %>%
            rename(source = id) %>%
            left_join(nodes, by = c("target" = "name")) %>%
            select(-target) %>%
            rename(target = id)
        
        edges$width <- 1
       
        ## Server logic for filtering out one JSR and/or firm
        filtered_data_1 <- if(f != ''){
            edges %>% left_join(nodes, by=c("target" = "id")) %>% 
            filter(!Reduce(`&`, lapply(strsplit(f,' ')[[1]], grepl,name,ignore.case=T))) %>% 
            select(-name,-group)
        }
        else{
            edges
        }
        
        filtered_data_2 <- if(j != ''){
            filtered_data_1 %>% 
            left_join(nodes, by=c("source" = "id")) %>% 
            filter(!Reduce(`&`, lapply(strsplit(j,' ')[[1]], grepl,name,ignore.case=T)))%>% 
            select(-name,-group)
        }
        else{
            filtered_data_1
        }
        

    ############
    #TEST CODE##
    ############
        
        # filtered_data_1 <- if(f != '' | j !='' )
        #     edges %>% 
        #     left_join(nodes, by = c("target" = "id")) %>% 
        #     filter((!Reduce(`&`, lapply(strsplit(f,' ')[[1]], grepl,name,ignore.case=T)) | 
        #                 (!Reduce(`&`, lapply(strsplit(j,' ')[[1]], grepl,source,ignore.case=T)))))
        # else 
        #     edges
        # 
        
        # test <-edges %>% left_join(nodes, by=c("target" = "id")) %>% 
        #      left_join(nodes, by=c("source" = "id")) 
        
        # filtered_period <- reactive({
        #     if(p != ''){
        #         edges %>% 
        #         left_join(nodes, by = c("target" = "id") ) %>% 
        #         filter(!Reduce(`&`, lapply(strsplit(p,' ')[[1]], grepl,name,ignore.case=T)))
        #     } else {
        #         edges
        #     }
        # })
        # 
        # filtered_jsr <- reactive({
        #     if(f != ''){
        #         filtered_period %>% 
        #         filter(!Reduce(`&`, lapply(strsplit(f,' ')[[1]], grepl,name,ignore.case=T)))
        #     }
        #     else {
        #         edges
        #     }
        # })
        
        # data1 <- if(f != '')
        #     edges %>% 
        #     left_join(nodes, by = c("target" = "id") ) %>% 
        #     filter(!Reduce(`&`, lapply(strsplit(f,' ')[[1]], grepl,name,ignore.case=T))) 
        # else
        # 
        # data2 <- if(j != '' )
        #     data1 %>%
        #     filter(!Reduce(`&`, lapply(strsplit(j,' ')[[1]], grepl,source,ignore.case=T)))
        # else
        #     edges
            
        
        ## Force Network function for visualization
        forceNetwork(Links = filtered_data_2, Nodes = nodes,
                     Source = "source",
                     Target = "target",
                     NodeID ="name",
                     Group = "group",
                     Value = "width",
                     opacity = 0.9,
                     zoom = TRUE)
        
        
    })
        
}



#### UI ####

ui <- shinyUI(fluidPage(
    
    titlePanel("Shiny networkD3"),
    #sliderInput("period", "Looping Animation:",
                # min = 1, max = 62,
                # value = 1, step = 1,
                # animate =
                #     animationOptions(interval = 10000, loop = TRUE)),
    
     sidebarLayout(
         sidebarPanel(
             sliderInput("period", "Period of Network", 1, min = 1,max = 67, 
                         step = 1),
            
             textInput("firmout","Filter out a Firm",NA),
             textInput("jsrout","Filter out a JSR", NA),
             submitButton("Apply Changes")
         ),
        mainPanel(
            tabsetPanel(
                tabPanel("Network", forceNetworkOutput("force",width = "1000",
                                                       height = "1000"))
            )
        )
    )
))

#### Run ####
shinyApp(ui = ui, server = server)





