library(shiny)
source("analytics_test.r")
profiles <- get.profiles()
lst <- list()
for(i in 1:length(profiles[,1])) lst[profiles[2][i,]] <- profiles[1][i,]
shinyUI(fluidPage(
  titlePanel('Google Analytics Data'),
  sidebarLayout(
     sidebarPanel(
      dateRangeInput(inputId = "dateRange",  
                     label = "Date range", 
                     start = Sys.Date() - 30,
                     max = Sys.Date()
      ),
      radioButtons('mobile', 'Filters',
                   c('All'='All',
                     'Mobile Summary'='Mobile Summary',
                     'Mobile Results'='Mobile Results',
                     'Device Category'='Device Category'), 
                   selected = 'All',
      ),
      selectInput(inputId = "profiles",
                label = "Select Account",
                choices = lst) ,
      downloadButton('downloadData', 'Download Data'),
      downloadButton('downloadGraphics', 'Download Graphics')
      ),
      mainPanel(
        tabsetPanel( 
          tabPanel("Summary", tableOutput('table')),
          tabPanel("Visitors", plotOutput("visitorGraph")),
          tabPanel("Sources", plotOutput("sourcesGraph")),
          tabPanel("Mobile Vs. Desktop", plotOutput('mobvsdesk'))
      )
    )
  )
))