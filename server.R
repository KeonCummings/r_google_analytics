library(shiny)
library(plyr)
library(ggplot2)
library(RGoogleAnalytics)
source("analytics.r")
load.token()
get.profiles()

shinyServer(function(input, output) {
  passData <- reactive({
    data <- pageviews(as.character(input$dateRange[1]), as.character(input$dateRange[2]), input$profiles)
    data
   if("date" %in% colnames(data)){
    data$date = as.Date(data$date , "%Y%m%d")
   }
    data
  })

  sourceData <- reactive({
    data <- sources(as.character(input$dateRange[1]), as.character(input$dateRange[2]), input$profiles)
    data <- data[rev(order(data$sessions)),]
    data <- data[1:5,]
    data
    })

  mobileData <- reactive({
    data <- device.info(as.character(input$dateRange[1]), as.character(input$dateRange[2]), input$profiles)
    data
    })

  mobileCategoryData <- reactive({
    data <- device.category(as.character(input$dateRange[1]), as.character(input$dateRange[2]), input$profiles)
    data
    })

   vsData <- reactive({
    data <- viewsvsmobile(as.character(input$dateRange[1]), as.character(input$dateRange[2]), input$profiles)
    data
    })

  output$table <- renderTable({
    if(length(input$mobile) == 0){
      data <- passData()
      return(data)
    }
    if(input$mobile == "Mobile Summary") {
      data <- mobile.sum(as.character(input$dateRange[1]), as.character(input$dateRange[2]), input$profiles)
      return(data)
    }
    if(input$mobile == "Device Category"){
      data <- device.category(as.character(input$dateRange[1]), as.character(input$dateRange[2]), input$profiles)
      return(data)
    }
    if(input$mobile == "Mobile Results"){
      data <- mobile(as.character(input$dateRange[1]), as.character(input$dateRange[2]), input$profiles)
      return(data)
    }
    if(input$mobile == "All"){
      data <- passData()
      return(data)
    }
   })

  output$visitorGraph <- renderPlot({
    ggplot(passData(), aes(x= date, y=sessions), colour=date) + geom_line() + geom_point()
    ggsave('plot.png')
    return(ggplot(passData(), aes(x= date, y=sessions), colour=date) + geom_line() + geom_point())
    })

  output$sourcesGraph <- renderPlot({
    if(input$mobile == "Mobile Results"){
      ggplot(mobileData(), aes(x=device, y=sessions, fill=device)) + geom_bar(stat='identity') + 
      scale_fill_brewer(palette="YlOrBr") +
      geom_text(aes(label=sessions), vjust="1.5", colour="black")
      ggsave('plot.png')
      return(ggplot(mobileData(), aes(x=device, y=sessions, fill=device)) + geom_bar(stat='identity') + 
      scale_fill_brewer(palette="YlOrBr") +
      geom_text(aes(label=sessions), vjust="1.5", colour="black"))
    }
    if(input$mobile == "Device Category"){
      ggplot(mobileCategoryData(), aes(x=deviceCategory, y=sessions, fill=deviceCategory)) + geom_bar(stat='identity') + 
      scale_fill_brewer(palette="YlOrBr") +
      geom_text(aes(label=sessions), vjust="1.5", colour="black") 
      ggsave('plot.png')
      return(ggplot(mobileCategoryData(), aes(x=deviceCategory, y=sessions, fill=deviceCategory)) + geom_bar(stat='identity') + 
      scale_fill_brewer(palette="YlOrBr") +
      geom_text(aes(label=sessions), vjust="1.5", colour="black"))
    }
    else{
      ggplot(sourceData(), aes(x=source, y=sessions, fill=source)) + geom_bar(stat='identity') + 
      scale_fill_brewer(palette="YlOrBr") +
      geom_text(aes(label=sessions), vjust="1.5", colour="black")
      ggsave("plot.png")
      return(ggplot(sourceData(), aes(x=source, y=sessions, fill=source)) + geom_bar(stat='identity') + 
      scale_fill_brewer(palette="YlOrBr") +
      geom_text(aes(label=sessions), vjust="1.5", colour="black"))
      }
    })

  output$mobvsdesk <- renderPlot({
    ggplot(vsData(), aes(x=date, y=sessions, colour= device)) + geom_line() + scale_colour_brewer(palette="Set1") 
    ggsave('plot.png')
    return(ggplot(vsData(), aes(x=date, y=sessions, colour= device)) + geom_line() + scale_colour_brewer(palette="Set1"))
    })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
     paste('test', '.csv', sep='') 
   },
    content = function(file) {
      write.csv(passData(), file)
    }
  )

  output$downloadGraphics <- downloadHandler(
    filename = function() {
        paste(input$profiles, '.png', sep='') 
    },
    content = function(file) {
        file.copy("plot.png", file, overwrite=TRUE)
    }
    )
})