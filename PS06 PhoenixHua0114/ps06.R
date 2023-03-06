library(readr)
library(dplyr)
library(plotly)
library(shiny)
library(DT)
data = read.csv("G:\\神机营\\20230328\\UAH-lower-troposphere-long.csv",sep="\t")


ui = shinyUI(
    #navbarPage session
    navbarPage(
      
      # sidebar style
      sidebarLayout(sidebarPanel("here is my
                                                                    homework ps06"),
                    mainPanel(
                      #show the result in each panel
                      tabsetPanel(tabPanel("the general information about the dataset:",radioButtons("var1","please select variable type:",c("num","chr")),DTOutput("table")),tabPanel("plot",radioButtons("var2","choose variable:",colnames(data)[-4]),plotlyOutput("plot")),
                                  tabPanel("table",radioButtons("var3","Average over:",c("year","month")),DTOutput("table1"))
                                  
                      ))
      )))
  
  
  server = function(input, output, session) {
    output$table <- renderDT({
      if (input$var1=="num"){
        apply(data[,-3],2,fivenum)
      } else {
        data.frame(table(data$region))
      }
    })
    
    output$plot <- renderPlotly({
      data1 <- data%>%select(input$var2,temp)
      colnames(data1)[1] <- "var"
      plot_ly(data1,x = ~var, y = ~temp, type = "bar")
    })
    
    output$table1 <- renderDT({
      data1 <- data%>%select(input$var3,region,temp)
      colnames(data1)[1] <- "decade"
      data1 <- data1%>%group_by(decade,region)%>%summarise(avg_temp=round(mean(temp),2))
      
      data1
    })
    
  }
shinyApp(ui=ui,server=server)