#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


server <- function(input, output){
  
  # Confidence Level
  cl <- reactive({
    if (is.na(input$id)){
      return(NA)
    }else{
      # Plant the random number seed
      set.seed(input$id + 1)
      cl <- sample(88:96, 1)
      return(cl)
    }
  })
  
  # Set the mean for numerical data
  mu <- reactive({
    
    # Plant the random number seed
    if (is.na(input$id)){
      return(NULL)
    }else{
      set.seed(input$id+1) 
      mu <- 1.671
    }})
  
  # Creating categorical data
  catData <- reactive({
    if (is.na(input$id)){
      userdata <- data.frame(Individual=NA, Concern=NA)
      return(userdata)
    }else{
      # Plant the random number seed
      set.seed(input$id+1) 
      cl <- sample(88:96, 1)
      n <- sample(100:200, 1)
      
      A_one <- 1:n
      group_one <- rep(c('COVID-19'), times = 7)
      group_two <- rep(c('Economy'), times = 3)
      population <- c(group_one, group_two)
      A_two <- sample(population, n, replace = TRUE)
      
      userdata <- data.frame(A_one, A_two)
      colnames(userdata) <- c('Individual', 'Concern')
      return(userdata)
    }
  }) # end reactive
  
  # Creating numerical data
  numData <- reactive({
    # Plant the random number seed
    if (is.na(input$id)){
      userdata <- data.frame(Station=NA, Price=NA, Notes=NA)
      return(userdata)
    }else{
      set.seed(input$id+1) 
      n <- sample(30:40, 1)
      
      A_one <- 1:n
      A_two <- sample(rnorm(1000, 1.671, 0.12), n)
      A_three <- A_one %% 2
      
      userdata <- data.frame(A_one, A_two, A_three)
      col_headings <- c('Station', 'Price', 'Notes')
      colnames(userdata) <- col_headings
      return(userdata)
    }
  }) # end reactive
  
  output$downloadnum <- downloadHandler(
    filename = function() {
      paste('Num', input$id, '.csv', sep='')
    },
    content = function(file) {
      write.csv(data.frame(numData()[,1:2]), file, row.names = FALSE)
    }
  )
  
  output$downloadcat <- downloadHandler(
    filename = function() {
      paste('Cat', input$id, '.csv', sep='')
    },
    content = function(file) {
      write.csv(data.frame(catData()), file, row.names = FALSE)
    }
  )
  
  # Output: Textblock 1 ----
  output$textblocknum <- renderText({
    out <- paste("<ul>",
                 "<li>Each row represents the regular gas price ($/gal) from one gas station</li>"
                 )
    
    if(!is.na(input$id) & (input$id >=7000 | input$id < 2000)){
      out <- paste(out, 
                   "<li>Find the <font color=\"#FF0000\"><b>", cl(), "%</b></font> confidence interval for the true average price of regular gas.</li>"
                   )
    }
    
    out <- paste(out, "</ul>")
    out
  })
  
  output$textblockcat <- renderText({
    out <- paste("<ul>",
                 "<li>Each row represents one person in the sample.</li>",
                 "<li>The second column shows people's concerns regarding the stay-at-home restrictions.</li>",
                 "<li><q>COVID-19</q> means people are more concerned that relaxing stay-at-home restrictions would lead to more COVID-19 deaths.</li>",
                 "<li><q>Econmy</q> means people are more concerned that the stay-at-home restrictions would hurt the U.S. economy.</li>"
                 )
    
    if(!is.na(input$id) & (input$id <7000 & input$id >= 2000)){
      out <- paste(out, 
                   "<li>Find the <font color=\"#FF0000\"><b>", cl(), "%</b></font> confidence interval for the true proportion of people who are more concerned that relaxing stay-at-home restrictions would lead to more COVID-19 deaths.</li>"
      )
    }
    
    out <- paste(out, "</ul>")
    out
  })
  
  
  # Output: Sample data table ----
  output$catTable <- DT::renderDT({
    datatable(catData(), rownames = FALSE, options = list(
      pageLength = 200,
      dom = "t",
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:1))))%>%
      formatStyle("Concern", backgroundColor=styleEqual(c("COVID-19","Economy"),c('orange','gray')))
  },striped = TRUE,colnames = TRUE)
  
  # Output: Sample data table ----
  output$numTable <- DT::renderDT({
    datatable(numData(), rownames = FALSE, options = list(
      pageLength = 50,
      dom = "t",
      columnDefs = list(list(className ='dt-center', targets = 0:1), 
                        list(visible=FALSE, targets=2))))%>%
      formatStyle("Notes", backgroundColor=styleEqual(c(1,0),c('orange','gray')), target = "row")%>%
      formatRound(columns=c('Price'), digits=3)
  },striped = TRUE,colnames = TRUE)
  
} #end server
