library(shiny)
library(DT)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    * {
                    font-family: Palatino,garamond,serif;
                    font-weight: 500;
                    line-height: 1.2;
                    #color: #000000;
                    }
                    "))
    ),    
  # App title 
  titlePanel(title="STA126 Test 4 Data Sets (Spring 2020)"),
  
  # Sidebar layout 
  sidebarLayout(
    
    # Sidebar objects
    sidebarPanel(
      
      numericInput(inputId = "id",
                   "Enter the last four of your MU ID", 
                   value=NULL),  
      # Object
      #actionButton(inputId = "newdata", "Get a Data Set"),
      
      # Download
      downloadButton(outputId = "downloadnum", label = "Download Question 7 Data", class = NULL, width = '155px'),
      
      # Download
      downloadButton(outputId = "downloadcat", label = "Download Question 8 Data", class = NULL, width = '155px'),
      
      # Sidebar width can not exceed 12, default 4.
      width = 4
    ), # end of sidebar panel
    
    # Main panel----
    mainPanel(
      
      tabsetPanel(
        tabPanel("Question 7", 
                 fluidRow(column(12, htmlOutput("textblocknum"))),
                 fluidRow(column(12, DT::dataTableOutput("numTable"))
                 )),
        tabPanel("Question 8",
                 fluidRow(column(12, htmlOutput("textblockcat"))),
                 fluidRow(column(12, DT::dataTableOutput("catTable"))
                 )
        ))
      # Display textblock 1 ----
      #verbatimTextOutput("textblock1"),
      
      
      # Display sample data table ----
      # tableOutput("table1")
      
      
      
    ) #end of mainPanel
    
  ) #end of sidebarlayout
  
) #end of fluidpage
