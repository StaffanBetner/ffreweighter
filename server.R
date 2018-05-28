library(shiny)
library(tidyverse)
options(shiny.maxRequestSize=50*1024^2)
calc_prob <- function(cM){(exp(-3.5+0.64*cM)/(1+exp(-3.5+0.64*cM))) %>% round(2)}

shinyServer(function(input, output, session) {

  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
    }
  })
  
  myData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      out <- inFile()$datapath %>% read_csv %>% 
        unique() %>% 
        mutate(new_cm = CENTIMORGANS*calc_prob(CENTIMORGANS)) %>% 
        group_by(MATCHNAME) %>% 
        summarise(`UNWEIGHTED SUM OF CENTIMORGANS` = sum(CENTIMORGANS) %>% round(2),
                  `REWEIGHTED SUM OF CENTIMORGANS` = (sum(new_cm)-max(new_cm)+max(CENTIMORGANS)) %>% round(2),
                  `LONGEST SEGMENT` = max(CENTIMORGANS) %>% round(2)) %>% ungroup %>% arrange(desc(`REWEIGHTED SUM OF CENTIMORGANS`))
      out}
  })
  
  observe({
    output$table <- renderDataTable({ if (is.null(inFile())) {
      return(NULL)
    } else {myData()}})})
  
})
