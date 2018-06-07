library(shiny)
library(tidyverse)
options(shiny.maxRequestSize=50*1024^2)
calc_prob <- function(cM){(exp(-5.86584+0.78623*cM)/(1+exp(-5.86584+0.78623*cM))) %>% round(2)}
interval <- function(x,y){x:y}
integrater <- function(a,b,c){integrate(approxfun(a), lower=b, upper=c)$value}

chromosomal_max <- tibble(CHROMOSOME = (1:22), 
                          mbp_max = c(247,243,199,191,181,171,159,146,140,135,134,132,114,106,100,89,79,76,64,62,47,50))

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
      out <- inFile()$datapath %>% read_csv %>% unique %>% 
        mutate(new_cm = CENTIMORGANS*calc_prob(CENTIMORGANS)) %>% filter(CHROMOSOME != "X")
      
     # if(input$checkbox==T){densities <- out %>% 
     #   mutate(mbp_start = round(`START LOCATION`/1000000),
     #          mbp_end = round(`END LOCATION`/1000000),
     #          values = Vectorize(interval)(x=mbp_start, y=mbp_end)) %>% 
     #   unnest(values) %>% 
     #   mutate(CHROMOSOME = CHROMOSOME %>% parse_number()) %>% 
     #   full_join(chromosomal_max) %>% 
     #   group_by(CHROMOSOME) %>% 
     #   summarise(density = density(values, from = 0, to = unique(mbp_max)) %>% list)}
     # 
     # if(input$checkbox==T){out <- out %>% 
     #   mutate(mbp_start = round(`START LOCATION`/1000000),
     #          mbp_end = round(`END LOCATION`/1000000),
     #          CHROMOSOME = CHROMOSOME %>% parse_number()) %>% 
     #   full_join(densities) %>% 
     #   mutate(prob = 1-Vectorize(integrater)(density, mbp_start, mbp_end)) %>% select(-mbp_start, -mbp_end, -density) %>% 
     #   mutate(new_cm = new_cm*prob)}
      segments_summary <- out %>% group_by(MATCHNAME) %>% summarise(`NUMBER OF SEGMENTS` = n())
      out <- out %>% 
        group_by(MATCHNAME) %>% 
        summarise(`UNWEIGHTED SUM OF CENTIMORGANS` = sum(CENTIMORGANS) %>% round(2),
                  `REWEIGHTED SUM OF CENTIMORGANS` = (sum(new_cm)-max(new_cm)+max(CENTIMORGANS)) %>% round(2),
                  `LONGEST SEGMENT` = max(CENTIMORGANS) %>% round(2)) %>% ungroup %>% arrange(desc(`REWEIGHTED SUM OF CENTIMORGANS`)) %>% 
        ungroup() %>% mutate(SCALING = (`REWEIGHTED SUM OF CENTIMORGANS`/`UNWEIGHTED SUM OF CENTIMORGANS`) %>% round(3)) %>% 
        full_join(segments_summary) %>% mutate(`EFFECTIVE NUMBER OF SEGMENTS` = `NUMBER OF SEGMENTS`*SCALING) %>% 
        select(MATCHNAME,
          `UNWEIGHTED SUM OF CENTIMORGANS`,
               `REWEIGHTED SUM OF CENTIMORGANS`,
               `LONGEST SEGMENT`,
               `NUMBER OF SEGMENTS`,
               `EFFECTIVE NUMBER OF SEGMENTS`,
               `SCALING`)
      
      
      #out <- out %>% 
      #  group_by(MATCHNAME) %>% 
      #  summarise(`UNWEIGHTED SUM OF CENTIMORGANS` = sum(CENTIMORGANS) %>% round(2),
      #            `REWEIGHTED SUM OF CENTIMORGANS` = (sum(new_cm)-max(new_cm)+max(CENTIMORGANS)) %>% round(2),
      #            `LONGEST SEGMENT` = max(CENTIMORGANS) %>% round(2)) %>% ungroup %>% arrange(desc(`REWEIGHTED SUM OF CENTIMORGANS`)) %>% 
      #  ungroup() %>% mutate(SCALING = (`REWEIGHTED SUM OF CENTIMORGANS`/`UNWEIGHTED SUM OF CENTIMORGANS`) %>% round(3))
      out}
  })
  
  observe({
    output$table <- renderDataTable({ if (is.null(inFile())) {
      return(NULL)
    } else {myData()}})})
  
})
