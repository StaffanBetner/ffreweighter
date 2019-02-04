library(shiny)
library(tidyverse)
library(sparkline)
library(htmlwidgets)
#library(DT)
options(shiny.maxRequestSize=50*1024^2)
calc_prob <- function(cM){(exp(-5.86584+0.78623*cM)/(1+exp(-5.86584+0.78623*cM))) %>% round(2)}
#interval <- function(x,y){x:y}
#integrater <- function(a,b,c){integrate(approxfun(a), lower=b, upper=c)$value}

#chromosomal_max <- tibble(CHROMOSOME = (1:22), 
#                          mbp_max = c(247,243,199,191,181,171,159,146,140,135,134,132,114,106,100,89,79,76,64,62,47,50))
probs_sparklines <- readRDS("probs_sparklines.RDS")
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
      #out <- inFile()$datapath %>% read_csv %>% unique %>% 
      #  mutate(new_cm = CENTIMORGANS*calc_prob(CENTIMORGANS)) %>% filter(CHROMOSOME != "X")
      
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
     # segments_summary <- out %>% group_by(MATCHNAME) %>% summarise(`NUMBER OF SEGMENTS` = n())
      out <- inFile()$datapath %>% 
        read_delim(delim = ",", 
                   quote = "", 
                   col_types = cols(
                     Name = col_character(),
                     `Match Name` = col_character(),
                     Chromosome = col_character(),
                     `Start Location` = col_integer(),
                     `End Location` = col_integer(),
                     Centimorgans = col_double(),
                     `Matching SNPs` = col_integer()
                   ), trim_ws = T)  %>% 
        unique %>% 
        mutate(`Match Name` = `Match Name` %>% gsub('[\"]', "",x = .)) %>% 
        rename(NAME = Name,
               MATCHNAME = `Match Name`,
               CHROMOSOME = Chromosome,
               `START LOCATION` = `Start Location`,
               `END LOCATION` = `End Location`,
               CENTIMORGANS = Centimorgans,
               `MATCHING SNPS` = `Matching SNPs`) %>% 
        mutate(new_cm = CENTIMORGANS*calc_prob(CENTIMORGANS)) %>% filter(CHROMOSOME != "X") %>% 
       # mutate(CHROMOSOME = CHROMOSOME %>% parse_number()) %>% 
        group_by(MATCHNAME) %>% mutate(`NUMBER OF SEGMENTS` = n()) %>% ungroup() %>% 
        group_by(MATCHNAME, `NUMBER OF SEGMENTS`) %>% 
        mutate(boolean = CENTIMORGANS>7) %>% 
        summarise(`UNWEIGHTED SUM OF CENTIMORGANS` = sum(CENTIMORGANS) %>% round(2),
                  `REWEIGHTED SUM OF CENTIMORGANS` = (sum(new_cm)-max(new_cm)+max(CENTIMORGANS)) %>% round(2),
                  `LONGEST SEGMENT` = max(CENTIMORGANS) %>% round(2),
                  `SUM OF >7 cM` = sum(CENTIMORGANS*boolean)) %>% 
        ungroup %>% 
        ungroup() %>% mutate(SCALING = (`REWEIGHTED SUM OF CENTIMORGANS`/`UNWEIGHTED SUM OF CENTIMORGANS`) %>% round(3)) %>% 
       # full_join(segments_summary) %>% 
        mutate(`EFFECTIVE NUMBER OF SEGMENTS` = `NUMBER OF SEGMENTS`*SCALING,
               `AVERAGE cM PER EFFECTIVE SEGMENT` = (`REWEIGHTED SUM OF CENTIMORGANS`/`EFFECTIVE NUMBER OF SEGMENTS`) %>% round(3)) %>% 
        select(MATCHNAME,
          `UNWEIGHTED SUM OF CENTIMORGANS`,
               `REWEIGHTED SUM OF CENTIMORGANS`,
          `SUM OF >7 cM`,
               `LONGEST SEGMENT`,
               `NUMBER OF SEGMENTS`,
               `EFFECTIVE NUMBER OF SEGMENTS`,
          `AVERAGE cM PER EFFECTIVE SEGMENT`,
               `SCALING`) %>% 
        mutate(cm = floor(`SUM OF >7 cM`)) %>% 
      #  left_join(probs_sparklines) %>% 
        select(-cm) %>% 
        arrange(desc(`UNWEIGHTED SUM OF CENTIMORGANS`)) %>% 
        mutate(`# (FTDNA)` = row_number()) %>% 
        select(`# (FTDNA)`, everything()) %>% 
        arrange(desc(`REWEIGHTED SUM OF CENTIMORGANS`))
      
      if(input$anonymize == T){out <- out %>% select(-MATCHNAME)}
      
      #out <- out %>% 
      #  group_by(MATCHNAME) %>% 
      #  summarise(`UNWEIGHTED SUM OF CENTIMORGANS` = sum(CENTIMORGANS) %>% round(2),
      #            `REWEIGHTED SUM OF CENTIMORGANS` = (sum(new_cm)-max(new_cm)+max(CENTIMORGANS)) %>% round(2),
      #            `LONGEST SEGMENT` = max(CENTIMORGANS) %>% round(2)) %>% ungroup %>% arrange(desc(`REWEIGHTED SUM OF CENTIMORGANS`)) %>% 
      #  ungroup() %>% mutate(SCALING = (`REWEIGHTED SUM OF CENTIMORGANS`/`UNWEIGHTED SUM OF CENTIMORGANS`) %>% round(3))
      out}
  })
  
  output$downloadData_xlsx <- downloadHandler(
    filename="reweighted matching list.xlsx", 
    content = function(file){
      openxlsx::write.xlsx(myData(), 
                       file, 
          #             sheetName = "Match list", 
                       row.names = FALSE)
    }
  )
  
  observe({
    output$table <- renderDataTable({ if (is.null(inFile())) {
      return(NULL)
    } else {myData()}}, escape = F#, options = list(drawCallback =  JS('function(){HTMLWidgets.staticRender()}'))#, 
  #  callback =  JS('function(){HTMLWidgets.staticRender()}')
    )})
  
})
