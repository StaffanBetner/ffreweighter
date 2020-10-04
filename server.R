pacman::p_load(shiny, tidyverse, htmlwidgets, data.table, dtplyr, rio, tidytable, shinycssloaders)

options(shiny.maxRequestSize = 50*1024^2)

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
      inFile()$datapath  %>% 
        import(encoding = "UTF-8", quote = "", setclass = "data.table") %>% 
        lazy_dt() %>% 
        transmute(name = Name,
                  match_name = `Match Name`,
                  chromosome = Chromosome,
                  start_location = `Start Location`,
                  end_location = `End Location`,
                  centimorgans = Centimorgans) %>% 
        filter(chromosome != "X") %>% 
        mutate(new_cm = centimorgans*plogis(-5.86584 + 0.78623*centimorgans),
               chromosome = chromosome %>% parse_number(),
               boolean = centimorgans > 7) %>% 
        as.data.table() -> step1
      
      setkey(step1, match_name, chromosome, start_location, end_location)
      
      overlaps = foverlaps(step1, step1, type="any", which = TRUE)[xid != yid]$xid
      step1$overlaps <- FALSE
      step1$overlaps[overlaps] <- TRUE
      
      step1 %>% 
        lazy_dt() %>% 
        group_by(match_name, overlaps) %>% 
        summarise(number_of_segments = n(),
                  unweighted_sum_of_centimorgans = sum(centimorgans),
                  reweighted_sum_of_centimorgans = (sum(new_cm) - max(new_cm) + max(centimorgans)),
                  longest_segment = max(centimorgans),
                  `sum_of_>7_cM` = sum(centimorgans*boolean)) %>% 
        ungroup() %>% 
        mutate(scale_factor = (reweighted_sum_of_centimorgans/unweighted_sum_of_centimorgans),
               effective_number_of_segments = number_of_segments*scale_factor,
               average_cM_per_effective_segment = (reweighted_sum_of_centimorgans/effective_number_of_segments)) %>% 
        arrange(desc(unweighted_sum_of_centimorgans)) %>% 
        transmute(`# (by shared cM at FTDNA)` = row_number(),
                  `MATCH NAME` = match_name,
                  `OVERLAPPING SEGMENTS WITHIN NAME` = overlaps, 
                  `UNWEIGHTED SUM OF CENTIMORGANS` = round(unweighted_sum_of_centimorgans, 2),
                  `REWEIGHTED SUM OF CENTIMORGANS` = round(reweighted_sum_of_centimorgans, 2),
                  `SUM OF >7 cM` = round(`sum_of_>7_cM`, 2),
                  `LONGEST SEGMENT` = round(longest_segment, 2),
                  `NUMBER OF SEGMENTS` = number_of_segments,
                  `EFFECTIVE NUMBER OF SEGMENTS` = round(effective_number_of_segments, 2),
                  `AVERAGE cM PER EFFECTIVE SEGMENT` = round(average_cM_per_effective_segment, 2),
                  `SCALE FACTOR (%)` = (scale_factor*100) %>% round(2)) %>% 
        arrange(desc(`REWEIGHTED SUM OF CENTIMORGANS`)) %>% 
        as.data.table() ->
        out
      
      if (input$anonymize == T) {out %>% lazy_dt() %>% select(-`MATCH NAME`) %>% as.data.table() -> out}
      
      out}
  })
  
  # output$downloadData_xlsx <- downloadHandler(
  #   filename = "reweighted matching list.xlsx", 
  #   content = function(file){
  #     openxlsx::write.xlsx(myData(), 
  #                      file, 
  #                      row.names = FALSE)
  #   }
  # )
  
  observe({
    output$table <- renderDataTable({if (is.null(inFile())) {
      return(NULL)
    } else {DT::datatable(
      myData(), rownames = FALSE,
      filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     buttons = list('excel', "csv"),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE))}}#, escape = F, 
    )})
  
})
