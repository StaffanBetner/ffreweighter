library(shiny)
shinyUI(
  navbarPage(
    "Reweighter of Family Finder Matches List",
    tabPanel(
      "Tool",
      sidebarPanel(
        fileInput('file', 'Upload one CSV File with Chromosome Browser Results (Max. Size 50 MB)',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple = F),
       # checkboxInput("checkbox", "Experimental weighting of segments with excessive overlapping (slow)", value = FALSE, width = NULL),
        helpText("Read the about page before use.")
      ),
      mainPanel(
        dataTableOutput("table")
      )),
    tabPanel("About",
             mainPanel(helpText("The tool weights each segment after the probability that it is a identical by descent (IBD) segment.",br(),
               "Chromium based (e.g. Chrome or Opera) browsers are recommended, Microsoft Edge won't work.", br(), 
                                "The uploaded files will only be stored temporary and will be deleted after the session is closed.", br(),
                             #   tags$p("Source available",tags$a(href = "https://github.com/StaffanBetner/overlappingsegments", "here.")),br(),
                                "Contact: staffan@betner.nu or @StaffanBetner (Twitter)")))
  ))