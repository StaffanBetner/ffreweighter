pacman::p_load(shiny, htmlwidgets, shinydashboard, DT, shinycssloaders)
dashboardPage(
  dashboardHeader(title = "Reweighter of Family Finder Matches List", titleWidth = 400),
  dashboardSidebar(fileInput('file', HTML("Upload one CSV File with<br />Chromosome Browser Results<br />(Max. Size 50 MB)"),
                             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple = F, width = '95%'),
                   checkboxInput("anonymize", "Hide names of matches", value = FALSE),
                   helpText("The tool weights each segment by the probability that it is a identical by descent (IBD) segment. The segments are summed up for each match name, so several matches with the same name will be combined." ,
                            "If any segments within a match name overlaps with another segment in the same name set, \"overlapping segments within name\" will be set as \"true\".",
                           # "However, if you also upload a matches list, then the segments will be allocated so that the sum will check out. There might be errors in smaller segment however.",
                            br(),
                           br(),
                            "Chromium based (e.g. Chrome or Opera) browsers are recommended, Microsoft Edge is not supported.", 
                            br(), 
                           br(),
                            "The uploaded files will only be stored temporary and will be deleted after the session is closed.", br(),
                            tags$p("Source available",tags$a(href = "https://github.com/StaffanBetner/ffreweighter/", "here.")),br(),
                            "Contact: staffan@betner.nu or @StaffanBetner (Twitter)"),
                   width = 350),
  dashboardBody(DTOutput("table") %>% withSpinner()))
