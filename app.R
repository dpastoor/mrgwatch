library(shiny)
library(miniUI)
library(dplyr)
ui <- miniPage(
  gadgetTitleBar("watch mrgsolve models"),
  miniContentPanel(
                   DT::dataTableOutput('modelStatus'),
                   height = "100%"
  ),
  miniButtonBlock(
    actionButton("reset", "Reset")
  )
)

server <- function(input, output, session) {
  data_available <- reactivePoll(2000, session, 
                                 checkFunc = function() {
                                   file.info(list.files(pattern = "*.cpp")) %>%
                                     select(-atime)
                                 },
                                 valueFunc = function() {
                                   file.info(list.files(pattern = "*.cpp")) %>%
                                     select(mtime)
                                 })
  # rv <- reactiveValues(
  #   tbl = data.frame(model = c("mod1", "mod2"), mod_time = c(as.numeric(Sys.time()) %% 86000, as.numeric(Sys.time()) %% 86000))
  # )
  output$modelStatus <- DT::renderDataTable({
    data_available() %>% mutate(model = row.names(.),
                                working = TRUE) %>%
      select(model, everything()) 
  }, class = "cell-border stripe compact", 
  options = list(pageLength = 10, dom = 'tip'))
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
  
  observeEvent(input$reset, {
    TRUE
  })
}

runGadget(ui, server, viewer = paneViewer())