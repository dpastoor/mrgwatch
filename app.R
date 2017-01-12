library(shiny)
library(miniUI)
library(dplyr)
ui <- miniPage(
  gadgetTitleBar("watch mrgsolve models"),
  miniTabstripPanel(
    miniTabPanel("models",
                 
  miniContentPanel(
                   DT::dataTableOutput('modelStatus'),
                   height = "100%"
  ),
  miniButtonBlock(
    actionButton("reset", "Reset")
  )
  ),
  miniTabPanel("errors",
  miniContentPanel(
    textOutput("modelErrors")
  )
  )
  )
)

server <- function(input, output, session) {
  tmpdir <- tempdir()
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
  model_errors <- reactive({
    "no errors"
  })
  model_output <- reactive({
    model_data <- data_available() %>% mutate(model = row.names(.)) %>%
      select(model, everything()) 
    
    worked <- lapply(model_data$model, function(m) {
      works <- tryCatch({
        mrgsolve::mread_cache(gsub(".cpp", "", m), soloc = tmpdir)
        return(TRUE)
    }, error = function(e) {
      return(e)
    })
    })
    did_work <- vapply(worked, function(w) {
      ifelse(isTRUE(w), TRUE, FALSE)
    }, logical(1)) 
    if (!all(did_work)) {
      model_errors <- paste(worked[!did_work], collapse = "\n\n")
    }
    model_data$worked <- did_work
    return(model_data)
  })
  
  output$modelStatus <- DT::renderDataTable({
    model_output()
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