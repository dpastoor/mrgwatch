library(shiny)
library(miniUI)
library(dplyr)
library(DT)
WATCH_LOCATION <-  "../vanco-models/models"
ui <- miniPage(
  gadgetTitleBar("watch mrgsolve models"),
  miniTabstripPanel(
    miniTabPanel("models",
                 miniContentPanel(
                   DT::dataTableOutput('modelStatus')
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
  data_available <- reactivePoll(500, session, 
                                 checkFunc = function() {
                                   file.info(normalizePath(list.files(WATCH_LOCATION, pattern = "*.cpp", full.names = T))) %>%
                                     select(-atime)
                                 },
                                 valueFunc = function() {
                                   file.info(normalizePath(list.files(WATCH_LOCATION, pattern = "*.cpp", full.names = T))) %>%
                                     select(mtime)
                                 })
  # rv <- reactiveValues(
  #   tbl = data.frame(model = c("mod1", "mod2"), mod_time = c(as.numeric(Sys.time()) %% 86000, as.numeric(Sys.time()) %% 86000))
  # )
  model_output <- reactive({
    model_data <- data_available() %>% mutate(model = row.names(.)) %>%
      select(model, everything()) 
    
    worked <- lapply(model_data$model, function(m) {
      works <- tryCatch({
        mrgsolve::mread_cache(basename(gsub(".cpp", "", m)),project = WATCH_LOCATION, soloc = tmpdir)
        return(TRUE)
      }, error = function(e) {
        return(e)
      })
    })
    did_work <- vapply(worked, function(w) {
      ifelse(isTRUE(w), TRUE, FALSE)
    }, logical(1)) 
    
    model_data$worked <- did_work
    model_data$potential_errors <- worked
    
    if (!all(did_work)) {
      if (requireNamespace("notifier", quietly = T)) {
        failed <- model_data %>% filter(!worked)
        msg <- paste(failed$potential_errors, collapse = "\n\n")
        notifier::notify(
          title = "model compilation failed",
          msg = msg 
        )
      }
    }
    
    return(model_data)
  })
  
  output$modelStatus <- DT::renderDataTable({
    model_output() %>% select(-potential_errors)
  }, class = "cell-border stripe compact", 
  options = list(pageLength = 10, dom = 't'))
  
  output$modelErrors <- renderText({
    # need to also track model_output so will re-compile models
    # while on the error panel
    out <- model_output()
    if(!all(out$worked)) {
      failed <- out %>% filter(!worked)
      return(paste(failed$potential_errors, collapse = "\n\n"))
    }
    return("no errors")
    
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
  
  observeEvent(input$reset, {
    TRUE
  })
}

runGadget(ui, server, viewer = paneViewer())