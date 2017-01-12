library(shiny)
library(miniUI)

ui <- miniPage(
  gadgetTitleBar("watch mrgsolve models"),
  miniContentPanel(padding = 0,
                   tableOutput("modelStatus")
  ),
  miniButtonBlock(
    actionButton("reset", "Reset")
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    tbl = data.frame(model = c("mod1", "mod2"), mod_time = c(as.numeric(Sys.time()) %% 86000, as.numeric(Sys.time()) %% 86000))
  )
  output$modelStatus <- renderTable(rv$tbl)
  observeEvent(input$done, {
    stopApp(TRUE)
  })
  
  observeEvent(input$reset, {
    print('reset')
    rv$tbl <- data.frame(model = c("mod1", "mod2"), mod_time = c(as.numeric(Sys.time()) %% 86000, as.numeric(Sys.time()) %% 86000))
  })
}

runGadget(ui, server, viewer = paneViewer())