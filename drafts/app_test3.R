library(shiny)
# source("modules/module_1.R")
# https://stackoverflow.com/questions/48882427/how-to-store-the-returned-value-from-a-shiny-module-in-reactivevalues

returnUI = function(id) {
  ns <- NS(id)

  tagList(
    textInput(ns("txt"), "Write something")
  )
}

returnServer = function(input, output, session) {
  myreturn <- reactiveValues()

  observe({ myreturn$txt <- input$txt })

  return(myreturn)
}


ui <- fluidPage(

  returnUI("returntxt"),
  textOutput("mytxt")

)

server <- function(input, output, session) {

  myvals <- reactiveValues(
    txt = NULL
  )

  mytxt <- callModule(returnServer, "returntxt")

  observe({
    myvals$txt <- mytxt$txt
    print(myvals$txt)
  })

  output$mytxt <- renderText({ myvals$txt })

}

shinyApp(ui, server)
