library(shiny)
library(compareR)

# dataset musi byc juz odfiltrowany

radarControlsUI <- function(id,
                            # dataset,
                            # categ_vars,
                            # id_var,
                            # grouper,
                            mapped_vars#,
                            # aggregation
) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("grouping_var_UI")),
    selectInput(ns("aggregation_type"),
                label = "Aggregation type",
                choices = c("mean", "median", "sd"),
                selected = "mean"),
    selectizeInput(ns("plot_vars"),
                   label = "Numeric variables for plot",
                   choices = mapped_vars,
                   selected = mapped_vars[c(1:3)],
                   options = list(maxItems = 5),
                   multiple = TRUE),
    uiOutput(ns("data_filters_UI")),
    uiOutput(ns("selected_levels")),
    actionButton(ns("radar_button"), label = "Go")
  )
}


radarControlsServer <- function(id, dataset, categ_vars#,
                                # id_var,
                                # grouper,
                                # mapped_vars,
                                # aggregation
) {
  moduleServer(
    id,
    function(input, output, session) {


      # == Reactive inputs ==

      output$grouping_var_UI <- renderUI({
        ns <- session$ns
        selectInput(ns("grouping_var"),
                    label = "Aggrebate by",
                    choices = categ_vars, #available_groupers(),
                    selected = categ_vars[1])
      })


      # One cen filter also categories
      available_filters <- reactive({
        # setdiff(categ_vars, input$grouping_var)
        categ_vars
      })


      output$data_filters_UI <- renderUI({
        ns <- session$ns
        # choices <- dataset[[categ_vars]]
        choices <- available_filters()
        selected <- choices[1]
        selectizeInput(ns("data_filters"),
                       label = "Categories to filter data by",
                       choices = choices,
                       selected = selected,
                       options = list(maxItems = 3),
                       multiple = TRUE)
      })


      output$selected_levels <- renderUI({
        ns <- session$ns
        req(input$data_filters)
        filter_vars <- input$data_filters
        # Utworz liste sliderow na podstawie select input wskaznikow, wybierz odpowiednie wartosci startowe (mean, max)
        lapply(1:length(input$data_filters), function(i) {
          tmp_cat <- filter_vars[i]
          choices <- dataset[[tmp_cat]]
          selected <- choices[1]
          selectizeInput(ns(paste0("filter_", i)),
                         label = tmp_cat,
                         choices = choices,
                         selected = selected,
                         options = list(maxItems = 3),
                         multiple = TRUE)
        })
      })


      generate_radar <- eventReactive(input$radar_button, {
        inputs <- reactiveValuesToList(input)
        selected_filters <- inputs %>% .[grepl("filter_[0-9]+", names(.))] %>% .[order(names(.))] %>% setNames(inputs$data_filters)
        result_plot <- compareR::draw_radar(data=dataset, id_var='state', grouper=input$grouping_var, mapped_vars=input$plot_vars, agreg_type=input$aggregation_type, filters=selected_filters)
        return(result_plot)
      })
      #
      # output$radar_plot <- plotly::renderPlotly(generate_radar())
      # radar_plot <- generate_radar()

      return(list(radar=generate_radar, filters=available_filters))

      # myreturn <- reactiveValues()



    }
  )
}


ui <- fluidPage(

  fluidPage(
    sidebarLayout(
      sidebarPanel(
        radarControlsUI("USA_data", mapped_vars = c("Population", "Income","Illiteracy", "Life Exp", "Murder", "HS Grad", "Frost", "Area"))
      ),
      mainPanel(
        textOutput("my_filters"),
        br(),
        plotly::plotlyOutput("radar_plot", height="400") # %>%  shinycssloaders::withSpinner(color = "#3F3138", type = 6)
      )
    )
  )


)

server <- function(input, output, session) {

  radar_plot_obj <- radarControlsServer("USA_data", compareR::usa_data, categ_vars=c("region", "rand_status", "rand_class"))

  # generate_radar <- eventReactive(input$radar_button, {
  #   inputs <- reactiveValuesToList(input)
  #   selected_filters <- inputs %>% .[grepl("filter_[0-9]+", names(.))] %>% .[order(names(.))] %>% setNames(inputs$data_filters)
  #   result_plot <- compareR::draw_radar(data=dataset, id_var='state', grouper=input$grouping_var, mapped_vars=input$plot_vars, agreg_type=input$aggregation_type, filters=selected_filters)
  #   return(result_plot)
  # })

  output$my_filters <- renderText(radar_plot_obj$filters())
  output$radar_plot <- plotly::renderPlotly(radar_plot_obj$radar())

}

shinyApp(ui, server)
