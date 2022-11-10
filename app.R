library(shiny)
library(tidyverse)
library(reactable)
library(stringr)
library(snakecase)


marathon_data = read_csv("joined_marathon_data.csv") %>% mutate(BMI = round(BMI,2), Power = round(Power,2))

ui <- fluidPage(

    # Application title
    titlePanel("Tplyr UI"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
          radioButtons("radio_buttons", "Default Dataset or Imported",
                       choices = c("Default","Imported")),
          fileInput("file", "Import Dataset (csv only)", multiple = F,
                    accept = ".csv"),
          selectInput("variable_choice1","Select Your Treatment Variable", choices = c("var1","var2","var3")),
          selectInput("variable_choice2","Select Variable 1 for Analysis", choices = c("var1","var2","var3")),
          radioButtons("group_type2","Analysis Type for Variable 1", choices = c("Count/Proportions","Summary Stats")),
          selectInput("variable_choice3","Select Variable 2 For Analysis", choices = c("var1","var2","var3")),
          radioButtons("group_type3","Analysis Type for Variable 2", choices = c("Count/Proportions","Summary Stats"), selected = "Summary Stats"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Dataset",
                     br(),
                     br(),
                     reactableOutput("data")
                     ),
            tabPanel("Summary Table",
                     div(style = "display:inline-block; float:right",downloadButton('download_csv', 'Download Summary as .csv')),
                     br(),
                     br(),
                     reactableOutput("summary"),
                     br(),
                     br(),
                     h5("Field Select"),
                     textOutput("test"),
                     br(),
                     br(),
                     reactableOutput("table_sc"),
                     br(),
                     br(),
                     h5("Field Select"),
                     textOutput("test_click2"),
              
            )
        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  snakecase_cols = function(x){
    names(x) <- tolower(names(x))
    names(x) <- trimws(str_replace_all(names(x), "[^[:alnum:]]", " "))
    names(x) <- str_replace_all(names(x), " ", "_")
    names(x) <- str_replace_all(names(x), "__", "_")
    return(x)
    
  }

  default_data_table = reactive({
      reactable(marathon_data, bordered = TRUE, striped = TRUE, highlight = TRUE,
                defaultPageSize = 20, filterable = TRUE)
  })
  
  imported_data = reactive({
    file <- input$file
    if (is.null(file)){
      return(NULL)
    } else {
      reactable(read.csv(file$datapath), bordered = TRUE, striped = TRUE, highlight = TRUE,
                defaultPageSize = 20, filterable = TRUE)
    }
  })
  
  data = reactive({
    if (input$radio_buttons == "Default"){
      default_data_table()
    } else if(is.null(input$file)){
      showNotification("Please import a csv file", duration = 45, closeButton = FALSE, type = "error")
      default_data_table()
    } else {
      imported_data()
    }
  })
  
  data_tibble = reactive({
    if (input$radio_buttons == "Default"){
      marathon_data
    } else if(is.null(input$file)){
      showNotification("Please import a csv file", duration = 45, closeButton = FALSE, type = "error")
      marathon_data
    } else {
      file <- input$file
      read.csv(file$datapath)
    }
  })
  
  variable_options = reactive(as_tibble(names(data_tibble())))
  
  observe({
    updateSelectInput(session, "variable_choice1", choices = variable_options(), selected = tail(variable_options(),n=1))
  })
  
  observe({
    updateSelectInput(session, "variable_choice2", choices = variable_options(), selected = variable_options()[2,] )
  })
  
  observe({
    updateSelectInput(session, "variable_choice3", 
                      choices = variable_options(), selected = variable_options()[3,] )
  })
  
  observeEvent(input$file, {
    if (is.numeric(variable1())){
      updateRadioButtons(session, "group_type2", selected = "Summary Stats")
    } else {
      updateRadioButtons(session, "group_type2", selected = "Count/Proportions")
    }
  })
  
  observeEvent(input$file, {
    if (is.numeric(variable1())){
      updateRadioButtons(session, "group_type3", selected = "Summary Stats")
    } else {
      updateRadioButtons(session, "group_type3", selected = "Count/Proportions")
    }
  })
  
  group_type = function(data, input_group, input_var){
      if(input_group=="Count/Proportions"){
        data %>% add_layer(
          group_count(!!as.name(input_var), by = !!input_var)
        )
      } else {
        data %>% add_layer(
          group_desc(!!as.name(input_var), by = !!input_var)
        )
      }
  }
  
  
  variable1 = reactive(data_tibble() %>% select(input$variable_choice1) )
  
  variable2 = reactive(data_tibble() %>% select(input$variable_choice2) )
  
  variable3 = reactive(data_tibble() %>% select(input$variable_choice3) )
  
  row_id = reactive(seq(1,length(variable1()), length.out = 1))
  
  var_data = reactive(bind_cols(row_id(),variable1(),variable2(), variable3()))
  
  tplyr_tab = reactive({
    tplyr_table(var_data(), !!rlang::sym(input$variable_choice1)) %>%
      group_type(input_group = input$group_type2, input_var = input$variable_choice2) %>%
      group_type(input_group = input$group_type3, input_var = input$variable_choice3) %>%
      build() %>% apply_row_masks() %>%
      select(starts_with("row"), starts_with("var")) %>%
      reactable(. , sortable = FALSE, bordered = TRUE, highlight = TRUE,
                onClick = JS("function(rowInfo, colInfo) {
                      if (window.Shiny) {
                        Shiny.setInputValue('row', { index: rowInfo.index + 1 })
                        Shiny.setInputValue('col', { column: colInfo.id })
                        }
                    }"))
    
  })
  
  tplyr_tibble = reactive({
    tplyr_table(var_data(), !!rlang::sym(input$variable_choice1)) %>%
      group_type(input_group = input$group_type2, input_var = input$variable_choice2) %>%
      group_type(input_group = input$group_type3, input_var = input$variable_choice3) %>%
      build() %>%
      select(starts_with("row"), starts_with("var"))
    
  })
  
  row <- reactive(input$row$index)
  col <- reactive(input$col$column)
  
  test = reactive(paste0("Row = ", row(), "  | Column = ", col()))
  
  
  output$data = renderReactable(data())
  output$summary = renderReactable(tplyr_tab())
  output$test = renderText(test())
  
  output$download_csv <- downloadHandler(
    filename = function(){paste0("summary_data_",Sys.time(),".csv")}, 
    content = function(fname){
      write.csv(tplyr_tibble(), fname)
    }
  )
  
  data_sc = reactive(snakecase_cols(data_tibble()))
  
  choice1 = reactive(to_snake_case(input$variable_choice1))
  choice2 = reactive(to_snake_case(input$variable_choice2))
  choice3 = reactive(to_snake_case(input$variable_choice3))
  
  variable1_sc = reactive(data_sc() %>% select(choice1()) )
  
  variable2_sc = reactive(data_sc() %>% select(choice2()) )
  
  variable3_sc = reactive(data_sc() %>% select(choice3()) )
  
  var_data_sc = reactive(bind_cols(variable1_sc(),variable2_sc(), variable3_sc()))
  
  tplyr_sc = reactive({
    tplyr_table(data_sc(), !!rlang::sym(choice1())) %>%
      group_type(input_group = input$group_type2, input_var = choice2()) %>%
      group_type(input_group = input$group_type3, input_var = choice3()) %>%
      build() %>% # adding 'metadata=T' here produces the following error: Problem while computing `meta = build_count_meta(...)`. Caused by error in `values[[1]]`: ! subscript out of bounds 
      select(starts_with("row"), starts_with("var")) %>% apply_row_masks() %>%
      reactable(. , sortable = FALSE, bordered = TRUE, highlight = TRUE,
                onClick = JS("function(rowInfo, colInfo) {
                      if (window.Shiny) {
                        Shiny.setInputValue('row2', { index: rowInfo.index + 1 })
                        Shiny.setInputValue('col2', { column: colInfo.id })
                        }
                    }"))
    
  })
  
  row2 <- reactive(tplyr_sc()[input$row$index,1]$row_id)
  col2 <- reactive(input$col$column)
  
  test_click2 = reactive(paste0("Row = ", row2(), "  | Column = ", col2()))
  
  output$table_sc = renderReactable(tplyr_sc())
  
  output$test_click2 = renderText(test_click2())
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
