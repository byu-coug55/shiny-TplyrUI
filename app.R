library(shiny)
library(tidyverse)
library(reactable)


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
                     br(),
                     br(),
                     reactableOutput("summary")
              
            )
        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

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
  
  var_data = reactive(bind_cols(variable1(),variable2(), variable3()))
  
  tplyr_tab = reactive({
    tplyr_table(var_data(), !!rlang::sym(input$variable_choice1)) %>%
      group_type(input_group = input$group_type2, input_var = input$variable_choice2) %>%
      group_type(input_group = input$group_type3, input_var = input$variable_choice3) %>%
      build() %>%
      select(starts_with("row"), starts_with("var")) %>%
      reactable()
    
  }) 
  
  
  output$data = renderReactable(data())
  output$summary = renderReactable(tplyr_tab())
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)