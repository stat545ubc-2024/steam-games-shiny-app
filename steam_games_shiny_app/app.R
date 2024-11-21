library(shiny)
library(datateachr)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)

# Install the package which contains the dataset.
devtools::install_github("UBC-MDS/datateachr")


#Information of page1 (table)
page1_ui <- fluidPage(
  #center the column headers
  align = "center",
  #add title of page 1
  titlePanel("Steam Games"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("You can select one or more words below to see the detail information."),
      
      #select base dataset
      selectInput(
        inputId = "selected_variables",
        label = "Select Vector(s) to Display:",
        choices = setdiff(names(steam_games), c("name", "id")),
        selected = NULL,
        multiple = TRUE
      ),
      
      # name of the output of second search bar
      uiOutput("second_search"),
      
      #set the reset button
      actionButton("reset", "Reset"),
      
      # set the download button
      downloadButton("download_table", "Download Table"),
      
      #set the switch page button (go to the graph page)
      actionButton("go_to_page2", "Go to Graph Page")
    ),
    
    mainPanel(
      #header of table
      h3("Steam Games Table"),
      
      #name of display data table
      DT::dataTableOutput("data_table")
    )
  )
)



#Information of page2 (Graph)
page2_ui <- fluidPage(
  
  # title of the second page
  titlePanel("Steam Games - Distribution Graph"),
  sidebarLayout(
    sidebarPanel(
      helpText("Use the buttons below to switch between graphs."),
      sliderInput(
        inputId = "range",
        
        #build a slider for filtering data by discount price range
        label = "Select range of discount price:",
        min = min(steam_games$discount_price, na.rm = TRUE), max = max(steam_games$discount_price, na.rm = TRUE), 
        value = c(min(steam_games$discount_price, na.rm = TRUE), max(steam_games$discount_price, na.rm = TRUE)), step = 1
      ),
      
      # set a button for distribution of discount price
      actionButton("hist_discount_price", "Discount Price"),
      
      # set a button for distribution of original price
      actionButton("hist_original_price", "Original Price"),
      
      # set a button for distribution of achievements
      actionButton("hist_achievements", "Achievements"),
      
      #set the switch page button (back to the table page)
      actionButton("go_to_page1", "Go to Table Page") 
    ),
    mainPanel(
      #dynamic heading for the graph
      h3(textOutput("current_graph_title")),
      
      #name of the output of graph
      plotOutput("steam_games_graph") 
    )
  )
)



#main ui; switch between page1 and page2
ui <- fluidPage(
  uiOutput("page_content")
)



#define the server logic
server <- function(input, output, session) {
  
  #set the default page
  current_page <- reactiveVal("page1")
  
  #set the default graph
  current_graph <- reactiveVal("discount_price")
  
  #logic of switches button
  output$page_content <- renderUI({
    if (current_page() == "page1") {
      page1_ui
    } else if (current_page() == "page2") {
      page2_ui
    }
  })
  
  #Page 1
  #clear the selected variable when click the reset button
  observeEvent(input$reset, {
    updateSelectInput(session, "selected_variables", selected = character(0))
  })
  
  # If there is no input in the second (or more) search bar/slider, return nothing (to avoid rendering unnecessary UI elements) 
  output$second_search <- renderUI({
    if (is.null(input$selected_variables) || length(input$selected_variables) == 0) {
      return(NULL)
    }
    
    lapply(input$selected_variables, function(element) {
      
      # slider input for numeric variables
      if (is.numeric(steam_games[[element]])) {
        sliderInput(
          inputId = paste0("chosen_", element),
          label = paste("Select range for", element, ":"),
          min = min(steam_games[[element]], na.rm = TRUE),
          max = max(steam_games[[element]], na.rm = TRUE),
          value = range(steam_games[[element]], na.rm = TRUE)
        )
      } else {
        
        # text input for non-numeric variables
        textInput(
          inputId = paste0("chosen_", element),
          label = paste("Search in", element, ":"),
          value = ""
        )
      }
    })
  })
  
  # filter data based on selected variables and their corresponding inputs 
  filtered_data <- reactive({
    data <- steam_games
    
    if (!is.null(input$selected_variables)) {
      for (element in input$selected_variables) {
        
        #create dynamically ID based on input in frist search bar
        chosen_element <- paste0("chosen_", element)
        
        # if the input of the first search bar is numeric then show the silder and given output based on the range in the silder
        if (is.numeric(steam_games[[element]])) {
          range <- input[[chosen_element]]
          if (!is.null(range)) {
            data <- data %>% filter(.data[[element]] >= range[1], .data[[element]] <= range[2])
          }
        } else {
          
          # If the input of the frist search bar is not numeric, show a new search bar, and according to the input of the second search bar, filter output
          input_text <- input[[chosen_element]]
          if (!is.null(input_text) && input_text != "") {
            data <- data %>% filter(grepl(input_text, .data[[element]], ignore.case = TRUE))
          }
        }
      }
    }
    # show the output
    data
  })
  
  # Feature 1: Dynamic Variable Selection and Filtering
  # This feature allows users to select variables and apply filters dynamically,
  # enabling customized exploration of the Steam games dataset.
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data() %>%
        
        #show id, name, and selected variables
        select(c(id, name), all_of(input$selected_variables)),
      options = list(
        
        #show up 10 lines in a page
        pageLength = 10,
        
        #automatically control the width of column
        autoWidth = TRUE,
        
        #allow horizontal scrolling
        scrollX = TRUE
      ),
      
      #remove row name
      rownames = FALSE
    )
  })
  
  #Setup the download information
  output$download_table <- downloadHandler(
    filename = function() {
      paste("steam_games_table", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  #Feature 2: Download button.
  #This feature allows users to download the result dataset as a CSV file for offline analysis.
  #If there are no selected variables, the download file would be the original steam games dataset.
  
  #Page 2
  
  #change current graph based on button clicks
  observeEvent(input$hist_discount_price, {
    current_graph("discount_price")
  })
  
  observeEvent(input$hist_original_price, {
    current_graph("original_price")
  })
  
  observeEvent(input$hist_achievements, {
    current_graph("achievements")
  })
  
  #change the graph title when click corresponding button
  output$current_graph_title <- renderText({
    switch(
      current_graph(),
      "discount_price" = "Histogram: Discount Price",
      "original_price" = "Histogram: Original Price",
      "achievements" = "Histogram: Achievements"
    )
  })
  
  #Detail of distribution graphs 
  output$steam_games_graph <- renderPlot({
    range_filter <- input$range
    data <- steam_games %>%
      #filter the range of discount price based on the input of slider
      filter(discount_price >= range_filter[1], discount_price <= range_filter[2])
    
    if (current_graph() == "discount_price") {
      ggplot(data, aes(x = discount_price)) +
        geom_histogram(aes(y = after_stat(density)), binwidth = 1, color = "red", na.rm = TRUE) +
        labs(title = "Discount Price Distribution", x = "Discount Price", y = "Density") +
        theme_minimal()
    } else if (current_graph() == "original_price") {
      ggplot(data, aes(x = original_price)) +
        geom_histogram(aes(y = after_stat(density)), binwidth = 1, color = "blue", na.rm = TRUE) +
        labs(title = "Original Price Distribution", x = "Original Price", y = "Density") +
        theme_minimal()
    } else if (current_graph() == "achievements") {
      ggplot(data, aes(x = achievements)) +
        geom_histogram( aes(y = after_stat(density)), binwidth = 1, color = "black", na.rm = TRUE)+
        labs(title = "Achievements Distribution", x = "Achievements", y = "Distribution") +
        theme_minimal()
    }
  })  
  
  # Feature 3: Interactive Histograms with Adjustable Range.
  # This feature provides interactive histograms for numerical variables,
  # allowing users to adjust the discount price range and explore data distributions.
  
  
  # Logical of swich page
  observeEvent(input$go_to_page2, {
    current_page("page2") 
  })
  
  observeEvent(input$go_to_page1, {
    current_page("page1") 
  })
}

# Feature 4: Page Navigation Between Table and Graph Views.
# This feature enables users to switch between the data table and graph pages,
# enhancing the user experience by providing easy navigation within the app.



#run this shiny app
shinyApp(ui = ui, server = server)