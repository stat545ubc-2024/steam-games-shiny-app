library(shiny)
library(shinyWidgets)
library(datateachr)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(httr)
library(jsonlite)

# Install the package which contains the dataset.
devtools::install_github("UBC-MDS/datateachr")

# ---- Variable Descriptions ----
variable_descriptions <- list(
  url = "url site at which the game is located",
  types = "Type of package - app, sub, bundle",
  name = "Game name",
  desc_snippet = "Game description snippet",
  recent_reviews = "Recent reviews",
  all_reviews = "All reviews",
  release_date = "Release date",
  developer = "Game developer",
  publisher = "Game publisher",
  popular_tags = "Game tags",
  game_details = "Game details",
  languages = "Languages the game is available in",
  achievements = "Achievement number",
  genre = "Game genre",
  game_description = "Game description",
  mature_content = "Description of mature content",
  minimum_requirements = "Minimum specifications/requirements to play the game",
  recommended_requirements = "Recommended specifications/requirements to play the game",
  original_price = "Regular price",
  discount_price = "Sale price with discount"
)

# ---- End ----

# ---- Defines supported currencies and their symbols ----
currencies_choices <- c(
  "US Dollar (USD) $" = "USD",
  "Euro (EUR) €" = "EUR",
  "Great Britain Pound (GBP) £" = "GBP",
  "Japanese Yen (JPY) ¥" = "JPY",
  "China Yuan (CNY) ¥" = "CNY"
)

currency_symbols <- c(
  "USD" = "$",
  "EUR" = "€",
  "GBP" = "£",
  "JPY" = "¥",
  "CNY" = "¥"
)
# ---- End ----


# ----Information of page1 (table) ----
page1_ui <- fluidPage(
  theme = shinytheme("cerulean"),  # select the Sky Blue Theme
  
  align = "center",  #center the column headers
  
  titlePanel("Steam Games Explorer"), #add title of page 1
  
  sidebarLayout(
    sidebarPanel(
      helpText("You can select one or more words below to see the detail information."),
      
      #select base data set
      selectInput(
        inputId = "selected_variables",
        label = "Select Vector(s) to Display:",
        choices = setNames(
          setdiff(names(variable_descriptions), c("name", "id")),
          paste0(setdiff(names(variable_descriptions), c("name", "id")), ": ", variable_descriptions[setdiff(names(variable_descriptions), c("name", "id"))])
        ),
        selected = NULL,
        multiple = TRUE
      ),
      
      uiOutput("second_search"), # name of the output of second search bar
      
      #select currency unit
      selectInput(inputId = "currency",
                  label = "Select currency unit",
                  choices = currencies_choices,
                  selected = "USD",
                    ),
      
      actionButton("reset", "Reset"), #set the reset button
      
      downloadButton("download_table", "Download Table"), # set the download button
      
    ),
    
    mainPanel(
      h3("Steam Games Table"), #header of table
      
      DT::dataTableOutput("data_table")  #name of display data table
    )
  )
)
# ---- End ----


# ----Information of page2 (Graph)----
page2_ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
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
        value = c(min(steam_games$discount_price, na.rm = TRUE), max(steam_games$discount_price, na.rm = TRUE)), 
        step = 1
      ),
      
      
      actionButton("hist_discount_price", "Discount Price"), # set a button for distribution of discount price
      
      
      actionButton("hist_original_price", "Original Price"), # set a button for distribution of original price
      
      
      actionButton("hist_achievements", "Achievements"), # set a button for distribution of achievements
      
      downloadButton("download_plot", "Down load the current plot"),
    ),
    
      
    
    mainPanel(
      #dynamic heading for the graph
      h3(textOutput("current_graph_title")),
      
      #name of the output of graph
      plotOutput("steam_games_graph") 
    )
  )
)

# ---- End ----

# ---- Information of page3 (help) ----

page3_ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Help"),
  mainPanel(
    h3("Steam Games Explorer Help"),
    p("Welcome to the Steam Games Explorer! This Shiny app allows you to explore and analyze Steam games data for educational purpose."),
    
    h4("Page 1: Data Table"),
    p("On the Data Table page, you can select variables to display, apply filters, and explore the dataset in a table format. You can also convert prices into different currencies using real-time exchange rates and download the filtered data."),
    
    h4("Page 2: Data Graph"),
    p("On the Data Graph page, you can view interactive histograms of various numerical variables with respected to  discount price range, and download the current plot."),
    
    h4("Features"),
    tags$ul(
      tags$li("Dynamic variable selection and filtering"),
      tags$li("Real-time currency conversion with up-to-date exchange rates"),
      tags$li("Clickable game titles that link directly to their Steam pages"),
      tags$li("Ability to download filtered data and plots for offline analysis")
    ),
    
    h4("How to Use"),
    p("Use the tabs at the top to navigate between the Data Table and Data Graph pages. On each page, use the controls in the sidebar to interact with the data. The 'Reset' button clears your selections, and the 'Download' buttons allow you to save the current data or plot.")
  )
)


# New feature number 5: create a help page to describe the usage of this shiny app.
# Help users quickly understand the usage and features of this shiny app.

# ---- End ----


# ----main ui; switch between page1 and page2 ----
ui <- navbarPage(
    "Steam Games Explorer",
    tabPanel("Data Table", page1_ui),
    tabPanel("Data Graph", page2_ui),
    tabPanel("Help", page3_ui)
  )
# ---- End ----



#define the server logic
server <- function(input, output, session) {
  observe({print(paste("Selected currency:", input$currency))}) # be able to provide the selected currency code for test
  
  observe({session$sendCustomMessage(type = 'addDescriptions', message = variable_descriptions)}) #Add discretion of variable to the secondary searching bar
  # New feature number 1: In the secondary search the explanation of the selected variable appears. (Also add this description near each variable)
  # Give the user a brief introduction to the selected variable.
  # Help users understand the meaning and detailed information of the new columns.
  
  
  #Page 1
  #clear the selected variable when click the reset button
  observeEvent(input$reset, {
    updateSelectInput(session, "selected_variables", selected = character(0))  
    updateSelectInput(session, "currency", selected = "USD")
  })
  
  #----Feature 1: Dynamic Variable Selection and Filtering----
  # If there is no input in the second (or more) search bar/slider, return nothing (to avoid rendering unnecessary UI elements) 
  output$second_search <- renderUI({
    if (is.null(input$selected_variables) || length(input$selected_variables) == 0) {
      return(NULL)
    }
    
    rate <- exchange_rate()  # Get the current exchange rate
    currency_symbol <- currency_symbols[[input$currency]]  # Get the current currency symbol
    
    lapply(input$selected_variables, function(element) {
      if (element %in% c("discount_price", "original_price")) {
        sliderInput(
          inputId = paste0("chosen_", element),
          label = paste("Select range for", variable_descriptions[[element]], "(", currency_symbol, "):"),
          min = round(min(steam_games[[element]] * 1/rate, na.rm = TRUE), 2),
          max = round(max(steam_games[[element]] * 1/rate, na.rm = TRUE), 2),
          value = range(round(steam_games[[element]] * rate, 2), na.rm = TRUE)
        )
      } else if (is.numeric(steam_games[[element]])) {
        sliderInput(
          inputId = paste0("chosen_", element),
          label = paste("Select range for", variable_descriptions[[element]], ":"),
          min = min(steam_games[[element]], na.rm = TRUE),
          max = max(steam_games[[element]], na.rm = TRUE),
          value = range(steam_games[[element]], na.rm = TRUE)
        )
      } else {
        textInput(
          inputId = paste0("chosen_", element),
          label = paste("Search in", variable_descriptions[[element]], ":"),
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
        
        #create dynamically ID based on input in first search bar
        chosen_element <- paste0("chosen_", element)
        
        # if the input of the first search bar is numeric then show the silder and given output based on the range in the silder
        if (is.numeric(steam_games[[element]])) {
          range <- input[[chosen_element]]
          if (!is.null(range)) {
            data <- data %>% filter(.data[[element]] >= range[1], .data[[element]] <= range[2])
          }
        } else {
          
          # If the input of the first search bar is not numeric, show a new search bar, and according to the input of the second search bar, filter output
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
  # ---- End ----
  
  # ---- new Feature number 2: be able to use Real-time exchange rate----
  api_key <- "ce94c805bf11494ba0d1f0b7" # the API Key from ExchangeRate-API
  
  exchange_rate <- reactiveVal(1) #default the initial currency (USD)
  
  observeEvent(input$currency, {
    req(input$currency) #ensure input$currency is not Null
    if (input$currency == "USD") {
      exchange_rate(1)   # if the input currency is USD, use the original currency
    } else {
      
      base_currency <- "USD" #default the base currency to be USD
      target_currency <- input$currency  #define the user's input currency to be target currency
      
      api_url <- paste0("https://v6.exchangerate-api.com/v6/", api_key, "/latest/", base_currency) # API required URL
      
  
      res <- GET(api_url)
      if (status_code(res) == 200) { #see if JSON works
        
        res_content <- content(res, as = "text", encoding = "UTF-8") #take content from res
        res_json <- fromJSON(res_content) #change the format of JSON into R
        
        #check the states
        if (res_json$result == "success") {
          rate <- res_json$conversion_rates[[target_currency]]  #take the rate of target currency from JSON and named as rate
          if (!is.null(rate)) {
            exchange_rate(rate) #if rate is not empty, update the rate 
          } else {
            exchange_rate(1) # if rate is empty, use the original one, and display a notification
            showNotification("Unable to get rate for target currency，default USD.", type = "error")
          }
        } else {
          exchange_rate(1) # if JASON result fail, again use the rate at 1 and display a notification
          showNotification("API require fail，default USD.", type = "error")
        }
      } else {
        exchange_rate(1) # If the API request fails, keep the exchange rate at 1 and display a notification
        showNotification("Unable to obtain real-time exchange rate, default USD.", type = "error")
      }
    }
  })
  
  
  
  
  output$data_table <- DT::renderDataTable({
    req(filtered_data())
    data <- filtered_data() %>%
      
      select(c(id, name, url), all_of(input$selected_variables)) #show id, name, url and selected variables
    
    # If currency conversion is selected, convert the price
    if ("discount_price" %in% input$selected_variables || "original_price" %in% input$selected_variables) {
      rate <- exchange_rate()
      currency_symbol <- currency_symbols[[input$currency]]
      
      if ("discount_price" %in% input$selected_variables) {
        data <- data %>%
          mutate(discount_price = paste0(round(discount_price * rate, 2)))
      }
      
      if ("original_price" %in% input$selected_variables) {
        data <- data %>%
          mutate(original_price = paste0(round(original_price * rate, 2)))
      }
    }
    # New Feature number 2: be able to use Real-time exchange rate.
    # Allow the user to change the currency with a real-time exchange rate.
    # Since the exchange rate is calculated in real-time, users do not need to search for the exchange rate and calculate it online.
    
    # Renders the game title as a clickable link by using the url in the table
    data <- data %>%
      mutate(name = ifelse(!is.na(url) & url != "",
                           paste0('<a href="', url, '" target="_blank">', name, '</a>'),
                           name)) %>%
      select(-url) # delete the url column 
    
    # New Feature number 3: Connect the url with each game
    # Allow users to directly enter the game's corresponding steam page by clicking on the game name.
    # Provides convenience for users. After seeing the game they like in the table, they can directly enter the game homepage without having to copy and paste the URL.
    
    DT::datatable(
      data,
      escape = FALSE, # Rendering Hyperlinks
      options = list(
        pageLength = 10, #show up 10 lines in a page
        autoWidth = TRUE, #automatically control the width of column
        scrollX = TRUE #allow horizontal scrolling
      ),
      rownames = FALSE #remove row name
    )
  })
    
  
  # ---- End ----
  
  
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
  
  # ---- End ----
  #Page 2
  
  
  current_graph <- reactiveVal("discount_price") #Defines the reactive value of the current chart type
  
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
  steam_games_graph <- reactive({
    range_filter <- input$range
    data <- steam_games %>%
      filter(discount_price >= range_filter[1], discount_price <= range_filter[2])
    
    # According to the current type draw distribution graph
    plot <- switch(
      current_graph(),
      "discount_price" = {
        ggplot(data, aes(x = discount_price)) +
          geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "red", color = "black", na.rm = TRUE) +
          labs(title = "Discount price distribution", x = "discount price ($)", y = "density") +
          theme_minimal()
      },
      "original_price" = {
        ggplot(data, aes(x = original_price)) +
          geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "blue", color = "black", na.rm = TRUE) +
          labs(title = "Original Price Distribution", x = "original price ($)", y = "density") +
          theme_minimal()
      },
      "achievements" = {
        ggplot(data, aes(x = achievements)) +
          geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "green", color = "black", na.rm = TRUE) +
          labs(title = "Achievements Distribution", x = "achievement number", y = "density") +
          theme_minimal()
      }
    )
    
    print(plot)
  })
  
  output$steam_games_graph <- renderPlot({
    steam_games_graph()
  })
  
  
  # Feature 3: Interactive Histograms with Adjustable Range.
  # This feature provides interactive histograms for numerical variables,
  # allowing users to adjust the discount price range and explore data distributions.
  
  
  # be able to download graph
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("steam_games_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      print(steam_games_graph())
      dev.off()
    }
  )
  
  
  # New feature number 4: be able to download the current graph.
  # Allow the user to download the current graph with the selected range.
  # Helps users study selected data offline.
  
  # Logical of switch page
  observeEvent(input$go_to_page1, {
    updateNavbarPage(session, "Steam Games Explorer", selected = "Data Table")
  })
}

# Feature 4: Page Navigation Between Table and Graph Views.
# This feature enables users to switch between the data table and graph pages,
# enhancing the user experience by providing easy navigation within the app.



#run this shiny app
shinyApp(ui = ui, server = server)