library(shiny)
library(shinythemes)
library(ggplot2)
library(glue)
library(dplyr)
library(tidyr)
library(caret)

# Define function to load CSV data
load_data <- function(csv_choice) {
  
  # Read in data
  if (csv_choice == "weapons") {
    data <- read.csv("weapons.csv")
  } else if (csv_choice == "bases") {
    data <- read.csv("bases.csv")
  } else if (csv_choice == "charms") {
    data <- read.csv("charms.csv")
  } else {
    data <- NULL
  }
  
  # Define custom order for titles and cheese_effect
  title_order <- c("Novice", "Recruit", "Apprentice", "Initiate", "Journeyman/Journeywoman", "Master", "Grandmaster", "Legendary", "Hero", "Knight", "Lord/Lady", "Baron/Baroness", "Count/Countess", "Duke/Duchess", "Grand Duke/Grand Duchess", "Archduke/Archduchess", "Viceroy", "Elder", "Sage", "Fabled")
  freshness_order <- c("Uber Stale", "Ultimately Stale", "Insanely Stale", "Extremely Stale", "Very Stale", "Stale", "No Effect", "Fresh", "Very Fresh", "Extremely Fresh", "Insanely Fresh", "Ultimately Fresh", "Uber Fresh")
  
  cheese_effect_order_numeric <- match(data$cheese_effect, freshness_order)
  data$cheese_effect_order_numeric <- cheese_effect_order_numeric
  
  if (csv_choice == "weapons" | csv_choice == "bases") {
    # Convert custom order to numeric order
    title_order_numeric <- match(data$title_req, title_order)
    # Add the numeric orders as columns to the data frame
    data$title_order_numeric <- title_order_numeric
  }
  
  return(data)
}

# Initialise choices for data variables based on `.csv` file selected for analysis
data_vars <- function(csv_choice) {
  
  # Initialise variables
  num_vars <- NULL
  cat_vars <- NULL
  
  # Read in data
  if (csv_choice == "weapons") {
    
    num_vars <- c("power", "power_percent_bonus", "attraction_percent_bonus", "luck", "cheese_effect_order_numeric", "title_order_numeric",	"gold_cost")
    cat_vars <- c("type", "obtain_method")
    
  } else if (csv_choice == "bases") {
    
    num_vars <- c("power", "power_percent_bonus", "attraction_percent_bonus", "luck", "cheese_effect_order_numeric", "title_order_numeric",	"gold_cost")
    cat_vars <- c("obtain_method")
    
  } else if (csv_choice == "charms") {
    
    num_vars <- c("power", "power_percent_bonus", "attraction_percent_bonus", "luck", "cheese_effect_order_numeric")
    cat_vars <- c("consumption_upon")
    
  }
  
  # Return variables
  return(list(num_vars = num_vars, cat_vars = cat_vars))
}

# Define UI
ui <- navbarPage(title = div(
                    style = "font-family: Gill Sans, sans-serif; font-size: 20px; font-weight: bold; color: #FFC300; margin-top: -10px;",
                    HTML("MouseHunt Trap  Statistics <br/> & Machine Learning")
                  ),
                 theme = shinytheme("united"),
                 tabPanel("Regression Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              tags$h2("Linear Regression Analysis"),
                              br(),
                              tags$h4("Select specific trap data"),
                              selectInput("csv_choice_1", markdown("Select `.csv` file data:"), choices = c("weapons", "bases", "charms")),
                              tags$h4("Select variables"),
                              selectInput("x", "X-axis variable", choices = NULL),
                              selectInput("y", "Y-axis variable", choices = NULL),
                              br(),
                              tags$h4("What each variable represents:"),
                              br(),
                              markdown("- `power`: The trap power in numerical form. (integer)"),
                              markdown("- `power_percent_bonus`: The trap power bonus in numerical form. (in percentage)"),
                              markdown("- `attraction_percent_bonus`: The trap attraction bonus in numerical form. (in percentage)"),
                              markdown("- `luck`: The trap luck (integer)"),
                              markdown("- `title_order_numeric`: The Hunter's label-encoded minimum title required to purchase and use the trap. In this case, 1 = Novice, 2 = Recruit ... 15 = Grand Duke/Grand Duchess, 16 = Archduke/Archduchess, 17 = Viceroy, etc. (integer)"),
                              markdown("- `gold_cost`: The amount of gold required (not economic cos) to purchase the trap. (integer)"),
                              markdown("- `cheese_effect`: The cheese effect (fresh/stale rate of bait) of the trap. In this case, 1 = Uber Stale, 2 = Ultimately Stale ... 7 = No Effect ... 12 = Ultimately Fresh, 13 = Uber Fresh etc. (integer)")
                            ),
                            mainPanel(
                              tags$h2("Data Scatterplot"),
                              plotOutput("plot"),
                              tags$h2("Linear Regression Summary"),
                              verbatimTextOutput("summary")
                            )
                          )
                 ),
                 tabPanel("Categorical Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              tags$h2("Categorical Exploratory Data Analysis (EDA)"),
                              br(),
                              tags$h4("Select specific trap data"),
                              selectInput("csv_choice_2", markdown("Select `.csv` file data:"), choices = c("weapons", "bases", "charms")),
                              tags$h4("Select categorical variable to view comparison between different numerical (or ordinal) variables"),
                              br(),
                              selectInput("category_var", "Categorical Variable", choices = NULL),
                              selectInput("numeric_var", "Numerical Variable", choices = NULL),                              
                              br(),
                              tags$h4("What each categorical variable represents:"),
                              br(),
                              markdown(
                                paste(
                                  "- `type`: The power type of the trap (10 such types)\n",
                                  "- `obtain_method`: The method of obtaining each trap.\n",
                                  "  - *Purchase*: Can be purchased with gold. (with or wihthout in-game resources)\n",
                                  "  - *Crafted*: Crafted using materials obtained in-game only.\n",
                                  "  - *LE*: Limited Edition, may not be available to players after a certain amount of time.\n",
                                  "- `consumption_upon`: How each charm is consumed *(for charms only)*\n"
                                )
                              ),
                              br()
                            ),
                            mainPanel(
                              tags$h2("Box-and-Whisker Plot Comparison by category"),
                              plotOutput("boxplot"),
                              tags$h2("Summary Statistics by Category"),
                              verbatimTextOutput("summary2")
                              )
                          )
                 ),
                 tabPanel("Data",
                          selectInput("csv_choice_3", markdown("Select `.csv` file data:"), choices = c("weapons", "bases", "charms")),
                          dataTableOutput("data_table")
                 )
)

# Define server functions
server <- function(input, output, session) {
  
  ### DATA PREPROCESSING FOR REACTIVE FUNCTIONS ###
  
  # Load CSV file (Reactive)
  mydata <- reactive({
    load_data(input$csv_choice_1)
  })
  mydata2 <- reactive({
    load_data(input$csv_choice_2)
  })
  mydataframe <- reactive({
    load_data(input$csv_choice_3)
  })
  
  # Load variable selection (Reactive)
  mydatavars <- reactive({
    data_vars(input$csv_choice_1)
  })
  mydatavars2 <- reactive({
    data_vars(input$csv_choice_2)
  })
  
  # Update choices for variables based on selected csv choice
  observeEvent(mydatavars(), {
    # For x and y variables in linear regression section
    updateSelectInput(session, "x", choices = mydatavars()$num_vars)
    updateSelectInput(session, "y", choices = mydatavars()$num_vars, selected = mydatavars()$num_vars[2])
  })
  
  observeEvent(mydatavars2(), {
    # For categorical and numerical variables in EDA section
    updateSelectInput(session, "category_var", choices = mydatavars2()$cat_vars)
    updateSelectInput(session, "numeric_var", choices = mydatavars2()$num_vars)
  })
  
  ### 1. LINEAR REGRESSION ###
  
  output$plot <- renderPlot({
    ggplot(mydata(), aes_string(x = input$x, y = input$y)) + 
      geom_point() + 
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = glue("Association between {input$x} and {input$y}")) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),  
        axis.title.x = element_text(size = 14, vjust = -0.8),  
        axis.title.y = element_text(size = 14, vjust = 3),  
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),   
        axis.ticks.x = element_line(size = 0.5),  
        axis.ticks.y = element_line(size = 0.5)   
      ) +
      scale_x_continuous(n.breaks = 10) +
      scale_y_continuous(n.breaks = 10)
  })
  
  # Show summary of linear regression results
  output$summary <- renderPrint({
    model <- lm(paste(y = input$y , x = input$x, sep = "~"), data = mydata())
    summary(model)
  })
  
  ### 2. EXPLORATORY DATA ANALYSIS ###
  
  # Generate box and whisker plot
  output$boxplot <- renderPlot({
    # Create the box and whisker plot
    ggplot(mydata2(), aes_string(x = as.name(input$category_var), y = input$numeric_var)) +
      geom_boxplot() +
      labs(x = input$category_var, y = input$numeric_var) +
      ggtitle(glue("Comparison of distribution of {input$numeric_var} based on different trap {input$category_var}")) +
      coord_cartesian(ylim = range(mydata2()[[input$numeric_var]])) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),  
        axis.title.x = element_text(size = 14, vjust = -0.8),  
        axis.title.y = element_text(size = 14, vjust = 3),  
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
      ) +
      scale_y_continuous(n.breaks = 10)
  })
  
  # Calculate summary statistics
  output$summary2 <- renderPrint({
    # Group data by selected category and calculate summary statistics for each category
    summary_stats <- mydata2() %>%
      group_by(across(all_of(input$category_var))) %>%
      summarise(
        Count = n(),
        Mean = mean(get(input$numeric_var)),
        Median = median(get(input$numeric_var)),
        SD = sd(get(input$numeric_var)),
        Min = min(get(input$numeric_var)),
        Max = max(get(input$numeric_var))
      )
    
    summary_stats
  })
  
  ### 3. ALL DATA ###
  
  # Display data table
  output$data_table <- renderDataTable({
    mydataframe()
  })
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)