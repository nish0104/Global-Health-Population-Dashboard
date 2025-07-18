# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(treemap)
library(dplyr)
library(RColorBrewer)
library(reshape2)

# Load dataset
data <- read.csv("Cleaned_HEALTH_NUTRITION_Data.csv")

# Rename columns for ease of use
colnames(data) <- c("Series_Name", "Series_Code", "Country_Name", "Country_Code", 
                    "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014", "Y2015", 
                    "Y2016", "Y2017", "Y2018", "Y2019", "Y2020", "Y2021", "Y2022", "Y2023")

# Define selected countries
selected_countries <- c("East Asia & Pacific", "South Asia", "India", "China", 
                        "OECD members", "Sub-Saharan Africa",
                        "Europe & Central Asia", "Africa Eastern and Southern", 
                        "Latin America & Caribbean", "Africa Western and Central")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Age Dependency Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top 10 Countries", tabName = "bar_chart", icon = icon("chart-bar")),
      menuItem("Trend Over Time", tabName = "line_chart", icon = icon("chart-line")),
      menuItem("Scatter Plot", tabName = "scatter", icon = icon("dot-circle")),
      menuItem("Proportional Representation", tabName = "treemap", icon = icon("th")),
      selectInput(inputId = "color_palette", 
                  label = "Select Color Palette", 
                  choices = c("Reds", "Blues", "Greens", "Purples", "Oranges", 
                              "Set1", "Set2", "Set3", "Pastel1", "Pastel2", 
                              "Dark2", "Accent", "Spectral", "Viridis", "Magma", 
                              "Plasma", "Cividis", "Inferno", "Turbo"),
                  selected = "Set1")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "bar_chart",
              fluidRow(
                box(plotlyOutput("bar_chart", height = "500px"), width = 12)
              )
      ),
      tabItem(tabName = "line_chart",
              fluidRow(
                box(plotlyOutput("line_chart", height = "500px"), width = 12)
              )
      ),
      tabItem(tabName = "scatter",
              fluidRow(
                box(plotlyOutput("scatter_plot", height = "500px"), width = 12)
              )
      ),
      tabItem(tabName = "treemap",
              fluidRow(
                box(plotOutput("treemap"), width = 12)
              )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Filter dataset
  filtered_data <- reactive({
    data %>%
      filter(Country_Name %in% selected_countries)
  })
  
  # Generate colors
  get_colors <- reactive({
    color_count <- length(selected_countries)  
    colorRampPalette(brewer.pal(9, input$color_palette))(color_count)  
  })
  
  
  # Which countries have the highest and lowest age dependency ratios?
  # Bar Chart
  output$bar_chart <- renderPlotly({
    colors <- get_colors()
    
    p <- ggplot(filtered_data(), aes(x = reorder(Country_Name, Y2023), y = Y2023, fill = Country_Name)) +
      geom_bar(stat = "identity", width = 0.7) +
      coord_flip() +  
      scale_fill_manual(values = colors) +  
      labs(title = "Top 10 Countries by Age Dependency Ratio (2023)",
           x = "", y = "Age Dependency Ratio (2023)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),  
        axis.title.y = element_text(size = 14)
      )
    
    ggplotly(p)
  })
  
  
  # How has the dependency ratio changed over time?
  # Line Chart
  output$line_chart <- renderPlotly({
    colors <- c("East Asia & Pacific" = "blue", "South Asia" = "red")  
    
    filtered_data <- data %>%
      filter(Country_Name %in% c("East Asia & Pacific", "South Asia")) %>%
      select(Country_Name, Y2009:Y2023) %>%
      melt(id.vars = "Country_Name", variable.name = "Year", value.name = "Dependency_Ratio")
    
    filtered_data$Year <- as.numeric(gsub("Y", "", filtered_data$Year))
    
    p <- ggplot(filtered_data, aes(x = Year, y = Dependency_Ratio, 
                                   color = Country_Name, group = Country_Name)) +
      geom_line(size = 1.5, alpha = 0.7) +
      geom_point(size = 3, alpha = 0.8) +
      geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(2009, 2023, by = 2)) +
      labs(title = "Age Dependency Ratio Trend (2009-2023)",
           x = "Year", y = "Age Dependency Ratio") +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      )
    
    ggplotly(p)
  })
  
  
  # How has the dependency ratio changed over time?
  # Scatter Plot
  output$scatter_plot <- renderPlotly({
    colors <- get_colors()
    
    p <- ggplot(filtered_data(), aes(x = Y2009, y = Y2023, text = Country_Name)) +
      geom_point(aes(color = Country_Name), size = 3, alpha = 0.6, 
                 position = position_jitter(width = 0.1, height = 0.1)) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_color_manual(values = colors) +
      scale_x_log10() + scale_y_log10() +
      labs(title = "Relationship Between 2009 and 2023",
           x = "2009 Age Dependency (log)", y = "2023 Age Dependency (log)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16)  
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # How does each region compare in terms of youth dependency?
  # Treemap
  output$treemap <- renderPlot({
    colors <- get_colors()
    
    treemap(filtered_data(), 
            index = "Country_Name", 
            vSize = "Y2023", 
            vColor = "Country_Name", 
            type = "categorical",
            palette = colors,  
            title = "Proportional Representation of Age Dependency Ratios (2023)",
            fontsize.title = 20)  
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
