# Install once if needed
# install.packages(c("tidyverse", "lubridate", "plotly", "DT", "shiny", "shinydashboard"))

library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(shiny)
library(shinydashboard)

# Read your dataset
data <- read_csv("C:/Users/DEVADHARSHINI/Downloads/generated_data.csv")

# If your file name is dev-dataset.csv, then use:
# data <- read_csv("C:/Users/DEVADHARSHINI/Downloads/dev-dataset.csv")
data <- data %>%
  mutate(
    review_date = ymd_hms(review_date),
    year_month  = floor_date(review_date, "month")
  )
# 1. Overall average rating: Amazon vs Flipkart
avg_rating <- data %>%
  summarise(
    amazon  = mean(amazon_rating, na.rm = TRUE),
    flipkart = mean(flipkart_rating, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "platform", values_to = "avg_rating")

p1 <- ggplot(avg_rating, aes(x = platform, y = avg_rating, fill = platform)) +
  geom_col() +
  labs(title = "Average Rating: Amazon vs Flipkart",
       x = "Platform", y = "Average Rating") +
  theme_minimal()

ggplotly(p1)
# 2. Category-wise comparison
cat_rating <- data %>%
  group_by(category) %>%
  summarise(
    amazon  = mean(amazon_rating, na.rm = TRUE),
    flipkart = mean(flipkart_rating, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(amazon, flipkart),
               names_to = "platform",
               values_to = "avg_rating")

p2 <- ggplot(cat_rating,
             aes(x = category, y = avg_rating, fill = platform)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Category-wise Average Rating",
       x = "Category", y = "Average Rating") +
  theme_minimal()

ggplotly(p2)
# 3. Trend over time (monthly)
time_rating <- data %>%
  group_by(year_month) %>%
  summarise(
    amazon   = mean(amazon_rating, na.rm = TRUE),
    flipkart = mean(flipkart_rating, na.rm = TRUE)
  )

p3 <- plot_ly(time_rating, x = ~year_month) %>%
  add_lines(y = ~amazon, name = "Amazon") %>%
  add_lines(y = ~flipkart, name = "Flipkart") %>%
  layout(title = "Monthly Rating Trend",
         xaxis = list(title = "Month"),
         yaxis = list(title = "Average Rating"))
p3
ui <- dashboardPage(
  dashboardHeader(title = "Amazon vs Flipkart Comparator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-column")),
      menuItem("By Category", tabName = "category", icon = icon("list")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          box(width = 6, plotlyOutput("overall_plot")),
          box(width = 6, plotlyOutput("trend_plot"))
        )
      ),
      tabItem(
        tabName = "category",
        fluidRow(
          box(
            width = 12,
            selectInput("cat_select", "Select Category:",
                        choices = sort(unique(data$category)),
                        selected = unique(data$category)[1]),
            plotlyOutput("category_plot")
          )
        )
      ),
      tabItem(
        tabName = "table",
        fluidRow(
          box(width = 12, DTOutput("data_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$overall_plot <- renderPlotly({
    avg_rating <- data %>%
      summarise(
        amazon   = mean(amazon_rating, na.rm = TRUE),
        flipkart = mean(flipkart_rating, na.rm = TRUE)
      ) %>%
      pivot_longer(everything(),
                   names_to = "platform",
                   values_to = "avg_rating")
    
    p <- ggplot(avg_rating,
                aes(x = platform, y = avg_rating, fill = platform)) +
      geom_col() +
      labs(title = "Average Rating: Amazon vs Flipkart",
           x = "Platform", y = "Average Rating") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$trend_plot <- renderPlotly({
    time_rating <- data %>%
      group_by(year_month) %>%
      summarise(
        amazon   = mean(amazon_rating, na.rm = TRUE),
        flipkart = mean(flipkart_rating, na.rm = TRUE)
      )
    
    plot_ly(time_rating, x = ~year_month) %>%
      add_lines(y = ~amazon, name = "Amazon") %>%
      add_lines(y = ~flipkart, name = "Flipkart") %>%
      layout(title = "Monthly Rating Trend",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Average Rating"))
  })
  
  output$category_plot <- renderPlotly({
    cat_rating <- data %>%
      filter(category == input$cat_select) %>%
      summarise(
        amazon   = mean(amazon_rating, na.rm = TRUE),
        flipkart = mean(flipkart_rating, na.rm = TRUE)
      ) %>%
      pivot_longer(everything(),
                   names_to = "platform",
                   values_to = "avg_rating")
    
    p <- ggplot(cat_rating,
                aes(x = platform, y = avg_rating, fill = platform)) +
      geom_col() +
      labs(title = paste("Average Rating in", input$cat_select),
           x = "Platform", y = "Average Rating") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$data_table <- renderDT({
    datatable(
      data %>%
        select(product_id, product_name, category,
               review_date, flipkart_rating, amazon_rating,
               helpful_votes, verified_purchase),
      options = list(pageLength = 10),
      filter = "top"
    )
  })
}

shinyApp(ui, server)