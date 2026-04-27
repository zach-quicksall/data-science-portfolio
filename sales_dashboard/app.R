# Global Superstore Sales Dashboard
# Packages ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(conflicted)
library(targets)
library(tarchetypes)
library(plotly)
library(DT)
library(scales)

conflicts_prefer(shinydashboard::box)
conflicts_prefer(plotly::layout)

# Data -------------------------------------------------------------------
tar_load(data_clean)
superstore <- data_clean

# UI ---------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Global Superstore Dashboard"),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Orders", tabName = "orders", icon = icon("table")),
      hr(),
      
      dateRangeInput(
        "date_range",
        "Order Date Range",
        start = min(superstore$order_date, na.rm = TRUE),
        end   = max(superstore$order_date, na.rm = TRUE)
      ),
      
      selectInput(
        "market",
        "Market",
        choices = c("All", sort(unique(superstore$market))),
        selected = "All",
        multiple = FALSE
      ),
      
      selectInput(
        "region",
        "Region",
        choices = c("All", sort(unique(superstore$region))),
        selected = "All",
        multiple = FALSE
      ),
      
      selectInput(
        "segment",
        "Segment",
        choices = c("All", sort(unique(superstore$segment))),
        selected = "All",
        multiple = FALSE
      ),
      
      selectInput(
        "category",
        "Category",
        choices = c("All", sort(unique(superstore$category))),
        selected = "All",
        multiple = FALSE
      ),
      
      uiOutput("subcat_ui")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box h3 { font-size: 28px; }
        .content-wrapper, .right-side { background-color: #f4f6f9; }
      "))
    ),
    
    tabItems(
      # -------------------------------------------------------------------
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("sales_box", width = 3),
          valueBoxOutput("profit_box", width = 3),
          valueBoxOutput("orders_box", width = 3),
          valueBoxOutput("margin_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Monthly Sales Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("sales_trend", height = 320)
          ),
          box(
            title = "Sales by Category",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("category_bar", height = 320)
          )
        ),
        
        fluidRow(
          box(
            title = "Profit by Sub-Category",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("subcategory_profit", height = 350)
          ),
          box(
            title = "Profit vs Discount",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("discount_scatter", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Sales by Country",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotOutput("country_sales", height = 400)
          )
        )
      ),
      
      # -------------------------------------------------------------------
      tabItem(
        tabName = "orders",
        
        fluidRow(
          box(
            title = "Order-Level Detail",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("orders_table")
          )
        )
      )
    )
  )
)

# Server -----------------------------------------------------------------
server <- function(input, output, session) {
  
  output$subcat_ui <- renderUI({
    df <- superstore
    
    if (input$category != "All") {
      df <- df %>% filter(category == input$category)
    }
    
    selectInput(
      "sub_category",
      "Sub-Category",
      choices = c("All", sort(unique(df$sub_category))),
      selected = "All",
      multiple = FALSE
    )
  })
  
  filtered_data <- reactive({
    df <- superstore %>%
      filter(order_date >= input$date_range[1],
             order_date <= input$date_range[2])
    
    if (input$market != "All") {
      df <- df %>% filter(market == input$market)
    }
    if (input$region != "All") {
      df <- df %>% filter(region == input$region)
    }
    if (input$segment != "All") {
      df <- df %>% filter(segment == input$segment)
    }
    if (input$category != "All") {
      df <- df %>% filter(category == input$category)
    }
    if (!is.null(input$sub_category) && input$sub_category != "All") {
      df <- df %>% filter(sub_category == input$sub_category)
    }
    
    df
  })
  
  # KPI boxes -------------------------------------------------------------
  output$sales_box <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = dollar(sum(df$sales, na.rm = TRUE)),
      subtitle = "Total Sales",
      icon = icon("dollar-sign"),
      color = "aqua"
    )
  })
  
  output$profit_box <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = dollar(sum(df$profit, na.rm = TRUE)),
      subtitle = "Total Profit",
      icon = icon("chart-line"),
      color = ifelse(sum(df$profit, na.rm = TRUE) >= 0, "green", "red")
    )
  })
  
  output$orders_box <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = comma(n_distinct(df$order_id)),
      subtitle = "Orders",
      icon = icon("shopping-cart"),
      color = "yellow"
    )
  })
  
  output$margin_box <- renderValueBox({
    df <- filtered_data()
    margin <- ifelse(sum(df$sales, na.rm = TRUE) > 0,
                     sum(df$profit, na.rm = TRUE) / sum(df$sales, na.rm = TRUE),
                     0)
    valueBox(
      value = percent(margin, accuracy = 0.1),
      subtitle = "Profit Margin",
      icon = icon("percent"),
      color = "purple"
    )
  })
  
  # Monthly sales trend ---------------------------------------------------
  output$sales_trend <- renderPlotly({
    df <- filtered_data() %>%
      group_by(month) %>%
      summarise(
        sales = sum(sales, na.rm = TRUE),
        profit = sum(profit, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(df, aes(x = month, y = sales)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Sales", title = NULL) +
      theme_minimal(base_size = 13)
    
    plotly::ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Category sales --------------------------------------------------------
  output$category_bar <- renderPlotly({
    df <- filtered_data() %>%
      group_by(category) %>%
      summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(sales))
    
    p <- ggplot(df, aes(x = reorder(category, sales), y = sales, text = paste0(
      "Category: ", category,
      "<br>Sales: ", dollar(sales)
    ))) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Sales") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # Sub-category profit ---------------------------------------------------
  output$subcategory_profit <- renderPlotly({
    df <- filtered_data() %>%
      group_by(sub_category) %>%
      summarise(profit = sum(profit, na.rm = TRUE), .groups = "drop") %>%
      arrange(profit)
    
    p <- ggplot(df, aes(x = reorder(sub_category, profit), y = profit,
                        text = paste0(
                          "Sub-Category: ", sub_category,
                          "<br>Profit: ", dollar(profit)
                        ))) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Profit") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # Profit vs Discount ----------------------------------------------------
output$discount_scatter <- renderPlotly({
  df <- filtered_data() %>%
    mutate(
      discount = as.numeric(discount),
      profit = as.numeric(profit),
      sales = as.numeric(sales),
      product_name = iconv(product_name, from = "", to = "UTF-8", sub = " ")
    ) %>%
    filter(!is.na(discount), !is.na(profit), !is.na(sales))

  p <- ggplot(
    df,
    aes(
      x = discount,
      y = profit,
      text = paste0(
        "Product: ", product_name,
        "<br>Discount: ", scales::percent(discount, accuracy = 0.1),
        "<br>Profit: ", scales::dollar(profit),
        "<br>Sales: ", scales::dollar(sales)
      )
    )
  ) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(x = "Discount", y = "Profit") +
    theme_minimal(base_size = 13)

  ggplotly(p, tooltip = "text")
})
  
  # Country sales ---------------------------------------------------------
output$country_sales <- renderPlot({

  df <- filtered_data() %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      sales = sum(sales, na.rm = TRUE),
      profit = sum(profit, na.rm = TRUE),
      orders = dplyr::n_distinct(order_id),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(sales)) %>%
    dplyr::slice_head(n = 15) %>%
    dplyr::arrange(sales) %>%
    dplyr::mutate(country = factor(country, levels = country))

  ggplot2::ggplot(df, ggplot2::aes(x = country, y = sales)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    ggplot2::labs(
      x = NULL,
      y = "Sales",
      title = "Top 15 Countries by Sales"
    ) +
    ggplot2::theme_minimal(base_size = 13)
})
  
  # Orders table ----------------------------------------------------------
  output$orders_table <- renderDT({
    df <- filtered_data() %>%
      select(
        order_id, order_date, ship_date, ship_mode,
        customer_name, segment, market, region, country, state, city,
        category, sub_category, product_name,
        sales, quantity, discount, profit, shipping_cost, order_priority
      )
    
    datatable(
      df,
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(c("sales", "profit", "shipping_cost")) %>%
      formatPercentage("discount", 1)
  })
}

# Run app ----------------------------------------------------------------
shinyApp(ui, server)