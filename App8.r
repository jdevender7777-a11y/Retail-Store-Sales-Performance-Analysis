
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# Load dataset
sales <- read.csv("SampleSuperstore.csv", check.names = FALSE)

# Standardize Postal Code for reliable filtering
sales$`Postal Code` <- as.character(sales$`Postal Code`)

# Remove missing values from filter choices
get_choices <- function(x) {
  sort(unique(as.character(na.omit(x))))
}

# ---------------- USER INTERFACE ----------------

ui <- fluidPage(
  
  titlePanel(
    h2("Retail Store Sales Performance Dashboard",
       style = "color:#6C3483; font-weight:bold;")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      h4("Data Filters"),
      
      selectizeInput(
        "country", "Country",
        choices = get_choices(sales$Country),
        multiple = TRUE,
        options = list(placeholder = "All Countries")
      ),
      
      selectizeInput(
        "region", "Region",
        choices = get_choices(sales$Region),
        multiple = TRUE,
        options = list(placeholder = "All Regions")
      ),
      
      selectizeInput(
        "state", "State",
        choices = get_choices(sales$State),
        multiple = TRUE,
        options = list(placeholder = "All States")
      ),
      
      selectizeInput(
        "city", "City",
        choices = get_choices(sales$City),
        multiple = TRUE,
        options = list(placeholder = "All Cities")
      ),
      
      selectizeInput(
        "postal", "Postal Code",
        choices = get_choices(sales$`Postal Code`),
        multiple = TRUE,
        options = list(
          placeholder = "All Postal Codes",
          maxOptions = 100
        )
      ),
      
      hr(),
      
      selectizeInput(
        "category", "Category",
        choices = get_choices(sales$Category),
        multiple = TRUE,
        options = list(placeholder = "All Categories")
      ),
      
      selectizeInput(
        "subcategory", "Sub-Category",
        choices = get_choices(sales$`Sub-Category`),
        multiple = TRUE,
        options = list(placeholder = "All Sub-Categories")
      ),
      
      selectizeInput(
        "segment", "Customer Segment",
        choices = get_choices(sales$Segment),
        multiple = TRUE,
        options = list(placeholder = "All Segments")
      ),
      
      selectizeInput(
        "shipmode", "Ship Mode",
        choices = get_choices(sales$`Ship Mode`),
        multiple = TRUE,
        options = list(placeholder = "All Ship Modes")
      ),
      
      actionButton(
        "reset", "Reset All Filters",
        class = "btn-primary"
      )
    ),
    
    mainPanel(
      width = 9,
      
      h4("Key Performance Indicators"),
      
      fluidRow(
        column(6, wellPanel(
          style = "background:#D6EAF8;",
          h4("Total Sales"),
          h3(textOutput("sales_kpi"))
        )),
        column(6, wellPanel(
          style = "background:#D5F5E3;",
          h4("Total Profit"),
          h3(textOutput("profit_kpi"))
        ))
      ),
      
      fluidRow(
        column(6, wellPanel(
          style = "background:#FCF3CF;",
          h4("Quantity Sold"),
          h3(textOutput("quantity_kpi"))
        )),
        column(6, wellPanel(
          style = "background:#F5CBA7;",
          h4("Average Discount"),
          h3(textOutput("discount_kpi"))
        ))
      ),
      
      h4("Sales and Profit Analysis"),
      
      fluidRow(
        column(6, plotOutput("category_plot", height = "300px")),
        column(6, plotOutput("subcategory_plot", height = "300px"))
      ),
      
      fluidRow(
        column(6, plotOutput("state_plot", height = "320px")),
        column(6, plotOutput("scatter_plot", height = "320px"))
      ),
      
      hr(),
      textOutput("record_count")
    )
  )
)

# ---------------- SERVER ----------------

server <- function(input, output, session) {
  
  # Helper: retain only selections that are still available
  update_filter <- function(id, choices, selected) {
    valid_selected <- intersect(selected, choices)
    
    updateSelectizeInput(
      session,
      inputId = id,
      choices = choices,
      selected = valid_selected,
      server = TRUE
    )
  }
  
  # Country -> Region
  observeEvent(input$country, {
    x <- sales
    
    if (length(input$country) > 0) {
      x <- filter(x, Country %in% input$country)
    }
    
    update_filter(
      "region",
      get_choices(x$Region),
      input$region
    )
  }, ignoreInit = FALSE)
  
  # Country + Region -> State
  observeEvent(
    list(input$country, input$region),
    {
      x <- sales
      
      if (length(input$country) > 0)
        x <- filter(x, Country %in% input$country)
      
      if (length(input$region) > 0)
        x <- filter(x, Region %in% input$region)
      
      update_filter(
        "state",
        get_choices(x$State),
        input$state
      )
    },
    ignoreInit = FALSE
  )
  
  # Country + Region + State -> City
  observeEvent(
    list(input$country, input$region, input$state),
    {
      x <- sales
      
      if (length(input$country) > 0)
        x <- filter(x, Country %in% input$country)
      
      if (length(input$region) > 0)
        x <- filter(x, Region %in% input$region)
      
      if (length(input$state) > 0)
        x <- filter(x, State %in% input$state)
      
      update_filter(
        "city",
        get_choices(x$City),
        input$city
      )
    },
    ignoreInit = FALSE
  )
  
  # Country + Region + State + City -> Postal Code
  observeEvent(
    list(input$country, input$region, input$state, input$city),
    {
      x <- sales
      
      if (length(input$country) > 0)
        x <- filter(x, Country %in% input$country)
      
      if (length(input$region) > 0)
        x <- filter(x, Region %in% input$region)
      
      if (length(input$state) > 0)
        x <- filter(x, State %in% input$state)
      
      if (length(input$city) > 0)
        x <- filter(x, City %in% input$city)
      
      update_filter(
        "postal",
        get_choices(x$`Postal Code`),
        input$postal
      )
    },
    ignoreInit = FALSE
  )
  
  # Reset all filters
  observeEvent(input$reset, {
    updateSelectizeInput(session, "country", selected = character(0))
    updateSelectizeInput(session, "region", selected = character(0))
    updateSelectizeInput(session, "state", selected = character(0))
    updateSelectizeInput(session, "city", selected = character(0))
    updateSelectizeInput(session, "postal", selected = character(0))
    updateSelectizeInput(session, "category", selected = character(0))
    updateSelectizeInput(session, "subcategory", selected = character(0))
    updateSelectizeInput(session, "segment", selected = character(0))
    updateSelectizeInput(session, "shipmode", selected = character(0))
  })
  
  # Apply all selected filters
  dat <- reactive({
    x <- sales
    
    if (length(input$country) > 0)
      x <- filter(x, Country %in% input$country)
    
    if (length(input$region) > 0)
      x <- filter(x, Region %in% input$region)
    
    if (length(input$state) > 0)
      x <- filter(x, State %in% input$state)
    
    if (length(input$city) > 0)
      x <- filter(x, City %in% input$city)
    
    if (length(input$postal) > 0)
      x <- filter(x, `Postal Code` %in% input$postal)
    
    if (length(input$category) > 0)
      x <- filter(x, Category %in% input$category)
    
    if (length(input$subcategory) > 0)
      x <- filter(x, `Sub-Category` %in% input$subcategory)
    
    if (length(input$segment) > 0)
      x <- filter(x, Segment %in% input$segment)
    
    if (length(input$shipmode) > 0)
      x <- filter(x, `Ship Mode` %in% input$shipmode)
    
    x
  })
  
  # ---------------- KPI CARDS ----------------
  
  output$sales_kpi <- renderText({
    if (nrow(dat()) == 0) return("$0.00")
    
    paste0(
      "$",
      format(round(sum(dat()$Sales), 2),
             big.mark = ",", nsmall = 2)
    )
  })
  
  output$profit_kpi <- renderText({
    if (nrow(dat()) == 0) return("$0.00")
    
    paste0(
      "$",
      format(round(sum(dat()$Profit), 2),
             big.mark = ",", nsmall = 2)
    )
  })
  
  output$quantity_kpi <- renderText({
    if (nrow(dat()) == 0) return("0")
    
    format(sum(dat()$Quantity), big.mark = ",")
  })
  
  output$discount_kpi <- renderText({
    if (nrow(dat()) == 0) return("N/A")
    
    paste0(round(mean(dat()$Discount) * 100, 2), "%")
  })
  
  output$record_count <- renderText({
    paste("Records displayed:", nrow(dat()), "of", nrow(sales))
  })
  
  # ---------------- CHART 1: SALES BY CATEGORY ----------------
  
  output$category_plot <- renderPlot({
    validate(
      need(nrow(dat()) > 0,
           "No matching records. Change your filters.")
    )
    
    dat() %>%
      group_by(Category) %>%
      summarise(TotalSales = sum(Sales), .groups = "drop") %>%
      ggplot(aes(Category, TotalSales, fill = Category)) +
      geom_col() +
      scale_fill_manual(values = c(
        "Furniture" = "#8E44AD",
        "Office Supplies" = "#3498DB",
        "Technology" = "#E67E22"
      )) +
      scale_y_continuous(labels = label_dollar()) +
      labs(title = "Sales by Category", x = NULL, y = "Total Sales") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(color = "#6C3483", face = "bold")
      )
  })
  
  # ---------------- CHART 2: PROFIT BY SUB-CATEGORY ----------------
  
  output$subcategory_plot <- renderPlot({
    validate(
      need(nrow(dat()) > 0,
           "No matching records. Change your filters.")
    )
    
    dat() %>%
      group_by(`Sub-Category`) %>%
      summarise(TotalProfit = sum(Profit), .groups = "drop") %>%
      ggplot(aes(
        x = reorder(`Sub-Category`, TotalProfit),
        y = TotalProfit,
        fill = TotalProfit >= 0
      )) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c(
        "TRUE" = "#27AE60",
        "FALSE" = "#E74C3C"
      )) +
      scale_y_continuous(labels = label_dollar()) +
      labs(title = "Profit by Sub-Category",
           x = NULL, y = "Total Profit") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(color = "#6C3483", face = "bold")
      )
  })
  
  # ---------------- CHART 3: TOP 10 STATES BY SALES ----------------
  
  output$state_plot <- renderPlot({
    validate(
      need(nrow(dat()) > 0,
           "No matching records. Change your filters.")
    )
    
    dat() %>%
      group_by(State) %>%
      summarise(TotalSales = sum(Sales), .groups = "drop") %>%
      slice_max(order_by = TotalSales, n = 10, with_ties = FALSE) %>%
      ggplot(aes(
        x = reorder(State, TotalSales),
        y = TotalSales
      )) +
      geom_col(fill = "#3498DB") +
      coord_flip() +
      scale_y_continuous(labels = label_dollar()) +
      labs(title = "Top 10 States by Sales",
           x = NULL, y = "Total Sales") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#6C3483", face = "bold")
      )
  })
  
  # ---------------- CHART 4: SALES VS PROFIT ----------------
  
  output$scatter_plot <- renderPlot({
    validate(
      need(nrow(dat()) > 0,
           "No matching records. Change your filters.")
    )
    
    ggplot(dat(), aes(Sales, Profit, color = Category)) +
      geom_point(alpha = 0.6, size = 1.8) +
      scale_color_manual(values = c(
        "Furniture" = "#E74C3C",
        "Office Supplies" = "#3498DB",
        "Technology" = "#27AE60"
      )) +
      scale_x_continuous(labels = label_dollar()) +
      scale_y_continuous(labels = label_dollar()) +
      labs(title = "Sales vs Profit",
           x = "Sales", y = "Profit", color = "Category") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(color = "#6C3483", face = "bold")
      )
  })
}

shinyApp(ui, server)
