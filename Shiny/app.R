library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(rnaturalearth) 
library(DT)         
library(ggplot2)    
library(dplyr)
library(bslib)
library(plotly)

available_countries <- sort(unique(monthly_data_cleaned_flags$ADM_name))

# set up UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "lumen"),
  titlePanel("Exploring Holidays and Travel Patterns by Country"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4, 
      
      h4("Select Countries by Clicking on the Map"),
      leafletOutput("country_map_selector", height = "400px"), 
      
      conditionalPanel(
        condition = "input.main_tabs == 'Holiday Details'",
        selectInput(
          inputId  = "month",
          label    = "Select a Month:",
          choices  = setNames(1:12, month.name),
          selected = 7
        )
      ),
      
      br(),
      tags$p(HTML("Analysis for the selected country(ies) and month.<br/>"))
    ),
    
    mainPanel(
      width = 8, 
      tabsetPanel( 
        id = "main_tabs", 
        
        tabPanel("Time Trends",
                 h3(textOutput("time_trends_title")), 
                 plotlyOutput("country_travel_time_plot", height = "400px"),
                 br(),
                 p("This plot shows the monthly overseas travel volume for the selected country across all available years. 
                   No datapoints means no data available for that year.", 
                   "Hover over datapoints to see detailed information.")
        ),
        
        tabPanel("Seasonal Holiday Patterns",
                 h3(textOutput("seasonal_patterns_title")), 
                 plotOutput("holiday_heatmap", height = "400px"),
                 br(),
                 p(paste0(
                   "This heatmap displays the number of public holidays per month and year for the selected country ",
                   "(based on data up to ", latest_year, " or the last year with holiday data). 
                   Empty block means no data available for that month.",
                   "It helps visualize seasonal holiday patterns."
                 ))
        ),
        
        tabPanel("Holiday Details",
                 h3(textOutput("holiday_details_title")),
                 DTOutput("detailed_holiday_table"),
                 br(),
                 DTOutput("holiday_dates_table"),
                 br(),
                 p("The table above lists unique holidays for the selected country(ies) and month across all available years. 
                   Click a holiday name to see its specific dates.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  iso_name_lookup <- monthly_data_cleaned_flags |>
    filter(!is.na(ADM_name)) |>
    distinct(ISO3, ADM_name) |>   
    group_by(ISO3) |>          
    slice(1) |>                   
    ungroup()
  
  available_iso3s <- unique(c(
    monthly_data_cleaned_flags$ISO3,
    holidays_travel$ISO3
  ))
  
  message("Server: Preparing world map geometry...")
  world_sf_base <- rnaturalearth::ne_countries(
    scale = "medium", 
    returnclass = "sf"
  ) |> select(iso_a3, name_long, geometry)
  message("Server: World map geometry prepared.")
  
  selected_iso3s <- reactiveVal(character())
  
  # map for Country Selection in sidebar
  output$country_map_selector <- renderLeaflet({
    req(world_sf_base)
    message("RenderLeaflet country_map_selector: Drawing map for ISO3 ", selected_iso3s())
    
    map_data <- world_sf_base |>
      mutate(
        has_data    = iso_a3 %in% available_iso3s,
        is_selected = iso_a3 %in% selected_iso3s()
      )
    
    leaflet(map_data) |>
      addTiles() |> 
      addPolygons(
        fillColor = ~case_when(
          !has_data               ~ "lightgrey",   # no data
          is_selected             ~ "steelblue",   # selected
          has_data & !is_selected ~ "white"        # available but not selected
        ),
        fillOpacity = 0.7,
        color       = "#444444",
        weight      = 0.5,
        label       = ~name_long,    # Use name_long for hover label
        layerId     = ~iso_a3,       # Use iso_a3 as layer ID for clicking
        highlightOptions = highlightOptions(
          weight       = 2,
          color        = "#666",
          fillOpacity  = 0.9,
          bringToFront = TRUE
        )
      ) |>
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  observeEvent(input$country_map_selector_shape_click, {
    click    <- input$country_map_selector_shape_click
    click_id <- click$id
    req(click_id)
    
    if (!(click_id %in% available_iso3s)) {
      showNotification("No data available for that country.", type = "warning")
      return()
    }
    
    current <- selected_iso3s()
    if (click_id %in% current) {
      selected_iso3s(setdiff(current, click_id))
    } else {
      selected_iso3s(c(current, click_id))
    }
  })
  
  # reactive data for monthly travel plot
  country_data_monthly <- reactive({
    req(length(selected_iso3s()) > 0, holidays_travel)
    
    holidays_travel |> 
      filter(ISO3 %in% selected_iso3s()) |>
      group_by(ISO3, Year, Month) |> 
      summarize(
        total_os_monthly = sum(coalesce(Total_OS, Total), na.rm = TRUE), # Use Total if Total_OS is NA
        .groups = "drop"
      ) |>
      arrange(ISO3, Year, Month) |>
      mutate(date = as.Date(sprintf("%04d-%02d-15", Year, Month)),
             total_os_monthly_thousands = total_os_monthly / 1000) |>
      left_join(iso_name_lookup, by = "ISO3") |>
      filter(!is.na(ADM_name))
  })
  
  # reactive data for holiday heatmaps
  country_holiday_markers <- reactive({
    req(length(selected_iso3s()) > 0, monthly_data_cleaned_flags) 
    
    monthly_data_cleaned_flags |> 
      filter(ISO3 %in% selected_iso3s(), count_public_holidays > 0) |> 
      distinct(Year, Month) |>
      mutate(date = as.Date(sprintf("%04d-%02d-15", Year, Month)))
  })
  
  country_holiday_heatmap_data <- reactive({
    req(length(selected_iso3s()) > 0, monthly_data_cleaned_flags) 
    
    monthly_data_cleaned_flags |> 
      filter(ISO3 %in% selected_iso3s()) |>
      select(Year, Month, num_total_holidays, count_public_holidays, ADM_name) 
  })
  
  # reactive data for holiday details
  detailed_holiday_data <- reactive({
    req(length(selected_iso3s()) > 0, input$month, holidays_travel) 
    
    holidays_travel |> 
      filter(ISO3 %in% selected_iso3s(), 
             Month == as.integer(input$month)) |>
      select(ADM_name, Year, Date, Name, Type) |>
      arrange(ADM_name, Year, Date) 
  })
  
  unique_holiday_names <- reactive({
    req(nrow(detailed_holiday_data()) > 0)
    
    df <- detailed_holiday_data()
    df |>
      distinct(ADM_name, Name, Type) |>
      arrange(ADM_name, Name)
  })
  
  selected_holiday_name <- reactiveVal(NULL)
  
  observeEvent(input$detailed_holiday_table_rows_selected, {
    selected_row_index <- input$detailed_holiday_table_rows_selected
    req(length(selected_row_index) > 0)
    
    selected_holiday_info <- unique_holiday_names()[selected_row_index, ]
    
    req(nrow(selected_holiday_info) > 0, "Name" %in% colnames(selected_holiday_info))
    selected_holiday_name(selected_holiday_info$Name)
    message("Observer (detailed_holiday_table_rows_selected): Selected holiday name: ", selected_holiday_info$Name)
  })
  
  dates_for_selected_holiday <- reactive({
    req(!is.null(selected_holiday_name()), nrow(detailed_holiday_data()) > 0)
    
    holiday_name <- selected_holiday_name()
    df <- detailed_holiday_data()
    
    message("Reactive dates_for_selected_holiday: Filtering for holiday name: ", holiday_name)
    
    df |>
      filter(Name == holiday_name) |>
      select(Date) |>
      arrange(Date)
  })
  
  # titles
  output$time_trends_title    <- renderText({
    req(length(selected_iso3s()) > 0, monthly_data_cleaned_flags) 
    names <- monthly_data_cleaned_flags |>
      filter(ISO3 %in% selected_iso3s()) |>
      pull(ADM_name) |> unique()
    names <- names[!is.na(names)]
    paste("Monthly Travel Trends for", paste(names, collapse = ", "))
  })
  
  output$seasonal_patterns_title <- renderText({
    req(length(selected_iso3s()) > 0)
    names <- monthly_data_cleaned_flags |>
      filter(ISO3 %in% selected_iso3s()) |>
      pull(ADM_name) |> unique()
    names <- names[!is.na(names)]
    paste("Seasonal Holiday Patterns for", paste(names, collapse = ", "))
  })
  
  output$holiday_details_title <- renderText({
    req(length(selected_iso3s()) > 0, input$month)
    
    selected_iso3s <- selected_iso3s()
    country_names <- monthly_data_cleaned_flags |>
      filter(ISO3 %in% selected_iso3s) |>
      pull(ADM_name) |>
      unique()
    country_names <- country_names[!is.na(country_names)]
    
    month_name <- month.name[as.integer(input$month)]
    if(length(country_names) > 0) {
      paste0("Holiday Details for ", paste(country_names, collapse = ", "), " in ", month_name, " Across All Years") 
    } else {
      "Holiday Details"
    }
  })
  
  # country specific Monthly Travel Time Plot (in "Time Trends" tab)
  output$country_travel_time_plot <- renderPlotly({
    # drop NA/zero months, used ChatGPT for this
    df <- country_data_monthly() |> 
      filter(!is.na(total_os_monthly) & total_os_monthly > 0)
    
    req(nrow(df) > 0) 
    message("RenderPlot country_travel_time_plot: Plotting travel data for ISO3 ", selected_iso3s())
    
    # get country name for plot title
    country_name <- df$ADM_name[1]
    
    p <- ggplot(df, aes(x = date, y = total_os_monthly_thousands, color = ADM_name)) +
      geom_line(size = 0.6, na.rm = TRUE) +
      geom_point(size = 0.8, na.rm = TRUE) +
      labs(
        x     = "Year",
        y     = "Monthly Overseas Travel Volume (in Thousands)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = 0.05))
    
    ggplotly(p) |> layout(legend = list(title = list(text = "Country")))
  })
  
  # country specific Seasonal Holiday Heatmap (in "Seasonal Holiday Patterns" tab)
  output$holiday_heatmap <- renderPlot({
    df <- country_holiday_heatmap_data() |> filter(!is.na(ADM_name))
    req(nrow(df) > 0)
    df$Month <- factor(df$Month, levels = 1:12, labels = month.abb)
    
    ggplot(df, aes(x = as.factor(Year), y = Month, fill = count_public_holidays)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = "gray", name = "Public Holidays") +
      facet_wrap(~ ADM_name) +
      labs(x = "Year", y = "Month") +
      theme_minimal() +
      theme(
        axis.text.x      = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title       = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      coord_cartesian(expand = FALSE)
  })
  
  # detailed Holiday Table for Selected Country and Month (in "Holiday Details" tab)
  output$detailed_holiday_table <- renderDT({
    req(nrow(unique_holiday_names()) > 0)
    
    df <- unique_holiday_names()
    datatable(
      df,
      options  = list(pageLength = 10),
      rownames = FALSE,
      selection = 'single',
      caption = "Click on a holiday name to see its dates across years."
    )
  })
  
  output$holiday_dates_table <- renderDT({
    req(nrow(dates_for_selected_holiday()) > 0, !is.null(selected_holiday_name()))
    
    df <- dates_for_selected_holiday() 
    holiday_name <- selected_holiday_name() 
    
    message("RenderDT holiday_dates_table: Rendering dates for holiday: ", holiday_name)
    
    datatable(df,
              options = list(pageLength = 10),
              rownames = FALSE,
              caption = htmltools::tags$caption( 
                style = 'caption-side: bottom; text-align: left;',
                paste0("Dates for '", holiday_name, "' across years.")
              )
    )
  })
  
}

shinyApp(ui, server)