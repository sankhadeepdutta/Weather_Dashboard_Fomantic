# Import libraries
library(shiny)
library(semantic.dashboard)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)
library(dplyr)
library(httr)
library(jsonlite)
library(shinyalert)
library(waiter)
library(imola)

# Responsive grid template
mytemplate <- gridTemplate("myareas", "grid", areas = list(
  default = c(
    "content1 content2 content3 content4",
    "content5 content6 content7 content8"
  ),
  lg = c(
    "content1 content2 content3 content4",
    "content5 content6 content7 content8"
  ),
  md = c(
    "content1",
    "content2",
    "content3",
    "content4",
    "content5",
    "content6",
    "content7",
    "content8"
  )
))

# Responsive template for search area
searchTemplate <- gridTemplate("myareas", "grid", areas = list(
  default = c("content1 content2", "content3 content3"),
  lg = c("content1 content2", "content3 content3"),
  md = c("content1", "content2", "content3")
))

# Define header row
header_row <- box(uiOutput("header"), color = "green")

# Define control panel
sidePanel <- gridPanel(
  template = searchTemplate,
  content1 = textInput(
    "search",
    label = "",
    placeholder = "Search Location",
    width = "100%"
  ),
  content2 = actionBttn(
    inputId = "search_btn",
    label = "Search/Update",
    icon = icon("search"),
    style = "gradient",
    size = "sm",
    block = F,
    color = "primary"
  ),
  content3 = box(
    uiOutput("temp_icon"),
    color = "orange",
    collapsible = F
  ),
  gap = "10px"
)

# Define main content
content = div(
  gridPanel(
    template = mytemplate,
    content1 = box(uiOutput("time"), color = "purple"),
    content2 = box(uiOutput("rain_chance"), color = "orange"),
    content3 = box(uiOutput("humidity"), color = "orange"),
    content4 = box(uiOutput("wind_speed"), color = "orange"),
    content5 = box(uiOutput("uv_index"), color = "orange"),
    content6 = box(uiOutput("snow_chance"), color = "orange"),
    content7 = box(uiOutput("feels_like"), color = "orange"),
    content8 = box(uiOutput("overcast"), color = "orange"),
    gap = "10px"
  )
)

# Define dashboard UI
ui <- dashboardPage(
  title = "Weather Dashboard",
  dashboardHeader(disable = T),
  dashboardSidebar(visible = F),
  dashboardBody(
    useWaiter(),
    autoWaiter(color = "white", html = spin_2()),
    tags$head(
      tags$style(
        "#header{font-family: 'Gill Sans', sans-serif;color: #053B50;font-size: 25px;}"
      )
    ),
    gridPanel(
      template = "grail-right-sidebar",
      header = header_row,
      content = content,
      sidebar = box(sidePanel),
      footer =
        gridPanel(
          template = "sidebar-right",
          content = box(plotlyOutput("forecast_plot"),
                        color = "orange"),
          sidebar = box(plotOutput("rain_percentage"),
                        color = "orange"),
          gap = "10px"
        ),
      gap = "10px"
    )
  )
)

# Define server
server <- function(input, output, session) {
  ## Intimation about the limited API requests
  shinyalert(
    title = "Information",
    text = "Enter your desired location name and click on the 'Search/Update' button.\n Note: You can search for maximum of 3 locations only.",
    size = "s",
    closeOnEsc = F,
    closeOnClickOutside = F,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#3B71CA",
    animation = F
  )
  
  ### Initial API setup ###
  api_base_url <-
    "https://weatherapi-com.p.rapidapi.com/forecast.json"
  
  headers <-
    c('X-RapidAPI-Key' = 'a02bbd1cf4msheba534c7b56fac7p174702jsnc0db991a4a76',
      'X-RapidAPI-Host' = 'weatherapi-com.p.rapidapi.com')
  
  # API call count
  count_request = reactiveVal(0)
  increment_count <- function() {
    count_request(count_request() + 1)
  }
  
  # Code execution flag
  run_code <- T
  
  # Required dataframes initialization
  current_data <- reactiveValues(df = NULL)
  
  forecast_data <- reactiveValues(df = NULL)
  
  rain_chance <- reactiveValues(df = NULL)
  
  jsonData <- reactiveVal(NULL)
  
  # Define search button functionality
  observeEvent(input$search_btn, {
    
    if (count_request() == 3) {
      
      ## Update flag
      run_code <- F
      
      ## Intimation about the limited API requests
      shinyalert(
        title = "Information",
        text = "You have reached your maximum limit for location searches. Press 'OK' to close the application.",
        size = "s",
        closeOnEsc = F,
        closeOnClickOutside = F,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#3B71CA",
        animation = F,
        callbackR = function() {
          stopApp()
        }
      )
    }
    
    # Query parameters
    query_params <- list(q = input$search, days = "3")
    
    # Encode the query parameters
    encoded_params <-
      URLencode(names(query_params), reserved = TRUE)
    
    query_string <-
      paste(encoded_params,
            "=",
            URLencode(unlist(query_params)),
            sep = "",
            collapse = "&")
    
    # Construct the complete URL
    api_url <- paste0(api_base_url, "?", query_string)
    
    response <- GET(api_url, add_headers(.headers = headers))
    
    # Check if the request was successful (status code 200)
    if (response$status_code == 200 & run_code == T) {
      
      # Record api call count
      increment_count()
      
      # Parse the JSON response
      json_data <-
        fromJSON(content(response, "text"), flatten = TRUE)
      
      jsonData(json_data)
      
      forecast_today <-
        as.data.frame(json_data$forecast$forecastday$hour[1])
      
      format_time <-
        strptime(json_data$location$localtime, format = "%Y-%m-%d %H:%M") %>% format(format = "%Y-%m-%d %H:00")
      
      # Current weather details dataframe
      current_data$df <- subset(forecast_today, time == format_time)
      
      forecast1 <-
        json_data$forecast$forecastday$hour[1] %>% as.data.frame()
      forecast1 <- forecast1[forecast1$time >= format_time, ]
      forecast2 <-
        json_data$forecast$forecastday$hour[2] %>% as.data.frame()
      forecast3 <-
        json_data$forecast$forecastday$hour[3] %>% as.data.frame()
      
      # Temperature forecast dataframe
      forecast_data$df <- rbind(forecast1, forecast2, forecast3)
      
      # Rain chance dataframe
      rain_chance$df <-
        forecast_today[forecast_today$chance_of_rain > 0 &
                         forecast_today$time >= format_time, ]
      rain_chance$df$time <-
        strptime(rain_chance$df$time, format = "%Y-%m-%d %H:%M") %>% format(format = "%H:%M")
      
    } else if (input$search == "") {
      
      # Shiny modal for no location name
      shinyalert(
        title = "Information",
        text = "Enter a location name to search",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#3B71CA",
        animation = F
      )
    }
    else if (response$status_code == 400 & run_code == T) {
      
      # Record api call count
      increment_count()
      
      # Shiny modal for error in API request
      shinyalert(
        title = "Oops!",
        text = "Unable to fetch data for the entered location.",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#3B71CA",
        animation = F
      )
    }
    else if (run_code == F) {
      
      # Shiny modal after application stops
      shinyalert(
        title = "Bye!",
        text = "The application has stopped. You can close the window now.",
        size = "s",
        closeOnEsc = F,
        closeOnClickOutside = F,
        html = FALSE,
        type = "info",
        showConfirmButton = F,
        showCancelButton = FALSE,
        animation = F
      )
    }
    else {
      
      # Shiny modal for any other error
      shinyalert(
        title = "Oops!",
        text = "An unexpected error has occured. Please try again!",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#3B71CA",
        animation = F
      )
    }
    
  })
  
  # Define value box for weather parameters
  output$wind_speed <- renderUI({
    input$search_btn
    value_box(
      title = "Wind Speed",
      value = paste(current_data$df$wind_kph, "Km/h"),
      showcase = bs_icon("wind")
    )
  })
  
  output$uv_index <- renderUI({
    input$search_btn
    value_box(
      title = "UV Index",
      value = current_data$df$uv,
      showcase = bs_icon("radioactive")
    )
  })
  
  output$rain_chance <- renderUI({
    input$search_btn
    value_box(
      title = "Rain Chance",
      value = paste(current_data$df$chance_of_rain, "%"),
      showcase = bs_icon("cloud-drizzle-fill")
    )
  })
  
  output$snow_chance <- renderUI({
    input$search_btn
    value_box(
      title = "Snow Chance",
      value = paste(current_data$df$chance_of_snow, "%"),
      showcase = bs_icon("cloud-snow-fill")
    )
  })
  
  output$humidity <- renderUI({
    input$search_btn
    value_box(
      title = "Humidity",
      value = paste(current_data$df$humidity, "%"),
      showcase = bs_icon("droplet-fill")
    )
  })
  
  output$feels_like <- renderUI({
    input$search_btn
    value_box(
      title = "Feels Like",
      value = paste(current_data$df$feelslike_c, "°C"),
      showcase = bs_icon("thermometer-sun")
    )
  })
  
  output$time <- renderUI({
    input$search_btn
    value_box(
      title = format(Sys.Date(), "%A, %b %d"),
      value = strptime(jsonData()$location$localtime, format = "%Y-%m-%d %H:%M") %>% format(format = "%H:%M"),
      showcase = bs_icon("clock"),
    )
  })
  
  output$overcast <- renderUI({
    input$search_btn
    value_box(
      title = "Cloud Overcast",
      value = paste(current_data$df$cloud, "%"),
      showcase = bs_icon("cloudy-fill")
    )
  })
  
  # Header text
  output$header <- renderUI({
    input$search_btn
    text = paste(
      "Today Overview : ",
      paste(
        jsonData()$location$name,
        jsonData()$location$region,
        jsonData()$location$country,
        sep = ", "
      )
    )
    text
  })
  
  
  # Displays current temperature and the associated icon
  output$temp_icon <- renderUI({
    input$search_btn
    image_url <- current_data$df$condition.icon
    img_tag <- tags$img(src = image_url, alt = "Image")
    div_tag <- tags$div(img_tag, style = "text-align: center;")
    value_box(
      title = "Current Temperature",
      current_data$df$condition.text,
      value = paste(current_data$df$temp_c, "°C"),
      showcase = div_tag
    )
  })
  
  
  # Displays Rain Chance graph
  output$rain_percentage <- renderPlot({
    input$search_btn
    data <- tryCatch({
      rain_chance$df
    }, error = function(e) {
      NULL
    })
    if (is.null(data)) {
      NULL
    }
    else{
      plot <- ggplot(data, aes(x = chance_of_rain,
                               y = time,
                               width = 0.3)) +
        geom_bar(stat = "identity",
                 show.legend = F,
                 fill = "#279EFF") +
        geom_text(
          aes(label = paste(chance_of_rain, "%")),
          hjust = -0.4,
          color = "black",
          size = 3
        ) +
        labs(x = NULL,
             y = NULL,
             title = "Chances of Rain (Today)") +
        xlim(0, 100) +
        theme_minimal() +
        theme(
          plot.background = element_blank(),
          # Set transparent background
          panel.grid.major = element_blank(),
          # Remove major gridlines
          panel.grid.minor = element_blank(),
          # Remove minor gridlines
          panel.border = element_blank(),
          # Remove plot border
          axis.text.x = element_blank(),
          # Remove y-axis tick labels
          axis.ticks.x = element_blank(),
          # Remove y-axis ticks
          plot.title = element_text(size = 16, hjust = 0)  # Customize title font style
        )
      
      if (nrow(data) == 0) {
        plot + labs(title = "No more rain chances for today")
      }
      else{
        plot
      }
    }
  })
  
  # Displays time series forecast plot
  output$forecast_plot <- renderPlotly({
    input$search_btn
    data <-
      tryCatch({
        forecast_data$df
      }, error = function(e) {
        NULL
      })
    if (is.null(data)) {
      NULL
    }
    else{
      data$time <-
        as.POSIXct(data$time, format = "%Y-%m-%d %H:%M")
      
      tooltip_text <-
        paste(
          "Date and Time:",
          format(data$time, format = "%Y-%m-%d %H:%M"),
          "Temperature:",
          data$temp_c,
          "°C"
        )
      
      plot_ly(
        data,
        x = ~ time,
        y = ~ temp_c,
        text = tooltip_text,
        hovertemplate = paste('%{text}'),
        type = "scatter",
        mode = "lines",
        name = "forecast"
      ) %>%
        layout(
          title = list(text = '3 days temperature forecast', x = 0),
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE
          ),
          paper_bgcolor = "rgba(0,0,0,0)",
          # Transparent background
          plot_bgcolor = "rgba(0,0,0,0)"    # Transparent plot area
        )
    }
  })
  
}

shinyApp(ui, server)
