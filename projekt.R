rm(list = ls())
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

#url <- 'https://covid.ourworldindata.org/data/owid-covid-data.csv'
#destfile <- 'C:\\Users\\Owner\\Desktop\\Lokacija\\owid-covid-data.csv'
#download.file(url,destfile)
data <-
  as.data.frame(
    read.csv(
      'C:\\Users\\Owner\\Desktop\\Lokacija\\owid-covid-data.csv',
      header = TRUE,
      sep = ','
    )
  )


ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Covid 19 data"),
  sidebarLayout(
    sidebarPanel(width = 3, 
                 # Select date range to be plotted
                 dateRangeInput(
                   "date",
                   strong("Date range"),
                   start = "2020-01-01",
                   end = "2022-12-31",
                   min = "2020-01-01",
                   max = "2022-12-31"
                 ),
    ),
    
    # Output: Description, lineplot, and reference
    mainPanel( width = 9,
               plotOutput(outputId = "lineplot", width = "90%"),
    )
  )
)
server <- function(input, output) {
  
  output$lineplot <- renderPlot({ 
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    chosenDay <- filter(data, date == input$date[2])
    dayBefore <- filter(data, date == input$date[1])
    
    chosenDay[is.na(chosenDay)] = 0
    chosenDay$location[chosenDay$location == "United States"] = "USA"
    chosenDay$location[chosenDay$location == "United Kingdom"] = "UK"
    chosenDay$location[chosenDay$location == "Cote d'Ivoire"] = "Ivory Coast"
    chosenDay$location[chosenDay$location == "Democratic Republic of Congo"] = "Democratic Republic of the Congo"
    chosenDay$location[chosenDay$location == "Congo"] = "Republic of Congo"
    
    # Zemlje koje nisu pokrivene COVID datasetom
    # Turkmenistan, Luxembourg, Yemen, Zambia, Zimbabwe, French Guiana, Antarctica
    
    dayBefore[is.na(dayBefore)] = 0
    dayBefore$location[dayBefore$location == "United States"] = "USA"
    dayBefore$location[dayBefore$location == "United Kingdom"] = "UK"
    dayBefore$location[dayBefore$location == "Cote d'Ivoire"] = "Ivory Coast"
    dayBefore$location[dayBefore$location == "Democratic Republic of Congo"] = "Democratic Republic of the Congo"
    dayBefore$location[dayBefore$location == "Congo"] = "Republic of Congo"
    
    casesDifference = ((chosenDay$new_cases / dayBefore$new_cases) * 100) - 100
    b <- c(-10000, 0, 10000)
    print(casesDifference)
    
    world <- map_data("world")
    
    world$variable <- casesDifference[match(world$region, chosenDay$location)]
    
    world_map <- ggplot(world, aes(x = long, y = lat)) +
      geom_polygon(aes(group = group, fill = variable),
                   col = "white",
                   lwd = 0.1)
    world_map + scale_fill_gradientn(
      limits = c(-10000,10000),
      labels = b,
      breaks = b,
      colors = c("#0093ff", "#c0e4ff", "#ffa6b6", "#ff0000"),
      values = scales::rescale(c(-10000, -1, 1, 10000))
    )
  })
}

shinyApp(ui = ui, server = server)
