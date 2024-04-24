library(shinydashboard)
library(tidyverse)
library(leaflet)
library(maps)
library(wordcloud)
library(tm)
library(igraph)
library(ggraph)

########################################################

transportation_data <- read.csv("THT_Data_5082.csv", header=FALSE)
fatalities_data <- transportation_data %>%
  summarise(
    Auto_Fatalities = mean(as.numeric(V24), na.rm = TRUE),
    Bicycle_Fatalities = mean(as.numeric(V26), na.rm = TRUE),
    Pedestrian_Fatalities = mean(as.numeric(V28), na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Mode", values_to = "Fatalities")

transportation_data <- transportation_data %>%
  mutate(V37 = as.numeric(as.character(V37)))

transportation_data$V37 <- as.numeric(as.character(transportation_data$V37))
transportation_data$V24 <- as.numeric(as.character(transportation_data$V24))

transportation_data$V20 <- as.numeric(as.character(transportation_data$V20))

transportation_data$state_code <- state.abb[match(transportation_data$V2, state.name)]

us_map <- map_data("state")
map_data <- merge(us_map, transportation_data, by.x = "region", by.y = "V3")

state_centers <- map_data %>%
  group_by(region) %>%
  summarise(center_long = mean(long, na.rm = TRUE),
            center_lat = mean(lat, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(transportation_data, by = c("region" = "V3"))

#A leaflet interactive map with activity scores
transportation_data$V20 <- as.numeric(as.character(transportation_data$V20))
transportation_data$state_code <- state.abb[match(transportation_data$V2, state.name)]
us_map <- map_data("state")
us_map$region <- tolower(us_map$region)

map_data2 <- merge(us_map, transportation_data, by.x = "region", by.y = "V3", all.x = TRUE)

state_centers2 <- map_data2 %>%
  group_by(region) %>%
  summarise(center_long = mean(long, na.rm = TRUE),
            center_lat = mean(lat, na.rm = TRUE),
            activity_score = mean(V20, na.rm = TRUE),
            state_name = first(V2))  

pal <- colorNumeric(palette = "viridis", domain = state_centers2$activity_score)

transportation_data$V4 <- as.numeric(as.character(transportation_data$V4))
transportation_data$V6 <- as.numeric(as.character(transportation_data$V6))

# Here calculates the similarity matrices for auto and transit mode shares
auto_similarity <- outer(transportation_data$V4, transportation_data$V4, FUN = function(x, y) abs(x - y) < 0.1 * x)
transit_similarity <- outer(transportation_data$V6, transportation_data$V6, FUN = function(x, y) abs(x - y) < 0.1 * x)

combined_similarity <- auto_similarity | transit_similarity

graph <- graph_from_adjacency_matrix(combined_similarity, mode = "undirected", diag = FALSE)
V(graph)$name <- transportation_data$V2

########################################################

# The UI

ui <- dashboardPage(
  
  dashboardHeader(
    title="Group I Final Project: Transportation and Health Across the United States", titleWidth = 800
  ),
  
  dashboardSidebar(
    
    width = 300,
    sidebarMenu(
      menuItem("static images based on ggplot2", tabName = "tab1"),
      menuItem("maps using geospatial data", tabName = "tab2"),
      menuItem("network analysis", tabName = "tab3"),
      menuItem("search", tabName = "tab4")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "tab1",
        fluidPage(
          h1("Interactive Analysis of Transportation Practices and Commuter Health Across the U.S"),
          span(style = "font-size:20px", 
               "Xiaokuan Ye, Yu (Richard) Li, Yulin (James) Mao"), 
          h2("Introduction"),
          span(style = "font-size:20px", 
               "The purpose of this project is to explore the association between commuting patterns and population health outcomes across U.S. states, specifically analyzing the impact of different transportation modes on physical activity levels and traffic safety. By using a combination of transportation data and health indicators, this project aims to reveal how public health can be enhanced through improved transportation policies and infrastructure."),
          h2("Data Description"),
          span(style = "font-size:20px", 
               "This dataset provides a comprehensive look at the transportation and health of each US state. Included are important indicators such as commute mode share (auto, transit, bicycle and walk), complete streets policies, person miles of travel by private vehicle and walking, physical activity from transportation sources, road traffic fatalities exposure rates (auto, bicycle and pedestrian), seat belt use, transit trips per capita, use of federal funds for bicycle/pedestrian efforts, vehicle miles traveled per capita and proximity to major roadways. https://www.kaggle.com/datasets/thedevastator/us-state-transportation-health."),
          br(),
          h2("Average Traffic Fatalities by Commute Mode"),
          span(style = "font-size:20px", 
               "Comparing average road traffic fatalities for auto, bicycle, and pedestrian"),
          br(),
          fluidRow(column(width = 6, offset = 3, plotOutput("p1"))),
          br(),
          h2("Seat Belt Use Score Across Different States"),
          span(style = "font-size:20px", 
               "Visualizing the distribution of seat belt use scores across states. Apparently Washington, Oregon, California, Texas and Minnesota are the top 5 states that uses seat belt, which could be very possibly associated with lower fatality rates. On the other hand, South Dakota, New Hampshire, Arkansas, Massachusetts, and Montana are the bottom 5 states that uses seat belt while driving."),
          br(),
          span(style = "font-size:20px", 
               "States like Washington, Oregon, and California, known for their progressive attitudes towards safety and stronger enforcement of traffic laws, likely exhibit higher seat belt usage. These states often have dense urban populations, well-maintained road networks, and significant public awareness campaigns promoting safety measures. In contrast, states with lower seat belt usage such as South Dakota and Montana may have more rural populations, longer distances between destinations, and a culture that places less emphasis on strict adherence to safety regulations."),
          br(),
          fluidRow(
            column(
              width = 6, 
              offset = 3, 
              box(width = 12,
                  height = 720,
                  plotOutput("p2"))
            )
          ),
          br(),
          h2("Correlation Between Seat Belt Usage and Fatality Rate"),
          span(style = "font-size:20px", 
               "As expected, the seat belt use and fatality rate is negatively correlated: the trendline fit with the pattern of the scatters, higher use of seat belt is associated in lower expected fatality rates."),
          br(),
          fluidRow(column(width = 6, offset = 3, plotOutput("p3")))
        )
      ),
      
      tabItem(
        tabName = "tab2",
        fluidPage(
          # br(),
          h2("Physical Activity Score"),
          span(style = "font-size:20px", 
               "Measure of physical activity associated with transportation in each state. 'Commute Mode Share': the percentage of people using active modes of transportation such as walking, biking, or public transit for their daily commute. States with higher percentages of people using these modes might have higher physical activity scores, which leads to a potentially better health outcomes."),
          br(),
          fluidRow(column(width = 6, offset = 3, plotOutput("p4"))),
          br(),
          fluidRow(column(width = 6, offset = 3, leafletOutput("p5")))
        )
      ),
      
      tabItem(
        tabName = "tab3",
        fluidPage(
          # br(),
          h2("Network of State With Similar Commuting Patterns"),
          span(style = "font-size:20px", 
               "Network that represents states with similar commuting patterns. - The dense cluster of nodes in the center with many overlapping edges suggests that many states have similar commuting patterns according to your similarity criteria. This could mean that most states share common percentages of commuters using auto or transit modes - Outliers: States like New York seem to be outliers with very few connections or possibly none at all. This implies that New York's commuting patterns are significantly different from most other states. Given New York's extensive public transportation system, this might reflect a higher use of transit compared to other states."),
          br(),
          fluidRow(column(width = 6, offset = 3, plotOutput("p6")))
        )
      )
      ,
      
      tabItem(
        tabName = "tab4",
        fluidPage(
          # br(),
          h2("Fatality by State and Type od Transportation"),
          span(style = "font-size:20px", 
               "What we want to achieve here are 2 search boxes with these features and functionalities: 1. selecting state 2. type of transportation to find fatality score of corresponding state and display on the state map"),
          br(),
          br(),
          box(
            width = 4,
            selectInput("state",
                        "Select a State:",
                        choices = c(
                          "Alabama", "Alaska", "Arizona", "Arkansas",
                          "California", "Colorado",
                          "Connecticut", "Delaware", "Florida",
                          "Georgia", "Hawaii", "Idaho",
                          "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
                          "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                          "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
                          "New Hampshire", "New Jersey", "New Mexico", "New York",
                          "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
                          "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                          "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                          "West Virginia", "Wisconsin", "Wyoming")),
            selectInput("transportType",
                        "Select Type of Transportation:", 
                        choices = c("Auto" = "Road.Traffic.Fatalities.per.100.000.Residents...Auto..Score", 
                                    "Bicycle" = "Road.Traffic.Fatalities.per.100.000.Residents...Bicycle..Score", 
                                    "Pedestrian" = "Road.Traffic.Fatalities.per.100.000.Residents...Pedestrian..Score")),
            actionButton("search", "Search")
          ),
          box(
            width = 8,
            textOutput("result"),
            leafletOutput("p7")
          )
        )
      )
    )
  )
)

########################################################

# The Server
server <- function(input, output) {
  
  output$p1 <- renderPlot({
    ggplot(fatalities_data, aes(x = Mode, y = Fatalities, fill = Mode)) +
      geom_col() +
      labs(x = "Mode of Transport", y = "Average Fatalities per 100,000 Residents") +
      ggtitle("Comparison of Average Traffic Fatalities by Commute Mode") +
      theme_minimal()
  }, res = 96)
  
  output$p2 <- renderPlot({
    ggplot(transportation_data, aes(x = reorder(V2, V37, FUN = median), y = V37, fill = V2)) + 
      geom_boxplot() +
      scale_fill_viridis_d() +  
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
            axis.text.y = element_text(size = 10),
            legend.position = "none") +  
      labs(x = "State", y = "Seat Belt Use: Score", title = "Distribution of Seat Belt Use Scores Across States") +
      coord_flip() 
  }, res = 96, height = 700)
  
  output$p3 <- renderPlot({
    ggplot(transportation_data, aes(x = V37, y = V24)) +
      geom_point(aes(color = V2), size = 3) +  
      geom_smooth(method = "lm", se = FALSE, color = "black") +  
      scale_color_viridis_d() +  
      theme_minimal() +
      labs(x = "Seat Belt Use: Score", 
           y = "Road Traffic Fatalities per 100,000 Residents - Auto", 
           title = "Relationship Between Seat Belt Use Scores and Auto Fatalities") +
      theme(legend.position = "none") 
  }, res = 96)
  
  output$p4 <- renderPlot({
    gg <- ggplot(data = map_data, aes(x = long, y = lat, group = group, fill = V20)) +
      geom_polygon(color = "white") +
      geom_path() +
      scale_fill_viridis_c(name = "Physical Activity Score") +
      theme_void() +
      labs(title = "Physical Activity by State", subtitle = "Based on Commute Mode Share")
    
    gg <- gg + geom_text(data = state_centers, aes(x = center_long, y = center_lat, label = state_code, group = NULL), size = 3, hjust = 0.5, vjust = 0.5)
    
    gg + coord_fixed(1.3)
  }, res = 96)
  
  output$p5 <- renderLeaflet({
    leaflet(state_centers2) %>%
      addTiles() %>%
      addCircles(lng = ~center_long, lat = ~center_lat, weight = 1,
                 radius = 50000, color = ~pal(activity_score),
                 fillOpacity = 0.8, popup = ~paste(state_name, "<br>Activity Score: ", activity_score)) %>%
      addLegend("bottomright", pal = pal, values = ~activity_score,
                title = "Activity Score", opacity = 0.7)
  })
  
  output$p6 <- renderPlot({
    ggraph(graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) +
      geom_node_point(color = "darkblue", size = 3) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      theme_graph() +
      labs(title = "Network of State Similarities in Commuting Patterns")
  })
  
  observeEvent(input$search, {
    
    transportation_data <- 
      read.csv("THT_Data_5082.csv", header=TRUE)
    
    fatality_score <- transportation_data[transportation_data$state == input$state, input$transportType]
    
    # Output the result
    output$result <- renderText({
      if (!is.na(fatality_score) && length(fatality_score) > 0) {
        paste("The fatality score for", input$transportType, "transportation in", input$state, "is", fatality_score,
              "\nState: ", input$state,
              "\nTransportation Type: ", input$transportType,
              "\nFatality Score: ", fatality_score)
      } else {
        paste("Data not available for the selected state and transportation type.",
              "\nState: ", input$state,
              "\nTransportation Type: ", input$transportType)
      }
    })
    
    transportation_data[['Road.Traffic.Fatalities.per.100.000.Residents...Auto..Score']] <- as.numeric(as.character(transportation_data[['Road.Traffic.Fatalities.per.100.000.Residents...Auto..Score']]))
    transportation_data[['Road.Traffic.Fatalities.per.100.000.Residents...Bicycle..Score']] <- as.numeric(as.character(transportation_data[['Road.Traffic.Fatalities.per.100.000.Residents...Bicycle..Score']]))
    transportation_data[['Road.Traffic.Fatalities.per.100.000.Residents...Pedestrian..Score']] <- as.numeric(as.character(transportation_data[['Road.Traffic.Fatalities.per.100.000.Residents...Pedestrian..Score']]))
    #print('here')
    transportation_data$state_code <- state.abb[match(transportation_data$state, state.name)]
    
    us_map <- map_data("state")
    
    us_map$region <- tolower(us_map$region)
    
    map_data <- merge(us_map, transportation_data, by.x = "region", by.y = "region", all.x = TRUE)
    
    #state_centers
    # here we calculated the center longitude and latitude for each state
    map_data$`Road.Traffic.Fatalities.per.100.000.Residents...Auto..Score`= as.numeric(map_data$`Road.Traffic.Fatalities.per.100.000.Residents...Auto..Score`)
    map_data$`Road.Traffic.Fatalities.per.100.000.Residents...Bicycle..Score`= as.numeric(map_data$`Road.Traffic.Fatalities.per.100.000.Residents...Bicycle..Score`)
    map_data$`Road.Traffic.Fatalities.per.100.000.Residents...Pedestrian..Score`= as.numeric(map_data$`Road.Traffic.Fatalities.per.100.000.Residents...Pedestrian..Score`)
    state_centers <- map_data %>%
      # mutate(
      #   `Road Traffic Fatalities per 100,000 Residents - Auto: Score` = as.numeric(`Road Traffic Fatalities per 100,000 Residents - Auto: Score`),
      #   `Road Traffic Fatalities per 100,000 Residents - Bicycle: Score` = as.numeric(`Road Traffic Fatalities per 100,000 Residents - Bicycle: Score`),
      #   `Road Traffic Fatalities per 100,000 Residents - Pedestrian: Score` = as.numeric(`Road Traffic Fatalities per 100,000 Residents - Pedestrian: Score`)
      # ) %>%
      group_by(region) %>%
      summarise(
        center_long = mean(long, na.rm = TRUE),
        center_lat = mean(lat, na.rm = TRUE),
        auto_score = mean(`Road.Traffic.Fatalities.per.100.000.Residents...Auto..Score`, na.rm = TRUE),
        bicycle_score = mean(`Road.Traffic.Fatalities.per.100.000.Residents...Bicycle..Score`, na.rm = TRUE),
        pedestrian_score = mean(`Road.Traffic.Fatalities.per.100.000.Residents...Pedestrian..Score`, na.rm = TRUE),
        state_name = first(state)
      )
    
    selected_state_data <- state_centers %>%
      filter(state_name == input$state) %>%
      select(center_long, center_lat, auto_score, bicycle_score, pedestrian_score)
    
    # Score to be displayed based on the selected transportation type
    transport_type_score <- switch(input$transportType,
                                   "Road.Traffic.Fatalities.per.100.000.Residents...Auto..Score" = selected_state_data$auto_score,
                                   "Road.Traffic.Fatalities.per.100.000.Residents...Bicycle..Score" = selected_state_data$bicycle_score,
                                   "Road.Traffic.Fatalities.per.100.000.Residents...Pedestrian..Score" = selected_state_data$pedestrian_score
    )
    # Create a color palette for the activity score
    pal <- colorNumeric(palette = "viridis", domain = state_centers[[input$transportType]])
    
    output$p7 <- renderLeaflet({
      if (nrow(selected_state_data) > 0) {
        leaflet() %>%
          addTiles() %>%
          addMarkers(
            lng = selected_state_data$center_long, 
            lat = selected_state_data$center_lat, 
            popup = paste(input$state, input$transportType, "Score:", transport_type_score)
          ) %>%
          setView(lng = selected_state_data$center_long, lat = selected_state_data$center_lat, zoom = 6) 
      } else { 
        leaflet() %>%
          addTiles() %>%
          setView(lng = -96.9, lat = 37.8, zoom = 4) 
      } 
    })
  })
  
}

########################################################

# Run the App
shinyApp(ui = ui, server = server)
