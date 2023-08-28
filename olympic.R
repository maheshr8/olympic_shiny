# Name - Mahesh Balasaheb Raut
# University of Guelph ID - 1227509
# DATA*6200 F22 - Data Manipulation and Visualization
# Final Project - Olympics Data Analysis using Shiny
# Link for dataset - https://www.kaggle.com/datasets/heesoo37/120-years-of-olympic-history-athletes-and-results

# Load Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(maps)

# Load athletes_events data 
data <- read.csv("athlete_events.csv", header = TRUE, stringsAsFactors = FALSE)

# Load data file matching NOCs with map regions (countries)
noc <- read.csv("noc_regions.csv")


########################### Growth of Games #################################
data_growth <- data %>% filter(Sport != "Art Competitions") %>%
  group_by(Year, Season) %>%
  summarize(
    Athletes = length(unique(ID)),
    Nations = length(unique(NOC)),
    Events = length(unique(Event))
  )


############################ Sports Leaderboard ##############################
data_sports <- data.frame(data)
events <- unique(data_sports$Event)
year <- unique(sort(data_sports$Year))


data_sports[sapply(data_sports, is.numeric)] <- lapply(data_sports[sapply(data_sports, is.numeric)], 
                                                       function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

data_sports$Medal <- ifelse(is.na(data_sports$Medal),"Did not win", data_sports$Medal)

data_sports["Sex"][data_sports["Sex"]=='M'] <- 'Male'
data_sports["Sex"][data_sports["Sex"]=='F'] <- 'Female'

data_sports_year <- data_sports %>% filter(Medal != "Did not win")

unique(data_sports_year$Year)


############################### Map View ###################################
medal_mapview <- data %>% filter(!is.na(Medal))%>% 
  group_by(NOC, Medal, Event, Games) %>%
  summarize(isMedal=1)

medal_mapview <-  medal_mapview %>% 
  group_by(NOC, Medal) %>%
  summarize(Count= sum(isMedal))

medal_mapview <- left_join(medal_mapview, noc, by= "NOC" ) %>% select(region, NOC, Medal, Count)

medal_mapview <- medal_mapview %>%
  group_by(region) %>%
  summarize(Total=sum(Count))

data_regions <- medal_mapview %>% 
  left_join(noc,by="region") %>%
  filter(!is.na(region))

world_data <- map_data("world")
world_data <- left_join(world_data, data_regions, by="region")


############################################################################
############################## UI page #####################################
############################################################################
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Olympic History of 1896 to 2016", titleWidth = 350),
  
  dashboardSidebar(
    sidebarUserPanel(name="Olympics",subtitle = "Dashboard", image ="https://www.freeiconspng.com/uploads/sports-olympic-rings-icon-3.png"),
    
    sidebarMenu(
      id = 'sidebar',
      menuItem("About", tabName = "tab1"),
      menuItem("Growth of the Games", tabName = "tab2"),
      menuItem("Sports Leaderboard by Medals", tabName = "tab3"),
      menuItem("Medal Tally", tabName = "tab4"),
      menuItem("Map View", tabName = "tab5"),
      style="margin-top: 20px"
    ),
    
    #icon under sidebarMenu
    tags$img(
      src="https://www.freeiconspng.com/uploads/sports-olympic-torch-png-23.png",
      style="width: 90%; margin-top: 100px"
    )
  ), 
  
  #begin body
  dashboardBody(
    #begin tab items
    tabItems(
      #first tab: landing
      tabItem(
        tabName = 'tab1',
        fluidPage(
          fluidRow(
            column(
              width = 12,
              h1('Olympic Games Analysis'),
              h4('Created by Mahesh Raut'),
              
              tags$hr(style="border-color: black;"),
              
              h3('Olympic dashboard!'),
              p('Welcome! This Shiny App analyzes Olympic data from 1896 to 2016. The dataset consists of events, nations, medals, season, 
                demographic information and more. The Summer and Winter Olympic Games occur every four years once. The first modern Olympics was held in 1896 and Athens was the first host city of the Olympics.'),
              br(),
              HTML('<p>Olympic data retrieved from <a href="https://www.kaggle.com/datasets/heesoo37/120-years-of-olympic-history-athletes-and-results">Kaggle</a>
                    and below image referred from <a href="https://www.digitalshadows.com/uploads/2018/02/Threats-to-the-Winter-Olympics.png">here</a>.</p>'),

            )
          ), #end title row
          
          tags$hr(style="border-color: black;"),
          
          #L pic row
          fluidRow(
            class = 'picrow',
            column(
              width = 7,
              img(
                src = 'https://www.digitalshadows.com/uploads/2018/02/Threats-to-the-Winter-Olympics.png',
                width = '90%'
              )
            ),
            
            column(
              class = 'Rtext',
              width = 5,
              br(), br(),
              p("The dashboard provides visualisations on data on the ",
                a("Olympic Games", href="https://en.wikipedia.org/wiki/Olympic_Games"),
                ", the international sporting event held every four years between countries."),
              p("Navigate the dashboard by the instructions provided in the following panels:"),
              tags$ul(
                tags$li(strong("Growth of the Games"), "- shows how the games has grown in plot and numbers for athletes, events and nations over time since its inception"),
                tags$li(strong("Sports Leaderboard By Medals"), "- shows total count of medals by Sport and Gender for top 10 countries by Gold, Silver and Bronze"),
                tags$li(strong("Medal Tally"), "- shows the medal results table for a selected year won by the countries"),
                tags$li(strong("Map View"), "- shows total medal winnings on a map by countries for all years from 1896 to 2016")
              ),
            )
          ), #end L pic row
          
          tags$hr(style="border-color: black;"),
          
        ) #end page
      ), #end first tab
      
      #second tab
      tabItem(
        tabName = 'tab2',
        #page
        fluidPage(
          #description row
          fluidRow(
            column(
              width = 12,
              h2('Growth of the Games - Athletes, Nations, Events over Time'),
              )
          ),
          
          br(),

          #row of input options
          fluidRow(
            
            #first choice: dropdown menu
            column(
              width = 6,
              radioButtons(
                inputId = 'season_input_col', label = 'Select Category to view:',
                choices = c('Number of Athletes Participited', 'Number of Participating Countries', 'Number of Events Contested'), 
                selected = 'Number of Athletes Participited')
              ),  
            ),
            
            #blank column
            column(width = 8)
          ), #end input options

          tags$hr(style="border-color: black;"),

          #row for viz
          fluidRow(
            column(
              width = 12,
              plotlyOutput('athletes')
            )
          ), #end viz row
          
          br(), 
          tags$hr(style="border-color: black;"),
          
      ), #end second tab
      
      #third tab
      tabItem(
        tabName = 'tab3',
        
        #page
        fluidPage(
          #description row
          fluidRow(
            column(
              width = 12,
              h2('Sports Leaderboard - By Medals (Top 10 Countries)'),
              h4('The leaderboard is shown for each medals viz. Gold, Silver, Bronze.\n',
                ' Top 10 countries are shown in each category, some sports are won by specific countries so only few countries will appear'),
            h4('The gender and sport must match to their respective Olympic event')
            ),
          ),
          
          br(),
          
          #row of input options
          fluidRow(
            
            #first choice: dropdown menu for gender
            column(
              width = 6,
              selectInput("gender", "Select gender",choices = c("Male","Female"),selected = "Male"),
            ),  
            #second choice: dropdown menu for sport
            column(
              width = 6,
              selectInput("event","Select sport",choices = events),
            ),
          ),
          
          #blank column
          column(width = 8)
        ), #end input options
        
        tags$hr(style="border-color: black;"),
        
        #row for medal box
        fluidRow(
          box(title = "Gold medal", 
              plotlyOutput("goldMedal"),width = 4),
          box(title = "Silver medal", 
              plotlyOutput("silverMedal"),width = 4),
          box(title = "Bronze Medal", 
              plotlyOutput("bronzeMedal"),width = 4)
        ), #end medal box row
        br(), 
        column(2,infoBoxOutput("box1", width = 12)),
        br(), 
        tags$hr(style="border-color: black;"),
        
      ), #end third tab
      
      
      #fourth tab
      tabItem(
        tabName = "tab4",
        fluidPage(
          
          fluidRow(
            column(
              width = 12,
              h2('Medal Tally throughout the years'),
            )
          ),
          
          br(),
          
          tags$style(type = "text/css", "
                     .irs-slider {width: 30px; height: 30px; top: 22px;}
                     .irs-grid-text { font-size: 12pt; }",
                     "text-align:center;"),
          
          fluidRow(
            column( width = 6,
                    box(width = 12,
                        sliderInput(inputId = "choice4",
                                    label = "Choose a Year:",
                                    min = 1896,
                                    max = 2016,
                                    value = 2016,
                                    sep = "",
                                    step = 2))
            )
          ), #end row
          
          tags$hr(style="border-color: black;"),
          
          fluidRow(
            box(
              width = 10,
              div(style = 'overflow-x: scroll', dataTableOutput("datatable")) 
            )),
          br(), br()
        ) 
      ), #end fourth tab
      
      #fifth tab: map view
      tabItem(
        tabName = 'tab5',
        #page
        fluidPage(
          #title row
          fluidRow(
            column(
              width = 12,
              h1('Map View - Countries'),
              tags$hr(style="border-color: black;"))
          ),
          
          br(),
          fluidRow(
            box(width = 14, (plotlyOutput(
                "worldmap", height = 700, width = 1250)))
          ),
        ) 
      ) #end fifth tab
    ) # end all tabs
  ) # end dashboard body
) # end UI


###########################################################################
######################### Begin server build ##############################
###########################################################################
server <- function(input, output, session){

    # Growth of the Games
    # Athletes, Nations, Events
    output$athletes <- renderPlotly({
      if(input$season_input_col == "Number of Athletes Participited"){
          info_graph <- ggplot(data_growth, aes(x=Year, y=Athletes, group=Season, color=Season)) +
          geom_point(size=2) +
          ggtitle("Number of Athletes over Time") +
          geom_line() +
          scale_color_manual(values=c("darkorange","darkblue"))
        }
      
      else if(input$season_input_col == "Number of Participating Countries") {
          info_graph <- ggplot(data_growth, aes(x=Year, y=Nations, group=Season, color=Season)) +
          geom_point(size=2) +
          ggtitle("Number of Nations over Time") +
          geom_line() +
          scale_color_manual(values=c("darkorange","darkblue"))
       } 
      
      else if(input$season_input_col == "Number of Events Contested") {
          info_graph <- ggplot(data_growth, aes(x=Year, y=Events, group=Season, color=Season)) +
          geom_point(size=2) +
          ggtitle("Number of Events Contested over Time") +
          geom_line() +
          scale_color_manual(values=c("darkorange","darkblue"))
     }
       ggplotly(info_graph)
    })
    
    # Sports Leaderboard
    # Gold
    output$goldMedal <- renderPlotly({
      df_filtered <- data_sports_year %>% filter(Event == input$event & Sex == input$gender)
      gold_count <- df_filtered %>% filter(Medal == "Gold") %>% count(NOC)
      gold_count_top_10 <- gold_count %>% arrange(desc(n)) %>% slice(1:10)
      fig <- ggplot(gold_count_top_10,aes(x=reorder(NOC,n),y=n))+
        geom_bar(position = "stack",stat="identity",fill="#FFDF00") +
        labs(x="Countries",y="Number of medal") +
        theme_minimal() +
        coord_flip()
      ggplotly(fig)
    })
    
    # Silver
    output$silverMedal <- renderPlotly({
      df_filtered <- data_sports_year %>% filter(Event == input$event & Sex == input$gender)
      silver_count <- df_filtered %>% filter(Medal == "Silver") %>% count(NOC)
      silver_count_top_10 <- silver_count %>% arrange(desc(n)) %>% slice(1:10)
      fig <- ggplot(silver_count_top_10,aes(x=reorder(NOC,n),y=n))+
        geom_bar(position = "stack",stat="identity",fill="#C0C0C0") +
        labs(x="Countries",y="Number of medal") +
        theme_minimal() +
        coord_flip()
      ggplotly(fig)
    })
    
    # Bronze
    output$bronzeMedal <- renderPlotly({
      df_filtered <- data_sports_year %>% filter(Event == input$event & Sex == input$gender)
      bronze_count <- df_filtered %>% filter(Medal == "Bronze") %>% count(NOC)
      bronze_count_top_10 <- bronze_count %>% arrange(desc(n)) %>% slice(1:10)
      fig <- ggplot(bronze_count_top_10,aes(x=reorder(NOC,n),y=n))+
        geom_bar(position = "stack",stat="identity",fill="#cd7f32") +
        labs(x="Countries",y="Number of medal") +
        theme_minimal() +
        coord_flip()
      ggplotly(fig)
    })
    
    # Render Medal Table
    yearData <- reactive({
      chosen_year <- data_sports_year %>% filter(Year == input$choice4)
      
      data_medals <- chosen_year %>% 
        group_by(NOC, Medal) %>% 
        summarise(TotalMedals = n())
      
      medals_yearly <- data_medals  %>%   group_by(NOC) %>% 
        summarise(TotalMedals = sum(TotalMedals))
      
      gold <- data_medals  %>%  filter(Medal == "Gold") %>% count(TotalMedals) %>% rename(Gold = TotalMedals) %>% select(Gold, NOC)%>% unique()
      medals_yearly <- merge(medals_yearly, gold, all = TRUE)
      medals_yearly[is.na(medals_yearly)] <- 0
      silver <- data_medals  %>%  filter(Medal == "Silver") %>% count(TotalMedals) %>% rename(Silver = TotalMedals) %>% select(Silver, NOC)%>% unique()
      medals_yearly <- merge(medals_yearly, silver, all = TRUE)
      medals_yearly[is.na(medals_yearly)] <- 0
      bronze <- data_medals  %>%  filter(Medal == "Bronze") %>% count(TotalMedals) %>% rename(Bronze = TotalMedals) %>% select(Bronze, NOC)%>% unique()
      medals_yearly <- merge(medals_yearly, bronze, all = TRUE)
      medals_yearly[is.na(medals_yearly)] <- 0
      medals_yearly
      })
    output$datatable <- renderDataTable(yearData(), options = list(pageLength = 25))
    
  # Map View
  # Plot: 
  output$worldmap <- renderPlotly({ggplot(world_data, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = Total, label= region)) +
    labs(title = "Participant Countries By Medals (All years)",
         x = NULL, y=NULL) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_colourbar(title="medals")) +
    scale_fill_gradient(low="lightblue",high="darkblue", na.value = "grey")})
  
} #end server


###########################################################################
############################ Run App ######################################
###########################################################################
shinyApp(ui,server)


