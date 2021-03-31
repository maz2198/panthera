# marang Mutloatse Panthera Urbanisation dashbaord

library(shiny)
library(knitr)
library(rmarkdown)
library(spData)
library(spdep)
library(data.table)
library(RColorBrewer)
library(stringr)
library(leaflet)
library(shiny)
library(dplyr)
library(readr)  # to read_csv
library(knitr)
library(shiny)
library(ggplot2)
library(ggdark)
library(countrycode)
library(data.table)
library(magrittr)
myfile <- "https://raw.githubusercontent.com/maz2198/panthera/main/urbanization_un.csv?token=AGJPTAZVKHMO4NM4MWXWQ4LANGPB6"
urban_1 <- read_csv(myfile)
setnames(urban_1, c( "X1","index", "year","rank_order","country\ncode","country_or_area", 
                     "city_code","urban_agglomeration" ,"note","population_millions" ,"lng","lat" ))
urban_1$lng<-as.numeric(gsub("c(", "", urban_1$lng, fixed=TRUE))
urban_1$lat<-as.numeric(gsub(")", "", urban_1$lat, fixed=TRUE))
#Adding the continent with country code library
urban_1$continent<- countrycode(sourcevar = urban_1$country_or_area,
                                origin = "country.name",
                                destination = "continent")

# Define UI for application that draws a histogram
shinyApp(
    #####UI 
    ui = shinyUI(fluidPage(
        tags$style('.container-fluid {
                             background-color: #fdd015;
              }'),
        # Application title
        titlePanel("Panthera's Urbanisation Review"),
        mainPanel(
            tabsetPanel(
                # Tab # 1 - Leaflet Map
                tabPanel("Map",leafletOutput("mymap"),
                         sliderInput("sliderA","Choose the year or press play button and see the urbanisation trends", 
                                     min=min(urban_1$year), max=max(urban_1$year), step=5, value=1950,
                                     animate = animationOptions(interval = 1500, loop = TRUE))),
                #Tab #2 - Urbanisation by the cities in a chosen country
                tabPanel("Historical Urbanisation: Country",plotOutput("myplot"),
                         selectInput("country", "Countries:", choices = unique(urban_1$country_or_area))),
                #Tab 3 - Comparison of the urbanisation for multiple user selected cities
                tabPanel("Historical Urbanisation: Cities",plotOutput("myplot1"),
                         helpText("Remember: the higher the rank the more populated the city"),
                         selectInput("cities", label = h3("Cities comparison"),
                                     choices = unique(urban_1$urban_agglomeration),
                                     selected = 1,
                                     width='55%',
                                     multiple = TRUE)),
                #Tab 4. Bar Plots with action button and user interaction
                tabPanel("Comparative Analysis",actionButton('do',"Plot"),plotOutput("myplot2"),
                         selectInput("chosen_year", "Year:", choices = unique(urban_1$year)),
                         selectInput("chosen_metric", "Metric:", choices = c("rank_order","population_millions")),
                         selectInput("chosen_group","Grouping",choices = c("country_or_area","continent"))),
                #Tab 5. Data Table based on sliders of 
                tabPanel("Statistical Overview",
                         helpText("Change the input of the previous tab to see how the stats update"),
                         column(12,DT::dataTableOutput("data_table"))
                         
                )
            )
        )
    )
    ),
    
    server = shinyServer(function(input, output) {
        
        # Reactive function which uses the slider to filter the DB by year
        urban_2<- reactive({urban_1 %>% dplyr::filter(year== as.numeric(input$sliderA)) })
        
        colorpal <- reactive({ colorNumeric("BuPu", urban_2()$population_millions) })
        
        # LeafletMap output
        output$mymap <- renderLeaflet({
            leaflet(urban_2()) %>%
                addProviderTiles("CartoDB.DarkMatter")
        } %>% fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
        )
        
        values<-reactive({c(min(urban_2()$population_millions),max(urban_2()$population_millions))})
        
        # Observe for labels to change as user choice changes
        observe({
            pal <- colorpal()
            leafletProxy("mymap", data = urban_2()) %>%
                clearShapes() %>%
                addCircleMarkers(lng =~lng, lat = ~lat,
                                 color=~pal(urban_2()$population_millions),
                                 popup  =~paste(urban_agglomeration,", " ,country_or_area,
                                                '<br>',
                                                urban_2()$population_millions," Million people")) 
        })
        

        observe({
            proxy <- leafletProxy("mymap", data = urban_2())
            proxy %>% clearControls()
            pal <- colorpal()
            proxy %>% addLegend(position = "bottomright",
                                pal = pal, values = ~population_millions)
            
        }
        )
        
        # Reactive function which uses the selected country 
        
        urban_3<- reactive({urban_1 %>%
                dplyr::filter(country_or_area == input$country) })
        
        output$myplot<- renderPlot({
            ggplot(urban_3(), aes(x=year, y=population_millions,colour=factor(urban_agglomeration))) +
                geom_line(size=1) +
                scale_color_brewer(palette = "Set3") +
                scale_y_continuous(limits=c(0,max(urban_3()$population_millions)))+
                theme(plot.background = element_rect(fill="#061b2d"), panel.background = element_rect(fill="#061b2d"),panel.grid.major=element_line(colour="#061b2d"),
                      panel.grid.minor=element_line(colour="#061b2d"),axis.ticks = element_blank(),
                      axis.title = element_blank(),plot.title = element_text(colour="#ebecec",face="bold"),
                      axis.text.x = element_text(face = "bold", size = 15, angle = 45,colour = "#ffffff"),
                      axis.text.y = element_text(face = "bold", size = 15,colour = "#ffffff"),
                      legend.background = element_rect(fill = "transparent"),legend.key = element_rect(fill = "transparent"),
                      legend.title = element_blank(),legend.text=element_text(color="#ebecec",face="bold"))+
                labs(title = "World's Most Populated Cities: Rank and Forecasting by Country")
        })
        
        #Reactive function for the cities selected
        urban_4<- reactive({urban_1 %>%dplyr::filter(urban_agglomeration %in%  input$cities) })   
        
        output$myplot1<-renderPlot({ 
            ggplot(urban_4(),aes(x=year, y=rank_order,group=factor(urban_agglomeration),colour=factor(urban_agglomeration))) +
                geom_line(size=1) +
                scale_color_brewer(palette = "Set3") +
                theme(plot.background = element_rect(fill="#061b2d"), panel.background = element_rect(fill="#061b2d"),panel.grid.major=element_line(colour="#061b2d"),
                      panel.grid.minor=element_line(colour="#061b2d"),
                      axis.ticks = element_blank(),plot.title = element_text(colour="#ebecec",face="bold"),
                      axis.title = element_blank(),axis.text.x = element_text(face = "bold",size = 12, angle = 45,colour = "#ffffff"),
                      axis.text.y = element_text(face = "bold", size = 12, angle = 45,colour = "#ffffff"),
                      legend.background = element_rect(fill = "transparent"),legend.key = element_rect(fill = "transparent"),
                      legend.title = element_blank(),,legend.text=element_text(color="#ebecec",face="bold"))+
                ggtitle("World's Most Populated Cities: Trend and Forecast by City")
        })
        
        # Reactive function for the year chosen in BarPlots tab
        urban_5<-reactive({urban_1 %>% filter(year == input$chosen_year)})
        
        # Reactive function for the group by variable chosen in BarPlots tab
        urban_6<-reactive({
            urban_5() %>%
                group_by_(input$chosen_group) %>%
                summarize(population_millions = mean(population_millions), rank_order = mean(rank_order))
            
        })
        
        # Observe event so the plot only appears when user clicks on the button
        observeEvent(input$do,{output$myplot2<-renderPlot({ 
            if(input$chosen_group == "continent"){
                ggplot(urban_6(),aes_string(x=input$chosen_group, y=input$chosen_metric)) +
                    geom_bar(stat="identity", fill ="#ebecec")+
                    theme(plot.background = element_rect(fill="#061b2d"),panel.background = element_rect(fill="#061b2d"),panel.grid.major=element_line(colour="#061b2d"),
                          panel.grid.minor=element_line(colour="#061b2d"),axis.ticks = element_blank(),
                          axis.title = element_blank(),axis.text.x = element_text(colour = "#ffffff",face = "bold",size = 12),
                          axis.text.y = element_text(colour = "#ffffff",face = "bold",size = 14),
                          legend.background = element_rect(fill = "black"),legend.key = element_rect(fill = "black"),
                          legend.title = element_blank()) 
            }
            
            else{ggplot(urban_6(),aes_string(x=input$chosen_group, y=input$chosen_metric)) +
                    geom_bar(stat="identity",fill = "#ebecec")+
                    theme(plot.background = element_rect(fill="#061b2d"),panel.background = element_rect(fill="#061b2d"),panel.grid.major=element_line(colour="#061b2d"),
                          panel.grid.minor=element_line(colour="#061b2d"),axis.ticks = element_blank(),
                          axis.title = element_blank(),axis.text.x = element_text(colour = "#ffffff",face = "bold",size = 12, angle=90),
                          axis.text.y = element_text(colour = "#ffffff", face = "bold",size = 12),
                          legend.background = element_rect(fill = "black"),legend.key = element_rect(fill = "black"),
                          legend.title = element_blank())
                
            }
        })
        })
        #output for the data table 
        output$data_table<-DT::renderDataTable(
            urban_6(), filter = 'top', rownames=FALSE
        )
        
    })
    , options = list(height = 750)
)  
