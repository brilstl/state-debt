## Packages ----
library(shiny)
library(shinyWidgets)
library(ggtext)
library(tidyverse)

## Sources ----

source("data.R")
source("text.R")

## Color scheme ----

COLOR <- c("#FFE0E2", "#FFC2C6", "#FFA4AA", "#FF868E", "#FF6872", "#E8515B", "#BA4149", "#8C3137", "#5D2125", "#2F1113")
COLOR2 <- c("#FFEDE2", "#FFDCC6", "#FFCBAA", "#FFB98E", "#FFA872", "#E8925B", "#BA7549", "#8C5837", "#5D3B25", "#462C1C")

## UI ----

ui <- fluidPage(
        
        includeCSS("stylesheet.css"),
        
        sidebarPanel(width = 5,
                     id = "sidebar",
               h1("'Buying time'"),
               
               br(),
               
               sliderTextInput(
                       inputId = "mySliderText", 
                       label = "Drag the time period you want to look at or press the play button", 
                       grid = TRUE, 
                       force_edges = TRUE,
                       choices = c(unique(tsabble$period)),
                       animate = animationOptions(interval = 1500)),
               
               p("This dashboard was initially motivated by the book of Wolfgang Streeck 'Buying Time: The delayed crisis of democratic capitalism'. 
                 In his book, Streeck argues that governments are 'buying time' through inflation, public debt and private indebtedness which masks any potential issue of societal disruption. 
                 When looking at the absolute amount that countries borrow, the driving forces behind the European Union (e.g. Germany, France and until recently the United Kingdom) are those countries lending by far the largest sum of money. 
                 In this application I invite you to further investigate the state debt of the countries within the European Union."),
               
               br(),
               
               plotOutput("info",
                          width = "100%",
                          height = 280
                          ),
               
               br(),
               
               selectInput("variable", "Choose a way to look at governmental debt lens:",
                           choices = names(tsabble)[c(3,4)],
                           selected = names(tsabble)[c(3)]),
               
               tags$div(
                       tags$a(
                               icon("github", "fa-2x"),
                               href="https://github.com/brilstl/"), 
                       tags$a(
                               icon("envelope", "fa-2x"),
                               href="mailto:tobias.brils@gmail.com")
               ),
               br(),
               
               ),
        
                mainPanel(width = 7,
                        plotOutput("plot1",
                                     height = "calc(100%)",
                                     width = "calc(112%)",
                                   click = "naam_click")
                )
)

## Server ----

server <- function(input, output) {
        
        observe({
                
                showNotification(tags$div(checked=NA, style = " 
        display: flex; 
        line-height: calc(125%);
        width: 50%; 
        font-size: 19px;
        border-radius: 5px; 
        color: #2F1113;
        padding: 10% 0; 
        background-color: #EDECED;
        margin: auto; /* Important */ 
        text-align: center;",
                             text1,
                ),
                                 duration = NULL)
        })
        
        
        selected <- reactive({
                # add clicked
                
                TELL  <- (tsabble %>% distinct(period) %>% nrow(.))
                
                selected_points <<- rbind(selected_points, nearPoints(tsabble, input$naam_click, 
                                                                                 xvar="lat",
                                                                                 yvar="long",
                                                                                 addDist = T))
                
                if (nrow(selected_points) <= TELL) {
                        selected_points
                } else {
                        selected_points <<- selected_points %>%
                                slice(- c(1:TELL))
                }
                
                str(selected_points)
                return(selected_points)
        })
        
        
        output$test <- renderTable({
                
                selected()
        })
        
        
        loco <- reactive ({
                tsabble %>%
                        mutate(period2 = as.character(period)) %>%
                        filter(period2 == input$mySliderText)
        })
        
        
        output$plot1 <- renderPlot({
        
        loco <- loco()
        
        selected <- selected() %>%
                mutate(period2 = as.character(period)) %>%
                filter(period2 == input$mySliderText)
                        
        mp <- euro_map +
                geom_point(data = loco,
                        aes_string(
                        x= "lat", 
                        y= "long",
                        size = glue::glue("`{input$variable}`"),
                        color = glue::glue("`{input$variable}`")),
                        alpha = 0.85) +
                scale_size_continuous(range = c(5,20)) +
                coord_sf(xlim = c(-20, 45), 
                         ylim = c(30, 73), expand = FALSE) +
                geom_curve(data = selected,
                           aes(x =lat, 
                               y = long, 
                               xend = lat + 2, 
                               yend = long + .5), colour='#D33F49', size=1.5,
                           arrow = arrow(length = unit(0.3, "cm"))) +
                annotate("richtext", x = selected$lat + 3.5,
                                 y = selected$long + 2.5,
                                 label = paste0(
                                         "<p style='color:#C9C6C9; font-size:13pt; font-family: 'Baloo Bhaina 2';'>",
                                         "Country: ",
                                         selected$name,
                                         "<br>",
                                         "State Debt:",
                                         "<br>",
                                         "€: ",
                                         format(selected$Euros, big.mark = ",", digits = 0), " bn",
                                         "<br>",
                                         "GDP ratio: ",
                                         "%",(selected$`GDP ratio`*100),
                                         "</p>"),
                                 fontface = "bold",
                         color = "#433B38",
                         fill = "#433B38",
                         alpha = 0.9) +
                        theme_void() +
                        theme(legend.position = "none")
         
         if(input$variable == "Euros"){
                 mp <- mp + scale_color_gradientn(colors = COLOR)

         } else{
                 mp <- mp + scale_color_gradientn(colors = COLOR2)
         }
        
        mp 
        
        }, bg = "transparent", height = 850)
        
        output$info <- renderPlot({
                

                validate(
                        need(nrow(selected()) > 0 , 
                             glue::glue("Click on a bubble within a country to see the debt of a state in {input$variable} within a time series"))
                )
                
                
                ga <- selected() %>% distinct(name) %$% name
                
                kom <- rlang::parse_quosures(glue::glue("`{input$variable}`"))
                
               
                vline <- selected() %>%
                        mutate(period2 = as.character(period)) %>%
                        filter(period2 == input$mySliderText)
               
                
                ts <- tsabble %>%
                       select(name, period,
                               !!!kom) %>%
                        rename(rot = !!names(.[3])) %>%
                        filter(name %in% ga)
               
               
               ts <- ts %>% 
                       ggplot(aes(x = period, y = rot)) +
                       geom_line() +
                       theme_bw() +
                       labs(
                               y = NULL,
                               x = NULL,
                               title = glue::glue("The plot shows the governmental debt of {ga} in {input$variable}.")
                       )
               
               ts <- ts +
               geom_vline(aes_string(xintercept=as.numeric(vline$period[1])),
                                                       linetype=4, colour="#D33F49")

               if(input$variable == "Euros"){
                       ts <- ts + scale_y_continuous(labels = scales::dollar_format(suffix = " bn",
                                                                                    prefix = "€ ")) +
                               geom_line(color = "#D33F49",
                                         size = 1)
               } else{
                       ts <- ts + scale_y_continuous(labels = scales::percent_format()) +
                               geom_line(color = "#FFA064",
                                         size = 1)
               }


               ts +
                       theme(legend.position = "none",
                             panel.background = element_rect(fill = "transparent"),
                             panel.border = element_rect(colour = "transparent", fill=NA, size=5),
                             plot.background = element_rect(fill = "transparent", color = NA),
                             panel.grid.major.x = element_blank(),
                             text = element_text(size = 10),
                             title = element_text(size = 10),
                             panel.grid.minor.x = element_blank(),
                             panel.grid.major.y = element_line(color = "grey85"),
                             panel.grid.minor.y = element_line(color = "transparent"),
                             legend.background = element_rect(fill = "transparent"),
                             legend.box.background = element_rect(fill = "transparent"),
                             axis.text = element_text(size = 12, face = "bold"),
                             axis.ticks = element_blank())

               
        }, bg = "transparent")


}

## Run app ----

shinyApp(ui, server)
