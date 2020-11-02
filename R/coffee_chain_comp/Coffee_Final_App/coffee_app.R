#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# load datasets, etc
library(ggplot2)
library(openintro)
library(tidyverse)
library(plotly)
library(quantmod)
library(ggthemes)
library(reshape2)
library(janitor)
library(gtrendsR)
library(stringr)
library(scales)


setwd('~/PLS397/final_project/coffee_project/Coffee_Final_App')

load(file = "coffee_vars.rda")

##################################### APP #########################################

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Starbucks vs. Dunkin' Donuts Analysis"),

    ########### 
    tabsetPanel(
        tabPanel("USA Map", fluid = TRUE,
                 sidebarLayout(sidebarPanel(selectInput("state",
                                           "Choose a State",
                                           choices = state.abb), tableOutput("usa_table")),
                 mainPanel(plotlyOutput("usa_map")))),
        
        tabPanel("Stock Price Analysis", fluid = TRUE,
                 verticalLayout(plotOutput("stocks_plot"), 
                                wellPanel(sliderInput("year","Year", min = 2011, max = 2020, value = 2020, 
                                                      step = 1, format = "####", sep = "")))),
        
        tabPanel("Google Trends Analysis", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("drink", "Choose a Drink Item",choices = c("Coffee", "Latte", "Americano", "Cappucino", 
                                                                                "Espresso", "Macchiato", "Cold Brew", 
                                                                                "Iced Coffee", "Iced Latte", "Tea", "Chai",
                                                                                "Dark Roast","Medium Roast", "Drinks")), 
                         selectInput("food", "Choose A Food Item", choices = c("Food", "Breakfast", "Lunch", "Sandwich", 
                                                                               "Muffin", "Bagel", "Bakery", "Beyond Meat",
                                                                               "Snacks"))),
                     mainPanel(plotOutput("drink_trends"), plotOutput("food_trends")))))

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$usa_map <- renderPlotly({
        # list with colors orange and dark green to assign below
        map_colors <- c("#FF8000", "#006600")
        
        
        spatial <- map_data
        spatial <- spatial %>%
            plot_ly(
                lat = ~Latitude,
                lon = ~Longitude,
                color = ~Brand, # color by Brand
                colors = map_colors, # assign map_colors list as colors to use
                marker = list(opacity = .4), # set opacity (alpha) to .4 to help show density better
                type = 'scattermapbox',
                hovertext = map_data[,"City.State"]) # use new column with City and State for hovertext
        
        # assign layout and style
        spatial <- spatial %>%
            layout(
                mapbox = list(
                    style = 'open-street-map',
                    zoom =2.5,
                    center = list(lon = -98, lat = 34))) 
        
        return(spatial)
    })
    
    output$usa_table <- renderTable({
        for(i in state.abb){
            if(input$state == i){
                summary_table <- summary_table %>% filter(State %in% c(i, "Total"));
                summary_table$`Dunkin Donuts` <- comma(summary_table$`Dunkin Donuts`);
                summary_table$Starbucks <- comma(summary_table$Starbucks)
            }
        }
        
        # return summary table
        return(summary_table)
        
    })
    
    output$stocks_plot <- renderPlot({
        
        # mask by year seleted by user
        if(input$year == 2011){
            sbux <- sbux %>% filter(year == 2011); dnkn <- dnkn %>% filter(year == 2011)}
        if(input$year == 2012){
            sbux <- sbux %>% filter(year %in% c(2011, 2012)); dnkn <- dnkn %>% filter(year %in% c(2011, 2012))}
        if(input$year == 2013){
            sbux <- sbux %>% filter(year %in% c(2011, 2012, 2013)); dnkn <- dnkn %>% filter(year %in% c(2011, 2012, 2013))}
        if(input$year == 2014){
            sbux <- sbux %>% filter(year %in% c(2011, 2012, 2013, 2014)); dnkn <- dnkn %>% filter(year %in% c(2011, 2012, 2013, 2014))}
        if(input$year == 2015){
            sbux <- sbux %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015)); dnkn <- dnkn %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015))}
        if(input$year == 2016){
            sbux <- sbux %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016)); dnkn <- dnkn %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016))}
        if(input$year == 2017){
            sbux <- sbux %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017)); dnkn <- dnkn %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017))}
        if(input$year == 2018){
            sbux <- sbux %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)); dnkn <- dnkn %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))}
        if(input$year == 2019){
            sbux <- sbux %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)); dnkn <- dnkn %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))}
        if(input$year == 2020){
            sbux <- sbux %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)); dnkn <- dnkn %>% filter(year %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))}
        
        
        # create stocks plot
        stocks <- stocks_plot <- ggplot() + 
            geom_line(sbux, mapping = aes(x = date, y = SBUX.Close, color = "Starbucks")) + 
            geom_line(dnkn, mapping = aes(x = date, y = DNKN.Close, color = "Dunkin' Donuts")) + 
            scale_color_manual(name = "", values = c("Starbucks" = "darkgreen", "Dunkin' Donuts" = "darkorange")) +
            labs(
                x = "Date",
                y = "Price",
                title = "SBUX vs DNKN Stock Price Analysis") + 
            theme_bw() +
            theme(
                text = element_text(family = "Times"),
                axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(hjust = 0.5, size = 16),
                plot.margin = unit(c(1,1,1,1), "cm"),
                legend.position = "top",
                legend.text = element_text(size = 10),
                legend.box.background = element_rect(color = "grey70"),
                panel.border = element_rect(linetype = 'solid', fill = NA, color = "grey80"),
                panel.grid = element_line(color = "grey90", linetype = "solid", lineend = "butt")) + 
            scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") + 
            scale_y_continuous(labels=scales::dollar_format()) 
    
        return(stocks)
        
    })
    
    output$drink_trends <- renderPlot({
        
        # drink list
        drink_selection <- c("Coffee", "Latte", "Americano", "Cappucino", 
                             "Espresso", "Macchiato", "Cold Brew", 
                             "Iced Coffee", "Iced Latte", "Tea", "Chai", 
                             "Hot Chocolate", "Dark Roast","Medium Roast",
                             "Drinks")
        
        # loop if statements to filter dunkin and starbucks dataframes by the drink item selected
        for(i in drink_selection){
            if(input$drink == i){
                sbux_drink <- sbux_drinks_trends %>% filter(item_total == i & month %in% c(2,8));
                dnkn_drink <- dnkn_drinks_trends %>% filter(item_total == i & month %in% c(2,8));
                title = i
            }
        }
        
        # make the plot
        drink_plot <- ggplot() + 
            geom_point(sbux_drink, mapping = aes(x = date, y = hits, color = "Starbucks")) +
            geom_line(sbux_drink, mapping = aes(x = date, y = hits, color = "Starbucks", group = 1)) + 
            geom_point(dnkn_drink, mapping = aes(x = date, y = hits, color = "Dunkin Donuts")) +
            geom_line(dnkn_drink, mapping = aes(x = date, y = hits, color = "Dunkin Donuts", group = 1)) +
            scale_color_manual(name = "", values = c("Starbucks" = "darkgreen", "Dunkin Donuts" = "darkorange")) +
            labs(
                x = "Year",
                y = "Hits",
                title = sbux_drink$item_total
            ) + 
            theme_bw() + 
            theme(
                text = element_text(family = "Times"),
                plot.title = element_text(hjust = 0.5, size = 16),
                plot.margin = unit(c(1,1,1,1), "cm"),
                legend.position = "top",
                legend.text = element_text(size = 10),
                legend.box.background = element_rect(color = "grey70"),
                panel.border = element_rect(linetype = 'solid', fill = NA, color = "grey80"),
                panel.grid = element_line(color = "grey90", linetype = "solid", lineend = "butt"),
            ) +
            scale_y_continuous(n.breaks = 10, expand = c(.25,.25))
        
        return(drink_plot)
        
    })
    
    output$food_trends <- renderPlot({
        
        # food list
        food_selection <- c("Food", "Breakfast", "Lunch", "Sandwich", 
                            "Muffin", "Bagel", "Bakery", "Beyond Meat",
                            "Snacks")
        
        # loop if statements to filter starbucks and dunkin dataframes by the food item selected
        for(i in food_selection){
            if(input$food == i){
                sbux_food <- sbux_food_trends %>% filter(item_total == i & month %in% c(2,8));
                dnkn_food <- dnkn_food_trends %>% filter(item_total == i & month %in% c(2,8))
            }
        }
        
        # make the plot 
        food_plot <- ggplot() + 
            geom_point(sbux_food, mapping = aes(x = date, y = hits, color = "Starbucks")) +
            geom_line(sbux_food, mapping = aes(x = date, y = hits, color = "Starbucks", group = 1)) + 
            geom_point(dnkn_food, mapping = aes(x = date, y = hits, color = "Dunkin Donuts")) +
            geom_line(dnkn_food, mapping = aes(x = date, y = hits, color = "Dunkin Donuts", group = 1)) +
            scale_color_manual(name = "", values = c("Starbucks" = "darkgreen", "Dunkin Donuts" = "darkorange")) +
            labs(
                x = "Year",
                y = "Hits",
                title = sbux_food$item_total
            ) + 
            theme_bw() + 
            theme(
                text = element_text(family = "Times"),
                plot.title = element_text(hjust = 0.5, size = 16),
                plot.margin = unit(c(1,1,1,1), "cm"),
                legend.position = "top",
                legend.text = element_text(size = 10),
                legend.box.background = element_rect(color = "grey70"),
                panel.border = element_rect(linetype = 'solid', fill = NA, color = "grey80"),
                panel.grid = element_line(color = "grey90", linetype = "solid", lineend = "butt"),
            ) +
            scale_y_continuous(n.breaks = 10, expand = c(.25,.25))
        
        return(food_plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
