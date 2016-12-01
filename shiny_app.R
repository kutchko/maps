rm(list=ls())


library(choroplethr)
library(choroplethrMaps)
library(tidyverse)
library(shiny)
library(lazyeval)


data(county.regions)
election.data.county <- read.csv('county_partisanship_2004_2012.csv')

election.data.wide <- election.data.county %>%
    as_data_frame %>%
    select(year, fips, CountyPartisanship) %>%
    spread(year, CountyPartisanship)



server <- function(input, output) {
    change.in.partisanship <- reactive({
        election.data.wide <- election.data.county %>%
            select(year, fips, CountyPartisanship) %>%
            spread(year, CountyPartisanship)
        data_frame(fips = election.data.wide$fips,
                   Delta = election.data.wide[[as.character(input$years[2])]] -
                       election.data.wide[[as.character(input$years[1])]]) %>%
            inner_join(county.regions, by=c(fips='county.fips.character')) %>%
            select(region, value = Delta)
    })
    
    output$distPlot <- renderPlot({
        change.in.partisanship() %>%
            county_choropleth(num_colors=1) +
            scale_fill_gradient2(low='red', high='blue', limits=c(-100, 100))
    })
}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            sliderInput("years", "Year difference:", min = 2004, max = 2012, value = c(2004, 2012),
                        step = 4, sep = '')
        ),
        mainPanel(plotOutput("distPlot"))
    )
)

shinyApp(ui = ui, server = server)



