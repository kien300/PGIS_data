library(tidyverse)
library(readxl)
library(qdap)
library(shiny)
#library(data.table)
library(scales)
#library(waffle)
#library(ggExtra)

#load dataset from Dropbox----
main <- read.csv('https://www.dropbox.com/s/a6jtuhe3sinc8wc/main.csv?raw=1')

#Tidying dataset----
#Visuals Setting
theme_set(theme_bw())
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", 
                  "#DDCC77", "#CC6677", "#882255", "#AA4499")

#tidy names
d <- c('Number','Country','Interviewer','Location','Commune','Village','HHID','Full_Name',
       'Sex','Age','Community','Education','Number_of_Adults', 'Number_of_Non_Adults',
       'Number_of_Fields','Number_of_Annuals', 'Number_of_Perennials', 
       'Number_of_Annual_Perennials','Migration_Origin','Years_of_Migration',
       'Off_Farm_Employment','Distribution_Labor','Livestock?','pigeon','cows','goats',
       'buffaloes','pig','chicken','ducks','fish','horses')
names(main)[1:32] <- d

b <- c('pigeon','cows','goats','buffaloes','pig','chicken','ducks','fish','horses')
main[b] <- lapply(main[b], 
                  function(x) as.numeric(as.character(x))) # change data types to numeric

#tidy livestock
livestock <- main %>% select(Country,HHID,Sex,Community,Education,Migration_Origin,
                             Off_Farm_Employment,Distribution_Labor,
                             pigeon,cows,goats,buffaloes,pig,chicken,
                             ducks,fish,horses) #choose livestock columns

livestock <- livestock %>% gather(livestock, number, 9:17) #new view
livestock <- livestock[!(is.na(livestock$number)),] #remove all NA observations
livestock <- livestock[livestock$number!=0,] #remove all 0s values

#tidy Demographics
df3 <- main %>% select(Country,Sex,Community,Education,Distribution_Labor,
                       Age,Number_of_Adults,Number_of_Non_Adults,Number_of_Fields,
                       Number_of_Annuals,Number_of_Perennials,Number_of_Annual_Perennials,
                       Years_of_Migration)

df3$Years_of_Migration <- as.character(df3$Years_of_Migration) #because it was factor
df3$Years_of_Migration <- as.integer(df3$Years_of_Migration)

ctgr <- c('Country','Sex','Community','Education','Distribution_Labor')
cont <- c('Age','Number_of_Adults','Number_of_Fields','Number_of_Annuals',
          'Number_of_Perennials','Number_of_Annual_Perennials','Years_of_Migration')

#define UI----
ui <- fluidPage(
  titlePanel("Hands & Minds"),
  
  navbarPage("", selected = 'Demographics',
             tabPanel("Demographics",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Select desired variables"),
                          
                          selectInput('xcol', 'X Variable', names(df3)),
                          selectInput('ycol', 'Y Variable', names(df3),
                                      selected = names(df3)[[3]]),
                          selectInput('color', 'Color', c('None', names(df3))),
                          
                          checkboxInput('jitter', 'Jitter'),
                          checkboxInput('smooth', 'Smooth'),
                          checkboxInput('flip', 'Flip'),
                          
                          selectInput('facet_row', 'Facet Row', c(None='.', names(df3))),
                          selectInput('facet_col', 'Facet Column', c(None='.', names(df3)))
                        ),
                        
                        mainPanel(
                          plotOutput('scatter')))),
             
             tabPanel("Livestock",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('facet_row2', 'Facet Row', c(None='.', names(livestock))),
                          selectInput('facet_col2', 'Facet Column', c(None='.', names(livestock)))
                        ),
                        
                        mainPanel(
                          plotOutput('bar')
                        ))
             )
  )
)

# Define server function ----
server <- function(input, output){
  
  #Reactivity
  selectedData <- reactive({df3})
  
  output$scatter <- renderPlot({
    if (input$xcol %in% ctgr & input$ycol %in% cont)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol,y=input$ycol)) + 
        geom_boxplot()
    else if (input$xcol %in% cont & input$ycol %in% ctgr)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol,y=input$ycol)) + 
        geom_boxplot() + coord_flip() #why this doesnt work???
    else if (input$xcol %in% ctgr & input$ycol %in% ctgr)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol)) + 
        geom_bar(width = .5, fill='#CC79A7') +
        geom_text(stat='count',aes(label=..count..),vjust=-0.6) +
        labs(x='Values')
    else if (input$xcol %in% cont & input$ycol %in% cont & input$xcol==input$ycol)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol)) + 
        geom_density()
    else
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol,y=input$ycol)) + 
        geom_point()
    
    if (input$color != 'None')
      g3 <- g3 + aes_string(color=input$color)
    
    facets1 <- paste(input$facet_row, '~', input$facet_col)
    if (facets1 != '. ~ .')
      g3 <- g3 + facet_grid(facets1)
    
    if (input$flip)
      g3 <- g3 + coord_flip()
    
    if (input$jitter)
      g3 <- g3 + geom_jitter()
    
    if (input$smooth)
      g3 <- g3 + geom_smooth(method = 'loess')
    
    print(g3)
  }, height = 600)
  
  output$bar <- renderPlot({
    g4 <- ggplot(livestock, aes(HHID,number,fill=livestock)) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +   #no tick marks
      labs(x = 'Households', y = 'Percentage') + 
      geom_bar(position = 'fill', stat = 'identity') +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(values=tol9qualitative)
    
    facets2 <- paste(input$facet_row2, '~', input$facet_col2)
    if (facets2 != '. ~ .')
      g4 <- g4 + facet_grid(facets2, space="free_x", scales="free_x")
    
    print(g4)
  }, height = 600)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)