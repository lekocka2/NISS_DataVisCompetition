library(tidyverse)
library(readxl)
library(plotly)
library(data.table)
library(gganimate)
library(gifski)
library(shiny)
library(forcats)
library(formattable)

## import data and clean
data <- read_excel("tabn502.20.xls", skip=1)
data <- data[9:52,]
subbed <- data[-c(2,3,5,7,9,11,13,14,15,17,19,21,23)] # take out cols with standard error and totals
subbed <- subbed[-c(22,23),] # take out rows with sex and missing data in middle
subbed <- subbed %>%
  mutate(Sex=ifelse(row_number()>=1 & row_number()<22, "Male", "Female"))
colNames <- c("Year","1","2","3","4","5","6","7","8","9","Sex")
colnames(subbed) <- colNames

melted_all <- melt(subbed, id=c("Year","Sex"))
melted_all$value <- as.numeric(melted_all$value)
# replace numbers with educational levels
melt_df <- as.data.frame(melted_all) # change to df to store the <br>
melt_df <- melt_df %>%
  mutate(eduGroup = case_when(melt_df$variable==1~"Less than<br>9th grade",
                              melt_df$variable==2~"Some high school,<br>no completion",
                              melt_df$variable==3~"High school completion<br>(includes equivalency)",
                              melt_df$variable==4~"Some college,<br>no degree",
                              melt_df$variable==5~"Associate's degree",
                              melt_df$variable==6~"Bachelor's degree",
                              melt_df$variable==7~"Master's degree",
                              melt_df$variable==8~"Professional degree",
                              melt_df$variable==9~"Doctor's degree"))
melt_df$eduGroup <- forcats::fct_relabel(melt_df$eduGroup, function(x) str_wrap(x, 10))
melt_df <- melt_df %>%
  group_by(Year) %>%
  mutate(Overall_Median=median(value))



# ui start
ui <- fluidPage(
  titlePanel(span("HOW DOES", style="color:black; font-size:30px",
                  span("MEDIAN SALARY", style="color:#F4832E; font-size:40px", br(),
                                 span("DIFFER BY", style="color:black; font-size:30px", 
                                      span("EDUCATION", style="color:#77B994; font-size:40px",
                                           span("AND", style="color:black; font-size:30px",
                                                span("SEX?", style="color:#77B994; font-size:40px", 
                                                     span("(1995-2019)", style="color:black; font-size:22px")))))))
  ),
  
  sidebarLayout(
    sidebarPanel(
                 radioButtons(inputId='radio', label='Select choice:', 
                              c("Full range animation"='fullRange', "Single year"='singleYear'), selected='fullRange'),
                 #selectInput("years", "Select year:", c(1995,2000:2019), NULL, width=120),
                 width=3,
                 
                 conditionalPanel(
                   condition="input.radio=='singleYear'",
                   selectInput("years", "Select year:", c(1995,2000:2019), NULL, width=120)
                 )
    ),
    
    mainPanel(
      uiOutput("out")
    )
    
  )
)

# server start
server <- function(input, output) {
  
  #save year choice
  yearChoice <- reactive({
    yr <- as.character(input$years)
  })
  
  #full range plot
  output$anim <- renderPlotly({
    names <- scales::dollar_format()(melt_df$value)
    
    plot_ly(melt_df, x=~value, y=~eduGroup, frame=~Year, type = 'bar', color=~Sex, text=names, textposition = 'auto',
            insidetextfont = list(size=10, color = 'black')) %>% 
      add_trace(melt_df, x=~Overall_Median, type='scatter', mode='lines', frame=~Year, color="Overall Median Salary", text="",
                line=list(color='black', dash='dash', width=2)) %>%
      layout(yaxis=list(title=" "), xaxis=list(title="Median Salary (USD)"), 
             title=list(text="<a href='https://nces.ed.gov/programs/digest/d20/tables/dt20_502.20.asp'>Source: Table 502.20; National Center for Education Statistics</a>", x = 0.17), # left align hyperlinked title
        margin=list(t=60), height=660, legend=list(x=0.66, y=0.1, bgcolor = "#E2E2E2"))
  })
  
  #single year plot
  output$year <- renderPlotly({
    melt_df2 <- melt_df %>%
      dplyr::filter(Year==yearChoice())
    
    names <- scales::dollar_format()(melt_df2$value)
    annotat <- list(x = 0.83, y = 0.24,
                 xref = 'paper', yref = 'paper',
                 text = yearChoice(),
                 xanchor = 'right', yanchor='bottom', 
                 showarrow = F, font=list(size=45))
    
    plot_ly(melt_df2, x=~value, y=~eduGroup, type = 'bar', color=~Sex, text=names, textposition = 'auto',
            insidetextfont = list(size=10, color = 'black')) %>% 
      add_trace(melt_df2, x=~Overall_Median, type='scatter', mode='lines', color="Overall Median Salary", text="",
                line=list(color='black', dash='dash', width=2)) %>%
      layout(yaxis=list(title=" "), xaxis=list(title="Median Salary (USD)"), 
             title=list(text="<a href='https://nces.ed.gov/programs/digest/d20/tables/dt20_502.20.asp'>Source: Table 502.20; National Center for Education Statistics</a>", x = 0.17), # left align hyperlinked title
             margin=list(t=60), height=660, legend=list(x=0.66, y=0.1, bgcolor = "#E2E2E2"),
             annotations=annotat)
  })
  
  #choose a plot to render based on radio input
  output$out <- renderUI({
    switch(input$radio, 
           "fullRange"=plotlyOutput("anim"),
           "singleYear"=plotlyOutput("year"))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

