## NISS Data Vis Competition
library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
library(data.table)
library(gifski)
library(hrbrthemes)
library(formattable)
library(scales)

## import data
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
  mutate(eduGroup = case_when(melt_df$variable==1~"Less than 9th grade",
                              melt_df$variable==2~"Some high school, no\ncompletion",
                              melt_df$variable==3~"High school completion\n(includes equivalency)",
                              melt_df$variable==4~"Some college, no degree",
                              melt_df$variable==5~"Associate's degree",
                              melt_df$variable==6~"Bachelor's degree",
                              melt_df$variable==7~"Master's degree",
                              melt_df$variable==8~"Professional degree",
                              melt_df$variable==9~"Doctor's degree"))

melt_df <- melt_df %>%
  group_by(Year) %>%
  mutate(Overall_Median=median(value))

names <- scales::dollar_format()(melt_df$value) # used in both plots

plot_ly(melt_df, x=~value, y=~eduGroup, frame=~Year, type = 'bar', color=~Sex, text=names, textposition = 'auto',
        insidetextfont = list(size=10, color = 'black')) %>% 
  add_trace(melt_df, x=~Overall_Median, type='scatter', mode='lines', frame=~Year, color="Overall Median Salary",
            line=list(color='black', dash='dash', width=2)) %>%
  layout(yaxis=list(title=" ", categoryorder = "total ascending"), xaxis=list(title="Median Salary (USD)"), 
         title=list(text="<a href='https://nces.ed.gov/programs/digest/d20/tables/dt20_502.20.asp'>Source: Table 502.20; National Center for Education Statistics</a>", x = 0.17), # left align hyperlinked title
         margin=list(t=60), legend=list(x=0.66, y=0.1, bgcolor = "#E2E2E2"))


# single year plot
yearChoice = 2010
melt_df2 <- melt_df %>%
  dplyr::filter(Year==yearChoice) %>% # filter for just the input year
  mutate(perc_diff = ((value - Overall_Median)/Overall_Median)*100)
# melt_df2$perc_diff <- sapply(melt_df2$perc_diff, function(x) ifelse(x>0, paste0("+", round(x,1)), round(x,1))) # add plus signs

annotat <- list(x = 0.83, y = 0.24, # add year in big font
                xref = 'paper', yref = 'paper',
                text = yearChoice,
                xanchor = 'right', yanchor='bottom', 
                showarrow = F, font=list(size=45))

melt_df2 <- melt_df2 %>%
  mutate(Change = ifelse(perc_diff > 0, paste0("+", round(perc_diff,1)), round(perc_diff,1)))

t="Salary"
if(t=="Percent from median"){
  text_type <- ~paste0(melt_df2$Change,"%")
} else {
  text_type <- names
}

plot_ly(melt_df2, x=~value, y=~eduGroup, type = 'bar', color=~Sex, text=text_type, textposition='auto',
        insidetextfont = list(size=10, color = 'black')) %>% 
  add_trace(melt_df2, x=~Overall_Median, type='scatter', mode='lines', color="Overall Median Salary", text="",
            line=list(color='black', dash='dash', width=2)) %>%
  layout(yaxis=list(title=" ", categoryorder = "total ascending"), xaxis=list(title="Median Salary (USD)"), 
         title=list(text="<a href='https://nces.ed.gov/programs/digest/d20/tables/dt20_502.20.asp'>Source: Table 502.20; National Center for Education Statistics</a>", x = 0.17), # left align hyperlinked title
         margin=list(t=60), height=660, legend=list(x=0.66, y=0.1, bgcolor = "#E2E2E2"),
         annotations=annotat)

# add CPI data to compare cost of living to wages
CPI <- read_excel("SeriesReport-20220416123146_87f4cd.xlsx", skip=11)
CPI <- CPI[-c(2:5),]
CPI <- CPI %>% select(-c(Annual, HALF1, HALF2))
CPI$avg = rowMeans(CPI[,c(2,13)])
CPI <- CPI %>% select(c(Year, avg))




