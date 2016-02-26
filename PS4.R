getwd()

library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

### data of interest:
table<-html_table(temp[2])
str(table)

## table is a list of one element, which is a data.frame
##  extracting the data.frame 
table<-table[[1]]

## fixing names
names(table)<-c("Term_Number", "Year_Elected", "Winner",
                "Winner_Party", "Winner_Pop_Vote_%", "Pop_Vote_%_Margin",
                "Winner_Pop_Vote_Raw", "Winner_Pop_Vote_Margin",
                "Runner-Up", "Runner-Up_Party", "Turnout")
head(table)
## no data in first two rows
## removing first two rows
table<-table[-c(1,2),]

rownames(table)
