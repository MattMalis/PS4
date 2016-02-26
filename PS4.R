getwd()

library(plyr)

library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

###### SCRAPING THE DATA #########

### data of interest:
table<-html_table(temp[2])
str(table)

## table is a list of one element, which is a data.frame
##  extracting the data.frame 
table<-table[[1]]

## fixing names
names(table)<-c("Term_Number", "Year_Elected", "Winner",
                "Winner_Party", "Winner_Pop_Vote_Percent", "Pop_Vote_%_Margin",
                "Winner_Pop_Vote_Raw", "Winner_Pop_Vote_Margin",
                "Runner_Up", "Runner_Up_Party", "Turnout")
head(table)
## no data in first two rows
## removing first two rows
table<-table[-c(1,2),]

## row names mean nothing. getting rid of them
rownames(table)<-NULL ## I guess that just reset them to start at 1... 

str(table)

head(table$Winner)
## all the names have been copied twice...
## ... but, they all follow a pattern: Last, FirstFirst Last
##    or: Last, First MiddleFirst Middle Last
## The ceiling of half of the number of characters in these hideous strings
##    will be the correct number of characters in the president's names
nchar(table$Winner[1:5])
substr(table$Winner[1:5], 1, ceiling(nchar(table$Winner[1:5])/2))

## creating a function...
halfChars<-function(x) substring(x, 1, ceiling((nchar(x))/2))
halfChars("Let's test this out")

## applying the function to Winner
table$Winner<-sapply(table$Winner, halfChars)
## applying the function to Runner-Up
table$Runner_Up<-sapply(table$Runner_Up, halfChars)

head(table)

## Problem: for both "Pop_Vote_%_Margin" and "Winner_Pop_Vote_Margin" mashed up two different figures...
##    we can separate out the figures for Pop_Vote_%_Margin

table$Percent_Margin<-strsplit(table$`Pop_Vote_%_Margin`, split = "%")

## deal with the first four values: figures combined by "−" instead of "%"
for (i in 1:4){
  table$Percent_Margin[i]<-strsplit(table$Percent_Margin[[i]], split="−")
  table$Percent_Margin[i]<-paste0("-", table$Percent_Margin[[i]][2])
}

## remaining values in "Percent_Margin" variable:
##    all lists, either with one element, or with two identical elements
for (i in 1:nrow(table)){
  table$Percent_Margin[i]<-table$Percent_Margin[[i]][1]
}
table$Percent_Margin<-unlist(table$Percent_Margin)

## now we have a clean Percent_Margin variable
table$Percent_Margin
table<-table[,-(which(names(table)=="Pop_Vote_%_Margin"))]

## NOTE: won't be able to use Winner_Pop_Vote_Margin

## let's make sure all variables are the right class...
str(table)
table[,c(1,5,6,10,11)]<- apply(table[,c(1,5,6,10,11)], 2, function(x){
  x<-gsub("%", "", x)
  x<-gsub(",", "", x)
x<-as.numeric(as.character(x))})

str(table)

###### VISUALIZING #########


