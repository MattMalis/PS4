getwd()
setwd("/Users/iramalis/Desktop/4625/wd")

library(plyr)
## use package html table, instead of rvest?

#library(htmltab)

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
## get rid of the ugly version...
table<-table[,-(which(names(table)=="Pop_Vote_%_Margin"))]

table$Raw_Margin<-NULL

## now, let's do the same for Winner_Pop_Vote_Margin

## get rid of the commas for all values:
table$Raw_Margin<-sapply(table$Winner_Pop_Vote_Margin, 
                         function(x) x<-gsub(",","",x) )

##  now deal with negative values (first four rows), same as before:
for (i in 1:4){
  table$Raw_Margin[i]<-strsplit(table$Raw_Margin[[i]], split="−")
  table$Raw_Margin[i]<-paste0("-", table$Raw_Margin[[i]][2])
}

## unlist:
table$Raw_Margin<-unlist(table$Raw_Margin)

#from the 5th row to end: all values are duplicated figures...


## character to numeric - drop the zeros
table$Raw_Margin<-as.numeric(table$Raw_Margin)

## now back to character
table$Raw_Margin<-as.character(table$Raw_Margin)

nchar(table$Raw_Margin) ## all values from 5th row to end are duplicated exactly twice...
## use halfChars function from earlier, starting at 5th row
table$Raw_Margin[5:nrow(table)]<-sapply(table$Raw_Margin[5:nrow(table)], halfChars)


## let's make sure all variables are the right class...
str(table)
## remove the '%' and ',' from numeric veriables (manually selected), then cast as numeric
table[,c(1,5,6,10,11,12)]<- apply(table[,c(1,5,6,10,11,12)], 2, function(x){
  x<-gsub("%", "", x)
  x<-gsub(",", "", x)
x<-as.numeric(as.character(x))})

str(table)

###### VISUALIZING #########

## Plot 1: percent margin of popular vote over time, points marked by party

par(mar=c(5,5,3,2))
plot(NULL, 
     xlim=c(min(table$Year_Elected-1),max(table$Year_Elected+1)) ,
     ylim=c(min(table$Percent_Margin-1),max(table$Percent_Margin+1)) ,
     main = "Percent Margin of Popular Vote",
     xlab = "Year Elected",
     ylab = "Percent Margin"
     )
points(table$Percent_Margin[table$Winner_Party=="Whig"]~table$Year_Elected[table$Winner_Party=="Whig"], pch="W", col="green")
points(table$Percent_Margin[table$Winner_Party=="Dem."]~table$Year_Elected[table$Winner_Party=="Dem."], pch="D", col="blue")
points(table$Percent_Margin[table$Winner_Party=="Rep."]~table$Year_Elected[table$Winner_Party=="Rep."], pch="R", col="red")
points(table$Percent_Margin[table$Winner_Party=="D.-R."]~table$Year_Elected[table$Winner_Party=="D.-R."], pch="X")


abline(h=0, lty=3)
abline(v=c(seq(1840,2000,20)), lty=2, col="gray")

legend("bottomright",
       legend=c("Democrat", "Republican", "Whig", "Democrat-Republican"), 
       pch=c("D","R","W","X"),
       col=c("blue", "red","green","black"),
       cex=.8
)


### Plot 2: Closest Runners-Up

## new matrix of winners' and runner-ups' pop vote %
runnersUp<-t(cbind(table[,5], table[,5]-table[,11])) 
colnames(runnersUp)<-(table$Runner_Up)  
runnersUp<-rbind(runnersUp, table$Year_Elected) ## including year in the matrix
rownames(runnersUp)<-c("Runner Up's %", "Winner's %", "")

par(mar=c(3,8,2,3))

barplot(runnersUp[1:2,10:1], beside=T, names.arg=c(table$Runner_Up[10:1]), 
        horiz=T, cex.names=.6, las=2,
        main="Closest Runners-Up",
        xlim = c(0,65)
)
legend(x=52, y=32, legend = c("Runner-Up's \n Pop Vote %", "Winner's \n Pop Vote %"),
       fill= c("gainsboro", "dimgray"), cex=.5, y.intersp=2, bty='n')




## Plot 3: Regression Discontinuity
##  before and after 15th and 19th amendment, in 1869 and 1919, respectively

table$pre15<-table$Year_Elected<1869
table$pre15[1]<-F ## 1824 was extreme outlier, excluding from this plot
table$pre19<-table$Year_Elected>1869 & table$Year_Elected<1919
table$post19<-table$Year_Elected>1919

## regressions over spans of time...
model1<- lm(table$Turnout[table$pre15==T]~table$Year_Elected[table$pre15==T])
model2<- lm(table$Turnout[table$pre19==T]~table$Year_Elected[table$pre19==T])
model3<- lm(table$Turnout[table$post19==T]~table$Year_Elected[table$post19==T])

coeffs<-rbind(coefficients(model1), coefficients(model2), coefficients(model3))

## years of discontinuity points
x1<-1828
x2<-1872
x3<-1920
x4<-2012 

## predicted y values, by different regression models
## pre-15 model:
y1<-(coeffs[1,1] + x1*coeffs[1,2])
y2a<-(coeffs[1,1] + x2*coeffs[1,2])
#between 15-19 model
y2b<-(coeffs[2,1] + x2*coeffs[2,2])
y3a<-(coeffs[2,1] + x3*coeffs[2,2])
#post-19 model
y3b<-(coeffs[3,1] + x3*coeffs[3,2])
y4<-(coeffs[3,1] + x4*coeffs[3,2])

#plotting points, excluding 1824 (outlier)
par(mar=c(5,5,5,5))
plot(table$Turnout[-1]~table$Year_Elected[-1],
     ylab = "Percent Turnout",
     xlab = "Election Year",
     main="Voter Turnout Before and After \n 15th and 19th Amendments")


segments(x1,y1,x2,y2a,lty=6, col="cadetblue", lwd=2)
segments(x2,y2b, x3, y3a, lty=6, col="chocolate", lwd=2)
segments(x3, y3b, x4, y4, lty=6, col="goldenrod", lwd=2)

abline(v=x2, col="purple", lty=3, lwd = 2)
abline(v=x3, col="green", lty=3, lwd = 2)

legend("topright", legend=c(
  "First election after 15th amendement",
  "First election after 19th amendment",
  "Prediction from pre-15th data",
  "Prediction from 15th-19th data",
  "Prediction from post-19th data"),
  lty = c( 3, 3, 6, 6, 6),
  lwd=c(3, 3, 3, 3, 3),
  col = c("purple", "green", "cadetblue", "chocolate", "goldenrod"),
  cex=.7
)
