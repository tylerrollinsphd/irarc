
##' Generate student_data sample data
##' @docType data
##'
##' @usage data(student_data)
##'
##' @keywords datasets
##'
##' @examples
##'
##' data(student_data)
"student_data"

## # Data Paramenters
## set.seed(10)
## library(dplyr)
## library(devtools)
## student_data<-data.frame(Instructor=sample(c("Professor 1","Professor 2","Professor 3"
##                                         ,"Professor 4","Professor 5","Professor 6"),
##                                       9000,replace=TRUE),
##                  Subject=rep(c("English"), each=9000, replace = TRUE),
##                  Crse_Num=rep(c("101","110", "120","300"), each=2250, replace = TRUE),
##                  Official_Grade=sample(c("A","B","C","D","F","W"),9000,replace=TRUE,
##                                        prob=c(.20,.26,.18,.16,.12,.08)),
##               Gender=sample(c('M', 'F','U','X'), 9000, replace=TRUE, prob=c(.45,.51,.3,.1)),
##               Race=sample(c("African American","Asian","Hispanic/Latino","White"),9000,replace=TRUE))
## student_data$Course<-paste(student_data$Subject,student_data$Crse_Num)
## student_data<-student_data%>%mutate(success=ifelse(Official_Grade=="A"|Official_Grade=="B"|Official_Grade=="C",1,0))
## student_data<-student_data%>%mutate(mastery=ifelse(Official_Grade=="A"|Official_Grade=="B",1,0))
## student_data<-student_data%>%mutate(drop=ifelse(Official_Grade=="D"|Official_Grade=="F"|Official_Grade=="W",1,0))

## # Export data set to ./data
## devtools::use_data(student_data,overwrite=TRUE)

