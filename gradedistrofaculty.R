##' Generate graph for grade distribution in course by filter characteristic
##'
##' @title Generate graph for course grade distribution by filter characteristic (e.g., grades by race)
##' @param data A data frame containing the variables of interest.
##' @param coursecol is a quoted reference to the column in your \code{data} that contains your course titles (alernatively you could also point to a column such as SUBJECT CODE and see distributions by subject rather than course).
##' @param coursename is a quoted name of a course that you would like to see the grade distribution for (e.g. ENG 101).
##' @param instructorcol is a quoted reference to the column that contains your instructor ID data (e.g., instructor name or instructor employee ID number).
##' @param instructorname is the quoted name of the instructor you want to generate distribution for.
##' @param filtercol is a quoted reference to the column in which your filtering characteristic for the students is found (e.g., Race, Instruction.Mode, Gender, CourseTime, etc.)
##' @param gradecol is a quoted reference to the column in your \code{data} that contains the letter grades
##' @param graphsubtitle is a quoted subtitle that will appear in smaller font just beneath title of the graph (something such as the date range for the data (Fall 16 to Spring 19) might be appropriate) - this is option and will auto fill in the graph if not specified
##' @param limit is an unquoted integer specifying the lower limit of the count required to be included (e.g., limit=10 will remove all groups that do not have a total of at least 10 entries...so if filtering characteristic is Race, it would remove all racial groups with less than 10 students in the course) - this is optional and will default to including all groups if limit is not specified
##' @param legend is a quoted title for your graph legend. If you don't include anything it will remain blank. Example might be "Race/Ethnicity" or "Gender"
##' @param leadingzero is an unquoted integer that allows you to set the number of leading zeros for ID numbers...R will read without zeros so in your .csv an ID might be 0000123 but R may just show 123 so setting \code{leadingzero} to 7 will ensure that the number is padded with zeros until it is 7 digits long to reflect 0000123.
##' @param savesheet is a quoted "Y" for yes or "N" for no, defaulting to "N" if left blank. If "Y" then it will save the graph as a .png in your working directory with a file name leading with the \code{instructorname}. This allows for looping and generating graphs for an entire dept or all courses an instructor teaches.
##' @examples
##' gradedistrofaculty(student_data,
##' "Course","English 101",
##' "Instructor","Professor 1",
##' "Race","Official_Grade",
##' "Grading Distribution for English 101",10,"Race (# students)",0,"N")

##' @export
##' @import dplyr rlang ggplot2 stringr


gradedistrofaculty<-function(data,coursecol,coursename,instructorcol,instructorname,filtercol,gradecol,graphsubtitle,limit,legend,leadingzero,savesheet){
  try({
    if(missing(graphsubtitle)){
    graphsubtitle=Sys.Date()
  } else {
    graphsubtitle<-(graphsubtitle)
  }
  if(missing(limit)){
    limit=0
  } else {
    limit=limit
  }
  if(missing(legend)){
    legend=""
  } else {
    legend<-(legend)
  }
  if(missing(leadingzero)){
    leadingzero=0
  } else {
    leadingzero=leadingzero
  }
  if(missing(savesheet)){
    savesheet="N"
  } else {
    savesheet=savesheet
  }

  empid<-as.character(instructorname)
  coursetitle<-as.character(coursename)
  course<-as.name(coursecol)
  data[,instructorcol]<-str_pad(data[,instructorcol],leadingzero,pad="0")
  empid<-as.name(str_pad(instructorname,leadingzero,pad="0"))
  instructorcol<-as.name(instructorcol)
  coursename<-enquo(coursename)
  instructorname<-as.name(instructorname)
  characteristic<-as.name(filtercol)
  grades<-as.name(gradecol)
  #graphtitle<-(graphtitle)

  instructorname<-as.name(empid)
  subject<-data%>%filter(!!(course)==!!(coursename))%>%filter(!!(instructorcol)==empid)


  percents<-subject%>%group_by(!!characteristic,!!grades)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(Count=sum(n))
  percents<-percents%>%filter(Count>=limit)
  percents$Percent<-round(percents$pct,2)*100
  percents$Count <- with(percents, paste0("(", Count, ")"))
  percents <- mutate(percents, RaceCount=paste(!!characteristic,Count))
  names(percents)[2]<-"grade"

  gdfacplot<<-ggplot(percents, aes(fill=RaceCount, y=Percent, x=grade, width=.8)) +
    geom_bar(position=position_dodge2(preserve="single"), stat="identity", colour="black") +
    ggtitle(paste0("Grade distribution for ",coursetitle," - ",empid), subtitle=graphsubtitle) + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    ylim(0,50)+
    labs(fill=legend,
         x="Grades",
         caption = paste0("Race/Ethnicity groups with less than ",limit," students were not included in this distribution")) +
    theme(plot.caption = element_text(hjust=.5))

  #fname<-paste0(empid," Grading Distribution For - ",coursetitle," - F15-S18.png")
  #ggsave(fname,plot=gdfacplot,width=11, height = 8,units="in")
  gdfacname<<-paste0(empid," Grading Variability For - ",coursetitle," - ",Sys.Date(),".png")
  #ggsave(fname,plot=temp_plot,width=11, height = 8,units="in")
  if(savesheet=="Y"){
    ggsave(gdfacname,plot=gdfacplot,width=11, height = 8,units="in");print("Saved to disk");print(gdfacplot);print("datafile:gdfacplot")
  }
  else{print("Not saved to disk");print(gdfacplot);print("datafile:gdfacplot")
  }
},silent=F)
}

