##' @title Generate grading variability for success, mastery, drop/dfw by course and instructor
##' @param data A data frame containing the variables of interest.
##' @param coursecol is a quoted reference to the column in your \code{data} that contains your course titles (alernatively you could also point to a column such as SUBJECT CODE and see distributions by subject rather than course).
##' @param coursename is a quoted name of a course that you would like to see the grade distribution for (e.g. ENG 101).
##' @param instructorcol is a quoted reference to the column that contains your instructor ID data (e.g., instructor name or instructor employee ID number).
##' @param instructorname is the quoted name of the instructor you want to generate distribution for.
##' @param successcol is a quoted reference to the column in which course success is indicated at 1 or 0
##' @param masterycol is a quoted reference to the column in which course mastery (usually grade A or B) is indicated at 1 or 0
##' @param dropcol is a quoted reference to the column in which course drops (either W, of D,F,W, whatever makes sense for your institution) is indicated at 1 or 0
##' @param indicator is an unquoted integer that indicates your positive indicator (1 = yes, usually)
##' @param limit is an unquoted integer specifying the lower limit of the count required to be included (e.g., limit=10 will remove all groups that do not have a total of at least 10 entries...so if filtering characteristic is Race, it would remove all racial groups with less than 10 students in the course) - this is optional and will default to including all groups if limit is not specified
##' @param graphtitle is a quoted title that will appear on the graph - this is optional and will auto fill in the graph title if not specified
##' @param leadingzero is an unquoted integer that allows you to set the number of leading zeros for ID numbers...R will read without zeros so in your .csv an ID might be 0000123 but R may just show 123 so setting \code{leadingzero} to 7 will ensure that the number is padded with zeros until it is 7 digits long to reflect 0000123.
##' @param savesheet is a quoted "Y" for yes or "N" for no, defaulting to "N" if left blank. If "Y" then it will save the graph as a .png in your working directory with a file name leading with the \code{instructorname}. This allows for looping and generating graphs for an entire dept or all courses an instructor teaches.
##' @examples

##' gradevar(student_data,
##' "Course","English 101",
##' "Instructor","Professor 3",
##' "success","mastery","drop",
##' 1,10,"Grading variability",0,"N")

##' @export
##' @import dplyr rlang ggplot2 stringr


gradevar<-function(data,coursecol,coursename,instructorcol,instructorname,successcol,masterycol,dropcol,indicator,limit,graphtitle,leadingzero, savesheet) {
  if(missing(limit)){
    limit=0
  } else {
    limit=limit
  }
  if(missing(indicator)){
    indicator=1
  } else {
    indicator=indicator
  }
  if(missing(graphtitle)){
    graphtitle=paste0("Grading Variability: ",coursename," - ",Sys.Date())
  } else {
    graphtitle=graphtitle
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

  coursetitle<-as.name(coursename)
  course<-as.name(coursecol)
  instructor<-as.name(instructorcol)
  instructorcol<-as.character(instructorcol)
  #empid<-as.name(instructorname)
  coursename<-enquo(coursename)
  success<-as.name(successcol)
  mastery<-as.name(masterycol)
  drop<-as.name(dropcol)
  INSTRUCTOR_EMPLID<-instructorcol
  df2<-data
  df2[,instructorcol]<-str_pad(df2[,instructorcol],leadingzero,pad="0")
  empid<-as.name(str_pad(instructorname,leadingzero,pad="0"))


  df2<-df2%>%filter(!!(course)==!!(coursename))
  percents<-df2%>%group_by(!!instructor,!!success)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(Count=sum(n))%>%droplevels()
  percents<-percents%>%filter(!!(success)==indicator)%>%group_by(!!instructor)
  percents$pct<-round(percents$pct,3)*100
  percents<-percents%>%filter(Count>=limit)

  percents$profcount <- with(percents, paste0("(", Count, ")"))
  percents <- mutate(percents, profcount=paste(!!instructor,profcount))
  percents$rank<-rank(percents$pct, ties.method = "random")
  percents$rank<-str_pad(percents$rank,2,pad="0")
  percents<-percents%>%mutate(rank1=ifelse(!!instructor==empid,paste0(rank," YOU"," (",Count,")"),paste0(rank," (",Count,")")))
  percents$rank<-paste0(percents$rank," (",percents$Count,")")
  str(percents)
  # percents<-percents%>%mutate(rank1=ifelse(INSTRUCTOR_EMPLID==empid,paste0("YOU"," (",Count,")"),rank))


  masterydf<-df2%>%filter(!!(course)==!!(coursename))
  masterypercents<-masterydf%>%group_by(!!instructor,!!mastery)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(Count=sum(n))%>%droplevels()
  masterypercents<-masterypercents%>%filter(!!(mastery)==indicator)%>%group_by(!!instructor)
  masterypercents$Mastery<-round(masterypercents$pct,3)*100
  masterypercents<-masterypercents%>%filter(Count>=limit)

  # masterypercents$profcount <- with(percents, paste0("(", Count, ")"))
  # masterypercents$onlycount<-masterypercents$profcount
  # masterypercents <- mutate(masterypercents, profcount=paste(str_sub(!!instructor,start=-4),"   ",profcount))
  masterypercents<-subset(masterypercents, select=-c(2,3,4,5))
  # #masterypercents<-masterypercents%>%mutate(rank1=ifelse(INSTRUCTOR_EMPLID==empid,paste0("YOU"," (",Count,")"),rank))

  dropdf<-df2%>%filter(!!(course)==!!(coursename))
  droppercents<-dropdf%>%group_by(!!instructor,!!drop)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(Count=sum(n))%>%droplevels()
  droppercents<-droppercents%>%filter(!!(drop)==indicator)%>%group_by(!!instructor)
  droppercents$Drop<-round(droppercents$pct,3)*100
  droppercents<-droppercents%>%filter(Count>=limit)

  # droppercents$profcount <- with(percents, paste0("(", Count, ")"))
  # droppercents$onlycount<-droppercents$profcount
  # droppercents <- mutate(droppercents, profcount=paste(str_sub(!!instructor,start=-4),"   ",profcount))

  droppercents<-subset(droppercents, select=-c(2,3,4,5))

  percents<-left_join(percents,masterypercents, by = instructorcol)
  percents<-left_join(percents,droppercents, by = instructorcol)
  percents$Success<-percents$pct

  newdf<-subset(percents, select=-c(1,2,3,4,5,6,7))
  newdf1<-melt(newdf,id.vars="rank1",variable.name="measure")
  newdf1$Rates<-as.numeric(newdf1$value)
  newdf1$Rates[is.na(newdf1$Rates)]<-0

  newdf1$measure<-factor(newdf1$measure, levels=c("Success","Mastery","Drop"))

  newdf1<-newdf1[with(newdf1,order(measure,Rates)),]

  temp_plot<<-newdf1%>%arrange(measure,Rates)%>%
    ggplot(aes(rank1,Rates,group=measure)) +
    geom_point(aes(color=measure)) +
    geom_line(aes(color=measure),size=1.3)+
    scale_color_manual(values=c(Success="royalblue2", Mastery="palegreen3", Drop="red2"))+
    ylim(0,100)+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          ### position legend to top right
          legend.position = c(1,1),
          legend.justification = c(1,0),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          ###### end position
          axis.text.x=element_text(angle=70, hjust=1,face=ifelse(str_detect(newdf1$rank1,"YOU"),"bold","plain"))) +
    labs(title=paste0("Grading Variability: ",coursetitle," - ",graphtitle),
         subtitle=paste0("See horizontal axis for your ",empid," data"),
         y="Rate (%)",
         x=paste0("Instructors with more than ", limit, " students"))

  fname<<-paste0(empid," Grading Variability For - ",coursetitle," - ",Sys.Date(),".png")
  #ggsave(fname,plot=temp_plot,width=11, height = 8,units="in")
  if(savesheet=="Y"){
    ggsave(fname,plot=temp_plot,width=11, height = 8,units="in");print(temp_plot);print("Saved to disk");print("datafile:temp_plot")
    }
  else{print(temp_plot);print("Not saved to disk");print("datafile:temp_plot")
  }

}




