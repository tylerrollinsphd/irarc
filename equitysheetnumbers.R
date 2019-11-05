##' Generate Grading Equity Report Numbers (N) for all faculty and identified professors.
##'
##' @title Generate Grading Equity Report Number for Instructor and All Faculty by course taught
##'
##' @param data A data frame containing the variables of interest. Must include a column for \code{course}, \code{coursename},\code{characteristic},\code{grades}.
##' @param coursecol is a quoted reference to the column in your \code{data} that contains your course titles
##' @param instructorcol is a quoted reference to the column containing the identifiers for your instructors (e.g., instructor name, employee id, etc.)
##' @param instructorid is a quoted reference to the identifier of the instructor you wish to examine (e.g., an instructor name or ID number depending on what you selected as your \code{instructorcol} )
##' @param gradecol is a quoted reference to the column in your \code{data} that contains the letter grades for each student has earned. IMPORTANT!! this must be formatted A,B,C,D,F,W (where W is withdrawl and similar) and other letter grades do not contain +/- grades. Grades must be capitalized.
##' @param racecol is a quoted reference to the column in your \code{data} that contains the race/ethnicity indicator for each student.
##' @param gendercol is a quoted reference to the column in your \code{data} that contains the gender indicators.
##' @param limit is an unquoted integer specifying the lower limit of the count required to be included (e.g., limit=10 will replace data for any group with less than 10 students with "---". This is optional and will default to include all groups if limit is not specified. Use this for masking groups with small N.
##' @param footer is an unquoted integer of 1 if you would like there to be a footer that shows "Equity Report for: 'PROFESSOR NAME' - Generated: 'DATE'. If you don't want the footer, leave this blank/don't specify.
##' @param leadingzero is an unquoted integer that allows you to set the number of leading zeros for ID numbers...R will read without zeros so in your .csv an ID might be 0000123 but R may just show 123 so setting \code{leadingzero} to 7 will ensure that the number is padded with zeros until it is 7 digits long to reflect 0000123.
##' @param savesheet is a quoted "Y" for yes or "N" for no, defaulting to "N" if left blank. If "Y" then it will save the graph as a .png in your working directory with a file name leading with the \code{instructorname}. This allows for looping and generating graphs for an entire dept or all courses an instructor teaches.

##' @examples
##' equitysheet(testdata,"Course","Instructor","Professor 1","Official_Grade","Ethnicity","Gender",0,1)

##' @export
##' @import dplyr rlang ggplot2 zip reshape2 flextable tidyr stringr



equitysheetnumbers<-function(data,coursecol,instructorcol,instructorid, gradecol,racecol,gendercol,limit,footer,leadingzero,savesheet){
  try({
    if(missing(limit)){
      limit=0
    } else {
      limit=limit
    }
    if(missing(footer)){
      footer=0
    } else {
      footer=footer
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
    course<-as.name(coursecol)
    data[,instructorcol]<-str_pad(data[,instructorcol],leadingzero,pad="0")
    empid<-as.name(str_pad(instructorid,leadingzero,pad="0"))
    instructorcol<-as.name(instructorcol)
    grade<-as.name(gradecol)
    race<-as.name(racecol)
    gender<-as.name(gendercol)
    instructor<-enquo(instructorid)
    instrid<-instructor

    data<-data%>%select(c(!!(coursecol),!!(instructorcol),!!(gradecol), !!(racecol),!!(gendercol)))

    data<-data%>%rename(Instructor=!!(instructorcol))
    data<-data%>%rename(Course=!!(coursecol))
    data<-data%>%rename(OFFICIAL_GRADE=!!(gradecol))
    data<-data%>%rename(Gender=!!(gendercol))
    data<-data%>%rename(Ethnicity=!!(racecol))

    data<-data[c("Course","Instructor","OFFICIAL_GRADE","Ethnicity","Gender")]
    instr<-data%>%filter(Instructor==!!(instructorid))
    instr$Instructor<-as.character(instr$Instructor)
    #instr$Instructor=substr(instr$Instructor,1,nchar(instr$Instructor)-12)
    instr$Instructor<-as.factor(instr$Instructor)
    instrname<-instr%>%group_by(Instructor)%>%count%>%droplevels
    name<-paste0("Equity Report for: ",instrname$Instructor,". ", " - Generated: ", Sys.Date())
    name<-ifelse(footer==1,name,"")

    courselist<-instr%>%group_by(Course)%>%count
    courselist<-subset(courselist,select=-c(n))
    courselist<-unname(courselist)
    courselist1<-courselist
    courselist1<-unlist(courselist1, recursive = FALSE)
    allfaculty<-data%>%filter(Course %in% courselist1)
    instr<-instr%>%mutate(success=ifelse(OFFICIAL_GRADE == "A" |OFFICIAL_GRADE == "B" |OFFICIAL_GRADE == "C"|OFFICIAL_GRADE == "P",1,0))
    instr<-instr%>%mutate(mastery=ifelse(OFFICIAL_GRADE == "A" |OFFICIAL_GRADE == "B",1,0))
    instr<-instr%>%mutate(drop=ifelse(OFFICIAL_GRADE == "W",1,0))
    instr<-instr%>%mutate(onecount=1)

    allfaculty<-allfaculty%>%mutate(success=ifelse(OFFICIAL_GRADE == "A" |OFFICIAL_GRADE == "B" |OFFICIAL_GRADE == "C"|OFFICIAL_GRADE == "P",1,0))
    allfaculty<-allfaculty%>%mutate(mastery=ifelse(OFFICIAL_GRADE == "A" |OFFICIAL_GRADE == "B",1,0))
    allfaculty<-allfaculty%>%mutate(drop=ifelse(OFFICIAL_GRADE == "W",1,0))
    allfaculty<-allfaculty%>%mutate(onecount=1)


    instr_coursetotal<-instr%>%group_by(Course)%>%count

    instr_coursetotal$Instructor<-instructorid
    allfaculty_coursetotal<-allfaculty%>%group_by(Course)%>%count
    allfaculty_coursetotal$Instructor<-"All Faculty"

    allfaculty$Instructor="All Faculty"

    coursedata<-rbind(instr,allfaculty)

    enrollment<-coursedata%>%group_by(Course, Instructor)%>%count

    successpct<-coursedata%>%group_by(Course,Instructor)%>%summarise(n=n())

    masterypct<-coursedata%>%group_by(Course,Instructor)%>%summarise(n=n())

    droppct<-coursedata%>%group_by(Course,Instructor)%>%summarise(n=n())

    coursedata<-coursedata%>%group_by(Instructor, Course,Gender)%>%mutate(gendercount=n())

    gendersuccesspct<-coursedata%>%group_by(Course,Instructor,Gender)%>%summarise(n=n())

    gendermasterypct<-coursedata%>%group_by(Course,Instructor,Gender)%>%summarise(n=n())

    genderdroppct<-coursedata%>%group_by(Course,Instructor,Gender)%>%summarise(n=n())


    ethnicitysuccesspct<-coursedata%>%group_by(Course,Instructor,Ethnicity)%>%summarise(n=n())

    ethnicitymasterypct<-coursedata%>%group_by(Course,Instructor,Ethnicity)%>%summarise(n=n())


    ethnicitydroppct<-coursedata%>%group_by(Course,Instructor,Ethnicity)%>%summarise(n=n())

    join<-left_join(ethnicitysuccesspct,ethnicitydroppct, by = c("Course","Instructor","Ethnicity"))
    join<-left_join(join,ethnicitymasterypct,by = c("Course","Instructor","Ethnicity"))
    cleanjoin<-subset(join,select=c(Course,Instructor,Ethnicity,n.x))
    cleanjoin<-cleanjoin%>%complete(Ethnicity) %>% group_by(Course,Ethnicity)


    genderjoin<-left_join(gendersuccesspct,genderdroppct, by = c("Course","Instructor","Gender"))
    genderjoin<-left_join(genderjoin,gendermasterypct,by = c("Course","Instructor","Gender"))
    gendercleanjoin<-subset(genderjoin,select=c(Course,Instructor,Gender,n.y))
    gendercleanjoin<-gendercleanjoin%>%complete(Gender) %>% group_by(Course,Gender)


    ethenroll<-right_join(enrollment,cleanjoin, by = c("Course","Instructor"))
    finalethenroll<-ethenroll%>%spread(key=Ethnicity, value="n.x")
    genenroll<-right_join(enrollment,gendercleanjoin,by = c("Course","Instructor"))
    finalgenenroll<-genenroll%>%spread(key=Gender, value="n.y")

    nfinaltable<-rbind(finalgenenroll, finalethenroll)

    lastjoin<-right_join(finalgenenroll,finalethenroll, by = c("Course","Instructor"))

    nfinaltable<-lastjoin


    colnames(nfinaltable)[3]<-"Enrollment"
    colnames(nfinaltable)[4]<-"F"
    colnames(nfinaltable)[5]<-"M"
    colnames(nfinaltable)[6]<-"U"
    colnames(nfinaltable)[7]<-"X"

    nfinaltable<-nfinaltable[,-8]

    nfinaltable<-nfinaltable%>%arrange(Course)

    ntable1<-nfinaltable%>%group_by(Course)%>%flextable()

    nmytable<-flextable(nfinaltable)
    nmytable<-theme_vanilla(nmytable)
    nmyft<-merge_v(nmytable, j = c("Course"))
    nmyft<-autofit(nmyft)


    nmyft<<-align(nmyft,align="center", part="all")
    ofile1<<-paste0(instructorid," - Equity Report Numbers - ",Sys.Date(),".html")
    outfile1<<-(ofile1)


    if(savesheet=="Y"){save_as_html(nmyft,outfile1);print("Saved to disk");print("datafile:nmyft");print(nmyft)
    }
    else {print("Not saved to disk");print(nmyft)
    }
  }, silent = F)
}

