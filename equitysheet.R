##' Generate Grading Equity Report for Success (A,B,C grades), Mastery (A,B grades), and Drops (W grades) for identified instructor and for all instructors who have taught each course that the identified instructor has taught.
##'
##' @title Generate Grading Equity Report for Instructor and All Faculty by course taught
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
##'
##' @examples
##' equitysheet(student_data,
##' "Course",
##' "Instructor",
##' "Professor 1",
##' "Official_Grade",
##' "Race",
##' "Gender",
##' 10,1,"N")

##' @export
##' @import dplyr rlang extdplyr ggplot2 zip reshape2 flextable tidyr stringr

equitysheet<-function(data,coursecol,instructorcol,instructorid, gradecol,racecol,gendercol,limit,footer,leadingzero,savesheet){
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
#instr$Instructor<-as.character(paste0(" W",instrid))
allfaculty<-allfaculty%>%mutate(success=ifelse(OFFICIAL_GRADE == "A" |OFFICIAL_GRADE == "B" |OFFICIAL_GRADE == "C"|OFFICIAL_GRADE == "P",1,0))
allfaculty<-allfaculty%>%mutate(mastery=ifelse(OFFICIAL_GRADE == "A" |OFFICIAL_GRADE == "B",1,0))
allfaculty<-allfaculty%>%mutate(drop=ifelse(OFFICIAL_GRADE == "W",1,0))
allfaculty<-allfaculty%>%mutate(onecount=1)


instr_coursetotal<-instr%>%group_by(Course)%>%count
#instr_coursetotal$Instructor<-instrname$Instructor
instr_coursetotal$Instructor<-instructorid
allfaculty_coursetotal<-allfaculty%>%group_by(Course)%>%count
allfaculty_coursetotal$Instructor<-"All Faculty"


allfaculty$Instructor="All Faculty"


coursedata<-rbind(instr,allfaculty)


enrollment<-coursedata%>%group_by(Course, Instructor)%>%count



successpct<-pct_routine(coursedata,Course,Instructor,success)
successpct<-successpct%>%filter(success==1)%>%group_by(Course,Instructor)
successpct$pct<-round(successpct$pct,2)*100
successpct$overallsuccesspct<-paste0(successpct$pct, "%")


masterypct<-pct_routine(coursedata,Course,Instructor,mastery)
masterypct<-masterypct%>%filter(mastery==1)%>%group_by(Course,Instructor)
masterypct$pct<-round(masterypct$pct,2)*100
masterypct$overallmasterpct<-paste0(masterypct$pct, "%")


droppct<-pct_routine(coursedata,Course,Instructor,drop)
droppct<-droppct%>%filter(drop==1)%>%group_by(Course,Instructor)
droppct$pct<-round(droppct$pct,2)*100
droppct$overalldroppct<-paste0(droppct$pct, "%")


coursedata<-coursedata%>%group_by(Instructor, Course,Gender)%>%mutate(gendercount=n())


gendersuccesspct<-coursedata%>%group_by(Course,Instructor,Gender,success)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(gendertot=sum(n))
gendersuccesspct<-gendersuccesspct%>%filter(success==1)%>%group_by(Course,Instructor)
gendersuccesspct$pct<-round(gendersuccesspct$pct,2)*100
gendersuccesspct$pct<-paste0(gendersuccesspct$pct, "%")
gendersuccesspct$gendersuccesspct<-ifelse(gendersuccesspct$gendertot<10,"---",gendersuccesspct$pct)



gendermasterypct<-coursedata%>%group_by(Course,Instructor,Gender,mastery)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(gendertot=sum(n))
gendermasterypct<-gendermasterypct%>%filter(mastery==1)%>%group_by(Course,Instructor)
gendermasterypct$pct<-round(gendermasterypct$pct,2)*100
gendermasterypct$pct<-paste0(gendermasterypct$pct, "%")
gendermasterypct$gendermasterypct<-ifelse(gendermasterypct$gendertot<10,"---",gendermasterypct$pct)

genderdroppct<-coursedata%>%group_by(Course,Instructor,Gender,drop)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(gendertot=sum(n))
genderdroppct<-genderdroppct%>%filter(drop==1)%>%group_by(Course,Instructor)
genderdroppct$pct<-round(genderdroppct$pct,2)*100
genderdroppct$pct<-paste0(genderdroppct$pct, "%")
genderdroppct$genderdroppct<-ifelse(genderdroppct$gendertot<10,"---",genderdroppct$pct)

ethnicitysuccesspct<-coursedata%>%group_by(Course,Instructor,Ethnicity,success)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(ethnicitytot=sum(n))
ethnicitysuccesspct<-ethnicitysuccesspct%>%filter(success==1)%>%group_by(Course,Instructor)
ethnicitysuccesspct$pct<-round(ethnicitysuccesspct$pct,2)*100
ethnicitysuccesspct$pct<-paste0(ethnicitysuccesspct$pct, "%")
ethnicitysuccesspct$ethnicitysuccesspct<-ifelse(ethnicitysuccesspct$ethnicitytot<10,"---",ethnicitysuccesspct$pct)

ethnicitymasterypct<-coursedata%>%group_by(Course,Instructor,Ethnicity,mastery)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(ethnicitytot=sum(n))
ethnicitymasterypct<-ethnicitymasterypct%>%filter(mastery==1)%>%group_by(Course,Instructor)
ethnicitymasterypct$pct<-round(ethnicitymasterypct$pct,2)*100

ethnicitymasterypct$pct<-paste0(ethnicitymasterypct$pct, "%")
ethnicitymasterypct$ethnicitymasterpct<-ifelse(ethnicitymasterypct$ethnicitytot<10,"---",ethnicitymasterypct$pct)

ethnicitydroppct<-coursedata%>%group_by(Course,Instructor,Ethnicity,drop)%>%summarise(n=n())%>%mutate(pct=n/sum(n))%>%mutate(ethnicitytot=sum(n))
ethnicitydroppct<-ethnicitydroppct%>%filter(drop==1)%>%group_by(Ethnicity,Course)
ethnicitydroppct$pct<-round(ethnicitydroppct$pct,2)*100

ethnicitydroppct$pct<-paste0(ethnicitydroppct$pct, "%")
ethnicitydroppct$ethnicitydroppct<-ifelse(ethnicitydroppct$ethnicitytot<10,"---",ethnicitydroppct$pct)


join<-left_join(ethnicitysuccesspct,ethnicitydroppct, by = c("Course","Instructor","Ethnicity"))
join<-left_join(join,ethnicitymasterypct,by = c("Course","Instructor","Ethnicity"))
cleanjoin<-subset(join,select=c(Course,Instructor,Ethnicity,ethnicitymasterpct,ethnicitysuccesspct,ethnicitydroppct))
cleanjoin<-cleanjoin%>%complete(Ethnicity) %>% group_by(Course,Ethnicity)
cleanjoin%>%group_by(Course, Instructor)%>%arrange(Course,Instructor,Ethnicity)


ethmastery<-subset(cleanjoin,select=-c(ethnicitysuccesspct,ethnicitydroppct))
ethsuccess<-subset(cleanjoin,select=-c(ethnicitymasterpct,ethnicitydroppct))
ethdrop<-subset(cleanjoin,select=-c(ethnicitysuccesspct,ethnicitymasterpct))
ethmastery$measure<-"Mastery"
ethsuccess$measure<-"Success"
ethdrop$measure<-"Drop"
colnames(ethmastery)[4]<-"Mastery Percent"
colnames(ethsuccess)[4]<-"Success Percent"
colnames(ethdrop)[4]<-"Drop Percent"


finalethmastery<-ethmastery%>%spread(key=Ethnicity, value="Mastery Percent")
finalethsuccess<-ethsuccess%>%spread(key=Ethnicity, value="Success Percent")
finalethdrop<-ethdrop%>%spread(key=Ethnicity, value="Drop Percent")

genderjoin<-left_join(gendersuccesspct,genderdroppct, by = c("Course","Instructor","Gender"))
genderjoin<-left_join(genderjoin,gendermasterypct,by = c("Course","Instructor","Gender"))
gendercleanjoin<-subset(genderjoin,select=c(Course,Instructor,Gender,gendermasterypct,gendersuccesspct,genderdroppct))
gendercleanjoin<-gendercleanjoin%>%complete(Gender) %>% group_by(Course,Gender)
gendercleanjoin<-gendercleanjoin%>%filter(Gender!="Unknown")
gendercleanjoin%>%group_by(Course, Instructor)%>%arrange(Course,Instructor,Gender)


gendermastery<-subset(gendercleanjoin,select=-c(gendersuccesspct,genderdroppct))
gendersuccess<-subset(gendercleanjoin,select=-c(gendermasterypct,genderdroppct))
genderdrop<-subset(gendercleanjoin,select=-c(gendersuccesspct,gendermasterypct))
gendermastery$measure<-"Mastery"
gendersuccess$measure<-"Success"
genderdrop$measure<-"Drop"
colnames(gendermastery)[4]<-"Mastery Percent"
colnames(gendersuccess)[4]<-"Success Percent"
colnames(genderdrop)[4]<-"Drop Percent"

finalgendermastery<-gendermastery%>%spread(key=Gender, value="Mastery Percent")
finalgendersuccess<-gendersuccess%>%spread(key=Gender, value="Success Percent")
finalgenderdrop<-genderdrop%>%spread(key=Gender, value="Drop Percent")


masteryenroll<-right_join(enrollment,finalgendermastery, by =c("Course","Instructor"))
masteryjoin<-right_join(masteryenroll,finalethmastery, by = c("Course","Instructor"))
masteryjoin<-subset(masteryjoin,select=-c(measure.y))
colnames(masteryjoin)[4]<-"Measure"

successenroll<-right_join(enrollment,finalgendersuccess, by =c("Course","Instructor"))
successjoin<-right_join(successenroll,finalethsuccess, by = c("Course","Instructor"))
successjoin<-subset(successjoin,select=-c(measure.y))
colnames(successjoin)[4]<-"Measure"

dropenroll<-right_join(enrollment,finalgenderdrop, by =c("Course","Instructor"))
dropjoin<-right_join(dropenroll,finalethdrop, by = c("Course","Instructor"))
dropjoin<-subset(dropjoin,select=-c(measure.y))
colnames(dropjoin)[4]<-"Measure"

mastersuccess<-rbind(masteryjoin,successjoin)
finaltable<-rbind(mastersuccess,dropjoin)
colnames(finaltable)[3]<-"Enrollment"

# REMOVED BECAUSE ERROR IF LESS THAN USUAL NUMBER OF RACES PER CCCAPPLY

# finaltable<-finaltable[,c(1,4,2,3,5,6,7,8,9,10,11,12,13,14,15,16,18,17)]

# colnames(finaltable)[9]<-"Afr. Am."
# colnames(finaltable)[12]<-"Hisp./ Lat."
# colnames(finaltable)[14]<-"Nat. Am."
# colnames(finaltable)[15]<-"Oth. NWht."
# colnames(finaltable)[16]<-"Pac. Isl."
# colnames(finaltable)[18]<-"Unk."

finaltable<-finaltable%>%arrange(Course)



# finaltable$aanum<-as.numeric(sub("%", "", finaltable$`Afr. Am.`))
# finaltable$asiannum<-as.numeric(sub("%", "", finaltable$Asian))
# finaltable$whitenum<-as.numeric(sub("%", "", finaltable$White))
# finaltable$hispnum<-as.numeric(sub("%", "", finaltable$`Hisp./ Lat.`))
#
# finaltable$whitenum<-ifelse(is.na(finaltable$whitenum)==TRUE,0,finaltable$whitenum)
# finaltable$asiannum<-ifelse(is.na(finaltable$asiannum)==TRUE,0,finaltable$asiannum)
#
# finaltable$AfrAm.HPG<-ifelse(finaltable$asiannum>=finaltable$whitenum,finaltable$aanum-finaltable$asiannum,finaltable$aanum-finaltable$whitenum)
# finaltable$AfrAm.HPG<-as.character(finaltable$AfrAm.HPG)
# finaltable$Hisp.HPG<-ifelse(finaltable$asiannum>=finaltable$whitenum,finaltable$hispnum-finaltable$asiannum,finaltable$hispnum-finaltable$whitenum)
# finaltable$Hisp.HPG<-as.character(finaltable$Hisp.HPG)
# finaltable<-finaltable[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,23,24)]
# colnames(finaltable)[19]<-"Difference: AfrAm & HiPerfGrp"
# colnames(finaltable)[20]<-"Difference: Hisp/Lat & HiPerfGrp"





#finaltable$aanum-finaltable$asiannum
table1<-finaltable%>%group_by(Course)%>%flextable()
#
# mytable<-finaltable%>%flextable()%>%width(j=2,200)
#
# mytable<-theme_vanilla(mytable)
mytable<-flextable(finaltable)
mytable<-theme_vanilla(mytable)
myft<-merge_v(mytable, j = c("Course","Measure"))
#  myft<-width(myft,j=3,width = 4)
myft<-autofit(myft)
# dim_pretty(myft)
# #myft
myft <- bg(myft, ~ Measure=="Mastery", ~ Measure, bg = "#ffdfb2")
myft <- bg(myft, ~ Measure=="Success", ~ Measure, bg = "#e4f9d4")
myft <- bg(myft, ~ Measure=="Drop", ~ Measure, bg = "#ffb7b2")

### REMOVED BECAUSE NO HI PERF
#colkeys=c("Difference: AfrAm & HiPerfGrp", "Difference: Hisp/Lat & HiPerfGrp")

#myft<-bg(myft,j=colkeys, bg="#FEF8B1",part="body")
#myft<-bg(myft,j=colkeys, bg="#FEF8B1",part="header")

myft<-align(myft,align="center", part="all")
print(instructorid)
myft<<-myft
#save_as_html(myft,"myfile.html")
ofile<<-paste0(instructorid," - Equity Report - ",Sys.Date(),".html")
outfile<<-(ofile)

#save_as_html(myft,outfile)
if(savesheet=="Y"){
  save_as_html(myft,outfile);print("Saved to disk");print(myft);print("datafile:myft")
 }
else{print("Not saved to disk");print(myft);print("datafile:myft")
 }
#ifelse(savesheet=="Y",writeLines(as.html(myft), "Equity Report.html"),print(myft))
}, silent = F)
}

