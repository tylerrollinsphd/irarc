##' Generate Grading Equity Report for Success (A,B,C grades), Mastery (A,B grades), and Drops (W grades) for identified instructor and for all instructors who have taught each course that the identified instructor has taught.
##'
##' @title Generate Grading Equity Report for Instructor and All Faculty by course taught
##' @param data A data frame containing the variables of interest. Must include a column for \code{course}, \code{coursename},\code{characteristic},\code{grades}.
##' @param subjectcol is a quoted reference to the column in your \code{data} that contains your subject codes/course designators (ENG, MATH, PHYS, etc.)
##' @param subject is a quoted reference to the subject code/course designator you would like to generate the DI report for.
##' @param coursecol is a quoted reference to the column containing the identifiers for your courses (e.g., ENGWR 300, MATH 100, etc.)
##' @param racecol is a quoted reference to the column in your \code{data} that contains the race/ethnicity indicator for each student.
##' @param gendercol is a quoted reference to the column in your \code{data} that contains the gender indicators.
##' @param gradecol is a quoted reference to the column in your \code{data} that contains the letter grades for each student has earned. IMPORTANT!! this must be formatted A,B,C,D,F,W (where W is withdrawl and similar) and other letter grades do not contain +/- grades. Grades must be capitalized.
##'@param limit is an unquoted integer specifying the lower limit of the count required to be included (e.g., limit=10 will replace data for any group with less than 10 students with "---". This is optional and will default to include all groups if limit is not specified. Use this for masking groups with small N.


##' @export
##' @import dplyr rlang tidyr openxlsx DisImpact




direport<-function(data,subjectcol,subject,coursecol,racecol,gendercol,gradecol,limit){




  subjectcol<-as.name(subjectcol)
  racecol<-as.name(racecol)
  gendercol<-as.name(gendercol)
  gradecol<-as.name(gradecol)





  data<-data%>%rename(SUBJECT_CD=!!(subjectcol))
  data<-data%>%rename(OFFICIAL_GRADE=!!(gradecol))
  data<-data%>%rename(Gender=!!(gendercol))
  data<-data%>%rename(Race=!!(racecol))
  data<-data%>%rename(course=!!(coursecol))
  data<-data%>%mutate(success=ifelse(OFFICIAL_GRADE=="A"|OFFICIAL_GRADE=="B"|OFFICIAL_GRADE=="C"|OFFICIAL_GRADE=="P",1,0))
  data<-data%>%mutate(mastery=ifelse(OFFICIAL_GRADE=="A"|OFFICIAL_GRADE=="B",1,0))
  data<-data%>%mutate(drops=ifelse(OFFICIAL_GRADE=="W",0,1))
  data<-data%>%mutate(RaceGender=paste0(Race," ",Gender))
  ###filter for departments/subject codes


  df1<-data%>%filter(SUBJECT_CD==!!(subject))# | department=="HLS")# | department =="PS")

  #df1$Subject<-df1$Subject
  #### INCLUDE THIS LINE IF YOU WANT TO LOOK AT ALL SubjectS AND NOT FILTER BY DEPARTMENT ###
  # df1<-student_equity
  ######################################


  DI_PPG_By_Cohort <- di_ppg(success=success, group=Race, cohort=course,  data=df1) %>%
    as.data.frame
  DI_PPG_By_Cohort
  DIbycourse <- DI_PPG_By_Cohort


  ### this is PPG within 5% ###

  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$pct<(DIbycourse$reference-.05),"1","0")
  DIbycourse$pct<-ifelse(DIbycourse$n<limit, "---", DIbycourse$pct)
  DIbycourse$success<-ifelse(DIbycourse$n<limit, "---", DIbycourse$success)
  #DIbycourse$di_indicator<-ifelse(DIbycourse$n<11, "---", DIbycourse$di_indicator) -- this will be removed below

  DIbycourse<-rename(DIbycourse, "Success (A,B,C,Cr,P)" = success)
  DIbycourse<-rename(DIbycourse, "Group Success Rate" = pct)
  DIbycourse<-rename(DIbycourse, "Average course Success Rate" = reference)
  DIbycourse<-rename(DIbycourse, "Enrollments" = n)
  DIbycourse<-rename(DIbycourse, "Race" = group)
  DIbycourse<-rename(DIbycourse, "course" = cohort)
  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$Enrollments<limit, "---", DIbycourse$DI.PPG.5percent)
  #DIbycourse%>%group_by(DI)%>%count


  #write.excel(DIbycourse)

  ###DI NO MOE
  DI_PPG_By_Cohort_noMOE <- di_ppg(success=success, group=Race, cohort=course, use_prop_in_moe = TRUE, data=df1) %>%
    as.data.frame

  #write.excel(DI_PPG_By_Cohort_noMOE$di_indicator)



  ###### PROP INDEX RACE
  DIProp<-di_prop_index(success=df1$success, group=df1$Race, cohort=df1$course) %>% as.data.frame

  #write.excel(DIProp$di_prop_index)

  ###### DI 80%
  success <- df1 %>% ungroup %>% mutate(success=success) %>% select(success) %>% unlist
  pct <- reference <- NULL
  results<-df1 %>%
    group_by(course, Race) %>%
    summarize(DI=sum(success), pct=sum(success/n()), n=n())%>%
    ungroup %>%
    group_by(course) %>%
    #mutate(ref=max(pct[n>]))

    mutate(reference=max(pct[n>9 & (Race == "White" | Race == "Asian" | Race == "African American" | Race == "Hispanic/Latino")]), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>%
    ungroup %>%
    arrange(course, Race)

  #LA<-results%>%group_by(course)%>%filter(str_detect(course,"LA "))
  #write.excel(LA)

  #### Compile all together
  DIbycourse$DI.PPG.noMOE<-DI_PPG_By_Cohort_noMOE$di_indicator
  DIbycourse$DI.PPG.noMOE<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PPG.noMOE)
  DIbycourse$DI.80Percent<-results$di_indicator
  DIbycourse$DI.80Percent<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.80Percent)
  DIbycourse$DI.PropIndex<-DIProp$di_prop_index
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$DI.PropIndex<.8, 1, 0)
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PropIndex)
  DIbycourse$DI<-ifelse(DIbycourse$DI.PropIndex > 0 | DIbycourse$DI.80Percent >0 | DIbycourse$DI.PPG.noMOE>0 | DIbycourse$DI.PPG.5percent >0, "Yes", "No")
  DIbycourse$DI<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI)
  DIbycourse<-subset(DIbycourse, select = -c(moe, pct_lo,pct_hi,di_indicator))
  # write as csv
  # write.csv(DIbycourse, "DI Dataset - History - 2015-2018.csv")

  # copy to clipboard to paste into xlsx
  # write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  # write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)}
  nrow(DIbycourse)
  #write.excel(DIbycourse)
  DIbycourse<-DIbycourse%>%rename(Course=course)
  DIbycourse<-subset(DIbycourse, select=-c(DI.PPG.5percent,DI.PPG.noMOE,DI.80Percent,DI.PropIndex))
  DIbycourse$`Group Success Rate`<- as.numeric(DIbycourse$`Group Success Rate`)
  ####
  redcell <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  greencell <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  yellowcell <-createStyle(fontColour = "#000000", bgFill = "#FFEC33")

  wb1<-createWorkbook()
  addWorksheet(wb1,"Success-DI-Race")
  writeData(wb1,"Success-DI-Race","This workbook shows the results of applying State Chancellor's Office Disproportionate Impact (DI) methodologies (see Interpretation Guide)", startRow = 1)
  writeData(wb1,"Success-DI-Race","to three different grade metrics (success, mastery, and drop rates) for race/ethnicity, gender, and gender within race/ethnicity. Below right, a traffic light ", startRow = 2)
  writeData(wb1,"Success-DI-Race","metaphor is used to indicate for each course and group whether there is 1) measurable DI (red), 2) the possibility of DI but there's too little data to be sure", startRow = 3)
  writeData(wb1,"Success-DI-Race","(yellow), or 3) no measurable DI (green). See the Interpretation Guide for further detail.", startRow = 4)
  writeData(wb1,"Success-DI-Race",DIbycourse, startRow = 6)
  rowlength<-nrow(DIbycourse)+6

  #DIbyCourse$DI2<-as.factor(DIbySubject$Success-DI-Race)
  #saveWorkbook(wb1, "testDI.xlsx", TRUE)
  #wb1<-loadWorkbook(file="testDI.xlsx")


  conditionalFormatting(wb1, "Success-DI-Race", cols = 7, rows=6:rowlength,type = "contains",rule="Yes", style = redcell)
  conditionalFormatting(wb1, "Success-DI-Race", cols = 7, rows = 6:rowlength, type = "contains",rule="No", style = greencell)
  conditionalFormatting(wb1, "Success-DI-Race", cols = 7, rows = 6:rowlength, type = "contains",rule="---", style = yellowcell)
  #
  addStyle(wb1, sheet = 'Success-DI-Race', cols = 4L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Success-DI-Race', cols = 5L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Success-DI-Race', cols = 6L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  # addStyle(wb1, sheet = 'Success-DI-Race', cols = 7L, rows = 1:rowlength,
  #          style = createStyle(halign = 'right'))
  # addStyle(wb1, sheet = 'Success-DI-Race', cols = 8L, rows = 1:rowlength,
  #          style = createStyle(halign = 'right'))
  # addStyle(wb1, sheet = 'Success-DI-Race', cols = 9L, rows = 1:rowlength,
  #          style = createStyle(halign = 'right'))
  # addStyle(wb1, sheet = 'Success-DI-Race', cols = 10L, rows = 1:rowlength,
  #          style = createStyle(halign = 'right'))

  ############# ADD PERCENTS #############
  # addStyle(wb1, sheet = 'Success-DI-Race', cols = 5, rows = 6:rowlength,
  #          style = createStyle(numFmt="0.0%"))
  # addStyle(wb1, sheet = 'Success-DI-Race', cols = 6, rows = 6:rowlength,
  #          style = createStyle(numFmt="0.0%"))


  # addStyle(wb1, sheet = 'Success-DI-Race', rows = 6, cols = 1:7,
  #          style = createStyle(textDecoration = 'bold',halign='right'))

  addStyle(wb1, sheet = 'Success-DI-Race', rows = 6, cols = 3:7,
           style = createStyle(textDecoration = 'bold',halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Race', rows = 7:rowlength, cols = 3,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Race', rows = 7:rowlength, cols = 4,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Race', rows = 7:rowlength, cols = 5,
           style = createStyle(numFmt = "0.0%",halign="center"))
  addStyle(wb1, sheet = 'Success-DI-Race', rows = 7:rowlength, cols = 6,
           style = createStyle(numFmt = "0.0%",halign="center"))
  # addStyle(wb1, sheet = 'Success-DI-Race', rows = 7:rowlength, cols = 6,
  #          style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Race', rows = 7:rowlength, cols = 7,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Race', rows = 6, cols = 1,
           style = createStyle(textDecoration = 'bold',halign='left'))
  addStyle(wb1, sheet = 'Success-DI-Race', rows = 6, cols = 2,
           style = createStyle(textDecoration = 'bold',halign='left'))

  #saveWorkbook(wb1, paste0("DI Report - ",subject," ",Sys.Date(),".xlsx"), TRUE)

  setColWidths(wb1, sheet='Success-DI-Race', cols = 2:ncol(DIbycourse), widths = "auto")
  setColWidths(wb1, sheet='Success-DI-Race', cols = 7, widths = "7")

  freezePane(wb1,sheet="Success-DI-Race",firstActiveRow =7)







  #GENDER##################################################################








  ####    DI BY COURSE

  DI_PPG_By_Cohort <- di_ppg(success=success, group=Gender, cohort=course,  data=df1) %>%
    as.data.frame
  DI_PPG_By_Cohort
  DIbycourse <- DI_PPG_By_Cohort


  ### this is PPG within 5% ###

  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$pct<(DIbycourse$reference-.05),"1","0")
  DIbycourse$pct<-ifelse(DIbycourse$n<limit, "---", DIbycourse$pct)
  DIbycourse$success<-ifelse(DIbycourse$n<limit, "---", DIbycourse$success)
  #DIbycourse$di_indicator<-ifelse(DIbycourse$n<11, "---", DIbycourse$di_indicator) -- this will be removed below

  DIbycourse<-rename(DIbycourse, "Success (A,B,C,Cr,P)" = success)
  DIbycourse<-rename(DIbycourse, "Group Success Rate" = pct)
  DIbycourse<-rename(DIbycourse, "Average course Success Rate" = reference)
  DIbycourse<-rename(DIbycourse, "Enrollments" = n)
  DIbycourse<-rename(DIbycourse, "Gender" = group)
  DIbycourse<-rename(DIbycourse, "course" = cohort)
  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$Enrollments<limit, "---", DIbycourse$DI.PPG.5percent)
  #DIbycourse%>%group_by(DI)%>%count


  #write.excel(DIbycourse)

  ###DI NO MOE
  DI_PPG_By_Cohort_noMOE <- di_ppg(success=success, group=Gender, cohort=course, use_prop_in_moe = TRUE, data=df1) %>%
    as.data.frame

  #write.excel(DI_PPG_By_Cohort_noMOE$di_indicator)



  ###### PROP INDEX Gender
  DIProp<-di_prop_index(success=df1$success, group=df1$Gender, cohort=df1$course) %>% as.data.frame

  #write.excel(DIProp$di_prop_index)

  ###### DI 80%
  success <- df1 %>% ungroup %>% mutate(success=success) %>% select(success) %>% unlist
  pct <- reference <- NULL
  results<-df1 %>%
    group_by(course, Gender) %>%
    summarize(DI=sum(success), pct=sum(success/n()), n=n())%>%
    ungroup %>%
    group_by(course) %>%
    mutate(reference=max(pct[n>9 & (Gender=="M" | Gender=="F")]), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>%
    ungroup %>%
    arrange(course, Gender)


  #LA<-results%>%group_by(course)%>%filter(str_detect(course,"LA "))
  #write.excel(LA)

  #### Compile all together
  DIbycourse$DI.PPG.noMOE<-DI_PPG_By_Cohort_noMOE$di_indicator
  DIbycourse$DI.PPG.noMOE<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PPG.noMOE)
  DIbycourse$DI.80Percent<-results$di_indicator
  DIbycourse$DI.80Percent<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.80Percent)
  DIbycourse$DI.PropIndex<-DIProp$di_prop_index
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$DI.PropIndex<.8, 1, 0)
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PropIndex)
  DIbycourse$DI<-ifelse(DIbycourse$DI.PropIndex > 0 | DIbycourse$DI.80Percent >0 | DIbycourse$DI.PPG.noMOE>0 | DIbycourse$DI.PPG.5percent >0, "Yes", "No")
  DIbycourse$DI<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI)
  DIbycourse<-subset(DIbycourse, select = -c(moe, pct_lo,pct_hi,di_indicator))
  # write as csv
  # write.csv(DIbycourse, "DI Dataset - History - 2015-2018.csv")

  # copy to clipboard to paste into xlsx
  # write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  # write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)}
  nrow(DIbycourse)
  #write.excel(DIbycourse)
  #DIbycourse<-DIbycourse%>%rename(Course=course)
  DIbycourse$`Group Success Rate`<- as.numeric(DIbycourse$`Group Success Rate`)
  ####
  DIbycourse<-subset(DIbycourse, select=-c(DI.PPG.5percent,DI.PPG.noMOE,DI.80Percent,DI.PropIndex))

  ####
  redcell <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  greencell <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  yellowcell <-createStyle(fontColour = "#000000", bgFill = "#FFEC33")


  addWorksheet(wb1,"Success-DI-Gender")
  writeData(wb1,"Success-DI-Gender","This workbook shows the results of applying State Chancellor's Office Disproportionate Impact (DI) methodologies (see Interpretation Guide)", startRow = 1)
  writeData(wb1,"Success-DI-Gender","to three different grade metrics (success, mastery, and drop rates) for race/ethnicity, gender, and gender within race/ethnicity. Below right, a traffic light ", startRow = 2)
  writeData(wb1,"Success-DI-Gender","metaphor is used to indicate for each course and group whether there is 1) measurable DI (red), 2) the possibility of DI but there's too little data to be sure", startRow = 3)
  writeData(wb1,"Success-DI-Gender","(yellow), or 3) no measurable DI (green). See the Interpretation Guide for further detail.", startRow = 4)
  writeData(wb1,"Success-DI-Gender",DIbycourse, startRow = 6)
  rowlength<-nrow(DIbycourse)+6

  #DIbyCourse$DI2<-as.factor(DIbySubject$Success-DI-Gender)
  #saveWorkbook(wb1, "testDI.xlsx", TRUE)
  #wb1<-loadWorkbook(file="testDI.xlsx")
  conditionalFormatting(wb1, "Success-DI-Gender", cols = 7, rows=6:rowlength,type = "contains",rule="Yes", style = redcell)
  conditionalFormatting(wb1, "Success-DI-Gender", cols = 7, rows = 6:rowlength, type = "contains",rule="No", style = greencell)
  conditionalFormatting(wb1, "Success-DI-Gender", cols = 7, rows = 6:rowlength, type = "contains",rule="---", style = yellowcell)
  #
  addStyle(wb1, sheet = 'Success-DI-Gender', cols = 4L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Success-DI-Gender', cols = 5L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Success-DI-Gender', cols = 6L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Success-DI-Gender', rows = 6, cols = 3:7,
           style = createStyle(textDecoration = 'bold',halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Gender', rows = 7:rowlength, cols = 3,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Gender', rows = 7:rowlength, cols = 4,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Gender', rows = 7:rowlength, cols = 5,
           style = createStyle(numFmt = "0.0%",halign="center"))
  addStyle(wb1, sheet = 'Success-DI-Gender', rows = 7:rowlength, cols = 6,
           style = createStyle(numFmt = "0.0%",halign="center"))
  # addStyle(wb1, sheet = 'Success-DI-Gender', rows = 7:rowlength, cols = 6,
  #          style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Gender', rows = 7:rowlength, cols = 7,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-Gender', rows = 6, cols = 1,
           style = createStyle(textDecoration = 'bold',halign='left'))
  addStyle(wb1, sheet = 'Success-DI-Gender', rows = 6, cols = 2,
           style = createStyle(textDecoration = 'bold',halign='left'))
  setColWidths(wb1, sheet='Success-DI-Gender', cols = 2:ncol(DIbycourse), widths = "auto")
  setColWidths(wb1, sheet='Success-DI-Gender', cols = 7, widths = "7")

  freezePane(wb1,sheet="Success-DI-Gender",firstActiveRow =7)





  #####################
  #####################
  #####################
  # MASTERY #



  DI_PPG_By_Cohort <- di_ppg(success=mastery, group=Race, cohort=course,  data=df1) %>%
    as.data.frame
  DI_PPG_By_Cohort
  DIbycourse <- DI_PPG_By_Cohort


  ### this is PPG within 5% ###

  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$pct<(DIbycourse$reference-.05),"1","0")
  DIbycourse$pct<-ifelse(DIbycourse$n<limit, "---", DIbycourse$pct)
  DIbycourse$success<-ifelse(DIbycourse$n<limit, "---", DIbycourse$success)
  #DIbycourse$di_indicator<-ifelse(DIbycourse$n<11, "---", DIbycourse$di_indicator) -- this will be removed below

  DIbycourse<-rename(DIbycourse, "Mastery (A,B)" = success)
  DIbycourse<-rename(DIbycourse, "Group Mastery Rate" = pct)
  DIbycourse<-rename(DIbycourse, "Average Course Mastery Rate" = reference)
  DIbycourse<-rename(DIbycourse, "Enrollments" = n)
  DIbycourse<-rename(DIbycourse, "Race" = group)
  DIbycourse<-rename(DIbycourse, "course" = cohort)
  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$Enrollments<limit, "---", DIbycourse$DI.PPG.5percent)
  #DIbycourse%>%group_by(DI)%>%count


  #write.excel(DIbycourse)

  ###DI NO MOE
  DI_PPG_By_Cohort_noMOE <- di_ppg(success=mastery, group=Race, cohort=course, use_prop_in_moe = TRUE, data=df1) %>%
    as.data.frame

  #write.excel(DI_PPG_By_Cohort_noMOE$di_indicator)



  ###### PROP INDEX RACE
  DIProp<-di_prop_index(success=df1$mastery, group=df1$Race, cohort=df1$course) %>% as.data.frame

  #write.excel(DIProp$di_prop_index)

  ###### DI 80%
  success <- df1 %>% ungroup %>% mutate(success=mastery) %>% select(success) %>% unlist
  pct <- reference <- NULL
  results<-df1 %>%
    group_by(course, Race) %>%
    summarize(DI=sum(success), pct=sum(success/n()), n=n())%>%
    ungroup %>%
    group_by(course) %>%
    #mutate(ref=max(pct[n>]))

    mutate(reference=max(pct[n>9 & (Race == "White" | Race == "Asian" | Race == "African American" | Race == "Hispanic/Latino")]), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>%
    ungroup %>%
    arrange(course, Race)

  #LA<-results%>%group_by(course)%>%filter(str_detect(course,"LA "))
  #write.excel(LA)

  #### Compile all together
  DIbycourse$DI.PPG.noMOE<-DI_PPG_By_Cohort_noMOE$di_indicator
  DIbycourse$DI.PPG.noMOE<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PPG.noMOE)
  DIbycourse$DI.80Percent<-results$di_indicator
  DIbycourse$DI.80Percent<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.80Percent)
  DIbycourse$DI.PropIndex<-DIProp$di_prop_index
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$DI.PropIndex<.8, 1, 0)
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PropIndex)
  DIbycourse$DI<-ifelse(DIbycourse$DI.PropIndex > 0 | DIbycourse$DI.80Percent >0 | DIbycourse$DI.PPG.noMOE>0 | DIbycourse$DI.PPG.5percent >0, "Yes", "No")
  DIbycourse$DI<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI)
  DIbycourse<-subset(DIbycourse, select = -c(moe, pct_lo,pct_hi,di_indicator))
  # write as csv
  # write.csv(DIbycourse, "DI Dataset - History - 2015-2018.csv")

  # copy to clipboard to paste into xlsx
  # write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  # write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)}
  nrow(DIbycourse)
  #write.excel(DIbycourse)
  DIbycourse<-DIbycourse%>%rename(Course=course)
  DIbycourse$`Group Mastery Rate`<- as.numeric(DIbycourse$`Group Mastery Rate`)
  ####

  ####

  DIbycourse<-subset(DIbycourse, select=-c(DI.PPG.5percent,DI.PPG.noMOE,DI.80Percent,DI.PropIndex))
  redcell <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  greencell <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  yellowcell <-createStyle(fontColour = "#000000", bgFill = "#FFEC33")


  addWorksheet(wb1,"Mastery-DI-Race")
  writeData(wb1,"Mastery-DI-Race","This workbook shows the results of applying State Chancellor's Office Disproportionate Impact (DI) methodologies (see Interpretation Guide)", startRow = 1)
  writeData(wb1,"Mastery-DI-Race","to three different grade metrics (success, mastery, and drop rates) for race/ethnicity, gender, and gender within race/ethnicity. Below right, a traffic light ", startRow = 2)
  writeData(wb1,"Mastery-DI-Race","metaphor is used to indicate for each course and group whether there is 1) measurable DI (red), 2) the possibility of DI but there's too little data to be sure", startRow = 3)
  writeData(wb1,"Mastery-DI-Race","(yellow), or 3) no measurable DI (green). See the Interpretation Guide for further detail.", startRow = 4)
  writeData(wb1,"Mastery-DI-Race",DIbycourse, startRow = 6)
  rowlength<-nrow(DIbycourse)+6

  #DIbyCourse$DI2<-as.factor(DIbySubject$Mastery-DI-Race)
  #saveWorkbook(wb1, "testDI.xlsx", TRUE)
  #wb1<-loadWorkbook(file="testDI.xlsx")
  conditionalFormatting(wb1, "Mastery-DI-Race", cols = 7, rows =6:rowlength,type = "contains",rule="Yes", style = redcell)
  conditionalFormatting(wb1, "Mastery-DI-Race", cols = 7, rows = 6:rowlength, type = "contains",rule="No", style = greencell)
  conditionalFormatting(wb1, "Mastery-DI-Race", cols = 7, rows = 6:rowlength, type = "contains",rule="---", style = yellowcell)
  #
  addStyle(wb1, sheet = 'Mastery-DI-Race', cols = 4L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Mastery-DI-Race', cols = 5L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Mastery-DI-Race', cols = 6L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Mastery-DI-Race', rows = 6, cols = 3:7,
           style = createStyle(textDecoration = 'bold',halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Race', rows = 7:rowlength, cols = 3,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Race', rows = 7:rowlength, cols = 4,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Race', rows = 7:rowlength, cols = 5,
           style = createStyle(numFmt = "0.0%",halign="center"))
  addStyle(wb1, sheet = 'Mastery-DI-Race', rows = 7:rowlength, cols = 6,
           style = createStyle(numFmt = "0.0%",halign="center"))
  # addStyle(wb1, sheet = 'Mastery-DI-Race', rows = 7:rowlength, cols = 6,
  #          style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Race', rows = 7:rowlength, cols = 7,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Race', rows = 6, cols = 1,
           style = createStyle(textDecoration = 'bold',halign='left'))
  addStyle(wb1, sheet = 'Mastery-DI-Race', rows = 6, cols = 2,
           style = createStyle(textDecoration = 'bold',halign='left'))
  setColWidths(wb1, sheet='Mastery-DI-Race', cols = 2:ncol(DIbycourse), widths = "auto")
  setColWidths(wb1, sheet='Mastery-DI-Race', cols = 7, widths = "7")

  freezePane(wb1,sheet="Mastery-DI-Race",firstActiveRow =7)











  #GENDER##################################################################








  ####    DI BY COURSE

  DI_PPG_By_Cohort <- di_ppg(success=mastery, group=Gender, cohort=course,  data=df1) %>%
    as.data.frame
  DI_PPG_By_Cohort
  DIbycourse <- DI_PPG_By_Cohort


  ### this is PPG within 5% ###

  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$pct<(DIbycourse$reference-.05),"1","0")
  DIbycourse$pct<-ifelse(DIbycourse$n<limit, "---", DIbycourse$pct)
  DIbycourse$success<-ifelse(DIbycourse$n<limit, "---", DIbycourse$success)
  #DIbycourse$di_indicator<-ifelse(DIbycourse$n<11, "---", DIbycourse$di_indicator) -- this will be removed below

  DIbycourse<-rename(DIbycourse, "Mastery (A,B)" = success)
  DIbycourse<-rename(DIbycourse, "Group Mastery Rate" = pct)
  DIbycourse<-rename(DIbycourse, "Average Course Mastery Rate" = reference)
  DIbycourse<-rename(DIbycourse, "Enrollments" = n)
  DIbycourse<-rename(DIbycourse, "Gender" = group)
  DIbycourse<-rename(DIbycourse, "course" = cohort)
  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$Enrollments<limit, "---", DIbycourse$DI.PPG.5percent)
  #DIbycourse%>%group_by(DI)%>%count


  #write.excel(DIbycourse)

  ###DI NO MOE
  DI_PPG_By_Cohort_noMOE <- di_ppg(success=mastery, group=Gender, cohort=course, use_prop_in_moe = TRUE, data=df1) %>%
    as.data.frame

  #write.excel(DI_PPG_By_Cohort_noMOE$di_indicator)



  ###### PROP INDEX Gender
  DIProp<-di_prop_index(success=df1$mastery, group=df1$Gender, cohort=df1$course) %>% as.data.frame

  #write.excel(DIProp$di_prop_index)

  ###### DI 80%
  success <- df1 %>% ungroup %>% mutate(success=mastery) %>% select(success) %>% unlist
  pct <- reference <- NULL
  results<-df1 %>%
    group_by(course, Gender) %>%
    summarize(DI=sum(success), pct=sum(success/n()), n=n())%>%
    ungroup %>%
    group_by(course) %>%
    mutate(reference=max(pct[n>9 & (Gender=="M" | Gender=="F")]), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>%
    ungroup %>%
    arrange(course, Gender)


  #LA<-results%>%group_by(course)%>%filter(str_detect(course,"LA "))
  #write.excel(LA)

  #### Compile all together
  DIbycourse$DI.PPG.noMOE<-DI_PPG_By_Cohort_noMOE$di_indicator
  DIbycourse$DI.PPG.noMOE<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PPG.noMOE)
  DIbycourse$DI.80Percent<-results$di_indicator
  DIbycourse$DI.80Percent<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.80Percent)
  DIbycourse$DI.PropIndex<-DIProp$di_prop_index
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$DI.PropIndex<.8, 1, 0)
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PropIndex)
  DIbycourse$DI<-ifelse(DIbycourse$DI.PropIndex > 0 | DIbycourse$DI.80Percent >0 | DIbycourse$DI.PPG.noMOE>0 | DIbycourse$DI.PPG.5percent >0, "Yes", "No")
  DIbycourse$DI<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI)
  DIbycourse<-subset(DIbycourse, select = -c(moe, pct_lo,pct_hi,di_indicator))
  # write as csv
  # write.csv(DIbycourse, "DI Dataset - History - 2015-2018.csv")

  # copy to clipboard to paste into xlsx
  # write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  # write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)}
  nrow(DIbycourse)
  #write.excel(DIbycourse)
  #DIbycourse<-DIbycourse%>%rename(Course=course)
  DIbycourse$`Group Mastery Rate`<- as.numeric(DIbycourse$`Group Mastery Rate`)
  ####
  DIbycourse<-subset(DIbycourse, select=-c(DI.PPG.5percent,DI.PPG.noMOE,DI.80Percent,DI.PropIndex))
  redcell <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  greencell <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  yellowcell <-createStyle(fontColour = "#000000", bgFill = "#FFEC33")


  addWorksheet(wb1,"Mastery-DI-Gender")
  writeData(wb1,"Mastery-DI-Gender","This workbook shows the results of applying State Chancellor's Office Disproportionate Impact (DI) methodologies (see Interpretation Guide)", startRow = 1)
  writeData(wb1,"Mastery-DI-Gender","to three different grade metrics (success, mastery, and drop rates) for race/ethnicity, gender, and gender within race/ethnicity. Below right, a traffic light ", startRow = 2)
  writeData(wb1,"Mastery-DI-Gender","metaphor is used to indicate for each course and group whether there is 1) measurable DI (red), 2) the possibility of DI but there's too little data to be sure", startRow = 3)
  writeData(wb1,"Mastery-DI-Gender","(yellow), or 3) no measurable DI (green). See the Interpretation Guide for further detail.", startRow = 4)
  writeData(wb1,"Mastery-DI-Gender",DIbycourse, startRow = 6)
  rowlength<-nrow(DIbycourse)+6

  #DIbyCourse$DI2<-as.factor(DIbySubject$Mastery-DI-Gender)
  #saveWorkbook(wb1, "testDI.xlsx", TRUE)
  #wb1<-loadWorkbook(file="testDI.xlsx")
  conditionalFormatting(wb1, "Mastery-DI-Gender", cols = 7, rows = 6:rowlength,type = "contains",rule="Yes", style = redcell)
  conditionalFormatting(wb1, "Mastery-DI-Gender", cols = 7, rows = 6:rowlength, type = "contains",rule="No", style = greencell)
  conditionalFormatting(wb1, "Mastery-DI-Gender", cols = 7, rows = 6:rowlength, type = "contains",rule="---", style = yellowcell)
  #
  addStyle(wb1, sheet = 'Mastery-DI-Gender', cols = 4L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', cols = 5L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', cols = 6L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', rows = 6, cols = 3:7,
           style = createStyle(textDecoration = 'bold',halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', rows = 7:rowlength, cols = 3,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', rows = 7:rowlength, cols = 4,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', rows = 7:rowlength, cols = 5,
           style = createStyle(numFmt = "0.0%",halign="center"))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', rows = 7:rowlength, cols = 6,
           style = createStyle(numFmt = "0.0%",halign="center"))
  # addStyle(wb1, sheet = 'Mastery-DI-Gender', rows = 7:rowlength, cols = 6,
  #          style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', rows = 7:rowlength, cols = 7,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', rows = 6, cols = 1,
           style = createStyle(textDecoration = 'bold',halign='left'))
  addStyle(wb1, sheet = 'Mastery-DI-Gender', rows = 6, cols = 2,
           style = createStyle(textDecoration = 'bold',halign='left'))
  setColWidths(wb1, sheet='Mastery-DI-Gender', cols = 2:ncol(DIbycourse), widths = "auto")
  setColWidths(wb1, sheet='Mastery-DI-Gender', cols = 7, widths = "7")

  freezePane(wb1,sheet="Mastery-DI-Gender",firstActiveRow =7)



  #####################
  #####################
  #####################
  # DROPS #



  DI_PPG_By_Cohort <- di_ppg(success=drops, group=Race, cohort=course,  data=df1) %>%
    as.data.frame
  DI_PPG_By_Cohort
  DIbycourse <- DI_PPG_By_Cohort


  ### this is PPG within 5% ###

  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$pct<(DIbycourse$reference-.05),"1","0")
  DIbycourse$pct<-ifelse(DIbycourse$n<limit, "---", DIbycourse$pct)
  DIbycourse$success<-ifelse(DIbycourse$n<limit, "---", DIbycourse$success)
  #DIbycourse$di_indicator<-ifelse(DIbycourse$n<11, "---", DIbycourse$di_indicator) -- this will be removed below

  DIbycourse<-rename(DIbycourse, "Drops (W)" = success)
  DIbycourse<-rename(DIbycourse, "Group Drops Rate" = pct)
  DIbycourse<-rename(DIbycourse, "Average Course Drops Rate" = reference)
  DIbycourse<-rename(DIbycourse, "Enrollments" = n)
  DIbycourse<-rename(DIbycourse, "Race" = group)
  DIbycourse<-rename(DIbycourse, "course" = cohort)
  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$Enrollments<limit, "---", DIbycourse$DI.PPG.5percent)
  #DIbycourse%>%group_by(DI)%>%count


  #write.excel(DIbycourse)

  ###DI NO MOE
  DI_PPG_By_Cohort_noMOE <- di_ppg(success=drops, group=Race, cohort=course, use_prop_in_moe = TRUE, data=df1) %>%
    as.data.frame

  #write.excel(DI_PPG_By_Cohort_noMOE$di_indicator)



  ###### PROP INDEX RACE
  DIProp<-di_prop_index(success=df1$drops, group=df1$Race, cohort=df1$course) %>% as.data.frame

  #write.excel(DIProp$di_prop_index)

  ###### DI 80%
  success <- df1 %>% ungroup %>% mutate(success=drops) %>% select(success) %>% unlist
  pct <- reference <- NULL
  results<-df1 %>%
    group_by(course, Race) %>%
    summarize(DI=sum(success), pct=sum(success/n()), n=n())%>%
    ungroup %>%
    group_by(course) %>%
    #mutate(ref=max(pct[n>]))

    mutate(reference=max(pct[n>9 & (Race == "White" | Race == "Asian" | Race == "African American" | Race == "Hispanic/Latino")]), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>%
    ungroup %>%
    arrange(course, Race)

  #LA<-results%>%group_by(course)%>%filter(str_detect(course,"LA "))
  #write.excel(LA)

  #### Compile all together
  DIbycourse$DI.PPG.noMOE<-DI_PPG_By_Cohort_noMOE$di_indicator
  DIbycourse$DI.PPG.noMOE<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PPG.noMOE)
  DIbycourse$DI.80Percent<-results$di_indicator
  DIbycourse$DI.80Percent<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.80Percent)
  DIbycourse$DI.PropIndex<-DIProp$di_prop_index
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$DI.PropIndex<.8, 1, 0)
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PropIndex)
  DIbycourse$DI<-ifelse(DIbycourse$DI.PropIndex > 0 | DIbycourse$DI.80Percent >0 | DIbycourse$DI.PPG.noMOE>0 | DIbycourse$DI.PPG.5percent >0, "Yes", "No")
  DIbycourse$DI<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI)
  DIbycourse<-subset(DIbycourse, select = -c(moe, pct_lo,pct_hi,di_indicator))
  # write as csv
  # write.csv(DIbycourse, "DI Dataset - History - 2015-2018.csv")

  # copy to clipboard to paste into xlsx
  # write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  # write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)}
  nrow(DIbycourse)
  #write.excel(DIbycourse)
  DIbycourse<-DIbycourse%>%rename(Course=course)
  DIbycourse$`Group Drops Rate`<- as.numeric(DIbycourse$`Group Drops Rate`)
  ####
  DIbycourse<-subset(DIbycourse, select=-c(DI.PPG.5percent,DI.PPG.noMOE,DI.80Percent,DI.PropIndex))
  redcell <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  greencell <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  yellowcell <-createStyle(fontColour = "#000000", bgFill = "#FFEC33")

  addWorksheet(wb1,"Drops-DI-Race")
  writeData(wb1,"Drops-DI-Race","This workbook shows the results of applying State Chancellor's Office Disproportionate Impact (DI) methodologies (see Interpretation Guide)", startRow = 1)
  writeData(wb1,"Drops-DI-Race","to three different grade metrics (success, mastery, and drop rates) for race/ethnicity, gender, and gender within race/ethnicity. Below right, a traffic light ", startRow = 2)
  writeData(wb1,"Drops-DI-Race","metaphor is used to indicate for each course and group whether there is 1) measurable DI (red), 2) the possibility of DI but there's too little data to be sure", startRow = 3)
  writeData(wb1,"Drops-DI-Race","(yellow), or 3) no measurable DI (green). See the Interpretation Guide for further detail.", startRow = 4)
  writeData(wb1,"Drops-DI-Race",DIbycourse, startRow = 6)
  rowlength<-nrow(DIbycourse)+6

  #DIbyCourse$DI2<-as.factor(DIbySubject$Drops-DI-Race)
  #saveWorkbook(wb1, "testDI.xlsx", TRUE)
  #wb1<-loadWorkbook(file="testDI.xlsx")
  conditionalFormatting(wb1, "Drops-DI-Race", cols = 7, rows = 6:rowlength,type = "contains",rule="Yes", style = redcell)
  conditionalFormatting(wb1, "Drops-DI-Race", cols = 7, rows = 6:rowlength, type = "contains",rule="No", style = greencell)
  conditionalFormatting(wb1, "Drops-DI-Race", cols = 7, rows = 6:rowlength, type = "contains",rule="---", style = yellowcell)
  #
  addStyle(wb1, sheet = 'Drops-DI-Race', cols = 4L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Drops-DI-Race', cols = 5L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Drops-DI-Race', cols = 6L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Drops-DI-Race', rows = 6, cols = 3:7,
           style = createStyle(textDecoration = 'bold',halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Race', rows = 7:rowlength, cols = 3,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Race', rows = 7:rowlength, cols = 4,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Race', rows = 7:rowlength, cols = 5,
           style = createStyle(numFmt = "0.0%",halign="center"))
  addStyle(wb1, sheet = 'Drops-DI-Race', rows = 7:rowlength, cols = 6,
           style = createStyle(numFmt = "0.0%",halign="center"))
  # addStyle(wb1, sheet = 'Drops-DI-Race', rows = 7:rowlength, cols = 6,
  #          style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Race', rows = 7:rowlength, cols = 7,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Race', rows = 6, cols = 1,
           style = createStyle(textDecoration = 'bold',halign='left'))
  addStyle(wb1, sheet = 'Drops-DI-Race', rows = 6, cols = 2,
           style = createStyle(textDecoration = 'bold',halign='left'))
  setColWidths(wb1, sheet='Drops-DI-Race', cols = 2:ncol(DIbycourse), widths = "auto")
  setColWidths(wb1, sheet='Drops-DI-Race', cols = 7, widths = "7")

  freezePane(wb1,sheet="Drops-DI-Race",firstActiveRow =7)








  #GENDER##################################################################








  ####    DI BY COURSE

  DI_PPG_By_Cohort <- di_ppg(success=drops, group=Gender, cohort=course,  data=df1) %>%
    as.data.frame
  DI_PPG_By_Cohort
  DIbycourse <- DI_PPG_By_Cohort


  ### this is PPG within 5% ###

  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$pct<(DIbycourse$reference-.05),"1","0")
  DIbycourse$pct<-ifelse(DIbycourse$n<limit, "---", DIbycourse$pct)
  DIbycourse$success<-ifelse(DIbycourse$n<limit, "---", DIbycourse$success)
  #DIbycourse$di_indicator<-ifelse(DIbycourse$n<11, "---", DIbycourse$di_indicator) -- this will be removed below

  DIbycourse<-rename(DIbycourse, "Drops (A,B)" = success)
  DIbycourse<-rename(DIbycourse, "Group Drops Rate" = pct)
  DIbycourse<-rename(DIbycourse, "Average Course Drops Rate" = reference)
  DIbycourse<-rename(DIbycourse, "Enrollments" = n)
  DIbycourse<-rename(DIbycourse, "Gender" = group)
  DIbycourse<-rename(DIbycourse, "course" = cohort)
  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$Enrollments<limit, "---", DIbycourse$DI.PPG.5percent)
  #DIbycourse%>%group_by(DI)%>%count


  #write.excel(DIbycourse)

  ###DI NO MOE
  DI_PPG_By_Cohort_noMOE <- di_ppg(success=drops, group=Gender, cohort=course, use_prop_in_moe = TRUE, data=df1) %>%
    as.data.frame

  #write.excel(DI_PPG_By_Cohort_noMOE$di_indicator)



  ###### PROP INDEX Gender
  DIProp<-di_prop_index(success=df1$drops, group=df1$Gender, cohort=df1$course) %>% as.data.frame

  #write.excel(DIProp$di_prop_index)

  ###### DI 80%
  success <- df1 %>% ungroup %>% mutate(success=drops) %>% select(success) %>% unlist
  pct <- reference <- NULL
  results<-df1 %>%
    group_by(course, Gender) %>%
    summarize(DI=sum(success), pct=sum(success/n()), n=n())%>%
    ungroup %>%
    group_by(course) %>%
    mutate(reference=max(pct[n>9 & (Gender=="M" | Gender=="F")]), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>%
    ungroup %>%
    arrange(course, Gender)


  #LA<-results%>%group_by(course)%>%filter(str_detect(course,"LA "))
  #write.excel(LA)

  #### Compile all together
  DIbycourse$DI.PPG.noMOE<-DI_PPG_By_Cohort_noMOE$di_indicator
  DIbycourse$DI.PPG.noMOE<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PPG.noMOE)
  DIbycourse$DI.80Percent<-results$di_indicator
  DIbycourse$DI.80Percent<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.80Percent)
  DIbycourse$DI.PropIndex<-DIProp$di_prop_index
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$DI.PropIndex<.8, 1, 0)
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PropIndex)
  DIbycourse$DI<-ifelse(DIbycourse$DI.PropIndex > 0 | DIbycourse$DI.80Percent >0 | DIbycourse$DI.PPG.noMOE>0 | DIbycourse$DI.PPG.5percent >0, "Yes", "No")
  DIbycourse$DI<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI)
  DIbycourse<-subset(DIbycourse, select = -c(moe, pct_lo,pct_hi,di_indicator))
  # write as csv
  # write.csv(DIbycourse, "DI Dataset - History - 2015-2018.csv")

  # copy to clipboard to paste into xlsx
  # write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  # write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)}
  nrow(DIbycourse)
  #write.excel(DIbycourse)
  #DIbycourse<-DIbycourse%>%rename(Course=course)
  DIbycourse$`Group Drops Rate`<- as.numeric(DIbycourse$`Group Drops Rate`)
  ####
  DIbycourse<-subset(DIbycourse, select=-c(DI.PPG.5percent,DI.PPG.noMOE,DI.80Percent,DI.PropIndex))
  redcell <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  greencell <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  yellowcell <-createStyle(fontColour = "#000000", bgFill = "#FFEC33")

  addWorksheet(wb1,"Drops-DI-Gender")
  writeData(wb1,"Drops-DI-Gender","This workbook shows the results of applying State Chancellor's Office Disproportionate Impact (DI) methodologies (see Interpretation Guide)", startRow = 1)
  writeData(wb1,"Drops-DI-Gender","to three different grade metrics (success, mastery, and drop rates) for race/ethnicity, gender, and gender within race/ethnicity. Below right, a traffic light ", startRow = 2)
  writeData(wb1,"Drops-DI-Gender","metaphor is used to indicate for each course and group whether there is 1) measurable DI (red), 2) the possibility of DI but there's too little data to be sure", startRow = 3)
  writeData(wb1,"Drops-DI-Gender","(yellow), or 3) no measurable DI (green). See the Interpretation Guide for further detail.", startRow = 4)
  writeData(wb1,"Drops-DI-Gender",DIbycourse, startRow = 6)
  rowlength<-nrow(DIbycourse)+6

  #DIbyCourse$DI2<-as.factor(DIbySubject$Drops-DI-Gender)
  #saveWorkbook(wb1, "testDI.xlsx", TRUE)
  #wb1<-loadWorkbook(file="testDI.xlsx")
  conditionalFormatting(wb1, "Drops-DI-Gender", cols = 7, rows = 6:rowlength,type = "contains",rule="Yes", style = redcell)
  conditionalFormatting(wb1, "Drops-DI-Gender", cols = 7, rows = 6:rowlength, type = "contains",rule="No", style = greencell)
  conditionalFormatting(wb1, "Drops-DI-Gender", cols = 7, rows = 6:rowlength, type = "contains",rule="---", style = yellowcell)
  #
  addStyle(wb1, sheet = 'Drops-DI-Gender', cols = 4L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Drops-DI-Gender', cols = 5L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Drops-DI-Gender', cols = 6L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Drops-DI-Gender', rows = 6, cols = 3:7,
           style = createStyle(textDecoration = 'bold',halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Gender', rows = 7:rowlength, cols = 3,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Gender', rows = 7:rowlength, cols = 4,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Gender', rows = 7:rowlength, cols = 5,
           style = createStyle(numFmt = "0.0%",halign="center"))
  addStyle(wb1, sheet = 'Drops-DI-Gender', rows = 7:rowlength, cols = 6,
           style = createStyle(numFmt = "0.0%",halign="center"))
  # addStyle(wb1, sheet = 'Drops-DI-Gender', rows = 7:rowlength, cols = 6,
  #          style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Gender', rows = 7:rowlength, cols = 7,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-Gender', rows = 6, cols = 1,
           style = createStyle(textDecoration = 'bold',halign='left'))
  addStyle(wb1, sheet = 'Drops-DI-Gender', rows = 6, cols = 2,
           style = createStyle(textDecoration = 'bold',halign='left'))
  setColWidths(wb1, sheet='Drops-DI-Gender', cols = 2:ncol(DIbycourse), widths = "auto")
  setColWidths(wb1, sheet='Drops-DI-Gender', cols = 7, widths = "7")

  freezePane(wb1,sheet="Drops-DI-Gender",firstActiveRow =7)






  #####################
  #################
  ###################
  ##################
  # RACE GENDER DISAGGREGATION

  DI_PPG_By_Cohort <- di_ppg(success=success, group=RaceGender, cohort=course,  data=df1) %>%
    as.data.frame
  DI_PPG_By_Cohort
  DIbycourse <- DI_PPG_By_Cohort


  ### this is PPG within 5% ###

  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$pct<(DIbycourse$reference-.05),"1","0")
  DIbycourse$pct<-ifelse(DIbycourse$n<limit, "---", DIbycourse$pct)
  DIbycourse$success<-ifelse(DIbycourse$n<limit, "---", DIbycourse$success)
  #DIbycourse$di_indicator<-ifelse(DIbycourse$n<11, "---", DIbycourse$di_indicator) -- this will be removed below

  DIbycourse<-rename(DIbycourse, "Success (A,B,C,Cr,P)" = success)
  DIbycourse<-rename(DIbycourse, "Group Success Rate" = pct)
  DIbycourse<-rename(DIbycourse, "Average course Success Rate" = reference)
  DIbycourse<-rename(DIbycourse, "Enrollments" = n)
  DIbycourse<-rename(DIbycourse, "Race.Gender" = group)
  DIbycourse<-rename(DIbycourse, "Course" = cohort)
  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$Enrollments<limit, "---", DIbycourse$DI.PPG.5percent)
  #DIbycourse%>%group_by(DI)%>%count


  #write.excel(DIbycourse)

  ###DI NO MOE
  DI_PPG_By_Cohort_noMOE <- di_ppg(success=success, group=RaceGender, cohort=course, use_prop_in_moe = TRUE, data=df1) %>%
    as.data.frame

  #write.excel(DI_PPG_By_Cohort_noMOE$di_indicator)



  ###### PROP INDEX RACE
  DIProp<-di_prop_index(success=df1$success, group=df1$RaceGender, cohort=df1$course) %>% as.data.frame

  #write.excel(DIProp$di_prop_index)

  ###### DI 80%
  success <- df1 %>% ungroup %>% mutate(success=success) %>% select(success) %>% unlist
  pct <- reference <- NULL
  results<-df1 %>%
    group_by(course, RaceGender) %>%
    summarize(DI=sum(success), pct=sum(success/n()), n=n())%>%
    ungroup %>%
    group_by(course) %>%
    #mutate(ref=max(pct[n>]))

    mutate(reference=max(pct[n>9 & (RaceGender == "White F" |RaceGender == "White M" | RaceGender == "Asian F" |RaceGender == "Asian M" | RaceGender == "African American F" |RaceGender == "African American M" | RaceGender == "Hispanic/Latino F"|RaceGender == "Hispanic/Latino M")]), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>%
    ungroup %>%
    arrange(course, RaceGender)

  #LA<-results%>%group_by(course)%>%filter(str_detect(course,"LA "))
  #write.excel(LA)

  #### Compile all together
  DIbycourse$DI.PPG.noMOE<-DI_PPG_By_Cohort_noMOE$di_indicator
  DIbycourse$DI.PPG.noMOE<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PPG.noMOE)
  DIbycourse$DI.80Percent<-results$di_indicator
  DIbycourse$DI.80Percent<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.80Percent)
  DIbycourse$DI.PropIndex<-DIProp$di_prop_index
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$DI.PropIndex<.8, 1, 0)
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PropIndex)
  DIbycourse$DI<-ifelse(DIbycourse$DI.PropIndex > 0 | DIbycourse$DI.80Percent >0 | DIbycourse$DI.PPG.noMOE>0 | DIbycourse$DI.PPG.5percent >0, "Yes", "No")
  DIbycourse$DI<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI)
  DIbycourse<-subset(DIbycourse, select = -c(moe, pct_lo,pct_hi,di_indicator))
  # write as csv
  # write.csv(DIbycourse, "DI Dataset - History - 2015-2018.csv")

  # copy to clipboard to paste into xlsx
  # write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  # write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)}
  nrow(DIbycourse)
  #write.excel(DIbycourse)
  #DIbycourse<-DIbycourse%>%rename(Course=course)
  DIbycourse$`Group Success Rate`<- as.numeric(DIbycourse$`Group Success Rate`)
  ####
  DIbycourse<-subset(DIbycourse, select=-c(DI.PPG.5percent,DI.PPG.noMOE,DI.80Percent,DI.PropIndex))
  redcell <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  greencell <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  yellowcell <-createStyle(fontColour = "#000000", bgFill = "#FFEC33")


  addWorksheet(wb1,"Success-DI-RaceGender")
  writeData(wb1,"Success-DI-RaceGender","This workbook shows the results of applying State Chancellor's Office Disproportionate Impact (DI) methodologies (see Interpretation Guide)", startRow = 1)
  writeData(wb1,"Success-DI-RaceGender","to three different grade metrics (success, mastery, and drop rates) for race/ethnicity, gender, and gender within race/ethnicity. Below right, a traffic light ", startRow = 2)
  writeData(wb1,"Success-DI-RaceGender","metaphor is used to indicate for each course and group whether there is 1) measurable DI (red), 2) the possibility of DI but there's too little data to be sure", startRow = 3)
  writeData(wb1,"Success-DI-RaceGender","(yellow), or 3) no measurable DI (green). See the Interpretation Guide for further detail.", startRow = 4)
  writeData(wb1,"Success-DI-RaceGender",DIbycourse, startRow = 6)
  rowlength<-nrow(DIbycourse)+6

  #DIbyCourse$DI2<-as.factor(DIbySubject$Success-DI-RaceGender)
  #saveWorkbook(wb1, "testDI.xlsx", TRUE)
  #wb1<-loadWorkbook(file="testDI.xlsx")
  conditionalFormatting(wb1, "Success-DI-RaceGender", cols = 7, rows = 6:rowlength,type = "contains",rule="Yes", style = redcell)
  conditionalFormatting(wb1, "Success-DI-RaceGender", cols = 7, rows = 6:rowlength, type = "contains",rule="No", style = greencell)
  conditionalFormatting(wb1, "Success-DI-RaceGender", cols = 7, rows = 6:rowlength, type = "contains",rule="---", style = yellowcell)
  #
  addStyle(wb1, sheet = 'Success-DI-RaceGender', cols = 4L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', cols = 5L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', cols = 6L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', rows = 6, cols = 3:7,
           style = createStyle(textDecoration = 'bold',halign='center'))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', rows = 7:rowlength, cols = 3,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', rows = 7:rowlength, cols = 4,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', rows = 7:rowlength, cols = 5,
           style = createStyle(numFmt = "0.0%",halign="center"))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', rows = 7:rowlength, cols = 6,
           style = createStyle(numFmt = "0.0%",halign="center"))
  # addStyle(wb1, sheet = 'Success-DI-RaceGender', rows = 7:rowlength, cols = 6,
  #          style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', rows = 7:rowlength, cols = 7,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', rows = 6, cols = 1,
           style = createStyle(textDecoration = 'bold',halign='left'))
  addStyle(wb1, sheet = 'Success-DI-RaceGender', rows = 6, cols = 2,
           style = createStyle(textDecoration = 'bold',halign='left'))
  setColWidths(wb1, sheet='Success-DI-RaceGender', cols = 2:ncol(DIbycourse), widths = "auto")
  setColWidths(wb1, sheet='Success-DI-RaceGender', cols = 7, widths = "7")

  freezePane(wb1,sheet="Success-DI-RaceGender",firstActiveRow =7)









  #####################
  #####################
  #####################
  # MASTERY #



  DI_PPG_By_Cohort <- di_ppg(success=mastery, group=RaceGender, cohort=course,  data=df1) %>%
    as.data.frame
  DI_PPG_By_Cohort
  DIbycourse <- DI_PPG_By_Cohort


  ### this is PPG within 5% ###

  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$pct<(DIbycourse$reference-.05),"1","0")
  DIbycourse$pct<-ifelse(DIbycourse$n<limit, "---", DIbycourse$pct)
  DIbycourse$success<-ifelse(DIbycourse$n<limit, "---", DIbycourse$success)
  #DIbycourse$di_indicator<-ifelse(DIbycourse$n<11, "---", DIbycourse$di_indicator) -- this will be removed below

  DIbycourse<-rename(DIbycourse, "Mastery (A,B)" = success)
  DIbycourse<-rename(DIbycourse, "Group Mastery Rate" = pct)
  DIbycourse<-rename(DIbycourse, "Average Course Mastery Rate" = reference)
  DIbycourse<-rename(DIbycourse, "Enrollments" = n)
  DIbycourse<-rename(DIbycourse, "RaceGender" = group)
  DIbycourse<-rename(DIbycourse, "Course" = cohort)
  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$Enrollments<limit, "---", DIbycourse$DI.PPG.5percent)
  #DIbycourse%>%group_by(DI)%>%count


  #write.excel(DIbycourse)

  ###DI NO MOE
  DI_PPG_By_Cohort_noMOE <- di_ppg(success=mastery, group=RaceGender, cohort=course, use_prop_in_moe = TRUE, data=df1) %>%
    as.data.frame

  #write.excel(DI_PPG_By_Cohort_noMOE$di_indicator)



  ###### PROP INDEX RACE
  DIProp<-di_prop_index(success=df1$mastery, group=df1$RaceGender, cohort=df1$course) %>% as.data.frame

  #write.excel(DIProp$di_prop_index)

  ###### DI 80%
  success <- df1 %>% ungroup %>% mutate(success=mastery) %>% select(success) %>% unlist
  pct <- reference <- NULL
  results<-df1 %>%
    group_by(course, RaceGender) %>%
    summarize(DI=sum(success), pct=sum(success/n()), n=n())%>%
    ungroup %>%
    group_by(course) %>%
    #mutate(ref=max(pct[n>]))

    mutate(reference=max(pct[n>9 & (RaceGender == "White F" |RaceGender == "White M" | RaceGender == "Asian F" |RaceGender == "Asian M" | RaceGender == "African American F" |RaceGender == "African American M" | RaceGender == "Hispanic/Latino F"|RaceGender == "Hispanic/Latino M")]), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>%
    ungroup %>%
    arrange(course, RaceGender)

  #LA<-results%>%group_by(course)%>%filter(str_detect(course,"LA "))
  #write.excel(LA)

  #### Compile all together
  DIbycourse$DI.PPG.noMOE<-DI_PPG_By_Cohort_noMOE$di_indicator
  DIbycourse$DI.PPG.noMOE<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PPG.noMOE)
  DIbycourse$DI.80Percent<-results$di_indicator
  DIbycourse$DI.80Percent<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.80Percent)
  DIbycourse$DI.PropIndex<-DIProp$di_prop_index
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$DI.PropIndex<.8, 1, 0)
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PropIndex)
  DIbycourse$DI<-ifelse(DIbycourse$DI.PropIndex > 0 | DIbycourse$DI.80Percent >0 | DIbycourse$DI.PPG.noMOE>0 | DIbycourse$DI.PPG.5percent >0, "Yes", "No")
  DIbycourse$DI<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI)
  DIbycourse<-subset(DIbycourse, select = -c(moe, pct_lo,pct_hi,di_indicator))
  # write as csv
  # write.csv(DIbycourse, "DI Dataset - History - 2015-2018.csv")

  # copy to clipboard to paste into xlsx
  # write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  # write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)}
  nrow(DIbycourse)
  #write.excel(DIbycourse)
  #DIbycourse<-DIbycourse%>%rename(Course=course)
  DIbycourse$`Group Mastery Rate`<- as.numeric(DIbycourse$`Group Mastery Rate`)
  ####
  DIbycourse<-subset(DIbycourse, select=-c(DI.PPG.5percent,DI.PPG.noMOE,DI.80Percent,DI.PropIndex))
  redcell <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  greencell <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  yellowcell <-createStyle(fontColour = "#000000", bgFill = "#FFEC33")


  addWorksheet(wb1,"Mastery-DI-RaceGender")
  writeData(wb1,"Mastery-DI-RaceGender","This workbook shows the results of applying State Chancellor's Office Disproportionate Impact (DI) methodologies (see Interpretation Guide)", startRow = 1)
  writeData(wb1,"Mastery-DI-RaceGender","to three different grade metrics (success, mastery, and drop rates) for race/ethnicity, gender, and gender within race/ethnicity. Below right, a traffic light ", startRow = 2)
  writeData(wb1,"Mastery-DI-RaceGender","metaphor is used to indicate for each course and group whether there is 1) measurable DI (red), 2) the possibility of DI but there's too little data to be sure", startRow = 3)
  writeData(wb1,"Mastery-DI-RaceGender","(yellow), or 3) no measurable DI (green). See the Interpretation Guide for further detail.", startRow = 4)
  writeData(wb1,"Mastery-DI-RaceGender",DIbycourse, startRow = 6)
  rowlength<-nrow(DIbycourse)+6

  #DIbyCourse$DI2<-as.factor(DIbySubject$Mastery-DI-RaceGender)
  #saveWorkbook(wb1, "testDI.xlsx", TRUE)
  #wb1<-loadWorkbook(file="testDI.xlsx")
  conditionalFormatting(wb1, "Mastery-DI-RaceGender", cols = 7, rows = 6:rowlength,type = "contains",rule="Yes", style = redcell)
  conditionalFormatting(wb1, "Mastery-DI-RaceGender", cols = 7, rows = 6:rowlength, type = "contains",rule="No", style = greencell)
  conditionalFormatting(wb1, "Mastery-DI-RaceGender", cols = 7, rows = 6:rowlength, type = "contains",rule="---", style = yellowcell)
  #
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', cols = 4L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', cols = 5L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', cols = 6L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', rows = 6, cols = 3:7,
           style = createStyle(textDecoration = 'bold',halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', rows = 7:rowlength, cols = 3,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', rows = 7:rowlength, cols = 4,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', rows = 7:rowlength, cols = 5,
           style = createStyle(numFmt = "0.0%",halign="center"))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', rows = 7:rowlength, cols = 6,
           style = createStyle(numFmt = "0.0%",halign="center"))
  # addStyle(wb1, sheet = 'Mastery-DI-RaceGender', rows = 7:rowlength, cols = 6,
  #          style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', rows = 7:rowlength, cols = 7,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', rows = 6, cols = 1,
           style = createStyle(textDecoration = 'bold',halign='left'))
  addStyle(wb1, sheet = 'Mastery-DI-RaceGender', rows = 6, cols = 2,
           style = createStyle(textDecoration = 'bold',halign='left'))
  setColWidths(wb1, sheet='Mastery-DI-RaceGender', cols = 2:ncol(DIbycourse), widths = "auto")
  setColWidths(wb1, sheet='Mastery-DI-RaceGender', cols = 7, widths = "7")

  freezePane(wb1,sheet="Mastery-DI-RaceGender",firstActiveRow =7)






  #####################
  #####################
  #####################
  # DROPS #



  DI_PPG_By_Cohort <- di_ppg(success=drops, group=RaceGender, cohort=course,  data=df1) %>%
    as.data.frame
  DI_PPG_By_Cohort
  DIbycourse <- DI_PPG_By_Cohort


  ### this is PPG within 5% ###

  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$pct<(DIbycourse$reference-.05),"1","0")
  DIbycourse$pct<-ifelse(DIbycourse$n<limit, "---", DIbycourse$pct)
  DIbycourse$success<-ifelse(DIbycourse$n<limit, "---", DIbycourse$success)
  #DIbycourse$di_indicator<-ifelse(DIbycourse$n<11, "---", DIbycourse$di_indicator) -- this will be removed below

  DIbycourse<-rename(DIbycourse, "Drops (W)" = success)
  DIbycourse<-rename(DIbycourse, "Group Drops Rate" = pct)
  DIbycourse<-rename(DIbycourse, "Average Course Drops Rate" = reference)
  DIbycourse<-rename(DIbycourse, "Enrollments" = n)
  DIbycourse<-rename(DIbycourse, "RaceGender" = group)
  DIbycourse<-rename(DIbycourse, "Course" = cohort)
  DIbycourse$DI.PPG.5percent<-ifelse(DIbycourse$Enrollments<limit, "---", DIbycourse$DI.PPG.5percent)
  #DIbycourse%>%group_by(DI)%>%count


  #write.excel(DIbycourse)

  ###DI NO MOE
  DI_PPG_By_Cohort_noMOE <- di_ppg(success=drops, group=RaceGender, cohort=course, use_prop_in_moe = TRUE, data=df1) %>%
    as.data.frame

  #write.excel(DI_PPG_By_Cohort_noMOE$di_indicator)



  ###### PROP INDEX RACE
  DIProp<-di_prop_index(success=df1$drops, group=df1$RaceGender, cohort=df1$course) %>% as.data.frame

  #write.excel(DIProp$di_prop_index)

  ###### DI 80%
  success <- df1 %>% ungroup %>% mutate(success=drops) %>% select(success) %>% unlist
  pct <- reference <- NULL
  results<-df1 %>%
    group_by(course, RaceGender) %>%
    summarize(DI=sum(success), pct=sum(success/n()), n=n())%>%
    ungroup %>%
    group_by(course) %>%
    #mutate(ref=max(pct[n>]))

    mutate(reference=max(pct[n>9 & (RaceGender == "White F" |RaceGender == "White M" | RaceGender == "Asian F" |RaceGender == "Asian M" | RaceGender == "African American F" |RaceGender == "African American M" | RaceGender == "Hispanic/Latino F"|RaceGender == "Hispanic/Latino M")]), di_80_index=pct/reference, di_indicator=ifelse(di_80_index < 0.80, 1, 0)) %>%
    ungroup %>%
    arrange(course, RaceGender)

  #LA<-results%>%group_by(course)%>%filter(str_detect(course,"LA "))
  #write.excel(LA)

  #### Compile all together
  DIbycourse$DI.PPG.noMOE<-DI_PPG_By_Cohort_noMOE$di_indicator
  DIbycourse$DI.PPG.noMOE<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PPG.noMOE)
  DIbycourse$DI.80Percent<-results$di_indicator
  DIbycourse$DI.80Percent<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.80Percent)
  DIbycourse$DI.PropIndex<-DIProp$di_prop_index
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$DI.PropIndex<.8, 1, 0)
  DIbycourse$DI.PropIndex<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI.PropIndex)
  DIbycourse$DI<-ifelse(DIbycourse$DI.PropIndex > 0 | DIbycourse$DI.80Percent >0 | DIbycourse$DI.PPG.noMOE>0 | DIbycourse$DI.PPG.5percent >0, "Yes", "No")
  DIbycourse$DI<-ifelse(DIbycourse$Enrollments<10, "---", DIbycourse$DI)
  DIbycourse<-subset(DIbycourse, select = -c(moe, pct_lo,pct_hi,di_indicator))
  # write as csv
  # write.csv(DIbycourse, "DI Dataset - History - 2015-2018.csv")

  # copy to clipboard to paste into xlsx
  # write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  # write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)}
  nrow(DIbycourse)
  #write.excel(DIbycourse)
  #DIbycourse<-DIbycourse%>%rename(Course=course)
  DIbycourse$`Group Drops Rate`<- as.numeric(DIbycourse$`Group Drops Rate`)
  ####
  DIbycourse<-subset(DIbycourse, select=-c(DI.PPG.5percent,DI.PPG.noMOE,DI.80Percent,DI.PropIndex))
  redcell <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  greencell <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  yellowcell <-createStyle(fontColour = "#000000", bgFill = "#FFEC33")


  addWorksheet(wb1,"Drops-DI-RaceGender")
  writeData(wb1,"Drops-DI-RaceGender","This workbook shows the results of applying State Chancellor's Office Disproportionate Impact (DI) methodologies (see Interpretation Guide)", startRow = 1)
  writeData(wb1,"Drops-DI-RaceGender","to three different grade metrics (success, mastery, and drop rates) for race/ethnicity, gender, and gender within race/ethnicity. Below right, a traffic light ", startRow = 2)
  writeData(wb1,"Drops-DI-RaceGender","metaphor is used to indicate for each course and group whether there is 1) measurable DI (red), 2) the possibility of DI but there's too little data to be sure", startRow = 3)
  writeData(wb1,"Drops-DI-RaceGender","(yellow), or 3) no measurable DI (green). See the Interpretation Guide for further detail.", startRow = 4)
  writeData(wb1,"Drops-DI-RaceGender",DIbycourse, startRow = 6)
  rowlength<-nrow(DIbycourse)+6

  #DIbyCourse$DI2<-as.factor(DIbySubject$Drops-DI-RaceGender)
  #saveWorkbook(wb1, "testDI.xlsx", TRUE)
  #wb1<-loadWorkbook(file="testDI.xlsx")
  conditionalFormatting(wb1, "Drops-DI-RaceGender", cols = 7, rows = 6:rowlength,type = "contains",rule="Yes", style = redcell)
  conditionalFormatting(wb1, "Drops-DI-RaceGender", cols = 7, rows = 6:rowlength, type = "contains",rule="No", style = greencell)
  conditionalFormatting(wb1, "Drops-DI-RaceGender", cols = 7, rows = 6:rowlength, type = "contains",rule="---", style = yellowcell)
  #
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', cols = 4L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', cols = 5L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', cols = 6L, rows = 6:rowlength,
           style = createStyle(halign = 'right'))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', rows = 6, cols = 3:7,
           style = createStyle(textDecoration = 'bold',halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', rows = 7:rowlength, cols = 3,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', rows = 7:rowlength, cols = 4,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', rows = 7:rowlength, cols = 5,
           style = createStyle(numFmt = "0.0%",halign="center"))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', rows = 7:rowlength, cols = 6,
           style = createStyle(numFmt = "0.0%",halign="center"))
  # addStyle(wb1, sheet = 'Drops-DI-RaceGender', rows = 7:rowlength, cols = 6,
  #          style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', rows = 7:rowlength, cols = 7,
           style = createStyle(halign='center'))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', rows = 6, cols = 1,
           style = createStyle(textDecoration = 'bold',halign='left'))
  addStyle(wb1, sheet = 'Drops-DI-RaceGender', rows = 6, cols = 2,
           style = createStyle(textDecoration = 'bold',halign='left'))
  setColWidths(wb1, sheet='Drops-DI-RaceGender', cols = 2:ncol(DIbycourse), widths = "auto")
  setColWidths(wb1, sheet='Drops-DI-RaceGender', cols = 7, widths = "7")

  freezePane(wb1,sheet="Drops-DI-RaceGender",firstActiveRow =7)
  worksheetOrder(wb1)<-c(1,2,7,3,4,8,5,6,9)

  saveWorkbook(wb1, paste0("DI Report - ",subject," ",Sys.Date(),".xlsx"), TRUE)

}













