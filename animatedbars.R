##' Generate animated bar chart for counts by subject
##'
##' @title Generate animated bar chart for counts by subject
##' @param data A data frame containing the variables of interest.
##' @param subjectcol is a quoted reference to the column in your \code{data} that contains your subject areas/codes
##' @param yearcol is a quoted reference to the column with your time parameter (semester, year, etc.)
##' @param countcol is a quoted reference to the column in your \code{data} that contains the enrollment count per \code{subjectcol} and \code{yearcol} - this means you will need to do your counting and filtering beforehand
##' @param rankcut is an unquoted number that determines how many bars you would like (top 10, top 20, etc)
##' @param gifname is a quoted name for your gif, if you chose to write the animation to a .gif file
##' @param gifwidth is an unquoted number of pixels used to determine the width of the animation (if left blank it will default to 800 pixels)
##' @param gifheight is an unquoted number of pixels used to determine the height of the animation (if left blank it will default to 800 pixels)
##' @param gifspeed is an unquoted number of seconds used to determine the length of the total animation (default is 15)
##' @param framespersecond determines how many frames per second (if left blank it will default to 15 fps)
##' @export
##' @import dplyr gganimate gifski ggplot2 rlang scales png



animatedbars<-function(data,subjectcol,yearcol,countcol,rankcut,gifname,gifwidth,gifheight,gifspeed,framespersecond){

  if(missing(rankcut)){
    rankcut=1000
  } else {
    rankcut=rankcut
  }
  if(missing(gifwidth)){
    gifwidth=800
  } else {
    gifwidth=gifwidth
  }
  if(missing(gifheight)){
    gifheight=800
  } else {
    gifheight=gifheight
  }
  if(missing(gifspeed)){
    gifspeed=10
  } else {
    gifspeed=gifspeed
  }
  if(missing(framespersecond)){
    framespersecond=15
  } else {
    framespersecond=framespersecond
  }
  if(missing(gifname)){
    gifname=paste0(subjectcol," animation - ",Sys.Date() )
  } else {
    gifname=gifname
  }

  # data=bars
  # subjectcol="SUBJECT_CD"
  # yearcol="Academic.Year"
  # countcol="count"
  # rankcut=20
  # gifname="tester"
  # gifwidth=600
  # gifheight=800
  # gifspeed=5
  # str(gap)




  Course=as.name(subjectcol)
  subjectcol<-as.name(subjectcol)
  yearcol<-as.name(yearcol)

  gifname<-as.name(gifname)
  n<-as.name(countcol)
  countcol<-as.name(countcol)


  #
  # coursename<-enquo(coursename)
  # characteristic<-as.name(filtercol)
  # grades<-as.name(gradecol)
  # #graphtitle<-(graphtitle)
  # subject<-data%>%filter(!!(course)==!!(coursename))


# df1<-read.csv("C:\\Users\\w1669082\\Desktop\\allgrades15-19.csv")
#
# df2<-df1
# ## LOOK AT A SUBJECT ##
# df3<-df2%>%filter(SUBJECT_CD=="ENGWR")%>%group_by(Course,strm)%>%count
#
# ## USE IF YOU WANT TO LOOK AT ALL SUBJECTS ####
# df3<-df2%>%filter(strm==1159|strm==1169|strm==1179)%>%group_by(SUBJECT_CD,strm)%>%count
# df3$Course<-df3$SUBJECT_CD
# ###############################################
df3<-data

gap <- df3 %>%
  group_by(!!yearcol) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(!!-n) * 1,
         Value = paste0(" ",round(!!n))) %>% filter(rank<=rankcut)%>%
  ungroup()

p <- ggplot(gap, aes(rank, group = !!Course,
                     fill = as.factor(!!Course), color = as.factor(!!Course))) +
  geom_tile(aes(y = !!n/2,
                height = !!n,
                width = 0.9), alpha = 0.8, color = NA) +

  # text in x-axis (requires clip = "off" in coord_*)
  # paste(country, " ")  is a hack to make pretty spacing, since hjust > 1
  #   leads to weird artifacts in text spacing.
  geom_text(aes(y = 0, label = paste(!!Course, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(rank,y=!!n,label = Value, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +

  labs(title='{closest_state}', x = "", y = "Enrollment") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,3,1,3, "cm")) +

  transition_states(!!yearcol, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, fps = framespersecond, duration = gifspeed, width = gifwidth, height = gifheight, renderer = gifski_renderer(paste0(gifname,".gif")))
}

