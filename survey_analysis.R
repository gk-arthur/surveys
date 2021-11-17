# Survey analysis using Qualtrics data

library(reshape2)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(wesanderson)

#Note: installed "wesanderson" color package
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually

setwd()

data <- read.csv("qualtricsdata_raw.csv",header = TRUE) 
str(data)

#remove top row under header (its a second header)
data <- data[-c(1),] 

data$Q1 <- as.character(data$Q1)

#change "Graduate student" to "Grad student" and "Other" to "Staff" 
#Note - checked "Other" and it had space after it, so need to use "Other "

data$Q1[data$Q1=="Graduate student"]="Grad student"
data$Q1[data$Q1=="Other "]="Staff"
data$Q1

#NOTE: remove tests and previews in "DistributionChannel" column for each Q

#Q1. Roles: count staff, faculty, grad students & other

data$Q1[data$DistributionChannel=="test"]=NA
data$Q1[data$DistributionChannel=="preview"]=NA

table(data$Q1)

respondent_tbl <- table(data$Q1)

prop.table(respondent_tbl)

respondent_pcnt <- round(prop.table(respondent_tbl),3)*100
respondent_pcnt

respondent_pcnt <- data.frame(respondent_pcnt)
respondent_pcnt

respondent_pcnt$Var1<-as.factor(respondent_pcnt$Var1)
respondent_pcnt$Freq<-as.numeric(respondent_pcnt$Freq)

names(respondent_pcnt) <- c("Role", "Percent")
head(respondent_pcnt)

respondent_plot <- ggplot(respondent_pcnt,aes(Role,Percent,fill=Role))
respondent_plot + geom_col() +
        ggtitle("CVM Roles") +
        labs(x="",y="Percent") +
        scale_fill_manual(values=c("#00CCCC","#003366","#6699FF")) +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))
        
ggsave(filename="plot_roles.png",plot=last_plot())
                
#Q6. Job description: count yes/no

table(data$Q6)

data$Q6[data$DistributionChannel=="test"]=NA
data$Q6[data$DistributionChannel=="preview"]=NA
data$Q6[data$Q6==""]=NA

table(data$Q6)

jobdescrip_tbl <- table(data$Q6)

jobdescrip_pcnt <- round(prop.table(jobdescrip_tbl),3)*100
jobdescrip_pcnt

prop.table(jobdescrip_tbl)

role_jobdescrip <- table(data$Q6,data$Q1)
role_jobdescrip

role_jobdescrip <- data.frame(role_jobdescrip)
role_jobdescrip

names(role_jobdescrip) <- c("Answer","Role","Freq")
role_jobdescrip

jobdescrip_plot <- ggplot(role_jobdescrip,aes(Role,Freq,fill=Answer))
jobdescrip_plot + geom_col() +
        ggtitle("Global health is part of my job description") +
        labs(x="",y="Count") +
        scale_fill_manual(values=c("#003366","#666666","#6699FF")) +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_jobdesc.png",plot=last_plot())

#Q4_1. Interested in GH: agree/disagree

table(data$Q4_1)

data$Q4_1[data$DistributionChannel=="test"]=NA
data$Q4_1[data$DistributionChannel=="preview"]=NA
data$Q4_1[data$Q4_1==""]=NA

table(data$Q4_1)

#compress agree/disagree groups

data$Q4_1[data$Q4_1=="Strongly agree"]="Agree"
data$Q4_1[data$Q4_1=="Strongly disagree"]="Disagree"

table(data$Q4_1)

role_interest <- table(data$Q4_1,data$Q1)
role_interest

role_interest <- data.frame(role_interest)
role_interest

names(role_interest) <- c("Answer","Role","Freq")
role_interest

interest_plot <- ggplot(role_interest,aes(Role,Freq,fill=Answer))
interest_plot + geom_col() +
        ggtitle("I am interested in global health") +
        labs(x="",y="Count") +
        scale_fill_manual(values=c("#6699FF","#003366","#CCCCCC","#666666")) +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_interest.png",plot=last_plot())

#Q4_2. GH is primary focus of work: agree/disagree

table(data$Q4_2)

data$Q4_2[data$DistributionChannel=="test"]=NA
data$Q4_2[data$DistributionChannel=="preview"]=NA
data$Q4_2[data$Q4_2==""]=NA

table(data$Q4_2)

#compress agree/disagree groups

data$Q4_2[data$Q4_2=="Strongly agree"]="Agree"
data$Q4_2[data$Q4_2=="Strongly disagree"]="Disagree"

table(data$Q4_2)

role_focus <- table(data$Q4_2,data$Q1)
role_focus

role_focus <- data.frame(role_focus)
role_focus

names(role_focus) <- c("Answer","Role","Freq")
role_focus

focus_plot <- ggplot(role_focus,aes(Role,Freq,fill=Answer))
focus_plot + geom_col() +
        ggtitle("Global health is a primary focus of my work") +
        labs(x="",y="Count") +
        scale_fill_manual(values=c("#6699FF","#003366","#CCCCCC","#666666")) +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_focus.png",plot=last_plot())

#Q4_3. Actively participating in program: agree/disagree

table(data$Q4_3)

data$Q4_3[data$DistributionChannel=="test"]=NA
data$Q4_3[data$DistributionChannel=="preview"]=NA
data$Q4_3[data$Q4_3==""]=NA

table(data$Q4_3)

#compress agree/disagree groups

data$Q4_3[data$Q4_3=="Strongly agree"]="Agree"
data$Q4_3[data$Q4_3=="Strongly disagree"]="Disagree"

table(data$Q4_3)

role_particip <- table(data$Q4_3,data$Q1)
role_particip

role_particip <- data.frame(role_particip)
role_particip

names(role_particip) <- c("Answer","Role","Freq")
role_particip

particip_plot <- ggplot(role_particip,aes(Role,Freq,fill=Answer))
particip_plot + geom_col() +
        ggtitle("I am actively participating in global health") +
        labs(x="",y="Count") +
        scale_fill_manual(values=c("#6699FF","#003366","#CCCCCC","#666666")) +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_particip.png",plot=last_plot())

#Q4_4. Previously participated in program: agree/disagree

table(data$Q4_4)

data$Q4_4[data$DistributionChannel=="test"]=NA
data$Q4_4[data$DistributionChannel=="preview"]=NA
data$Q4_4[data$Q4_4==""]=NA

table(data$Q4_4)

#compress agree/disagree groups

data$Q4_4[data$Q4_4=="Strongly agree"]="Agree"
data$Q4_4[data$Q4_4=="Strongly disagree"]="Disagree"

table(data$Q4_4)

role_prevpart <- table(data$Q4_4,data$Q1)
role_prevpart

role_prevpart <- data.frame(role_prevpart)
role_prevpart

names(role_prevpart) <- c("Answer","Role","Freq")
role_prevpart

prevpart_plot <- ggplot(role_prevpart,aes(Role,Freq,fill=Answer))
prevpart_plot + geom_col() +
        ggtitle("I have previously participated in global health") +
        labs(x="",y="Count") +
        scale_fill_manual(values=c("#6699FF","#003366","#CCCCCC","#666666")) +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_prevpart.png",plot=last_plot())

#Q4_5. Making program more of a priority: agree/disagree

table(data$Q4_5)

data$Q4_5[data$DistributionChannel=="test"]=NA
data$Q4_5[data$DistributionChannel=="preview"]=NA
data$Q4_5[data$Q4_5==""]=NA

table(data$Q4_5)

#compress agree/disagree groups

data$Q4_5[data$Q4_5=="Strongly agree"]="Agree"
data$Q4_5[data$Q4_5=="Strongly disagree"]="Disagree"

table(data$Q4_5)

role_priority <- table(data$Q4_5,data$Q1)
role_priority

role_priority <- data.frame(role_priority)
role_priority

names(role_priority) <- c("Answer","Role","Freq")
role_priority

priority_plot <- ggplot(role_priority,aes(Role,Freq,fill=Answer))
priority_plot + geom_col() +
        ggtitle("I want to make global health more of a priority") +
        labs(x="",y="Percent") +
        scale_fill_manual(values=c("#6699FF","#003366","#CCCCCC","#666666")) +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_priority.png",plot=last_plot())

#Q4_6. Program important for advancing career: agree/disagree

table(data$Q4_6)

data$Q4_6[data$DistributionChannel=="test"]=NA
data$Q4_6[data$DistributionChannel=="preview"]=NA
data$Q4_6[data$Q4_6==""]=NA

table(data$Q4_6)

#compress agree/disagree groups

data$Q4_6[data$Q4_6=="Strongly agree"]="Agree"
data$Q4_6[data$Q4_6=="Strongly disagree"]="Disagree"

table(data$Q4_6)

role_advance <- table(data$Q4_6,data$Q1)
role_advance

role_advance <- data.frame(role_advance)
role_advance

names(role_advance) <- c("Answer","Role","Freq")
role_advance

advance_plot <- ggplot(role_advance,aes(Role,Freq,fill=Answer))
advance_plot + geom_col() +
        ggtitle("Global health is important for advancing my career") +
        labs(x="",y="Percent") +
        scale_fill_manual(values=c("#6699FF","#003366","#CCCCCC","#666666")) +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_advance.png",plot=last_plot())

#Q5 -  For this work, I have enough...

#Q5_1 funding: agree/disagree

table(data$Q5_1)

data$Q5_1[data$DistributionChannel=="test"]=NA
data$Q5_1[data$DistributionChannel=="preview"]=NA
data$Q5_1[data$Q5_1==""]=NA
table(data$Q5_1)

data$Q5_1[data$Q5_1=="Strongly agree"]="Agree"
data$Q5_1[data$Q5_1=="Strongly disagree"]="Disagree"
table(data$Q5_1)

funding <- data.frame(table(data$Q5_1))
funding

#Q5_2. time: agree/disagree

table(data$Q5_2)

data$Q5_2[data$DistributionChannel=="test"]=NA
data$Q5_2[data$DistributionChannel=="preview"]=NA
data$Q5_2[data$Q5_2==""]=NA
table(data$Q5_2)

data$Q5_2[data$Q5_2=="Strongly agree"]="Agree"
data$Q5_2[data$Q5_2=="Strongly disagree"]="Disagree"
table(data$Q5_2)

time <- data.frame(table(data$Q5_2))
time

#Q5_3. dept support: agree/disagree

table(data$Q5_3)

data$Q5_3[data$DistributionChannel=="test"]=NA
data$Q5_3[data$DistributionChannel=="preview"]=NA
data$Q5_3[data$Q5_3==""]=NA
table(data$Q5_3)

data$Q5_3[data$Q5_3=="Strongly agree"]="Agree"
data$Q5_3[data$Q5_3=="Strongly disagree"]="Disagree"
table(data$Q5_3)

deptsupp <- data.frame(table(data$Q5_3))
deptsupp

#Q5_4. college support: agree/disagree

table(data$Q5_4)

data$Q5_4[data$DistributionChannel=="test"]=NA
data$Q5_4[data$DistributionChannel=="preview"]=NA
data$Q5_4[data$Q5_4==""]=NA
table(data$Q5_4)

data$Q5_4[data$Q5_4=="Strongly agree"]="Agree"
data$Q5_4[data$Q5_4=="Strongly disagree"]="Disagree"
table(data$Q5_4)

collsupp <- data.frame(table(data$Q5_4))
collsupp

#Q5_5. expertise: agree/disagree

table(data$Q5_5)

data$Q5_5[data$DistributionChannel=="test"]=NA
data$Q5_5[data$DistributionChannel=="preview"]=NA
data$Q5_5[data$Q5_5==""]=NA
table(data$Q5_5)

data$Q5_5[data$Q5_5=="Strongly agree"]="Agree"
data$Q5_5[data$Q5_5=="Strongly disagree"]="Disagree"
table(data$Q5_5)

expertise <- data.frame(table(data$Q5_5))
expertise

#Q5_6. internal collab: agree/disagree

table(data$Q5_6)

data$Q5_6[data$DistributionChannel=="test"]=NA
data$Q5_6[data$DistributionChannel=="preview"]=NA
data$Q5_6[data$Q5_6==""]=NA
table(data$Q5_6)

data$Q5_6[data$Q5_6=="Strongly agree"]="Agree"
data$Q5_6[data$Q5_6=="Strongly disagree"]="Disagree"
table(data$Q5_6)

interncollab <- data.frame(table(data$Q5_6))
interncollab

#Q5_7. external collaborations: agree/disagree

table(data$Q5_7)

data$Q5_7[data$DistributionChannel=="test"]=NA
data$Q5_7[data$DistributionChannel=="preview"]=NA
data$Q5_7[data$Q5_7==""]=NA
table(data$Q5_7)

data$Q5_7[data$Q5_7=="Strongly agree"]="Agree"
data$Q5_7[data$Q5_7=="Strongly disagree"]="Disagree"
table(data$Q5_7)

externcollab <- data.frame(table(data$Q5_7))
externcollab

#Q5_8. mentors: agree/disagree

table(data$Q5_8)

data$Q5_8[data$DistributionChannel=="test"]=NA
data$Q5_8[data$DistributionChannel=="preview"]=NA
data$Q5_8[data$Q5_8==""]=NA
table(data$Q5_8)

data$Q5_8[data$Q5_8=="Strongly agree"]="Agree"
data$Q5_8[data$Q5_8=="Strongly disagree"]="Disagree"
table(data$Q5_8)

mentors <- data.frame(table(data$Q5_8))
mentors

#Q5 combined plot

role_funding <-as.data.frame(table(data$Q5_1,data$Q1))
role_funding <- role_funding%>%
        mutate(id="Funding")
role_funding

role_time <-as.data.frame(table(data$Q5_2,data$Q1))
role_time <- role_time%>%
        mutate(id="Time")%>%
        filter(!Var1=="")
role_time

role_deptsup <- as.data.frame(table(data$Q5_3,data$Q1))
role_deptsup <- role_deptsup%>%
        mutate(id="Dept Support")
role_deptsup

role_collsup <- as.data.frame(table(data$Q5_4,data$Q1))
role_collsup <- role_collsup%>%
        mutate(id="College Support")
role_collsup 

role_exp <- as.data.frame(table(data$Q5_5,data$Q1))
role_exp <- role_exp%>%
        mutate(id="Expertise")
role_exp 

role_incollab <- as.data.frame(table(data$Q5_6,data$Q1))
role_incollab <- role_incollab%>%
        mutate(id="Internal Collab")
role_incollab 

role_excollab <- as.data.frame(table(data$Q5_7,data$Q1))
role_excollab <- role_excollab%>%
        mutate(id="External Collab")
role_excollab 

role_ment <- as.data.frame(table(data$Q5_8,data$Q1))
role_ment <- role_ment%>%
        mutate(id="Mentors")
role_ment 

# use rbind() to add all data frames together

combined <- rbind(role_funding,role_time,role_deptsup,role_collsup,role_exp,role_incollab,role_excollab,role_ment)
combined

combined$Freq<-as.numeric(combined$Freq)
combined$id<-as.factor(combined$id)

# https://sebastiansauer.github.io/two_ways_barplots_with_ggplot2/
# For geom_bar colors: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

names(combined) <- c("Answer", "Role", "Freq", "Factor")
head(combined)

combined_plot <- ggplot(combined,aes(Factor,Freq,fill=Answer))
combined_plot + geom_col() +
        coord_flip() +
        ggtitle("For global health work, I have sufficient...") +
        labs(x="",y="Percent") +
        scale_fill_manual(values=c("#6699FF","#003366","#CCCCCC","#666666")) +
        theme(axis.text.y=element_text(size=12),
              axis.text.x=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_fundingetc.png",plot=last_plot())

#Q7 ...

#Q7_1 internal funding: agree/disagree

table(data$Q7_1)

data$Q7_1[data$DistributionChannel=="test"]=NA
data$Q7_1[data$DistributionChannel=="preview"]=NA
data$Q7_1[data$Q7_1==""]=NA
table(data$Q7_1)

data$Q7_1[data$Q7_1=="Strongly agree"]="Agree"
data$Q7_1[data$Q7_1=="Strongly disagree"]="Disagree"
table(data$Q7_1)

#Q7_2 external funding: agree/disagree

table(data$Q7_2)

data$Q7_2[data$DistributionChannel=="test"]=NA
data$Q7_2[data$DistributionChannel=="preview"]=NA
data$Q7_2[data$Q7_2==""]=NA
table(data$Q7_2)

data$Q7_2[data$Q7_2=="Strongly agree"]="Agree"
data$Q7_2[data$Q7_2=="Strongly disagree"]="Disagree"
table(data$Q7_2)

#Q7_3 data needed: agree/disagree

table(data$Q7_3)

data$Q7_3[data$DistributionChannel=="test"]=NA
data$Q7_3[data$DistributionChannel=="preview"]=NA
data$Q7_3[data$Q7_3==""]=NA
table(data$Q7_3)

data$Q7_3[data$Q7_3=="Strongly agree"]="Agree"
data$Q7_3[data$Q7_3=="Strongly disagree"]="Disagree"
table(data$Q7_3)

#Q7_4 pubs needed: agree/disagree

table(data$Q7_4)

data$Q7_4[data$DistributionChannel=="test"]=NA
data$Q7_4[data$DistributionChannel=="preview"]=NA
data$Q7_4[data$Q7_4==""]=NA
table(data$Q7_4)

data$Q7_4[data$Q7_4=="Strongly agree"]="Agree"
data$Q7_4[data$Q7_4=="Strongly disagree"]="Disagree"
table(data$Q7_4)

#Q7_5 competitive: agree/disagree

table(data$Q7_5)

data$Q7_5[data$DistributionChannel=="test"]=NA
data$Q7_5[data$DistributionChannel=="preview"]=NA
data$Q7_5[data$Q7_5==""]=NA
table(data$Q7_5)

data$Q7_5[data$Q7_5=="Strongly agree"]="Agree"
data$Q7_5[data$Q7_5=="Strongly disagree"]="Disagree"
table(data$Q7_5)

# Q7 combined PART 1: I understand funding opps - EXCLUDE

intern_opps <- as.data.frame(table(data$Q7_1,data$Q1))
intern_opps <- intern_opps%>%
        mutate(id="Internal")
intern_opps 

extern_opps <- as.data.frame(table(data$Q7_2,data$Q1))
extern_opps <- extern_opps%>%
        mutate(id="External")
extern_opps 

opps_combined <- rbind(intern_opps,extern_opps)
opps_combined

opps_combined$Freq<-as.numeric(opps_combined$Freq)
opps_combined$id<-as.factor(opps_combined$id)

names(opps_combined) <- c("Response", "Role", "Freq", "Factor")
head(opps_combined)

opps_combined %>%
        ggplot()+
        geom_bar(aes(Factor,Freq,fill = Response),position = "fill",stat='identity')+
        ylab("Frequency")+
        xlab("")+ 
        scale_fill_manual(values=c("#993399","#FF33CC","#666666","#333333")) +
        theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
                axis.line = element_line(size=0.8, colour = "black"),
                axis.text.x=element_text(colour="black", size = 10 ,angle = 0, vjust = 0, hjust=0.5),
                axis.text.y=element_text(colour="black", size = 15),
                axis.title.x = element_text(size = 15, margin = margin(t = 1, r = 0, b = 0, l = 0)),
                axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
                text = element_text(size = 15, face = "bold"),
                strip.background = element_rect(
                        color="black",size=1.5, linetype="solid"
                )         
        )

# Q7 combined PART 2: I have enough

dataneed <- as.data.frame(table(data$Q7_3,data$Q1))
dataneed <- dataneed%>%
        mutate(id="Data")
dataneed 

pubneed <- as.data.frame(table(data$Q7_4,data$Q1))
pubneed <- pubneed%>%
        mutate(id="Publications")
pubneed 

competitive <- as.data.frame(table(data$Q7_5,data$Q1))
competitive <- competitive%>%
        mutate(id="Competitiveness")
competitive 

need_combined <- rbind(dataneed,pubneed,competitive)
need_combined

need_combined$Freq<-as.numeric(need_combined$Freq)
need_combined$id<-as.factor(need_combined$id)

names(need_combined) <- c("Answer", "Role", "Freq", "Factor")
head(need_combined)

need_plot <- ggplot(need_combined,aes(Factor,Freq,fill=Answer))
need_plot + geom_col() +
        coord_flip() +
        ggtitle("To apply for funding, I have enough...") +
        labs(x="",y="Percent") +
        scale_fill_manual(values=c("#6699FF","#003366","#CCCCCC","#666666")) +
        theme(axis.text.y=element_text(size=12),
              axis.text.x=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_need.png",plot=last_plot())

#Q8 previous internal funding: yes/no

table(data$Q8)

data$Q8[data$DistributionChannel=="test"]=NA
data$Q8[data$DistributionChannel=="preview"]=NA
data$Q8[data$Q8==""]=NA
table(data$Q8)

#Q9 previous external funding: yes/no

table(data$Q9)

data$Q9[data$DistributionChannel=="test"]=NA
data$Q9[data$DistributionChannel=="preview"]=NA
data$Q9[data$Q9==""]=NA
table(data$Q9)

#Q8 & 9 combined
#exclude staff and grad students

prev_intern <- as.data.frame(table(data$Q8,data$Q1))
prev_intern <- prev_intern%>%
        mutate(id="Internal") %>%
        filter(!Var2=="Staff") %>%
        filter(!Var2=="Grad student")
prev_intern 

prev_extern <- as.data.frame(table(data$Q9,data$Q1))
prev_extern <- prev_extern%>%
        mutate(id="External")%>%
        filter(!Var2=="Staff") %>%
        filter(!Var2=="Grad student")
prev_extern 

prev_funding <- rbind(prev_intern,prev_extern )
prev_funding

prev_funding$Freq<-as.numeric(prev_funding$Freq)
prev_funding$id<-as.factor(prev_funding$id)

names(prev_funding) <- c("Answer", "Role", "Freq", "Factor")
head(prev_funding)

prev_funding_plot <- ggplot(prev_funding,aes(Factor,Freq,fill=Answer))
prev_funding_plot + geom_col() +
        coord_flip() +
        ggtitle("% of faculty who have applied for GH funding") +
        labs(x="",y="Percent") +
        scale_fill_manual(values=c("#003366","#CCCCCC","#6699FF")) +
        theme(axis.text.y=element_text(size=12),
              axis.text.x=element_text(size=12),
              axis.title=element_text(size=15),
              axis.line = element_line(size=0.8, colour = "black"),
              plot.title=element_text(size=18,face="bold"))

ggsave(filename="plot_prevfund.png",plot=last_plot())

#Q10 - I have... - EXCLUDE

table(data$Q10_1)
data$Q10_1[data$DistributionChannel=="test"]=NA
data$Q10_1[data$DistributionChannel=="preview"]=NA
data$Q10_1[data$Q10_1==""]=NA
table(data$Q10_1)
data$Q10_1[data$Q10_1=="Strongly agree"]="Agree"
data$Q10_1[data$Q10_1=="Strongly disagree"]="Disagree"
table(data$Q10_1)

table(data$Q10_2)
data$Q10_2[data$DistributionChannel=="test"]=NA
data$Q10_2[data$DistributionChannel=="preview"]=NA
data$Q10_2[data$Q10_2==""]=NA
table(data$Q10_2)
data$Q10_2[data$Q10_2=="Strongly agree"]="Agree"
data$Q10_2[data$Q10_2=="Strongly disagree"]="Disagree"
table(data$Q10_2)

table(data$Q10_3)
data$Q10_3[data$DistributionChannel=="test"]=NA
data$Q10_3[data$DistributionChannel=="preview"]=NA
data$Q10_3[data$Q10_3==""]=NA
table(data$Q10_3)
data$Q10_3[data$Q10_3=="Strongly agree"]="Agree"
data$Q10_3[data$Q10_3=="Strongly disagree"]="Disagree"
table(data$Q10_3)

table(data$Q10_4)
data$Q10_4[data$DistributionChannel=="test"]=NA
data$Q10_4[data$DistributionChannel=="preview"]=NA
data$Q10_4[data$Q10_4==""]=NA
table(data$Q10_4)
data$Q10_4[data$Q10_4=="Strongly agree"]="Agree"
data$Q10_4[data$Q10_4=="Strongly disagree"]="Disagree"
table(data$Q10_4)

int_exp <- as.data.frame(table(data$Q10_1,data$Q1))
int_exp <- int_exp%>%
        mutate(id="Int. experience")
int_exp 

dom_exp <- as.data.frame(table(data$Q10_2,data$Q1))
dom_exp <- dom_exp%>%
        mutate(id="Dom. experience")
dom_exp 

int_contact <- as.data.frame(table(data$Q10_3,data$Q1))
int_contact <- int_contact%>%
        mutate(id="Int. contacts")
int_contact 

dom_contact <- as.data.frame(table(data$Q10_4,data$Q1))
dom_contact <- dom_contact%>%
        mutate(id="Dom. contacts")
dom_contact 

exp_contact <- rbind(int_exp,dom_exp,int_contact,dom_contact)
exp_contact

exp_contact$Freq<-as.numeric(exp_contact$Freq)
exp_contact$id<-as.factor(exp_contact$id)

names(exp_contact) <- c("Response", "Role", "Freq", "Factor")
head(exp_contact)

exp_contact %>%
        ggplot()+
        geom_bar(aes(Factor,Freq,fill = Response),position = "fill",stat='identity')+
        ylab("Frequency")+
        xlab("")+ 
        scale_fill_manual(values=c("#993399","#FF33CC","#666666","#333333")) +
        theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
                axis.line = element_line(size=0.8, colour = "black"),
                axis.text.x=element_text(colour="black", size = 10 ,angle = 90, vjust = 0, hjust=1),
                axis.text.y=element_text(colour="black", size = 15),
                axis.title.x = element_text(size = 15, margin = margin(t = 1, r = 0, b = 0, l = 0)),
                axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
                text = element_text(size = 15, face = "bold"),
                strip.background = element_rect(
                        color="black",size=1.5, linetype="solid"
                )         
        )
