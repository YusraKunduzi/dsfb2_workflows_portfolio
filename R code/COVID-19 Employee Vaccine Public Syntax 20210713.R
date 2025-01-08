#' ---
#' title: "COVID-19 Employee Simple Analysis and Visualization"
#' author: "Santos, H. C."
#' date: 2021.05.21 (final revisions) 2021.07.13 (cleaned for public version)
#' output:
#'   html_document:
#'     toc: TRUE
#'     toc_float: TRUE
#' ---

#+ include = FALSE
knitr::opts_chunk$set(echo=FALSE, message = TRUE, include = FALSE)

#### Data Loading ####
#' # Data Loading

# Loading packages and functions
library(dplyr)
library(psych)
library(gmodels)
library(zoo) ## for working with dates
library(chron) ##for working with dates
library(ggplot2)
library(effects) ## for CI
library(DT) ## for tables
library(RColorBrewer) ## for survey plot

# Removing scientific notation and digits
options(scipen = 999)

# Load main data csv - COVID Vaccine Employee Data All DeID 20210519 OSF

#file.path(file.choose()) ##manually find file if needed 

VAC.APPT.PATH <- "C:/Users/yusra/OneDrive/Documenten/dsfb2_workflows_portfolio/ruwe_data/COVID_Vaccine_Employee_Data_All_DeID_20210519_OSF_Version.csv"

## replace with actual file path if needed

VAC.APPT <- read.csv(gsub("[\r\n]", "", VAC.APPT.PATH), header = TRUE,
                       na.strings = c("", "N/A", "NA")) 
##should have 9566 obs. of 29 variables

glimpse(VAC.APPT) ##bird's-eye view of data to check for any anomalies

# Load date information csv - COVID Vaccine Employee Data by Date 20210519 OSF

#file.path(file.choose()) ##manually find file if needed 

VAC.APPT.DATE.PATH <- "C:/Users/yusra/OneDrive/Documenten/dsfb2_workflows_portfolio/ruwe_data/COVID_Vaccine_Employee_Data_by_Date_20210519_OSF_Version.csv"
## replace with actual file path if needed

VAC.APPT.DATE <- read.csv(gsub("[\r\n]", "", VAC.APPT.DATE.PATH), header = TRUE,
                          na.strings = c("", "N/A", "NA")) 
##should have 935 obs. of 6 variables

glimpse(VAC.APPT.DATE) ##bird's-eye view of data to check for any anomalies

# Load survey information csv - COVID Vaccine Employee Survey 20210202 OSF

#file.path(file.choose()) ##manually find file if needed 

VAC.SURVEY.PATH <- "C:/Users/yusra/OneDrive/Documenten/dsfb2_workflows_portfolio/ruwe_data/COVID_Vaccine_Employee_Survey_20210202_OSF.csv"
## replace with actual file path if needed

VAC.SURVEY <- read.csv(gsub("[\r\n]", "", VAC.SURVEY.PATH), header = TRUE,
                          na.strings = c("", "N/A", "NA")) 
##should have 1229 obs. of 8 variables

glimpse(VAC.SURVEY) ##bird's-eye view of data to check for any anomalies

#### Date Data Preparation ####

VAC.APPT.DATE$regDateTimeFull2 <-
    as.POSIXct(VAC.APPT.DATE$regDateTimeFull2,
               format = "%Y-%m-%d %H:%M:%OS")
## Fixing date format

summary.factor(VAC.APPT.DATE$email2CatDetail)
VAC.APPT.DATE <- VAC.APPT.DATE %>%
    mutate("email2CatDetailR" = case_when(
        email2CatDetail == "A: Social Proof" ~ "A: Social Norms",
        email2CatDetail == "A2: Social Proof" ~ "A2: Social Norms",
        TRUE ~ email2CatDetail
    ))
summary.factor(VAC.APPT.DATE$email2CatDetailR)
## relabelling for the manuscript

#### Main Data Preparation ####

# Checking for duplicates

sum(ifelse(VAC.APPT$deliveredA1 == 1 & VAC.APPT$deliveredB1 == 1, 1, 0),
    na.rm = TRUE) ## 0 duplicates
sum(ifelse(VAC.APPT$deliveredA2 == 1 & VAC.APPT$deliveredB2 == 1, 1, 0),
    na.rm = TRUE) ## 0 duplicates
sum(ifelse(VAC.APPT$deliveredA2C == 1 & VAC.APPT$deliveredB2C == 1, 1, 0),
    na.rm = TRUE) ## 0 duplicates
sum(ifelse(VAC.APPT$openedA1 == 1 & VAC.APPT$openedB1 == 1, 1, 0),
    na.rm = TRUE) ## 0 duplicates
sum(ifelse(VAC.APPT$openedA2 == 1 & VAC.APPT$openedB2 == 1, 1, 0),
    na.rm = TRUE) ## 0 duplicates
sum(ifelse(VAC.APPT$openedA2C == 1 & VAC.APPT$openedB2C == 1, 1, 0),
    na.rm = TRUE) ## 0 duplicates
sum(ifelse(VAC.APPT$clickedA1 == 1 & VAC.APPT$clickedB1 == 1, 1, 0),
    na.rm = TRUE) ## 0 duplicates
sum(ifelse(VAC.APPT$clickedA2 == 1 & VAC.APPT$clickedB2 == 1, 1, 0),
    na.rm = TRUE) ## 0 duplicates
sum(ifelse(VAC.APPT$clickedA2C == 1 & VAC.APPT$clickedB2C == 1, 1, 0),
    na.rm = TRUE) ## 0 duplicates

#+ include = FALSE
knitr::opts_chunk$set(echo=FALSE, message = TRUE, include = TRUE)

#### Main Data Analysis ####
#' # Main Analysis  
#'  
#' ## Overall Numbers  

#' Condition sample sizes
summary.factor(VAC.APPT$email1Cat) #3198, 3190, 3178

#' Registrations by time
summary.factor(filter(VAC.APPT, regFull == 1)$email1Cat) ## 310, 325, 366
summary.factor(filter(VAC.APPT, reg1 == 1)$email1Cat) ## 207, 220, 101
summary.factor(filter(VAC.APPT, reg2 == 1)$email2CatDetail) 
## 85, 113, 90, 88
summary.factor(filter(VAC.APPT, reg2C == 1)$email2CatDetail)
## 18, 40, 15, 24

#' ### Delivered numbers (emails successfully delivered)
#' A: Social Proof
summary.factor(VAC.APPT$deliveredA1) ## 3198
#' B: Reframing Risks
summary.factor(VAC.APPT$deliveredB1) ## 3190
#' A: Social Proof 2
summary.factor(VAC.APPT$deliveredA2) ## 1589
#' B: Reframing Risks 2
summary.factor(VAC.APPT$deliveredB2) ## 1589
#' A: Social Proof 2 Corrected
summary.factor(VAC.APPT$deliveredA2C) ## 1589
#' B: Reframing Risks 2 Corrected
summary.factor(VAC.APPT$deliveredB2C) ## 1589

#' ### Opened numbers (emails opened)
#' A: Social Proof
summary.factor(VAC.APPT$openedA1) ## 1481
#' B: Reframing Risks
summary.factor(VAC.APPT$openedB1) ## 1398
#' A: Social Proof 2
summary.factor(VAC.APPT$openedA2) ## 1190
#' B: Reframing Risks 2
summary.factor(VAC.APPT$openedB2) ## 1131
#' A: Social Proof 2 Corrected
summary.factor(VAC.APPT$openedA2C) ## 1175
#' B: Reframing Risks 2 Corrected
summary.factor(VAC.APPT$openedB2C) ## 1112

#' ### Clicked numbers (links within email clicked: Yes/No to vaccination)
#' A: Social Proof
summary.factor(VAC.APPT$clickedA1) ## 562
#' B: Reframing Risks
summary.factor(VAC.APPT$clickedB1) ## 382
#' A: Social Proof 2
summary.factor(VAC.APPT$clickedA2) ## 503
#' B: Reframing Risks 2
summary.factor(VAC.APPT$clickedB2) ## 358
#' A: Social Proof 2 Corrected
summary.factor(VAC.APPT$clickedA2C) ## 308
#' B: Reframing Risks 2 Corrected
summary.factor(VAC.APPT$clickedB2C) ## 180

#' ## Logistic Regressions

#' Number of registrations in data
summary.factor(VAC.APPT$regFull)
#summary.factor(VAC.APPT$regFullR) #should be 1001

#' Number of registrations made within first 3 days
summary.factor(VAC.APPT$reg1)
#summary.factor(VAC.APPT$reg1R) #should be 528

#' ### Registration: Control vs. Emails (Email 1 only)

summary.factor(filter(VAC.APPT)$email1Cat)
## 3058, 3072, 3049

VAC.APPT$email1Cat <- 
    factor(VAC.APPT$email1Cat, 
           levels = c("Delay", "A: Social Proof", "B: Reframing Risks"))

summary.factor(filter(VAC.APPT, reg1 == 1)$email1Cat)
## should be 55, 151, 157 after excluding registrations before email
apptLogit1 <- glm(reg1 ~ email1Cat, data = VAC.APPT, 
                  family = "binomial")
summary(apptLogit1)
round(exp(coef(apptLogit1)), 2) ## odds ratios
round(exp(confint.default(apptLogit1)),3) ## CI

#' ### Registration: Social Proof vs. Reframing Risks (Email 1 only)

apptLogit2 <- glm(reg1 ~ email1Cat, 
                  data = filter(VAC.APPT, email1Cat != "Delay"),
                  family = "binomial")
summary(apptLogit2)
round(exp(coef(apptLogit2)), 2) ## odds ratios
round(exp(confint.default(apptLogit2)),3) ## CI

#### Graphs ####
#' # Graphs  
#'   
#' ## Main Graph

APPT.LOGIT1 <- as.data.frame(Effect(c("email1Cat"), apptLogit1))
APPT.LOGIT1$label <- c("3.2%\n(101/3178)", "6.4%\n(207/3198)", 
                       "6.9%\n(220/3190)")
APPT.LOGIT1$email <- c("Delayed control", "Social norms", "Reframing risks")
APPT.LOGIT1$email <- factor(APPT.LOGIT1$email, 
                            c("Delayed control", "Social norms", 
                              "Reframing risks"))

ggplot(data = APPT.LOGIT1, aes(x = email, y = fit)) +
    geom_bar(stat = "identity", fill = "#c2a5cf") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2,
                  position = position_dodge(.9)) +
    geom_text(aes(label = label), size = 5.5, 
              y = 0, vjust = -.3, color = 'black') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    xlab("") + ylab("Vaccine appointment registrations") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 15, color='black'),
          axis.text.y = element_text(size = 15, color='black'),
          axis.title.y = element_text(size = 15, color='black', vjust = 2),
          panel.grid.major.y = element_line(color = "grey", size = 0.5)) 
## 600 x 450

#' (No Contact added to Versions A2 and B2)
ggplot(VAC.APPT.DATE, aes(x = regDateTimeFull2, y = Cumulative_Registrations2, 
                          color = email2CatDetail,
                          linetype = email2CatDetail)) + 
    geom_step(size = 1) + 
    scale_linetype_manual(values=c("solid", "dotted",
                                   "solid", "dotted", 
                                   "solid")) +
    scale_color_manual(values = c("#2c7bb6", "#92c5de", 
                                  "#d7191c", "#f4a582",
                                  "#4daf4a")) +
    scale_x_datetime(minor="1 day") +
    geom_vline(xintercept = as.POSIXct("2021-01-15 16:55:00 EST")) +
    geom_vline(xintercept = as.POSIXct("2021-01-18 16:40:00 EST")) +
    geom_vline(xintercept = as.POSIXct("2021-01-19 12:49:00 EST")) +
    theme_minimal() +
    labs(color = "Email Phase", linetype = "Email Phase", 
         x = "Date and Time", y = "Cumulative Count of Registrations") +
    annotate("text", x = as.POSIXct("2021-01-15 16:55:00 EST"), y = 200,
             label="Email 1: 16:55 EST", vjust = 1.5, angle  = 90) +
    annotate("text", x = as.POSIXct("2021-01-18 16:40:00 EST"), y = 50,
             label="Email 2: 16:40 EST", vjust = 1.5, angle  = 90) +
    annotate("text", x = as.POSIXct("2021-01-19 12:49:00 EST"), y = 81,
             label="Corrected Email 2: 12:49 EST", vjust = 1.5, angle  = 90)
##600 x 450

#' ## Bar graph showing number of responses to survey
#' File already cleaned (to all who completed before end of study period)

total <- nrow(VAC.SURVEY)
freq_table <- data.frame(table(VAC.SURVEY$main))
names(freq_table) <- c("Codes", "Freq")

tmp <- freq_table %>% arrange(Freq)
freq_table$Codes <- factor(freq_table$Codes, levels = tmp$Codes)
freq_table$Perc <- paste(round((freq_table$Freq/total)*100,1), "%", sep = "")

ggplot(freq_table, aes(x = Codes, y = Freq, fill = Codes)) +
    geom_bar(stat = 'identity', position = position_dodge(), color = 'black') + 
    theme_classic() +
    theme(axis.text.x = element_text(vjust = 1, hjust = 1),
          axis.text = element_text(size = 7, color = 'black'),
          strip.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.title.y = element_text(margin = unit(c(5, 0, 0, 0, 0), "mm")),
          legend.position = 'none') + xlab("") + ylab("\nNumber of respondents") +
    geom_text(aes(label=Perc), size=3, position=position_dodge(width=0.9), 
              hjust=-0.2, color='black') +
    scale_fill_manual(values = c('white', 'white', 'white', 'white', 
                                 'white', 'white', 'white', 'white', 
                                 'white', 'white', 'white',
                                 "#1B7837", "#5AAE61", "#A6DBA0", 
                                 "#D9F0D3", "#F7F7F7", "#E7D4E8", 
                                 "#C2A5CF", "#9970AB", "#762A83")) + 
    coord_flip() +
    ylim(c(0, max(freq_table$Freq)*1.3))


