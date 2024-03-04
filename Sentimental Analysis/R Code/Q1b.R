# Activate the library
library(dplyr)
library(ggplot2)
library(reshape2)

# Read the data into R
data <- read.csv("../data/webforum.csv")

#Show the summary, found that there is weird data (eg. WC = 0, QM = 400)
summary(data)

# Change the Date format
data$Date <- as.Date(data$Date)
# Change the Time format
data$Time <- as.character(data$Time)
data$Time <- as.POSIXct(data$Time, format="%H:%M")

#---- PART 1: Create a subset of data to analyse i.e. Clean the data ------------------------------------------------------#

# ---- Step 1: Select posts with significant word counts ----------

# i) Select wordcount >= 25
data = data[data$WC>=25,]
# Remove Qmark = 400
data = data[data$QMark!=400,]
# Summary
  # Rows: 15771
  #ThreadAmount <- unique(data$ThreadID)
  # Threads: 300

# ---- Step 2: find out what is the mode number of Rows for a ThreadID ----------

# i) show the number of posts in each ThreadID
freq = data %>% group_by(data$ThreadID) %>% summarise(Freq=n()) %>% arrange(-Freq)
# ii) rename the columns
colnames(freq) = c("ThreadID","Freq")
# iii) 
modeData <- freq %>% group_by(Freq) %>% summarise(tally = n()) %>% filter(tally == max(tally)) 
modeData$Freq # Answer: 35

# ---- Step 3: Filter the data and choose the ThreadIDs which have the same or more than 35 (mode) ------
topFreq = freq %>% filter(freq$Freq >= 35)
highPostData = data %>% filter (data$ThreadID %in% topFreq$ThreadID) 
# Summary
  # Rows: 13295
  # Threads: 213

# ---- Step 4: Narrow down the years of data sampled from 2005 to 2011 -----------------------
#                  Most of the threads occur within this time period
# i) Version 1: Data grouped by ThreadID
hpd0511 = highPostData %>% filter(Date > "2004-12-31" & Date < "2012-1-1") %>% group_by(ThreadID) %>% arrange(Date)
# ii) Version 2: Data NOT grouped by ThreadID
hpd <- highPostData %>% filter(Date > "2004-12-31" & Date < "2012-1-1")

# ------ PART 2: QUESTION 1 - Analyse changes in ThreadIDs over time (by year and then by Time) ------------

#                 --- STEP 1: GROUP THE DATA BY TIME ONLY ------
  # Note: The date part of the time variable will change according to the day that the code is run
  # Before 12pm             8047 rows
  hpd0511b12 <- hpd0511 %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59") %>% arrange(Time)
  # After 12pm              4313 rows 
  hpd0511b24 <- hpd0511 %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59") %>% arrange(Time)

#                  --- STEP 2: GROUP THE DATA BY YEAR ------------
# i) Group by Year: from 2005 till 2011 
  hpd05 = hpd0511 %>% filter(Date > "2004-12-31" & Date < "2006-1-1")
  hpd06 = hpd0511 %>% filter(Date > "2005-12-31" & Date < "2007-1-1")
  hpd07 = hpd0511 %>% filter(Date > "2006-12-31" & Date < "2008-1-1")
  hpd08 = hpd0511 %>% filter(Date > "2007-12-31" & Date < "2009-1-1")
  hpd09 = hpd0511 %>% filter(Date > "2008-12-31" & Date < "2010-1-1")
  hpd10 = hpd0511 %>% filter(Date > "2009-12-31" & Date < "2011-1-1")
  hpd11 = hpd0511 %>% filter(Date > "2010-12-31" & Date < "2012-1-1")

# ii) Group each year by time block
  # 2005
    # Before 12pm 
    hpd05B12 <- hpd05 %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
    # After 12pm
    hpd05B24 <- hpd05 %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2006
    # Before 12pm 
    hpd06B12 <- hpd06 %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
    # After 12pm
    hpd06B24 <- hpd06 %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2007
    # Before 12pm 
    hpd07B12 <- hpd07 %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
    # After 12pm
    hpd07B24 <- hpd07 %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2008
    # Before 12pm 
    hpd08B12 <- hpd08 %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
    # After 12pm
    hpd08B24 <- hpd08 %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2009
    # Before 12pm 
    hpd09B12 <- hpd09 %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
    # After 12pm
    hpd09B24 <- hpd09 %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2010
    # Before 12pm 
    hpd10B12 <- hpd10 %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
    # After 12pm
    hpd10B24 <- hpd10 %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2011
    # Before 12pm 
    hpd11B12 <- hpd11 %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
    # After 12pm 
    hpd11B24 <- hpd11 %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
    
# ------------- STEP 3 Find the median proportion of Positive Emotion in each ThreadID according to year and time ------
  # Show the ThreadID which had the highest overall (median) proportion of positive emotion in year
  # and at certain times during a year.    
# 2005
  # By year
  pos05 <- hpd05 %>% summarise(pos05 = round(median(posemo),2)) %>% arrange(-pos05) # ThreadID 246014 - 7.41%
  # Before 12pm 
  pos5b12 <- hpd05B12 %>% summarise(pos05 = round(median(posemo),2)) %>% arrange(-pos05) # ThreadID 126241 - 11.10%
  # After 12pm 
  pos5b24 <- hpd05B24 %>% summarise(pos05 = round(median(posemo),2)) %>% arrange(-pos05) # ThreadID 246014 - 8.55%
# 2006
  # By Year
  # 2006
  pos06 <- hpd06 %>% summarise(pos06 = round(median(posemo),2)) %>% arrange(-pos06) # ThreadID 249001 - 4.23% 
  # Before 12pm 
  pos6b12 <- hpd06B12 %>% summarise(pos06 = round(median(posemo),2)) %>% arrange(-pos06) # ThreadID 179689 - 4.55%
  # After 12pm
  pos6b24 <- hpd06B24 %>% summarise(pos06 = round(median(posemo),2)) %>% arrange(-pos06) # ThreadID 191377 - 4.81%
# 2007
  # By Year
  pos07 <- hpd07 %>% summarise(pos07 = round(median(posemo),2)) %>% arrange(-pos07) # ThreadID 296985 - 7.69% 
  # Before 12pm 
  pos7b12 <- hpd07B12 %>% summarise(pos07 = round(median(posemo),2)) %>% arrange(-pos07) # ThreadID 296985 - 7.69%
  # After 12pm
  pos7b24 <- hpd07B24 %>% summarise(pos07 = round(median(posemo),2)) %>% arrange(-pos07) # ThreadID 296985 - 9.90%
# 2008
  # By Year
  pos08 <- hpd08 %>% summarise(pos08 = round(median(posemo),2)) %>% arrange(-pos08) # ThreadID 120790 - 7.41% 
  # Before 12pm 
  pos8b12 <- hpd08B12 %>% summarise(pos08 = round(median(posemo),2)) %>% arrange(-pos08) # ThreadID 467590 - 6.77%
  # After 12pm
  pos8b24 <- hpd08B24 %>% summarise(pos08 = round(median(posemo),2)) %>% arrange(-pos08) # ThreadID 120790 - 9.62%
# 2009
  # By Year
  pos09 <- hpd09 %>% summarise(pos09 = round(median(posemo),2)) %>% arrange(-pos09) # ThreadID 246014 - 5.00% 
  # Before 12pm 
  pos9b12 <- hpd09B12 %>% summarise(pos09 = round(median(posemo),2)) %>% arrange(-pos09) # ThreadID 92985 - 5.45%
  # After 12pm
  pos9b24 <- hpd09B24 %>% summarise(pos09 = round(median(posemo),2)) %>% arrange(-pos09) # ThreadID 246014 - 5.92%
# 2010
  # By Year
  pos10 <- hpd10 %>% summarise(pos10 = round(median(posemo),2)) %>% arrange(-pos10) # ThreadID 482758  6.45%
  # Before 12pm 
  pos10b12 <- hpd10B12 %>% summarise(pos10 = round(median(posemo),2)) %>% arrange(-pos10) # ThreadID 651844 - 11.40%
  # After 12pm
  pos10b24 <- hpd10B24 %>% summarise(pos10 = round(median(posemo),2)) %>% arrange(-pos10) # ThreadID 482758 - 6.98%
# 2011
  # By Year
  pos11 <- hpd11 %>% summarise(pos11 = round(median(posemo),2)) %>% arrange(-pos11) # ThreadID 283958 - 6.56%
  # Before 12pm 
  pos11b12 <- hpd11B12 %>% summarise(pos11 = round(median(posemo),2)) %>% arrange(-pos11) # ThreadID 283958 - 6.31%
  # After 12pm
  pos11b24 <- hpd11B24 %>% summarise(pos11 = round(median(posemo),2)) %>% arrange(-pos11) # ThreadID 283958 - 8.33%

  # ii) Create a grapth to show how the proportion of positivity within a group varies according to year
  PosGroupMatrix <- merge(pos05,pos06,all = TRUE)
  PosGroupMatrix <- merge(PosGroupMatrix,pos07,all = TRUE)
  PosGroupMatrix <- merge(PosGroupMatrix,pos08,all = TRUE)
  PosGroupMatrix <- merge(PosGroupMatrix,pos09,all = TRUE)
  PosGroupMatrix <- merge(PosGroupMatrix,pos10,all = TRUE)
  PosGroupMatrix <- merge(PosGroupMatrix,pos11,all = TRUE)

  # ------------ STEP 4 Find the median proportion of Negative Emotion in each ThreadID according to year and time ------
    # Show the ThreadID which had the highest overall (median) proportion of negative emotion in year
    # and at certain times of day during a year.    
# 2005
  # By Year
  neg05 <- hpd05 %>% summarise(neg05 = round(median(negemo),2)) %>% arrange(-neg05) # ThreadID 231935 - 4.84%
  # Before 12pm 
  neg5b12 <- hpd05B12 %>% summarise(neg05 = round(median(negemo),2)) %>% arrange(-neg05) # ThreadID 231935 - 5.06%
  # After 12pm
  neg5b24 <- hpd05B24 %>% summarise(neg05 = round(median(negemo),2)) %>% arrange(-neg05) # ThreadID 233124 - 4.29%
# 2006
  # By Year
  neg06 <- hpd06 %>% summarise(neg06 = round(median(negemo),2)) %>% arrange(-neg06) # ThreadID 231935 - 3.51%
  # Before 12pm
  neg6b12 <- hpd06B12 %>% summarise(neg06 = round(median(negemo),2)) %>% arrange(-neg06) # ThreadID 301325 - 3.75%
  # After 12pm
  neg6b24 <- hpd06B24 %>% summarise(neg06 = round(median(negemo),2)) %>% arrange(-neg06) # ThreadID 325032 - 5.14%
# 2007
  # By Year
  neg07 <- hpd07 %>% summarise(neg07 = round(median(negemo),2)) %>% arrange(-neg07) # ThreadID 179689 -  4.17%
  # Before 12pm
  neg7b12 <- hpd07B12 %>% summarise(neg07 = round(median(negemo),2)) %>% arrange(-neg07) # ThreadID 179689 - 5.62%
  # After 12pm
  neg7b24 <- hpd07B24 %>% summarise(neg07 = round(median(negemo),2)) %>% arrange(-neg07) # ThreadID 361963 - 5.62%
# 2008
  # By Year
  neg08 <- hpd08 %>% summarise(neg08 = round(median(negemo),2)) %>% arrange(-neg08) # ThreadID 461357 - 4.63%
  # Before 12pm
  neg8b12 <- hpd08B12 %>% summarise(neg08 = round(median(negemo),2)) %>% arrange(-neg08) # ThreadID 461357 - 4.50%
  # After 12pm
  neg8b24 <- hpd08B24 %>% summarise(neg08 = round(median(negemo),2)) %>% arrange(-neg08) # ThreadID 461357 - 5.07%
# 2009
  # By Year
  neg09 <- hpd09 %>% summarise(neg09 = round(median(negemo),2)) %>% arrange(-neg09) # ThreadID 558874 - 3.92%
  # Before 12pm
  neg9b12 <- hpd09B12 %>% summarise(neg09 = round(median(negemo),2)) %>% arrange(-neg09) # ThreadID 558874 - 3.95%
  # After 12pm
  neg9b24 <- hpd09B24 %>% summarise(neg09 = round(median(negemo),2)) %>% arrange(-neg09) # ThreadID 140099 - 4.92%
# 2010
  # By Year
  neg10 <- hpd10 %>% summarise(neg10 = round(median(negemo),2)) %>% arrange(-neg10) # ThreadID 539505 - 9.68%
  # Before 12pm
  neg10b12 <- hpd10B12 %>% summarise(neg10 = round(median(negemo),2)) %>% arrange(-neg10) # ThreadID 539505 - 9.68%
  # After 12pm
  neg10b24 <- hpd10B24 %>% summarise(neg10 = round(median(negemo),2)) %>% arrange(-neg10) # ThreadID 767538 - 3.68%
# 2011
  # By Year
  neg11 <- hpd11 %>% summarise(neg11 = round(median(negemo),2)) %>% arrange(-neg11) # ThreadID 426705 - 4.29%
  # Before 12pm
  neg11b12 <- hpd11B12 %>% summarise(neg11 = round(median(negemo),2)) %>% arrange(-neg11) # ThreadID 426705 - 4.79%
  # After 12pm
  neg11b24 <- hpd11B24 %>% summarise(neg11 = round(median(negemo),2)) %>% arrange(-neg11) # ThreadID 426705 - 4.16%

# ii) Create a grapth to show how the proportion of negativity within a group varies according to year
  NegGroupMatrix <- merge(neg05,neg06,all = TRUE)
  NegGroupMatrix <- merge(NegGroupMatrix,neg07,all = TRUE)
  NegGroupMatrix <- merge(NegGroupMatrix,neg08,all = TRUE)
  NegGroupMatrix <- merge(NegGroupMatrix,neg09,all = TRUE)
  NegGroupMatrix <- merge(NegGroupMatrix,neg10,all = TRUE)
  NegGroupMatrix <- merge(NegGroupMatrix,neg11,all = TRUE)

# ------ PART 3: QUESTION 1 - Analyse changes in the data in general over time ------------

# ---- STEP 1: GROUP DATA according to Year and Time of Day
# i) Group by Time of Day:
  # Before 12pm             
  hpdNewB12 <- hpd %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59") 
  # After 12pm              
  hpdNewB24 <- hpd %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59") 
 
# ii) Group by Year: from 2005 until 2011 
  hpd05New = hpd %>% filter(Date > "2004-12-31" & Date < "2006-1-1")
  hpd06New = hpd %>% filter(Date > "2005-12-31" & Date < "2007-1-1")
  hpd07New = hpd %>% filter(Date > "2006-12-31" & Date < "2008-1-1")
  hpd08New = hpd %>% filter(Date > "2007-12-31" & Date < "2009-1-1")
  hpd09New = hpd %>% filter(Date > "2008-12-31" & Date < "2010-1-1")
  hpd10New = hpd %>% filter(Date > "2009-12-31" & Date < "2011-1-1")
  hpd11New = hpd %>% filter(Date > "2010-12-31" & Date < "2012-1-1")

# iii) Group each year by time block
  # 2005
  # Before 12pm
  hpd05NewB12 <- hpd05New %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
  # After 12pm
  hpd05NewB24 <- hpd05New %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2006
  # Before 12pm
  hpd06NewB12 <- hpd06New %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
  # After 12pm
  hpd06NewB24 <- hpd06New %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2007
  # Before 12pm
  hpd07NewB12 <- hpd07New %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
  # After 12pm
  hpd07NewB24 <- hpd07New %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2008
  # Before 12pm
  hpd08NewB12 <- hpd08New %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
  # After 12pm
  hpd08NewB24 <- hpd08New %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2009
  hpd09NewB12 <- hpd09New %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
  # After 12pm
  hpd09NewB24 <- hpd09New %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2010
  # Before 12pm
  hpd10NewB12 <- hpd10New %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
  # After 12pm
  hpd10NewB24 <- hpd10New %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")
  # 2011
  # Before 12pm
  hpd11NewB12 <- hpd11New %>% filter(Time >= "2019-04-26 00:00:00" & Time <= "2019-04-26 11:59:59")
  # After 12pm
  hpd11NewB24 <- hpd11New %>% filter(Time >= "2019-04-26 12:00:00" & Time <= "2019-04-26 23:59:59")

# ------------ STEP 2 Find the median proportion of Positive Emotion according to year and time of day ------
 # i) In general
     # Before 12pm
     round(median(hpdNewB12$posemo),2) # 2.53%
     # After 12pm
     round(median(hpdNewB24$posemo),2) # 2.61%
   
#2005
  # By Year
  round(median(hpd05New$posemo),2) # 2.43%
  # Before 12
  round(median(hpd05NewB12$posemo),2) # 2.39%
  # After 12
  round(median(hpd05NewB24$posemo),2) # 2.50%
#2006
  # By Year
  round(median(hpd06New$posemo),2) # 2.68%
  # Before 12
  round(median(hpd06NewB12$posemo),2) # 2.65%
  # After 12
  round(median(hpd06NewB24$posemo),2) # 2.73%
#2007
  # By Year
  round(median(hpd07New$posemo),2) # 2.59%
  # Before 12
  round(median(hpd07NewB12$posemo),2) # 2.54%
  # After 12
  round(median(hpd07NewB24$posemo),2) # 2.70%
#2008
  # By Year
  round(median(hpd08New$posemo),2) # 2.56%
  # Before 12
  round(median(hpd08NewB12$posemo),2) # 2.50%
  # After 12
  round(median(hpd08NewB24$posemo),2) # 2.62%
#2009
  # By Year
  round(median(hpd09New$posemo),2) # 2.49%
  # Before 12
  round(median(hpd09NewB12$posemo),2) # 2.53%
  # After 12
  round(median(hpd09NewB24$posemo),2) # 2.38% 
#2010
  # By Year
  round(median(hpd10New$posemo),2) # 2.52%
  # Before 12
  round(median(hpd10NewB12$posemo),2) # 2.50%
  # After 12
  round(median(hpd10NewB24$posemo),2) # 2.54% 
#2011
  # By Year
  round(median(hpd11New$posemo),2) # 2.56%
  # Before 12
  round(median(hpd11NewB12$posemo),2) # 2.50%
  # After 12
  round(median(hpd11NewB24$posemo),2) # 2.69%

# ------------ STEP 3 Find the median proportion of Negative Emotion according to year and time of day ------
# i) Group Data by Time of Day only 
  # Before 12pm
  round(median(hpdNewB12$negemo),2) # 1.85%
  # After 12pm
  round(median(hpdNewB24$negemo),2) # 1.72%
  
# ii) Group data by Year and by Time of Day
  #2005
    # By Year
    round(median(hpd05New$negemo),2) # 1.97%
    # Before 12
    round(median(hpd05NewB12$negemo),2) # 2.02%
    # After 12
    round(median(hpd05NewB24$negemo),2) # 1.92%
  #2006
    # By Year
    round(median(hpd06New$negemo),2) # 1.96%
    # Before 12
    round(median(hpd06NewB12$negemo),2) # 2.00%
    # After 12
    round(median(hpd06NewB24$negemo),2) # 1.88%
  #2007
    # By Year
    round(median(hpd07New$negemo),2) # 1.69%
    # Before 12
    round(median(hpd07NewB12$negemo),2) # 1.67%
    # After 12
    round(median(hpd07NewB24$negemo),2) # 1.74%
  #2008
    # By Year
    round(median(hpd08New$negemo),2) # 1.71%
    # Before 12
    round(median(hpd08NewB12$negemo),2) # 1.75%
    # After 12
    round(median(hpd08NewB24$negemo),2) # 1.66%
  #2009
    # By Year
    round(median(hpd09New$negemo),2) # 1.69%
    # Before 12
    round(median(hpd09NewB12$negemo),2) # 1.75%
    # After 12
    round(median(hpd09NewB24$negemo),2) # 1.54%
  #2010
    # By Year
    round(median(hpd10New$negemo),2) # 1.38%
    # Before 12
    round(median(hpd10NewB12$negemo),2) # 1.50%
    # After 12
    round(median(hpd10NewB24$negemo),2) # 1.23%
  #2011
    # By Year
    round(median(hpd11New$negemo),2) # 1.69%
    # Before 12
    round(median(hpd11NewB12$negemo),2) # 1.79%
    # After 12
    round(median(hpd11NewB24$negemo),2) # 1.53%

# ------ PART 3: QUESTION 1 - Correlation between Negative and Positive Emotion in the data---------------

# i) overall
    cor.test(hpd0511$negemo,hpd0511$posemo) # cor = -0.09920716 , p-value < 2.2e-16 ACCEPT
    
 # ii) By Time
    # Before 12pm             
    cor.test((hpdNewB12$negemo),hpdNewB12$posemo) # cor = -0.0968531 , p-value < 2.2e-16 ACCEPT
    # After 12pm              
     cor.test(hpdNewB24$negemo,hpdNewB24$posemo) # cor = -0.1034317  , p-value = 9.827e-12 ACCEPT
    
  # 2005 
    # By Year
    cor.test(hpd05New$negemo,hpd05New$posemo) # cor = -0.1166316 , p-value = 9.236e-10 ACCEPT
    # Before 12
    cor.test((hpd05NewB12$negemo),hpd05NewB12$posemo) # cor = -0.1257961 , p-value = 2.106e-07 ACCEPT
    # After 12pm
    cor.test(hpd05NewB24$negemo,hpd05NewB24$posemo) # cor = -0.09989987 , p-value = 0.001203 ACCEPT
  # 2006
    # By Year
    cor.test(hpd06New$negemo,hpd06New$posemo) # cor = -0.1016225 , p-value = 3.091e-08 ACCEPT
    # Before 12
    cor.test(hpd06NewB12$negemo,hpd06NewB12$posemo) # cor = -0.09515808 , p-value = 2.191e-05 ACCEPT
    # After 12pm 
    cor.test(hpd06NewB24$negemo,hpd06NewB24$posemo) #  cor = -0.1161966 , p-value = 0.0002831 ACCEPT
  # 2007
    # By Year
    cor.test(hpd07New$negemo,hpd07New$posemo) # cor = -0.09644692  , p-value = 1.829e-05 ACCEPT
    # Before 12
    cor.test(hpd07NewB12$negemo,hpd07NewB12$posemo) # cor = -0.1159505 , p-value = 3.542e-05 ACCEPT
    # After 12pm
    cor.test(hpd07NewB24$negemo,hpd07NewB24$posemo) # cor = -0.06098206 , p-value = 0.1067 REJECT
  # 2008
    # By Year
    cor.test(hpd08New$negemo,hpd08New$posemo) # cor = -0.08743787 , p-value = 0.0008733 ACCEPT
    # Before 12
    cor.test(hpd08NewB12$negemo,hpd08NewB12$posemo) # cor = -0.09560451 , p-value = 0.00315 ACCEPT
    # After 12pm
    cor.test(hpd08NewB24$negemo,hpd08NewB24$posemo) # cor = -0.07359733 ,  p-value = 0.1023 REJECT
  # 2009
    # By Year
    cor.test(hpd09New$negemo,hpd09New$posemo) # cor = -0.1147673 , p-value = 6.33e-06 ACCEPT
    # Before 12
    cor.test(hpd09NewB12$negemo,hpd09NewB12$posemo) # cor = -0.07376356 , p-value = 0.01965 ACCEPT
    # After 12pm
    cor.test(hpd09NewB24$negemo,hpd09NewB24$posemo) # cor = -0.1865013 ,  p-value = 1.288e-05 ACCEPT
  # 2010
    # By Year
    cor.test(hpd10New$negemo,hpd10New$posemo) # cor = -0.05903251 , p-value = 0.128 REJECT
    # Before 12
    cor.test(hpd10NewB12$negemo,hpd10NewB12$posemo) # cor = -0.08175056 , p-value = 0.08603 REJECT
    # After 12pm
    cor.test(hpd10NewB24$negemo,hpd10NewB24$posemo) # cor = -0.008361441 , p-value = 0.901 REJECT
  # 2011
    # By Year
    cor.test(hpd11New$negemo,hpd11New$posemo) # cor = -0.07680528 , p-value = 0.01292 ACCEPT
    # Before 12
    cor.test(hpd11NewB12$negemo,hpd11NewB12$posemo) # cor = -0.03744019 , p-value = 0.3181 REJECT
    # After 12pm
    cor.test(hpd11NewB24$negemo,hpd11NewB24$posemo) # cor = -0.1507724 , p-value = 0.005764 ACCEPT
