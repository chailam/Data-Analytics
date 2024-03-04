# Activate the library
library(dplyr)
library(ggplot2)

# Read the data into R
data = read.csv("../data/webforum.csv")

#Find the number of groups
length(unique(data$ThreadID)) # 300 ThreadID = 300 different group

#Show the summary, found that there is weird data (eg. WC = 0, QM = 400)
summary(data)

# Change the Date format
data$Date <- as.Date(data$Date)

#----Create a subset of data to analyse. Choose the ThreadID with more posts and more WC----#
# Select wordcount >= 25
data = data[data$WC>=25,]
# Remove Qmark = 400
data = data[data$QMark!=400,]

# Create a freq which show the frequency of post group by ThreadID
freq = data %>% group_by(data$ThreadID) %>% summarise(Freq=n()) %>% arrange(-Freq)
colnames(freq) = c("ThreadID","Freq")
summary(freq)

# Filter the data and choose the group which has frequency of post more than 35 (mode)
topFreq = freq %>% filter(freq$Freq >= 35)
highPostData = data %>% filter (data$ThreadID %in% topFreq$ThreadID) #highPostData shows the ThreadID which has 1000 and above number of post


# Correlation between anger and anxious with negemo
cor.test(highPostData$negemo,highPostData$anger)
cor.test(highPostData$negemo,highPostData$anx)
cor.test(highPostData$anger,highPostData$anx)
cor.test(highPostData2008$posemo,highPostData2008$anger)


thread120790In2008 = highPostData2008 %>% filter (highPostData2008$ThreadID == "120790") 
cor.test(thread120790In2008$posemo,thread120790In2008$anger)


#----A. Draw boxplots of sentiments versus date by years from 2005 till 2011----#
# Draw a boxplot of emotional(negemo) vs time, group by ThreadID
ggplot(highPostData,aes(y=negemo,x = Date,group=highPostData$ThreadID))+geom_boxplot() + ggtitle("Negetive Emotion over Year for all Threads") + xlab("Year") +ylab("Negemo")
ggplot(highPostData,aes(y=posemo,x = Date,group=highPostData$ThreadID))+geom_boxplot() + ggtitle("Positive Emotion over Year for all Threads") + xlab("Year") +ylab("Posemo")
ggplot(highPostData,aes(y=anx,x = Date,group=highPostData$ThreadID))+geom_boxplot() + ggtitle("Anxious Emotion over Year for all Threads") + xlab("Year") +ylab("Anx")
ggplot(highPostData,aes(y=anger,x = Date,group=highPostData$ThreadID))+geom_boxplot() + ggtitle("Anger Emotion over Year for all Threads") + xlab("Year") +ylab("Anger")


# From the boxplot, decided to investigate the time from 2005 till 2011
highPostData2005 = highPostData %>% filter(Date > "2004-12-31" & Date < "2006-1-1")
highPostData2006 = highPostData %>% filter(Date > "2005-12-31" & Date < "2007-1-1")
highPostData2007 = highPostData %>% filter(Date > "2006-12-31" & Date < "2008-1-1")
highPostData2008 = highPostData %>% filter(Date > "2007-12-31" & Date < "2009-1-1")
highPostData2009 = highPostData %>% filter(Date > "2008-12-31" & Date < "2010-1-1")
highPostData2010 = highPostData %>% filter(Date > "2009-12-31" & Date < "2011-1-1")
highPostData2011 = highPostData %>% filter(Date > "2010-12-31" & Date < "2012-1-1")

# For 2011,>>
Negemo2011 = ggplot(highPostData2011,aes(x=factor(highPostData2011$ThreadID),y=negemo,group=highPostData2011$ThreadID,fill = factor(highPostData2011$ThreadID)))+geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
Negemo2011 = Negemo2011 + ggtitle("Year 2011 negemo") + xlab("Thread") + ylab("Negemo") + labs(fill = "Thread")
Negemo2011

Posemo2011 = ggplot(highPostData2011,aes(x=factor(highPostData2011$ThreadID),y=posemo,group=highPostData2011$ThreadID,fill = factor(highPostData2011$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Posemo2011 = Posemo2011+ggtitle("Year 2011 posemo")+ xlab("Thread") + ylab("Posemo") + labs(fill = "Thread")
Posemo2011

Anx2011 = ggplot(highPostData2011,aes(x=factor(highPostData2011$ThreadID),y=anx,group=highPostData2011$ThreadID,fill = factor(highPostData2011$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anx2011 = Anx2011 +ggtitle("Year 2011 anxiety")+ xlab("Thread") + ylab("Anx") + labs(fill = "Thread")
Anx2011

Anger2011 = ggplot(highPostData2011,aes(x=factor(highPostData2011$ThreadID),y=anger,group=highPostData2011$ThreadID,fill = factor(highPostData2011$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anger2011= Anger2011 +ggtitle("Year 2011 anger")+ xlab("Thread") + ylab("Anger") + labs(fill = "Thread")
Anger2011


# For 2005,  <<<<< 
Negemo2005 = ggplot(highPostData2005,aes(x=factor(highPostData2005$ThreadID),y=negemo,group=highPostData2005$ThreadID,fill = factor(highPostData2005$ThreadID)))+geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
Negemo2005 = Negemo2005+ggtitle("Year 2005 negemo")+ xlab("Thread") + ylab("Negemo") + labs(fill = "Thread")
Negemo2005

Posemo2005 = ggplot(highPostData2005,aes(x=factor(highPostData2005$ThreadID),y=posemo,group=highPostData2005$ThreadID,fill = factor(highPostData2005$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Posemo2005 = Posemo2005+ggtitle("Year 2005 posemo")+ xlab("Thread") + ylab("Posemo") + labs(fill = "Thread")
Posemo2005

Anx2005 = ggplot(highPostData2005,aes(x=factor(highPostData2005$ThreadID),y=anx,group=highPostData2005$ThreadID,fill = factor(highPostData2005$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anx2005 = Anx2005 +ggtitle("Year 2005 anxiety")+ xlab("Thread") + ylab("Anx") + labs(fill = "Thread")
Anx2005

Anger2005 = ggplot(highPostData2005,aes(x=factor(highPostData2005$ThreadID),y=anger,group=highPostData2005$ThreadID,fill = factor(highPostData2005$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anger2005 = Anger2005 +ggtitle("Year 2005 anger")+ xlab("Thread") + ylab("Anger") + labs(fill = "Thread")
Anger2005


# For 2006,<<
Negemo2006 = ggplot(highPostData2006,aes(x=factor(highPostData2006$ThreadID),y=negemo,group=highPostData2006$ThreadID,fill = factor(highPostData2006$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Negemo2006 = Negemo2006+ggtitle("Year 2006 negemo")+ xlab("Thread") + ylab("Negemo") + labs(fill = "Thread")
Negemo2006

Posemo2006 = ggplot(highPostData2006,aes(x=factor(highPostData2006$ThreadID),y=posemo,group=highPostData2006$ThreadID,fill = factor(highPostData2006$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Posemo2006 = Posemo2006+ggtitle("Year 2006 posemo")+ xlab("Thread") + ylab("Posemo") + labs(fill = "Thread")
Posemo2006

Anx2006 = ggplot(highPostData2006,aes(x=factor(highPostData2006$ThreadID),y=anx,group=highPostData2006$ThreadID,fill = factor(highPostData2006$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anx2006 = Anx2006 +ggtitle("Year 2006 anxiety")+ xlab("Thread") + ylab("Anx") + labs(fill = "Thread")
Anx2006

Anger2006 = ggplot(highPostData2006,aes(x=factor(highPostData2006$ThreadID),y=anger,group=highPostData2006$ThreadID,fill = factor(highPostData2006$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anger2006 = Anger2006 +ggtitle("Year 2006 anger")+ xlab("Thread") + ylab("Anger") + labs(fill = "Thread")
Anger2006


# For 2007 <<<
Negemo2007 = ggplot(highPostData2007,aes(x=factor(highPostData2007$ThreadID),y=negemo,group=highPostData2007$ThreadID,fill = factor(highPostData2007$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Negemo2007 = Negemo2007+ggtitle("Year 2007 negemo")+ xlab("Thread") + ylab("Negemo") + labs(fill = "Thread")
Negemo2007

Posemo2007 = ggplot(highPostData2007,aes(x=factor(highPostData2007$ThreadID),y=posemo,group=highPostData2007$ThreadID,fill = factor(highPostData2007$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Posemo2007 = Posemo2007+ggtitle("Year 2007 posemo")+ xlab("Thread") + ylab("Posemo") + labs(fill = "Thread")
Posemo2007

Anx2007 = ggplot(highPostData2007,aes(x=factor(highPostData2007$ThreadID),y=anx,group=highPostData2007$ThreadID,fill = factor(highPostData2007$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anx2007 = Anx2007 +ggtitle("Year 2007 anxiety")+ xlab("Thread") + ylab("Anx") + labs(fill = "Thread")
Anx2007

Anger2007 = ggplot(highPostData2007,aes(x=factor(highPostData2007$ThreadID),y=anger,group=highPostData2007$ThreadID,fill = factor(highPostData2007$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anger2007 = Anger2007 +ggtitle("Year 2007 anger")+ xlab("Thread") + ylab("Anger") + labs(fill = "Thread")
Anger2007


# For 2008, <<<
Negemo2008 = ggplot(highPostData2008,aes(x=factor(highPostData2008$ThreadID),y=negemo,group=highPostData2008$ThreadID,fill = factor(highPostData2008$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Negemo2008= Negemo2008+ggtitle("Year 2008 negemo")+ xlab("Thread") + ylab("Negemo") + labs(fill = "Thread")
Negemo2008

Posemo2008 = ggplot(highPostData2008,aes(x=factor(highPostData2008$ThreadID),y=posemo,group=highPostData2008$ThreadID,fill = factor(highPostData2008$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Posemo2008 = Posemo2008+ggtitle("Year 2008 posemo")+ xlab("Thread") + ylab("Posemo") + labs(fill = "Thread")
Posemo2008

Anx2008 = ggplot(highPostData2008,aes(x=factor(highPostData2008$ThreadID),y=anx,group=highPostData2008$ThreadID,fill = factor(highPostData2008$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anx2008 = Anx2008 +ggtitle("Year 2008 anxiety")+ xlab("Thread") + ylab("Anx") + labs(fill = "Thread")
Anx2008

Anger2008 = ggplot(highPostData2008,aes(x=factor(highPostData2008$ThreadID),y=anger,group=highPostData2008$ThreadID,fill = factor(highPostData2008$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anger2008 = Anger2008 +ggtitle("Year 2008 anger")+ xlab("Thread") + ylab("Anger") + labs(fill = "Thread")
Anger2008


# For 2009, big gap <
Negemo2009 = ggplot(highPostData2009,aes(x=factor(highPostData2009$ThreadID),y=negemo,group=highPostData2009$ThreadID,fill = factor(highPostData2009$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Negemo2009 = Negemo2009+ggtitle("Year 2009 negemo")+ xlab("Thread") + ylab("Negemo") + labs(fill = "Thread")
Negemo2009

Posemo2009 = ggplot(highPostData2009,aes(x=factor(highPostData2009$ThreadID),y=posemo,group=highPostData2009$ThreadID,fill = factor(highPostData2009$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Posemo2009 = Posemo2009+ggtitle("Year 2009 posemo")+ xlab("Thread") + ylab("Posemo") + labs(fill = "Thread")
Posemo2009

Anx2009 = ggplot(highPostData2009,aes(x=factor(highPostData2009$ThreadID),y=anx,group=highPostData2009$ThreadID,fill = factor(highPostData2009$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anx2009 = Anx2009 +ggtitle("Year 2009 anxiety")+ xlab("Thread") + ylab("Anx") + labs(fill = "Thread")
Anx2009

Anger2009 = ggplot(highPostData2009,aes(x=factor(highPostData2009$ThreadID),y=anger,group=highPostData2009$ThreadID,fill = factor(highPostData2009$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anger2009 = Anger2009 +ggtitle("Year 2009 anger")+ xlab("Thread") + ylab("Anger") + labs(fill = "Thread")
Anger2009


# For 2010, 
Negemo2010 = ggplot(highPostData2010,aes(x=factor(highPostData2010$ThreadID),y=negemo,group=highPostData2010$ThreadID,fill = factor(highPostData2010$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Negemo2010 = Negemo2010+ggtitle("Year 2010 negemo")+ xlab("Thread") + ylab("Negemo") + labs(fill = "Thread")
Negemo2010

Posemo2010 = ggplot(highPostData2010,aes(x=factor(highPostData2010$ThreadID),y=posemo,group=highPostData2010$ThreadID,fill = factor(highPostData2010$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Posemo2010 = Posemo2010+ggtitle("Year 2010 posemo")+ xlab("Thread") + ylab("Posemo") + labs(fill = "Thread")
Posemo2010

Anx2010 = ggplot(highPostData2010,aes(x=factor(highPostData2010$ThreadID),y=anx,group=highPostData2010$ThreadID,fill = factor(highPostData2010$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anx2010 = Anx2010 +ggtitle("Year 2010 anxiety")+ xlab("Thread") + ylab("Anx") + labs(fill = "Thread")
Anx2010

Anger2010 = ggplot(highPostData2010,aes(x=factor(highPostData2010$ThreadID),y=anger,group=highPostData2010$ThreadID,fill = factor(highPostData2010$ThreadID)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90))
Anger2010 = Anger2010 +ggtitle("Year 2010 anger")+ xlab("Thread") + ylab("Anger") + labs(fill = "Thread")
Anger2010


#----B. Find Correlation using highPostData----#
# Calculate the cor between negemo, posemo, anx, anger and i,we,you,heshe,they group by ThreadID
sentimentProponTemp = by(highPostData,  
                    highPostData$ThreadID, 
                    function(df) 
                      round(cor(df[12:16],df[19:22], 
                                use="complete.obs"), 
                            digits = 3))


# Show the matrix graph for thread 274018,229152,330904
corPlot = ggcorrplot(as.data.frame(sentimentProponTemp[72]),lab = TRUE) + ggtitle("Corrrelation of Thread 274018")
corPlot1 = ggcorrplot(as.data.frame(sentimentProponTemp[48]),lab = TRUE) + ggtitle("Corrrelation of Thread 229152")
corPlot2 = ggcorrplot(as.data.frame(sentimentProponTemp[105]),lab = TRUE) + ggtitle("Corrrelation of Thread 330904")


# Tidy up the data
sentimentPropon = do.call(rbind,sentimentProponTemp)
sentimentPropon = as.data.frame(sentimentPropon)
sentimentPropon$Type = rep(c("i", "we", "you","shehe","they"))

ID.Name = unique(highPostData$ThreadID)
ID.Name = as.data.frame(ID.Name[order(ID.Name)])
ID.Name = ID.Name %>% slice(rep(1:n(), each = 5))
sentimentPropon = cbind(sentimentPropon,ID.Name)

# Select the number which is >= abs(0.5)
corResult = sentimentPropon %>% filter (abs(sentimentPropon$posemo) >= 0.5 | abs(sentimentPropon$negemo) >= 0.5 | abs(sentimentPropon$anx) >= 0.5 | abs(sentimentPropon$anger) >= 0.5) 
colnames(corResult) = c("posemo","negemo","anx","anger","type","threadID")
View(corResult)
str(corResult)


install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corResult)


install.packages("xlsx")
library(xlsx)
write.xlsx(corResult, file = "mydata.xlsx")






