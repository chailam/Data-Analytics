# install required packages and load
packages = function(){
  install.packages('plotly')
  install.packages('ggplot2')
  install.packages('tidyverse')
  install.packages("forecast")
  install.packages('xts')
  
  library(ggplot2)
  library(plotly)
  library(tidyverse)
  library(plyr)
  library(forecast)
  library(data.table)
  library(xts)
}
# if the packages need to be instlled/loaded, run the function:
packages()

# =========================================================================
# In this section, we load, clean and extract informtation from
# the provided file
# =========================================================================

# read in the file
df1 = read.csv('webforum.csv')

# view the data to see if there are any zero values for WC
df2 = df1[order(df1$WC),]
head(df2)

# remove any rows where the wordcount is zero 
df1 = subset(df1, WC != 0)

# =========================================================================
# we may want to remove other rows where the WC is too low 
# (explanation in written report), so in this section, we find
# out what value to cut off at
# =========================================================================

# find the median WC
median(df1$WC)

# show the distribution of the wordcount 
hist(df1$WC)

# though there are some values > 1000, the vast majority of the
# values are <500, so we look at these to check the lower bounds
ggplot(df1, aes(x = WC)) +
       geom_histogram(binwidth = 10) +
       xlim(0, 500) + 
       geom_histogram(aes(y =..density..), colour = "black", fill = "white") 
       
# cut off rows where WC is < 25 as they are not useful for insight
df3 = subset(df1, WC >= 25)

# =========================================================================
# in this section we are going to find the 20 most active users in the 
# forums (based on number of posts/occurrences of their AuthorID)
# =========================================================================

# find the number of posts (occurences/observations) for each unique 
# AuthorID
df4 = count(df1, "AuthorID")

# remove the row for AuthorID = -1 (for anonymous users)
df4 = subset(df4, AuthorID != -1)

# order the number of author ID occurences (descending)
df4 = df4[order(-df4$freq),]

# find the top 23 users
df5 = head(df4, 23)

# make empty dataframe with the same headers as the original dataframe
df6 = df1[FALSE]

# select rows that contain AuthorID of the top 20 authors and add these
# to df6
for (value in df5$AuthorID){
  temp = filter(df3, AuthorID == value)
  df6 = rbind(df6, temp)
}

# copy df3
df7 = df3

# convert the dataframe containing the top 20 authors into a vector
AuthorID_vector = df5[, "AuthorID"]
print(AuthorID_vector)

# remove the rows that contain one of the top 20 authors from the original
# dataframe
for (value in AuthorID_vector){
  df7 = filter(df7, AuthorID != value)
}

# change the date format
df6$Date = as.Date(df6$Date)
df7$Date = as.Date(df7$Date)

# =========================================================================
# now we want to compare the most active members of the forum to the other
# members (overall mean or median values)
# =========================================================================

# create the new table
newdf = data.frame(matrix(ncol = 3, nrow = 1))
temp = c("Category", "TopUsers", "Other")
colnames(newdf) = temp

# make a table that contains the median or median values for each of the
# categories for both groups
for (i in seq(7, ncol(df6))){
  category = names(df6)[i] # select the column title 
  temp_mean1 = mean(df6[,i])
  temp_mean2 = mean(df7[,i])
  newdf = rbind(newdf, c(category, temp_mean1, temp_mean2))
  
}

# delete the first row (with na values)
newdf = newdf[-1,]

# make a new temporary vector
vec = vector()

# add new column which is the most active users - others for each attribute
for (i in seq(1, nrow(newdf))){
  temp = as.numeric(newdf[i, 2]) - as.numeric(newdf[i, 3])
  vec = c(vec, temp)
}

# add these calculated values to the dataframe
newdf$Difference = vec

# above / below avg flag
newdf$Type = ifelse(newdf$Difference < 0, "below", "above")  

newdf = newdf[order(newdf$Difference), ]  # sort
# convert to factor to retain sorted order in plot.
newdf$Category = factor(newdf$Category, levels = newdf$Category)  

# graph comparing the mean values for each of the groups
# Diverging Barcharts
ggplot(newdf, aes(x=Category, y=Difference, label="difference")) + 
  geom_bar(stat='identity', aes(fill=Type), width=.5)  +
  scale_fill_manual(name="Difference (%)", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#40beae", "below"="#f8766d")) + 
  labs(subtitle="temp'", 
       title= "Diverging Bars") + 
  coord_flip()

# make a new temporary vector
vec1 = vector()

# add new column which is the most active users - others for each attribute
for (i in seq(1, nrow(newdf))){
  temp = as.numeric(newdf[i, 2]) - as.numeric(newdf[i, 3])
  temp1 = (temp/(as.numeric(newdf[i, 2])))*100
  vec1 = c(vec1, temp1)
}

# add these calculated values to the dataframe
newdf$Difference_percent = vec1

newdf = newdf[order(newdf$Difference_percent), ]  # sort
# convert to factor to retain sorted order in plot.
newdf$Category = factor(newdf$Category, levels = newdf$Category)  

# graph comparing the mean values for each of the groups
# Diverging Barcharts
ggplot(newdf, aes(x=Category, y=Difference_percent, label="difference")) + 
  geom_bar(stat='identity',aes(fill=Type), width=.5)  +
  scale_fill_manual(name="Difference (%)", 
                    labels = c("Greater proportion \n in more active members"
                               , "Smaller proportion \n in more active members"), 
                    values = c("above"="#40beae", "below"="#f8766d")) + 
  labs(title= "Percentage Difference in Language") + 
  coord_flip()

# =========================================================================
# performing statistical tests to verify findings
# =========================================================================

# make summary table for stat tests
stattests = data.frame(matrix(ncol = 2, nrow = 1))
temp_colnames = c("Category", "p_value")
colnames(stattests) = temp_colnames

# make a vector for all the unique categories

for (i in seq(7, ncol(df6))){
  category = names(df6)[i] # select the column title 
  x = df6[i]
  y = df7[i]
  ttest = t.test(x,y)
  pval = ttest$p.value
  stattests = rbind(stattests, c(category, pval))
}

# delete the first row (with na values)
stattests = stattests[-1,]

# order descending
stattests1 = stattests[order(stattests$p_value),]
View(stattests1)
# =========================================================================
# now we want to compare the most active members of the forum to the other
# members over time
# =========================================================================

# find rows for each year
by_year = function(year){
  df6.1 = subset(df6, as.numeric(format(df6$Date,'%Y')) == year)
  df7.1 = subset(df7, as.numeric(format(df7$Date,'%Y')) == year)
  
  
  # create the new table
  newdf1 = data.frame(matrix(ncol = 3, nrow = 1))
  temp = c("Category", "TopUsers", "Other")
  colnames(newdf1) = temp
  
  # make a table that contains the median or median values for each of the
  # categories for both groups
  for (i in seq(7, ncol(df6.1))){
    category = names(df6.1)[i] # select the column title 
    temp_mean1 = mean(df6.1[,i])
    temp_mean2 = mean(df7.1[,i])
    newdf1 = rbind(newdf1, c(category, temp_mean1, temp_mean2))
    
  }
  
  # delete the first row (with na values)
  newdf1 = newdf1[-1,]
  # remove last row (irrelevant)
  newdf1 = newdf1[-25,]
  
  # make a new temporary vector
  vec = vector()
  
  # add new column which is the most active users - others for each attribute
  for (i in seq(1, nrow(newd1f))){
    temp = as.numeric(newdf1[i, 2]) - as.numeric(newdf1[i, 3])
    temp1 = (temp/(as.numeric(newdf1[i, 2])))*100
    vec = c(vec, temp1)
  }
  
  # add these calculated values to the dataframe
  newdf1$Difference_percent = vec
  
  newdf1_temp = newdf1
  newdf1_temp$TopUsers = NULL
  newdf1_temp$Other = NULL
  newdf1_temp$Year = year
  return(newdf1_temp)
}

# make new df to store data
bigdf = by_year(2002)

for (i in seq(2003, 2011)){
  temp = by_year(i)
  bigdf = rbind(bigdf, temp)
}

ggplot(bigdf, aes(x=Year, y=Difference_percent, colour=Category)) + 
  geom_line() +
  labs(subtitle="This graph shows the % difference in each category for each year", 
       title= "Pecentage Difference in Language over Time") + 
  labs( x = "Year", y = "Percentage Difference")

# select only the 5 most relevant categories
temp1 = subset(bigdf, bigdf$Category == "Analytic")
temp2 = subset(bigdf, bigdf$Category == "ppron")
bigdfnew = rbind(temp1, temp2)
temp3 = subset(bigdf, bigdf$Category == "Clout")
bigdfnew = rbind(bigdfnew, temp3)
temp4 = subset(bigdf, bigdf$Category == "Tone")
bigdfnew = rbind(bigdfnew, temp4)
temp5 = subset(bigdf, bigdf$Category == "Social")
bigdfnew = rbind(bigdfnew, temp5)
temp6 = subset(bigdf, bigdf$Category == "Authentic")
bigdfnew = rbind(bigdfnew, temp6)

ggplot(bigdfnew, aes(x=Year, y=Difference_percent, colour=Category)) + 
  geom_line() +
  labs(subtitle="This graph shows the % difference in each attribute for each year", 
       title= "Pecentage Difference in Language over Time") + 
  labs( x = "Year", y = "Percentage Difference")