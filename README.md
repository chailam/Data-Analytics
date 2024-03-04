# Data-Analytics
This repo is the code storage for the Data Analytics unit I completed when I was in Monash University.

It consists of two assignment tasks:
- [Complex Data Analysis using R](#complex-data-analysis-using-r)
- [Weather Forecasting Using Classification Models](#weather-forecasting-using-classification-models)

## Complex Data Analysis using R
Exploring the Social and Linguistic Dynamics of an Online Community

The code is in folder `Sentimental Analysis`. 

**Introduction:**   
There is a theory in social science, that people adopt similar patterns of language use when they interact. This task will investigate whether this concept is present in an online forum, where participants communicate with each other via conversations online. 

The purpose of this task is to analyse the language in an online forum, with specific focus on different groups within this forum and how the use of language changes over time. The investigation was split into two questions: 

- Are the sentiments expressed in language different between groups, for example, the proportion of language expressing optimism? Does this change over time?

- Is the language used by the most active and/or socially connected members of the forum dif erent to that used by the other members? Does this change over time?


The R programming language was employed for data analysis, utilizing a range of data analytics techniques.



**Report:**    
The data analysis finding has been recorded in a report named `Sentimental_Analysis_Report.pdf`.

<br>

**Q1: Are the sentiments expressed in language dif erent between groups, for example, the proportion of language expressing optimism? Does this change over time?**   
The approach taken to analysing the sentiments expressed in language in the data was as follows included
the following steps:
1. Identifying a broad subset of the data.
2. Identifying narrower subsets of data.
3. Sentiment Analysis according to Year and Time of Day.
4. Sentiment Analysis according to Year; Time of Day; and ThreadID.

![Q1 screenshot](./Sentimental%20Analysis/asset/Q1_screenshot.png)


<br>

**Q2: Is the language used by the most active and/or socially connected members of the forum dif erent to that used by the other members? Does this change over time?** 

The approach taken to analysing the language differences between the most active members of the forum
and the other users was as follows:
1. Identifying the most active members
2. Identifying the most important/significant attributes
3. Comparing the groups overall
4. Comparing the groups over time


![Q2 screenshot](./Sentimental%20Analysis/asset/Q2_screenshot.png)
![Q2 screenshot2](./Sentimental%20Analysis/asset/Q2_screenshot2.png)
<br>

**Data Source:**   
The dataset, stored in `data/webforum.csv`, comprises metadata and linguistic analysis of 20,000 forum posts spanning from 2002 to 2011.

The Linguistic analysis was conducted using Linguistic Inquiry and Word Count (LIWC), which calculates the proportion of key words used in communication to assess the prevalence of certain thoughts, feelings, and motivations.

For detailed information on LIWC and its analysis techniques, refer to:
- LIWC: [http://liwc.wpengine.com/](http://liwc.wpengine.com/)
- Language Manual: [http://liwc.wpengine.com/wp-content/uploads/2015/11/LIWC2015_LanguageManual.pdf](http://liwc.wpengine.com/wp-content/uploads/2015/11/LIWC2015_LanguageManual.pdf)

<br>

----

<br>

## Weather Forecasting Using Classification Models

The code is in folder `Weather Forecasting`. 


**Introduction:**   
The objective is to apply the classification models to develop a weather forecasting system.    

Each model will be evaluated for its predictive performance by constructing Receiver Operating Characteristic (ROC) curves. The dataset has been partitioned into a 70% training set and a 30% test set to assess the accuracy of the models.

<br>

**Data Source:**   
The dataset used in this project is under `./data/WAUS2019.csv`. It is a modified version of the Kaggle competition data: "Predict Rain Tomorrow in Australia" ([Link](https://www.kaggle.com/jsphyg/weather-dataset-rattle-package)). It comprises meteorological observations as attributes, with the class attribute "Rain Tomorrow" indicating whether it will rain the next day.

<br>

**Model Selection:**
   - Decision Tree
   - Naïve Bayes
   - Bagging
   - Boosting
   - Random Forest
   - Artificial Neural Network

<br>

**Report:**   
A comprehensive report was generated to analyze the classification models and the variables used in weather prediction. This report provides insights into the effectiveness of each model and the significance of different meteorological attributes in forecasting rain in Australia.


The report named `Weather Forecasting Classification Model.pdf`.