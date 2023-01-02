## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library('tidyverse')

# Import the data set.
df<- read.csv("turtle_sales.csv", header = TRUE)

# Print the data frame.
head (df)


#Global Sales made by each publisher 
ggplot(data = df, aes(y = Publisher) )+ geom_bar() +
  labs(title = "Global sales made by publishers",
       subtitle = "",
       caption = "")


# Create a new data frame from a subset of the sales data frame
# Remove unnecessary columns.
 df2<- select(df, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(df2)

# View the descriptive statistics.
summary(df2)

#Remove NA values
df3 <- filter(df2,NA_Sales>0,EU_Sales>0)

# View the descriptive statistics.
summary(df3)
 
# Assign the "Product" column as a factor
df3 <- mutate(df3,
              Product = as.factor(Product))   

# Check that the Product numbers have been changed to a factor.
summary(df3)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# Create scatterplots to show the relationship between Product and North
# American Sales.
qplot(Product, NA_Sales, colour = Platform, data=df3)

# Create scatterplots to show the relationship between Product and European Sales.
qplot(Product,EU_Sales, colour = Platform, data=df3)

# Create scatterplots to show the relationship between Product and Global Sales.
qplot(Product,Global_Sales, colour = Platform, data=df3)

# Create scatterplots to show the relationship between North American and
# European Sales.
qplot(NA_Sales,EU_Sales,data=df3, colour=Platform)

## 2b) Histograms
# Create histograms.
# Create histograms showing the distribution of each variable:

# Plaform
qplot(Platform,data=df3)

# Product
qplot(Product,data=df3)

# North American Sales
qplot(NA_Sales,data=df3)

# European Sales
qplot(EU_Sales,data=df3)

# Global Sales
qplot(Global_Sales,data=df3)


## 2c) Boxplots all 
# Create boxplots

# Create a boxplot for North American Sales segmented by Platform.
qplot(NA_Sales, Platform,data=df3, geom="boxplot")

# Create a boxplot for European Sales segmented by Platform.
qplot(EU_Sales, Platform,data=df3, geom="boxplot")

# Create a boxplot for Global Sales segmented by Platform.
qplot(Global_Sales, Platform,data=df3, geom="boxplot")

#Due to so many categories, it is impossible to segment by product.

#Change all of the Platform column values to characters.
df3$Platform <- as.character(df3$Platform)

# Check if the data has been updated
summary(df3$Platform)

#Platforms are Grouped by product family using sapply and switch.
df3$Console_Family <- sapply(df3$Platform,
                                            switch,
                                            '2600'="Atari",
                                            '3DS'="DS Console",
                                            DS="DS Console",
                                            GB="Gameboy",
                                            GBA= "Gameboy",
                                            GC= "GameCube",
                                            GEN= "Sega Genesis",
                                            N64= "Nintendo Game Console",
                                            NES= "Nintendo Game Console",
                                            PC= "PC",
                                            PS= "Playstation",
                                            PS2= "Playstation",
                                            PS3= "Playstation",
                                            PS4="Playstation",
                                            PSP="Playstation",
                                            PSV="Playstation",
                                            SNES="Nintendo Game Console",
                                            Wii="Wii",
                                            WiiU="Wii",
                                            X360="XBOX",
                                            XB="XBOX",
                                            XOne="XBOX")


#Arrange the products by type
df3$Console_Type <- sapply(df3$Platform,
                                          switch,
                                          '2600'="Home",
                                          '3DS'="Handheld",
                                          DS="Handheld",
                                          GB="Handheld",
                                          GBA="Handheld",
                                          GC= "Home",
                                          GEN= "Home",
                                          N64= "Home",
                                          NES= "Home",
                                          PC= "Home",
                                          PS= "Home",
                                          PS2= "Home",
                                          PS3= "Home",
                                          PS4="Home",
                                          PSP="Handheld",
                                          PSV= "Handheld",
                                          SNES="Dedicate",
                                          Wii="Home",
                                          WiiU="Home",
                                          X360="Home",
                                          XB="Home",
                                          XOne="Home")

#Wikipedia was used to find Console FAmily and Console Types.

# View the updated DataFrame.
View(df3)

# Make a boxplot
qplot(NA_Sales, Console_Family,
      data=df3, 
      geom="boxplot", 
      colour = Console_Type)

# Create a boxplot for EU Sales by Console Type
qplot(EU_Sales,Console_Family,data=df3, geom="boxplot", colour = Console_Type)

# Create a boxplot for Global Sales by Console Type
qplot(Global_Sales,Console_Family,data=df3, geom="boxplot", colour= Console_Type)


###############################################################################

# 3. Observations and insights

##The first part of this assignment involved cleaning and wrangling the turtle 
#sales data to make it more valuable for our insights. Due to too many products,
#product segmentation does not provide clear reading, so products were 
#categorised by type to enable easier reading. In addition, Consistent outliers
#were found in Wii games. The Xbox, Wii, PlayStation (home), pc, and Nintendo
#consoles generate the most revenue, followed by the Gameboy and DS.



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data 

# View data frame created in Week 4.
View(df3)

# Convert the "Product" column in the DataFrame to
# a factor.
df3 <- mutate(df3,
              Product = as.factor(Product))


# Check output: Determine the min, max, and mean values.
#NA_Sales min,max and mean value 
min(df3$NA_Sales)
max(df3$NA_Sales)
mean(df3$NA_Sales)

#EU_Sales min,max and mean value
min(df3$EU_Sales)
max(df3$EU_Sales)
mean(df3$EU_Sales)

#Global_Sales min,max and mean value
min(df3$Global_Sales)
max(df3$Global_Sales)
mean(df3$Global_Sales)


# View the descriptive statistics.
summary(df3)


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
df4 <- df3 %>%
  group_by(Product) %>%
  summarise(NA_Sales_total = sum(NA_Sales),
            EU_Sales_total = sum(EU_Sales),
            Global_Sales_total = sum(Global_Sales),
            .groups = 'drop')


# View the data frame.
view(df4)


# Join Console_Family and Console_Type to sales_by_pdt.
df4 <- left_join(df4, df3)

# View the data frame.
view(df4)

# Make a Scatterplot showing total North American Sales by
# product, segmented by Console type.
ggplot(df4,
       aes(x=NA_Sales_total,
       y=Product,
       col=Console_Type))+
  geom_point()+
  coord_flip()

# Change total sales by product to "Long form"
# so we can differentiate by sale type.
dfPivot <- data.frame(pivot_longer(df4,
                                cols=c(NA_Sales_total,
                                EU_Sales_total,
                                Global_Sales_total),
                                names_to = "Type of sale"))

# Import the ggplot library to prevent errors.
library(ggplot2)

# Use dev.off incase errors show when running the plots below.
dev.off()

# Create a faceted scatterplot displaying regional sales by Console Family.
ggplot(data=dfPivot,
       aes(x=Product,
           y=value,
           col=Type.of.sale))+
  geom_point()+
  facet_wrap(~Console_Family)+
  scale_y_continuous(breaks=seq(0,70,10))+
  labs(title="Regional Sales Distribution by Console Family",
       subtitle="Source: turtle_sales.csv",
       x="Product Id",
       y="Revenue in millions £")+
  theme_bw()

# Create a faceted boxplot displaying the regional sales breakdown by Console 
#Family.
ggplot(dfPivot,
       aes(x=Console_Family,
           y=value,
           col=Console_Type))+
  geom_boxplot()+
  facet_wrap(~Type.of.sale)+
  scale_y_continuous(breaks=seq(0,70,10))+
  labs(title="Boxplot of Console Family & Console Type Sales by Region",
       subtitle="Source: turtle_sales.csv",
       x="Console Family",
       y="Revenue in millions £")+
  theme_bw()+
  coord_flip()

# Create a faceted histogram displaying the regional sales breakdown by Console 
#Family.

ggplot(data=dfPivot,
       aes(x=value,
           fill=Type.of.sale))+
  geom_histogram()+
  facet_wrap(~Console_Family)+
  scale_y_continuous(breaks=seq(0,70,10))+
  labs(title="Regional Sales Distribution by Console Family",
       subtitle="Source: turtle_sales.csv",
       x="Product Id",
       y="Revenue in millions £")+
  theme_bw()

# Using qqplots and a line of best fit, determine the normality of each sale 
#type.

# Determine the normality of North_American Sales.
qqnorm(dfPivot$NA_Sales)
qqline(dfPivot$NA_Sales)

# Determine the normality of European Sales.
qqnorm(dfPivot$EU_Sales)
qqline(dfPivot$EU_Sales)

# Determine the normality of Global Sales.
qqnorm(dfPivot$Global_Sales)
qqline(dfPivot$Global_Sales)


# Apply the shapiro.wilk test across all sales data
# to determine normality.
shapiro.test(dfPivot$NA_Sales)
shapiro.test(dfPivot$EU_Sales)
shapiro.test(dfPivot$Global_Sales)

# Import the moments library to enable kurtosis and skewness.
library(moments)

# Determine the kurtosis values for North American Sales.
kurtosis(dfPivot$NA_Sales)

# Determine the kurtosis values for European Sales.
kurtosis(dfPivot$EU_Sales)

# Determine the kurtosis values for Global Sales.
kurtosis(dfPivot$Global_Sales)

# Determine the Skewness values for North American Sales.
skewness(dfPivot$NA_Sales)

# Determine the Skewness values for European Sales.
skewness(dfPivot$EU_Sales)

# Determine the Skewness values for Global Sales.
skewness(dfPivot$Global_Sales)

# Subset the dfPivot data to contain 
# NA_Sales, EU_Sales and Global_Sales only.
sales_subset_dfPivot <- select(dfPivot,
                          NA_Sales,
                          EU_Sales,
                          Global_Sales)


# Determine the correlation between all three sales types.
cor(sales_subset_dfPivot)


###############################################################################

# 5. Observations and insights
# Your observations and insights here...

#It is clear from the visualisation created in week 5 that there is an opposite
#linear relationship between product id and sales. Regardless of region, 
#the lower the product id, the higher the number of sales. This pattern can be
#found across all Console Families.
#PC, Playstayion  ect are seen to have a higher median and are more frequent 
#outliers than other consoles. Suggesting they sell more compared to other type
#of consoles.

#For the global Sales, EU Sales and North America, the Distributions did not
#match perfectly, as not all quantile points lie along the trend line. The
#deviation seen is a cause of concern. To help with this, normal distributed 
#random numbers should be used to see how it looks on a probability plot. If that
#relatively small number of data points are normally distributed, and most lie
#along the trend lines and a few showing in the highest and few lowest quantile, 
#this will suggest that the data is not precisely normal, but not far too off,
#we are more likely to see the results of random fluctuations at the extreme 
#ends. 

#All the p-values obtained from the Shapiro-Wilk test are less than 0.05.
#Therefore, the null hypothesis gets rejected that data from the sales collected
#from the 3 regions are normally distributed. On the other hand, the low P-value
#and not great qqnorm plots suggest that the sales data collected from the 3 
#categories are not normally distributed.
#The skewness levels for all three sales data are greater than 1, suggesting 
#positive skewness and that the distribution is positively right skewed.
#Kurtosis levels for all sales regions are greater than 3, indicating that the
#data is exposed to outliers and will produce more extreme outliers than the 
#normal distribution suggesting that the data is not reliable.
#A high positive correlation was noticed between NA-Sales and EU Sales, 
#indicating that NA Sales and EU Sales contribute to global sales but influence
#each other only moderately.



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
view(dfPivot)


# Determine a summary of the data frame.
summary(dfPivot)

#Sales total 
NA_Sales_sum <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales_sum <- c(23.80, 1.56, 0.65, 0.97, 0.52)

test_sales_value <- data.frame(NA_Sales_sum, EU_Sales_sum)
###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model1 <- lm(Global_Sales~NA_Sales, data=dfPivot)

# View the model.
model1

# View the full regression table.
summary(model1)

# Model2 Global vs EU sales.
model2 <- lm(Global_Sales~EU_Sales, data=dfPivot)

# View the model.
model2

# View the full regression table.
summary(model2)


# Model3 NA vs EU sales.
model3 <- lm(NA_Sales~EU_Sales, data=dfPivot)

# View the model.
model3

# View the full regression table.
summary(model3)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# Print residuals for each model type on a plot.
plot(model1$residuals)
plot(model2$residuals)
plot(model3$residuals)

# Basic visualisation of relationship using base R graphics
# Model 1:
plot(dfPivot$NA_Sales, dfPivot$Global_Sales)
coefficients(model1)

# Add line-of-best-fit.
abline(coefficients(model1))

# Model 2:
plot(dfPivot$EU_Sales, dfPivot$Global_Sales)
coefficients(model2)

# Add line-of-best-fit.
abline(coefficients(model2))

# Model 3:
plot(dfPivot$NA_Sales, dfPivot$EU_Sales)
coefficients(model3)

# Add line-of-best-fit.
abline(coefficients(model3))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
model4 = lm(Global_Sales~NA_Sales + EU_Sales, data=dfPivot)

# Multiple linear regression model.
summary(model4)


###############################################################################



# 4. Predictions based on given values
# Compare with observed values for a number of records.
forecast_global <- data.frame(NA_Sales=c(34.02,3.93, 2.73, 2.26, 22.08),
                    EU_Sales=c(23.80,1.56, 0.65, 0.97, 0.52))
data
predict(model4, newdata = forecast_global)

# View the predicted values.
View(forecast_global)


###############################################################################

# 6. Observations and insights
# By observation, the prediction of Global sales is overestimated. Suggestions 
#will be to use ggplotly to create interactive charts and explore breuschpagan 
#tests and other types of R-squared tests.



###############################################################################
###############################################################################




