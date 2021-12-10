


### *** Survey: cleaninng data, linear regression visualization plots***

# 1 Data set structure --------------------------------
### ** DATA SET STRUCTURE **
str(people)
nrow(people)
class(people) #[1] "tbl_df" "tbl" "data.frame"   # This data set has three classes, let`s treat NA`s and class() again!
mean(people$age, na.rm = TRUE) #It doesn´t work because it`s a character class, let`s change the class, and try again the code:

people1 <- select(people, "gender", "country", "age", "education", "income", "work_status")
sapply(people1, class) #Class of all columns = "character" 

#Working with NA´s

people1$age <- as.numeric(as.character(people1$age))
people1$education <- as.numeric(as.character(people1$education))
people1$income <- as.numeric(as.character(people1$income))
sapply(people1, class)
mean(people1$age, na.rm = TRUE) # (Try again!) Now it`s works because it`s numeric class

### ** DATA SET CLASS AND CLEANING**
### ** NUMERICAL SETTINGS **
# 2 Variables data set classes and modeling some information-------------------
# First change the class:
# Create people1 and change from "character" to "numeric" class to: age, education and income:

# age and income (Problems: Outliers, -0 age and more)
plot(people1$age, people1$income) # Plot age and income, there are a outlier point [PICTURE 3.1]
which.max(people1$income) # See the row that has a maximum income
people1[9002,] # Look that row
people1 <- people1[-c(9002),] #Remove the outlier that was in row 9002 (INFORM THAT ROW WAS A 40.000 INCOME VALUE) ### ### ###[PICTURE 3.3 PARECE I GESS]
plot(people1$age, people1$income) #Now the plot doesn`t have the outlier, but appear other in x-value, which.max:

which(people1$age >= 100) # Which registers are grater than or equal to 100 age (years)
people1[c(212, 1548, 1601, 2331, 4278, 7898),] #We have here these rows with more than 100 ages
people1 <- people1[-c(212, 1548, 1601, 2331, 4278, 7898),] #Remove them (INFORM ABOUT THESE ROWS) [PICTURE 3.4]### ### ### ### ### ### ### ### ### ### ###
plot(people1$age, people1$income) # NOW APPEAR NA`s less han 0 ages, so let`s remove less than 18:

which(people1$age < 0)
people1[c(2906, 5013, 5814, 5990, 6572, 8749),]

which(people1$age < 18)
people1[c(223, 305, 440, 1247, 1397, 1415, 1466, 1507, 1713, 1718, 2049, 2593, 2703, 2739, 2859, 2870, 2906, 
          2955, 3268, 3396, 3434, 3575, 3837, 4031, 4350, 4450, 4738, 4888, 4922, 4987, 5013, 5137, 5298, 
          5388, 5695, 5733, 5814, 5890, 5904, 5990, 6026, 6080, 6375, 6455, 6572, 6785, 6930, 7072,
          7229, 7509, 7712, 7875, 8076, 8224, 8580, 8643, 8749, 8751, 8896),]

people1 <- people1[-c(223, 305, 440, 1247, 1397, 1415, 1466, 1507, 1713, 1718, 2049, 2593, 2703, 2739, 2859, 2870, 2906, 
                     2955, 3268, 3396, 3434, 3575, 3837, 4031, 4350, 4450, 4738, 4888, 4922, 4987, 5013, 5137, 5298, 
                     5388, 5695, 5733, 5814, 5890, 5904, 5990, 6026, 6080, 6375, 6455, 6572, 6785, 6930, 7072,
                     7229, 7509, 7712, 7875, 8076, 8224, 8580, 8643, 8749, 8751, 8896),]

plot(people1$age, people1$income) #Now the visualization it`s clear and more well adecuaded [PHOTO 3.7]


#Education level

people1 <- people1 %>%
  mutate(Education_Level = case_when(
    education == 0  ~ "No education",
    education >= 1 & education <= 6 ~ "Basic",
    education >= 7 ~ "High"))

# 3 NA´s exploratory --------------------------------------------------------

# This data set has a lot of Missing Values or NA`s Non Availables Values.
# View some functions to identify missing values usinng is.na()which return a
# logical vector with TRUE. is.na() works on vectors, lists, matrice and data frames.

# How many NA`s are in people data set?

which(is.na(people1)) #See 467 observations where are NA`s
sum(is.na(people1)) #Now have the confirmation, and yes, are 467 NA´s observations

# What variables has these NA´s? columns called "country" and "20"

colSums(is.na(people1)) #Shows all columns and the quantity of NA`s that have these columns

# 4 NA´s replacements (With critical thinking, these decisions can changes) -----------------------------------------------------

# NA´s repleacement in age - Numerical Variable (Critical thinking = age mean)
which(is.na(people1$age)) #491 NA´s were found.
mean(people1$age, na.rm = TRUE) #[1] 30.13061
median(people1$age, na.rm = TRUE) #[1] 28
people1$age[is.na(people1$age)] <- mean(people1$age, trim = 0, na.rm = TRUE)

# NA´s repleacement in education - Numerical Variable (Critical thinking = education mean)
which(is.na(people1$education)) #425 are NA, these NA are going to be repleaced by the mean. (Critical Thinking or not, it`s a simple decision)
mean(people1$education, na.rm = TRUE) #[1] 8.265656 #These NA`s can be repleaced by the arithmetical average or...
median(people1$education, na.rm = TRUE) # [1] 7 # Or can be repleaced by the median or central value in all that education variable
people1$education[is.na(people1$education)] <- mean(people1$education, na.rm = TRUE) #All NA´s were replaced by the mean ####### COULD BE REPLACED BY MEDIAN TOO, BUT MEAN IT`S THE CRITERIA IN THIS PROJECT

# NA´s repleacement in income - Numerical Variable (Critical thinking = income mean)
which(is.na(people1$income)) # 1568 NA´s values
mean(people1$income, na.rm = TRUE) #[1] 90.34501
median(people1$income, na.rm = TRUE) # [1] 23
people1$income[is.na(people1$income)] <- mean(people1$income, na.rm = TRUE) #This decision NA`s replacement, changed the median to 35.
# rename(people1, incomeweek  = income) #OPTIONAL
# which.min(people1$incomeweek) #OPTIONAL
# people1[51,] # 2000, from Perù and doesn`t work #OPTIONAL

# NA´s repleacement in gender and "change the name" - Categorical Variable (Critical thinking = ("M" & "F") to ("Male" & "Female"))
which(is.na(people1$gender)) #0 NA´s were found
people1$gender[people1$gender == "m"] <- "Male"
people1$gender[people1$gender == "f"] <- "Female"
people1$gender[people1$gender == "null"] <- "Prefer not to say"

# NA`s country (Critical thinking = Replace NA´s for "World" word)
which(is.na(people1$country)) #464 NA`s were found
people1$country[people1$country == "NA"] <- "World"
people1$country[is.na(people1$country)] <- "World" 

# NA´s repleacement in work_status (Critical thinking = "null" replace to "Without answer")
which(is.na(people1$work_status))
people1$work_status[people1$work_status == "null"] <- "Without answer"
people1$work_status[people1$work_status == 0] <- "Others" #Doing a ggplot in line 138, i saw these values, now i assign "Others" to them.
people1$work_status[people1$work_status == 1] <- "Others"
people1$work_status[people1$work_status == 2] <- "Others"
people1$work_status[people1$work_status == 3] <- "Others"
people1$work_status[people1$work_status == 4] <- "Others"
people1$work_status[people1$work_status == 6] <- "Others"
people1$work_status[people1$work_status == 11] <- "Others"
## ## ## ## ## ## ## All NA`s have been changed ## ## ## ## ## ## ## 
    
# 5 Inference, new variable products and plots------

table(people1$country)
levels(people1$work_status)

# Who answered the survey? "Employees" and "Do not work" people:

  people1 %>% filter(age %in% 18:100)%>% 
  ggplot(aes(x = work_status, y = age, fill = education))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = "jitter", aes(color = gender))

# What`s the income of "Employees" and "Do not work"?
  
    people1 %>% filter(age %in% 18:100 & work_status %in% c("Employee", "Do not work"))%>% 
    ggplot(aes(x = work_status, y = income))+
    geom_boxplot(outlier.shape = NA)+
    geom_point(position = "jitter", aes(color = gender))

# Sales focus on: "Employees" What course we can sale to them? Most of them are female:

    table(people1$work_status, people1$gender)

# The Sales Manager decision was prepare Business Magazines to people that answered the survey
# Design magazines variable and inncorporate to people1 data frame
    
sample1 = people1 %>% filter(age %in% 30:35 & work_status %in% c("Business owner", "Employee", "Self-employed"))

BM <- c("BusinnessMen Magazine")
BMP <- c(250)
Business_Men_Magazine <- data.frame(BM, BMP, stringsAsFactors = FALSE)


BW <- c("BusinessWomen Magazine")
BWP <-c(200)
Business_Women_Magazine <- data.frame(BW, BWP, stringsAsFactors = FALSE)


people1 <- people1 %>% 
  mutate(product = case_when(
    gender == "Female"~ "BusinessWomen Magazine",
    gender == "Male" ~ "BusinessMan Magazine"))

people1 <- people1 %>% 
  mutate(annual_price = case_when(
  gender == "Female" ~ 200,
  gender == "Male" ~ 250)
)

# Now we can viauslize the quantity of magazine per genre:

people1 %>% ggplot(aes(x = product)) +
  geom_bar(mapping = aes(x = product, color = gender, fill = gender))+
  theme_bw(base_size = 10, base_rect_size = 1, base_line_size = 1.5)+
  labs(y = "Magazine Quantity", x = "Magazine Name", title = "Magazine Quantity per gender")
  



#
# 6 Linear regression and plots ----------------------------------------------------


country.sales.lm <- lm(annual_price ~ gender + income, data = people1)
country.sales.lm2 <- lm(income ~ gender + annual_price, data = people1)
country.sales.lm3 <- lm(income ~ gender + country, data = people1)
summary(country.sales.lm3)


#Based on the latest three tests, we can see that lm3, shows a relationship with the income, gender and country:

summary(country.sales.lm3)

# The estimation of gender and country in Afganistan is 147 and Taiwan is -55
# This means that for every 1% of the gender there`s a correlated of 147% in Afganistan in incomes
# And for everyone 1% in Taiwan there´s a correlated of -55.

# The Standard error in Afganistan is 147 and in Taiwain it`s 142 it`s notoriuous in both countries, and all others has over 100.
# The T-statistics or T-values are all in -0 and 1, exceptionally Madagascar that has 3.562
# The p-values reflects these errors all over zero and there is almost zero probability that this effect is due to chance.

# Afghanistán and Taiwán, here in this plot, we can see the enormeus difference in income:

lm3plot <- people1 %>% select(country, income, gender) %>% filter(country %in% c("Afghanistan", "Taiwan"))
lm3plot %>% ggplot(aes(country, income))+
  geom_point(aes(country, income, color = gender))+
  labs(y = "Country", x = "Income", title = "The maximum and the minimum countries correlated")

# Taiwan Income and gender and product:

people1 %>% filter(country == "Taiwan") %>% 
  ggplot()+
  geom_bar(mapping = aes(x = gender, y = income, color = product, fill = product), stat = "identity")



# Afghanistan Income and gender and product:

people1 %>% filter(country == "Afghanistan") %>% 
  ggplot()+
  geom_bar(mapping = aes(x = gender, y = income, color = product, fill = product), stat = "identity")

# In all data, the maximun concentration it`s less than 500 income between 20 and 40 years old:

ggplot(people1, aes(x = age, y = income))+
  geom_point(aes(x = age, y = income, color = gender))+
  labs(y = "Income", x = "Age", title = "Income and Age Visualization colored by gender")

# More age, more income:
lm3plot2 <- ggplot(people1, aes(age, income))+geom_point()
graphlm3 <- lm3plot2 + geom_smooth(method = "lm", col = "black") + scale_x_log10()
graphlm3 #Here it is a linear

#Obviously y axis in this line is disproportionated, but anyway we can see the line better
graphlm3 <- lm3plot2 + geom_smooth(method = "lm", col = "black") + scale_x_log10() + scale_y_log10()

#or:
ggplot(data = people1, aes(age, income))+
  geom_smooth(method = lm, se = FALSE)+
  geom_point(aes(color = gender))

# Much better is if we take a sample more or equal to 250:
people1 %>% 
  filter(income <= 250) %>%  # Less or equal to 250 of income
  ggplot(aes(x = age, income))+
  geom_smooth(method = lm, se = FALSE)+
  geom_point(aes(color = gender))+
  labs(x = "Age", y = "Income", title = "Less or equal to 250 week income")

people1 %>% ggplot(aes(x = age, y = income))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)




#Linar regression line

add_line <- function(slope_people){
  people1_summary <- people1 %>% 
    summarize(N = n(), r = cor(age, income), 
              mean_age = mean(age), mean_income = mean(income),
               sd_age = sd(age), sd_income = sd(income)) %>% 
    mutate(true_slope = r * sd_age / sd_income,
           true_intercept = mean_income - true_slope*mean_age) # This vector shows means, slope, sd and intercept
  p <- ggplot(data = people1, aes(x = age, y = income)) +
    geom_point()+
    geom_point(data = people1_summary,
               aes(x = mean_age, y = mean_income), # This scatter plot intercept mean age and mean income
               color = "red", size = 3) 
  my_dat <- people1_summary %>% 
    mutate(slope_people = slope_people,
           my_intercept = mean_income - slope_people * mean_age)
  p + geom_abline(data = my_dat,
                aes(intercept = my_intercept, slope = slope_people), color = "dodgerblue")
  }

add_line(slope_people = 1.0) # And yes!, we have a line and a red point interception mean income and mean age interception

###

# -4.79636e-15 Residuals:

mod <- lm(income ~ gender + country, data = people1)
summary(mod)
coef(mod)
mean(residuals(mod))

#### 

# The argument function take the object and gives the data that adjust to the model, with other information (residuals and others)
library(broom)

people_1_tidy <- augment(mod)
glimpse(people_1_tidy)


# Now RMSE:
mean(residuals(mod))
sqrt(sum(residuals(mod)^2)/df.residual(mod))
# RMSE it is: 273.0691 



# Simple linear regression consists of generating a regression model (equation of a line) that allows us to explain the linear relationship that exists between two variables. The dependent or response variable is identified as Y and the predictor or independent variable as X
summary(mod)
people_1_tidy %>% 
  summarize(var_y = var(income), var_e = var(.resid)) %>% 
  mutate(R_squared = 1 - var_e / var_y) #We ger 0.0392, so the correlation between the income and the gender and country it`s 0.0392


# This statistic indicates the percentage of the variance in the dependent variable that the independent variables explain collectively. R-squared measures the strength of the relationship between your model and the dependent variable on a convenient 0 – 100% scale. This result of 0.0392 of R Sqared means that has a low relationship between income and country.

# Other value is .cooksd that were connsider in the line glimpse(people_1_tidy), the measurement of influence, this is used to identify some outliers:
  
mod %>% 
  augment() %>% 
  arrange(desc(.cooksd)) %>%
            head()

At the begining, this project removed the outliers and reduced th NAS values. But now more NAS were apprear:

These NAS are in product and annual_price, lets remove them and again check NAS presence:
  
which(is.na(people1$product)) 
people1[17,]


people1$product[people1$product == "NA"] <- "Offertwomagazines"
people1$product[is.na(people1$product)] <- "Offertwomagazines"
people1$annual_price[people1$annual_price == "NA"] <- 200
people1$annual_price[is.na(people1$annual_price)] <- 200


And now we don`t have NAS:
  
sum(is.na(people1)) 
colSums(is.na(people1))

mean(people1$age)
mean(people1$income)


# And now that definetely don´t have NAS, and we know that we´ll have more alternative to make sellings to 
# over 30 years old people, let`s see the countries were people have more income, we have 366 people that is 
# More or equal to 30 years old and has an income of more or equal tan 120 dollars per week
# These people will be the focus at the beginning, because as they have more income,they could buy the magazine more quicky:

# The survey were answered inn 179 counntries
unique(people1$country)

# But the main business periscope is in people older than 30 ans more than 250 dollars of income per week:
topsales <- people1 %>% filter(age >= 30 & income >= 250) 

# Here it is the main business periscope, 155 countries:
topsales %>% ggplot(aes(x = country, y = income, color = gender)) + 
  geom_point(aes(x = country, y = income))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))

# 61 countries will be the main focus of the magazine business
unique(topsales$country)





++
  
  
  
  
  
  