# libraries
library(AER)
library(foreign)
library(tidyverse)

# set working directory
#setwd("C:/Users/jnunley/Dropbox/Teaching/Econometrics/Fall 2017/data/wooldridge")
setwd("C:/Eco307/data")

# call in data
ceosal1 <- read.csv("ceosal1.csv")

# take a look at data, dimensions, variables available, and variable formats
ceosal1
str(ceosal1)

# create variables
# salary in dollars (in lieu of 1000s of dollars)
ceosal1 <- mutate(ceosal1, salary_full=salary*1000)

# summary statistics for salary and sales
summary(ceosal1$salary) # in thousands of dollars
summary(ceosal1$sales)  # in millions of dollars

# histogram of ceo salaries and sales
ggplot(data=ceosal1, aes(ceosal1$salary))+
  geom_histogram(binwidth=100) +
  labs(title="Histogram: CEO Salaries (in thousands)", x="CEO Salary (in thousands)", y="Count")
ggplot(data=ceosal1, aes(ceosal1$sales))+
  geom_histogram(binwidth=500)+
  labs(title="Histogram: Sales (in millions)", x="Sales (in millions)", y="Count")

# plot kdensities of ceo salaries and sales
ggplot(ceosal1, aes(x=salary)) + geom_density()
ggplot(ceosal1, aes(x=sales)) + geom_density()

# summarize all variables
summary(ceosal1)

# summarize a single variable: salary and sales
summary(ceosal1$salary)
summary(ceosal1$salary_full)
summary(ceosal1$sales)

# scatterplot between salary and sales
ggplot(data=ceosal1) + 
  geom_point(mapping=aes(x=sales, y=salary))

# fit linear line
ggplot(data=ceosal1) + 
  geom_point(mapping=aes(x=sales, y=salary)) + 
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x, color="red", se=TRUE)

# fit quadratic line
ggplot(data=ceosal1) + 
  geom_point(mapping=aes(x=sales, y=salary)) + 
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x+I(x^2), color="green", se=TRUE) 

# fit local regression line (loess)
ggplot(data=ceosal1) + 
  geom_point(mapping=aes(x=sales, y=salary)) + 
  geom_smooth(mapping=aes(x=sales, y=salary), method="loess", span=0.8, se=TRUE) 
  
# fit linear, quadratic and loess together
ggplot(data=ceosal1) + 
  geom_point(mapping=aes(x=sales, y=salary)) + 
  geom_smooth(mapping=aes(x=sales, y=salary), method="loess", span=0.8, se=FALSE) +
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x, color="red", se=FALSE) +
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x+I(x^2), color="green", se=FALSE)


####################################################################################################


# create subset of data
ceosal1_subset <- filter(ceosal1, sales<=15000) 
ceosal1_subset <- filter(ceosal1_subset, salary<=2000)

# fit linear line
ggplot(data=ceosal1_subset) + 
  geom_point(mapping=aes(x=sales, y=salary)) + 
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x, color="red", se=TRUE) 

# fit quadratic line
ggplot(data=ceosal1_subset) + 
  geom_point(mapping=aes(x=sales, y=salary)) + 
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x+I(x^2), color="green", se=TRUE) 

# fit local regression line (loess)
ggplot(data=ceosal1_subset) + 
  geom_point(mapping=aes(x=sales, y=salary)) + 
  geom_smooth(mapping=aes(x=sales, y=salary), method="loess", span=0.8, se=TRUE) 
  
# fit linear, quadratic and loess together
ggplot(data=ceosal1_subset) + 
  geom_point(mapping=aes(x=sales, y=salary)) + 
  geom_smooth(mapping=aes(x=sales, y=salary), method="loess", span=0.8, se=FALSE) +
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x, color="red", se=FALSE) +
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x+I(x^2), color="green", se=FALSE)


####################################################################################################

# summary statistics for industry variables
summary(ceosal1$indus)
summary(ceosal1$finance) 
summary(ceosal1$consprod) 
summary(ceosal1$utility) 

# industry id variable
ceosal1_subset <- mutate(ceosal1, industry=(indus==1 & finance==0 & consprod==0 & utility==0 ) + 
                    2*(indus==0 & finance==1 & consprod==0 & utility==0) +
                    3*(indus==0 & finance==0 & consprod==1 & utility==0) +
                    4*(indus==0 & finance==0 & consprod==0 & utility==1))

# scatterplot with indstries indicated with different colors
ggplot(data=ceosal1_subset) + 
  geom_point(mapping=aes(x=sales, y=salary, alpha=industry))

# scatterplot with indstries indicated with different colors with loess
ggplot(data=ceosal1_subset) + 
  geom_point(mapping=aes(x=sales, y=salary, alpha=industry)) + 
  geom_smooth(mapping=aes(x=sales, y=salary), method="loess", span=0.8, se=TRUE)

# scatterplot with indstries indicated with different colors with loess, linear and quadratic
ggplot(data=ceosal1_subset) + 
  geom_point(mapping=aes(x=sales, y=salary, alpha=industry)) + 
  geom_smooth(mapping=aes(x=sales, y=salary), method="loess", span=0.8, se=FALSE) +
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x, color="red", se=FALSE) +
  stat_smooth(aes(y=salary, x=sales), method="lm", formula=y~x+I(x^2), color="green", se=FALSE)

# scatterplots with loess by industry (see code above)
ind_labels <- as_labeller(c(`1`="Industrial", `2`="Financial", 
                           `3`="Consumer Products", `4`="Utility/Transportation"))
plot <- ggplot(data=ceosal1_subset) + 
  geom_point(mapping=aes(x=sales, y=salary)) + 
  geom_smooth(mapping=aes(x=sales, y=salary), method="loess", span=0.8, se=TRUE)
plot + facet_wrap(~industry, labeller=ind_labels)


####################################################################################################






