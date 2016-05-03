#module 6 Data vis fun

library(ggplot2)

library(dplyr)

library(vcd)

library(GGally)

library(tidyr)

library(Hmisc)

water <- read.csv("Water_taste.csv") 

seaice <- read.csv("Sea_ice.csv")

shoesize <- read.csv("Shoesize.csv")
getwd()

setwd("Data Repository")

#q1 is probs best to use proportions for the clustered bar chart
# A, B, C, and D correspond to Sam's Choice,
#Aquafina, Fiji, and Tap water, respectively.
# look at mid-term solutions for prop


corsstab1 <- data.frame(prop.table(crosstab1,1))
w1 <- ggplot(data = corsstab1, aes(fill = Gender, x = First , y = Freq)) + labs( title = "s3516645 Q1 association between gender and first preference ")

w1 + geom_bar(position = "dodge", stat = "identity")

# Q2 Males tend to prefer Sam's choice more than females


# Q3 Create crosstabulation  For some reason I can't get any colour
# prop.table = crosstab1,1) proportion of F relative to B (row 
# row proportion
crosstab1 <- table(water$Gender,water$First , dnn = c("Gender","First"))
labs <- round(prop.table(crosstab1,1),2)

mosaic(crosstab1, pop = FALSE, legend = TRUE, shade = TRUE)
labeling_cells(text = labs, margin = 0)(crosstab1)
#Q4

seaice$Year <- as.Date(seaice$Year, "%Y")
q4 <-ggplot(data = seaice, aes(y = Ice, x = Year)) +labs(title = "s3516645")
q4 + geom_line() + geom_smooth()

#Q5
# it appears that the relations is a non-Negative 

#Q6
q6 <- ggplot(shoesize,aes(x = Height, y = Size) )
q6 + geom_point() + geom_smooth(method = "lm", colour = "orange")
#Q7 Positive linear relationship between Size and Height

#Q8

q8 <- ggplot(data = shoesize, aes(x = Gender, y = Size))
q8 + geom_boxplot() + coord_flip()

#Q9

q8 + geom_dotplot()
