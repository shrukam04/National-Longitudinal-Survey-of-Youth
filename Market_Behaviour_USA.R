library(readr)
library(ggplot2)
NLSY1979_1990 <- read_csv("Datasets/NLSY1979_1990.csv")
View(NLSY1979_1990)

Data = NLSY1979_1990
colnames(NLSY1979_1990)
View(Data)
head(Data)
is.na(Data)
view(Data)
na.omit(Data)

Data = Data[, c("YEAR_OF_BIRTH", "COUNTRY_OF_BIRTH", "SAMPLE_RACE", "SAMPLE_SEX", "FAMSIZE_", "JOBSNUM_", "INCOME_MAX", "EVER_DIVORCED_", "HAVING_HEALTHPLAN")]
colnames(Data)
filtered_data = Data
head(filtered_data)
filtered_data
view(filtered_data)

filtered_data$Sex = ifelse(filtered_data$SAMPLE_SEX == "MALE", 1, 0)
filtered_data$Health = ifelse(filtered_data$HAVING_HEALTHPLAN == "Yes", 1, 0)
filtered_data$Divorce = ifelse(filtered_data$EVER_DIVORCED_ == "Yes", 1, 0)
filtered_data$Birth = ifelse(filtered_data$COUNTRY_OF_BIRTH == "IN THE US", 1, 0)
filtered_data$Race = ifelse(filtered_data$SAMPLE_RACE == "BLACK" & filtered_data$SAMPLE_RACE == "HISPANIC", 1, 0)
filtered_data$Race0 = ifelse(filtered_data$SAMPLE_RACE == "NON-BLACK" & filtered_data$SAMPLE_RACE == "NON-HISPANIC", 0, 1)

view(filtered_data)
colnames(filtered_data)

#Descriptive Stats for entire Data sample
Stats = psych::describe(filtered_data)
Stats
view(Stats)

#Descriptive Stats of subset

Stats_BirthYear = psych::describe(filtered_data$YEAR_OF_BIRTH)
Stats_FamSize = psych::describe(filtered_data$FAMSIZE_)
Stats_JobsNum = psych::describe(filtered_data$JOBSNUM_)
Stats_Income = psych::describe(filtered_data$INCOME_MAX)
Stats_BirthCountry = psych::describe(filtered_data$Birth)
Stats_Race = psych::describe(filtered_data$Race)
Stats_Race0 = psych::describe(filtered_data$Race0)
Stats_Divorce = psych::describe(filtered_data$Divorce)
Stats_Health = psych::describe(filtered_data$Health)
Stats_Sex = psych::describe(filtered_data$Sex)

#Visualization for Dataset
#boxplot

Box_plot1 = ggplot(data = filtered_data, mapping = aes(x = SAMPLE_RACE, y = YEAR_OF_BIRTH, fill = SAMPLE_RACE)) +
       geom_boxplot() + theme_classic() + labs(title = "Box Plot for Races and Birth Year") +
       labs(subtitle = "Births of Races")
Box_plot1


fig <- plot_ly(ggplot2::diamonds, x = filtered_data$COUNTRY_OF_BIRTH, y = filtered_data$JOBSNUM_ , type = "box")
fig <- fig %>% layout(title = "Health Plans as per Country")
fig

#histograms

hist(filtered_data$FAMSIZE_, freq = FALSE,
     ylim = c(0,0.5),
     xlim = c(1,10),
     main = "Family Size",
     xlab = "Family Member",
     las = 1,
     col = "Light Yellow" 
     )


Hist_status = ggplot(filtered_data, aes(YEAR_OF_BIRTH)) + geom_histogram(binwidth = 0.1)+ 
              ggtitle("Birth Year") + labs(x = "Birth Counts", y = "Birth Year") 
Hist_status


#Scatterplot

Scat_Plot = plot(filtered_data$JOBSNUM_, main = "Number of Jobs", xlab = "Index", ylab = "Jobs")

Scat_Plot01 = plot(filtered_data$Race, filtered_data$FAMSIZE_, xlab = "Race", ylab = "Members of family")
abline(col = "Red", h = 4.2)


