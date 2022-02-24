install.packages("tinytex")
install.packages("tidyverse")
install.packages("ggmap")
install.packages("ggpubr")
kmeanspkg <- c("factoextra",  "NbClust")
install.packages(kmeanspkg)
install.packages("rmarkdown")
install.packages("maps")
install.packages("data.table") 
install.packages("choro")
library(data.table) 
library(choroplethr)
library(choroplethrMaps)
library(rmarkdown)
library(tidyverse)
library(lubridate)
library(ggmap)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)
library(psych)
library(maps)
library(factoextra)
library(NbClust)
library(plotly)

#Loading the dataset
raw_data <- read.csv("C:\\Users\\jzolabe-doukou\\Desktop\\R Project\\US_Mass_Shootings.csv")
project_data <- as_tibble(raw_data)
glimpse(project_data)

project_data = project_data %>% select(-S.)


#1.1 Renaming the variables
project_data <- project_data %>% rename("City" = "Location", "Incident_Area" = "Incident.Area",
                                        "Mental_Issues" = "Mental.Health.Issues", "Policeman_Killed" = "Policeman.Killed",
                                        "Total_Victims" = "Total.victims", "Weapon_Type" = "Weapon.Type", 
                                        "Open_Close_Location" = "Open.Close.Location" )



#1.2 Wrangling the Longitute and Latitute

register_google(key = "AIzaSyA8RtFgKJu-v4vCqFNlXAa7gOajPEySXdA")   #Google API key is needed to perform the geocoding and the reverse geocoding
project_data$City <- as.character(project_data$City)
lon_lat <- geocode(project_data$City)
project_data$lon_lat <- lon_lat
project_data = project_data %>% select(-Longitude, Latitude)
project_data$Latitude <- project_data$lon_lat$lat
project_data$Longitude <- project_data$lon_lat$lon
project_data = project_data %>% select(-lon_lat)



#1.3 Types checking and types conversion

glimpse(project_data)  #No data type is needed at this point



#1.4 Variable $Age data wrangling

project_data <-separate(project_data,Age,c('Age','Age2'),2)  #Separating rows containing decimal values 
project_data$Age <- as.numeric(project_data$Age)
project_data <- project_data %>% select(-Age2)
project_data$Age[ project_data$Age == "" ] <- NA
project_data$Age[ project_data$Age == 0 ] <- NA
project_data$Age



#1.5 Variable $Date data wrangling

#First we convert the Date variable into the 'mdy date format', then separate it into three columns
project_data$Date
project_data$Date <- mdy(project_data$Date)
project_data$Day <- as.character(weekdays(project_data$Date))
project_data$Month <- as.character(month(project_data$Date, label=TRUE)) 
project_data$Year <- as.numeric(year(project_data$Date))
project_data = project_data %>% select(-Date) #Removing the Date variable


 
#1.6 General data wrangling

#Performing smart grouping

#Cleaning the variable $Race
project_data <- project_data %>%  
  mutate(Race = fct_recode(Race, "Black American" = "Black American or African American",
                           "White American" = "White American or European American",
                           "White American" = "White American or European American/Some other Race",
                           "Asian American" = "Asian", "Latino American" = "Latino", "White American" = "White",
                           "Black American" = "Black","White American" = "white","Black American" = "black",
                           "Native American" = "Native American or Alaska Native","Other Race" = "Some other race",
                           "Asian American" = "Asian American/Some other race",
                           "Black American" = "Black American or African American/Unknown",
                           "Mixed Race" = "Two or more races","Other Race" = "Other", "Unknown" = "" 
 ))


#Cleaning the variable $Cause
plot(project_data$Cause)
project_data <- project_data %>%  
  mutate(Cause = fct_recode(Cause, "Undetermined" = "", "Undetermined" = "unknown", "Domestic Dispute" = "domestic disputer", 
                           "Anger" = "anger", "Drunk/Robbery" = "drunk", "Psychological" = "psycho", "Racism" = "racism", 
                           "Drunk/Robbery" = "robbery", "Frustration" = "failing exams", "Frustration" = "frustration", 
                           "Frustration" = "revenge", "Frustration" = "suspension", "Terrorism" = "religious radicalism",
                           "Terrorism" = "terrorism", "Frustration" = "unemployement","Domestic Dispute" = "domestic dispute",
                           "Frustration" = "breakup"
                           
  ))

any(is.na(project_data$Cause))
unique(project_data$Cause)


#Cleaning the variable $Open or Close Location
project_data <- project_data %>%
  mutate(`Open_Close_Location` = fct_recode(Open_Close_Location, "Open/Close" = "","Open/Close" = "Open+Close", "Open/Close" = "Open+CLose",
                                            "Open/Close" = "Unknown" ))


#Cleaning the variable $Target
project_data <- project_data %>%
  mutate(Target = fct_recode(Target, "Random" = "random", "Relatives/Neighbors/Ex" = "Family", "Relatives/Neighbors/Ex" = "neighbors",
                             "Others" = "protestors", "Policeman/Army" = "Marines", "Relatives/Neighbors/Ex" = "Ex-Girlfriend & Family",
                             "Professional" = "TSA Officer", "Coworkers" = "Ex-Coworkers", "Students/Teachers" = "Students+Teachers",
                             "Students/Teachers" = "Teachers", "Professional" = "Social Workers", "Coworkers" = "coworkers",
                             "Club/Party" = "uninvited guests", "Club/Party" = "club members", "Students/Teachers" = "Students",
                             "Relatives/Neighbors/Ex" = "Ex-girlfriend", "Relatives/Neighbors/Ex" = "Ex-Wife & Family",
                             "Relatives/Neighbors/Ex" = "partner's family", "Others" = "Sikhs", "Students/Teachers" = "school girls",
                             "Students/Teachers" = "Students+Parents", "Others" = "monks", "Others" = "women", 
                             "Club/Party" = "birthday party bus", "Policeman/Army" = "Policeman","Relatives/Neighbors/Ex" = "Ex-Wife",  
                             "Others" = "House Owner", "Relatives/Neighbors/Ex" = "Ex-Girlfriend+random", "Relatives/Neighbors/Ex" = "Girlfriend",
                             "Others" = "black men","Others" = "basketball players", "Others" = "prayer group", "Others" = "Children",
                             "Policeman/Army" = "police","Policeman/Army" = "Trooper","Relatives/Neighbors/Ex" = "Family/Neighbors",
                             "Coworkers" = "Coworkers", "Relatives/Neighbors/Ex" = "Friends", "Relatives/Neighbors/Ex" = "Family+random",
                             "Coworkers" = "Coworker's Family", "Others" = "Congresswoman","Relatives/Neighbors/Ex" = "Ex-GirlFriend",
                             "Professional" = "psychologist+psychiatrist", "Professional" = "postmaster", "Club/Party" = "party guests",
                             "Others" = "drug dealer","Relatives/Neighbors/Ex" = "Ex-Girlfriend","Others" = "Contestant", 
                             "Random" = "rapper+random", "Students/Teachers" = "Family+students", "Policeman/Army" = "Policeman+Council Member", 
                             "Others" = "hunters","Professional" = "lawyers",  "Professional" = "welding shop employees", "Random"=""
                           
))
unique(project_data$Target)


#Cleaning the variable $Gender
unique(project_data$Gender)
project_data <- project_data %>%  
  mutate(Gender = fct_recode(Gender, "Male" = "M", "Unknown" = "M/F", "Unknown" = "Male/Female" ))


#Cleaning the variable $Mental Health Issue
unique(project_data$Mental_Issues)
project_data <- project_data %>%
  mutate(Mental_Issues = fct_recode(Mental_Issues, "Unknown" = "unknown" ))



#2.1 binary categorical variable for total victims
#Dummy variable for count of Total_Victims higer or lesser than 10
project_data$Binary <- as.numeric(project_data$Total_Victims >= 10)



#2.2 Study of categorical variables 

#Frequency of State
freq_state <- as.data.frame(table(project_data$State)) #Getting the count of the Sates
freq_state <- (subset(freq_state, freq_state$Freq > 7)) #Getting only the States with a count higher than 5
ggplot(freq_state, aes(x = reorder(Var1, -Freq), y = Freq)) + geom_bar(stat="identity", fill="steelblue") + 
  labs(title = "States Frequency", x = "", y = "Frequency") + theme_bw() + geom_text(aes(label = Freq), vjust = -0.3)


#Frequency of Race
freq_race <- as.data.frame(table(project_data$Race))
freq_race
ggplot(freq_race, aes(x = reorder(Var1, -Freq), y = Freq)) + geom_bar(stat="identity", fill="#F76731") + 
  labs(title = "Number of Shooter par Race", x = "", y = "Frequency") + theme_bw() + geom_text(aes(label = Freq), vjust = -0.3)


#Proportion of Mental Health Issues
freq_mental <- data.frame(project_data$Mental_Issues)
freq_mental <- freq_mental %>% group_by(project_data.Mental_Issues) %>% summarise(counts = n())
freq_mental  <- freq_mental %>% arrange(desc(project_data.Mental_Issues)) %>% 
  mutate(prop = round(counts*100/sum(counts),1),lab.ypos = cumsum(prop) - 0.5*prop)
freq_mental$label <- scales::percent(freq_mental$prop/100) 

ggplot(freq_mental, aes(x = "", y = prop, fill = project_data.Mental_Issues)) + 
  geom_bar(width = 1, stat = "identity", color = "White") + 
  geom_text(aes(x = 1, y = lab.ypos, label = label), color = "white") +
  coord_polar("y", start = 0)  +  theme_void() + labs(title = "Proportion of mental health issues", x = "") + 
  labs(fill = "Mental Issues")



#Frequency of Cause

freq_cause <- as.data.frame(table(project_data$Cause))
ggplot(freq_cause, aes(Var1, Freq)) +
  geom_linerange(
    aes(x = Var1, ymin = 0, ymax = Freq), 
    color = "lightgray", size = 1.5)+
  geom_point(aes(color = Var1), size = 5)+
  ggpubr::color_palette("jco") + labs(x="", title = "Frequency of cause", y = "") +
  theme_pubclean() 


#Proportion of Gender
freq_gender <- as.data.frame(table(project_data$Gender))
freq_gender
ggplot(freq_gender, aes(x= 2, y=Freq, fill=Var1))+ geom_bar(width = 1, stat = "identity", color = "white") + 
  coord_polar("y", start=0)  +  theme_void() + labs(title = "Proportion of Gnder", x = "") + 
  labs(fill = "Gender") + xlim(0.5, 2.5)


#Frequency of Target
unique(project_data$Target)
freq_target <- as.data.frame(table(project_data$Target))
freq_target <- (subset(freq_target, freq_target$Freq > 1))
ggplot(freq_target, aes(x = reorder(Var1, -Freq), y = Freq)) + geom_bar(stat="identity", fill="#22A194") + theme_bw() +
  geom_text(aes(label = Freq), vjust = -0.3)
  


#Proportion of Open or Close Location
freq_open_close <- as.data.frame(table(project_data$Open_Close_Location))
ggplot(freq_open_close, aes(x= 2, y=Freq, fill=Var1))+ geom_bar(width =, stat = "identity") + 
  coord_polar("y", start=0) + theme_void() +  xlim(0.9, 2.5) + geom_text(aes(y= Freq, label = Freq, vjust = -1.4), color = "white") +
  labs(title = "Porportion of Open or Close", fill = "Open or Close")


#Frequencies of City
freq_city <- as.data.frame(table(project_data$City))
freq_city <- (subset(freq_city, freq_city$Freq > 2))
ggplot(freq_city, aes(x = reorder(Var1, -Freq), y = Freq)) + geom_bar(stat="identity", fill="#6222A1") + theme_bw() + 
  labs(title = "Most Frequent Cities", x = "", y = "Frequency")


#Frequency of Month
freq_month <- as.data.frame(table(project_data$Month))
ggplot(freq_month, aes(x=Var1, y=Freq))+ geom_bar(stat = "identity", fill="steelblue") + theme_bw() + 
  labs(title = "Frequency of Month", x = "", y = "Frquency") 

#Chi square test of goodness Race
chi_race <- as.data.frame(table(project_data$Race))
chi_race
chi_race <- chi_race[-c(1), ]
result <- chisq.test(chi_race$Freq, p = c (0.5/10, 1.3/10, 1.8/10, 0.01/10, 0.01/10, 0.01/10, 6.37/10 ))
result


#2.3 Association between pairs of categorical variables

#Race/Mental_Issue
ggplot(data = project_data, aes(x = Race, y ="", fill = Mental_Issues, label =)) +  geom_bar(stat = 'identity', position = "fill") +
  labs(title = "Proportion of meantal health issues in each race", y = "", x = "") + coord_flip() + labs(fill = "Mental Issue")
#facet_wrap(~ Mental_Issues) 


#Race/Gender
ggplot(data = project_data, aes(x = Race, y = "", fill = Gender, label = '')) + geom_bar(stat = 'identity', position = "fill") +
  coord_flip() + labs(title = "Propotion Race/Gender", x = "", y ="")


#Gender/Cause
unique(project_data$Cause)
ggplot(project_data, aes(x=Gender, y="", fill=Cause)) + geom_bar(stat="identity", position = "fill") + 
  labs(title = "Cause of shootings by Gender", x = "", y = "")


#Cause/Mental_Issues
ggplot(project_data, aes(Cause, y ="", fill = Mental_Issues)) + geom_bar(stat="identity") + 
  labs(title = "Meantal health issues for each cause", y = "", x = "", fill = "Mental Issues")


#Race/Open_Close_Location
ggplot(project_data, aes(Race, y = Open_Close_Location, fill = Open_Close_Location)) + geom_bar(stat="identity")

#Cause/Open_Close_Location
ggplot(project_data, aes(x=Cause, y=Open_Close_Location, fill=Open_Close_Location)) + 
  geom_bar(stat="identity", position=position_dodge())


#Open_Close_Location/Mental_Issues
ggplot(project_data, aes(x=Month, y=Fatalities, fill=Gender)) + 
  geom_bar(stat="identity", position=position_dodge())


#Gender/Target
ggplot(data=project_data, aes(x=Gender, y="", fill=Target)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=""), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+ labs(x = "")
  theme_minimal()




#2.4 Association between pairs of categorical variables with Chi-squared tests

#Race/Mental_Issues
mental_race <- table(project_data$Mental_Issues, project_data$Race)
result <- chisq.test(mental_race)
result
result$observed
result$expected 
result$residuals


#Gender/Cause
gender_cause <- table(project_data$Gender, project_data$Cause)
result <- chisq.test(gender_cause)
result
result$observed
result$expected 
result$residuals


#Cause/Target
cause_target <- table(project_data$Cause, project_data$Target)
result <- chisq.test(cause_target)
result
result$observed
result$expected 
result$residuals



#3.1 Study of Quantitative variables

#Study of Age
summary(project_data$Age)
describe(project_data$Age)
ggplot(project_data, aes(x=Age)) + geom_histogram(color = "white", binwidth=5, fill = "#F12A99") +
   labs(fill = "", title = "Distribution of Age")

hist((project_data$Age),breaks=5)
boxplot(project_data$Age, notch = T, horizontal = T, col = "#669966")


#Study of Year
summary(project_data$Year)
describe(project_data$Year)
ggplot(project_data, aes(x=Year)) + geom_histogram(color = "white", binwidth=5) +
  labs(title = "Distribution of Year")


#Density of Fatalities
ggplot(project_data, aes(x = Fatalities)) + geom_density(color="white", fill="#A93FE9", adjust = 10) +
  geom_vline(aes(xintercept=mean(Fatalities)), color="white", linetype="dashed", size=1) + 
  labs(title = "Density: Total number of Fatalities")
summary(project_data$Fatalities)


#Study of Total Vicitims
summary(project_data$Total_Victims)
describe(project_data$Total_Victims)


#Percentile Total_Victims

pinf = 0.05 
psup = 0.95

binf <- quantile(project_data$Total_Victims,pinf) #Lower percentile
binf

bsup<- quantile(project_data$Total_Victims,psup) # Higher percentile
bsup



#3.2 Association between pairs of Categorical and Qualitative variables

#Study of State/Injured
#In order to facilitate the analysis, we concentrate only on States having an Injured count higher than 15
temp_data1 <- project_data %>% filter(Injured > 10) #creating a new date frame

ggplot(temp_data1, aes(x=State, y=Injured)) + geom_point(size=2, shape=15, color = "#F14B2A") + theme_light() + ylim(0, 75)


#Study of Month/Injured
ggplot(project_data, aes(x=Month, y=Injured)) + geom_point(size=2, shape=15, color = "#224EA1") + theme_light() + ylim(0, 75)


#Study of Age/Target
summary(project_data$Age)
describe(project_data$Age)
ggplot(project_data, aes(x=Age, fill = Target)) + geom_histogram(color = "white", binwidth=5) +
  labs(fill = "Target", title = "Distribution of Age by Target", y = "")


#Distribution of Age/Race by Cause
ggplot(project_data, aes(x=Age, fill = Race)) + geom_histogram(color = "blue", binwidth=2)  +
 facet_wrap(~Cause)


#Study of Age/Race
ggplot(project_data, aes(x=Age, fill = Race)) + geom_histogram(color = "white", binwidth=4)


#Study of Year/Cause
ggplot(project_data, aes(x=Cause, y = Year, fill = Cause)) + geom_boxplot()


#Study of Year/Target
ggplot(project_data, aes(x=Year, fill = Target)) + geom_histogram(color = "white", binwidth=4)


#Study of Year/Cause from 2000
ggplot(project_data, aes(x=Year, y=Fatalities, fill = Cause)) +  geom_bar(stat="identity", position=position_dodge()) +
 coord_cartesian(xlim = c(2000, 2017))


#Study of Year/Cause from 2000
ggplot(project_data, aes(x=Year, y=Fatalities, fill = Target)) +  geom_bar(stat="identity", position=position_dodge()) +
  coord_cartesian(xlim = c(2000, 2017)) + 
  labs(title = "Relationship between the cause and the total count of fatalities for the last 17 years")


#Study of the density of Fatalities/Open_Close_Location
ggplot(project_data, aes(x = Fatalities)) + geom_density(color="white", fill="#A93FE9", adjust = 10) +
  geom_vline(aes(xintercept=mean(Fatalities)), color="white", linetype="dashed", size=1) + facet_wrap( ~ Open_Close_Location)
summary(project_data$Fatalities)


#Study of Age/Mental Issue
ggplot(project_data, aes(x = Age, fill = Mental_Issues)) + 
  geom_bar(position = "dodge", binwidth=4) +
  labs(x = "Year",  y = "Number of Observations", fill = "Mental Issues")


#Study of Age/Fatalities
ggplot(project_data, aes(x=Fatalities, y=Age)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)


#Study of Cause/Ages
ggplot(project_data, aes(x=Cause, y=Age, fill=Cause)) + 
  geom_boxplot() +
  xlab("class") +
  theme(legend.position="none") +
  xlab("") +
  xlab("")


#Total_Victims per Months
ggplot(project_data, aes(Month, Total_Victims)) + geom_bar(stat = "identity", fill = "#458D31") +
  labs(x = "", y = "", title = "Total Number of victims by Month") 


#Fatalities per Months
ggplot(project_data, aes(Month, Fatalities)) + geom_boxplot(fill = "#8D315C")


#Study of Fatalities, Injured and Total_Victimes per Month
victims_month <- data.frame(project_data$Injured, project_data$Fatalities, project_data$Total_Victims, project_data$Month)
victims_month <- victims_month %>% group_by(project_data.Month) %>% 
  summarise(sum(project_data.Fatalities), sum(project_data.Injured),sum(project_data.Total_Victims))
summary(victims_month)


#correlation between Fatalities/Cause
ggplot(project_data, aes(x=Mental_Issues, y=Fatalities)) + geom_point(size=3, shape=10, color = "red") + theme_light()


#Correlation between Age and Fatalities
ggplot(project_data, aes(x=Age, y=Fatalities)) + geom_point(size=3, shape=19, color = "blue") + theme_light()


#Policemen Killed/Year
ggplot(project_data, aes(Year, y = Policeman_Killed))+ geom_path(size = 1.2)





#3.3 T-Test 

#Total_Victims/Mental_Issues
#Subset data frame containing only "Yes" and "No" value for Mental_Issues
tv_mi <- subset(project_data, project_data$Mental_Issues == "Yes" | project_data$Mental_Issues == "No") 
t.test(tv_mi$Total_Victims ~ tv_mi$Mental_Issues)


#Total_Victims/Gender
#Subset data frame containing only "Male" and "Female" value for Gender
tv_gender <- subset(project_data, project_data$Gender == "Male" | project_data$Gender == "Female") 
t.test(tv_gender$Total_Victims ~ tv_gender$Gender)


#Age/Gender
#Subset data frame containing only "Male" and "Female" value for Gender
a_gender <- subset(project_data, project_data$Gender == "Male" | project_data$Gender == "Female")
t.test(a_gender$Age ~ a_gender$Gender)


#Age/Open_Close_Location
#Subset data frame containing only "Open" and "Close" value for Open_Close_Location
a_ocl <- subset(project_data, project_data$Open_Close_Location == "Open" | project_data$Open_Close_Location == "Close") 
t.test(a_ocl$Age ~ a_ocl$Open_Close_Location)



#3.4 ANOVA

#ANOVA Total_Victims/Cause
anova_tv_cause <- aov(project_data$Total_Victims ~ project_data$Cause)
summary(anova_tv_cause)


#ANOVA Total_Victims/Mental_Issues
anova_tv_mi <- aov(project_data$Total_Victims ~ project_data$Mental_Issues)
summary(anova_tv_mi)


#ANOVA Age/Mental_Issues
anova_a_mi <- aov(project_data$Age ~ project_data$Mental_Issues)
summary(anova_a_mi)



#3.5 New table
#Creating a new table with sum of Injured, Fatalities and Total_Vitcims, grouped by Year
victims_table <- data.frame(project_data$Injured, project_data$Fatalities, project_data$Total_Victims, project_data$Year)
victims_table <- victims_table %>% group_by(project_data.Year) %>% 
  summarise(sum(project_data.Fatalities), sum(project_data.Injured),sum(project_data.Total_Victims))

victims_table <- victims_table %>% rename("Year" = "project_data.Year", "Fatalities" = "sum(project_data.Fatalities)",
                                        "Injured" = "sum(project_data.Injured)", "Total_Victims" = "sum(project_data.Total_Victims)")

#Scatterplot for each variables
plot.vt1 <- ggplot(victims_table, aes(x=Year, y=Injured)) + geom_point(size=3, shape=15, color = "#8D315C") + theme_light()
plot.vt2 <- ggplot(victims_table, aes(x=Year, y=Fatalities)) + geom_point(size=3, shape=15, color = "blue") + theme_light()
plot.vt3 <- ggplot(victims_table, aes(x=Year, y=Total_Victims)) + geom_point(size=3, shape=10, color = "red") + theme_light() 
plot.vt1
plot.vt2
plot.vt3

#3.6 Regression

plot.regression <- ggplot(victims_table, aes(x=Year, y= Total_Victims)) + geom_point(size=3, shape=10, color = "red") + 
                     theme_light() + geom_smooth(method='lm', se = FALSE) + labs(y = "", title = "Regression Analysis")
plot.regression

#New data frame containing only data from 2010 and later
temp_data2 <- victims_table %>% filter(Year >= 2010) #creating a new date frame
temp_data2

plot.regression2 <- ggplot(temp_data2, aes(x=Year, y= Total_Victims)) + geom_point(size=3, shape=10, color = "red") + 
  theme_light() + geom_smooth(method='lm', se = FALSE) + labs(y = "", title = "Regression Analysis from 2010")

plot.regression2
  


# 4 Map with Longitute and Latitude

#Styling the map
usa_map <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = "#F2F5FB",
  subunitcolor = "#000",
  countrycolor = "#174092",
  countrywidth = 0.5,
  subunitwidth = 0.5
)


#Map plotting
plot_usa <- plot_geo(project_data, lat = ~Latitude, lon = ~Longitude) %>%
  add_markers(
    text = ~paste(City),
    color = ~State, symbol = I("square"), size = I(15), hoverinfo = "text"
  ) %>%
  colorbar(title = "City via Longi") %>%
  layout(
    title = 'City via Longitude and Latitude', geo = usa_map
  )

plot_usa


# 5 K Mean Clustering Algorithm

# New dataframe
km_data <- data.frame(project_data$Total_Victims, project_data$Latitude, project_data$Longitude)
km_data <- km_data %>% rename("Total_Victims" = "project_data.Total_Victims", "Latitude" = "project_data.Latitude",
                                          "Longitude" = "project_data.Longitude")


km_data


#Determining the optimal number of clusters
fviz_nbclust(km_data, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# K Means Clustering
#set.seed(2811)
km_model <- km_data %>% kmeans(centers = 2, nstart=5)
km_data$Cluster <- km_model$cluster


# Plotting the K Means
km_map <- plot_geo(km_data, lat = ~Latitude, lon = ~Longitude) %>%
  add_markers(text = ~paste(Cluster), color = ~Cluster, symbol = I("square"), size = I(8), hoverinfo = "text") %>% 
  colorbar(title = "cluster ") %>% layout( title = 'k-means clustering', geo = usa_map )
km_map



# 6 Hierarchical Clustering Algorithm

#hc_data <- table(project_data$Total_Victims, project_data$Latitude, project_data$Longitude)
#hc_model <- dist(hc_data)
#hc_model
#hclust_avg <- hclust(dist_mat, method = 'average')
#plot(hclust_avg)


# 8 Choropleth maps to compare the different states

#Total Victims
choropleth_data <- data.frame("region"=tolower(project_data$State),"value" = project_data$Total_Victims)
choropleth_data$region <- gsub(" (u.s. state)", "", choropleth_data$region, fixed=TRUE)

newdata <- data.table(choropleth_data)
newdata <- newdata[, sum(value), by = region]

newdata <- newdata %>% rename("value"="V1")
state_choropleth(newdata, 
                 title      = "Total Victims by State", 
                 legend     = "Death", 
                 num_colors = 4)


#Fatalities
choropleth_data <- data.frame("region"=tolower(project_data$State),"value" = project_data$Fatalities)
choropleth_data$region <- gsub(" (u.s. state)", "", choropleth_data$region, fixed=TRUE)

newdata <- data.table(choropleth_data)
newdata <- newdata[, sum(value), by = region]

newdata <- newdata %>% rename("value"="V1")
state_choropleth(newdata, 
                 title      = "Total Victims by State", 
                 legend     = "Death", 
                 num_colors = 4)
