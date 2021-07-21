# LIBRARIES ---------------------------------------------------------------
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")

require(dplyr)
require(ggplot2)
require(tidyr)

# BODY --------------------------------------------------------------------

#Read and import dataset into RStudio
GT <- read.csv("C:\\Users\\Thomas\\Downloads\\GlobalTerrorism.csv") 

#Dimension of dataset
dim(GT)

#Display first 5 rows in dataset
head(GT)

#All columns in dataset
colnames(GT)

# Selected required columns
Data <- GT %>% select(iyear, imonth, country_txt, region_txt, provstate,
                      city, attacktype1_txt, attacktype2_txt, attacktype3_txt,
                      targtype1_txt, targsubtype1_txt, gname, weaptype1_txt, nkill, nwound)

# Rename column names
Data <- Data %>% rename(Year = iyear, Month = imonth, Country = country_txt,
                        Region = region_txt, State = provstate, City = city, 
                        Attack_1 = attacktype1_txt, Attack_2 = attacktype2_txt, Attack_3 = attacktype3_txt,
                        Target = targtype1_txt, Sub_Target = targsubtype1_txt,
                        Group = gname, Weapon = weaptype1_txt, Killed = nkill, Wounded = nwound)

# Display first 5 rows in modified dataset
head(Data)


# TIME SERIES DATA BY REGION -------------------------------------------------------

Data %>% group_by(Region)  %>% 
  ggplot( aes(x= Year, col=Region)) +
  geom_line(stat="count", size = 1.5) +
  labs(title="Terrorist Activities By Region & Year", x = "Year", y = "Number of incidents") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45), 
        legend.position = c(0.2,0.7), legend.background = element_rect(linetype="solid", colour ="black")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15))

# MOST AFFECTED REGION ----------------------------------------------------

Data %>% group_by(Region) %>% summarise(Freq=n()) %>%
  ggplot(aes(reorder(Region, -Freq), Freq, fill = Region, na.rm=TRUE)) + 
  geom_bar(stat="identity", col="black",position=position_dodge()) + 
  labs(title="Activity by Region",x="",y="No. of incidents") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30), legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))



# TOP AFFECTED COUNTRIES --------------------------------------------------

Data %>% group_by(Country) %>% summarise(Freq = n()) %>% top_n(10) %>%
  ggplot(aes(reorder(Country, -Freq), Freq, fill = Country, na.rm=TRUE)) + 
  geom_bar(stat="identity", col="black",position=position_dodge()) + 
  labs(title="Top Affected Countries",x="",y="No. of incidents") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30), legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))


# TOP AFFECTED CITIES -----------------------------------------------------

Data %>% group_by(City) %>% filter(City != "Unknown") %>% summarise(Freq = n()) %>% top_n(10) %>%
  ggplot(aes(reorder(City, -Freq), Freq, fill = City, na.rm=TRUE)) + 
  geom_bar(stat="identity", col="black",position=position_dodge()) + 
  labs(title="Top Affected Cities",x="",y="No. of incidents") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30), legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

# YEAR WISE TERRORIST ACTIVITY --------------------------------------------

Data %>% group_by(Year) %>% summarise(Freq = n()) %>% 
  ggplot(aes(as.factor(Year), Freq, fill = as.factor(Year), na.rm=TRUE)) + 
  geom_bar(stat="identity", col="black",position=position_dodge()) + 
  labs(title="Yearwise Terrorist Activities",x="",y="No. of incidents") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))


# METHODS USED -----------------------------------------------------------

#Find unique attack ways in dataset

unique(Data$Attack_1)
unique(Data$Attack_2)
unique(Data$Attack_3)

#Merge All Attacks into 1 variable

A1<-Data %>% drop_na(Attack_1) %>% filter(Attack_1 != "Unknown") %>% group_by(Attack_1) %>% summarise(Freq=n())
A1<- A1 %>% rename(Attack = Attack_1)

A2<-Data %>% drop_na(Attack_2) %>% filter(Attack_1 != "Unknown") %>% group_by(Attack_2) %>% summarise(Freq=n())
A2<- A2 %>% rename(Attack = Attack_2)

A3<-Data %>% drop_na(Attack_3) %>% filter(Attack_1 != "Unknown") %>% group_by(Attack_3) %>% summarise(Freq=n())
A3<- A3 %>% rename(Attack = Attack_3)

Attack_Data<-merge(A1,A2, by="Attack")
Attack_Data<-merge(Attack_Data, A3, by="Attack")
Attack_Data$Freq<- Attack_Data$Freq.x + Attack_Data$Freq.y + Attack_Data$Freq

#Plotting attack methods

Attack_Data %>%
  ggplot(aes(reorder(Attack, -Freq), Freq, fill = Attack, na.rm=TRUE)) + 
  geom_bar(stat="identity", col="black",position=position_dodge()) + 
  labs(title="Methods Employed For Attacks",x="",y="No. of incidents") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))


# TARGET ------------------------------------------------------------------

Data %>% group_by(Target) %>% summarise(Freq = n()) %>%
  ggplot(aes(reorder(Target, -Freq), Freq, fill = Target, na.rm=TRUE)) + 
  geom_bar(stat="identity", col="black",position=position_dodge()) + 
  labs(title="Targeted Groups",x="",y="No. of incidents") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
# MOST FREQUENT GROUPS ----------------------------------------------------

#Most Frequent Groups

Data %>% group_by(Group) %>% filter(Group != "Unknown") %>% summarise(Freq = n()) %>% top_n(10) %>%
  ggplot(aes(reorder(Group, -Freq), Freq, fill = Group, na.rm=TRUE)) + 
  geom_bar(stat="identity", col="black",position=position_dodge()) + 
  labs(title="Most Frequent Groups",x="",y="No. of incidents") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

#Groups with most people killed

Data %>% group_by(Group) %>% filter(Group != "Unknown") %>% 
  summarise(Freq = n(), Tot_Killed=sum(Killed)) %>% top_n(10) %>%
  ggplot(aes(reorder(Group, -Freq), Freq, fill = Group, na.rm=TRUE)) + 
  geom_bar(stat="identity", col="black",position=position_dodge()) + 
  labs(title="Groups With Most People Killed",x="",y="No. of people killed") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

#Groups with most people wounded

Data %>% group_by(Group) %>% filter(Group != "Unknown") %>% 
  summarise(Freq = n(), Tot_Wounded=sum(Wounded)) %>% top_n(10) %>%
  ggplot(aes(reorder(Group, -Freq), Freq, fill = Group, na.rm=TRUE)) + 
  geom_bar(stat="identity", col="black",position=position_dodge()) + 
  labs(title="Groups With Most People Wounded",x="",y="No. of people wounded") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90), legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))