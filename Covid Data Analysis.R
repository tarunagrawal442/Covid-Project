library(tidyverse)
data<-read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
data<- replace_na(data, list(0))
data<-data %>% mutate_if(is.numeric, funs(replace_na(., 0)))
colnames(data)
# Selecting columns for Total Cases
data_t<- data %>% select(date,continent,location,population,total_cases)

# Filtering data with mutating total cases as a percentage of total population
data1<- data %>% select(date,continent,location,population,total_cases) %>% filter(!is.na(continent)) %>% mutate(tc_percentage=(total_cases/population)*100) %>% arrange(location)

# Grouping by location which is country
data2<- data1 %>% group_by(location) %>% summarise(popluation=mean(population,na.rm=TRUE),total_case=max(total_cases, na.rm = TRUE),tc_percentage=max(tc_percentage,na.rm = FALSE)) %>% arrange(tc_percentage)
total_cases<- data2 %>% mutate(tc_percentage=ifelse(is.na(tc_percentage),0,tc_percentage),total_case=ifelse(is.na(total_case),0,total_case), popluation=ifelse(is.na(popluation),0,popluation))

# Selecting the death data for the locations, and grouping by country
death_data_country<- data %>% select(continent,location,total_deaths) %>% filter(!is.na(continent)) %>% group_by(location) %>% summarise(total_deaths=max(total_deaths, na.rm = TRUE)) %>% arrange(desc(total_deaths))

# # Selecting the death data for the locations, and grouping by continent
death_data_continent <- death_data_country<- data %>% select(continent,location,total_deaths) %>% filter(!is.na(continent)) %>% group_by(continent) %>% summarise(total_deaths=max(total_deaths, na.rm = TRUE)) %>% arrange(desc(total_deaths))

# Daily cases total across the world 
# keep in mind to remove the null continents
newcases_daywise <- data %>% filter(!is.na(continent)) %>% select(date,new_cases,new_deaths) %>%  group_by(date) %>% summarise(new_case=sum(new_cases,na.rm=TRUE), deaths=sum(new_deaths, na.rm=TRUE))

# Deaths as a percentage of total cases
death_per_cases<- data %>% filter(!is.na(continent)) %>% select(date,continent,location,population,total_cases,total_deaths) %>% group_by(location) %>% summarise(case=max(total_cases,na.rm = TRUE), deaths=max(total_deaths,na.rm = TRUE))%>%mutate(death_percentage=(deaths/case)*100) %>% arrange(location)

# Covid Vaccinations Data
vaccinations<- data %>% filter(!is.na(continent)) %>% select(date,continent,location,population,new_vaccinations) %>% arrange(location,date)
vaccination_total<- vaccinations %>% group_by(location) %>% mutate(total_vaccinations=cumsum(ifelse(is.na(new_vaccinations),0,new_vaccinations)))
vaccination_total_percent<-vaccination_total %>% mutate(per_vaccinated=total_vaccinations/population*100) %>% mutate(per_vaccinated=round(per_vaccinated,2))
     
write_csv(total_cases,"/Users/tarunagrawal/Desktop/R Projects/totalcases_country.csv")   
write_csv(death_data_country,"/Users/tarunagrawal/Desktop/R Projects/deaths_country.csv")   
write_csv(death_data_continent,"/Users/tarunagrawal/Desktop/R Projects/deaths_continents.csv")   
write_csv(newcases_daywise,"/Users/tarunagrawal/Desktop/R Projects/newcases_daywise.csv")  
write_csv(death_per_cases,"/Users/tarunagrawal/Desktop/R Projects/deaths_percases_country.csv")  
write_csv(vaccination_total_percent,"/Users/tarunagrawal/Desktop/R Projects/totalvaccineper_country.csv")   








