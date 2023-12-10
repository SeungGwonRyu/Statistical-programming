# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
options(scipen=999)
# Importing the data
setwd("/Users/Ryu Seung Gwon/Library/CloudStorage/OneDrive-한양대학교/3-2/통계프로그래밍과 데이터분석/team project/geo")
geo3 <- read.csv("geo3.csv", header = T)
geo2 <- read.csv("geo2.csv", header = T)
geo1 <- read.csv("geo1.csv", header = T)

### Exploratory Data Analysis (EDA)

# [geo1] Check variables
names(geo1)  
str(geo1)
summary(geo1)
dim(geo1)
head(geo1, 10)

# [geo1] Statistical Analysis 

# Find the Missing Values
sum(is.na(geo1))

# Find the duplicate data and Drop the duplication
duplicated(geo1)
length(unique(geo1$PIN1))
geo1 <- distinct(geo1)

# Find the Outlier
table(geo1$COMMUNITY_NAME)
table(geo1$COMMUNITY_ID)

# [geo2] Check variables
names(geo2)  
str(geo2)
summary(geo2)
dim(geo2)
head(geo2, 10)

# [geo2] Statistical Analysis 

# Find the Missing Values
sum(is.na(geo2))

# Find the duplicate data
duplicated(geo2)
length(unique(geo2$PIN2))

# Find the Outlier
table(geo2$HOUSE_UNIT)
table(geo2$PROPERTY_TYPE)

# [geo3] Check variables
names(geo3)  
str(geo3)
summary(geo3)
dim(geo3)
head(geo3, 10)

# Find the Missing Values and Drop the Missing values
#sum(is.na(geo3))
#geo3 <- na.omit(geo3)


# Find the duplicate data Drop the duplication
duplicated(geo3)
length(unique(geo3$PIN3))
geo3 <- distinct(geo3)

# Check for empty values
sum(geo3$ASSESSOR_FINAL_ADDR_LN == "")
sum(geo3$ASSESSOR_CITY_NM == "")
sum(geo3 == "")

#################################################################

#geo2에 geo1을 join
geo_temp = geo1 %>% inner_join (geo2, by = c("PIN1" = "PIN2"))

geo1 %>% count(PIN1)
geo2 %>% count(PIN2)
geo_temp %>% count(PIN1)

# Find the Missing Values and Drop the Missing values
sum(is.na(geo_temp))

# Find the duplicate data Drop the duplication
duplicated(geo_temp)
length(unique(geo_temp$PIN1))

#geo_temp에 geo3을 join
length(unique(geo3$PIN3)) #191731

geo_master = geo3 %>% inner_join(geo_temp, by = c("PIN3" = "PIN1"))

sum(is.na(geo_master))

city_counts <- table(geo_master$ASSESSOR_CITY_NM)
print(city_counts)

sum(geo_master == "")

#balanced panel 만들기
geo_2017 <- geo_master[geo_master$PROPERTY_TYPE_YR == 2017, ]

geo_2017_unique <- geo_2017 %>%
  group_by(PIN3) %>%
  filter(row_number() == 1)
##
geo_2018 <- geo_master[geo_master$PROPERTY_TYPE_YR == 2018, ]

geo_2018_unique <- geo_2018 %>%
  group_by(PIN3) %>%
  filter(row_number() == 1)
##
geo_2019 <- geo_master[geo_master$PROPERTY_TYPE_YR == 2019, ]

geo_2019_unique <- geo_2019 %>%
  group_by(PIN3) %>%
  filter(row_number() == 1)

combined_geo <- bind_rows(geo_2019_unique %>% mutate(Year = 2019),geo_2018_unique %>% mutate(Year = 2018),geo_2017_unique %>% mutate(Year = 2017))

combined_geo <- combined_geo %>% arrange(PIN3, PROPERTY_TYPE_YR)

combined_geo <- combined_geo %>% select(-Year)

geo_final <- combined_geo %>%
  group_by(PIN3) %>%
  filter(all(c(2017, 2018, 2019) %in% PROPERTY_TYPE_YR) & length(unique(PROPERTY_TYPE_YR)) == 3)

duplicated(geo_final)
length(unique(geo_final$PIN3))

#library(openxlsx)

#write.xlsx(geo_final, "geo_final", rowNames = FALSE)

table(geo_final$PROPERTY_TYPE_YR)

table(geo_final$ASSESSOR_CITY_NM)

#sum(is.na(geo_final))

#geo_final_2 <- na.omit(geo_final)

##############################################################

geo_final_processed <- geo_final %>%
  group_by(PIN3) %>%
  mutate(LONGITUDE_NBR = sample(LONGITUDE_NBR, 1))

unique_longitude_count <- sort(tapply(geo_final_processed$LONGITUDE_NBR, geo_final_processed$PIN3, function(x) length(unique(x))), decreasing = FALSE)

print(unique_longitude_count)

geo_final_processed2 <- geo_final_processed %>%
  group_by(PIN3) %>%
  mutate(LATITUDE_NBR = sample(LATITUDE_NBR, 1))

unique_latitude_count2 <- sort(tapply(geo_final_processed2$LATITUDE_NBR, geo_final_processed2$PIN3, function(x) length(unique(x))), decreasing = FALSE)

print(unique_latitude_count2)

#library(openxlsx)

#write.xlsx(geo_final_processed2, "geo_final_processed2", rowNames = FALSE)


### Chicago city
table(geo_final_processed2$COMMUNITY_NAME)
colnames(geo_final_processed2)

unit <- c('unit_1', 'unit_2 to4', 'unit5 or more')
counts_2017 <- c(315156 + 39894, 175543 + 181361, 145805 + 53461 + 75083 + 223652)
counts_2018 <- c(322986 + 43995, 179781 + 182797, 141270 + 50446 + 75983 + 226903)
counts_2019 <- c(306614 + 42836, 172703 + 184383, 138443 + 51378 + 74624 + 243963)

data <- data.frame(
  REF_AREA2 = rep("Chicago city", each = 9),
  ACS_YR = rep(c(2017, 2018, 2019), each = 3),
  unit = rep(unit, times = 3),
  counts = c(counts_2017, counts_2018, counts_2019)
)

unit_counts_17 <- c("unit_1" = 355050, "unit_2 to4" = 356904, "unit5 or more" = 498001)
total_units_17 <- sum(unit_counts_17)
ratios_17 <- unit_counts_17 / total_units_17 * 100
ratios_17

unit_counts_2018 <- c("unit_1" = 366981, "unit_2 to4" = 362578, "unit5 or more" = 494602)
total_units_2018 <- sum(unit_counts_2018)
ratios_2018 <- unit_counts_2018 / total_units_2018 * 100
ratios_2018

unit_counts_2019 <- c("unit_1" = 349450, "unit_2 to4" = 357086, "unit5 or more" = 508408)
total_units_2019 <- sum(unit_counts_2019)
ratios_2019 <- unit_counts_2019 / total_units_2019 * 100
ratios_2019

data$ratios <- c(ratios_17,ratios_2018, ratios_2019)

ggplot(data, aes(x = factor(ACS_YR), y = ratios, fill = REF_AREA2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "'unit5 or more' Ratios by Year in Cicago city",
       x = "Year",
       y = "Ratios (%)") +
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal()


###Cook county, Illinois
unit <- c('unit_1', 'unit_2 to4', 'unit5 or more')
counts_2017_1 <- c(878456+112460 , 213212+237149, 226281+103275+126533+278434)
counts_2018_1 <- c(881389+114197, 219387+234748, 222055+102976+121815+287797)
counts_2019_1 <- c(879407+118407, 213094+237053, 211634+101333+124187+303671)

data_2 <- data.frame(
  REF_AREA2 = rep("Cook county, Illinois", each = 9),
  ACS_YR = rep(c(2017, 2018, 2019), each = 3),
  unit = rep(unit, times = 3),
  counts = c(counts_2017_1, counts_2018_1, counts_2019_1)
)

counts_2017_1
unit_counts_17_1 <- c("unit_1" = 990916, "unit_2 to4" = 450361, "unit5 or more" = 734523)
total_units_17_1 <- sum(unit_counts_17_1)
ratios_17_1 <- unit_counts_17_1 / total_units_17_1 * 100
ratios_17_1

counts_2018_1
unit_counts_18_1 <- c("unit_1" = 995586, "unit_2 to4" = 454135, "unit5 or more" = 734643)
total_units_18_1 <- sum(unit_counts_18_1)
ratios_18_1 <- unit_counts_18_1 / total_units_18_1 * 100
ratios_18_1

counts_2019_1
unit_counts_19_1 <- c("unit_1" = 997814, "unit_2 to4" = 450147, "unit5 or more" = 740825)
total_units_19_1 <- sum(unit_counts_19_1)
ratios_19_1 <- unit_counts_19_1 / total_units_19_1 * 100
ratios_19_1

data_2$ratios <- c(ratios_17_1,ratios_18_1, ratios_19_1)


###Illinois
unit <- c('unit_1', 'unit_2 to4', 'unit5 or more')
counts_2017_2 <- c(3162927+300920, 302686+347719, 338175+200939+199057+373789)
counts_2018_2 <- c(3139808+310106, 312337+346724, 347987+208127+195272+386854)
counts_2019_2 <- c(3149162+313725, 305868+341228, 338789+119540+196466+410464)

data_3 <- data.frame(
  REF_AREA2 = rep("Illinois", each = 9),
  ACS_YR = rep(c(2017, 2018, 2019), each = 3),
  unit = rep(unit, times = 3),
  counts = c(counts_2017_2, counts_2018_2, counts_2019_2)
)

counts_2017_2
unit_counts_17_2 <- c("unit_1" = 3463847, "unit_2 to4" = 650405, "unit5 or more" = 1111960)
total_units_17_2 <- sum(unit_counts_17_2)
ratios_17_2 <- unit_counts_17_2 / total_units_17_2 * 100
ratios_17_2

counts_2018_2
unit_counts_18_2 <- c("unit_1" = 3449914, "unit_2 to4" = 659061, "unit5 or more" = 1138240)
total_units_18_2 <- sum(unit_counts_18_2)
ratios_18_2 <- unit_counts_18_2 / total_units_18_2 * 100
ratios_18_2

counts_2019_2
unit_counts_19_2 <- c("unit_1" = 3462887, "unit_2 to4" = 647096, "unit5 or more" = 1065259)
total_units_19_2 <- sum(unit_counts_19_2)
ratios_19_2 <- unit_counts_19_2 / total_units_19_2 * 100
ratios_19_2

data_3$ratios <- c(ratios_17_2,ratios_18_2, ratios_19_2)


colnames(geo_final_processed2)

merged_data <- cbind(data, data_2, data_3)

# library(openxlsx)
# write.xlsx(merged_data, "merged_data", rowNames = FALSE)

library(readxl)
master_DB <- read_excel("master_DB.xlsx")


colnames(data)

library(ggplot2)
data
year <- c(2017, 2018, 2019)
unit_1_ratios <- c(29.34407, 29.49730, 28.76264)
data_4 <- data.frame(year, unit_1_ratios)
ggplot(data_4, aes(x = as.factor(year), y = unit_1_ratios)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Unit_1 Ratios from 2017 to 2019 (Chicago City)", x = "Year", y = "Ratios") +
  theme_minimal() +
  ylim(0, 80)


data_2
year <- c(2017, 2018, 2019)
unit_1_ratios <- c(45.54261, 45.57784, 45.58755)
data_6 <- data.frame(year, unit_1_ratios)
ggplot(data_6, aes(x = as.factor(year), y = unit_1_ratios)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Unit_1 Ratios from 2017 to 2019(Cook County)", x = "Year", y = "Ratios") +
  theme_minimal() +
  ylim(0, 80)

data_3
year <- c(2017, 2018, 2019)
unit_1_ratios <- c(66.27835, 65.74753, 66.91256)
data_5 <- data.frame(year, unit_1_ratios)
ggplot(data_5, aes(x = as.factor(year), y = unit_1_ratios)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Unit_1 Ratios from 2017 to 2019(Illinois)", x = "Year", y = "Ratios") +
  theme_minimal() +
  ylim(0, 80)

unit <- c('unit = 1', 'unit = 2_to_4', 'unit = 5_or_more')
ratios <- c(28.76264, 29.39115, 41.84621)
Chicago_city<- data.frame(unit, ratios)
print(Chicago_city)

ggplot(Chicago_city, aes(x = unit, y = ratios, fill = unit)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Ratio of House Unit in Chicago City",
       x = "Unit Types", y = "Ratios") +
  theme_minimal()

data_2
unit <- c('unit = 1', 'unit = 2_to_4', 'unit = 5_or_more')
ratios <- c(45.58755, 20.56606, 33.84639)
Cook_county <- data.frame(unit, ratios)
print(Cook_county)
ggplot(Cook_county, aes(x = unit, y = ratios, fill = unit)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Ratio of House Unit in  Cook county",
       x = "Unit Types", y = "Ratios") +
  theme_minimal()


data_3
unit <- c('unit = 1', 'unit = 2_to_4', 'unit = 5_or_more')
ratios <- c(66.91256, 12.50369, 20.58375)
Illinois <- data.frame(unit, ratios)
print(Illinois)
ggplot(Illinois, aes(x = unit, y = ratios, fill = unit)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Ratio of House Unit in Illinois",
       x = "Unit Types", y = "Ratios") +
  theme_minimal()










