# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
# Importing the data
geo3 <- read.csv("/Users/Ryu Seung Gwon/data/geo/geo3.csv", header = T)
geo2 <- read.csv("/Users/Ryu Seung Gwon/data/geo/geo2.csv", header = T)
geo1 <- read.csv("/Users/Ryu Seung Gwon/data/geo/geo1.csv", header = T)

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


# [geo3] Statistical Analysis 

# Find the Missing Values and Drop the Missing values
sum(is.na(geo3))
geo3 <- na.omit(geo3)

summary(geo3$LONGITUDE_NBR)
summary(geo3$LATITUDE_NBR)

# Find the duplicate data Drop the duplication
duplicated(geo3)
length(unique(geo3$PIN3))
geo3 <- distinct(geo3)

# Check for empty values
sum(geo3$ASSESSOR_FINAL_ADDR_LN == "")
sum(geo3$ASSESSOR_CITY_NM == "")
sum(geo3 == "")

# Delete the empty row of ASSESSOR_FINAL_ADDR_LN
geo3 <- geo3[geo3$ASSESSOR_FINAL_ADDR_LN != "", , drop = FALSE]
# Delete the empty row of ASSESSOR_CITY_NM
geo3 <- geo3[geo3$ASSESSOR_CITY_NM != "", , drop = FALSE]

# Delete rows with zero longitude and latitude
geo3 = geo3 %>%
  filter(LONGITUDE_NBR != 0, LATITUDE_NBR != 0)
# Delete if the values in the ASSESSOR_FINAL_ADDR_LN and ASSESSOR_CITY_NM columns are 'UNKNOWN'
geo3 = geo3 %>%
  filter(ASSESSOR_FINAL_ADDR_LN != 'UNKNOWN' | ASSESSOR_CITY_NM != 'UNKNOWN')
# 주소 열의 고유한 값 확인
print(unique(geo3$ASSESSOR_CITY_NM))


### Visualization

## geo1
ggplot(geo1, aes(x = COMMUNITY_ID)) +
  geom_bar() +
  labs(title = "COMMUNITY_ID Frequency", x = "COMMUNITY_ID", y = "Frequency")

## geo2
boxplot(geo2$HOUSE_UNIT, main = "Box Plot of HOUSE_UNIT", xlab = "HOUSE_UNIT")

barplot(table(geo2$PROPERTY_TYPE), main = "PROPERTY_TYPE Frequency", xlab = "PROPERTY_TYPE", ylab = "Frequency", col="Light Sky Blue")

filtered_geo2 <- geo2[geo2$PROPERTY_TYPE %in% c(1, 2, 3, 4), ]


filtered_geo2$log_HOUSE_UNIT = log(filtered_geo2$HOUSE_UNIT)
ggplot(filtered_geo2, aes(x = factor(PROPERTY_TYPE), y = log_HOUSE_UNIT)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average log(HOUSE_UNIT) by PROPERTY_TYPE", x = "PROPERTY_TYPE", y = "Average log(HOUSE_UNIT)")


ggplot(filtered_geo2, aes(x = factor(PROPERTY_TYPE), y = HOUSE_UNIT)) +
  geom_bar(stat = "identity") +
  labs(title = "Barplot of HOUSE_UNIT", x = "PROPERTY_TYPE", y = "HOUSE_UNIT_Total")

##geo3
ggplot(geo3, aes(x = PROPERTY_TYPE_YR, fill = RESIDENTIAL_IND)) +
  geom_bar(position = "dodge") +
  labs(title = "Relationship between PROPERTY_TYPE_YR and RESIDENTIAL_IND", x = "PROPERTY_TYPE_YR")

ggplot(geo3, aes(x = ASSESSOR_CITY_NM, fill = PROPERTY_TYPE_YR)) + geom_bar(position = "dodge")

table(geo1$COMMUNITY_NAME)


#
cook_county <- read_csv("/Users/Ryu Seung Gwon/data/file/data/ACSDT1Y2022.B25024-2023-11-14T095556.csv")

table(geo2$PROPERTY_TYPE)
unit <- c('unit1', 'unit2_to_4', 'unit5_or_more')
geo2_unit <- c(59423 + 44009, 41817, 6243)
geo2_house_unit_count <- data.frame(unit, geo2_unit)
geo2_house_unit_count$ratio <- geo2_house_unit_count$geo2_unit / sum(geo2_house_unit_count$geo2_unit) * 100
print(geo2_house_unit_count)


table(cook_county)
unit <- c('unit1', 'unit2_to_4', 'unit5_or_more')
cook_county_unit <- c(320889 + 48381, 158116 + 175161, 153512 + 59456 + 87909 + 255233 )
cook_county_unit_count <- data.frame(unit, cook_county_unit)
cook_county_unit_count$ratio <- cook_county_unit_count$cook_county_unit / sum(cook_county_unit_count$cook_county_unit) * 100
print(cook_county_unit_count)

# geo2 데이터셋의 비율 시각화
ggplot(geo2_house_unit_count, aes(x = unit, y = ratio, fill = unit)) +
  geom_bar(stat = "identity") +
  labs(title = "Ratio of Residential Units in geo2", x = "Unit", y = "Percentage") +
  theme_minimal()

# cook_county 데이터셋의 비율 시각화
ggplot(cook_county_unit_count, aes(x = unit, y = ratio, fill = unit)) +
  geom_bar(stat = "identity") +
  labs(title = "Ratio of Residential Units in Cook County", x = "Unit", y = "Percentage") +
  theme_minimal()

