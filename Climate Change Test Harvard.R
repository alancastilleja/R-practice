update.packages("dslabs")
library(tidyverse)
library(dslabs)
library(ggthemes)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)


str(temp_carbon)
summary(temp_carbon)
# Question 1 filtering out years with no carbon emissions and choosing which one provided the correct answer
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

# Question 2
?pull
# setting variables for min and max years with carbon emission data
min_year <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>% 
  pull(year) %>% 
  min()

max_year <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()


# where the max carbon emission is in the data frame
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(carbon_emissions) %>%
  which.max()

# finding year with carbon emissions both ways
c1 <- temp_carbon %>% filter(year == min_year) %>% pull(carbon_emissions)
c2 <- temp_carbon %>% filter(year == max_year) %>% .$carbon_emissions
c2/c1

# Question 3 temp anomaly
min_year_temp <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>% 
  pull(year) %>% 
  min()

max_year_temp <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>% 
  pull(year) %>% 
  max()

# finding difference between oldest and recent temperature
t1 <- temp_carbon %>% filter(year == min_year_temp) %>% pull(temp_anomaly)
t2 <- temp_carbon %>% filter(year == max_year_temp) %>% .$temp_anomaly
t2 - t1

# Question 4
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly)) 
p <- p + geom_line(aes(color = "temp_anomaly"))
p <- p + geom_hline(aes(yintercept = 0), col = "blue")


# Question 5 + 6 + 7, you make a legend by setting the color to the variable you want within aes
p <- p + geom_line(aes(y = ocean_anomaly, color = "ocean_anomaly"))
p <- p + geom_line(aes(y = land_anomaly, color = "land_anomaly"))

# Question 5 portion

p <- p + ylab(" Temperature anomaly (degrees C)") + ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p + theme_clean()

# Question 6 extra, data only starts when all three vars have data, tinkering around
p_sub <- temp_carbon[, c("temp_anomaly", "ocean_anomaly", "land_anomaly", "year")]
library(reshape2)
p2 <- melt(p_sub, id=c("year"))
p2 %>% filter(!is.na())

ggplot(p2) + geom_line(aes(x = year, y = value, color = variable)) + scale_color_manual(values = c("red", "green", "blue"))

# Question 8 + 9
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid( gas~. , scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")
?facet_grid

# Question 10
q <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions, color = "carbon_emissions")) + geom_line()
q <- q + ylab("metric tons of carbon emitted per year")
q + geom_vline(xintercept = 1850)

# Question 11
co2_time <- historic_co2 %>% 
  filter(!is.na(co2)) %>% 
  ggplot(aes(year, co2, col = source)) 
co2_time + geom_line()

# Question 12
co2_time + xlim(-3000, 2018) + geom_line()
