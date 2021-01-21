library(tidyverse)

view(starwars)

starwars %>%
  filter(species == "Human")%>%
  summarise(mean_height = (mean(height, na.rm = TRUE)))
# summarises mean of values)

human_height_sumary <- starwars%>%
  filter(!is.na(height)) %>% #filters out missing values in the 'height' column
  filter(species == "Human") %>%
  summarise(mean_height = mean(height), median_height = mean(height),
            min_height = min(height), max_height = max(height),
            height_range = max(height)-min(height))

species_height <- starwars %>%
  group_by(species) %>%
  filter(!is.na(height)) %>%
  summarise(mean_height = mean(height), median_height = median(height),
            min_height = min(height), max_height = max(height),
            height_range = max(height)-min(height)) %>%
  arrange(-mean_height)
  

#mean height of each species
mean_height <- starwars %>%
  group_by(species) %>%
  filter(!is.na(height)) %>%
  summarise (mean_height = mean(height))

starwars_new <- starwars %>%
  select(-skin_color, -films, -vehicles, -starships, -gender) %>%
  mutate(height = height/100) %>%
  filter(!is.na(height)) %>%
  filter(!is.na(mass)) %>%
  filter(species == "Human") %>%
  mutate(sex = str_to_title(sex), eye_color = str_to_title(eye_color),
         hair_color = str_to_title(hair_color)) %>%
  mutate(BMI = mass/ height^2) %>%
  arrange(-birth_year)
#generates new data frame, arranges human characters to be descending
# age, then calcuate BMI and put into a new column.

alphabetised <- starwars[order(starwars$name),]

?summarise
