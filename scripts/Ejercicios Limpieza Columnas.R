penguins %>%
  select(-species)

penguins %>%
  rename(island_new=island)

rename_with(penguins, toupper)
rename_with(penguins, tolower)

install.packages("janitor")
library(janitor)
clean_names(penguins)
