
#-----------------------------------------------------------------------------------------#
                #Fruit crops production data for the elevation map
#-----------------------------------------------------------------------------------------#



library(dplyr)

# inventory of crops in provinces
crop_inventory <- read.csv('data/data.csv')

#The goal is to identify the fewest provinces that contribute the majority of the production, to reduce the number of provinces

# I define a function to consider other crops as well
crops_cumulative <- function(data, crop_column, threshold = 80) {
  data %>%
    select(Province, !!sym(crop_column)) %>% # !! tells R to evaluate sym = Almond_p as a column name, not text
    mutate(!!sym(crop_column) := as.numeric(gsub(",", "", !!sym(crop_column)))) %>%
    arrange(desc(!!sym(crop_column))) %>%
    filter(!is.na(!!sym(crop_column))) %>%
    mutate(cumulative_production = cumsum(!!sym(crop_column)),
           total_sum = sum(!!sym(crop_column)),
           cumulative_percentage = (cumulative_production / total_sum) * 100) %>%
    filter(cumulative_percentage <= threshold)
}


# apply this to all crops in a loop
crops <- c("Almond",   "Walnut",  "Apple", "Apricot", "Peach", "Plum" , "Pomegranate", "Grape", "Cherry", "Citrus", "Fig")

list_provinces <- c()

for (crop in crops) {

  top_province <- crops_cumulative(data = crop_inventory, crop_column = crop)
  top_province$Crop <- crop  # Add crop name as a column
  names(top_province)[2] <- 'Production_MT' # rename second column to ease bind rows lateron
  list_provinces[[crop]] <- top_province # append data to list
  
} 

# put in one dataframe
province_list <- bind_rows(list_provinces)

# add pistachio and pinenuts (wild and organic)
pistachio <- data.frame(Province = c('Badghis', 'Samangan', 'Hirat', 'Faryab', 'Balkh', 'Jawzjan', 'Kunduz'),
                       Crop = 'Pistachio')

pinenut <- data.frame(Province = c('Paktya', 'Paktika', 'Khost', 'Logar', 'Nuristan', 'Kunar', 'Nangarhar', 'Kapisa'),
                      Crop = 'Pinenut')

province_list <- bind_rows(province_list, pistachio, pinenut)

write.csv(province_list, 'data/crop_province_share.csv', row.names = FALSE)

