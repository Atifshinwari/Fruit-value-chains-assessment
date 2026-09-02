# Discussion with Hoa for plots:
# i. sub group the results into: nuts, stone fruits, fruits and others
# ii. do not plot all the variables. Identify the variables that are important for all the crops, make it as a criteria and plot those important
# because it is not necessary to plot the descriptive variables

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


# read data
all_responses_long <- read.csv('all_responses_long.csv', check.names = FALSE)
all_responses_long <- all_responses_long %>%
  rename(Questions = 'Questions/Variables') # rename to match question options

# read the questions with all the possible answers
questions_options <- read.csv('valuechain_interventions_prioritization/Experts feedback/Questions and options.csv')

# change in answer levels as well
answer_levels <- questions_options %>%
  pivot_longer(cols = starts_with("Option"),
               names_to = "OptionNo",
               values_to = "Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(No) %>%
  mutate(level = row_number(),
         Answer = trimws(Answer)) %>% # I remove space from start of text in options
  ungroup() 

# I am dividing the questions into 2 categories 1) quantitative response 2) qualitative

# I identify the qualitative questions manually, i create a vector for this
qualitative_questions <- c(4, 5, 13, 24, 25, 26, 28, 29, 33, 38, 43, 49, 50, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62)

# keep only qualitative 
qualitative <- all_responses_long %>%
  filter(No %in% qualitative_questions) %>%
  mutate(ResponseType = ifelse(is.na(Midpoint), 'NA', 'Known')) # I divided it in order to deal with 'I don't knows'

require(stringr)
# now keep only quantitative
quantitative <- all_responses_long %>%
  filter(!No %in% qualitative_questions) %>% 
  mutate(
    Answers = str_replace_all(Answers, "’", "'"),   # there was problem with apostrophe, I changed curly → straight
    ResponseType = case_when(                       # I categorize the answers into different responsetypes known are values
      !is.na(Midpoint) ~ "known",
      Answers == "I don't know" ~ "IDK",
      is.na(Midpoint) ~ "other",
      
    ),
    Midpoint = ifelse(                             # as in the Answer column, I also write IDK also in the Midpoint column 
      Answers == "I don't know",
      "IDK",
      as.character(Midpoint)
    )
  )

# Now I wrote the remaining answers from Answer column into the Mid point column
quantitative <- quantitative %>% 
  mutate(
    Midpoint = ifelse(is.na(Midpoint), Answers, as.character(Midpoint))
  )

# I would like to point out real NA's
quantitative <- quantitative %>% 
  mutate(
    ResponseType = ifelse(is.na(Midpoint) & ResponseType == 'other', NA, ResponseType)
  )

# To plot: Two ways
## 1. Experts on y axis and crops on x axis and only plot 1 or 2 or max 3 variables in single plot
## 15 experts × ~40 questions × 13 crops = ~10k responses. The heatmap will be too dense

## 2. Variables on y axis, crops on x axis and aggregating (mean or median) per Question × Crop: median would be better in such studies
## Heatmap will show average Experts % for that Question X Crop
## Jitterpoints from ggjitter will show qualitative answers, I don't knows' and some NAs.

# First aggregate the data
aggregate_quantitative <- quantitative %>%
  # in below line I exclude qualitative answ and only computing median of values but still qual answ for overlay points
  filter(!Midpoint %in% c('Not needed (natural forest)', 'Rainfed', 'IDK', 
                          'Not required', 'Mostly consumed locally', 
                          'Wild forests', '', 'Yes – Available but inefficient and costly')) %>% 
  group_by(`Sub-areas`, No, Questions, Crops, ResponseType) %>%
  summarise(
    median_val = median(as.numeric(Midpoint), na.rm = TRUE),
    n = n(), # this tells us how many observations (values) contribute to this question... how many experts answer this with value
    .groups = 'drop'
  ) %>% 
  arrange(No) # b/c the order was changing I kept the original order for questions

#--------------------------------------------------------------------------------------------
# EDIT QUESTIONS ITSELF: 
# edit the text of some long questions text
aggregate_quantitative <- aggregate_quantitative %>%
  mutate(Questions= case_when(
    Questions == "Excluding India and Pakistan, to what extent does the private sector (traders and processing companies) meet the certification requirements of other potential markets? (e.g. Germany, US, EU)."
    ~ "To what extent does the private sector meet the certification requirements of other potential markets? (e.g. Germany, US, EU).",
    Questions == "What proportion of the orchards follow standard orchard design (e.g., square, rectangular, etc.?" 
    ~ "What proportion of the orchards follow standard orchard design (e.g., square, rectangular, etc.)?",
    TRUE ~ Questions
  ))

quantitative <- quantitative %>%
  mutate(Questions= case_when(
    Questions == "Excluding India and Pakistan, to what extent does the private sector (traders and processing companies) meet the certification requirements of other potential markets? (e.g. Germany, US, EU)."
    ~ "To what extent does the private sector meet the certification requirements of other potential markets? (e.g. Germany, US, EU).",
    Questions == "What proportion of the orchards follow standard orchard design (e.g., square, rectangular, etc.?" 
    ~ "What proportion of the orchards follow standard orchard design (e.g., square, rectangular, etc.)?",
    TRUE ~ Questions
  ))

# change in answer levels as well
answer_levels <- questions_options %>%
  pivot_longer(cols = starts_with("Option"),
               names_to = "OptionNo",
               values_to = "Answer") %>%
  filter(!is.na(Answer)) %>%
  group_by(No) %>%
  mutate(level = row_number(),
         Answer = trimws(Answer)) %>% # I remove space from start of text in options
  ungroup() 

answer_levels <- answer_levels %>%
  mutate(Questions= case_when(
    Questions == "Excluding India and Pakistan, to what extent does the private sector (traders and processing companies) meet the certification requirements of other potential markets? (e.g. Germany, US, EU)."
    ~ "To what extent does the private sector meet the certification requirements of other potential markets? (e.g. Germany, US, EU).",
    Questions == "What proportion of the orchards follow standard orchard design (e.g., square, rectangular, etc.?" 
    ~ "What proportion of the orchards follow standard orchard design (e.g., square, rectangular, etc.)?",
    TRUE ~ Questions
  ))
#--------------------------------------------------------------------------------------------


# Jitters for NAs are not shown in the plot so I rewrite them (I do not do this in few steps above bc the format would disturb)
quantitative <- quantitative %>%
  mutate(ResponseType= case_when(
    is.na(ResponseType) ~ "NA",
    TRUE ~ ResponseType
  ))

#--------------------------------------------------------------------------------------------
# EDIT SUB-AREAS NAMES: 
quantitative <- quantitative %>%
  mutate(`Sub-areas`= case_when(
    `Sub-areas`== "Orchard establishment and breeding"   ~ "Orchards establishment",
    `Sub-areas`== "Soil, planting and irrigation management"   ~ "Soil and irrigation",
    `Sub-areas`== "Other orchard management practices"  ~ "Orchard management practices",
    `Sub-areas`== "On-farm actions"  ~ "On-farm handling practices",
    TRUE ~ `Sub-areas`
  ))

aggregate_quantitative <- aggregate_quantitative %>%
  mutate(`Sub-areas`= case_when(
    `Sub-areas`== "Orchard establishment and breeding"  ~ "Orchards establishment",
    `Sub-areas`== "Soil, planting and irrigation management"  ~ "Soil and irrigation",
    `Sub-areas`== "Other orchard management practices" ~ "Orchard management practices",
    `Sub-areas`== "On-farm actions"  ~ "On-farm handling practices",
    TRUE ~ `Sub-areas`
  ))
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# SUB-GROUPS ALLOCATION: I will change allocation of some questions to their subgroups
quantitative <- quantitative %>% 
  mutate(`Sub-areas` = case_when(
    Questions == "What proportion of the orchards follow standard orchard design (e.g., square, rectangular, etc.?" ~ "Orchards establishment",
    Questions == "What proportion of orchards follow proper planting distance?" ~ "Orchards establishment",
    Questions == "What proportion of farmers follow and have access to standard farm level sorting, grading, and packing facilities?" ~ "On-farm handling practices",
    Questions == "Is the crop harvested and supplied in mixed grades due to the lack of proper sorting and grading tools?" ~ "On-farm handling practices",
    Questions == "Do farmers/growers receive adequate extension services?" ~ "On-farm handling practices",
    Questions == "How would you rate the overall development level of the PRODUCTION sector of the mentioned fruit value chains?" ~ "On-farm handling practices",
    TRUE ~ `Sub-areas`
  ))

aggregate_quantitative <- aggregate_quantitative %>% 
  mutate(`Sub-areas` = case_when(
    Questions == "What proportion of the orchards follow standard orchard design (e.g., square, rectangular, etc.?" ~ "Orchards establishment",
    Questions == "What proportion of orchards follow proper planting distance?" ~ "Orchards establishment",
    Questions == "What proportion of farmers follow and have access to standard farm level sorting, grading, and packing facilities?" ~ "On-farm handling practices",
    Questions == "Is the crop harvested and supplied in mixed grades due to the lack of proper sorting and grading tools?" ~ "On-farm handling practices",
    Questions == "Do farmers/growers receive adequate extension services?" ~ "On-farm handling practices",
    Questions == "How would you rate the overall development level of the PRODUCTION sector of the mentioned fruit value chains?" ~ "On-farm handling practices",
    TRUE ~ `Sub-areas`
  ))
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# QUESTION NUMBERING: since the order is changed for questions, i will keep an order 
quantitative <- quantitative %>%
  mutate(No = case_when(
    Questions == "Do farmers/growers receive adequate extension services?" ~ 31,
    Questions == "How would you rate the overall development level of the PRODUCTION sector of the mentioned fruit value chains?" ~ 32,
    Questions == "Do farmers practice proper pre-cooling for fresh fruits?" ~ 27,
    Questions == "Are there sufficient on-farm pack houses for initial cleaning, sorting, grading, pre-cooling, and packaging?" ~ 30,
    TRUE ~ No
  ))

aggregate_quantitative <- aggregate_quantitative %>%
  mutate(No = case_when(
    Questions == "Do farmers/growers receive adequate extension services?" ~ 31,
    Questions == "How would you rate the overall development level of the PRODUCTION sector of the mentioned fruit value chains?" ~ 32,
    Questions == "Do farmers practice proper pre-cooling for fresh fruits?" ~ 27,
    Questions == "Are there sufficient on-farm pack houses for initial cleaning, sorting, grading, pre-cooling, and packaging?" ~ 30,
    TRUE ~ No
  ))
#--------------------------------------------------------------------------------------------

# SHORTEN THE QUESTIONS TEXT:
#--------------------------------------------------------------------------------------------

# join 
aggregate_quantitative <- aggregate_quantitative %>%
  left_join(short_questions, by = "Questions") 

# join
quantitative <- quantitative %>%
left_join(short_questions, by = "Questions") 

#--------------------------------------------------------------------------------------------

# change one answer type
quantitative <- quantitative %>%
  mutate(Answers = case_when(
    Answers == "Not needed (natural forest)" ~ "Wild forests",
    TRUE ~ Answers # all other values are kept unchanged
  ))

answer_levels <- answer_levels %>%
  mutate(Answer = case_when(
    Answer == "Not needed (natural forest)" ~ "Wild forests",
    TRUE ~ Answer # all other values are kept unchanged
  ))
#--------------------------------------------------------------------------------------------
# Changes to 3 questions: Turn around the question to keep the correct order which is followed by other questions in quantitative: less +, more -ve
aggregate_quantitative <- aggregate_quantitative %>%
  mutate(Questions= case_when(
    Questions == "Is the crop harvested as a mix due to mix varieties cultivated?"
    ~ "Is the crop harvested uniform (un-mixed) due to monocultivar orchards?",
    Questions == "Is the crop harvested and supplied in mixed grades due to the lack of proper sorting and grading tools?" 
    ~ "Is the crop harvested and supplied in uniform (un-mixed) grades due to the availability of proper grading tools?",
    Questions == "What proportion of the current total export is affected by high tariffs and border challenges?"
    ~ "What proportion of the current total export is not affected by high tariffs and border challenges?",
    TRUE ~ Questions
    ))

aggregate_quantitative <- aggregate_quantitative %>%
    mutate(new_questions = case_when(
    new_questions == "Mix-harvest due to mix varieties cultivated" ~ "Uniform vs. mixed harvest due to polycultivar orchards",
    new_questions == "Mix-grades due to lack of sorting & grading tools" ~ "Uniform vs. mixed grades due to lack of grading tools",
    new_questions == "Export affected by high tariffs and border challenges" ~ "Export not affected by high tariffs and border challenges",
    TRUE ~ new_questions
    
  ))

quantitative <- quantitative %>%
  mutate(Questions= case_when(
    Questions == "Is the crop harvested as a mix due to mix varieties cultivated?"
    ~ "Is the crop harvested uniform (un-mixed) due to monocultivar orchards?",
    Questions == "Is the crop harvested and supplied in mixed grades due to the lack of proper sorting and grading tools?" 
    ~ "Is the crop harvested and supplied in uniform (un-mixed) grades due to the availability of proper grading tools?",
    Questions == "What proportion of the current total export is affected by high tariffs and border challenges?"
    ~ "What proportion of the current total export is not affected by high tariffs and border challenges?",
    TRUE ~ Questions
  ))

quantitative <- quantitative %>%
  mutate(new_questions= case_when(    
    new_questions == "Mix-harvest due to mix varieties cultivated" ~ "Uniform vs. mixed harvest due to polycultivar orchards",
    new_questions == "Mix-grades due to lack of sorting & grading tools" ~ "Uniform vs. mixed grades due to lack of grading tools",
    new_questions == "Export affected by high tariffs and border challenges" ~ "Export not affected by high tariffs and border challenges",
        TRUE ~ new_questions
  ))

answer_levels <- answer_levels %>%
  mutate(Questions= case_when(
    Questions == "Is the crop harvested as a mix due to mix varieties cultivated?"
    ~ "Is the crop harvested uniform (un-mixed) due to monocultivar orchards?",
    Questions == "Is the crop harvested and supplied in mixed grades due to the lack of proper sorting and grading tools?" 
    ~ "Is the crop harvested and supplied in uniform (un-mixed) grades due to the availability of proper grading tools?",
    Questions == "What proportion of the current total export is affected by high tariffs and border challenges?"
    ~ "What proportion of the current total export is not affected by high tariffs and border challenges?",
    TRUE ~ Questions
  ))

# I will also change the values the opposite way, previous 30% would become 70% now
# define questions
reverse_questions <- c("Uniform vs. mixed harvest due to polycultivar orchards",
                    "Uniform vs. mixed grades due to lack of grading tools",
                    "Export not affected by high tariffs and border challenges")

Test <- aggregate_quantitative %>%
       filter(new_questions %in% reverse_questions) %>%
       select(new_questions, Crops, median_val)

# we assinged new values based on looking at the previous median values
 aggregate_quantitative <- aggregate_quantitative %>%
  mutate(
    median_val = ifelse(
      new_questions %in% reverse_questions,
      case_when(
        between(median_val, 0, 10) ~ 87.5,   # No → Full
        between(median_val, 10, 20) ~ 57.5,    # 10–20 → Moderate
        between(median_val, 20, 40) ~ 30,      # 20–40 → Partial
        between(median_val, 40, 75) ~ 12.5,    # 40–75 → Minimal
        between(median_val, 75, 100) ~ 2.5,    # 75–100 → No
        TRUE ~ median_val
  ),
  median_val
  ))
 
 Test_2 <- aggregate_quantitative %>%
   filter(new_questions %in% reverse_questions) %>%
   select(new_questions, Crops, median_val)
 
 
 
#----------------------------------------------------------------------------------------# 
 # Try to combine few qualitative questions with quantitative by changing their structure# 
#----------------------------------------------------------------------------------------#
 
 
all_qual_orderedd <- read.csv('all_qual_ordered.csv')

 # Remove specific options
 qual_summary_test <- all_qual_orderedd %>%
   group_by(Questions, Crops, `Sub_areas`) %>%
   mutate(         # remove specific options
     include = case_when(
       Questions == "Is there an active breeding program for developing new high-yielding varieties in the country?" & 
         Answers %in% c("No (not active)", "No (never existed)", "I don't know") ~ FALSE,
       
       Questions == "Which irrigation system is mainly used?" & 
         Answers %in% c("Drip irrigation", "Sprinkle irrigation","I don't know") ~ FALSE,
       
       Questions == "Are there available and accessible subsidy programs for farmers?" & 
         Answers %in% c("No – Not currently available", "I don't know") ~ FALSE,
       
       Questions == "Are there reliable and accessible credit facilities for farmers?" & 
         Answers %in% c("No – Not currently available", "I don't know") ~ FALSE,
       
       Questions == "To what extent do the extension officers have knowledge and skills in fruit production practices?" & 
         Answers %in% c("No knowledge", "Basic knowledge", "Intermediate knowledge", "I don't know") ~ FALSE,
       
       Questions == "Is there an active and timely early warning for farmers on frost, winds, pests, diseases, etc.?" & 
         Answers %in% c("No – Not currently available", "I don't know") ~ FALSE,
       
       Questions == "Is there a crop-specific active growers association?" & 
         Answers %in% c("Yes – But not functional", "No – no growers' association exist", "I don't know") ~ FALSE,
       
       Questions == "Is there an efficient local transport system available for supply from the farm to local markets?" & 
         Answers %in% c("No – Not available at all", "Yes – Available but inefficient and costly", "Yes – Available but inefficient", "I don't know") ~ FALSE,
       
       Questions == "Depending on the fruit species, which type of private sector is mainly involved in fruit processing?" & 
         Answers %in% c("Middlemen/Exporters", "Exporters", "All are equally involved", "Middlemen", "I don't know") ~ FALSE,
       
       Questions == "Is there an accessible, functional, and affordable cold transport system for international supply?" & 
         Answers %in% c("No – Not available at all", "Very few and costly", "Accessible but limited", "I don't know") ~ FALSE,
       
       TRUE ~ TRUE
     )
   ) %>% 
   
   # Combine certain options that represent most selected (dominant) and highly relevant answer options
   filter(include) %>% 
   # Combine specific answer options (sum or average)
   mutate(
     Answers = case_when(
       Questions == "Is there an active breeding program for developing new high-yielding varieties in the country?" &
         Answers %in% c("Yes (actively working)", "Yes (but limited in scope and funding)") ~ "Yes (actively working) + Yes (but limited in scope and funding)",
       
       Questions == "Which irrigation system is mainly used?" &
         Answers %in% c("Conventional and rainfed", "Conventional irrigation (flood/basin/furrow)","Rainfed") ~ "Conventional and rainfed + Conventional irrigation (flood/basin/furrow) + Rainfed",
       
       Questions == "Are there available and accessible subsidy programs for farmers?" &
         Answers %in% c("Yes – Available and accessible", "Yes – Available but poor access","Yes – Available but limited") ~ "Yes – Available and accessible + Yes – Available but poor access + Yes – Available but limited",
       
       Questions == "Are there reliable and accessible credit facilities for farmers?" &
         Answers %in% c("Yes – Available and accessible", "Yes – Available but poor access","Yes – Available but limited") ~ "Yes – Available and accessible + Yes – Available but poor access + Yes – Available but limited",
       
       Questions == "To what extent do the extension officers have knowledge and skills in fruit production practices?" &
         Answers %in% c("Expert-level knowledge", "Good knowledge") ~ "Expert-level knowledge + Good knowledge",
       
       Questions == "Is there an active and timely early warning for farmers on frost, winds, pests, diseases, etc.?" &
         Answers %in% c("Yes – Active and timely", "Yes – But inactive", "Yes – Active but not timely") ~ "Yes – Active and timely + Yes – But inactive + Yes – Active but not timely",
       
       Questions == "Is there a crop-specific active growers association?" &
         Answers %in% c("Yes – Active and functioning") ~ "Yes – Active and functioning",
       
       Questions == "Is there an efficient local transport system available for supply from the farm to local markets?" &
         Answers %in% c("Yes – Available and efficient", "Yes – Available but costly") ~ "Yes – Available and efficient + Yes – Available but costly",
       
       Questions == "Depending on the fruit species, which type of private sector is mainly involved in fruit processing?" &
         Answers %in% c("SMEs (Small and Medium Enterprises)", "Large processing firms", "SMEs/Exporters") ~ "SMEs (Small and Medium Enterprises) + Large processing firms + SMEs/Exporters",
       
       Questions == "Is there an accessible, functional, and affordable cold transport system for international supply?" & 
         Answers %in% c("Yes – Available and accessible") ~ "Yes – Available and accessible",
       
       TRUE ~ Answers
       
     )
   ) %>%
   group_by(No, Questions, Crops, Answers, `Sub_areas`) %>%
   summarise(
     perc = sum(perc, na.rm = TRUE),  # or mean(perc, na.rm = TRUE)
     .groups = "drop"
   )
 
 
 
 # only need to invert the values for irrigation question
 # change the questions into short and newly defined forms
 # merge with quantitative for plotting
 
 
 # I assigned new values to irrigation question, to make it the other way around
 qual_summary_test <- qual_summary_test %>%
   mutate(
     perc = ifelse(
       Questions == 'Which irrigation system is mainly used?',
       100 - perc,
       perc
     )
   )
 
 
 # assign short form of questions
 qual_summary_test <- qual_summary_test %>% 
   mutate(
     new_questions = case_when( # add to a new column
       
       Questions == "Is there an active breeding program for developing new high-yielding varieties in the country?" ~ "Breeding program (although limited in scope & fund)",
       Questions == "Which irrigation system is mainly used?" ~ "Modern vs. conventional irrigation (flood, basin, furrow) ",
       Questions == "Are there available and accessible subsidy programs for farmers?" ~ "Available & accessible subsidy programs",
       Questions == "Are there reliable and accessible credit facilities for farmers?" ~ "Availability of credit facilities", 
       Questions == "To what extent do the extension officers have knowledge and skills in fruit production practices?" ~ "Expertise of extension officers (Good vs. basic knowledge)",
       Questions == "Is there an active and timely early warning for farmers on frost, winds, pests, diseases, etc.?" ~ "Active early warning for farmers vs. non-availability",
       #Questions == "Is there a crop-specific active growers association?" ~ 
       Questions == "Is there an efficient local transport system available for supply from the farm to local markets?" ~ "Standardized vs. inefficient & costly local transport system",
       Questions == "Depending on the fruit species, which type of private sector is mainly involved in fruit processing?" ~ "Involvement of SMEs vs. middlemen & exporters for fruit processing",
       Questions == "Is there an accessible, functional, and affordable cold transport system for international supply?" ~ "Available & affordable cold transport system",
       
       TRUE ~ Questions  # keep the original question if no match
       
     )
   )
 
 
 # merge with aggregate quantitative
 # change col names to match aggregate quantitative
 qual_summary_test <- qual_summary_test %>%
   rename(`Sub-areas` = "Sub_areas",
          median_val = "perc")
 
 # correct the sub areas names
 qual_summary_test <- qual_summary_test %>% 
   mutate(
     `Sub-areas` = case_when(
     `Sub-areas` == "Orchard establishment and breeding" ~ "Orchards establishment",
     `Sub-areas` == "Soil, planting and irrigation management" ~ "Soil and irrigation",
     `Sub-areas` == "Government support and other" ~ "Government support",
     `Sub-areas` == "On-farm actions" ~ "On-farm handling practices",
     `Sub-areas` == "Government support and other" ~ "Government support",
     TRUE ~ `Sub-areas`
     )
   )
 
 quant_merge <- bind_rows(aggregate_quantitative, qual_summary_test)
 
 # change a problemetic value
 quant_merge <- quant_merge %>% 
   mutate(
     median_val = ifelse(No == 13 & Crops == 'Pistachio', 0, median_val))
 
 # again correct the order
 quant_merge <- quant_merge %>%
   mutate(No = case_when(
     Questions == "How would you rate the overall development level of the PRODUCTION sector of the mentioned fruit value chains?" ~ 33,
     Questions == "Is there an efficient local transport system available for supply from the farm to local markets?" ~ 32,
     TRUE ~ No
   ))
 
 # rearrange to get correct order of No
 quant_merge <- quant_merge %>%
   arrange(No)
 
 # quant_merge %>%
 #   distinct(`Sub-areas`) %>%
 #   mutate(clean = paste0("[", `Sub-areas`, "]"))
 
 #--------------------------------------------------------------------------------------------
 # reorder the crops on x axis, i tried within plot but not working
 library(forcats)
 # Compute mean per crop first, then use it for reordering
 # crop_order <- quant_merge %>%
 #   group_by(Crops) %>%
 #   summarise(mean_val = mean(median_val, na.rm = TRUE)) %>%
 #   arrange(desc(mean_val)) %>%
 #   pull(Crops)
 
 crop_order <- quant_merge %>%
   filter(!is.na(median_val)) %>%
   group_by(Crops) %>%
   summarise(med_overall = median(median_val, na.rm = TRUE), .groups = "drop") %>%
   arrange(desc(med_overall)) %>%     # low -> high
   pull(Crops)
 
 # Apply this as the factor order
 quant_merge <- quant_merge %>%
   mutate(Crops_reorder = factor(Crops, levels = crop_order))
 
 # correct some spellings
 quant_merge <- quant_merge %>%
   mutate(
     `Sub-areas` = recode(
       `Sub-areas`,
       "Orchards establishment" = "Orchard establishment",
       "Transportation and markets" = "Transportation and market access"
     )
   )
 
 # correct the sub areas names
 quant_merge <- quant_merge %>% 
   mutate(
     new_questions = case_when(
       # Grammatical renaming
       new_questions == "Breeding program (although limited in scope & fund)" ~ 
         "Breeding program (although limited in scope & funding)",
       
       new_questions == "Farmers performing soil test" ~ 
         "Farmers performing soil tests",
       
       new_questions == "Orchards properly levelled" ~ 
         "Orchards properly leveled",
       
       new_questions == "Farmers follow standard irrigation schedule" ~ 
         "Farmers follow a standard irrigation schedule",
       
       new_questions == "Farmers knowledge of common pest & diseases" ~ 
         "Farmers' knowledge of common pests & diseases",
       
       new_questions == "Farmers knowledge best pre- & post-harvest practices" ~ 
         "Farmers' knowledge of best pre- & post-harvest practices",
       
       new_questions == "Uniform vs. mixed grades due to lack of grading tools" ~ 
         "Uniform vs. mixed grades due to a lack of grading tools",
       
       new_questions == "Growers awareness of market information" ~ 
         "Growers' awareness of market information",
       
       new_questions == "Traders awareness of market information" ~ 
         "Traders' awareness of market information",
       
       new_questions == "Current export using cold chain systems" ~ 
         "Current exports using cold chain systems",
       
       new_questions == "Export not affected by high tariffs and border challenges" ~ 
         "Exports not affected by high tariffs and border challenges",
       
       # technical renaming
       new_questions == "Involvement of SMEs vs. middlemen & exporters for fruit processing" ~ 
         "Role of SMEs vs. middlemen & exporters in processing",
       new_questions == "Private sector fulfilling certification requirements of potential markets" ~ 
         "Market certification compliance by the private sector",
       new_questions == "Trade-requirements fullilled by trade-agreements (India & Pakistan)" ~ 
         "Trade requirements met by agreements (India & Pakistan)",
       TRUE ~ new_questions
     )
   )
 
 
 # Plot
 quantitative_variables <- ggplot(
   quant_merge %>% 
     filter(!is.na(median_val)), # ignore NAs for question 44 (if yes,...)
   aes(x = Crops_reorder, y = reorder(new_questions, -No), fill = median_val)) + # - with No change the order of arrangement bottom to top
   geom_tile(color = "white") +
   
   # # jitter for other responses e.g. 'I dont knows', 'wild forests', 'NA'
   # geom_jitter(
   #   data = quantitative %>% filter(Answers %in% c('Wild forests', 'Rainfed', "I don't know",
   #                                                 'Not required', 'Mostly consumed locally',
   #                                                 'Yes – Available but inefficient and costly', '')), # we use quantitative because it has all the individual response for jitter points
   #   aes(x = Crops, y = new_questions, color = Answers),
   #   width = 0.3, height = 0.3, size = 1.8, alpha = 0.6,
   #   inherit.aes = FALSE # prevents it from looking for median_val otherwise gives error
   # ) +
   
 facet_grid(reorder(`Sub-areas`, No) ~ ., scales = "free_y", space = "free_y", labeller = label_wrap_gen(width = 18)) +
   #scale_fill_gradient(low = "white", high = "steelblue", na.value = "grey90") +
   scale_fill_gradientn(
     colors = c('orange', 'white', 'steelblue'),
     limits = c(0,100),
     breaks = seq(0, 100, 20)) +
   
   # scale_color_manual(values = c('Wild forests' = "darkgreen", 'Rainfed' = "blue", "I don't know" = "red",
   #                               'Not required' = "green", 'Mostly consumed locally' = "purple",
   #                               'Yes – Available but inefficient and costly' = "black"),
   #                    labels = c('Wild forests' = 'Wild forests', 'Rainfed' = 'Rainfed', "I don't know" = "I don't know",   # putting labels just to edit legend text
   #                               'Not required' = 'Not required', 'Mostly consumed locally' = 'Mostly consumed locally',
   #                               'Yes – Available but inefficient and costly' = 'Yes – Available but inefficient\nand costly')) +
   
   labs(x = "Fruit crops", y = "Questions / Indicators", fill = "Median\nresponse (%)")+ #, color = "Other responses") +
   theme_bw() +
   theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
         axis.text.y = element_text(size = 16),
         axis.title = element_text(size = 22),
         legend.title = element_text(size = 20),
         legend.text = element_text(size = 16),
         legend.key.size = ,
         strip.text.y.right = element_text(
           size = 13.4, 
           face = "bold",
           #lineheight = 0.9, 
           hjust = 0           # align nicely to the left
         )
   ) 
 
 ggsave("plot/questions/260605_quant_vars_crops_reordered_Combined.png", width = 15, height = 15.5, dpi = 600)  
 
 # The x axis crops are reorder from high to low (left (best) to right (worst)) 


# calculations for paper
# =============================
# FULL CLEAN WORKING CODE
# - Crop median row at TOP (no bottom spacing issue)
# - Crop median numbers shown ONLY ONCE (top facet only)
# - Row median numbers stay per facet (right side)
# - Highest = green, Lowest = red, both bold (for row & crop medians)
# =============================

# library(dplyr)
# library(ggplot2)
# library(scales)
# library(forcats)
# 
# # 1nd approach: Without GLOBAL MEDIAN
# 
# # 1) Crop order
# crop_order <- quant_merge %>%
#   filter(!is.na(median_val)) %>%
#   group_by(Crops) %>%
#   summarise(med_overall = median(median_val, na.rm = TRUE), .groups = "drop") %>%
#   arrange(desc(med_overall)) %>%
#   pull(Crops)
# 
# quant_merge <- quant_merge %>%
#   mutate(Crops_reorder = factor(Crops, levels = crop_order))
# 
# # 2) Base plotting data
# plot_df <- quant_merge %>%
#   filter(!is.na(median_val)) %>%
#   mutate(
#     y_q = reorder(new_questions, -No),
#     Sub_area_facet = factor(`Sub-areas`, levels = c(
#       "Orchard establishment",
#       "Soil and irrigation",
#       "Orchard management practices",
#       "Government support",
#       "On-farm handling practices",
#       "Off-farm processing",
#       "Transportation and market access"
#     ))
#   )
# 
# # 3) Row medians per question within facet
# row_meds <- plot_df %>%
#   group_by(Sub_area_facet, y_q) %>%
#   summarise(row_median = median(median_val, na.rm = TRUE), .groups = "drop") %>%
#   mutate(Crops_reorder = "Indicator median")
# 
# # 4) Column medians per crop within facet
# col_meds <- plot_df %>%
#   group_by(Sub_area_facet, Crops_reorder) %>%
#   summarise(col_median = median(median_val, na.rm = TRUE), .groups = "drop")
# 
# # 5) Extend factor levels
# plot_df2 <- plot_df %>%
#   mutate(
#     Crops_reorder = factor(
#       as.character(Crops_reorder),
#       levels = c(levels(quant_merge$Crops_reorder), "Indicator median")
#     ),
#     y_q = factor(
#       as.character(y_q),
#       levels = c("Crop median", levels(y_q))
#     )
#   )
# 
# row_meds2 <- row_meds %>%
#   mutate(
#     Crops_reorder = factor(as.character(Crops_reorder), levels = levels(plot_df2$Crops_reorder)),
#     y_q = factor(as.character(y_q), levels = levels(plot_df2$y_q)),
#     flag = case_when(
#       row_median > 40 ~ "max",
#       row_median < 20 ~ "min",
#       TRUE ~ "mid"
#     )
#   )
# 
# col_meds2 <- col_meds %>%
#   mutate(
#     Crops_reorder = factor(as.character(Crops_reorder), levels = levels(plot_df2$Crops_reorder)),
#     y_q = factor("Crop median", levels = levels(plot_df2$y_q)),
#     flag = case_when(
#       col_median > 40 ~ "max",
#       col_median < 20 ~ "min",
#       TRUE ~ "mid"
#     )
#   )
# 
# # 6) Plot
# quantitative_variables <- ggplot(
#   plot_df2,
#   aes(x = Crops_reorder, y = y_q, fill = median_val)
# ) +
#   geom_tile(color = "white") +
#   
#   facet_grid(
#     Sub_area_facet ~ .,
#     scales = "free_y",
#     space = "free_y",
#     labeller = label_wrap_gen(width = 18)
#   ) +
#   
#   scale_fill_gradientn(
#     colors = c("orange", "white", "steelblue"),
#     limits = c(0, 100),
#     breaks = seq(0, 100, 20),
#     oob = squish
#   ) +
#   
#   geom_text(
#     data = row_meds2,
#     aes(x = Crops_reorder, y = y_q, label = sprintf("%.1f", row_median), color = flag),
#     inherit.aes = FALSE,
#     size = 4.5,
#     fontface = "bold"
#   ) +
#   
#   geom_text(
#     data = col_meds2,
#     aes(x = Crops_reorder, y = y_q, label = sprintf("%.1f", col_median), color = flag),
#     inherit.aes = FALSE,
#     size = 4.5,
#     fontface = "bold"
#   ) +
#   
#   scale_color_manual(
#     values = c(max = "darkgreen", min = "red3", mid = "black"),
#     guide = "none"
#   ) +
#   
#   labs(
#     x = "Fruit crops",
#     y = "Questions / Indicators",
#     fill = "Median\nresponse (%)"
#   ) +
#   
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
#     axis.text.y = element_text(size = 16),
#     axis.title = element_text(size = 22),
#     legend.title = element_text(size = 20),
#     legend.text = element_text(size = 16),
#     legend.key.size = unit(0.6, "cm"),
#     strip.text.y.right = element_text(size = 13.4, face = "bold", hjust = 0)
#   ) +
#   coord_cartesian(clip = "off")
# 
# 
# ggsave(
#   "plot/questions/260311_quant_vars_crops_reordered_Combined_with_rowcol_medians.png",
#   quantitative_variables,
#   width = 16, height = 15.5, dpi = 600
# )





# change questions thematic area:
quant_merge <- quant_merge %>%
  mutate(
    `Sub-areas` = ifelse(
      new_questions == "How developed is the PRODUCTION sector?",
      "Government support",
      `Sub-areas`
    )
  )


# 2nd approach: Include GLOBAL MEDIAN
# =========================
# Libraries
# =========================
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(ggtext)
library(ggh4x)
library(grid)

# =========================
# 0) Move one question to Government support
# =========================
quant_merge <- quant_merge %>%
  mutate(
    `Sub-areas` = case_when(
      new_questions == "How developed is the PRODUCTION sector?" ~ "Government support",
      new_questions == "Orchards with standard design" ~ "Orchard establishment",
      TRUE ~ `Sub-areas`
    )
  )

 write.csv(quant_merge, "aggregate_quantitative_combined.csv", row.names = FALSE)
 
# =========================
# 1) Crop order
# =========================
crop_order <- quant_merge %>%
  filter(!is.na(median_val)) %>%
  group_by(Crops) %>%
  summarise(
    med_overall = median(median_val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(med_overall)) %>%
  pull(Crops)

quant_merge <- quant_merge %>%
  mutate(
    Crops_reorder = factor(Crops, levels = crop_order)
  )

# =========================
# 2) Base plotting data
# =========================
plot_df <- quant_merge %>%
  filter(!is.na(median_val)) %>%
  mutate(
    y_q = reorder(new_questions, -No),
    Sub_area_facet = factor(
      `Sub-areas`,
      levels = c(
        "Orchard establishment",
        "Soil and irrigation",
        "Orchard management practices",
        "Government support",
        "On-farm handling practices",
        "Off-farm processing",
        "Transportation and market access",
        "Global median"
      )
    ),
    Stage_group = case_when(
      `Sub-areas` %in% c(
        "Orchard establishment",
        "Soil and irrigation",
        "Orchard management practices",
        "Government support"
      ) ~ "Production",
      `Sub-areas` %in% c(
        "On-farm handling practices",
        "Off-farm processing"
      ) ~ "Processing",
      `Sub-areas` %in% c(
        "Transportation and market access"
      ) ~ "Marketing",
      TRUE ~ NA_character_
    ),
    Stage_group = factor(
      Stage_group,
      levels = c("Production", "Processing", "Marketing", "Overall")
    )
  )

# =========================
# 3) Row medians per question within facet
# =========================
row_meds <- plot_df %>%
  group_by(Stage_group, Sub_area_facet, y_q) %>%
  summarise(
    row_median = median(median_val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Crops_reorder = "Indicator median"
  )

# =========================
# 4) Column medians per crop within facet
# =========================
col_meds <- plot_df %>%
  group_by(Stage_group, Sub_area_facet, Crops_reorder) %>%
  summarise(
    col_median = median(median_val, na.rm = TRUE),
    .groups = "drop"
  )

# =========================
# 5) Extend factor levels
# =========================
plot_df2 <- plot_df %>%
  mutate(
    Crops_reorder = factor(
      as.character(Crops_reorder),
      levels = c(levels(quant_merge$Crops_reorder), "Indicator median")
    ),
    y_q = factor(
      as.character(y_q),
      levels = c("Crop median", levels(y_q), "Global median")
    )
  )

row_meds2 <- row_meds %>%
  mutate(
    Crops_reorder = factor(
      as.character(Crops_reorder),
      levels = levels(plot_df2$Crops_reorder)
    ),
    y_q = factor(
      as.character(y_q),
      levels = levels(plot_df2$y_q)
    ),
    Stage_group = factor(
      as.character(Stage_group),
      levels = c("Production", "Processing", "Marketing", "Overall")
    ),
    flag = case_when(
      row_median > 40 ~ "max",
      row_median < 20 ~ "min",
      TRUE ~ "mid"
    )
  )

col_meds2 <- col_meds %>%
  mutate(
    Crops_reorder = factor(
      as.character(Crops_reorder),
      levels = levels(plot_df2$Crops_reorder)
    ),
    y_q = factor(
      "Crop median",
      levels = levels(plot_df2$y_q)
    ),
    Stage_group = factor(
      as.character(Stage_group),
      levels = c("Production", "Processing", "Marketing", "Overall")
    ),
    flag = case_when(
      col_median > 40 ~ "max",
      col_median < 20 ~ "min",
      TRUE ~ "mid"
    )
  )

# =========================
# 6) Global medians per crop from long data
# =========================
global_meds <- plot_df2 %>%
  filter(as.character(Crops_reorder) != "Indicator median") %>%
  group_by(Crops_reorder) %>%
  summarise(
    median_val = median(median_val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Stage_group = factor(
      "Overall",
      levels = c("Production", "Processing", "Marketing", "Overall")
    ),
    Sub_area_facet = factor(
      "Global median",
      levels = c(
        "Orchard establishment",
        "Soil and irrigation",
        "Orchard management practices",
        "Government support",
        "On-farm handling practices",
        "Off-farm processing",
        "Transportation and market access",
        "Global median"
      )
    ),
    y_q = factor(
      "Global median",
      levels = levels(plot_df2$y_q)
    ),
    flag = case_when(
      median_val > 40 ~ "max",
      median_val < 20 ~ "min",
      TRUE ~ "mid"
    )
  )

# =========================
# 7) Stage labels for merged strips
# =========================
stage_levels_markdown <- c(
  "<b>Production</b>",
  "<b>Processing</b>",
  "<b>Marketing</b>",
  ""
)

plot_df2 <- plot_df2 %>%
  mutate(
    Stage_group_lab = factor(
      case_when(
        as.character(Stage_group) == "Production" ~ "<b>Production</b>",
        as.character(Stage_group) == "Processing" ~ "<b>Processing</b>",
        as.character(Stage_group) == "Marketing"  ~ "<b>Marketing</b>",
        as.character(Stage_group) == "Overall"    ~ ""
      ),
      levels = stage_levels_markdown
    ))

row_meds2 <- row_meds2 %>%
  mutate(
    Stage_group_lab = factor(
      case_when(
        as.character(Stage_group) == "Production" ~ "<b>Production</b>",
        as.character(Stage_group) == "Processing" ~ "<b>Processing</b>",
        as.character(Stage_group) == "Marketing"  ~ "<b>Marketing</b>",
        as.character(Stage_group) == "Overall"    ~ ""
      ),
      levels = stage_levels_markdown
    ))

col_meds2 <- col_meds2 %>%
  mutate(
    Stage_group_lab = factor(
      case_when(
        as.character(Stage_group) == "Production" ~ "<b>Production</b>",
        as.character(Stage_group) == "Processing" ~ "<b>Processing</b>",
        as.character(Stage_group) == "Marketing"  ~ "<b>Marketing</b>",
        as.character(Stage_group) == "Overall"    ~ ""
      ),
      levels = stage_levels_markdown
    )
  )

global_meds <- global_meds %>%
  mutate(
    Stage_group_lab = factor(
      case_when(
        as.character(Stage_group) == "Production" ~ "<b>Production</b>",
        as.character(Stage_group) == "Processing" ~ "<b>Processing</b>",
        as.character(Stage_group) == "Marketing"  ~ "<b>Marketing</b>",
        as.character(Stage_group) == "Overall"    ~ ""
      ),
      levels = stage_levels_markdown
    )
  )

# =========================
# 8) Final plotting data
# =========================
plot_df2    <- plot_df2    %>% mutate(across(where(is.numeric), round))
row_meds2   <- row_meds2   %>% mutate(across(where(is.numeric), round))
col_meds2   <- col_meds2   %>% mutate(across(where(is.numeric), round))
global_meds <- global_meds %>% mutate(across(where(is.numeric), round))

plot_df_final <- bind_rows(plot_df2, global_meds)



# =========================
# 9) Plot
# =========================
quantitative_variables <- ggplot(
  plot_df_final,
  aes(x = Crops_reorder, y = y_q, fill = median_val)
) +
  geom_tile(color = "white") +
  
  ggh4x::facet_nested(
    Stage_group_lab + Sub_area_facet ~ .,
    scales = "free_y",
    space = "free_y",
    labeller = labeller(
      Sub_area_facet = function(x) {
        x[x == "Global median"] <- ""
        x <- stringr::str_wrap(x, width = 16)
        x <- gsub("\n", "<br>", x)
        x
      }
    ),
    strip = ggh4x::strip_nested(
      bleed = FALSE,
      background_y = list(
        element_rect(fill = "white", colour = "black"),
        element_rect(fill = "grey90", colour = "black")
      ),
      by_layer_y = TRUE
    )
  ) +
  
  scale_x_discrete(
    labels = function(x) {
      x[x == "Indicator median"] <- "<b>Indicator median</b>"
      x
    }
  ) +
  
  scale_y_discrete(
    labels = function(x) {
      x[x == "Crop median"] <- "<b>Crop median</b>"
      x[x == "Global median"] <- "<b>Global median</b>"
      x
    }
  ) +
  
  scale_fill_gradientn(
    colors = c("orange", "white", "steelblue"),
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    oob = scales::squish
  ) +
  
  geom_text(
    data = row_meds2,
    aes(
      x = Crops_reorder,
      y = y_q,
      label = row_median,
      color = flag
    ),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold"
  ) +

geom_text(
  data = col_meds2,
  aes(
    x = Crops_reorder,
    y = y_q,
    label = col_median,
    color = flag
  ),
  inherit.aes = FALSE,
  size = 5,
  fontface = "bold"
) +

geom_text(
  data = global_meds,
  aes(
    x = Crops_reorder,
    y = y_q,
    label = median_val,
    color = flag
  ),
  inherit.aes = FALSE,
  size = 5,
  fontface = "bold"
) +
  
  scale_color_manual(
    values = c(
      max = "darkgreen",
      min = "red3",
      mid = "black"
    ),
    guide = "none"
  ) +
  
  labs(
    x = "Fruit crops",
    y = "Questions / Indicators",
    fill = "Median\nresponse (%)"
  ) +
  
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "grey95", colour = NA),
    axis.text.x = ggtext::element_markdown(size = 17, angle = 45, hjust = 1),
    axis.text.y = ggtext::element_markdown(size = 17),
    axis.title = element_text(size = 24),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
    legend.key.size = unit(0.6, "cm"),
    strip.text.y = element_text(size = 14, face = "bold", colour = 'black', margin = margin(t = 10, b = 10)),
    strip.text.y.right = ggtext::element_markdown(
      size = 14,
      hjust = 0.5,
      vjust = 0.5,
      colour = 'black',
      face = 'bold',
      margin = margin(t = 10, b = 10)
    )
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "plot/questions/260605_quant_vars_crops_reordered_Combined_with_rowcol_medians.png",
  quantitative_variables,
  width = 15.7,
  height = 15.9,
  dpi = 600
)






# Table for calculations
library(dplyr)
library(tidyr)

# -----------------------------
# 1) Filter valid data
# -----------------------------
df <- quant_merge %>%
  filter(!is.na(median_val))

# -----------------------------
# 2) Compute crop order (overall median across variables)
# -----------------------------
crop_order <- df %>%
  group_by(Crops) %>%
  summarise(med_overall = median(median_val, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(med_overall)) %>%   # remove desc() if you want low -> high
  pull(Crops)

# -----------------------------
# 3) Stable question order (same as plot: by No)
# -----------------------------
question_levels <- df %>%
  arrange(No) %>%
  distinct(No, new_questions) %>%
  pull(new_questions)

# -----------------------------
# 4) Prepare ordered data
# -----------------------------
df <- df %>%
  mutate(
    Crops = factor(Crops, levels = crop_order),
    new_questions = factor(new_questions, levels = rev(question_levels))
  )

# -----------------------------
# 5) Create wide table (heatmap shape)
# -----------------------------
wide_tbl <- df %>%
  select(new_questions, Crops, median_val) %>%
  pivot_wider(names_from = Crops, values_from = median_val)

# -----------------------------
# 6) Add row medians (across crops)
# -----------------------------
row_meds <- df %>%
  group_by(new_questions) %>%
  summarise(IndicatorMedian = median(median_val, na.rm = TRUE), .groups = "drop")

wide_tbl2 <- wide_tbl %>%
  left_join(row_meds, by = "new_questions")

# Ensure crop columns follow correct order
wide_tbl2 <- wide_tbl2 %>%
  select(new_questions, all_of(crop_order), IndicatorMedian)

# -----------------------------
# 7) Add bottom row (crop medians across variables)
# -----------------------------
col_meds <- df %>%
  group_by(Crops) %>%
  summarise(col_median = median(median_val, na.rm = TRUE), .groups = "drop")

bottom_vals <- setNames(col_meds$col_median, as.character(col_meds$Crops))

bottom_row <- tibble(
  new_questions = "Crop median",
  !!!as.list(bottom_vals),
  RowMedian = NA_real_
)

final_table <- bind_rows(wide_tbl2, bottom_row)

# -----------------------------
# 8) Save table
# -----------------------------
final_table    <- final_table    %>% mutate(across(where(is.numeric), round))

write.csv(final_table,
          "data/results/260224_quant_vars_median_table.csv",
          row.names = FALSE)




# =========================================
# THEMATIC MEDIANS (RECOMMENDED APPROACH)
# Median of question-level medians (explanation in learning material)
# =========================================

# 1) Question-level medians WITH thematic area (and facet id)
row_meds_ok <- quant_merge %>%
  filter(!is.na(median_val)) %>%
  group_by(`Sub-areas`, No, new_questions) %>%
  summarise(
    row_median = median(median_val, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Thematic medians (7 values) = median of question medians
thematic_medians <- row_meds_ok %>%
  group_by(`Sub-areas`) %>%
  summarise(
    thematic_median = median(row_median, na.rm = TRUE),
    Q1 = quantile(row_median, 0.25, na.rm = TRUE),
    Q3 = quantile(row_median, 0.75, na.rm = TRUE),
    n_questions     = n_distinct(No),
    .groups = "drop"
  ) %>%
  arrange(desc(thematic_median))

# 3) Join them
theme_medians <- row_meds_ok %>%
  left_join(thematic_medians, by = "Sub-areas") %>%
  arrange(No)

theme_medians    <- theme_medians    %>% mutate(across(where(is.numeric), round))
write.csv(theme_medians,
          "data/results/260224_thematic_medians.csv",
          row.names = FALSE)


# Overall Median (inter-quartile range IQR)
Overall_median <- quant_merge %>% 
  summarise(
    overall_median = median(median_val, na.rm = TRUE),
    Q1 = quantile(median_val, 0.25, na.rm = TRUE),
    Q3 = quantile(median_val, 0.75, na.rm = TRUE)
  )


overall_median <- final_table %>%
  left_join(theme_medians, by = 'new_questions') %>%
  arrange(No)

overall_median    <- overall_median    %>% mutate(across(where(is.numeric), round))

write.csv(overall_median,
          "data/results/260224_overall_medians.csv",
          row.names = FALSE)

global_median <- quant_merge %>%
  filter(!is.na(median_val)) %>%
  summarise(overall_median = median(median_val, na.rm = TRUE),
            Q1 = quantile(median_val, 0.25, na.rm = TRUE),
            Q3 = quantile(median_val, 0.75, na.rm = TRUE))

global_median    <- global_median    %>% mutate(across(where(is.numeric), round))
write.csv(global_median,
          "data/results/260224_global_medians.csv",
          row.names = FALSE)

# global_median_theme <- thematic_medians %>%
#   summarise(overall_median = median(thematic_median, na.rm = TRUE),
#             Q1 = quantile(thematic_median, 0.25, na.rm = TRUE),
#             Q3 = quantile(thematic_median, 0.75, na.rm = TRUE))
# global_median_theme
