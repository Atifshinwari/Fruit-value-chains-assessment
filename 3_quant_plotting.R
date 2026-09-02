# Discussion with Hoa for plots:
# i. sub group the results into: nuts, stone fruits, fruits and others
# ii. do not plot all the variables. Identify the variables that are important for all the crops, make it as a criteria and plot those important
# because it is not necessary to plot the descriptive variables

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
#--------------------------------------------------------------------------------------------
# reorder the crops on x axis, i tried within plot but not working
library(forcats)
aggregate_quantitative <- aggregate_quantitative %>% 
  mutate(Crops_reorder = fct_reorder(Crops, -median_val, .na_rm = TRUE)) # for descending order will use -median_val

#Warning message:
#  There was 1 warning in `mutate()`.
#ℹ In argument: `Crops_reorder = fct_reorder(Crops, -median_val)`.
#Caused by warning:
#  ! `fct_reorder()` removing 13 missing values.
#ℹ Use `.na_rm = TRUE` to silence this message.
#ℹ Use `.na_rm = FALSE` to preserve NAs.

# NAs at this point are due to question 44, which I may delete at the end, if less people has answer it????

# Plot
quantitative_variables <- ggplot(
  aggregate_quantitative %>% 
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
  
  facet_grid(reorder(`Sub-areas`, No) ~ ., scales = "free_y", space = "free_y", labeller = label_wrap_gen(width = 20)) +
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
  
  labs(x = "Fruit crops", y = "Questions / Variables", fill = "Median response (%)", color = "Other responses") +
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

ggsave("plot/questions/251020_quant_vars_crops_reordered.png", width = 18, height = 15, dpi = 600)  

# The x axis crops are reorder from high to low (left (best) to right (worst)) 

write.csv(aggregate_quantitative, "aggregate_quantitative.csv", row.names = FALSE)









