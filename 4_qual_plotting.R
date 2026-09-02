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


## Qualitative data
library(dplyr)
library(stringr)

# remove some disturbing spaces in the dfs
qualitative <- qualitative %>%
  mutate(Answers = str_replace_all(Answers, "’", "'"), # change curely apostrophe into straight string
         Questions = Questions %>%
           str_replace_all("\\u00A0", " ") %>%  # replace non-breaking space with normal space
           str_squish()                         # trim + collapse multiple spaces into one
  )


# also from options df
questions_options <- questions_options %>%
  mutate(across(3:10,  ~ str_replace_all(.x, "’", "'")), # change curely apostrophe into straight string
         Questions = Questions %>%
           str_replace_all("\\u00A0", " ") %>%
           str_squish()
  )

# # here i woulg change some of NA to I dont knows for certificate related questions (the part is linked to cert plot at the end)
# qualitative <- qualitative %>%
#   filter(Questions %in% two) %>% # run two from below 
#   mutate(Answers = if_else(is.na(Answers), "I don't know", Answers))

qualitative_remove <- c(5, 49, 50, 62) # 4

qualitative_aggregate <- qualitative %>%
  filter(!No %in% qualitative_remove) %>%    # remove these questions 
  #  I remove some descriptive questions from experts, which disturbs the normal plotting (i should add 5 to quantitative)
  group_by(`Sub-areas`, No, Crops, Questions, Answers) %>%
  summarise(
    n = n(), .groups = "drop_last") %>% # how many experts answering
  #total = n_distinct(Expert_id), # total experts for this question
  mutate(
    perc  = round(100 * n / sum(n), 1) # percentage (I am doing this for 10% bc it again stackup in ggplot plot facet making 100 there)
  ) %>% 
  
  ungroup()  %>% 
  mutate(
    label = paste0(n, " (", perc, "%)") # text labels
  ) %>% 
  arrange(No)

# ill keep the order of questions the same for plot
qualitative_aggregate <- qualitative_aggregate %>%
  mutate(Questions = factor(Questions, levels = unique(Questions)))



# plott is too dense, I divide it into two plots
one <- c(4, 13, 24, 25, 26, 28, 29, 33, 38, 43)
two <- c(53, 54, 55, 56, 57, 58, 59, 60, 61)

qualitative_aggregate_1 <- qualitative_aggregate %>%
  filter(No %in% one)

qualitative_aggregate_2 <- qualitative_aggregate %>%
  filter(No %in% two)

################################

#q_aggregate_1 <-  ggplot(qualitative_aggregate_1, 
#                         aes(x = `Questions/Variables`, y = perc, fill = Answers)) +
#  geom_col(position = "stack") +

# add labels inside stacks
#  geom_text(aes(label = ifelse(perc > 5, label, "")),
#            position = position_stack(vjust = 0.5), size = 3) +
#  
# crops on y axis
#  facet_grid(rows = vars(Crops), scales = "free_y", space = "free_y") +

#  labs(x = "Questions", y = "Crops", fill = "Responses") +
#  theme_bw() +
#  theme(
#    axis.text.x = element_blank(),
#    strip.text.y = element_text(size = 9, face = "bold"),
#    legend.position = "none"
#  )

#ggsave("plot/questions/qualitative/quali_1.png", width = 20, height = 15, dpi = 600)  

# Define the text you want to find and replace for clarity

old_text <- "Is there an accessible, functional, and affordable cold transport system for international supply? (If the answer is Yes, answer question 4 as well; if Not, then skip question 4)"
new_text <- "Is there an accessible, functional, and affordable cold transport system for international supply?"

# Apply the replacement to the 'Answers' column and all 'Option' columns
qualitative_aggregate_1 <- qualitative_aggregate_1 %>%
  mutate(Questions = str_replace(
    as.character(Questions),
    fixed(old_text),
    new_text
  ))

qual_1 <- write.csv(qualitative_aggregate_1, 'qual_1.csv')
#######################################




# I will merge qualitative_aggregate_1 and 2 it with questions options



# join with your aggregated data
all_qual <- qualitative_aggregate_1 %>%
  merge(questions_options, by = c("No", "Questions"))


# First, create a unique row identifier to keep track of original rows
all_qual <- all_qual %>%
  mutate(original_row_id = row_number())

# Below parts brings options columns into rows: to show all possible options on plots-----------start
# Process one row at a time without affecting others
all_qual <- all_qual %>% 
  rowwise() %>%
  mutate(
    # Get all non-NA options for this specific row
    AllOptions = list(na.omit(c_across(starts_with("Option")))),
    AllOptions = list(trimws(AllOptions)),   # <-- keep only trimmed options
    Answers = trimws(Answers)                # <-- make sure Answers is trimmed too
  ) %>%
  # For each row, create new rows for non-matching options
  mutate(
    expanded_data = list({
      # Get current row's data as a tibble
      current_row <- tibble(
        original_row_id = original_row_id,
        No = No,
        Sub_areas = `Sub-areas`,
        Crops = Crops,
        Questions = Questions,
        Answers = Answers,
        n = n,
        perc = perc,
        label = label,
        # Include other columns you need to preserve
        OptionValue = Answers  # Start with the original answer
      )
      
      # Find options that don't match the answer
      non_matching_options <- setdiff(AllOptions, Answers)
      
      # Create new rows for non-matching options
      if (length(non_matching_options) > 0) {
        new_rows <- tibble(
          original_row_id = original_row_id,
          No = No,
          Sub_areas = `Sub-areas`,
          Crops = Crops,
          Questions = Questions,
          Answers = non_matching_options,
          n = 0,
          perc = 0,
          label = "0 (0%)",
          OptionValue = non_matching_options
        )
        # Combine original row with new rows
        bind_rows(current_row, new_rows)
      } else {
        current_row
      }
    })
  ) %>%
  ungroup() %>%
  # Extract all the expanded data
  select(expanded_data) %>%
  unnest(expanded_data) %>%
  # Remove the temporary column and clean up
  select(-OptionValue) %>%
  # Remove any empty answers
  filter(trimws(Answers) != "")

# Optional: Remove the original_row_id if you don't need it
all_qual <- all_qual %>% select(-original_row_id)

# there are some duplicate rows while creating new option rows, so I remove them
all_qual <- all_qual %>%
  arrange(Crops, Questions, Answers, desc(n), desc(perc)) %>%  # real rows first
  distinct(Crops, Questions, Answers, .keep_all = TRUE)

#-----------End


# 3rd step
# Reshape options into long format to get ordering for faceted y axis options
# answer_levels <- questions_options %>%
#   pivot_longer(cols = starts_with("Option"),
#                names_to = "OptionNo",
#                values_to = "Answer") %>%
#   filter(!is.na(Answer)) %>%
#   group_by(No) %>%
#   mutate(level = row_number(),
#          Answer = trimws(Answer)) %>% # I remove space from start of text in options
#   ungroup() 

all_qual <- all_qual %>%
  left_join(answer_levels %>% select(No, Answer, level),
            by = c("No", "Answers" = "Answer")) %>%
  arrange(No, level) %>%
  group_by(No) %>%
  mutate(Answers = factor(Answers, levels = unique(Answers[order(level)]))) %>%
  ungroup()


# Define the text you want to find and replace for clarity
old_text <- "Yes – Available but difficult to access due to strict terms and conditions"
new_text <- "Yes – Available but poor access"

# Apply the replacement to the 'Answers' column and all 'Option' columns
all_qual <- all_qual %>%
  mutate(across(c(Answers, starts_with("Option")), 
                ~ str_replace(.x, fixed(old_text), new_text)))

answer_levels <- answer_levels %>%
  mutate(Answer = str_replace(Answer, fixed(old_text), new_text))

# Define the text you want to find and replace for clarity
old_text <- "Yes – But not functional (i.e. only by name)"
new_text <- "Yes – But not functional"

# Apply the replacement to the 'Answers' column and all 'Option' columns
all_qual <- all_qual %>%
  mutate(across(c(Answers, starts_with("Option")), 
                ~ str_replace(.x, fixed(old_text), new_text)))

answer_levels <- answer_levels %>%
  mutate(Answer = str_replace(Answer, fixed(old_text), new_text))


# # Define the text you want to find and replace for clarity
# old_text <- "Is there an accessible, functional, and affordable cold transport system for international supply?    (If the answer is Yes, answer question 4 as well; if Not, then skip question 4)"
# new_text <- "Is there an accessible, functional, and affordable cold transport system for international supply?"

# # Apply the replacement to the 'Answers' column and all 'Option' columns
# all_qual <- all_qual %>%
#   mutate(Questions = str_replace(
#     as.character(Questions),
#     fixed(old_text),
#     new_text
#   ))
# 
# answer_levels <- answer_levels %>%
#   mutate(Questions = str_replace(Questions, fixed(old_text), new_text))


# ill keep the order of questions the same for plot
all_qual <- all_qual %>%
  mutate(Questions = factor(Questions,
                            levels = unique(Questions)))




all_qual <- all_qual %>%
  mutate(Answers = factor(Answers, 
                          levels = unique(Answers))) #%>% 
#mutate(Answers = fct_rev(Answers))

# # Get the consistent order from your answer_levels dataframe
# consistent_order <- answer_levels %>%
#   #filter(No == '4') %>%
#   arrange(level) %>%
#   pull(Answer) %>%
#   unique()
# 
# 
# all_qual <- all_qual %>%
#   mutate(Answers = factor(Answers, 
#                           levels = unique(consistent_order))) %>% 
#   mutate(Answers = fct_rev(Answers))



#-----------------------------------------------------------------------------#
# Plotting: 4 versions of plots are below for qualitative 9 questions

# Version 1: answers are stacked reaching 900 value on x axis 
qualitative_1 <- ggplot(all_qual, aes(x = Answers, y = perc, fill = Crops)) +
  geom_col() +
  
  # ADD THIS LINE TO FLIP THE CHART
  coord_flip() + 
  
  facet_wrap(~ Questions, scales = "free_y", labeller = label_wrap_gen(width = 45), ncol = 2) + # Note: scales is now "free_y"
  labs(
    #title = "Respondent Answers by Crop and Question",
    x = "Answer choice by respondents", # This label now refers to the y-axis
    y = "Percentage of respondents (%)" # This label now refers to the x-axis
  ) +
  
  #scale_y_continuous(limits = c(0,100)) + 
  
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 23),
        axis.text = element_text(size = 17),
        legend.title = element_text(size = 23),
        legend.text = element_text(size = 17),
        strip.text = element_text(size = 15, face = "bold"))

ggsave("plot/questions/qualitative/qualitative_1_100.png", width = 21, height = 15, dpi = 600)  

#-----------------------------------------------------------------------------#
library(ggplot2)
library(scales)   # for percent labels
library(dplyr)
library(forcats)
library(tidytext)


my_palette <- c(
  # Yes – strong positive (dark green)
  "Yes (actively working)"                     = "#006d2c",
  "Yes – Available and accessible"             = "#006d2c",
  "Yes – Available and efficient"              = "#006d2c",
  "Yes – Active and timely"                    = "#006d2c",
  "Yes – Active and functioning"               = "#006d2c",

  
  
  # Yes – partial / limited (lighter greens)
  "Yes – Available but limited"                = "#74c476",
  "Yes – Available but poor access"            = "#74c476",
  "Yes – Active but not timely"                = "#74c476",
  "Yes – Available but inefficient"            = "#74c476",
  "Yes (but limited in scope and funding)"     = "#74c476",
  "Accessible but limited"                     = "#74c476",
  
  
  
  "Yes – Available but costly"                 = "#c7e9c0",
  "Yes – Available but inefficient and costly" = "lightpink",
  "Very few and costly"                        = "lightpink",

  
  "Yes – But inactive"                         = "#d9f0a3",
  "Yes – But not functional"                   = "#d9f0a3",
  
  
  # No (all types = reds)
  "No (not active)"                            = "#d73027",
  "No (never existed)"                         = "#ef6548",
  "No – Not currently available"               = "#ef6548",
  "No – no growers' association exist"         = "#ef6548",
  "No – Not available at all"                  = "#b2182b",

  # Knowledge (purples)
  "No knowledge"                               = "#f2f0f7",
  "Basic knowledge"                            = "#dadaeb",
  "Intermediate knowledge"                     = "#bcbddc",
  "Good knowledge"                             = "#9e9ac8",
  "Expert-level knowledge"                     = "#54278f",
  
  # Irrigation (adjusted blues, lighter progression)
  "Drip irrigation"                              = "#08306b",
  "Sprinkle irrigation"                          = "#9ecae1",
  "Rainfed"                                      = "#4292c6",
  "Conventional and rainfed"                     = "#deebf7",
  "Conventional irrigation (flood/basin/furrow)" = "#2171b5",  # moved last, lightest
  
  # Uncertainty
  "I don't know"                               = "grey60",
  
  # Actors (browns/teals)
  "SMEs (Small and Medium Enterprises)"        = "#8c510a",
  "Exporters"                                  = "#d8b365",
  "Middlemen"                                  = "#dfc27d",
  "SMEs/Exporters"                             = "#f6e8c3",
  "Middlemen/Exporters"                        = "#c7eae5",
  "Large processing firms"                     = "#80cdc1",
  "All are equally involved"                   = "#35978f"
)



# Version 2:
# put the maximum % per crop for a given answer
max_labels <- all_qual %>%
  group_by(Questions, Crops) %>%
  filter(perc == max(perc)) %>%
  ungroup() %>%
  mutate(Crops = reorder_within(Crops, perc, Questions))

# Crops re-ordered
all_qual_ordered <- all_qual %>%
  group_by(Questions, Crops) %>%
  mutate(max_perc = max(perc)) %>%
  ungroup() %>%
  mutate(Crops = reorder_within(Crops, max_perc, Questions)) # we reorder here to match the first plot, thats y we have separete order for 2nd plot


qualitative_2 <- ggplot(all_qual_ordered, aes(x = Crops, y = perc, fill = Answers)) +
  # Use "fill" so bars are normalized to 100% per crop
  geom_col(position = "fill") +
  
  
  # Flip so crops run along y-axis, answers shown in bar segments
  coord_flip() +
  
  # Facet per question, since answer sets differ
  facet_wrap(~ Questions, scales = "free_y", labeller = label_wrap_gen(width = 45), ncol = 2) +
  
  labs(
    x = "Fruit crops",
    y = "Share of respondents (%)",
    fill = "Answers"
  ) +
  
  #scale_y_continuous(labels = percent_format(accuracy = 1)) +  # show as %
  scale_y_continuous(labels = scales::percent) +
  
  
  # 🔑 this cleans up the labels from reorder_within
  scale_x_reordered() +
  
  # add palette here
  scale_fill_manual(values = my_palette) +
  
  #add max % value
  geom_text(
    data = max_labels,
    aes(x = Crops, y = perc/100, label = paste0(round(perc), "%")),
    inherit.aes = FALSE,
    hjust = 1.4, size = 3.5, fontface = "bold"
  ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 13),
    strip.text = element_text(size = 15, face = "bold")
  )

ggsave("plot/questions/qualitative/qualitative_2_crops_reordered.png",
       plot = qualitative_2, width = 22, height = 16, dpi = 600)
#-----------------------------------------------------------------------------#


# Version 3:
# put the maximum % per crop for a given answer
max_labels <- all_qual %>%
  group_by(Questions, Crops) %>%
  filter(perc == max(perc)) %>%   # keep only the maximum
  ungroup()

# Crop alphabetical but Answer % reordered from left (high) to right (low)
all_qual_ordered <- all_qual %>%
  # Reverse order so A is at the top, Z at the bottom
  mutate(Crops = factor(Crops, levels = rev(sort(unique(Crops))))) %>%
  #mutate(Crops = factor(Crops, levels = sort(unique(Crops)))) %>%
  group_by(Questions, Crops) %>%
  arrange(desc(perc), .by_group = TRUE) %>%
  mutate(Answer_ordered = reorder_within(Answers, perc, interaction(Questions, Crops))) %>%
  ungroup()

qualitative_2 <- ggplot(all_qual_ordered, aes(x = Crops, y = perc, fill = Answers)) +   # fill = Answers (so palette works)
  geom_col(position = "fill", aes(fill = Answers, group = Answer_ordered)) +                   # keep palette mapping
  coord_flip() +
  facet_wrap(~ Questions, 
             #scales = "free_y", 
             labeller = label_wrap_gen(width = 45), ncol = 2) +
  
  labs(
    x = "Fruit crops",
    y = "Share of respondents (%)",
    fill = "Answers"
  ) +
  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = my_palette) +   # your custom palette
  scale_x_reordered() +                      # apply ordering based on Answer_ordered
  
  # add the max % value for answer option
  geom_text(
    data = max_labels,
    aes(x = Crops, y = perc/100, label = paste0(round(perc), "%")),
    inherit.aes = FALSE,
    hjust = 3, size = 3.5, fontface = "bold"
  ) +
  
  theme_bw() + 
  theme(    panel.grid.minor = element_blank(),
            axis.title = element_text(size = 22),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 21),
            legend.text = element_text(size = 13),
            strip.text = element_text(size = 15, face = "bold")
  )


ggsave("plot/questions/qualitative/qualitative_2_values_reordered.png",
       plot = qualitative_2, width = 22, height = 16, dpi = 600)

#In the second plot, we have alphabetical order for crops and the values of answer options 
#goes from highest to lowest (left to right). In the first plot, we ordered the crops by 
# the maximimum value of percentage for answer option per question (stronger response top, weaker response bottom)
#-----------------------------------------------------------------------------#

# Version 4:
# Below is a try to plot each with separate legend

library(patchwork)

# Make a list of plots, one per Question
plots <- lapply(unique(all_qual_ordered$Questions), function(q) {
  ggplot(filter(all_qual_ordered, Questions == q),
         aes(x = Crops, y = perc, fill = Answers)) +
    geom_col(position = "fill") +
    coord_flip() +
    
    facet_wrap(~ Questions, labeller = label_wrap_gen(width = 55), strip.position = "top") +
    
    
    labs(
      #title = str_wrap(q, width = 60),
      x = "Fruit crops",
      y = "Share of respondents (%)",
      fill = "Answers"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = my_palette) +
    scale_x_reordered() +
    geom_text(
      data = filter(max_labels, Questions == q),
      aes(x = Crops, y = perc/100, label = paste0(round(perc), "%")),
      inherit.aes = FALSE,
      hjust = 3, size = 3.5, fontface = "bold"
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 24),
      axis.text = element_text(size = 15),
      legend.title = element_text(size = 21),
      legend.text = element_text(size = 14),
      plot.title = element_text(size = 17, face = "bold"),
      strip.text = element_text(size = 16, face = "bold"))
  
})

# Combine with patchwork, keep each legend separate
final_plot <- wrap_plots(plots, ncol = 2) + 
  plot_layout(axis_titles = 'collect')

ggsave("plot/questions/qualitative/qualitative_2_fill_merge_new.png",
       plot = final_plot, width = 24, height = 18, dpi = 600)

#write.csv(all_qual_ordered, 'all_qual_ordered.csv', row.names = FALSE)
#-----------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#
# Certificates#
#-----------------------------------------------------------------------------#
# Plotting the certificate questions
certificates_q <- qualitative_aggregate_2 %>%
  merge(questions_options, by = c("No", "Questions"))


# remove undesirable columns
certificates_q <- certificates_q[ ,-c(15,16)]


# merge with answer levels to get all options on y axis
certificates_q <- certificates_q %>%
  left_join(answer_levels %>% select(No, Answer, level),
            by = c("No", "Answers" = "Answer")) %>%
  arrange(No, level) %>%
  group_by(No) %>%
  mutate(Answers = factor(Answers, levels = unique(answer_levels$Answer))) %>%
  ungroup()


# I convert NAs into I don't knows b/c experts have not chosen it
certificates_q <- certificates_q %>%
  mutate(Answers = if_else(is.na(Answers), "I don't know", Answers))  %>% 
  group_by(No, Questions, `Sub-areas`, Crops, Answers) %>%
  summarise(n = sum(n), .groups = "drop") %>%   # sum up duplicate counts
  group_by(No, Questions, `Sub-areas`, Crops) %>%
  mutate(
    perc = 100 * n / sum(n),
    label = paste0(n, " (", round(perc, 1), "%)")
  ) %>%
  ungroup()


# Version 1: with upto 1300 on x axis
certificates <- ggplot(certificates_q, aes(x = Answers, y = perc, fill = Crops)) +
  geom_col() +
  
  # ADD THIS LINE TO FLIP THE CHART
  coord_flip() + 
  
  facet_wrap(~ Questions, scales = "free_x", labeller = label_wrap_gen(width = 45), ncol = 2) + # Note: scales is now "free_y"
  labs(
    #title = "Respondent Answers by Crop and Question",
    x = "Answer choice by respondents", # This label now refers to the y-axis
    y = "Percentage of respondents (%)" # This label now refers to the x-axis
  ) +
  
  #scale_y_continuous(limits = c(0,100)) + 
  
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 23),
        axis.text = element_text(size = 17),
        legend.title = element_text(size = 23),
        legend.text = element_text(size = 17),
        strip.text = element_text(size = 15, face = "bold"))

ggsave("plot/questions/qualitative/certificates_1.png", width = 21, height = 15, dpi = 600) 


# Version 2 certificates

# Reorder answers consistently using levels
certificates_q <- certificates_q %>%
  mutate(Answers = factor(Answers, 
                          levels = unique(answer_levels$Answer)))

# Put crops in consistent alphabetical order
certificates_q <- certificates_q %>%
  mutate(Crops = factor(Crops, levels = rev(sort(unique(Crops)))))

max_labels_cert <- certificates_q %>%
  group_by(Questions, Crops) %>%
  filter(perc == max(perc)) %>%
  ungroup()



# plot
certificates <- ggplot(certificates_q, aes(x = Crops, y = perc, fill = Answers)) +
  geom_col(position = "fill") +   # normalize bars to 100% per crop
  coord_flip() +
  
  facet_wrap(~ Questions, 
             #scales = "free", # free_x to avoid repetition on x axis and free_y for y axis, i turned off to apply both by default
             labeller = label_wrap_gen(width = 45), ncol = 3) +
  
  labs(
    x = "Fruit crops",
    y = "Share of respondents (%)",
    fill = "Answers"
  ) +
  
  scale_y_continuous(labels = scales::percent) +
  
  geom_text(
    data = max_labels_cert,
    aes(label = paste0(round(perc), "%")),
    position = position_fill(vjust = 0.5),
    inherit.aes = TRUE, 
    size = 4.5, fontface = "bold", color = "black"
  ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 20, face = "bold")
  )

ggsave("plot/questions/qualitative/certificates_2.png",
       plot = certificates, width = 22, height = 16, dpi = 600)



# version 3: combined version with x axis reduced to 100 by dividing the perc by crops to avoid stacking them upto 1300 but rather 100
# this below one step is only for the combined plot as it avoid stacking into 1300 for 13 crops into 10% each,
# for uniform x axis and % labels on plot

certificates_q <- certificates_q %>%
  mutate(perc_norm = perc /  13) %>%
  ungroup()

certificates_alt <- ggplot(certificates_q, 
                           aes(x = perc_norm, y = Answers, fill = Crops)) +
  geom_col(position = "stack") +   # now stacks different crops per answer
  facet_wrap(~ Questions, 
             #scales = "free_y", # i comment out this b/c it was removing ans option not for some facet plots
             labeller = label_wrap_gen(width = 45), ncol = 3) +
  
  labs(
    x = "Share of respondents (%)",
    y = "Answer options",
    fill = "Fruit crops"
  ) +
  
  scale_x_continuous(limits = c(0, 100)) +
  
  
  geom_text( data = certificates_q %>% 
               group_by(Questions, Answers) %>% 
               summarise(total_perc = sum(perc_norm), .groups = "drop") %>% 
               distinct(), # makes sure there’s only one row per bar 
             aes(x = total_perc, y = Answers, label = paste0(round(total_perc), "%")), 
             inherit.aes = FALSE, hjust = -0.2, size = 5, fontface = "bold", color = "black" ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 17),
    legend.position = "right",   # keep legend now since colors = crops
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 24),
    strip.text = element_text(size = 20, face = "bold")
  )


ggsave("plot/questions/qualitative/certificates_3.png",
       plot = certificates_alt, width = 22, height = 16, dpi = 600)

# I should convert unanswered into i dont know in certificates questions (I did that)









