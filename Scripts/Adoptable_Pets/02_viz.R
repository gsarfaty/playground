# AUTHOR:   G. Sarfaty
# PURPOSE:  Pull adoptable pets data from API
# LICENSE:  MIT
# DATE:     2025-03-03
# UPDATED: 


# LOCALS & SETUP ===============================================================

#Libraries

library(tidyverse)
library(janitor)
library(gagglr)
library(scales)
library(systemfonts)
library(glue)
library(ggtext)
library(waffle)

# LOAD API EXPORT FROM GITHUB ==================================================
url<-"https://raw.githubusercontent.com/gsarfaty/playground/refs/heads/main/Dataout/2025-03-02_AdoptablePets.csv"

df_data<-read_csv(url)


# VIZ ==========================================================================

#What kinds of animals are available?

animal_type <- df_data %>% 
  clean_names() %>% 
  count(animal_type) %>% 
  mutate(Total = sum(n, na.rm = TRUE),
         Percent = n / Total) %>% 
  arrange(desc(Percent)) %>%  # Ensure it's sorted by Percent
  mutate(highlight = if_else(row_number() == 1, "highlight", "other")) %>%  # Tag highest value
  ggplot(aes(x = fct_reorder(animal_type, Percent), y = Percent, fill = highlight)) +  
  geom_col() +
  scale_fill_manual(values = c("highlight" = si_palettes$electric_indigo_t[2], 
                               "other" = "gray")) +  # Highlight top, gray others
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  coord_flip() +
  si_style_xgrid()+
  labs(x = NULL, y = NULL,
       title = "IN MARCH 2025, <span style='color:#627CCB;'><b>DOGS REPRESENTED 73%</b></span> OF ADOPTABLE PETS AT THE MONTGOMERY COUNTY SHELTER",
       caption = "Source: Montgomery County Animal Services | Accessed 3/2/25") +
  theme(
    legend.position = "none",
    plot.title = element_markdown()  # Enable HTML formatting in title
  )


print(animal_type)

si_save("Images/MD_County_AdoptablePets_Animal_Type.png")


# What day of the week are pets brought in?

popular_days<-df_data %>%
  clean_names() %>% 
  mutate(day_of_week = wday(as.Date(in_date), label = TRUE, abbr = FALSE)) %>%  # Extract weekday name
  count(day_of_week, name = "surrenders") %>%  # Count total surrenders per weekday
  mutate(highlight=if_else(day_of_week=="Wednesday", "highlight", "other")) %>% 
  ggplot(aes(x = fct_relevel(day_of_week, 
                             c("Monday", "Tuesday", "Wednesday", 
                               "Thursday", "Friday", "Saturday", "Sunday")), 
             y = surrenders, fill = highlight)) +  # Reorder for correct day sequence
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("highlight" = si_palettes$electric_indigo_t[2], 
                               "other" = "gray")) +  # Highlight top, gray others
  si_style_ygrid()+
  labs(x = NULL, y = "Number of Surrenders",
       title = "THE LOWEST VOLUME OF PET SURRENDERS OCCURRED ON <span style='color:#627CCB;'><b>WEDNESDAYS</b></span>",
       caption = "Source: Montgomery County Animal Services | Accessed 3/2/25")+
  theme(
    plot.title = element_markdown()  # Enable HTML formatting in title
  )

print(popular_days)

si_save("Images/MD_County_AdoptablePets_Popular_Intake_Days.png")



# Which dog breeds are a surrendered by their owners most?

df_data %>% 
  clean_names() %>% 
  filter(animal_type=="DOG") %>% 
  count(breed) #inspect counts by breed to identify further categorization


breeds <- df_data %>% 
  clean_names() %>% 
  filter(animal_type == "DOG") %>% 
  mutate(breed_cat = case_when(
    str_detect(breed, "PIT BULL") ~ "PIT OR PIT MIX",
    TRUE ~ "Other Breeds"
  )) %>% 
  count(breed_cat) %>% 
  mutate(Percent = round(n / sum(n) * 100)) %>% 
  arrange(desc(breed_cat)) %>% 
  mutate(label_y = round((cumsum(Percent) - (Percent / 2)) / 10, 0)) 


waffle_data <- setNames(breeds$Percent, breeds$breed_cat)


custom_colors <- c("PIT OR PIT MIX" = si_palettes$electric_indigo_t[2], "Other Breeds" = "lightgray")


waffle<-breeds %>% 
  ggplot() +
  geom_waffle(aes(fill = breed_cat, values = Percent),
              flip = TRUE, size = 0.5, color = "white", n_rows = 10) +  # Ensures square shape
  scale_fill_manual(values = custom_colors) +
  coord_equal() +  # Ensures grid stays square
  si_style_nolines()+
  annotate("text", x = 0.65, y = breeds$label_y, 
           label = breeds$breed_cat,
           color = "black", size =3.5, hjust = "left", family="Source Sans Pro")+
  labs(x=NULL, y=NULL,
       title = "70% OF AVAILABLE DOGS ARE <span style='color:#627CCB;'><b>PIT MIXES</b></span>",
       caption = "Source: Montgomery County Animal Services | Accessed 3/2/25")+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown())

print(waffle)


si_save("Images/MD_County_AdoptablePets_DOG_BREEDs.png")


# How old are most dogs available?

dog_age<-df_data %>% 
  clean_names() %>% 
  filter(animal_type=="DOG") %>%
  # age column has numeric and text, along with multiple units; must tidy it!
  mutate(
    years = as.numeric(str_extract(pet_age, "\\d+(?=\\s*YEARS?|\\s*YEAR)")),
    months = as.numeric(str_extract(pet_age, "\\d+(?=\\s*MONTHS?|\\s*MONTH)")),
    years = replace_na(years, 0),
    months = replace_na(months, 0),
    total_months = years * 12 + months,
    total_years = years + months / 12,
    # collapse breeds
    breed_cat = case_when(
      str_detect(breed, "PIT BULL") ~ "PIT OR PIT MIX",
      str_detect(breed, "HUSKY") ~ "HUSKY MIX",
      str_detect(breed, "BOXER") ~ "BOXER MIX",
      TRUE ~ "OTHER BREEDS"
    )) %>%
  select(breed_cat,pet_age,years, months, total_months, total_years) %>% 
  group_by(breed_cat) %>% 
  mutate(mean_OVR=mean(total_years)) %>% 
  ungroup() %>% 
  arrange(desc(mean_OVR)) %>% 
  ggplot(aes(x=total_years, 
             y=reorder(breed_cat,mean_OVR), 
             color=breed_cat,
             fill=breed_cat))+
  geom_boxplot(alpha=0.3)+
  # geom_jitter(alpha=0.3)+
  # coord_flip()+
  scale_fill_manual(values=si_palettes$hemsworth)+
  scale_color_manual(values=si_palettes$hemsworth)+
  si_style_xgrid()+
  labs(x="Age in Years", y=NULL,
       title = "ADOPTABLE DOGS ARE AN AVERAGE OF 3 YEARS OLD",
       caption = "Source: Montgomery County Animal Services | Accessed 3/2/25")+
  theme(
    legend.position = "none",
    plot.title = element_markdown() 
  )

print(dog_age)

si_save("Images/MD_County_AdoptablePets_DOG AGES_BoxPlot.png")