# AUTHOR:   G. Sarfaty
# PURPOSE:  Viz Baby Names
# LICENSE:  MIT
# DATE:     2025-03-04
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
library(patchwork)
library(ggstream)


# LOAD API EXPORT FROM GITHUB ==================================================
url<-"https://raw.githubusercontent.com/gsarfaty/playground/refs/heads/main/Dataout/2025-03-04_SSA_BabyNames_1880to2023.csv"

df_data<-read_csv(url)


# VIZ ==========================================================================

#What are the top 10 most popular names overall by sex?

#female | Lollipop graph
top10_names_f_viz <- df_data %>% 
  filter(sex=="F") %>% 
  group_by(name) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  mutate(rank=rank(-value, ties.method = "first")) %>% 
  filter(rank <11) %>% 
  mutate(highlight=if_else(rank==1, "highlight", "other")) %>%  
  ggplot(aes(x = reorder(name, value), y = value, color=highlight)) +
  geom_segment(aes(xend = name, y = 0, yend = value, color = highlight), 
               linewidth = 1.25) +
  geom_point(size = 5)+
  scale_fill_manual(values = c("highlight" = si_palettes$moody_blue[1], 
                               "other" = "gray"))+
  scale_color_manual(values = c("highlight" = si_palettes$moody_blue[1], 
                               "other" = "gray"))+
  coord_flip() +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))+
  si_style_xgrid()+
  labs(title = "TOP 10 MOST POPULAR FEMALE NAMES SINCE 1880:", 
       subtitle= "<span style='color:#8980cb;'><b>MARY MORE THAN TWICE AS POPULAR</b></span> AS ALL OTHER NAMES ",
       x = NULL,
       y = "Number of People",
       caption="Source: Social Security Administration | Accessed 3/4/25") +
  theme(
    legend.position = "none",
    plot.subtitle = element_markdown()  # Enable HTML formatting in title
  )


print(top10_names_f_viz)

si_save("Images/SSA_BabyNames_Top10FemaleNames.png")


# male | Lollipop graph
top10_names_m_viz <- df_data %>% 
  filter(sex=="M") %>% 
  group_by(name) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  mutate(rank=rank(-value, ties.method = "first")) %>% 
  filter(rank <11) %>% 
  mutate(highlight=if_else(rank<3, "highlight", "other")) %>%  
  ggplot(aes(x = reorder(name, value), y = value, color=highlight)) +
  geom_segment(aes(xend = name, y = 0, yend = value, color = highlight), 
               linewidth = 1.25) +
  geom_point(size = 5)+
  scale_fill_manual(values = c("highlight" = si_palettes$genoa[1], 
                               "other" = "gray"))+
  scale_color_manual(values = c("highlight" = si_palettes$genoa[1], 
                                "other" = "gray"))+
  coord_flip() +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))+
  si_style_xgrid()+
  labs(title = "TOP 10 MOST POPULAR MALE NAMES SINCE 1880:", 
       subtitle= "<span style='color:#287c6f;'><b>JAMES AND JOHN ARE THE MOST POPULAR</b></span> NAMES WITH ROBERT CLOSE BEHIND ",
       x = NULL,
       y = "Number of People",
       caption="Source: Social Security Administration | Accessed 3/4/25") +
  theme(
    legend.position = "none",
    plot.subtitle = element_markdown()  # Enable HTML formatting in title
  )


print(top10_names_m_viz)


si_save("Images/SSA_BabyNames_Top10MaleNames.png")


# When did the top 5 female names gain their popularity?

top5_names_f <- df_data %>% 
  filter(sex=="F") %>% 
  group_by(name) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  mutate(rank=rank(-value, ties.method = "first")) %>% 
  ungroup() %>% 
  filter(rank <=5)

#stream graph w curved arrow annotation
top5_names_f_stream <- df_data %>% 
  filter(sex == "F", 
         name %in% top5_names_f$name) %>% 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_stream(type = "mirror", bw = 0.6) +  
  geom_vline(xintercept=c(1946,1964), linetype = "dashed", color = grey20k, linewidth = 0.5) +
  annotate(
    geom = "curve", x = 1970, y = 95000, xend = 1957, yend = 100000, 
    curvature = .3,
    arrow = arrow(length = unit(3, "mm")),
    color=grey50k
  ) +
  annotate("text", x = 1970, y = 91000 , hjust="left",na.rm = TRUE, 
           label = "Baby Boomer Era", color = grey70k, size = 4,
           family="Source Sans Pro") +
  glitr::scale_fill_si("lavender_haze_d",discrete = TRUE)+
  # scale_y_continuous(labels = label_comma())+
  si_style_void()+
  labs(
    title = "POPULARITY OF TOP 5 FEMALE NAMES OVER TIME",
    subtitle = "<span style='color:#3B5BBE;'><b>LINDA</b></span> and <span style='color:#5BB5D5;'><b>PATRICIA</b></span> PEAKED DURING THE BABY BOOMER ERA",
    x = "Year",
    y = NULL,
    caption="Source: Social Security Administration | Accessed 3/4/25")+
  theme(
    legend.position = "right",
    plot.subtitle = element_markdown()  # Enable HTML formatting in title
  )
  
print(top5_names_f_stream)

si_save("Images/SSA_BabyNames_Top5FemaleNames_StreamGraph.png")


#faceted area graph with annotation
female_facet_annotation<-data.frame(
  name="Linda",
  year=1965,
  value=95000,
  label="Baby Boomer Era")


top5_names_f_facet_area<-df_data %>% 
  filter(sex == "F", 
         name %in% top5_names_f$name) %>% 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_area() +  
  glitr::scale_fill_si("lavender_haze_d",discrete = TRUE)+
  scale_y_continuous(labels = label_comma())+
  si_style_ygrid()+
  geom_vline(xintercept = c(1946, 1964), linetype = "dashed", color = grey20k, linewidth = 0.5) +
  facet_wrap(~name)+
  geom_text(data=female_facet_annotation, aes(x=year, y=value, label=label),
            inherit.aes=FALSE, color=grey60k, size=3, hjust="left")+
  geom_segment(data = df_data %>% filter(name == "Linda"),aes(x=1964, xend = 1960, y = 95000, yend = 95000),
               color = grey60k,
               linewidth = 0.25) +
  labs(
    title = "POPULARITY OF TOP 5 FEMALE NAMES OVER TIME",
    subtitle = "<span style='color:#3B5BBE;'><b>LINDA</b></span> and <span style='color:#5BB5D5;'><b>PATRICIA</b></span> PEAKED DURING THE BABY BOOMER ERA",
    x = "Year",
    y = NULL,
    caption="Source: Social Security Administration | Accessed 3/4/25")+
  theme(
    legend.position = "none",
    plot.subtitle = element_markdown()  # Enable HTML formatting in title
  )
  
print(top5_names_f_facet_area)

si_save("Images/SSA_BabyNames_Top5FemaleNames_FacetedAreaGraph.png")


# When did the top 5 male names gain their popularity?

top5_names_m <- df_data %>% 
  filter(sex=="M") %>% 
  group_by(name) %>% 
  summarize_at(vars(value),sum,na.rm=TRUE) %>% 
  mutate(rank=rank(-value, ties.method = "first")) %>% 
  ungroup() %>% 
  filter(rank <=5)

#stream graph w curved arrow annotation
male_facet_annotation<-data.frame(
  name="Michael",
  year=1965,
  value=90000,
  label="Baby Boomer Era")


top5_names_m_stream <- df_data %>% 
  filter(sex == "M", 
         name %in% top5_names_m$name) %>% 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_stream(type = "mirror", bw = 0.6) +  
  glitr::scale_fill_si("hunter_d",discrete = TRUE)+
  # scale_y_continuous(labels = label_comma())+
  geom_vline(xintercept = c(1946, 1964), linetype = "dashed", color = grey50k, linewidth = 0.335) +
  annotate(
    geom = "curve", x = 1970, y = 195000, xend = 1957, yend = 197000, 
    curvature = .3,
    arrow = arrow(length = unit(3, "mm")),
    color=grey50k
  ) +
  annotate("text", x = 1970, y = 190000 , hjust="left",na.rm = TRUE, 
           label = "Baby Boomer Era", color = grey70k, size = 4,
           family="Source Sans Pro") +
  si_style_void()+
  labs(
    title = "POPULARITY OF TOP 5 MALE NAMES OVER TIME",
    subtitle = "<span style='color:#E14BA1;'><b>MICHAEL'S</b></span> POPULARITY STARTED DURING THE BABY BOOMER ERA WHILE OTHERS PRE-DATED THIS TIME",
    x = "Year",
    y = NULL,
    caption="Source: Social Security Administration | Accessed 3/4/25")+
  theme(
    legend.position = "right",
    plot.subtitle = element_markdown()  # Enable HTML formatting in title
  )

print(top5_names_m_stream)

si_save("Images/SSA_BabyNames_Top5MaleNames_StreamGraph.png")

# faceted area graph
top5_names_m_facet_area<-df_data %>% 
  filter(sex == "M", 
         name %in% top5_names_m$name) %>% 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_area() +  
  glitr::scale_fill_si("hunter_d",discrete = TRUE)+
  scale_y_continuous(labels = label_comma())+
  geom_vline(xintercept = c(1946, 1964), linetype = "dashed", color = grey20k, linewidth = 0.5) +
  geom_segment(data = df_data %>% filter(name == "Michael"),
               aes(x=1964, xend = 1960, y = 90000, yend = 90000),
               color = grey60k,
               linewidth = 0.25) +
  geom_text(data=male_facet_annotation, aes(x=year, y=value, label=label),
            inherit.aes=FALSE, color=grey60k, size=3, hjust="left")+
  si_style_ygrid()+
  facet_wrap(~name)+
  labs(
    title = "POPULARITY OF TOP 5 MALE NAMES OVER TIME",
    subtitle = "<span style='color:#E14BA1;'><b>MICHAEL'S</b></span> POPULARITY STARTED DURING THE BABY BOOMER ERA WHILE OTHERS PRE-DATED THIS TIME",
    x = "Year",
    y = NULL,
    caption="Source: Social Security Administration | Accessed 3/4/25")+
  theme(
    legend.position = "none",
    plot.subtitle = element_markdown()  # Enable HTML formatting in title
  )

print(top5_names_m_facet_area)

si_save("Images/SSA_BabyNames_Top5MaleNames_FacetedAreaGraph.png")