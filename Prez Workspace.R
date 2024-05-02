library(haven)
library(readxl)
library(rmdformats)
library(patchwork)
library(cowplot)
library(dplyr)
library(knitr)
library(tidyr)
library(tidyverse)
library(ggplot2)

#LOAD DATA----
events = read_excel("events.xlsx")
View(events)
#GRAPH 1-----
# Define data
frequency_table1 = table(events$PTYPE)
frequency_levels = c("0 - Low", "500 - Medium Low", "1000 - Medium", "2000 - High")
colors = c("lavender", "lightpink", "plum", "mediumpurple1")

custom_labels = c("General Warfare", "Inter-communal Warfare", "Armed Battle", "Armed Attack", "Pro-Government Terrorism",
                   "Anti-Government Terrorism","Communal Terrorism", "Organized Violent Riot", "Spontaneous Violent Riot", 
                   "Organized Demonstration","Pro-Government Demonstration", "Spontaneous Demonstration", "Other")
frequency_table1 = c(500, 1200, 300, 1500, 2500, 800, 2000, 700, 1000, 400, 600, 200, 300)

df = data.frame(custom_labels = custom_labels, frequency_table1 = frequency_table1)

# Plot using ggplot2
graph6 = ggplot(df, aes(x = custom_labels, y = frequency_table1, fill = cut(frequency_table1, breaks = c(0, 500, 1000, 2000, 3000), labels = frequency_levels))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = custom_labels), vjust = -0.7, size = 2.3, fontface = "bold") +
  labs(title = "Societal Conflict Magnitude",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = c(0.95, 0.9),  
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +  
  scale_fill_manual(values = colors) +
  guides(fill = guide_legend(title = "Magnitude Levels")) +
  geom_hline(yintercept = 0) +
  ggtitle("Societal Conflict Magnitude") +
  scale_x_discrete(labels = NULL) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, color = "black", fill = NA)

print(graph6)

           








#COMBINED GRAPH ----
#Filter by country
nev = events |>
  mutate(NPART = ifelse(NPART == "99", "0", NPART))

flevents = nev |>
  select(NPART, NDEATH, CITY, CITY_ID, COUNTRY) |>
  group_by(COUNTRY) %>%
  summarise(NPART = first(NPART),
            NDEATH = first(NDEATH),
            CITY = first(CITY),
            CITY_ID = toString(unique(CITY_ID)))


#Filter by Gulf
gulf_countries = c("Bahrain", "Oman", "Qatar", "Saudi Arabia", "Kuwait", "United Arab Emirates")
gulf_events <- flevents |>
  filter(COUNTRY %in% gulf_countries) |>
mutate(COUNTRY = factor(COUNTRY, levels = rev(gulf_countries))) 

gulf_events$NPART = factor(gulf_events$NPART, levels = c("0", "1", "4", "11"))


#Filter by LA
LA_countries = c("Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico", "Paraguay", "Venezuela")
LA_events = flevents |>
  filter(COUNTRY %in% LA_countries) 

graph2 = gulf_events |>
  ggplot(aes(x = COUNTRY, y = NPART, fill = factor(NPART))) +
  geom_bar(stat = "identity", color = "black") + 
  labs(title = "Participation by Gulf Countries",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("0" = "thistle1", "1" = "mistyrose1","11" = "plum3", "4"= "pink"),
                    name = "Number of Participants",
                    breaks = c("0", "1", "11", "4"), 
                    labels = c("0" = "Unknown", "1" = "Less than 10","11" = "Less than 1,000", "4" = "1,001-10,000")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.background = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        legend.position = c(0.88, 0.9))

print(graph2)

graph3 = LA_events |>
  ggplot(aes(x = COUNTRY, y = NPART, fill = factor(NPART))) +
  geom_bar(stat = "identity", color = "black") + 
  labs(title = "Participation by Latin American Countries",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("0" = "thistle1", "1" = "mistyrose1","11" = "plum3", "4"= "pink"),
                    name = "Number of Participants",
                    breaks = c("0", "1", "11", "4"), 
                    labels = c("0" = "Unknown", "1" = "Less than 10","11" = "Less than 1,000", "4" = "1,001-10,000")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = "none")               
print(graph3)

combined_plots = graph3 + graph2
print(combined_plots)


nev = events |>
  mutate(NPART = ifelse(NPART == "99", "0", NPART))

flevents = nev |>
  select(NPART, NDEATH, CITY, CITY_ID, COUNTRY) |>
  group_by(COUNTRY) |>
  summarise(NPART = first(NPART),
            NDEATH = first(NDEATH),
            CITY = first(CITY),
            CITY_ID = toString(unique(CITY_ID)))


#Filter by Gulf
gulf_countries = c("Bahrain", "Oman", "Qatar", "Saudi Arabia", "Kuwait", "United Arab Emirates")
gulf_events = flevents |>
  filter(COUNTRY %in% gulf_countries) |>
  mutate(COUNTRY = factor(COUNTRY, levels = rev(gulf_countries)))  

gulf_events$NPART = factor(gulf_events$NPART, levels = c("0", "1", "4", "11"))


#Filter by LA
LA_countries = c("Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico", "Paraguay", "Venezuela")
LA_events = flevents |>
  filter(COUNTRY %in% LA_countries) 

graph2 <- gulf_events |>
  ggplot(aes(x = COUNTRY, y = NPART, fill = factor(NPART))) +
  geom_bar(stat = "identity", color = "black") + 
  labs(title = "Participation by Gulf Countries",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("0" = "thistle1", "1" = "mistyrose1","11" = "plum3", "4"= "pink"),
                    name = "Number of Participants",
                    breaks = c("0", "1", "11", "4"), 
                    labels = c("0" = "Unknown", "1" = "Less than 10","11" = "Less than 1,000", "4" = "1,001-10,000")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.background = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        legend.position = c(0.88, 0.9))

print(graph2)

graph3 = LA_events |>
  ggplot(aes(x = COUNTRY, y = NPART, fill = factor(NPART))) +
  geom_bar(stat = "identity", color = "black") + 
  labs(title = "Participation by Latin American Countries",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("0" = "thistle1", "1" = "mistyrose1","11" = "plum3", "4"= "pink"),
                    name = "Number of Participants",
                    breaks = c("0", "1", "11", "4"), 
                    labels = c("0" = "Unknown", "1" = "Less than 10","11" = "Less than 1,000", "4" = "1,001-10,000")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = "none")               
print(graph3)

combined_plots = graph3 + graph2
print(combined_plots)