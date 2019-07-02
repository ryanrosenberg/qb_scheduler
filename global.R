library(tidyverse)
library(shiny)
library(knitr)
library(kableExtra)
library(sortable)
library(glue)

teams <- tibble("Site" = c(rep("Northeast", 2), rep("Midwest", 12)),
                "Team" = c("Yale", "Harvard", paste("Chicago", LETTERS[1:12])))
packets <- paste("Chicago", LETTERS[1:12], "+ OSU")

formats <- read_csv("formats-nest.csv") %>% 
  group_by(Teams, Format) %>% 
  nest() %>% 
  mutate(data = map(data, function(df){
    df %>% 
      select_if(~!all(is.na(.)))
  })) 

