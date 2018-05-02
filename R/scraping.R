library(tidyverse)
library(rvest)
library(glue)


tims_scaper <- function(base_url, target) {
  
  if (!str_detect(target, ".html\\b")) {
    target <- paste0(target, ".html")
  }
  
  if (str_detect(target, "\\.\\.")|grepl("norfolk|whitehorse|dover|aberdeen", target)) {
    
    target <- gsub("\\.\\.\\/", "", target)
    
    out <- tibble(item  = c("store_name", "address", "city", "state", "postal_code"),
           nodes =  c(".Teaser-titleLink",".c-address-street-1", ".c-address-city" , ".c-address-state" , ".c-address-postal-code" )) %>% 
      mutate(data = map(nodes, ~read_html(glue(base_url, "{target}")) %>% 
                               html_nodes(.x) %>% 
                               html_text())) %>% 
      unnest() %>%
      group_by(item) %>% 
      mutate(idx = row_number()) %>% 
      ungroup() %>% 
      select(-nodes) %>% 
      spread(item, data) %>% 
      select(-idx) %>% 
      filter(map_lgl(gsub(" ", "-", city), ~grepl(.x, target, ignore.case = TRUE)))
    
    
  
  } else {
  
 out <- read_html(glue(base_url, "{target}")) %>% 
    html_nodes(".c-directory-list-content-item-link") %>% 
    html_attr("href") 
 }
 
 if (is_empty(out)) {
   
   return(target)
 } else {
   return(out)
 }
  
  
}

tims_data <- tibble(base_url = "https://locations.timhortons.com/",
                    country = c("ca", "us")) %>% 
  mutate(provinces_states = pmap(list(base_url, country), tims_scaper)) %>% 
  unnest() %>% 
  mutate(cities = pmap(list(base_url, provinces_states), tims_scaper)) %>% 
  unnest() %>% 
  mutate(location_data = pmap(list(base_url, cities), tims_scaper)) 

out <- tims_data %>% 
  select(country, location_data) %>% 
  unnest() %>% 
  mutate(out, country = ifelse(country == "ca", "CA", "US"))

saveRDS(out, "TimHortonsLocations.RDS")
  



            

          