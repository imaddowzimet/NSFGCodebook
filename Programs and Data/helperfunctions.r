# Function to extract section IDs; 2002 and 2006-10
extract_section_ids <- function(url) {
  result <- read_html(url) %>% 
    html_nodes("form a") %>% 
    html_attr("onclick") %>% 
    enframe() %>% 
    filter(!is.na(value)) %>%
    pull(value) %>% 
    str_extract("'[A-Za-z0-9]*'") %>% 
    unquote()
}

# Function to extract section IDs; 2011-2013
# THis is extremely unelegant, but works
extract_section_ids_alt <- function(url, type) {
read_html(url) %>% 
  html_nodes("td a") %>% html_attrs() -> temp
  
  temp[str_detect(temp, paste0(type,"Respondent"))] %>%
  unlist() %>%
  enframe() %>%
  filter(str_detect(value, "variableGroupParent")) %>%
  mutate(value = str_extract(value,"(?<=variableGroupParent/)[0-9]+(?=\\?)")) %>%
  pull(value)  
}


# Function to extract other key pieces
extract_key_text <- function(text, pattern) {
  text %>% 
    enframe() %>% 
    filter(str_detect(value, pattern)) %>%
    mutate(value = str_remove_all(value, pattern)) %>%
    pull(value) 
}


#### Function to do full scrape
scrape_webdoc <- function(links, section_names, filename) {
  NSFG <- list()
  for (i in 1:length(links)) {
    section <- section_names[i]  
    print(section)
    
    # Extract variables 
    # Note that this includes column numbers
    links[[i]] %>% html_nodes("p b") %>% html_text() -> variables
    
    # Remove bolded ORs and ands, which get picked up accidentally (there shouldn't
    # ever be a variable that is just two letters, so this code is safe)
    variables <- filter(enframe(variables), str_detect(value, "^[A-Za-z]{2,3}$", negate = T))
    variables <- variables$value
    
    # Extract tables
    links[[i]] %>% html_nodes("table") %>% html_table() -> alltables
    
    # Remove third column, which is just a blank column filled with NAs; not clear why
    # and convert to a list of tibbles
    map(alltables, select, -3) %>% map(as_tibble) -> alltables
    
    # Extract everything else
    links[[i]] %>% html_nodes("p") %>% html_text() -> alltext
    
    # Extract variable type
    variabletype <- extract_key_text(alltext, "^Variable Type : ")
    
    # Extract universe 
    universe <- extract_key_text(alltext, "^Universe : ")
    
    # Extract notes (if any)
    notes <- tibble(notes = alltext[which(str_detect(alltext, "Notes"))], 
                    var_raw = alltext[which(str_detect(alltext, "Notes")) - 4])
    
    # Extract description or question (always the line before universe)
    alltext[which(str_detect(alltext, "^Universe : "))-1] %>%
      enframe() %>% 
      mutate(value = str_remove_all(value, "^\\nDescription : "),
             value = str_remove_all(value, "^\\n[A-Z]{1,2}-[0-9]+[A-Za-z]* : "),
             value = str_remove_all(value, "\\n")) %>%
      pull(value) -> question
    
    # Extract Section
    section <- section_names[i]  
    
    # Extract Subsection
    sub_sections <- enframe(alltext) %>%
      mutate(has_subsect = str_detect(alltext, " :: "),
             sub_section = ifelse(has_subsect, 
                                  str_remove_all(value, "[A-Za-z ]+::[A-Za-z ]+:: "),
                                  NA),
             sub_section = str_remove_all(sub_section, "\\n")) %>% 
      fill(sub_section) %>%
      filter(str_detect(value, "^Universe : ")) %>%
      pull(sub_section)
    
    # Combine into one data frame
    metadata <-  tibble("section" = section, "sub_section" = sub_sections,
                        bind_cols("var_raw" = variables, 
                                  "type" = variabletype, 
                                  "question" = question, 
                                  "universe" = universe), 
                        table = alltables)
    
    metadata <- left_join(metadata, notes, by = "var_raw")
    print(metadata)
    NSFG[[i]] <- metadata
    
  }
  NSFG_FULL <- bind_rows(NSFG)
  write_rds(NSFG_FULL, filename)
}

