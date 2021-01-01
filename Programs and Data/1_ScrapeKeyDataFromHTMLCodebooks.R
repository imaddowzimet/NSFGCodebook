### System set-up
library(rvest)
library(tidyverse)
library(stringr)
library(eply)
source("helperfunctions.r")

# This code scrapes key data from downloaded HTML versions of the NSFG webdocs (created using the
# "generate codebook for "section [X]" link, as described in the user guide, and saved in the HTML
# folder at root). For 2011-13 on, this is done fairly economically using a function; for earlier waves
# the structure is a bit different, so the code is more verbose (but also could probably be wrapped
# in a function, eventually)

# List all HTML files we are going to scrape
htmlfilenames <- list.files("HTML/", full.names = T)

################################################################
### Female Respondents, 2002
################################################################

html_list <- htmlfilenames[str_detect(htmlfilenames, "2002_FEMALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z]")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}

# Extract key data from each section
NSFG_2002_Female_Resp <- list()
for (i in 1:length(codebooksHTML)) {
  section <- section_names[i]  
  print(section)
  
  # Extract variables 
  # Note that this includes column numbers
  codebooksHTML[[i]] %>% html_nodes("p b") %>% html_text() -> variables
  
  # Extract tables
  codebooksHTML[[i]] %>% html_nodes("form table") %>% html_table() -> alltables
  
  # Remove third column, which is just a blank column filled with NAs; not clear why
  # and convert to a list of tibbles
  map(alltables, select, -3) %>% map(as_tibble) -> alltables
  
  # Extract everything else
  codebooksHTML[[i]] %>% html_nodes("p") %>% html_text() -> alltext
  
  # Extract variable type
  variabletype <- extract_key_text(alltext, "^Variable Type : ")
  
  # Extract universe 
  universe <- extract_key_text(alltext, "^Universe : ")
  
  # Extract notes (if any)
  notes <- tibble(notes = alltext[which(str_detect(alltext, "Notes"))], 
                  var_raw = alltext[which(str_detect(alltext, "Notes")) - 4])
  
  # Extract description or question
  alltext %>% 
    enframe() %>% 
    filter(str_detect(value, "^\\nDescription : ") |
             str_detect(value, "^\\n[A-Z]{1,2}-[0-9]+[A-Za-z]* : ")) %>%
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
    filter(str_detect(value, "^\\nDescription : ") |
             str_detect(value, "^\\n[A-Z]{1,2}-[0-9]+[A-Za-z]* : ")) %>%
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
  NSFG_2002_Female_Resp[[i]] <- metadata
  
}
NSFG_2002_Female_Resp_FULL <- bind_rows(NSFG_2002_Female_Resp)
write_rds(NSFG_2002_Female_Resp_FULL, "Data/NSFG_2002_Female_Resp_FULL")

################################################################
### Male Respondents, 2002
################################################################

html_list <- htmlfilenames[str_detect(htmlfilenames, "2002_MALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z]")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}

# Extract key data from each section
NSFG_2002_Male_Resp <- list()
for (i in 1:length(codebooksHTML)) {
  section <- section_names[i]  
  print(section)
  
  # Extract variables 
  # Note that this includes column numbers
  codebooksHTML[[i]] %>% html_nodes("p b") %>% html_text() -> variables
  
  # Extract tables
  codebooksHTML[[i]] %>% html_nodes("form table") %>% html_table() -> alltables
  
  # Remove third column, which is just a blank column filled with NAs; not clear why
  # and convert to a list of tibbles
  map(alltables, select, -3) %>% map(as_tibble) -> alltables
  
  # Extract everything else
  codebooksHTML[[i]] %>% html_nodes("p") %>% html_text() -> alltext
  
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
  NSFG_2002_Male_Resp[[i]] <- metadata
  
}
NSFG_2002_Male_Resp_FULL <- bind_rows(NSFG_2002_Male_Resp)
write_rds(NSFG_2002_Male_Resp_FULL, "Data/NSFG_2002_Male_Resp_FULL")

################################################################
### Pregnancy, 2002
################################################################

html_list <- htmlfilenames[str_detect(htmlfilenames, "2002_FEM_PREG")]

codebooksHTML <- list()
codebooksHTML[[1]] <-  read_html(html_list[1])

# Extract key data from each section
NSFG_2002_Fem_Preg <- list()
for (i in 1:length(codebooksHTML)) {
  section <- "not available"
  print(section)
  
  # Extract variables 
  # Note that this includes column numbers
  codebooksHTML[[i]] %>% html_nodes("p b") %>% html_text() -> variables
  
  # Extract tables
  codebooksHTML[[i]] %>% html_nodes("form table") %>% html_table() -> alltables
  
  # Remove third column, which is just a blank column filled with NAs; not clear why
  # and convert to a list of tibbles
  map(alltables, select, -3) %>% map(as_tibble) -> alltables
  
  # Extract everything else
  codebooksHTML[[i]] %>% html_nodes("p") %>% html_text() -> alltext
  
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
  section <- "Not available"
  
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
  NSFG_2002_Fem_Preg[[i]] <- metadata
  
}
NSFG_2002_Fem_Preg_FULL <- bind_rows(NSFG_2002_Fem_Preg)
write_rds(NSFG_2002_Fem_Preg_FULL, "Data/NSFG_2002_Fem_Preg_FULL")

################################################################
### Female Respondents, 2006-10
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "200610_FEMALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z]")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}

# Extract key data from each section
NSFG_200610_Female_Resp <- list()
for (i in 1:length(codebooksHTML)) {
  section <- section_names[i]  
  print(section)
  
  # Extract variables 
  # Note that this includes column numbers
  codebooksHTML[[i]] %>% html_nodes("p b") %>% html_text() -> variables
  
  # Remove bolded ORs and ANDs, which get picked up accidentally (there shouldn't
  # ever be a variable that is just two letters, so this code is safe)
  variables <- filter(enframe(variables), str_detect(value, "^[A-Za-z]{2,3}$", negate = T))
  variables <- variables$value
  
  # Extract tables
  codebooksHTML[[i]] %>% html_nodes("form table") %>% html_table() -> alltables
  
  # Remove third column, which is just a blank column filled with NAs; not clear why
  # and convert to a list of tibbles
  map(alltables, select, -3) %>% map(as_tibble) -> alltables
  
  # Extract everything else
  codebooksHTML[[i]] %>% html_nodes("p") %>% html_text() -> alltext
  
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
  NSFG_200610_Female_Resp[[i]] <- metadata
  
}
NSFG_200610_Female_Resp_FULL <- bind_rows(NSFG_200610_Female_Resp)
write_rds(NSFG_200610_Female_Resp_FULL, "Data/NSFG_200610_Female_Resp_FULL")



################################################################
### Male Respondents, 2006-10
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "200610_MALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z]")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}


# Extract key data from each section
NSFG_200610_Male_Resp <- list()
for (i in 1:length(codebooksHTML)) {
  section <- section_names[i]  
  print(section)
  
  # Extract variables 
  # Note that this includes column numbers
  codebooksHTML[[i]] %>% html_nodes("p b") %>% html_text() -> variables
  
  # Remove bolded ORs and ANDs, which get picked up accidentally (there shouldn't
  # ever be a variable that is just two or three letters, so this code is safe)
  variables <- filter(enframe(variables), str_detect(value, "^[A-Za-z]{2,3}$", negate = T))
  variables <- variables$value
  
  # Extract tables
  codebooksHTML[[i]] %>% html_nodes("form table") %>% html_table() -> alltables
  
  # Remove third column, which is just a blank column filled with NAs; not clear why
  # and convert to a list of tibbles
  map(alltables, select, -3) %>% map(as_tibble) -> alltables
  
  # Extract everything else
  codebooksHTML[[i]] %>% html_nodes("p") %>% html_text() -> alltext
  
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
  NSFG_200610_Male_Resp[[i]] <- metadata
  
}
NSFG_200610_Male_Resp_FULL <- bind_rows(NSFG_200610_Male_Resp)
write_rds(NSFG_200610_Male_Resp_FULL, "Data/NSFG_200610_Male_Resp_FULL")


################################################################
### Pregnancy, 2006-10
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "200610_FEM_PREG")]

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}


# Extract key data from each section
NSFG_200610_Fem_Preg <- list()
for (i in 1:length(codebooksHTML)) {
  section <- "not available"
  print(section)
  
  # Extract variables 
  # Note that this includes column numbers
  codebooksHTML[[i]] %>% html_nodes("p b") %>% html_text() -> variables
  
  # Remove bolded ORs and ANDs, which get picked up accidentally (there shouldn't
  # ever be a variable that is just two letters, so this code is safe)
  variables <- filter(enframe(variables), str_detect(value, "^[A-Za-z]{2,3}$", negate = T))
  variables <- variables$value
  
  # Extract tables
  codebooksHTML[[i]] %>% html_nodes("form table") %>% html_table() -> alltables
  
  # Remove third column, which is just a blank column filled with NAs; not clear why
  # and convert to a list of tibbles
  map(alltables, select, -3) %>% map(as_tibble) -> alltables
  
  # Extract everything else
  codebooksHTML[[i]] %>% html_nodes("p") %>% html_text() -> alltext
  
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
  section <- "Not available"
  
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
  NSFG_200610_Fem_Preg[[i]] <- metadata
  
}
NSFG_200610_Fem_Preg_FULL <- bind_rows(NSFG_200610_Fem_Preg)
write_rds(NSFG_200610_Fem_Preg_FULL, "Data/NSFG_200610_Fem_Preg_FULL")
################################################################
### Female Respondents, 2011-13
################################################################

html_list <- htmlfilenames[str_detect(htmlfilenames, "201113_FEMALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z0-9]+")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}

scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201113_Female_Resp_FULL")

################################################################
### Male Respondents, 2011-13
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201113_MALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z0-9]+")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}

scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201113_Male_Resp_FULL")

################################################################
### Pregnancy, 2011-13
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201113_FEM_PREG")]
section_names <- "Not available"

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201113_Fem_Preg_FULL")

################################################################
### Female Respondents, 2013-15
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201315_FEMALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z0-9]+")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201315_Female_Resp_FULL")

################################################################
### Male Respondents, 2013-15
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201315_MALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z0-9]+")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201315_Male_Resp_FULL")

################################################################
### Pregnancy, 2013-15
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201315_FEM_PREG")]
section_names <- "Not available"

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201315_Fem_Preg_FULL")

################################################################
### Female Respondents, 2015-17
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201517_FEMALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z0-9]+")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201517_Female_Resp_FULL")

################################################################
### Male Respondents, 2015-17
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201517_MALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z0-9]+")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201517_Male_Resp_FULL")

################################################################
### Pregnancy, 2015-17
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201517_FEM_PREG")]
section_names <- "Not available"

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201517_Fem_Preg_FULL")

################################################################
### Female Respondents, 2017-19
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201719_FEMALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z0-9]+")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201719_Female_Resp_FULL")

################################################################
### Male Respondents, 2017-19
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201719_MALE")]
section_names <- str_extract(html_list, "(?<=SECTION_)[A-Z0-9]+")

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201719_Male_Resp_FULL")

################################################################
### Pregnancy, 2017-19
################################################################
html_list <- htmlfilenames[str_detect(htmlfilenames, "201719_FEM_PREG")]
section_names <- "Not available"

# Extract HTML of codebook for each section
codebooksHTML <- list()
for (i in 1:length(html_list)) {
  
  codebooksHTML[[i]] <-  read_html(html_list[i])
  
}
scrape_webdoc(codebooksHTML, section_names, "Data/NSFG_201719_Fem_Preg_FULL")

