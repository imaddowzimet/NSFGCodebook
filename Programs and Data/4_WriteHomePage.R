# System set-up
library(tidyverse)

# Load data
alldata <- readRDS("Data/completeNSFGmetadata.rds") %>% filter(type == "Female" | type == "Male")

# Grab one version of variable and question for each variable (in other words, if there are multiple
# capitalizations of the variable, or slightly different questions,
# the first one will display on the home page, though
# on the actual variable page it will be distinguished)
alldata %>% mutate(sectionorder = str_remove(section, "SECTION "), 
                   sectionorder = ifelse(str_detect(section, "RESPONDENT"), 0, sectionorder)) %>% 
  arrange(sectionorder) %>%
  mutate(lowervars = tolower(varname), index = row_number()) %>% 
  group_by(lowervars) %>%
  summarize(varname = first(varname), 
            question = first(question), 
            index = first(index), 
            section = first(section),
            sectionorder = first(sectionorder)) %>% arrange(sectionorder, index) -> alldata_homepage

# Grab list of variables that have different capitalizations across waves; for those
# we'll just show the lower case version on the home page
alldata %>% distinct(varname) %>% pull() %>% as.character() -> allvars
allvars_lower<- tolower(allvars)
alldata %>% filter(varname %in% allvars[duplicated(allvars_lower)]) %>% 
  mutate(lowervar = tolower(varname)) %>% pull(lowervar) %>% unique() -> mixedcasevars

# Generate home page (basecode has some preamble and the javascript search function)
basepage <- readLines("basecode.html")
vars <- alldata_homepage %>% mutate(vars = ifelse(tolower(varname) %in% mixedcasevars, tolower(varname) , varname)) %>%
                    pull(vars)
labels <- alldata_homepage$question
section <- alldata_homepage$section
for (i in 1:length(vars)) {
  
  if (i == 1) {
    sectioncode <- paste0('<h3> ', section[i], ':</h3>')
    basepage <- c(basepage, sectioncode)
  } else if (section[i]!=section[i-1]) {
    sectioncode <- paste0('<h3> ', section[i], ':</h3>')
    basepage <- c(basepage, sectioncode)
  }
  
  print(i)
  print(vars[i])
  print(labels[i])
  varcode <- paste0('<a href="HTML_Var_Pages/', vars[i], '.html"><strong>', vars[i], '</strong>: ', labels[i], "</a>")
  basepage <- c(basepage, "<li>")
  basepage <- c(basepage, varcode)
  basepage <- c(basepage, "</li>")
}

# Add in pregnancy variables
alldata_pregnancy <- readRDS("Data/completeNSFGmetadata.rds") %>% filter(type == "Pregnancy")

# Grab one version of variable and question for each variable (in other words, if there are multiple
# capitalizations of the variable, or slightly different questions,
# the first one will display on the home page, though
# on the actual variable page it will be distinguished)
alldata_pregnancy %>% mutate(section = "PREGNANCY FILE:",
                             lowervars = tolower(varname), 
                             index = row_number()) %>% 
  group_by(lowervars) %>%
  summarize(varname = first(varname), 
            question = first(question), 
            index = first(index), 
            section = first(section)) %>% arrange(index) -> alldata_homepage_pregnancy

# Grab list of variables that have different capitalizations across waves; for those
# we'll just show the lower case version on the home page
alldata_pregnancy %>% distinct(varname) %>% pull() %>% as.character() -> allvars
allvars_lower<- tolower(allvars)
alldata_pregnancy %>% filter(varname %in% allvars[duplicated(allvars_lower)]) %>% 
  mutate(lowervar = tolower(varname)) %>% pull(lowervar) %>% unique() -> mixedcasevars

preg_vars <- alldata_homepage_pregnancy %>% mutate(vars = ifelse(tolower(varname) %in% mixedcasevars, tolower(varname) , varname)) %>%
  pull(vars)
preg_labels <- alldata_homepage_pregnancy$question
section <- alldata_homepage_pregnancy$section
for (i in 1:length(preg_vars)) {
  
  if (i == 1) {
    sectioncode <- paste0('<h3> ', section[i], ':</h3>')
    basepage <- c(basepage, sectioncode)
  }
  
  print(i)
  print(preg_vars[i])
  print(preg_labels[i])
  varcode <- paste0('<a href="HTML_Var_Pages/pregnancy_', preg_vars[i], '.html"><strong>', preg_vars[i], '</strong>: ', preg_labels[i], " (pregnancy file)</a>")
  basepage <- c(basepage, "<li>")
  basepage <- c(basepage, varcode)
  basepage <- c(basepage, "</li>")
}

basepage <- c(basepage, "</ul>")
basepage <- c(basepage, "</body>")
basepage <- c(basepage, "</html>")
write_lines(basepage, "../index.html")