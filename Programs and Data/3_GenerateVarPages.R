library(tidyverse)

# Function to generate complete variable pages
render_fun <- function(varname) {
  rmarkdown::render(
    input = "Template_MALE_FEMALE.rmd",
    params = list(varname = varname),
    output_file = glue::glue("HTML_Var_Pages/{varname}.html")
  )
}

# find variables to create pages for, and create pages
alldata <- readRDS("Data/completeNSFGmetadata.rds") %>% filter(type == "Female" | type == "Male")
alldata %>% distinct(varname) %>% pull() %>% as.character() -> allvars
purrr::walk(allvars, render_fun)

# Grab any variables that didn't generate for some reason
test<- list.files("../HTML_VAR_Pages")
test <- str_remove(test, "\\.html")
missingvars<- setdiff(allvars, test)
purrr::walk(missingvars, render_fun)

# Regenerate for situations where variables are uppercase in one wave and lower case in another
allvars_lower<- tolower(allvars)
alldata %>% filter(varname %in% allvars[duplicated(allvars_lower)]) %>% 
  mutate(lowervar = tolower(varname)) %>% pull(lowervar) %>% unique() -> mixedcasevars

render_fun_mixed <- function(varname) {
  rmarkdown::render(
    input = "Template_MALE_FEMALE_MixedCase.rmd",
    params = list(varname = varname),
    output_file = glue::glue("../HTML_Var_Pages/{varname}.html")
  )
}

purrr::walk(mixedcasevars, render_fun_mixed)






