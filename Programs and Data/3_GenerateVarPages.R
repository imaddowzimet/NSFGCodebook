library(tidyverse)
render_fun <- function(varname) {
  rmarkdown::render(
    input = "Template_MALE_FEMALE.rmd",
    params = list(varname = varname),
    output_file = glue::glue("HTML_Var_Pages/{varname}.html")
  )
}

alldata <- readRDS("Data/completeNSFGmetadata.rds") %>% filter(type == "Female" | type == "Male")
alldata %>% distinct(varname) %>% pull() %>% as.character() -> allvars
purrr::walk(allvars, render_fun)
test<- list.files("HTML_VAR_Pages")
test <- str_remove(test, "\\.html")
missingvars<- setdiff(allvars, test)
purrr::walk(missingvars, render_fun)

allvars_lower<- tolower(allvars)
alldata %>% filter(varname %in% allvars[duplicated(allvars_lower)]) %>% 
  mutate(lowervar = tolower(varname)) %>% pull(lowervar) %>% unique() -> mixedcasevars

render_fun_mixed <- function(varname) {
  rmarkdown::render(
    input = "Template_MALE_FEMALE_MixedCase.rmd",
    params = list(varname = varname),
    output_file = glue::glue("HTML_Var_Pages/{varname}.html")
  )
}

purrr::walk(mixedcasevars, render_fun_mixed)

# Once I get basic thing working, I should basically take this list of variables and make them
# all lower case or uppercase (depending on what's more frequent), and then add a note to the
# rows I changed that the variable is uppercase
# alternately, i can change the template just for these, with the parameter set to an upper
# case version, but the varnames set to the original version - that might be easier 
# right now, randvar1 doesn't work because 




