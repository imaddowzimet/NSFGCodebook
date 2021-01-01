
# System set-up
library(tidyverse)

# Load datasets, label and combine
datasets <- list()
datasetnames <- list.files("Data/", full.names = T)
datasetnames <- datasetnames[!(str_detect(datasetnames, "Archive") | str_detect(datasetnames, "complete"))]
for (i in 1:length(datasetnames)) {
  year <- str_extract(datasetnames[i], "[0-9]{4,6}")
  type <- case_when(str_detect(datasetnames[i], "Female") ~ "Female",
                    str_detect(datasetnames[i], "Male")   ~ "Male",
                    TRUE ~ "Pregnancy")
  datasets[[i]] <- read_rds(datasetnames[i])
  datasets[[i]]$year <- year
  datasets[[i]]$vartype <- datasets[[i]]$type
  datasets[[i]]$type <- type
}
alldata <- map_df( datasets, bind_rows)

# Check to make sure there aren't duplicate entries (in case I scraped the same thing twice)
dim(distinct(alldata[,-2])) - dim(alldata[,-2]) 

# remove - a few of these are duplicate entries in the actual codebook, strangely
duplicaterows <- which(duplicated(alldata[,-2]))
alldata <- alldata[-duplicaterows,]
dim(distinct(alldata[,-2])) - dim(alldata[,-2]) 

# Clean up variable names, sections and sub sections
alldata %>% mutate(varname = str_remove(var_raw, "\\([0-9]*\\-[0-9]*\\)$"),
                   section = ifelse(str_detect(section, "^[0-9]+$"), 
                                    str_extract(sub_section, "(?<=SECTION )[A-Z]"),
                                    section),
                   sub_section = str_remove(sub_section, "[A-Za-z ]+::[A-Za-z ]+: "),
                   sub_section = str_to_sentence(sub_section),
                   sub_section = str_replace(sub_section, "\\(.*\\)", toupper),
                   sub_section = str_replace(sub_section, "(?<=[sS]ection )[a-z]", toupper),
                   sub_section = str_replace(sub_section, "(?<=[sS]ection [A-Z] and )[a-z]", toupper),
                   sub_section = str_replace(sub_section, "casi", toupper),
                   sub_section = case_when(type == "Pregnancy" ~ paste("Pregnancy:", sub_section),
                                           section == "R" ~ paste("RECODES:", sub_section),
                                           TRUE ~ sub_section),
                   section = case_when(section == "R" ~ "RECODES",
                                       section == "W" ~ "WEIGHTS AND OTHER VARIABLES",
                                       type == "Pregnancy" ~ "PREGNANCY",
                                       str_detect(sub_section, "[Rr]espondent id and screener items") ~ "RESPONDENT ID AND SCREENER ITEMS",
                                       TRUE ~ paste("SECTION", section)),
                   question = str_remove(question, "Description :"),
                   notes = str_remove(notes, "Notes :")) -> alldata

# NOTE: Sub sections would need to be improved and harmonized if they are going to be useful

saveRDS(alldata, "Data/completeNSFGmetadata.rds")


