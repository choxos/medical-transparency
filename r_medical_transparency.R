
# loading the required packages
pacman::p_load(dplyr,
               rtransparent, 
               metareadr, 
               europepmc)


# Load the datasets
## Load International Standard Serial Numbers (ISSNs)
ISSNs = read.csv("data/journals.csv")

## Some modifications are needed...
ISSNs = ISSNs %>% mutate(search.term = paste0("ISSN:", ISSN))
ISSNsQuery = ISSNs %>% 
        group_by(Category) %>% summarize(query = paste(search.term, 
                                                       collapse = " OR "))
ISSNsQuery = as.data.frame(ISSNsQuery)


# Now, we search for the articles in the EPMC database for each field and store all of them as a list
output_list = lapply(ISSNsQuery$query, function(issn) {
        search_string = paste0(
                "(",
                issn,
                ") ",
                'AND (SRC:"MED") 
    AND (LANG:"eng" OR LANG:"en" OR LANG:"us") 
    AND (FIRST_PDATE:[1900-01-01 TO 2024-12-31])
    AND (OPEN_ACCESS:y)
    AND (PUB_TYPE:"Journal Article" OR PUB_TYPE:"research-article")'
        )
        epmc_search(query = search_string, limit = 1000000, output = "parsed", verbose = FALSE)
})

names(output_list) = ISSNsQuery$Category

## Saving as an RDS file
saveRDS(output_list, "data/output_list.rds")



### Next, we download xmls in format accessible with metareadr.
### To skip errors (i.e., The metadata format 'pmc' is not supported by the
### item or by the repository.), first define a new function:

# Next, we download xmls in format accessible with rtransparent:

skipping_errors = function(x) tryCatch(mt_read_pmcoa(x), error = function(e) e)


# Function to download and save XML files
download_xml = function(pmcid) {
  # Remove "PMC" prefix
  pmcid_without_prefix = gsub("PMC", "", as.character(pmcid))
  
  # Attempt to download XML using skipping_errors function
  xml_file = skipping_errors(pmcid_without_prefix)

}

setwd("xmls")

# Iterate through each element in the filtered_list
for (element_name in names(output_list)) {
  print(element_name)
        
  setwd("xmls")
  # Create a folder for the current element
  dir.create(element_name)
  setwd(element_name)
  
  # Get the tibble for the current element
  current_tibble = filtered_list[[element_name]]
  
  # Iterate through each pmcid in the tibble
  for (pmcid in current_tibble$pmcid) {
    download_xml(pmcid)
  }
  
  setwd("../..")
}



#Now we run rtransparent:

opendata = list()

for (element_name in ISSNsQuery$Category) {
        print(element_name)
        
        setwd(element_name)
        
        filepath = dir(pattern=glob2rx("PMC*.xml"))
        
        results_table_all = lapply(filepath, rt_all_pmc)
        
        results_table_data = rt_data_code_pmc_list(
                filepath,
                remove_ns=F,
                specificity = "low")
        
        df = data.table::rbindlist(results_table_all, fill = TRUE)
        
        db = filtered_list[[element_name]]
        
        merged = merge(db, results_table_data, by = "pmid") %>% merge(df)
        
        mylist = list(merged)
        names(mylist) = c(element_name)
        
        opendata = c(opendata, mylist)
        
        setwd("..")
}

opendata = opendata %>% select(pmid,
                               pmcid,
                               doi,
                               title,
                               authorString,
                               journalTitle,
                               journalIssn,
                               publisher,
                               firstPublicationDate,
                               journalVolume,
                               pageInfo,
                               issue,
                               type,
                               category,
                               is_research,
                               is_review,
                               citedByCount,
                               is_coi_pred,
                               coi_text,
                               is_fund_pred,
                               fund_text,
                               is_register_pred,
                               register_text,
                               is_open_data,
                               open_data_category,
                               open_data_statements,
                               is_open_code,
                               open_code_statements)


# Adding publisher:
scimago = read.csv("data/scimagojr.csv", sep = ";")
scimago$Issn = gsub(",", "", scimago$Issn)

scimago = scimago %>% separate(Issn, c("issn1", "issn2"))
# Removing empty issns:
scimago = scimago[scimago$issn1 != "",]

# Adding dash between digits of ISSN:
scimago$issn1 = gsub("(\\d{4})(\\d{4})$", "\\1-\\2", scimago$issn1)
scimago$issn2 = gsub("(\\d{4})(\\d{4})$", "\\1-\\2", scimago$issn2)

# Modifying ISSN in opendata dataset:
opendata$journalIssn = toupper(opendata$journalIssn)
opendata$journalIssn2 = opendata$journalIssn
opendata = opendata %>% separate(journalIssn2, c("issn1", "issn2"), sep = "; ")

#### Merging scimago publisher with opendata:
opendata = merge(x = opendata, 
                            y = scimago[, c("issn1", "issn2", "Publisher")],
                            by.x = "issn1",
                            by.y = "issn1",
                            all.x = TRUE)

opendata = merge(x = opendata, 
                            y = scimago[, c("issn1", "issn2", "Publisher")],
                            by.x = "issn2.x",
                            by.y = "issn1",
                            all.x = TRUE)

opendata = merge(x = opendata, 
                            y = scimago[, c("issn1", "issn2", "Publisher")],
                            by.x = "issn1",
                            by.y = "issn2",
                            all.x = TRUE)


# Removing unnecessary variables:
opendata = opendata %>% 
        mutate(scimago_publisher = coalesce(Publisher.x, Publisher.y, Publisher)) %>%
        subset(select = -c(issn2.y, issn2.x, issn1.y, Publisher.x, Publisher.y, Publisher))


# Changing some of the publishers names:
opendata$scimago_publisher = ifelse(grepl("Elsevier", opendata$scimago_publisher), "Elsevier",
        ifelse(grepl("Springer|Nature|BioMed|BMC", opendata$scimago_publisher), "Nature Publishing Group", 
        ifelse(grepl("Sage|SAGE", opendata$scimago_publisher), "SAGE Publications Inc.", 
        ifelse(grepl("Wiley|Blackwell", opendata$scimago_publisher), "John Wiley & Sons Inc.", 
        ifelse(grepl("MDPI", opendata$scimago_publisher), "Multidisciplinary Digital Publishing Institute (MDPI)", 
        opendata$scimago_publisher)))))



# Adding RCT:
oa_trials = epmc_search(query = '(SRC:"MED") 
    AND (LANG:"eng" OR LANG:"en" OR LANG:"us") 
    AND (FIRST_PDATE:[1900-01-01 TO 2024-12-31])
    AND (OPEN_ACCESS:y)
    AND (PUB_TYPE:"Randomized Controlled Trial" OR PUB_TYPE:"Clinical Trial" OR PUB_TYPE:"Clinical Trial, Phase III" OR PUB_TYPE:"Clinical Trial, Phase IV")',
                       limit = 1000000,
                       verbose = FALSE
)

write.csv(oa_trials, file = "data/oa_trials.csv", row.names = FALSE)


trials = merge(x = opendata,
               y = data.frame("pmid" = oa_trials[, "pmid"]),
               by = "pmid")

trials = trials[!duplicated(trials$pmid), ]

write.csv(trials, "data/trials.csv", row.names = FALSE)

# Merging with opendata:
trials$is_trial = TRUE

opendata = merge(x = opendata,
                 y = trials[, c("pmid", "is_rct")],
                 by = "pmid",
                 all.x = TRUE)

opendata$is_trial = ifelse(is.na(opendata$is_trial), FALSE, TRUE)
opendata$is_review = ifelse(opendata$type == "research-article", FALSE, TRUE)


# Selecting only the necessary columns:
opendata = opendata %>% select(pmid,
                               pmcid,
                               doi,
                               title,
                               authorString,
                               journalTitle,
                               journalIssn,
                               publisher,
                               scimago_publisher,
                               firstPublicationDate,
                               journalVolume,
                               pageInfo,
                               issue,
                               type,
                               category,
                               is_research,
                               is_review,
                               is_trial,
                               citedByCount,
                               is_coi_pred,
                               coi_text,
                               is_fund_pred,
                               fund_text,
                               is_register_pred,
                               register_text,
                               is_open_data,
                               open_data_category,
                               open_data_statements,
                               is_open_code,
                               open_code_statements)

# Saving the final dataset:
write.csv(opendata, "data/medicaltransparency_opendata.csv", row.names = FALSE)
