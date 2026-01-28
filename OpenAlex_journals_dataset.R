library(parallel)
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(readr)
library(jsonlite)
library(stringr)
library(purrr)
library(bit64)
library(ggplot2)
library(ggforce)
library(ggridges)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(eulerr)
library(UpSetR)
library(cld3)
options(scipen = 999)


### MEGA JOURNALS DATAFRAME CONSTRUCTION
# OpenAlex upload and data mining
openalex <- list.files(path = "~/Desktop/OpenAlex_journals_dataset/OpenAlex", pattern = "all_journals_and_articles_.*", full.names = TRUE)
openalex <- rbindlist(lapply(openalex, fread, sep = ","), fill = TRUE)
openalex$journal_id <- as.numeric(as.character(openalex$journal_id))
openalex$article_id <- as.numeric(as.character(openalex$article_id))

openalex_journals <- subset(openalex, select = c(journal_id, journal_name, abbreviated_title, issn, issn_l, publisher, country_code, is_oa, homepage_url, works_count, cited_by_count, price, currency))
openalex_journals <- openalex_journals %>% distinct()
openalex_journals$APC_prices <- ifelse(is.na(openalex_journals$price) | openalex_journals$price == "" |
                                         is.na(openalex_journals$currency) | openalex_journals$currency == "", NA, 
                                       paste(openalex_journals$price, openalex_journals$currency))
openalex_journals <- openalex_journals %>% group_by(journal_id) %>%
                                           summarise(across(everything(), ~ first(.)),
                                                     APC_prices = paste(unique(APC_prices[!is.na(APC_prices) & APC_prices != ""]), collapse = "; ")) %>%
                                           ungroup()

openalex_articles <- subset(openalex, select = c(journal_id, article_id, year, title, topic_display_name, subfield, field, domain, primary_topic_display_name))
openalex_articles <- openalex_articles %>% distinct()

concat_unique <- function(x) {x <- unique(na.omit(x))
                              x <- x[x != ""]
                              if (length(x) > 0) paste(x, collapse = ";") else NA}
openalex_journals_topics <- openalex_articles %>% group_by(journal_id) %>%
                                                  summarise(topic_display_name = concat_unique(topic_display_name),
                                                            subfield = concat_unique(subfield),
                                                            field = concat_unique(field),
                                                            domain = concat_unique(domain),
                                                            primary_topic_display_name = concat_unique(primary_topic_display_name))

# replace the subfield, field and domain codes with the corresponding tags from openalex_topics
openalex_topics <- readxl::read_excel("~/Desktop/OpenAlex_journals_dataset/OAtopics.xlsx")
subfield_lookup <- openalex_topics %>% select(subfield_id, subfield_name) %>%
                                       distinct()
field_lookup <- openalex_topics %>% select(field_id, field_name) %>%
                                    distinct()
domain_lookup <- openalex_topics %>% select(domain_id, domain_name) %>%
                                     distinct()
replace_codes_with_tags <- function(code_column, lookup_table, code_name, tag_name) {sapply(code_column, function(codes) {
                                                                                     code_list <- strsplit(codes, ";")[[1]]
                                                                                     tags <- sapply(code_list, function(code) {
                                                                                       match_code <- lookup_table %>% filter(!!sym(code_name) == code)
                                                                                       if (nrow(match_code) > 0) match_code[[tag_name]] else NA})
                                                                                     paste(tags, collapse = ";")})}
openalex_journals_topics <- openalex_journals_topics %>% mutate(subfield = replace_codes_with_tags(subfield, subfield_lookup, "subfield_id", "subfield_name"),
                                                                field = replace_codes_with_tags(field, field_lookup, "field_id", "field_name"),
                                                                domain = replace_codes_with_tags(domain, domain_lookup, "domain_id", "domain_name"))

# incorporate all topics related variables to complete openalex_journals dataframe
openalex_journals <- openalex_journals %>% left_join(openalex_journals_topics %>%
                                           select(journal_id, topic_display_name, primary_topic_display_name, subfield, field, domain), 
                                           by = "journal_id")


# MJL upload and data mining
mjl_journals <- read.csv("~/Desktop/OpenAlex_journals_dataset/MJL.csv")
mjl_journals <- mjl_journals %>% distinct()
mjl_journals <- mjl_journals %>% group_by(journal_name, issn, eissn) %>%
                                 summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")


# JCR upload and data mining
jcr_journals <- list.files(path = "~/Desktop/OpenAlex_journals_dataset/JCR", pattern = "VictoriaDi.*JCR_JournalResults.*", full.names = TRUE)
jcr_journals <- rbindlist(lapply(jcr_journals, fread, sep = ","), fill = TRUE)
jcr_journals <- jcr_journals %>% group_by(`Journal name`, `JCR Abbreviation`, Publisher, ISSN, eISSN) %>%
                                 summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
jcr_journals <- jcr_journals %>% rename(journal_name = `Journal name`, issn = ISSN, eissn = eISSN)
jcr_journals[jcr_journals == "N/A"] <- NA
jcr_journals <- bind_rows(jcr_journals, read.csv("~/Desktop/OpenAlex_journals_dataset/JCR/JCR_missing_journals.csv", check.names = FALSE) %>%
                            mutate(across(everything(), as.character)))


# Scopus upload and data mining
scopus_journals <- readxl::read_excel("~/Desktop/OpenAlex_journals_dataset/Scopus.xlsx")
scopus_journals <- scopus_journals %>% group_by(journal_name, issn, eissn) %>%
                                       summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
scopus_journals[scopus_journals == "NA"] <- NA
scopus_journals$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_journals$issn)
scopus_journals$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_journals$eissn)


# DOAJ upload and data mining
doaj_journals <- read.csv("~/Desktop/OpenAlex_journals_dataset/DOAJ.csv")
doaj_journals$issn <- ifelse(!is.na(doaj_journals$issn) & doaj_journals$issn != "", 
                             str_pad(doaj_journals$issn, width = 8, side = "left", pad = "0"), 
                             doaj_journals$issn)
doaj_journals$eissn <- ifelse(!is.na(doaj_journals$eissn) & doaj_journals$eissn != "", 
                              str_pad(doaj_journals$eissn, width = 8, side = "left", pad = "0"), 
                              doaj_journals$eissn)
doaj_journals$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_journals$issn)
doaj_journals$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_journals$eissn)


# SJR upload and data mining
sjr_journals <- readxl::read_excel("~/Desktop/OpenAlex_journals_dataset/SJR.xlsx")
sjr_journals <- sjr_journals %>% separate(eissn, into = c("eissn", "issn"), sep = ", ", extra = "merge", fill = "right") %>%
                                 mutate(issn = ifelse(is.na(issn), NA, issn))
sjr_journals <- sjr_journals %>% group_by(journal_name, issn, eissn) %>%
                                 summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
sjr_journals$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", sjr_journals$issn)
sjr_journals$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", sjr_journals$eissn)


# CWTS upload
cwts_journals <- readxl::read_excel("~/Desktop/OpenAlex_journals_dataset/CWTS.xlsx")


# add unique identifiers to each dataframe
openalex_journals <- openalex_journals %>% mutate(OA_ID = paste0("OA", row_number())) %>%
                                           relocate(OA_ID)
mjl_journals <- mjl_journals %>% mutate(MJL_ID = paste0("MJL", row_number())) %>%
                                 relocate(MJL_ID)
jcr_journals <- jcr_journals %>% mutate(JCR_ID = paste0("JCR", row_number())) %>%
                                 relocate(JCR_ID)
scopus_journals <- scopus_journals %>% mutate(SCOP_ID = paste0("SCOP", row_number())) %>%
                                       relocate(SCOP_ID)
doaj_journals <- doaj_journals %>% mutate(DOAJ_ID = paste0("DOAJ", row_number())) %>%
                                   relocate(DOAJ_ID)
sjr_journals <- sjr_journals %>% mutate(SJR_ID = paste0("SJR", row_number())) %>%
                                 relocate(SJR_ID)
cwts_journals <- cwts_journals %>% mutate(CWTS_ID = paste0("CWTS", row_number())) %>%
                                   relocate(CWTS_ID)


# create variable to unify all ISSN codes per dataframe
openalex_journals$issn <- openalex_journals$issn %>% str_replace_all('\\[|\\]|"|\\s+', '') %>%
                                                     str_replace_all(',', ';')
openalex_journals$issn_codes <- apply(openalex_journals[, c("issn", "issn_l")], 1, function(x) {
                                      unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                      paste(unique_values, collapse = ";")})
mjl_journals$issn_codes <- apply(mjl_journals[, c("issn", "eissn")], 1, function(x) {
                                 unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                 paste(unique_values, collapse = ";")})
jcr_journals$issn_codes <- apply(jcr_journals[, c("issn", "eissn")], 1, function(x) {
                                 unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                 paste(unique_values, collapse = ";")})
scopus_journals$issn_codes <- apply(scopus_journals[, c("issn", "eissn")], 1, function(x) {
                                    unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                    paste(unique_values, collapse = ";")})
doaj_journals$issn_codes <- apply(doaj_journals[, c("issn", "eissn")], 1, function(x) {
                                  unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                  paste(unique_values, collapse = ";")})
sjr_journals$issn_codes <- apply(sjr_journals[, c("issn", "eissn")], 1, function(x) {
                                 unique_values <- unique(unlist(strsplit(na.omit(x), ";")))
                                 paste(unique_values, collapse = ";")})


# create variable to unify all journal names variants in Scopus
scopus_journals <- scopus_journals %>% rowwise() %>%
                                       mutate(journal_name_variants = paste(unique(unlist(strsplit(c_across(`Related Title 1`:`Other Related Title 4`)[!is.na(c_across(`Related Title 1`:`Other Related Title 4`))], ";"))), collapse = ";")) %>%
                                       ungroup()


# select, rename and organize all variables per dataframe
openalex_journals <- openalex_journals %>% select(OA_ID, OA_source_ID = journal_id, OA_ISSN_codes = issn_codes, OA_journal_name = journal_name, OA_journal_name_variants = abbreviated_title,
                                                  OA_publisher = publisher, OA_publisher_country = country_code, OA_topics = topic_display_name, OA_primary_topics = primary_topic_display_name,
                                                  OA_subfields = subfield, OA_fields = field, OA_domains = domain, OA_open_access = is_oa, OA_APC_prices = APC_prices, OA_website = homepage_url,
                                                  OA_total_articles = works_count, OA_total_citations = cited_by_count)

mjl_journals <- mjl_journals %>% select(MJL_ID, MJL_ISSN_codes = issn_codes, MJL_journal_name = journal_name, MJL_publisher = Publisher.name,
                                        MJL_publisher_country = Publisher.address, MJL_language = Languages, MJL_categories = Web.of.Science.Categories)

jcr_journals <- jcr_journals %>% select(JCR_ID, JCR_ISSN_codes = issn_codes, JCR_journal_name = journal_name, JCR_journal_name_variants = `JCR Abbreviation`,
                                        JCR_publisher = Publisher, JCR_categories = Category, JCR_edition = Edition, JCR_JCI_2023 = `2023 JCI`,
                                        JCR_JCI_rank = `JCI Rank`, JCR_JCI_quartile = `JCI Quartile`, JCR_JCI_percentile = `JCI Percentile`,
                                        JCR_JIF_2023 = `2023 JIF`, JCR_JIF_rank = `JIF Rank`, JCR_JIF_quartile = `JIF Quartile`, JCR_JIF_percentile = `JIF Percentile`,
                                        JCR_JIF_5_years = `5 Year JIF`, JCR_JIF_5_years_quartile = `5 Year JIF Quartile`, JCR_JIF_no_self_cites = `JIF Without Self Cites`,
                                        JCR_immediacy_index = `Immediacy Index`, JCR_AIS = `Article Influence Score`, JCR_AIS_quartile = `AIS Quartile`,
                                        JCR_eigenfactor = Eigenfactor, JCR_eigenfactor_normalized = `Normalized Eigenfactor`, JCR_citing_half_life = `Citing Half-Life`,
                                        JCR_cited_half_life = `Cited Half-Life`, JCR_percent_articles_citable_items = `% of Articles in Citable items`,
                                        JCR_percent_citable_open_access = `% of Citable OA`, JCR_total_articles = `Total Articles`, JCR_total_citations = `Total Citations`, JCR_citable_articles = `Citable Items`)

scopus_journals <- scopus_journals %>% select(SCOP_ID, SCOP_source_ID = `Sourcerecord ID`, SCOP_ISSN_codes = issn_codes, SCOP_journal_name = journal_name,
                                              SCOP_journal_name_variants = journal_name_variants, SCOP_publisher = Publisher, SCOP_main_publisher = `Publisher Imprints Grouped to Main Publisher`,
                                              SCOP_language = language, SCOP_ASJC_codes = `All Science Journal Classification Codes (ASJC)`, SCOP_coverage = Coverage, SCOP_open_access = `Open Access Status`,
                                              SCOP_discontinued = `Titles Discontinued by Scopus Due to Quality Issues`, SCOP_medline_sourced = `Medline-sourced Title? (See additional details under separate tab.)`)

doaj_journals <- doaj_journals %>% select(DOAJ_ID, DOAJ_ISSN_codes = issn_codes, DOAJ_continues_ISSN = Continues, DOAJ_continued_by_ISSN = Continued.By,
                                          DOAJ_journal_name = journal_name, DOAJ_journal_name_variants = Alternative.title, DOAJ_publisher = Publisher,
                                          DOAJ_publisher_country = Country.of.publisher, DOAJ_other_organization = Other.organisation, DOAJ_other_organization_country = Country.of.other.organisation,
                                          DOAJ_language = language, DOAJ_LCC_codes = LCC.Codes, DOAJ_subjects = Subjects, DOAJ_keywords = Keywords,
                                          DOAJ_open_access = Does.the.journal.comply.to.DOAJ.s.definition.of.open.access., DOAJ_open_license_since = When.did.the.journal.start.to.publish.all.content.using.an.open.license.,
                                          DOAJ_license = Journal.license, DOAJ_license_attributes = License.attributes, DOAJ_author_unrestricted_rights = Author.holds.copyright.without.restrictions,
                                          DOAJ_open_citations = Journal.complies.with.I4OC.standards.for.open.citations, DOAJ_machine_readable_license = Machine.readable.CC.licensing.information.embedded.or.displayed.in.articles,
                                          DOAJ_review_process = Review.process, DOAJ_average_weeks_for_publication = Average.number.of.weeks.between.article.submission.and.publication,
                                          DOAJ_APC_prices = APC.amount, DOAJ_other_fees = Has.other.fees, DOAJ_waiver_policy = Journal.waiver.policy..for.developing.country.authors.etc.,
                                          DOAJ_deposit_policy = Deposit.policy.directory, DOAJ_plagiarism_policy = Journal.plagiarism.screening.policy,
                                          DOAJ_persistent_identifiers = Persistent.article.identifiers, DOAJ_preservation_services = Preservation.Services,
                                          DOAJ_national_library_preservation_services = Preservation.Service..national.library, DOAJ_website = Journal.URL)

sjr_journals <- sjr_journals %>% select(SJR_ID, SJR_source_ID = Sourceid, SJR_ISSN_codes = issn_codes, SJR_journal_name = journal_name, SJR_publisher = Publisher,
                                        SJR_publisher_country = Country, SJR_region = Region, SJR_categories = Categories, SJR_areas = Areas, SJR_coverage = Coverage,
                                        SJR_SJR = SJR, SJR_rank = Rank, SJR_best_quartile = `SJR Best Quartile`, SJR_h_index = `H index`, SJR_total_articles_2023 = `Total Docs. (2023)`,
                                        SJR_total_articles_3_years = `Total Docs. (3years)`, SJR_total_references = `Total Refs.`, SJR_references_per_articles = `Ref. / Doc.`,
                                        SJR_total_citations_3_years = `Total Cites (3years)`, SJR_citations_per_articles_2_years = `Cites / Doc. (2years)`,
                                        SJR_citable_articles_3_years = `Citable Docs. (3years)`, SJR_percent_female = `%Female`, SJR_SDG = SDG, SJR_overton = Overton)

cwts_journals <- cwts_journals %>% select(CWTS_ID, CWTS_ISSN_codes = issn_codes, CWTS_journal_name = `Source title`, CWTS_percent_self_citations = `% self cit`,
                                          CWTS_SNIP = SNIP, CWTS_SNIP_lower_bound = `SNIP (lower bound)`, CWTS_SNIP_upper_bound = `SNIP (upper bound)`,
                                          CWTS_IPP = IPP, CWTS_IPP_lower_bound = `IPP (lower bound)`, CWTS_IPP_upper_bound = `IPP (upper bound)`)


# prepare dataframes for matching
openalex_journals_match <- subset(openalex_journals, select = c("OA_ID", "OA_ISSN_codes"))
openalex_journals_match <- openalex_journals_match %>% mutate(OA_ISSN_codes = strsplit(as.character(OA_ISSN_codes), ";")) %>%
                                                       unnest(OA_ISSN_codes) %>%
                                                       mutate(OA_ISSN_codes = gsub("\\s+", "", OA_ISSN_codes)) %>%
                                                       filter(OA_ISSN_codes != "") %>%
                                                       distinct(OA_ID, OA_ISSN_codes)

mjl_journals_match <- subset(mjl_journals, select = c("MJL_ID", "MJL_ISSN_codes"))
mjl_journals_match <- mjl_journals_match %>% mutate(MJL_ISSN_codes = strsplit(as.character(MJL_ISSN_codes), ";")) %>%
                                             unnest(MJL_ISSN_codes) %>%
                                             mutate(MJL_ISSN_codes = gsub("\\s+", "", MJL_ISSN_codes)) %>%
                                             filter(MJL_ISSN_codes != "") %>%
                                             distinct(MJL_ID, MJL_ISSN_codes)

jcr_journals_match <- subset(jcr_journals, select = c("JCR_ID", "JCR_ISSN_codes"))
jcr_journals_match <- jcr_journals_match %>% mutate(JCR_ISSN_codes = strsplit(as.character(JCR_ISSN_codes), ";")) %>%
                                             unnest(JCR_ISSN_codes) %>%
                                             mutate(JCR_ISSN_codes = gsub("\\s+", "", JCR_ISSN_codes)) %>%
                                             filter(JCR_ISSN_codes != "") %>%
                                             distinct(JCR_ID, JCR_ISSN_codes)

scopus_journals_match <- subset(scopus_journals, select = c("SCOP_ID", "SCOP_ISSN_codes"))
scopus_journals_match <- scopus_journals_match %>% mutate(SCOP_ISSN_codes = strsplit(as.character(SCOP_ISSN_codes), ";")) %>%
                                                   unnest(SCOP_ISSN_codes) %>%
                                                   mutate(SCOP_ISSN_codes = gsub("\\s+", "", SCOP_ISSN_codes)) %>%
                                                   filter(SCOP_ISSN_codes != "") %>%
                                                   distinct(SCOP_ID, SCOP_ISSN_codes)

doaj_journals_match <- subset(doaj_journals, select = c("DOAJ_ID", "DOAJ_ISSN_codes"))
doaj_journals_match <- doaj_journals_match %>% mutate(DOAJ_ISSN_codes = strsplit(as.character(DOAJ_ISSN_codes), ";")) %>%
                                               unnest(DOAJ_ISSN_codes) %>%
                                               mutate(DOAJ_ISSN_codes = gsub("\\s+", "", DOAJ_ISSN_codes)) %>%
                                               filter(DOAJ_ISSN_codes != "") %>%
                                               distinct(DOAJ_ID, DOAJ_ISSN_codes)

sjr_journals_match <- subset(sjr_journals, select = c("SJR_ID", "SJR_ISSN_codes"))
sjr_journals_match <- sjr_journals_match %>% mutate(SJR_ISSN_codes = strsplit(as.character(SJR_ISSN_codes), ";")) %>%
                                             unnest(SJR_ISSN_codes) %>%
                                             mutate(SJR_ISSN_codes = gsub("\\s+", "", SJR_ISSN_codes)) %>%
                                             filter(SJR_ISSN_codes != "") %>%
                                             distinct(SJR_ID, SJR_ISSN_codes)

cwts_journals_match <- subset(cwts_journals, select = c("CWTS_ID", "CWTS_ISSN_codes"))
cwts_journals_match <- cwts_journals_match %>% mutate(CWTS_ISSN_codes = strsplit(as.character(CWTS_ISSN_codes), ";")) %>%
                                               unnest(CWTS_ISSN_codes) %>%
                                               mutate(CWTS_ISSN_codes = gsub("\\s+", "", CWTS_ISSN_codes)) %>%
                                               filter(CWTS_ISSN_codes != "") %>%
                                               distinct(CWTS_ID, CWTS_ISSN_codes)


# match all dataframes by the journals' ISSN codes to OpenAlex
ddff_ISSNs_match <- openalex_journals_match %>% left_join(mjl_journals_match, by = c("OA_ISSN_codes" = "MJL_ISSN_codes"), relationship = "many-to-many") %>%
                                                left_join(jcr_journals_match, by = c("OA_ISSN_codes" = "JCR_ISSN_codes"), relationship = "many-to-many") %>%
                                                left_join(scopus_journals_match, by = c("OA_ISSN_codes" = "SCOP_ISSN_codes"), relationship = "many-to-many") %>%
                                                left_join(doaj_journals_match, by = c("OA_ISSN_codes" = "DOAJ_ISSN_codes"), relationship = "many-to-many") %>%
                                                left_join(sjr_journals_match, by = c("OA_ISSN_codes" = "SJR_ISSN_codes"), relationship = "many-to-many") %>%
                                                left_join(cwts_journals_match, by = c("OA_ISSN_codes" = "CWTS_ISSN_codes"), relationship = "many-to-many") %>%
                                                select(OA_ID, MJL_ID, JCR_ID, SCOP_ID, DOAJ_ID, SJR_ID, CWTS_ID, OA_ISSN_codes) %>%
                                                rename(ISSN_code = OA_ISSN_codes)


# remove ISSN code variable, duplicated rows, cases with only one ID, and group rows by the OpenAlex ID
ddff_ISSNs_match <- subset(ddff_ISSNs_match, select = c("OA_ID", "MJL_ID", "JCR_ID", "SCOP_ID", "DOAJ_ID", "SJR_ID", "CWTS_ID"))
ddff_ISSNs_match <- ddff_ISSNs_match %>% distinct()
ddff_ISSNs_match <- ddff_ISSNs_match[rowSums(!is.na(ddff_ISSNs_match)) > 1, ]
ddff_ISSNs_match <- ddff_ISSNs_match %>% group_by(OA_ID) %>%
                                         summarise(across(everything(), ~ unique(na.omit(.))[1]), .groups = "drop")


# store separately the rows where there's only one OpenAlex ID and incorporate the corresponding journals' titles
ddff_ISSNs_no_match <- ddff_ISSNs_match[rowSums(!is.na(ddff_ISSNs_match)) == 1, ]
ddff_ISSNs_no_match <- ddff_ISSNs_no_match %>% left_join(select(openalex_journals, OA_ID, OA_source_ID, OA_ISSN_codes, OA_journal_name, OA_journal_name_variants, OA_total_articles), by = "OA_ID")
ddff_ISSNs_no_match <- ddff_ISSNs_no_match %>% select(OA_ID, OA_source_ID, OA_ISSN_codes, OA_journal_name, OA_journal_name_variants, OA_total_articles)
write.csv(ddff_ISSNs_no_match, "~/Desktop/OpenAlex_journals_dataset/titles_matching/OA_titles_matching.csv")


# isolate the journals from the rest of the ddbb that don't match with OpenAlex through their ISSN codes in order to match via titles
mjl_journals_no_match <- mjl_journals %>% anti_join(ddff_ISSNs_match, by = "MJL_ID") %>%
                                          select(MJL_ID, MJL_ISSN_codes, MJL_journal_name)
#write.csv(mjl_journals_no_match, "~/Desktop/OpenAlex_journals_dataset/titles_matching/MJL_titles_matching.csv")

jcr_journals_no_match <- jcr_journals %>% anti_join(ddff_ISSNs_match, by = "JCR_ID") %>%
                                          select(JCR_ID, JCR_ISSN_codes, JCR_journal_name, JCR_journal_name_variants)
#write.csv(jcr_journals_no_match, "~/Desktop/OpenAlex_journals_dataset/titles_matching/JCR_titles_matching.csv")

scopus_journals_no_match <- scopus_journals %>% anti_join(ddff_ISSNs_match, by = "SCOP_ID") %>%
                                                select(SCOP_ID, SCOP_ISSN_codes, SCOP_journal_name, SCOP_journal_name_variants)
#write.csv(scopus_journals_no_match, "~/Desktop/OpenAlex_journals_dataset/titles_matching/SCOP_titles_matching.csv")

doaj_journals_no_match <- doaj_journals %>% anti_join(ddff_ISSNs_match, by = "DOAJ_ID") %>%
                                            select(DOAJ_ID, DOAJ_ISSN_codes, DOAJ_journal_name, DOAJ_journal_name_variants)
#write.csv(doaj_journals_no_match, "~/Desktop/OpenAlex_journals_dataset/titles_matching/DOAJ_titles_matching.csv")

sjr_journals_no_match <- sjr_journals %>% anti_join(ddff_ISSNs_match, by = "SJR_ID") %>%
                                          select(SJR_ID, SJR_ISSN_codes, SJR_journal_name)
#write.csv(sjr_journals_no_match, "~/Desktop/OpenAlex_journals_dataset/titles_matching/SJR_titles_matching.csv")

cwts_journals_no_match <- cwts_journals %>% anti_join(ddff_ISSNs_match, by = "CWTS_ID") %>%
                                            select(CWTS_ID, CWTS_ISSN_codes, CWTS_journal_name)
#write.csv(cwts_journals_no_match, "~/Desktop/OpenAlex_journals_dataset/titles_matching/CWTS_titles_matching.csv")


# prepare all journals_no_match dataframes to match each other, leaving OpenAlex out
mjl_journals_no_match <- mjl_journals_no_match %>% mutate(MJL_ISSN_codes = strsplit(as.character(MJL_ISSN_codes), ";")) %>%
                                                   unnest(MJL_ISSN_codes) %>%
                                                   mutate(MJL_ISSN_codes = gsub("\\s+", "", MJL_ISSN_codes)) %>%
                                                   filter(MJL_ISSN_codes != "") %>%
                                                   distinct(MJL_ID, MJL_ISSN_codes, MJL_journal_name)

jcr_journals_no_match <- jcr_journals_no_match %>% mutate(JCR_ISSN_codes = strsplit(as.character(JCR_ISSN_codes), ";")) %>%
                                                   unnest(JCR_ISSN_codes) %>%
                                                   mutate(JCR_ISSN_codes = gsub("\\s+", "", JCR_ISSN_codes)) %>%
                                                   filter(JCR_ISSN_codes != "") %>%
                                                   distinct(JCR_ID, JCR_ISSN_codes, JCR_journal_name)

scopus_journals_no_match <- scopus_journals_no_match %>% mutate(SCOP_ISSN_codes = strsplit(as.character(SCOP_ISSN_codes), ";")) %>%
                                                         unnest(SCOP_ISSN_codes) %>%
                                                         mutate(SCOP_ISSN_codes = gsub("\\s+", "", SCOP_ISSN_codes)) %>%
                                                         filter(SCOP_ISSN_codes != "") %>%
                                                         distinct(SCOP_ID, SCOP_ISSN_codes, SCOP_journal_name)

doaj_journals_no_match <- doaj_journals_no_match %>% mutate(DOAJ_ISSN_codes = strsplit(as.character(DOAJ_ISSN_codes), ";")) %>%
                                                     unnest(DOAJ_ISSN_codes) %>%
                                                     mutate(DOAJ_ISSN_codes = gsub("\\s+", "", DOAJ_ISSN_codes)) %>%
                                                     filter(DOAJ_ISSN_codes != "") %>%
                                                     distinct(DOAJ_ID, DOAJ_ISSN_codes, DOAJ_journal_name)

sjr_journals_no_match <- sjr_journals_no_match %>% mutate(SJR_ISSN_codes = strsplit(as.character(SJR_ISSN_codes), ";")) %>%
                                                   unnest(SJR_ISSN_codes) %>%
                                                   mutate(SJR_ISSN_codes = gsub("\\s+", "", SJR_ISSN_codes)) %>%
                                                   filter(SJR_ISSN_codes != "") %>%
                                                   distinct(SJR_ID, SJR_ISSN_codes, SJR_journal_name)

cwts_journals_no_match <- cwts_journals_no_match %>% mutate(CWTS_ISSN_codes = strsplit(as.character(CWTS_ISSN_codes), ";")) %>%
                                                     unnest(CWTS_ISSN_codes) %>%
                                                     mutate(CWTS_ISSN_codes = gsub("\\s+", "", CWTS_ISSN_codes)) %>%
                                                     filter(CWTS_ISSN_codes != "") %>%
                                                     distinct(CWTS_ID, CWTS_ISSN_codes, CWTS_journal_name)


# match all dataframes by the journals' ISSN codes to process their DOIs later
ddff_DOIs <- mjl_journals_no_match %>% full_join(jcr_journals_no_match, by = c("MJL_ISSN_codes" = "JCR_ISSN_codes"), relationship = "many-to-many") %>%
                                       full_join(scopus_journals_no_match, by = c("MJL_ISSN_codes" = "SCOP_ISSN_codes"), relationship = "many-to-many") %>%
                                       full_join(doaj_journals_no_match, by = c("MJL_ISSN_codes" = "DOAJ_ISSN_codes"), relationship = "many-to-many") %>%
                                       full_join(sjr_journals_no_match, by = c("MJL_ISSN_codes" = "SJR_ISSN_codes"), relationship = "many-to-many") %>%
                                       full_join(cwts_journals_no_match, by = c("MJL_ISSN_codes" = "CWTS_ISSN_codes"), relationship = "many-to-many") %>%
                                       rename(ISSN_code = MJL_ISSN_codes)


# remove ISSN code variable and duplicated rows
ddff_DOIs <- subset(ddff_DOIs, select = -ISSN_code)
ddff_DOIs <- ddff_DOIs %>% distinct()


# export the dataframes merge without OpenAlex, import them separately to process duplicated rows and merge again identifying their matching status
#write.csv(ddff_DOIs, "~/Desktop/OpenAlex_journals_dataset/DOIs_matching/ddff_DOIs.csv", row.names = FALSE)

ddff_DOIs_MJL <- read.csv("~/Desktop/OpenAlex_journals_dataset/DOIs_matching/ddff_DOIs_MJL.csv")
ddff_DOIs_MJL <- ddff_DOIs_MJL %>% group_by(MJL_ID) %>%
                                   summarise(across(everything(), ~ {non_na_vals <- na.omit(.)
                                   if (length(non_na_vals) > 0) first(non_na_vals) else NA})) %>%
                                   ungroup()

ddff_DOIs_JCR <- read.csv("~/Desktop/OpenAlex_journals_dataset/DOIs_matching/ddff_DOIs_JCR.csv")
ddff_DOIs_JCR <- ddff_DOIs_JCR %>% group_by(JCR_ID) %>%
                                   summarise(across(everything(), ~ {non_na_vals <- na.omit(.)
                                   if (length(non_na_vals) > 0) first(non_na_vals) else NA})) %>%
                                   ungroup()

ddff_DOIs_SCOP <- read.csv("~/Desktop/OpenAlex_journals_dataset/DOIs_matching/ddff_DOIs_SCOP.csv")
ddff_DOIs_SCOP <- ddff_DOIs_SCOP %>% group_by(SCOP_ID) %>%
                                     summarise(across(everything(), ~ {non_na_vals <- na.omit(.)
                                     if (length(non_na_vals) > 0) first(non_na_vals) else NA})) %>%
                                     ungroup()

ddff_DOIs_DOAJ <- read.csv("~/Desktop/OpenAlex_journals_dataset/DOIs_matching/ddff_DOIs_DOAJ.csv")
ddff_DOIs_DOAJ <- ddff_DOIs_DOAJ %>% group_by(DOAJ_ID) %>%
                                     summarise(across(everything(), ~ {non_na_vals <- na.omit(.)
                                     if (length(non_na_vals) > 0) first(non_na_vals) else NA})) %>%
                                     ungroup()

ddff_DOIs_SJR <- read.csv("~/Desktop/OpenAlex_journals_dataset/DOIs_matching/ddff_DOIs_SJR.csv")
ddff_DOIs_SJR <- ddff_DOIs_SJR %>% group_by(SJR_ID) %>%
                                   summarise(across(everything(), ~ {non_na_vals <- na.omit(.)
                                   if (length(non_na_vals) > 0) first(non_na_vals) else NA})) %>%
                                   ungroup()

ddff_DOIs_CWTS <- read.csv("~/Desktop/OpenAlex_journals_dataset/DOIs_matching/ddff_DOIs_CWTS.csv")
ddff_DOIs_CWTS <- ddff_DOIs_CWTS %>% group_by(CWTS_ID) %>%
                                     summarise(across(everything(), ~ {non_na_vals <- na.omit(.)
                                     if (length(non_na_vals) > 0) first(non_na_vals) else NA})) %>%
                                     ungroup()

ddff_DOIs <- bind_rows(ddff_DOIs_MJL, ddff_DOIs_JCR, ddff_DOIs_SCOP, ddff_DOIs_DOAJ, ddff_DOIs_SJR, ddff_DOIs_CWTS)

ddff_DOIs <- ddff_DOIs %>% select(MJL_ID, MJL_journal_name, JCR_ID, JCR_journal_name, SCOP_ID, SCOP_journal_name, DOAJ_ID,
                                  DOAJ_journal_name, SJR_ID, SJR_journal_name, CWTS_ID, CWTS_journal_name) %>%
                                  mutate(Match_Status = ifelse(rowSums(!is.na(select(., MJL_ID, JCR_ID, SCOP_ID, DOAJ_ID, SJR_ID, CWTS_ID))) > 1, "Matched", "Unmatched"))

ddff_DOIs <- ddff_DOIs %>% select(SCOP_ID, MJL_ID, JCR_ID, DOAJ_ID, SJR_ID, CWTS_ID) %>%
                           distinct(SCOP_ID, .keep_all = TRUE)
ddff_DOIs <- ddff_DOIs %>% slice(-c(1288, 5))


### MEGA MERGE
# by ISSNs
ddff_ISSNs_megamerge <- ddff_ISSNs_match %>% left_join(openalex_journals, by = "OA_ID") %>%
                                             left_join(mjl_journals, by = "MJL_ID") %>%
                                             left_join(jcr_journals, by = "JCR_ID") %>%
                                             left_join(scopus_journals, by = "SCOP_ID") %>%
                                             left_join(doaj_journals, by = "DOAJ_ID") %>%
                                             left_join(sjr_journals, by = "SJR_ID") %>%
                                             left_join(cwts_journals, by = "CWTS_ID")


# by titles
ddff_titles_match <- readxl::read_excel("~/Desktop/OpenAlex_journals_dataset/ddff_titles_match.xlsx")
ddff_titles_megamerge <- ddff_titles_match %>% left_join(openalex_journals, by = "OA_ID") %>%
                                               left_join(mjl_journals, by = "MJL_ID") %>%
                                               left_join(jcr_journals, by = "JCR_ID") %>%
                                               left_join(scopus_journals, by = "SCOP_ID") %>%
                                               left_join(doaj_journals, by = "DOAJ_ID") %>%
                                               left_join(sjr_journals, by = "SJR_ID") %>%
                                               left_join(cwts_journals, by = "CWTS_ID")


# by DOIs
ddff_DOIs_match <- read.csv("~/Desktop/OpenAlex_journals_dataset/ddff_DOIs_match.csv")
ddff_DOIs_match <- ddff_DOIs_match %>% mutate(SCOP_source_ID = as.character(SCOP_source_ID)) %>%
                                       left_join(scopus_journals %>% select(SCOP_source_ID, SCOP_ID),
                                                 by = "SCOP_source_ID")
ddff_DOIs_match <- ddff_DOIs_match %>% filter(!is.na(SCOP_ID))
ddff_DOIs_match <- ddff_DOIs_match %>% left_join(ddff_DOIs, by = "SCOP_ID")

ddff_DOIs_megamerge <- ddff_DOIs_match %>% left_join(openalex_journals, by = "OA_ID") %>%
                                           left_join(mjl_journals, by = "MJL_ID") %>%
                                           left_join(jcr_journals, by = "JCR_ID") %>%
                                           left_join(scopus_journals, by = "SCOP_ID") %>%
                                           left_join(doaj_journals, by = "DOAJ_ID") %>%
                                           left_join(sjr_journals, by = "SJR_ID") %>%
                                           left_join(cwts_journals, by = "CWTS_ID")


# merge of partial ISSNs, titles and DOIs merges in one final mega dataframe
ddff_megamerge <- bind_rows(ddff_ISSNs_megamerge, ddff_titles_megamerge, ddff_DOIs_megamerge)

# add remaining OpenAlex journals
openalex_journals_no_match <- openalex_journals %>% filter(!(OA_ID %in% ddff_megamerge$OA_ID))
ddff_megamerge <- bind_rows(ddff_megamerge, openalex_journals_no_match)

ddff_megamerge <- ddff_megamerge %>% select(-SCOP_source_ID.x, -SCOP_source_ID.y)
ddff_megamerge <- ddff_megamerge %>% mutate(across(where(is.character), ~ na_if(.x, "")))

# nest repeted variables between ddbb
ddff_megamerge <- ddff_megamerge %>% nest(other_IDs = c(MJL_ID, JCR_ID, SCOP_ID, DOAJ_ID, SJR_ID, CWTS_ID),
                                          other_source_IDs = c(SCOP_source_ID, SJR_source_ID),
                                          ISSN_codes = c(OA_ISSN_codes, MJL_ISSN_codes, JCR_ISSN_codes, SCOP_ISSN_codes, DOAJ_ISSN_codes, DOAJ_continues_ISSN, DOAJ_continued_by_ISSN, SJR_ISSN_codes, CWTS_ISSN_codes),
                                          journal_name = c(OA_journal_name, MJL_journal_name, JCR_journal_name, SCOP_journal_name, DOAJ_journal_name, SJR_journal_name, CWTS_journal_name),
                                          journal_name_variants = c(OA_journal_name_variants, JCR_journal_name_variants, SCOP_journal_name_variants, DOAJ_journal_name_variants),
                                          publisher = c(OA_publisher, MJL_publisher, JCR_publisher, SCOP_publisher, DOAJ_publisher, SJR_publisher),
                                          country = c(OA_publisher_country, MJL_publisher_country, DOAJ_publisher_country, SJR_publisher_country),
                                          language = c(MJL_language, SCOP_language, DOAJ_language),
                                          coverage = c(SCOP_coverage, SJR_coverage),
                                          open_access = c(OA_open_access, SCOP_open_access, DOAJ_open_access),
                                          APC_prices = c(OA_APC_prices, DOAJ_APC_prices),
                                          website = c(OA_website, DOAJ_website),
                                          total_articles = c(OA_total_articles, JCR_total_articles),
                                          total_citations = c(OA_total_citations, JCR_total_citations))

# reorder simple and nested variables
ddff_megamerge <- ddff_megamerge %>% select(OA_ID, other_IDs, OA_source_ID, other_source_IDs, ISSN_codes,
                                            journal_name, journal_name_variants, publisher, country,
                                            SCOP_main_publisher, DOAJ_other_organization, DOAJ_other_organization_country, SJR_region, language,
                                            OA_topics, OA_primary_topics, OA_subfields, OA_fields, OA_domains, MJL_categories, JCR_categories, SJR_categories, SJR_areas, SCOP_ASJC_codes, DOAJ_LCC_codes, DOAJ_subjects, DOAJ_keywords,
                                            JCR_edition, coverage, open_access, DOAJ_open_license_since, DOAJ_license, DOAJ_license_attributes, DOAJ_author_unrestricted_rights, DOAJ_open_citations, DOAJ_machine_readable_license,
                                            DOAJ_review_process, DOAJ_average_weeks_for_publication, APC_prices, DOAJ_other_fees, DOAJ_waiver_policy, DOAJ_deposit_policy, DOAJ_plagiarism_policy,
                                            DOAJ_persistent_identifiers, DOAJ_preservation_services, DOAJ_national_library_preservation_services, SCOP_medline_sourced, website,
                                            JCR_JCI_2023, JCR_JCI_rank, JCR_JCI_quartile, JCR_JCI_percentile, JCR_JIF_2023, JCR_JIF_rank, JCR_JIF_quartile, JCR_JIF_percentile, JCR_JIF_5_years, JCR_JIF_5_years_quartile, JCR_JIF_no_self_cites,
                                            JCR_immediacy_index, JCR_AIS, JCR_AIS_quartile, JCR_eigenfactor, JCR_eigenfactor_normalized, JCR_citing_half_life, JCR_cited_half_life, JCR_percent_articles_citable_items, JCR_percent_citable_open_access,
                                            SJR_SJR, SJR_rank, SJR_best_quartile, SJR_h_index,
                                            total_articles, SJR_total_articles_2023, SJR_total_articles_3_years, SJR_total_references, SJR_references_per_articles, total_citations, SJR_total_citations_3_years, SJR_citations_per_articles_2_years, JCR_citable_articles, SJR_citable_articles_3_years,
                                            CWTS_percent_self_citations, CWTS_SNIP, CWTS_SNIP_lower_bound, CWTS_SNIP_upper_bound, CWTS_IPP, CWTS_IPP_lower_bound, CWTS_IPP_upper_bound, SJR_percent_female, SJR_SDG, SJR_overton)


### LOCAL VARIABLES COMPUTATION
## REFERENCES
# read files and split into 20 dataframes for processing
references_files <- list.files(path = "~/Desktop/OpenAlex_journals_dataset/references_local_variable", pattern = "^local_research_OA2410_references_local_variable_\\d{12}$", full.names = TRUE)
num_parts <- 20  
num_files <- length(references_files)
chunk_size <- ceiling(num_files / num_parts)
for (i in 1:num_parts) {chunk_files <- references_files[((i - 1) * chunk_size + 1):min(i * chunk_size, num_files)]
                        chunk_files <- chunk_files[!is.na(chunk_files)]
                        chunk_data <- rbindlist(lapply(chunk_files, fread), fill = TRUE)
                        assign(paste0("references_part_", i), chunk_data, envir = .GlobalEnv)
                        rm(chunk_data)
                        gc()}

# compute the refs_count and refs_total variables recurrently per each dataframe partition (references_part_1 until references_part_20)
references_part_1 <- references_part_1 %>% group_by(journal_id, journal_name, country) %>%
                                           mutate(refs_count = n()) %>%
                                           ungroup()
references_part_1 <- within(references_part_1, rm(article_id, reference_id))
references_part_1 <- references_part_1 %>% distinct()
references_part_1 <- references_part_1 %>% group_by(journal_id, journal_name) %>%
                                           mutate(refs_total = sum(refs_count, na.rm = TRUE)) %>%
                                           ungroup()

# merge all parts together without loosing rows
local_variable_refs <- rbind(references_part_1, references_part_2, references_part_3, references_part_4, references_part_5, references_part_6,
                             references_part_7, references_part_8, references_part_9, references_part_10, references_part_11, references_part_12,
                             references_part_13, references_part_14, references_part_15, references_part_16, references_part_17, references_part_18,
                             references_part_19, references_part_20, fill = TRUE)

# compute variables refs_count, refs_total and refs_prop per unique combination of journal and its most referenced country
local_variable_refs <- local_variable_refs %>% group_by(journal_id, journal_name, country) %>%
                                                        summarise(refs_count = sum(refs_count, na.rm = TRUE), .groups = "drop")
local_variable_refs <- local_variable_refs %>% filter(!(journal_id == 1 & journal_name == "TRUE" & country == "TRUE" & refs_count == 1))
local_variable_refs <- local_variable_refs %>% group_by(journal_id, journal_name) %>%
                                                        mutate(refs_total = sum(refs_count, na.rm = TRUE)) %>%
                                                        ungroup()
local_variable_refs <- local_variable_refs %>% group_by(journal_id, journal_name) %>%
                                                        filter(refs_count == max(refs_count)) %>%
                                                        ungroup()
local_variable_refs <- local_variable_refs %>% mutate(refs_prop = round(refs_count / refs_total, 2))


## CITATIONS
local_variable_cits <- list.files(path = "~/Desktop/OpenAlex_journals_dataset/citations_local_variable", pattern = "^local_research_OA2410_citations_local_variable_\\d{12}$", full.names = TRUE)
local_variable_cits <- rbindlist(lapply(local_variable_cits, fread, sep = ","), fill = TRUE)

# compute variables cits_count, cits_total and cits_prop per unique combination of journal and its most citing country
local_variable_cits <- local_variable_cits %>% group_by(journal_id, journal_name, country) %>%
                                                        mutate(cits_count = n()) %>%
                                                        ungroup()
local_variable_cits <- within(local_variable_cits, rm(article_id, citing_work_id))
local_variable_cits <- local_variable_cits %>% distinct()
local_variable_cits <- local_variable_cits %>% group_by(journal_id, journal_name) %>%
                                                        mutate(cits_total = sum(cits_count, na.rm = TRUE)) %>%
                                                        ungroup()
local_variable_cits <- local_variable_cits %>% group_by(journal_id, journal_name) %>%
                                                        filter(cits_count == max(cits_count)) %>%
                                                        ungroup()
local_variable_cits <- local_variable_cits %>% mutate(cits_prop = round(cits_count / cits_total, 2))


## AUTHORS
local_variable_pubs <- list.files(path = "~/Desktop/OpenAlex_journals_dataset/publications_local_variable", pattern = "^local_research_OA2410_publications_local_variable_\\d{12}$", full.names = TRUE)
local_variable_pubs <- rbindlist(lapply(local_variable_pubs, fread, sep = ","), fill = TRUE)

# compute variables pubs_count, pubs_total and pubs_prop per unique combination of journal and its most publishing country
local_variable_pubs <- local_variable_pubs %>% group_by(journal_id, journal_name, country) %>%
                                               mutate(pubs_count = n()) %>%
                                               ungroup()
local_variable_pubs <- within(local_variable_pubs, rm(article_id))
local_variable_pubs <- local_variable_pubs %>% distinct()
local_variable_pubs <- local_variable_pubs %>% group_by(journal_id, journal_name) %>%
                                               mutate(pubs_total = sum(pubs_count, na.rm = TRUE)) %>%
                                               ungroup()
local_variable_pubs <- local_variable_pubs %>% group_by(journal_id, journal_name) %>%
                                               filter(pubs_count == max(pubs_count)) %>%
                                               ungroup()
local_variable_pubs <- local_variable_pubs %>% mutate(pubs_prop = round(pubs_count / pubs_total, 2))


## TOPONYMS
local_variable_tops <- list.files(path = "~/Desktop/OpenAlex_journals_dataset/toponyms_local_variable", pattern = "^local_research_OA2410_toponyms_local_variable_\\d{12}$", full.names = TRUE)
local_variable_tops <- rbindlist(lapply(local_variable_tops, fread, sep = ","), fill = TRUE)

local_variable_tops$language <- cld3::detect_language(local_variable_tops$title)
# 95% of languages in the titles:
## EN = english (3472125)
## ID = indonesian (144362)
## ES = spanish (107388)
## PT = portuguese (105655)
## DE = german (62374)
## FR = french (60272)
## AR = arabic (36210)
## UK = ukranian (31701)
## TR = turkish (30449)
## RU = russian (27421)

# obtain toponyms from the map.world dataframe in local.research.data.Rdata
name_en <- map.world$NAME_EN
writeLines(name_en, "~/Desktop/OpenAlex_journals_dataset/name_en.txt")
name_id <- map.world$NAME_ID
writeLines(name_id, "~/Desktop/OpenAlex_journals_dataset/name_id.txt")
name_es <- map.world$NAME_ES
writeLines(name_es, "~/Desktop/OpenAlex_journals_dataset/name_es.txt")
name_pt <- map.world$NAME_PT
writeLines(name_pt, "~/Desktop/OpenAlex_journals_dataset/name_pt.txt")
name_de <- map.world$NAME_DE
writeLines(name_de, "~/Desktop/OpenAlex_journals_dataset/name_de.txt")
name_fr <- map.world$NAME_FR
writeLines(name_fr, "~/Desktop/OpenAlex_journals_dataset/name_fr.txt")
name_ar <- map.world$NAME_AR
writeLines(name_ar, "~/Desktop/OpenAlex_journals_dataset/name_ar.txt")
name_uk <- map.world$NAME_UK
writeLines(name_uk, "~/Desktop/OpenAlex_journals_dataset/name_uk.txt")
name_tr <- map.world$NAME_TR
writeLines(name_tr, "~/Desktop/OpenAlex_journals_dataset/name_tr.txt")
name_ru <- map.world$NAME_RU
writeLines(name_ru, "~/Desktop/OpenAlex_journals_dataset/name_ru.txt")

# upload the already detected toponyms from BigQuery to compute tops_count, tops_total and tops_prop variables
local_variable_tops_2 <- list.files(path = "~/Desktop/OpenAlex_journals_dataset/toponyms_local_variable_2", pattern = "^local_research_OA2410_toponyms_local_variable_\\d{12}$", full.names = TRUE)
local_variable_tops_2 <- rbindlist(lapply(local_variable_tops_2, fread, sep = ","), fill = TRUE)

local_variable_tops_2 <- local_variable_tops_2 %>% group_by(journal_id, journal_name) %>%
                                                   mutate(tops_count = n_distinct(article_id[tops_detect == 1])) %>%
                                                   ungroup()
local_variable_tops_2 <- local_variable_tops_2 %>% group_by(journal_id, journal_name) %>%
                                                   mutate(tops_total = n_distinct(article_id)) %>%
                                                   ungroup()
local_variable_tops_2 <- local_variable_tops_2 %>% mutate(tops_prop = round(tops_count / tops_total, 2))
local_variable_tops_2 <- within(local_variable_tops_2, rm(article_id, title, tops_detect))
local_variable_tops_2 <- local_variable_tops_2 %>% distinct()


## LANGUAGES
# incorporate the OpenAlex language data to ddff_megamerge
local_variable_langs <- readr::read_csv("~/Desktop/OpenAlex_journals_dataset/languages_local_variable/local_research_OA2410_languages_local_variable")
local_variable_langs <- local_variable_langs %>% group_by(journal_id, journal_name) %>%
                                                 summarise(language = str_c(unique(language), collapse = "; ")) %>%
                                                 ungroup()

ddff_megamerge <- ddff_megamerge %>% left_join(local_variable_langs %>%
                                     mutate(journal_id = as.character(journal_id)) %>%
                                     select(journal_id, OA_language = language), by = c("OA_source_ID" = "journal_id")) %>%
                                     mutate(language = map2(language, OA_language, ~ {.x$OA_language <- .y
                                                                                      .x})) %>%
                                     select(-OA_language)

# look for expressions "ENG", "English" or "en" to create local binary variable langs, where 1 is global and 0 is local
ddff_megamerge <- ddff_megamerge %>% mutate(langs = map_int(language, ~ {clean_text <- function(x) {if (is.null(x) || all(is.na(x))) return(NA_character_)
                                                                         x <- unique(na.omit(str_trim(as.character(x))))
                                                                         if (length(x) == 0) return(NA_character_)
                                                                         paste(x, collapse = "; ")}
                                     mjl  <- clean_text(.x$MJL_language)
                                     scop <- clean_text(.x$SCOP_language)
                                     doaj <- clean_text(.x$DOAJ_language)
                                     oa   <- clean_text(.x$OA_language)
                                     is_english <- function(x) {!is.na(x) && str_detect(tolower(x), "\\benglish\\b|\\beng\\b|\\ben\\b")}
                                     is_empty <- function(x) is.na(x) || str_trim(x) == ""
                                     if (!is_empty(mjl) || !is_empty(scop)) {
                                       if (is_english(mjl) || is_english(scop)) 1L else 0L}
                                     else if (!is_empty(doaj)) {
                                       if (is_english(doaj)) 1L else 0L}
                                     else {if (is_english(oa)) 1L else 0L}}))


## DATABASES
# look for data within MJL_ID or SCOP_ID to create local binary variable mains, where 1 is global and 0 is local
ddff_megamerge <- ddff_megamerge %>% mutate(mains = map_int(other_IDs, ~ {mjl  <- .x$MJL_ID
                                                                          scop <- .x$SCOP_ID
                                                                          valid <- function(x) {!is.null(x) && !all(is.na(x)) && any(str_trim(as.character(x)) != "")}
                                                                          if (valid(mjl) || valid(scop)) 1L else 0L}))


# incorporate the local variables to ddff_megamerge
local_variable_refs$journal_id <- as.character(local_variable_refs$journal_id)
local_variable_cits$journal_id <- as.character(local_variable_cits$journal_id)
local_variable_pubs$journal_id <- as.character(local_variable_pubs$journal_id)
local_variable_tops_2$journal_id <- as.character(local_variable_tops_2$journal_id)
ddff_megamerge$OA_source_ID <- as.character(ddff_megamerge$OA_source_ID)

ddff_megamerge <- ddff_megamerge %>% left_join(local_variable_refs %>%
                                               select(journal_id, refs_prop, refs_country = country),
                                               by = c("OA_source_ID" = "journal_id")) %>%
                                     left_join(local_variable_cits %>%
                                               select(journal_id, cits_prop, cits_country = country),
                                               by = c("OA_source_ID" = "journal_id")) %>%
                                     left_join(local_variable_pubs %>%
                                               select(journal_id, pubs_prop, pubs_country = country),
                                               by = c("OA_source_ID" = "journal_id")) %>%
                                     left_join(local_variable_tops_2 %>%
                                               select(journal_id, tops_prop),
                                               by = c("OA_source_ID" = "journal_id"))


## LOCAL JOURNALS BY TERRITORY
# create a new variable local_territory considering the 3ºQ of variable tops_prop distribution (>= 0.14)
quantile(ddff_megamerge %>% distinct(OA_ID, .keep_all = TRUE) %>% pull(tops_prop), 0.75, na.rm = TRUE)
ddff_megamerge <- ddff_megamerge %>% group_by(OA_ID) %>%
                                     mutate(local_territory = if_else(tops_prop >= 0.14, "local", "global")) %>%
                                     ungroup()


## LOCAL JOURNALS BY PRODUCERS
# create a new variable local_producers considering the 3ºQ of variable refs_prop distribution (>= 0.43)
quantile(ddff_megamerge %>% distinct(OA_ID, .keep_all = TRUE) %>% pull(refs_prop), 0.75, na.rm = TRUE)
ddff_megamerge <- ddff_megamerge %>% group_by(OA_ID) %>%
                                     mutate(local_producers = if_else(refs_prop >= 0.43, "local", "global")) %>%
                                     ungroup()


## LOCAL JOURNALS BY RECIPIENTS
# create a new variable local_recipients considering the 3ºQ of variable cits_prop distribution (= 1)
quantile(ddff_megamerge %>% distinct(OA_ID, .keep_all = TRUE) %>% pull(cits_prop), 0.75, na.rm = TRUE)
ddff_megamerge <- ddff_megamerge %>% group_by(OA_ID) %>%
                                     mutate(local_recipients = if_else(cits_prop == 1, "local", "global")) %>%
                                     ungroup()


# save as CSV through JSON for sharing purposes
ddff_megamerge_flat <- ddff_megamerge %>% mutate(other_IDs = sapply(other_IDs, toJSON, auto_unbox = TRUE),
                                                 other_source_IDs = sapply(other_source_IDs, toJSON, auto_unbox = TRUE),
                                                 ISSN_codes = sapply(ISSN_codes, toJSON, auto_unbox = TRUE),
                                                 journal_name = sapply(journal_name, toJSON, auto_unbox = TRUE),
                                                 journal_name_variants = sapply(journal_name_variants, toJSON, auto_unbox = TRUE),
                                                 publisher = sapply(publisher, toJSON, auto_unbox = TRUE),
                                                 country = sapply(country, toJSON, auto_unbox = TRUE),
                                                 language = sapply(language, toJSON, auto_unbox = TRUE),
                                                 coverage = sapply(coverage, toJSON, auto_unbox = TRUE),
                                                 open_access = sapply(open_access, toJSON, auto_unbox = TRUE),
                                                 APC_prices = sapply(APC_prices, toJSON, auto_unbox = TRUE),
                                                 website = sapply(website, toJSON, auto_unbox = TRUE),
                                                 total_articles = sapply(total_articles, toJSON, auto_unbox = TRUE),
                                                 total_citations = sapply(total_citations, toJSON, auto_unbox = TRUE))
#write.csv(ddff_megamerge_flat, "~/Desktop/OpenAlex_journals_dataset/OpenAlex_dataset_merge.csv", row.names = FALSE)


### OVERALL RESULTS
## FIGURE 2
my_colors <- list(OpenAlex = "#3288BD", MJL = "#D53E4F", JCR = "#F46D43",
                  Scopus = "#FDAE61", SJR = "#FEE08B", CWTS = "#E6F598", DOAJ = "#66C2A5")

# function to compute the overlap areas and plot accordingly
draw_2set_venn <- function(set1_size, set2_size, intersection_size) {
                           r1 <- sqrt(set1_size / pi)
                           r2 <- sqrt(set2_size / pi)
                  overlap_area <- function(d, r1, r2) {
                                           if(d >= r1 + r2) return(0)
                                           if(d <= abs(r1 - r2)) return(pi * min(r1,r2)^2)
                                            acos((d^2 + r1^2 - r2^2)/(2*d*r1))*r1^2 +
                                            acos((d^2 + r2^2 - r1^2)/(2*d*r2))*r2^2 -
                                            0.5*sqrt((-d+r1+r2)*(d+r1-r2)*(d-r1+r2)*(d+r1+r2))}
                  d <- uniroot(function(d) overlap_area(d, r1, r2) - intersection_size,
                                        lower = 0, upper = r1 + r2)$root
                  circles <- data.frame(x = c(0, d),
                                        y = c(0, 0),
                                        r = c(r1, r2),
                                        set = factor(c("Set 1", "Set 2")))
                  p <- ggplot(circles) +
                    geom_circle(aes(x0 = x, y0 = y, r = r, fill = set), alpha = 0.7, color = "grey20") +
                    coord_fixed() +
                    theme_minimal() +
                    scale_fill_manual(values = c("Set 1" = my_colors[["OpenAlex"]], "Set 2" = my_colors[["DOAJ"]])) +
                    theme(legend.position = "none", axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
                  return(p)}

# set sizes
OA_size <- 75694
MJL_size <- 22455
JCR_size <- 21988
SCOP_size <- 29221
SJR_size <- 28174
CWTS_size <- 27879
DOAJ_size <- 20955

# OA intersections
OA_MJL_intersection <- 20419
OA_JCR_intersection <- 20038
OA_SCOP_intersection <- 24997
OA_SJR_intersection <- 23963
OA_CWTS_intersection <- 23759
OA_DOAJ_intersection <- 16242

# MJL intersections
MJL_JCR_intersection <- 19983 + 1208
MJL_SCOP_intersection <- 18553 + 1286
MJL_SJR_intersection <- 18302 + 1231
MJL_CWTS_intersection <- 18151 + 1194
MJL_DOAJ_intersection <- 5695 + 269
  
# JCR intersections
JCR_SCOP_intersection <- 18241 + 1231
JCR_SJR_intersection <- 18080 + 1198
JCR_CWTS_intersection <- 17933 + 1164
JCR_DOAJ_intersection <- 5469 + 259
  
# Scopus intersections
SCOP_SJR_intersection <- 23762 + 3756
SCOP_CWTS_intersection <- 23442 + 3609
SCOP_DOAJ_intersection <- 7395 + 835
  
# SJR intersections
SJR_CWTS_intersection <- 23638 + 3608
SJR_DOAJ_intersection <- 6721 + 727
  
# CWTS intersections
CWTS_DOAJ_intersection <- 6691 + 715

# final drawing of the Venn diagrams
figure2 <- draw_2set_venn(set1_size = OA_size,
                          set2_size = DOAJ_size,
                          intersection_size = OA_DOAJ_intersection)
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig2_OA_DOAJ.png", figure2, width = 6, height = 6, dpi = 300)


## TABLE 3
# count unique journals with MJL or JCR IDs in order to group under WOS. The same can be done for Scopus and its sources SCOP, SJR and CWTS
ddff_megamerge %>% mutate(MJL_ID = map_chr(other_IDs, ~ .x$MJL_ID %||% NA_character_),
                          JCR_ID  = map_chr(other_IDs, ~ .x$JCR_ID  %||% NA_character_)) %>%
                   mutate(any_ID = coalesce(MJL_ID, JCR_ID)) %>%
                   summarise(unique_journals_with_any_ID = n_distinct(na.omit(any_ID))) %>%
                   pull(unique_journals_with_any_ID)

# fractional count of unique WOS journals by knowledge field. The same can be done for Scopus, DOAJ and OpenAlex
ddff_megamerge %>% mutate(MJL_ID = map_chr(other_IDs, ~ {ids <- .x$MJL_ID
                                           if (is.null(ids) || all(is.na(ids))) NA_character_ else paste(unique(na.omit(as.character(ids))), collapse = ";")}),
                                           JCR_ID = map_chr(other_IDs, ~ {ids <- .x$JCR_ID
                                           if (is.null(ids) || all(is.na(ids))) NA_character_ else paste(unique(na.omit(as.character(ids))), collapse = ";")}),
                                   journal_key = coalesce(OA_ID, OA_source_ID)) %>%
                   filter((!is.na(MJL_ID) & MJL_ID != "") | (!is.na(JCR_ID)  & JCR_ID  != "")) %>%
                   filter(!is.na(OA_domains), OA_domains != "", OA_domains != "NA") %>%
                   mutate(OA_domains = strsplit(OA_domains, ";\\s*")) %>%
                   unnest(OA_domains) %>%
                   mutate(OA_domains = str_trim(OA_domains)) %>%
                   filter(!is.na(OA_domains), OA_domains != "", OA_domains != "NA") %>%
                   distinct(journal_key, OA_domains) %>%
                   group_by(journal_key) %>%
                   mutate(n_domains = n(), weight = 1 / n_domains) %>%
                   ungroup() %>%
                   group_by(OA_domains) %>%
                   summarise(fractional_count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
                   arrange(desc(fractional_count)) %>%
                   print(n = Inf)

# count unique WOS journals that are local according to local_territory, local_producers and local_recipients variables. The same can be done for Scopus, DOAJ and OpenAlex
ddff_megamerge %>% mutate(MJL_ID = map_chr(other_IDs, ~ .x$MJL_ID %||% NA_character_),
                          JCR_ID = map_chr(other_IDs, ~ .x$JCR_ID %||% NA_character_),
                          WOS_ID = coalesce(MJL_ID, JCR_ID)) %>%
                   filter(local_territory == "local") %>%
                   summarise(unique_local_WOS_journals = n_distinct(na.omit(WOS_ID))) %>%
                   pull(unique_local_WOS_journals)

# fractional count of unique WOS journals by knowledge field, according to local_territory, local_producers and local_recipients variables. The same can be done for Scopus, DOAJ and OpenAlex
ddff_megamerge %>% mutate(MJL_ID = map_chr(other_IDs, ~ {ids <- .x$MJL_ID
                                           if (is.null(ids) || all(is.na(ids))) NA_character_ else paste(unique(na.omit(as.character(ids))), collapse = ";")}),
                                           JCR_ID = map_chr(other_IDs, ~ {ids <- .x$JCR_ID
                                           if (is.null(ids) || all(is.na(ids))) NA_character_ else paste(unique(na.omit(as.character(ids))), collapse = ";")}),
                                   journal_key = coalesce(OA_ID, OA_source_ID)) %>%
                   filter(local_territory == "local", (!is.na(MJL_ID) & MJL_ID != "") | (!is.na(JCR_ID) & JCR_ID != "")) %>%
                   filter(!is.na(OA_domains), OA_domains != "", OA_domains != "NA") %>%
                   mutate(OA_domains = strsplit(OA_domains, ";\\s*")) %>%
                   unnest(OA_domains) %>%
                   mutate(OA_domains = str_trim(OA_domains)) %>%
                   filter(!is.na(OA_domains), OA_domains != "", OA_domains != "NA") %>%
                   distinct(journal_key, OA_domains) %>%
                   group_by(journal_key) %>%
                   mutate(n_domains = n(), weight = 1 / n_domains) %>%
                   ungroup() %>%
                   group_by(OA_domains) %>%
                   summarise(fractional_count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
                   arrange(desc(fractional_count)) %>%
                   print(n = Inf)

# count unique WOS journals in non-English language according to langs variable. The same can be done for Scopus, DOAJ and OpenAlex
ddff_megamerge %>% mutate(MJL_ID = map_chr(other_IDs, ~ .x$MJL_ID %||% NA_character_),
                          JCR_ID = map_chr(other_IDs, ~ .x$JCR_ID %||% NA_character_),
                          WOS_ID = coalesce(MJL_ID, JCR_ID)) %>%
                   filter(langs == 0) %>%
                   summarise(unique_local_WOS_journals = n_distinct(na.omit(WOS_ID))) %>%
                   pull(unique_local_WOS_journals)

# fractional count of unique WOS journals by knowledge field, according to langs variable. The same can be done for Scopus, DOAJ and OpenAlex
ddff_megamerge %>% mutate(MJL_ID = map_chr(other_IDs, ~ {ids <- .x$MJL_ID
                                           if (is.null(ids) || all(is.na(ids))) NA_character_ else paste(unique(na.omit(as.character(ids))), collapse = ";")}),
                                           JCR_ID = map_chr(other_IDs, ~ {ids <- .x$JCR_ID
                                           if (is.null(ids) || all(is.na(ids))) NA_character_ else paste(unique(na.omit(as.character(ids))), collapse = ";")}),
                                   journal_key = coalesce(OA_ID, OA_source_ID)) %>%
                   filter(langs == 0, (!is.na(MJL_ID) & MJL_ID != "") | (!is.na(JCR_ID) & JCR_ID != "")) %>%
                   filter(!is.na(OA_domains), OA_domains != "", OA_domains != "NA") %>%
                   mutate(OA_domains = strsplit(OA_domains, ";\\s*")) %>%
                   unnest(OA_domains) %>%
                   mutate(OA_domains = str_trim(OA_domains)) %>%
                   filter(!is.na(OA_domains), OA_domains != "", OA_domains != "NA") %>%
                   distinct(journal_key, OA_domains) %>%
                   group_by(journal_key) %>%
                   mutate(n_domains = n(), weight = 1 / n_domains) %>%
                   ungroup() %>%
                   group_by(OA_domains) %>%
                   summarise(fractional_count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
                   arrange(desc(fractional_count)) %>%
                   print(n = Inf)

# count unique WOS journals that are open access according to apen_access nested variable. The same can be done for Scopus, DOAJ and OpenAlex
ddff_megamerge %>% mutate(MJL_ID = map_chr(other_IDs, ~ .x$MJL_ID %||% NA_character_),
                          JCR_ID = map_chr(other_IDs, ~ .x$JCR_ID %||% NA_character_),
                          WOS_ID = coalesce(MJL_ID, JCR_ID)) %>%
                   filter(map_lgl(open_access, ~ {clean_val <- function(x) {
                     if (is.null(x) || all(is.na(x))) return(NA_character_)
                     x <- unique(na.omit(str_trim(as.character(x))))
                     if (length(x) == 0) return(NA_character_)
                     x[1]
                   }
                   scop <- clean_val(.x$SCOP_open_access)
                   doaj <- clean_val(.x$DOAJ_open_access)
                   oa   <- clean_val(.x$OA_open_access)
                   if (!is.na(scop) && scop == "Unpaywall Open Access") return(TRUE)
                   if (!is.na(doaj) && tolower(doaj) == "yes") return(TRUE)
                   if (!is.na(oa)   && oa == TRUE) return(TRUE)
                   FALSE})) %>%
                summarise(unique_OA_WOS_journals = n_distinct(na.omit(WOS_ID))) %>%
                pull(unique_OA_WOS_journals)

# fractional count of unique WOS journals by knowledge field, according to open_access nested variable. The same can be done for Scopus, DOAJ and OpenAlex
ddff_megamerge %>% mutate(MJL_ID = map_chr(other_IDs, ~ .x$MJL_ID %||% NA_character_),
                          JCR_ID = map_chr(other_IDs, ~ .x$JCR_ID %||% NA_character_),
                          WOS_ID = coalesce(MJL_ID, JCR_ID)) %>%
                   filter(!is.na(WOS_ID) & WOS_ID != "") %>%
                   filter(map_lgl(open_access, ~ {clean_val <- function(x) {
                     if (is.null(x) || all(is.na(x))) return(NA_character_)
                     x <- unique(na.omit(str_trim(as.character(x))))
                     if (length(x) == 0) return(NA_character_)
                     x[1]
                   }
                   scop <- clean_val(.x$SCOP_open_access)
                   doaj <- clean_val(.x$DOAJ_open_access)
                   oa   <- clean_val(.x$OA_open_access)
                   if (!is.na(scop) && scop == "Unpaywall Open Access") return(TRUE)
                   if (!is.na(doaj) && tolower(doaj) == "yes") return(TRUE)
                   if (!is.na(oa)   && oa == TRUE) return(TRUE)
                   FALSE})) %>%
                filter(!is.na(OA_domains), OA_domains != "", OA_domains != "NA") %>%
                mutate(OA_domains = strsplit(OA_domains, ";\\s*")) %>%
                unnest(OA_domains) %>%
                mutate(OA_domains = str_trim(OA_domains)) %>%
                filter(!is.na(OA_domains), OA_domains != "", OA_domains != "NA") %>%
                  distinct(WOS_ID, OA_domains) %>%
                  group_by(WOS_ID) %>%
                  mutate(n_domains = n(), weight = 1 / n_domains) %>%
                  ungroup() %>%
                    group_by(OA_domains) %>%
                    summarise(fractional_count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
                    arrange(desc(fractional_count)) %>%
                    print(n = Inf)


## TABLE 4
table4 <- ddff_megamerge %>% select(OA_ID, OA_domains, mains, local_territory, local_producers, local_recipients) %>%
                             distinct()

table4 <- table4 %>% mutate(OA_domains = strsplit(OA_domains, ";\\s*")) %>% 
                     unnest(OA_domains) %>%
                     filter(!is.na(OA_domains) & OA_domains != "", OA_domains != "NA")

table4 <- table4 %>% pivot_longer(cols = c(local_territory, local_producers, local_recipients),
                                  names_to = "conceptualization",
                                  values_to = "is_local") %>%
                     mutate(is_local = is_local == "local") %>%
                     group_by(conceptualization, OA_domains, mains) %>%
                     summarise(total_journals = n_distinct(OA_ID),
                               local_journals = sum(is_local, na.rm = TRUE),
                               percentage_local = (local_journals / total_journals) * 100,
                               .groups = "drop")


### TERRITORY RESULTS
local_journals_territory <- ddff_megamerge %>% filter(local_territory == "local")

## FIGURE 3
figure3 <- local_journals_territory %>% mutate(OA_domains = strsplit(OA_domains, ";")) %>%
                                        unnest(OA_domains) %>%
                                        mutate(OA_domains = trimws(OA_domains)) %>%
                                        filter(!is.na(OA_domains) & OA_domains != "") %>%
                                        mutate(is_OA = map_lgl(open_access, ~ {if (is.null(.x)) return(FALSE)
                                          .x$SCOP_open_access == "Unpaywall Open Access" |
                                            .x$DOAJ_open_access == "Yes" |
                                            .x$OA_open_access == TRUE}),
                                          is_non_english = langs == 0,
                                          is_overall = TRUE)

figure3 <- figure3 %>% select(OA_ID, OA_domains, mains, is_OA, is_non_english, is_overall) %>%
                       distinct() %>%
                       pivot_longer(cols = c(is_OA, is_non_english, is_overall), names_to = "group", values_to = "value") %>%
                       filter(value == TRUE) %>%
                       mutate(group = recode(group, "is_OA" = "Open Access", "is_non_english" = "Non-English", "is_overall" = "Overall"))

figure3 <- figure3 %>% filter(!OA_ID %in% c("OA18498", "OA38322", "OA39390"))

figure3 <- figure3 %>% group_by(group, OA_domains, mains) %>%
                       summarise(n = n(), .groups = "drop") %>%
                       group_by(group, OA_domains) %>%
                       mutate(perc = n / sum(n) * 100)

figure3 <- figure3 %>% mutate(mains_label = factor(mains, levels = c(0, 1), labels = c("Non-mainstream", "Mainstream")))
figure3 <- figure3 %>% mutate(OA_domains = factor(OA_domains, levels = c("Social Sciences", "Physical Sciences", "Life Sciences", "Health Sciences")))
figure3 <- figure3 %>% mutate(group = factor(group, levels = c("Overall", "Non-English", "Open Access")))

ggplot(figure3, aes(x = perc, y = OA_domains, fill = mains_label)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.6) +
  facet_wrap(~ group, ncol = 3) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(x = "% of local journals", y = "Field", fill = "Indexing status") +
  scale_fill_manual(values = c("Mainstream" = "#D53E4F", "Non-mainstream" = "#66C2A5")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 7),
        legend.position = "bottom", legend.direction = "horizontal", legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 7))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig3.png", width = 6, height = 3, dpi = 300)


## FIGURE 4A
openalex_articles_2023 <- list.files(path = "~/Desktop/OpenAlex_journals_dataset/publications_local_variable", pattern = "^local_research_OA2410_publications_local_variable_\\d{12}$", full.names = TRUE)
openalex_articles_2023 <- rbindlist(lapply(openalex_articles_2023, fread, sep = ",", colClasses = list(character = "journal_id")), fill = TRUE)

# count number of articles per country and journal
openalex_articles_2023 <- openalex_articles_2023 %>% group_by(journal_id, journal_name, country) %>%
                                                     summarise(article_count = n(), .groups = "drop")

# correct country names to macth those in world dataframe
openalex_articles_2023$country[openalex_articles_2023$country == "Bahamas"] <- "The Bahamas"
openalex_articles_2023$country[openalex_articles_2023$country == "Congo Republic"] <- "Republic of the Congo"
openalex_articles_2023$country[openalex_articles_2023$country == "Curacao"] <- "Curaçao"
openalex_articles_2023$country[openalex_articles_2023$country == "DR Congo"] <- "Democratic Republic of the Congo"
openalex_articles_2023$country[openalex_articles_2023$country == "Eswatini"] <- "eSwatini"
openalex_articles_2023$country[openalex_articles_2023$country == "French Guiana"] <- "NA"
openalex_articles_2023$country[openalex_articles_2023$country == "Gibraltar"] <- "NA"
openalex_articles_2023$country[openalex_articles_2023$country == "Guadeloupe"] <- "NA"
openalex_articles_2023$country[openalex_articles_2023$country == "Hong Kong"] <- "Hong Kong S.A.R."
openalex_articles_2023$country[openalex_articles_2023$country == "Macao"] <- "Macao S.A.R"
openalex_articles_2023$country[openalex_articles_2023$country == "Martinique"] <- "NA"
openalex_articles_2023$country[openalex_articles_2023$country == "Micronesia"] <- "Federated States of Micronesia"
openalex_articles_2023$country[openalex_articles_2023$country == "Palestinian Territory"] <- "Palestine"
openalex_articles_2023$country[openalex_articles_2023$country == "Réunion"] <- "NA"
openalex_articles_2023$country[openalex_articles_2023$country == "Sao Tome and Principe"] <- "São Tomé and Príncipe"
openalex_articles_2023$country[openalex_articles_2023$country == "Serbia"] <- "Republic of Serbia"
openalex_articles_2023$country[openalex_articles_2023$country == "St Kitts and Nevis"] <- "Saint Kitts and Nevis"
openalex_articles_2023$country[openalex_articles_2023$country == "Svalbard and Jan Mayen"] <- "NA"
openalex_articles_2023$country[openalex_articles_2023$country == "Tanzania"] <- "United Republic of Tanzania"
openalex_articles_2023$country[openalex_articles_2023$country == "The Netherlands"] <- "Netherlands"
openalex_articles_2023$country[openalex_articles_2023$country == "Timor-Leste"] <- "East Timor"
openalex_articles_2023$country[openalex_articles_2023$country == "Türkiye"] <- "Turkey"
openalex_articles_2023$country[openalex_articles_2023$country == "U.S. Virgin Islands"] <- "United States Virgin Islands"
openalex_articles_2023$country[openalex_articles_2023$country == "United States"] <- "United States of America"

# save as CSV for sharing purposes
setnames(openalex_articles_2023, "journal_id", "OA_source_ID")
#write.csv(openalex_articles_2023, "~/Desktop/OpenAlex_journals_dataset/OpenAlex_articles_2023.csv", row.names = FALSE)

# sum the number of articles per country to know their 2023 publications total within all OpenAlex journals
openalex_articles_2023_all <- openalex_articles_2023 %>% group_by(country) %>%
                                                         summarise(article_total = sum(article_count, na.rm = TRUE), .groups = "drop")

# sum the number of articles per country to know their 2023 publications total within local journals, and compute the proportion of articles in local journals with respect to all their 2023 publications
figure4A <- openalex_articles_2023 %>% filter(OA_source_ID %in% local_journals_territory$OA_source_ID) %>%
                                       group_by(country) %>%
                                       summarise(article_total = sum(article_count, na.rm = TRUE),
                                                 .groups = "drop")
figure4A <- figure4A %>% mutate(article_percent = 100 * article_total / openalex_articles_2023_all$article_total[match(country, openalex_articles_2023_all$country)])

# load world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# merge using full country names
figure4A <- world %>% left_join(figure4A, by = c("admin" = "country"))

# plot map
ggplot(figure4A) +
  geom_sf(aes(fill = article_percent), color = "grey", size = 0.1) +
  scale_fill_gradient(low = "#E6F598", high = "#3288BD", na.value = "grey80",
                      #limits = c(0, 86),
                      name = "% of articles in local journals") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.4, "cm"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig4A.png", width = 12, height = 6, dpi = 600)


## FIGURE 4B
# identify the local journals where each country has published during 2023, and compute the percentages of articles in mainstream and non-mainstream local journals
figure4B <- openalex_articles_2023 %>% mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                       inner_join(local_journals_territory %>%
                                                    mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                                    select(OA_source_ID, mains), by = "OA_source_ID") %>%
                                       group_by(country, mains) %>%
                                       summarise(n_articles = sum(article_count, na.rm = TRUE),
                                                 .groups = "drop") %>%
                                       group_by(country) %>%
                                       mutate(total_articles = sum(n_articles, na.rm = TRUE),
                                              percent_articles = 100 * n_articles / total_articles,
                                              mains_label = ifelse(mains == 1, "Mainstream local journals",
                                                                   "Non-mainstream local journals")) %>%
                                       ungroup()

# merge only mainstream cases using full country names
figure4B <- world %>% left_join(figure4B %>% filter(mains == 1), by = c("admin" = "country"))

# plot map
ggplot(figure4B) +
  geom_sf(aes(fill = percent_articles), color = "grey", size = 0.1) +
  scale_fill_gradient(low = "#E6F598", high = "#3288BD", na.value = "grey80",
                      #limits = c(0, 86),
                      name = "% of articles in mainstream local journals") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.4, "cm"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig4B.png", width = 12, height = 6, dpi = 600)


## FIGURE 5
# identify the local journals where each country has published during 2023, and compute the percentages of articles in non-English local journals
figure5A <- openalex_articles_2023 %>% mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                       inner_join(local_journals_territory %>%
                                                    mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                                    select(OA_source_ID, langs), by = "OA_source_ID") %>%
                                       group_by(country, langs) %>%
                                       summarise(n_articles = sum(article_count, na.rm = TRUE),
                                                 .groups = "drop") %>%
                                       group_by(country) %>%
                                       mutate(total_articles = sum(n_articles),
                                              percent_articles = 100 * n_articles / total_articles,
                                              langs_label = ifelse(langs == 1, "English local journals",
                                                                   "Non-English local journals")) %>%
                                       ungroup()

# merge only non-English cases using full country names
figure5A <- world %>% left_join(figure5A %>% filter(langs == 0), by = c("admin" = "country"))

# group countries at the region level
figure5A <- figure5A %>% mutate(region_custom = case_when(region_un %in% c("Africa", "Asia", "Europe", "Oceania") ~ region_un,
                                                          region_un == "Americas" & subregion == "Northern America" ~ "North America",
                                                          region_un == "Americas" & subregion %in% c("Central America", "Caribbean") ~ "Central America & the Caribbean",
                                                          region_un == "Americas" & subregion == "South America" ~ "South America",
                                                          TRUE ~ NA_character_))

# convert to long format
figure5A <- figure5A %>% select(name_long, region_custom, percent_articles, langs_label) %>%
                         rename(percent = percent_articles) %>%
                         mutate(metric = "Non-English")

# identify the local journals where each country has published during 2023, and compute the percentages of articles in open-access local journals
figure5B <- openalex_articles_2023 %>% mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                       inner_join(local_journals_territory %>%
                                       mutate(OA_source_ID = as.character(OA_source_ID),
                                              is_OA = map_lgl(open_access, ~ {if (is.null(.x)) return(FALSE)
                                                .x$SCOP_open_access == "Unpaywall Open Access" |
                                                  .x$DOAJ_open_access == "Yes" |
                                                  .x$OA_open_access == TRUE})) %>%
                                       select(OA_source_ID, is_OA), by = "OA_source_ID") %>%
                                       group_by(country, is_OA) %>%
                                       summarise(n_articles = sum(article_count, na.rm = TRUE),
                                                 .groups = "drop") %>%
                                       group_by(country) %>%
                                       mutate(total_articles = sum(n_articles),
                                              percent_articles = 100 * n_articles / total_articles,
                                              OA_label = ifelse(is_OA, "Open-access local journals",
                                                                "Closed-access local journals")) %>%
                                       ungroup()

# merge only open-access cases using full country names
figure5B <- world %>% left_join(figure5B %>% filter(is_OA == TRUE), by = c("admin" = "country"))

# group countries at the region level
figure5B <- figure5B %>% mutate(region_custom = case_when(region_un %in% c("Africa", "Asia", "Europe", "Oceania") ~ region_un,
                                                          region_un == "Americas" & subregion == "Northern America" ~ "North America",
                                                          region_un == "Americas" & subregion %in% c("Central America", "Caribbean") ~ "Central America & the Caribbean",
                                                          region_un == "Americas" & subregion == "South America" ~ "South America",
                                                          TRUE ~ NA_character_))

# convert to long format
figure5B <- figure5B %>% select(name_long, region_custom, percent_articles, OA_label) %>%
                         rename(percent = percent_articles) %>%
                         mutate(metric = "Open Access")

# combine both figure5 A & B into a single dataframe for plotting purposes
figure5 <- bind_rows(figure5A %>% rename(label = langs_label),
                     figure5B %>% rename(label = OA_label))

figure5 <- figure5 %>% mutate(region_custom = case_when(name_long == "Mexico" ~ "North America", TRUE ~ region_custom))
figure5$region_custom <- factor(figure5$region_custom, levels = c("South America", "Central America & the Caribbean", "North America", "Europe", "Africa", "Asia", "Oceania"))

# plot boxplot
figure5 %>% filter(!is.na(percent), !is.na(region_custom)) %>%
  ggplot(aes(y = region_custom, x = percent, fill = region_custom)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(height = 0.15, size = 1.2, alpha = 0.6, color = "grey50") +
  facet_wrap(~ metric, ncol = 2) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(y = "Region",
       x = "% of articles in mainstream local journals") +
  scale_y_discrete(labels = c("South America", "Central America\n& the Caribbean", "North America",
                              "Europe", "Africa", "Asia", "Oceania")) +
  scale_fill_manual(values = c("South America" = "#FEE08B", "Central America & the Caribbean" = "#FDAE61", "North America" = "#F46D43",
                               "Europe" = "#D53E4F", "Africa" = "#3288BD", "Asia" = "#66C2A5", "Oceania" = "#E6F598")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.position = "none",
        strip.text = element_text(size = 17, face = "bold"))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig5.png", width = 9, height = 6, dpi = 600)


### PRODUCERS RESULTS
local_journals_producers <- ddff_megamerge %>% filter(local_producers == "local")

## FIGURE 6
figure6 <- local_journals_producers %>% mutate(OA_domains = strsplit(OA_domains, ";")) %>%
                                        unnest(OA_domains) %>%
                                        mutate(OA_domains = trimws(OA_domains)) %>%
                                        filter(!is.na(OA_domains) & OA_domains != "") %>%
                                        mutate(is_OA = map_lgl(open_access, ~ {if (is.null(.x)) return(FALSE)
                                          .x$SCOP_open_access == "Unpaywall Open Access" |
                                            .x$DOAJ_open_access == "Yes" |
                                            .x$OA_open_access == TRUE}),
                                          is_non_english = langs == 0,
                                          is_overall = TRUE)

figure6 <- figure6 %>% select(OA_ID, OA_domains, mains, is_OA, is_non_english, is_overall) %>%
                       distinct() %>%
                       pivot_longer(cols = c(is_OA, is_non_english, is_overall), names_to = "group", values_to = "value") %>%
                       filter(value == TRUE) %>%
                       mutate(group = recode(group, "is_OA" = "Open Access", "is_non_english" = "Non-English", "is_overall" = "Overall"))

figure6 <- figure6 %>% filter(!is.na(OA_domains) & OA_domains != "" & OA_domains != "NA")

figure6 <- figure6 %>% group_by(group, OA_domains, mains) %>%
                       summarise(n = n(), .groups = "drop") %>%
                       group_by(group, OA_domains) %>%
                       mutate(perc = n / sum(n) * 100)

figure6 <- figure6 %>% mutate(mains_label = factor(mains, levels = c(0, 1), labels = c("Non-mainstream", "Mainstream")))
figure6 <- figure6 %>% mutate(OA_domains = factor(OA_domains, levels = c("Social Sciences", "Physical Sciences", "Life Sciences", "Health Sciences")))
figure6 <- figure6 %>% mutate(group = factor(group, levels = c("Overall", "Non-English", "Open Access")))

ggplot(figure6, aes(x = perc, y = OA_domains, fill = mains_label)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.6) +
  facet_wrap(~ group, ncol = 3) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(x = "% of local journals", y = "Field", fill = "Indexing status") +
  scale_fill_manual(values = c("Mainstream" = "#D53E4F", "Non-mainstream" = "#66C2A5")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 7),
        legend.position = "bottom", legend.direction = "horizontal", legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 7))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig6.png", width = 6, height = 3, dpi = 300)


## FIGURE 7A
# sum the number of articles per country to know their 2023 publications total within local journals, and compute the proportion of articles in local journals with respect to all their 2023 publications
figure7A <- openalex_articles_2023 %>% filter(OA_source_ID %in% local_journals_producers$OA_source_ID) %>%
                                       group_by(country) %>%
                                       summarise(article_total = sum(article_count, na.rm = TRUE),
                                       .groups = "drop")
figure7A <- figure7A %>% mutate(article_percent = 100 * article_total / openalex_articles_2023_all$article_total[match(country, openalex_articles_2023_all$country)])

# load world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# merge using full country names
figure7A <- world %>% left_join(figure7A, by = c("admin" = "country"))

# plot map
ggplot(figure7A) +
  geom_sf(aes(fill = article_percent), color = "grey", size = 0.1) +
  scale_fill_gradient(low = "#E6F598", high = "#3288BD", na.value = "grey80",
                      #limits = c(0, 86),
                      name = "% of articles in local journals") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.4, "cm"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig7A.png", width = 12, height = 6, dpi = 600)


## FIGURE 7B
# identify the local journals where each country has published during 2023, and compute the percentages of articles in mainstream and non-mainstream local journals
figure7B <- openalex_articles_2023 %>% mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                       inner_join(local_journals_producers %>%
                                       mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                       select(OA_source_ID, mains), by = "OA_source_ID") %>%
                                       group_by(country, mains) %>%
                                       summarise(n_articles = sum(article_count, na.rm = TRUE),
                                                 .groups = "drop") %>%
                                       group_by(country) %>%
                                       mutate(total_articles = sum(n_articles, na.rm = TRUE),
                                              percent_articles = 100 * n_articles / total_articles,
                                              mains_label = ifelse(mains == 1, "Mainstream local journals",
                                                                   "Non-mainstream local journals")) %>%
                                       ungroup()

# merge only mainstream cases using full country names
figure7B <- world %>% left_join(figure7B %>% filter(mains == 1), by = c("admin" = "country"))

# plot map
ggplot(figure7B) +
  geom_sf(aes(fill = percent_articles), color = "grey", size = 0.1) +
  scale_fill_gradient(low = "#E6F598", high = "#3288BD", na.value = "grey80",
                      #limits = c(0, 86),
                      name = "% of articles in mainstream local journals") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.4, "cm"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig7B.png", width = 12, height = 6, dpi = 600)


## FIGURE 8
# identify the local journals where each country has published during 2023, and compute the percentages of articles in non-English local journals
figure8A <- openalex_articles_2023 %>% mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                       inner_join(local_journals_producers %>%
                                       mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                       select(OA_source_ID, langs), by = "OA_source_ID") %>%
                                       group_by(country, langs) %>%
                                       summarise(n_articles = sum(article_count, na.rm = TRUE),
                                                 .groups = "drop") %>%
                                       group_by(country) %>%
                                       mutate(total_articles = sum(n_articles),
                                              percent_articles = 100 * n_articles / total_articles,
                                              langs_label = ifelse(langs == 1, "English local journals",
                                                                   "Non-English local journals")) %>%
                                       ungroup()

# merge only non-English cases using full country names
figure8A <- world %>% left_join(figure8A %>% filter(langs == 0), by = c("admin" = "country"))

# group countries at the region level
figure8A <- figure8A %>% mutate(region_custom = case_when(region_un %in% c("Africa", "Asia", "Europe", "Oceania") ~ region_un,
                                                          region_un == "Americas" & subregion == "Northern America" ~ "North America",
                                                          region_un == "Americas" & subregion %in% c("Central America", "Caribbean") ~ "Central America & the Caribbean",
                                                          region_un == "Americas" & subregion == "South America" ~ "South America",
                                                          TRUE ~ NA_character_))

# convert to long format
figure8A <- figure8A %>% select(name_long, region_custom, percent_articles, langs_label) %>%
                                rename(percent = percent_articles) %>%
                                mutate(metric = "Non-English")

# identify the local journals where each country has published during 2023, and compute the percentages of articles in open-access local journals
figure8B <- openalex_articles_2023 %>% mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                       inner_join(local_journals_producers %>%
                                       mutate(OA_source_ID = as.character(OA_source_ID),
                                              is_OA = map_lgl(open_access, ~ {if (is.null(.x)) return(FALSE)
                                                .x$SCOP_open_access == "Unpaywall Open Access" |
                                                  .x$DOAJ_open_access == "Yes" |
                                                  .x$OA_open_access == TRUE})) %>%
                                       select(OA_source_ID, is_OA), by = "OA_source_ID") %>%
                                       group_by(country, is_OA) %>%
                                       summarise(n_articles = sum(article_count, na.rm = TRUE),
                                                 .groups = "drop") %>%
                                       group_by(country) %>%
                                       mutate(total_articles = sum(n_articles),
                                              percent_articles = 100 * n_articles / total_articles,
                                              OA_label = ifelse(is_OA, "Open-access local journals",
                                                                "Closed-access local journals")) %>%
                                       ungroup()

# merge only open-access cases using full country names
figure8B <- world %>% left_join(figure8B %>% filter(is_OA == TRUE), by = c("admin" = "country"))

# group countries at the region level
figure8B <- figure8B %>% mutate(region_custom = case_when(region_un %in% c("Africa", "Asia", "Europe", "Oceania") ~ region_un,
                                                          region_un == "Americas" & subregion == "Northern America" ~ "North America",
                                                          region_un == "Americas" & subregion %in% c("Central America", "Caribbean") ~ "Central America & the Caribbean",
                                                          region_un == "Americas" & subregion == "South America" ~ "South America",
                                                          TRUE ~ NA_character_))

# convert to long format
figure8B <- figure8B %>% select(name_long, region_custom, percent_articles, OA_label) %>%
                         rename(percent = percent_articles) %>%
                         mutate(metric = "Open Access")

# combine both figure8 A & B into a single dataframe for plotting purposes
figure8 <- bind_rows(figure8A %>% rename(label = langs_label),
                     figure8B %>% rename(label = OA_label))

figure8 <- figure8 %>% mutate(region_custom = case_when(name_long == "Mexico" ~ "North America", TRUE ~ region_custom))
figure8$region_custom <- factor(figure8$region_custom, levels = c("South America", "Central America & the Caribbean", "North America", "Europe", "Africa", "Asia", "Oceania"))

# plot boxplot
figure8 %>% filter(!is.na(percent), !is.na(region_custom)) %>%
  ggplot(aes(y = region_custom, x = percent, fill = region_custom)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(height = 0.15, size = 1.2, alpha = 0.6, color = "grey50") +
  facet_wrap(~ metric, ncol = 2) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(y = "Region",
       x = "% of articles in mainstream local journals") +
  scale_y_discrete(labels = c("South America", "Central America\n& the Caribbean", "North America",
                              "Europe", "Africa", "Asia", "Oceania")) +
  scale_fill_manual(values = c("South America" = "#FEE08B", "Central America & the Caribbean" = "#FDAE61", "North America" = "#F46D43",
                               "Europe" = "#D53E4F", "Africa" = "#3288BD", "Asia" = "#66C2A5", "Oceania" = "#E6F598")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.position = "none",
        strip.text = element_text(size = 17, face = "bold"))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig8.png", width = 9, height = 6, dpi = 600)


### RECIPIENTS RESULTS
local_journals_recipients <- ddff_megamerge %>% filter(local_recipients == "local")

## FIGURE 9
figure9 <- local_journals_recipients %>% mutate(OA_domains = strsplit(OA_domains, ";")) %>%
                                         unnest(OA_domains) %>%
                                         mutate(OA_domains = trimws(OA_domains)) %>%
                                         filter(!is.na(OA_domains) & OA_domains != "") %>%
                                         mutate(is_OA = map_lgl(open_access, ~ {if (is.null(.x)) return(FALSE)
                                           .x$SCOP_open_access == "Unpaywall Open Access" |
                                             .x$DOAJ_open_access == "Yes" |
                                             .x$OA_open_access == TRUE}),
                                           is_non_english = langs == 0,
                                           is_overall = TRUE)

figure9 <- figure9 %>% select(OA_ID, OA_domains, mains, is_OA, is_non_english, is_overall) %>%
                       distinct() %>%
                       pivot_longer(cols = c(is_OA, is_non_english, is_overall), names_to = "group", values_to = "value") %>%
                       filter(value == TRUE) %>%
                       mutate(group = recode(group, "is_OA" = "Open Access", "is_non_english" = "Non-English", "is_overall" = "Overall"))

figure9 <- figure9 %>% filter(!is.na(OA_domains) & OA_domains != "" & OA_domains != "NA")

figure9 <- figure9 %>% group_by(group, OA_domains, mains) %>%
                       summarise(n = n(), .groups = "drop") %>%
                       group_by(group, OA_domains) %>%
                       mutate(perc = n / sum(n) * 100)

figure9 <- figure9 %>% mutate(mains_label = factor(mains, levels = c(0, 1), labels = c("Non-mainstream", "Mainstream")))
figure9 <- figure9 %>% mutate(OA_domains = factor(OA_domains, levels = c("Social Sciences", "Physical Sciences", "Life Sciences", "Health Sciences")))
figure9 <- figure9 %>% mutate(group = factor(group, levels = c("Overall", "Non-English", "Open Access")))

ggplot(figure9, aes(x = perc, y = OA_domains, fill = mains_label)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.6) +
  facet_wrap(~ group, ncol = 3) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(x = "% of local journals", y = "Field", fill = "Indexing status") +
  scale_fill_manual(values = c("Mainstream" = "#D53E4F", "Non-mainstream" = "#66C2A5")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 7),
        legend.position = "bottom", legend.direction = "horizontal", legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 7))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig9.png", width = 6, height = 3, dpi = 300)


## FIGURE 10A
# sum the number of articles per country to know their 2023 publications total within local journals, and compute the proportion of articles in local journals with respect to all their 2023 publications
figure10A <- openalex_articles_2023 %>% filter(OA_source_ID %in% local_journals_recipients$OA_source_ID) %>%
                                        group_by(country) %>%
                                        summarise(article_total = sum(article_count, na.rm = TRUE),
                                                  .groups = "drop")
figure10A <- figure10A %>% mutate(article_percent = 100 * article_total / openalex_articles_2023_all$article_total[match(country, openalex_articles_2023_all$country)])

# load world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# merge using full country names
figure10A <- world %>% left_join(figure10A, by = c("admin" = "country"))

# plot map
ggplot(figure10A) +
  geom_sf(aes(fill = article_percent), color = "grey", size = 0.1) +
  scale_fill_gradient(low = "#E6F598", high = "#3288BD", na.value = "grey80",
                      #limits = c(0, 86),
                      name = "% of articles in local journals") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.4, "cm"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig10A.png", width = 12, height = 6, dpi = 600)


## FIGURE 10B
# identify the local journals where each country has published during 2023, and compute the percentages of articles in mainstream and non-mainstream local journals
figure10B <- openalex_articles_2023 %>% mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                        inner_join(local_journals_recipients %>%
                                        mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                        select(OA_source_ID, mains), by = "OA_source_ID") %>%
                                        group_by(country, mains) %>%
                                        summarise(n_articles = sum(article_count, na.rm = TRUE),
                                                  .groups = "drop") %>%
                                        group_by(country) %>%
                                        mutate(total_articles = sum(n_articles, na.rm = TRUE),
                                               percent_articles = 100 * n_articles / total_articles,
                                               mains_label = ifelse(mains == 1, "Mainstream local journals",
                                                                    "Non-mainstream local journals")) %>%
                                        ungroup()

# merge only mainstream cases using full country names
figure10B <- world %>% left_join(figure10B %>% filter(mains == 1), by = c("admin" = "country"))

# plot map
ggplot(figure10B) +
  geom_sf(aes(fill = percent_articles), color = "grey", size = 0.1) +
  scale_fill_gradient(low = "#E6F598", high = "#3288BD", na.value = "grey80",
                      #limits = c(0, 86),
                      name = "% of articles in mainstream local journals") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.4, "cm"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig10B.png", width = 12, height = 6, dpi = 600)


# FIGURE 11
# identify the local journals where each country has published during 2023, and compute the percentages of articles in non-English local journals
figure11A <- openalex_articles_2023 %>% mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                        inner_join(local_journals_recipients %>%
                                        mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                        select(OA_source_ID, langs), by = "OA_source_ID") %>%
                                        group_by(country, langs) %>%
                                        summarise(n_articles = sum(article_count, na.rm = TRUE),
                                                  .groups = "drop") %>%
                                        group_by(country) %>%
                                        mutate(total_articles = sum(n_articles),
                                               percent_articles = 100 * n_articles / total_articles,
                                               langs_label = ifelse(langs == 1, "English local journals",
                                                                    "Non-English local journals")) %>%
                                        ungroup()

# merge only non-English cases using full country names
figure11A <- world %>% left_join(figure11A %>% filter(langs == 0), by = c("admin" = "country"))

# group countries at the region level
figure11A <- figure11A %>% mutate(region_custom = case_when(region_un %in% c("Africa", "Asia", "Europe", "Oceania") ~ region_un,
                                                          region_un == "Americas" & subregion == "Northern America" ~ "North America",
                                                          region_un == "Americas" & subregion %in% c("Central America", "Caribbean") ~ "Central America & the Caribbean",
                                                          region_un == "Americas" & subregion == "South America" ~ "South America",
                                                          TRUE ~ NA_character_))

# convert to long format
figure11A <- figure11A %>% select(name_long, region_custom, percent_articles, langs_label) %>%
                           rename(percent = percent_articles) %>%
                           mutate(metric = "Non-English")

# identify the local journals where each country has published during 2023, and compute the percentages of articles in open-access local journals
figure11B <- openalex_articles_2023 %>% mutate(OA_source_ID = as.character(OA_source_ID)) %>%
                                        inner_join(local_journals_recipients %>%
                                        mutate(OA_source_ID = as.character(OA_source_ID),
                                               is_OA = map_lgl(open_access, ~ {if (is.null(.x)) return(FALSE)
                                                 .x$SCOP_open_access == "Unpaywall Open Access" |
                                                   .x$DOAJ_open_access == "Yes" |
                                                   .x$OA_open_access == TRUE})) %>%
                                        select(OA_source_ID, is_OA), by = "OA_source_ID") %>%
                                        group_by(country, is_OA) %>%
                                        summarise(n_articles = sum(article_count, na.rm = TRUE),
                                                  .groups = "drop") %>%
                                        group_by(country) %>%
                                        mutate(total_articles = sum(n_articles),
                                               percent_articles = 100 * n_articles / total_articles,
                                               OA_label = ifelse(is_OA, "Open-access local journals",
                                                                 "Closed-access local journals")) %>%
                                        ungroup()

# merge only open-access cases using full country names
figure11B <- world %>% left_join(figure11B %>% filter(is_OA == TRUE), by = c("admin" = "country"))

# group countries at the region level
figure11B <- figure11B %>% mutate(region_custom = case_when(region_un %in% c("Africa", "Asia", "Europe", "Oceania") ~ region_un,
                                                          region_un == "Americas" & subregion == "Northern America" ~ "North America",
                                                          region_un == "Americas" & subregion %in% c("Central America", "Caribbean") ~ "Central America & the Caribbean",
                                                          region_un == "Americas" & subregion == "South America" ~ "South America",
                                                          TRUE ~ NA_character_))

# convert to long format
figure11B <- figure11B %>% select(name_long, region_custom, percent_articles, OA_label) %>%
                           rename(percent = percent_articles) %>%
                           mutate(metric = "Open Access")

# combine both figure11 A & B into a single dataframe for plotting purposes
figure11 <- bind_rows(figure11A %>% rename(label = langs_label),
                      figure11B %>% rename(label = OA_label))

figure11 <- figure11 %>% mutate(region_custom = case_when(name_long == "Mexico" ~ "North America", TRUE ~ region_custom))
figure11$region_custom <- factor(figure11$region_custom, levels = c("South America", "Central America & the Caribbean", "North America", "Europe", "Africa", "Asia", "Oceania"))

# plot boxplot
figure11 %>% filter(!is.na(percent), !is.na(region_custom)) %>%
  ggplot(aes(y = region_custom, x = percent, fill = region_custom)) +
  geom_boxplot(width = 0.6, outlier.shape = NA) +
  geom_jitter(height = 0.15, size = 1.2, alpha = 0.6, color = "grey50") +
  facet_wrap(~ metric, ncol = 2) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(y = "Region",
       x = "% of articles in mainstream local journals") +
  scale_y_discrete(labels = c("South America", "Central America\n& the Caribbean", "North America",
                              "Europe", "Africa", "Asia", "Oceania")) +
  scale_fill_manual(values = c("South America" = "#FEE08B", "Central America & the Caribbean" = "#FDAE61", "North America" = "#F46D43",
                               "Europe" = "#D53E4F", "Africa" = "#3288BD", "Asia" = "#66C2A5", "Oceania" = "#E6F598")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.position = "none",
        strip.text = element_text(size = 17, face = "bold"))
ggsave("~/Desktop/OpenAlex_journals_dataset/figures/Fig11.png", width = 9, height = 6, dpi = 600)