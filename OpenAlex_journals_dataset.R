library(parallel)
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(readr)
library(stringr)
library(bit64)
library(ggplot2)
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


## PUBLICATIONS
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
# ddff_megamerge, buscar la presencia de "ENG" o "English" en la variable anidada language. Traer los datos language de OpenAlex. Guardar como 1-0 en una nueva variable languages_local_variable


## DATABASES


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


### FIGURES
## FIGURE 1
# manually build the vector to plot databases overlap
figure1 <- c(OpenAlex = 40619, MJL = 0, JCR = 0, Scopus = 0, SJR = 0, CWTS = 0, DOAJ = 0,
             "OpenAlex&MJL" = 46, "OpenAlex&JCR" = 5, "OpenAlex&Scopus" = 1296, "OpenAlex&SJR" = 5, "OpenAlex&CWTS" = 86, "OpenAlex&DOAJ" = 7941,
             "OpenAlex&MJL&JCR" = 865, "OpenAlex&MJL&Scopus" = 14, "OpenAlex&MJL&SJR" = 0, "OpenAlex&MJL&CWTS" = 1, "OpenAlex&MJL&DOAJ" = 47,
             "OpenAlex&JCR&Scopus" = 0, "OpenAlex&JCR&SJR" = 0, "OpenAlex&JCR&CWTS" = 0, "OpenAlex&JCR&DOAJ" = 4,
             "OpenAlex&Scopus&SJR" = 110, "OpenAlex&Scopus&CWTS" = 1, "OpenAlex&Scopus&DOAJ" = 511,
             "OpenAlex&SJR&CWTS" = 112, "OpenAlex&SJR&DOAJ" = 0, "OpenAlex&CWTS&DOAJ" = 6,
             "OpenAlex&MJL&JCR&Scopus" = 101, "OpenAlex&MJL&JCR&SJR" = 1, "OpenAlex&MJL&JCR&CWTS" = 20, "OpenAlex&MJL&JCR&DOAJ" = 813, "OpenAlex&MJL&Scopus&SJR" = 2, "OpenAlex&MJL&Scopus&CWTS" = 0, "OpenAlex&MJL&Scopus&DOAJ" = 61, "OpenAlex&MJL&SJR&CWTS" = 3, "OpenAlex&MJL&SJR&DOAJ" = 0, "OpenAlex&MJL&CWTS&DOAJ" = 0,
             "OpenAlex&JCR&Scopus&SJR" = 0, "OpenAlex&JCR&Scopus&CWTS" = 0, "OpenAlex&JCR&Scopus&DOAJ" = 0, "OpenAlex&JCR&SJR&CWTS" = 13, "OpenAlex&JCR&SJR&DOAJ" = 0, "OpenAlex&JCR&CWTS&DOAJ" = 0,
             "OpenAlex&Scopus&SJR&CWTS" = 3317, "OpenAlex&Scopus&SJR&DOAJ" = 29, "OpenAlex&Scopus&CWTS&DOAJ" = 2,
             "OpenAlex&SJR&CWTS&DOAJ" = 13,
             "OpenAlex&MJL&JCR&Scopus&SJR" = 165, "OpenAlex&MJL&JCR&Scopus&CWTS" = 5, "OpenAlex&MJL&JCR&Scopus&DOAJ" = 132, "OpenAlex&MJL&JCR&SJR&CWTS" = 44, "OpenAlex&MJL&JCR&SJR&DOAJ" = 0, "OpenAlex&MJL&JCR&CWTS&DOAJ" = 13, "OpenAlex&MJL&Scopus&SJR&CWTS" = 129, "OpenAlex&MJL&Scopus&SJR&DOAJ" = 3, "OpenAlex&MJL&Scopus&CWTS&DOAJ" = 0, "OpenAlex&MJL&SJR&CWTS&DOAJ" = 0,
             "OpenAlex&JCR&Scopus&SJR&CWTS" = 21, "OpenAlex&JCR&Scopus&SJR&DOAJ" = 0, "OpenAlex&JCR&Scopus&CWTS&DOAJ" = 0, "OpenAlex&JCR&SJR&CWTS&DOAJ" = 5,
             "OpenAlex&Scopus&SJR&CWTS&DOAJ" = 2038,
             "OpenAlex&MJL&JCR&Scopus&SJR&CWTS" = 13329, "OpenAlex&MJL&JCR&Scopus&SJR&DOAJ" = 19, "OpenAlex&MJL&JCR&Scopus&CWTS&DOAJ" = 0, "OpenAlex&MJL&JCR&SJR&CWTS&DOAJ" = 14, "OpenAlex&MJL&Scopus&SJR&CWTS&DOAJ" = 131,
             "OpenAlex&JCR&Scopus&SJR&CWTS&DOAJ" = 7,
             "OpenAlex&MJL&JCR&Scopus&SJR&CWTS&DOAJ" = 4462)

# keep only cases with more than one set and values > 0
figure1 <- figure1[grepl("&", names(figure1)) & figure1 > 0]

# plot
figure1 <- upset(fromExpression(figure1),
                      nintersects = NA,
                      nsets = 7,
                      sets = c("OpenAlex", "MJL", "JCR", "Scopus", "SJR", "CWTS", "DOAJ"),
                      mainbar.y.label = "Intersection size",
                      main.bar.color = "#F8766D",
                      sets.x.label = "Set size",
                      point.size = 1.5,
                      matrix.color = "grey50",
                      line.size = 0.5,
                      order.by = "freq", 
                      decreasing = TRUE,
                      show.numbers = "no",
                      mb.ratio = c(0.5, 0.5))
png(filename = "~/Desktop/OpenAlex_journals_dataset/figure1.png", width = 6.27, height = 3.14, units = "in", res = 300)
print(figure1)
dev.off()


## FIGURE 2
# isolate the journals that belong to each group: locally informed, situated and relevant research considering the 3ºQ of their variables distributions respectively: refs_prop (0.43), tops_prop (0.14), cits_prop (1) + pubs_prop (0.94)
locally_informed_res <- ddff_megamerge %>% distinct(OA_source_ID, .keep_all = TRUE) %>%
                                           filter(refs_prop >= 0.43) %>%
                                           select(OA_source_ID, other_IDs, refs_prop, refs_country)
locally_informed_res <- locally_informed_res %>% unnest(other_IDs)

locally_situated_res <- ddff_megamerge %>% distinct(OA_source_ID, .keep_all = TRUE) %>%
                                           filter(tops_prop >= 0.14) %>%
                                           select(OA_source_ID, other_IDs, tops_prop, pubs_country)
locally_situated_res <- locally_situated_res %>% unnest(other_IDs)

locally_relevant_res <- ddff_megamerge %>% distinct(OA_source_ID, .keep_all = TRUE) %>%
                                           filter(cits_prop == 1 | pubs_prop >= 0.94) %>%
                                           select(OA_source_ID, other_IDs, cits_prop, cits_country, pubs_prop, pubs_country)
locally_relevant_res <- locally_relevant_res %>% unnest(other_IDs)

# produce intersections diagram
informed <- locally_informed_res %>% pull(OA_source_ID) %>% unique()
situated <- locally_situated_res %>% pull(OA_source_ID) %>% unique()
relevant <- locally_relevant_res %>% pull(OA_source_ID) %>% unique()

informed_situated <- intersect(informed, situated)
informed_relevant <- intersect(informed, relevant)
situated_relevant <- intersect(situated, relevant)
informed_situated_relevant <- Reduce(intersect, list(informed, situated, relevant))

# create an Euler diagram with input data
venn_data <- c("Locally informed research" = length(informed),
               "Locally situated research" = length(situated),
               "Locally relevant research" = length(relevant),
               "Locally informed research&Locally situated research" = length(informed_situated),
               "Locally informed research&Locally relevant research" = length(informed_relevant),
               "Locally situated research&Locally relevant research" = length(situated_relevant),
               "Locally informed research&Locally situated research&Locally relevant research" = length(informed_situated_relevant))
fit <- euler(venn_data)

# plot the diagram
figure2 <- plot(fit, 
                fills = adjustcolor(c("#7CAE00", "#00BA38", "#00BFC4"), alpha.f = 0.6),
                labels = c("Locally Informed\nResearch", "Locally Situated\nResearch", "Locally Relevant\nResearch"),
                edges = FALSE, 
                quantities = list(font = ifelse(venn_data == length(informed_situated_relevant), 2, 1)))
ggsave("~/Desktop/OpenAlex_journals_dataset/figure2.png", plot = figure2, width = 6.5, height = 6, dpi = 300)


## FIGURE 3
# include a categoric label in each dataframe
locally_informed_res <- locally_informed_res %>% mutate(group = "Locally Informed Research")
locally_situated_res <- locally_situated_res %>% mutate(group = "Locally Situated Research")
locally_relevant_res <- locally_relevant_res %>% mutate(group = "Locally Relevant Research")
all_journals <- ddff_megamerge %>% select(OA_source_ID, other_IDs) %>%
                                   unnest(other_IDs) %>%
                                   mutate(group = "All journals")

# bind all data together
figure3 <- bind_rows(locally_informed_res %>% select(group, OA_source_ID, MJL_ID, JCR_ID, SCOP_ID, SJR_ID, CWTS_ID, DOAJ_ID),
                     locally_situated_res %>% select(group, OA_source_ID, MJL_ID, JCR_ID, SCOP_ID, SJR_ID, CWTS_ID, DOAJ_ID),
                     locally_relevant_res %>% select(group, OA_source_ID, MJL_ID, JCR_ID, SCOP_ID, SJR_ID, CWTS_ID, DOAJ_ID),
                     all_journals %>% select(group, OA_source_ID, MJL_ID, JCR_ID, SCOP_ID, SJR_ID, CWTS_ID, DOAJ_ID))

# convert to long format
figure3 <- figure3 %>% pivot_longer(cols = c(OA_source_ID, MJL_ID, JCR_ID, SCOP_ID, SJR_ID, CWTS_ID, DOAJ_ID),
                                    names_to = "source",
                                    values_to = "id") %>%
                                    mutate(present = !is.na(id))
figure3 <- figure3 %>% filter(present == TRUE)
figure3 <- figure3 %>% distinct()
figure3 <- figure3 %>% count(group, source, name = "count") %>%
                       group_by(group)
figure3$source <- factor(figure3$source, levels = c("OA_source_ID", "MJL_ID", "JCR_ID", "SCOP_ID", "SJR_ID", "CWTS_ID", "DOAJ_ID"))

ggplot(figure3, aes(x = source, y = count, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ group) +
  labs(x = "Data Sources", y = "Number of Journals") +
  coord_flip() +
  scale_x_discrete(labels = c("OA_source_ID" = "OpenAlex", "MJL_ID" = "MJL", "JCR_ID" = "JCR", "SCOP_ID" = "Scopus", "SJR_ID" = "SJR", "CWTS_ID" = "CWTS", "DOAJ_ID" = "DOAJ")) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("~/Desktop/OpenAlex_journals_dataset/figure3.png", width = 6.27, height = 3.14, dpi = 300)


## FIGURE 4 distribución de proporciones por dimensiones y para todas las revistas?


## FIGURE 5 mapas como los de Elvira?


#write.csv(ddff_megamerge, "~/Desktop/OpenAlex_journals_dataset/mega_merge.csv")