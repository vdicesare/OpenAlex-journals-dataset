library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(readr)
library(stringr)
library(bit64)
options(scipen = 999)


### MEGA JOURNALS DATAFRAME CONSTRUCTION
# OpenAlex upload and data mining
openalex <- list.files(path = "~/Desktop/Local.Journals/OpenAlex", pattern = "all_journals_and_articles_.*", full.names = TRUE)
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
openalex_topics <- readxl::read_excel("~/Desktop/Local.Journals/OAtopics.xlsx")
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
mjl_journals <- read.csv("~/Desktop/Local.Journals/MJL.csv")
mjl_journals <- mjl_journals %>% distinct()
mjl_journals <- mjl_journals %>% group_by(journal_name, issn, eissn) %>%
                                 summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")


# JCR upload and data mining
jcr_journals <- list.files(path = "~/Desktop/Local.Journals/JCR", pattern = "VictoriaDi.*JCR_JournalResults.*", full.names = TRUE)
jcr_journals <- rbindlist(lapply(jcr_journals, fread, sep = ","), fill = TRUE)
jcr_journals <- jcr_journals %>% group_by(`Journal name`, `JCR Abbreviation`, Publisher, ISSN, eISSN) %>%
                                 summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
jcr_journals <- jcr_journals %>% rename(journal_name = `Journal name`, issn = ISSN, eissn = eISSN)
jcr_journals[jcr_journals == "N/A"] <- NA
jcr_journals <- bind_rows(jcr_journals, read.csv("~/Desktop/Local.Journals/JCR/JCR_missing_journals.csv", check.names = FALSE) %>%
                            mutate(across(everything(), as.character)))


# Scopus upload and data mining
scopus_journals <- readxl::read_excel("~/Desktop/Local.Journals/Scopus.xlsx")
scopus_journals <- scopus_journals %>% group_by(journal_name, issn, eissn) %>%
                                       summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
scopus_journals[scopus_journals == "NA"] <- NA
scopus_journals$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_journals$issn)
scopus_journals$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_journals$eissn)


# DOAJ upload and data mining
doaj_journals <- read.csv("~/Desktop/Local.Journals/DOAJ.csv")
doaj_journals$issn <- ifelse(!is.na(doaj_journals$issn) & doaj_journals$issn != "", 
                             str_pad(doaj_journals$issn, width = 8, side = "left", pad = "0"), 
                             doaj_journals$issn)
doaj_journals$eissn <- ifelse(!is.na(doaj_journals$eissn) & doaj_journals$eissn != "", 
                              str_pad(doaj_journals$eissn, width = 8, side = "left", pad = "0"), 
                              doaj_journals$eissn)
doaj_journals$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_journals$issn)
doaj_journals$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_journals$eissn)


# SJR upload and data mining
sjr_journals <- readxl::read_excel("~/Desktop/Local.Journals/SJR.xlsx")
sjr_journals <- sjr_journals %>% separate(eissn, into = c("eissn", "issn"), sep = ", ", extra = "merge", fill = "right") %>%
                                 mutate(issn = ifelse(is.na(issn), NA, issn))
sjr_journals <- sjr_journals %>% group_by(journal_name, issn, eissn) %>%
                                 summarise(across(everything(), ~ paste(unique(.), collapse = ";")), .groups = "drop")
sjr_journals$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", sjr_journals$issn)
sjr_journals$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", sjr_journals$eissn)


# CWTS upload
cwts_journals <- readxl::read_excel("~/Desktop/Local.Journals/CWTS.xlsx")


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
ddff_ISSNs_no_match <- ddff_ISSNs_no_match %>% left_join(select(openalex_journals, OA_ID, OA_ISSN_codes, OA_journal_name, OA_journal_name_variants), by = "OA_ID")
ddff_ISSNs_no_match <- ddff_ISSNs_no_match %>% select(OA_ID, OA_ISSN_codes, OA_journal_name, OA_journal_name_variants)
write.csv(ddff_ISSNs_no_match, "~/Desktop/Local.Journals/OA_titles_matching.csv")


# isolate the journals from the rest of the ddbb that don't match with OpenAlex through their ISSN codes in order to match via titles
mjl_journals_no_match <- mjl_journals %>% anti_join(ddff_ISSNs_match, by = "MJL_ID") %>%
                                          select(MJL_ID, MJL_ISSN_codes, MJL_journal_name)
write.csv(mjl_journals_no_match, "~/Desktop/Local.Journals/MJL_titles_matching.csv")

jcr_journals_no_match <- jcr_journals %>% anti_join(ddff_ISSNs_match, by = "JCR_ID") %>%
                                          select(JCR_ID, JCR_ISSN_codes, JCR_journal_name)
write.csv(jcr_journals_no_match, "~/Desktop/Local.Journals/JCR_titles_matching.csv")

scopus_journals_no_match <- scopus_journals %>% anti_join(ddff_ISSNs_match, by = "SCOP_ID") %>%
                                                select(SCOP_ID, SCOP_ISSN_codes, SCOP_journal_name)
write.csv(scopus_journals_no_match, "~/Desktop/Local.Journals/SCOP_titles_matching.csv")

doaj_journals_no_match <- doaj_journals %>% anti_join(ddff_ISSNs_match, by = "DOAJ_ID") %>%
                                            select(DOAJ_ID, DOAJ_ISSN_codes, DOAJ_journal_name)
write.csv(doaj_journals_no_match, "~/Desktop/Local.Journals/DOAJ_titles_matching.csv")

sjr_journals_no_match <- sjr_journals %>% anti_join(ddff_ISSNs_match, by = "SJR_ID") %>%
                                          select(SJR_ID, SJR_ISSN_codes, SJR_journal_name)
write.csv(sjr_journals_no_match, "~/Desktop/Local.Journals/SJR_titles_matching.csv")

cwts_journals_no_match <- cwts_journals %>% anti_join(ddff_ISSNs_match, by = "CWTS_ID") %>%
                                            select(CWTS_ID, CWTS_ISSN_codes, CWTS_journal_name)
write.csv(cwts_journals_no_match, "~/Desktop/Local.Journals/CWTS_titles_matching.csv")


### MEGA MERGE by ISSNs... (faltan los matches por títulos)
ddff_megamerge <- ddff_ISSNs_match %>% left_join(openalex_journals, by = "OA_ID") %>%
                                       left_join(mjl_journals, by = "MJL_ID") %>%
                                       left_join(jcr_journals, by = "JCR_ID") %>%
                                       left_join(scopus_journals, by = "SCOP_ID") %>%
                                       left_join(doaj_journals, by = "DOAJ_ID") %>%
                                       left_join(sjr_journals, by = "SJR_ID") %>%
                                       left_join(cwts_journals, by = "CWTS_ID")

ddff_megamerge <- ddff_megamerge %>% select(OA_ID, MJL_ID, JCR_ID, SCOP_ID, DOAJ_ID, SJR_ID, CWTS_ID,
                                            OA_source_ID, SCOP_source_ID, SJR_source_ID,
                                            OA_ISSN_codes, MJL_ISSN_codes, JCR_ISSN_codes, SCOP_ISSN_codes, DOAJ_ISSN_codes, DOAJ_continues_ISSN, DOAJ_continued_by_ISSN, SJR_ISSN_codes, CWTS_ISSN_codes,
                                            OA_journal_name, MJL_journal_name, JCR_journal_name, SCOP_journal_name, DOAJ_journal_name, SJR_journal_name, CWTS_journal_name,
                                            OA_journal_name_variants, JCR_journal_name_variants, SCOP_journal_name_variants, DOAJ_journal_name_variants,
                                            OA_publisher, MJL_publisher, JCR_publisher, SCOP_publisher, DOAJ_publisher, SJR_publisher,
                                            OA_publisher_country, MJL_publisher_country, DOAJ_publisher_country, SJR_publisher_country,
                                            SCOP_main_publisher, DOAJ_other_organization, DOAJ_other_organization_country, SJR_region,
                                            MJL_language, SCOP_language, DOAJ_language,
                                            OA_topics, OA_primary_topics, OA_subfields, OA_fields, OA_domains, MJL_categories, JCR_categories, SJR_categories, SJR_areas, SCOP_ASJC_codes, DOAJ_LCC_codes, DOAJ_subjects, DOAJ_keywords,
                                            JCR_edition, SCOP_coverage, SJR_coverage,
                                            OA_open_access, SCOP_open_access, DOAJ_open_access, DOAJ_open_license_since, DOAJ_license, DOAJ_license_attributes, DOAJ_author_unrestricted_rights, DOAJ_open_citations, DOAJ_machine_readable_license,
                                            DOAJ_review_process, DOAJ_average_weeks_for_publication, OA_APC_prices, DOAJ_APC_prices, DOAJ_other_fees, DOAJ_waiver_policy, DOAJ_deposit_policy, DOAJ_plagiarism_policy,
                                            DOAJ_persistent_identifiers, DOAJ_preservation_services, DOAJ_national_library_preservation_services, SCOP_medline_sourced, OA_website, DOAJ_website,
                                            JCR_JCI_2023, JCR_JCI_rank, JCR_JCI_quartile, JCR_JCI_percentile, JCR_JIF_2023, JCR_JIF_rank, JCR_JIF_quartile, JCR_JIF_percentile, JCR_JIF_5_years, JCR_JIF_5_years_quartile, JCR_JIF_no_self_cites,
                                            JCR_immediacy_index, JCR_AIS, JCR_AIS_quartile, JCR_eigenfactor, JCR_eigenfactor_normalized, JCR_citing_half_life, JCR_cited_half_life, JCR_percent_articles_citable_items, JCR_percent_citable_open_access,
                                            SJR_SJR, SJR_rank, SJR_best_quartile, SJR_h_index,
                                            OA_total_articles, JCR_total_articles, SJR_total_articles_2023, SJR_total_articles_3_years, SJR_total_references, SJR_references_per_articles, OA_total_citations, JCR_total_citations, SJR_total_citations_3_years, SJR_citations_per_articles_2_years, JCR_citable_articles, SJR_citable_articles_3_years,
                                            CWTS_percent_self_citations, CWTS_SNIP, CWTS_SNIP_lower_bound, CWTS_SNIP_upper_bound, CWTS_IPP, CWTS_IPP_lower_bound, CWTS_IPP_upper_bound, SJR_percent_female, SJR_SDG, SJR_overton)

ddff_megamerge <- ddff_megamerge %>% mutate(across(where(is.character), ~ na_if(.x, "")))
#write.csv(ddff_megamerge, "~/Desktop/Local.Journals/mega_merge.csv")