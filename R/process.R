# source("R/global_utils.R")
# 
# load <- read_xlsx("inst/table5.xlsx",skip=1) %>%
#   suppressMessages() %>%
#   rename(end_id=1,cov_id=2,author=3,title=4,
#          summary=5,journal=6,pub_year=7,
#          study_start=8,study_end=9,no_pat=10,
#          cancer_type=11,comorbidity_type=12,
#          inequality_type=13,ses_measure=14,
#          age_measure=15,sex_measure=16,
#          ethnicity_measure=17,geography_measure=18,
#          lgbtq_measure=19,other_measure=20,
#          comorbidity_measure=21) %>%
#   pivot_longer(-c(1:21),names_to="var",values_to="count") %>%
#   mutate(count=ifelse(is.na(count),0,count),
#          type=case_when(var%in%c("All Cancer",
#                                  "Bone/sarcoma","Brain","Breast",
#                                  "Cancer - general","Colorectal",
#                                  "Gynecological","Haematological",
#                                  "Head & neck","Liver","Lung",
#                                  "Oesophagogastic","Other Cancer",
#                                  "Other GI","Pancreatic","Prostate",
#                                  "Renal","Skin")~"cancer",
# 
#                         var%in%c("International","Regional","UK",
#                                  "Scotland","Wales","Northern Ireland",
#                                  "England")~"country",
# 
#                         var%in%c("Age","Comorbidity","SES_Ed_Employ",
#                                  "Ethnicity","Geography",
#                                  "LGBTQ","Other Inequality",
#                                  "Sex")~"inequality",
# 
#                         var%in%c("Access to care","Awareness","Diagnosis",
#                                  "Experience","Incidence","Mortality",
#                                  "Other Care","Prevalence","Quality of life",
#                                  "Risk","Screening","Survival",
#                                  "Treatment")~"poc",
# 
#                         var%in%c("Registry","Hosp_Discharge","Screening Data",
#                                  "Waits","Audit","EHR","Survey/question",
#                                  "Trial","Cohort","CPES","Civil registration",
#                                  "Primary care",
#                                  "Other Source")~"source"),
#          var=case_when(grepl("Other\\...",var)~"Other",
#                        grepl("Screening\\...",var)~"Screening",
#                        var=="SES_Ed_Employ"~"SES/Ed/Employ",
#                        var=="Hosp_Discharge"~"Hospital Discharge",
#                        T ~ var)) %>%
#   filter(count==1) %>%
#   #######################
#   #######################
#   # mutate(inequality_type=gsub(", ",",",inequality_type)) %>%
#   # separate_rows(inequality_type,sep=",") %>%
#   #######################
#   #######################
#   mutate_at(c("study_start","study_end","no_pat"),~as.numeric(.)) %>%   # suppress Warnings messes up the code...
# 
#   filter(pub_year<2024) %>%
#   mutate(no_pat_org=no_pat,
#          no_pat=case_when(between(no_pat,0,1999)~"0 to 1,999",
#                           between(no_pat,2000,19999)~"2,000 to 19,999",
#                           between(no_pat,20000,99999)~"20,000 to 99,999",
#                           no_pat>99999~">100,000"),
#          no_pat=factor(no_pat,levels=c("0 to 1,999","2,000 to 19,999",
#                                        "20,000 to 99,999",">100,000"))
#          ) %>%
#   mutate(cancer_group=gsub(", |;",",",cancer_type)) %>%
#   separate_rows(cancer_group,sep=",") %>%
#   cancerStringClean(.) %>%
#   cancerFormalGroup(.) %>%
#   mutate_at(c("pub_year","study_start","study_end"),
#             ~case_when(.>1900~.,
#                        T~NA)) # FOR PREVIOUS TYPO...CAN DELETE EVENTUALLY
# 
# cat_list <- list()
# 
# cattype <- unique(load$type)[!is.na(unique(load$type))]
# 
# for (i in 1:length(cattype)) {
# 
#   cat_list[[i]] <- load %>%
#     filter(type==cattype[i]) %>%
#     rename_with(~cattype[i],var) %>%
#     dplyr::select(-c(type,count))
# 
# }
# 
# final <- cat_list[[1]] %>%
#   full_join(.,cat_list[[2]],by=c("end_id","cov_id","author","title","journal",
#                                  "pub_year","study_start","study_end",
#                                  "no_pat","cancer_type","inequality_type",
#                                  "cancer_group","cancer_broad","summary","no_pat_org",
#                                  "comorbidity_type","ses_measure",
#                                  "age_measure","sex_measure",
#                                  "ethnicity_measure","geography_measure",
#                                  "lgbtq_measure","other_measure",
#                                  "comorbidity_measure"),
#             relationship="many-to-many") %>%
#   full_join(.,cat_list[[3]],by=c("end_id","cov_id","author","title","journal",
#                                  "pub_year","study_start","study_end",
#                                  "no_pat","cancer_type","inequality_type",
#                                  "cancer_group","cancer_broad","summary","no_pat_org",
#                                  "comorbidity_type","ses_measure",
#                                  "age_measure","sex_measure",
#                                  "ethnicity_measure","geography_measure",
#                                  "lgbtq_measure","other_measure",
#                                  "comorbidity_measure"),
#             relationship="many-to-many") %>%
#   full_join(.,cat_list[[4]],by=c("end_id","cov_id","author","title","journal",
#                                  "pub_year","study_start","study_end",
#                                  "no_pat","cancer_type","inequality_type",
#                                  "cancer_group","cancer_broad","summary","no_pat_org",
#                                  "comorbidity_type","ses_measure",
#                                  "age_measure","sex_measure",
#                                  "ethnicity_measure","geography_measure",
#                                  "lgbtq_measure","other_measure",
#                                  "comorbidity_measure"),
#             relationship="many-to-many") %>%
#   full_join(.,cat_list[[5]],by=c("end_id","cov_id","author","title","journal",
#                                  "pub_year","study_start","study_end",
#                                  "no_pat","cancer_type","inequality_type",
#                                  "cancer_group","cancer_broad","summary","no_pat_org",
#                                  "comorbidity_type","ses_measure",
#                                  "age_measure","sex_measure",
#                                  "ethnicity_measure","geography_measure",
#                                  "lgbtq_measure","other_measure",
#                                  "comorbidity_measure"),
#             relationship="many-to-many") %>%
#   mutate(color=case_when(inequality=="SES/Ed/Employ"~"#2e008b",
#                               inequality=="Age"~"#e40074",
#                               inequality=="Sex"~"#00b6ed",
#                               inequality=="Ethnicity"~"#005870",
#                               inequality=="Comorbidity"~"#BB0071",
#                               inequality=="Geography"~"#2e2d2c",
#                               inequality=="Other Inequality"~"#9A9A9A",
#                               inequality=="LGBTQ"~"#710044"))
# 
# 
# 
# write_rds(final,"inst/processed_table3.rds")

# FOR MISSING -------------------------------------------------------------

# miss_process <- read_rds("inst/processed_table3.rds")
# 
# sum(is.na(miss_process$cancer_group)) # 0
# sum(is.na(miss_process$pub_year)) # 0
# sum(is.na(miss_process$study_start)) # 52
# sum(is.na(miss_process$study_end)) # 52
# sum(is.na(miss_process$no_pat)) # 1376
# sum(is.na(miss_process$poc)) # 2671
# sum(is.na(miss_process$source)) # 46
# sum(is.na(miss_process$country)) # 51
# 
# class(miss_process$cancer_group) # character
# class(miss_process$pub_year) # numeric
# class(miss_process$study_start) # numeric
# class(miss_process$study_end) # numeric
# class(miss_process$no_pat) # factor
# class(miss_process$poc) # character
# class(miss_process$source) # character
# class(miss_process$country) # character
# 
# fill_miss <- miss_process %>%
#   mutate_at(c("no_pat","poc","source","country"),
#             ~case_when(is.na(.)~"Missing",T~.)) %>%
#   mutate(no_pat=factor(no_pat,
#                        levels=c("0 to 1,999","2,000 to 19,999",
#                                 "20,000 to 99,999",">100,000","Missing")),
#          poc=factor(poc,
#                     levels=c(sort(unique(miss_process$poc)),"Missing")),
#          source=factor(source,
#                        levels=c(sort(unique(miss_process$source)),"Missing")),
#          country=factor(country,
#                         levels=c("England","Northern Ireland","Scotland",
#                                  "Wales","Regional","UK","International",
#                                  "Missing")))
# 
# write_rds(fill_miss,"inst/processed_table3_miss.rds")


# GEO ---------------------------------------------------------------------
# 
# geo_1<-st_read("inst/geo/CTRY_DEC_2022_UK_BGC.shp")
# geo_2<-st_transform(geo, '+proj=longlat +datum=WGS84')
# geo_3<-ms_simplify(geo_2)
# write_rds(geo_3,"inst/geo.rds")





