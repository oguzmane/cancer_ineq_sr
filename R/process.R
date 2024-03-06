# source("R/global_utils.R")
# 
# load <- read_xlsx("inst/table3 copy.xlsx",skip=1) %>%
#   suppressMessages() %>%
#   rename(end_id=1,cov_id=2,author=3,title=4,
#          summary=5,journal=6,pub_year=7,
#          study_start=8,study_end=9,no_pat=10,
#          cancer_type=11,inequality_type=12) %>%
#   pivot_longer(-c(1:12),names_to="var",values_to="count") %>%
#   mutate(count=ifelse(is.na(count),0,count),
#          type=case_when(var%in%c("Bone/sarcoma","Brain","Breast",
#                                  "Cancer - general","Colorectal",
#                                  "Gynecological","Haematological",
#                                  "Head & neck","Liver","Lung",
#                                  "Oesophagogastic","Other...24",
#                                  "Other GI","Pancreatic","Prostate",
#                                  "Renal","Skin")~"cancer",
# 
#                         var%in%c("International","Regional","UK",
#                                  "Scotland","Wales","Northern Ireland",
#                                  "England")~"country",
# 
#                         var%in%c("Age","Comorbidity","SES_Ed_Employ",
#                                  "Ethnicity","Geography","LGBTQ","Other...43",
#                                  "Sex")~"inequality",
# 
#                         var%in%c("Access to care","Awareness","Diagnosis",
#                                  "Experience","Incidence","Mortality",
#                                  "Other...51","Prevalence","Quality of life",
#                                  "Risk","Screening...55","Survival",
#                                  "Treatment")~"poc",
# 
#                         var%in%c("Registry","Hosp_Discharge","Screening...60",
#                                  "Waits","Audit","EHR","Survey/question",
#                                  "Trial","Cohort","CPES","Civil registration",
#                                  "Primary care",
#                                  "Other...70")~"source"),
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
#   mutate_at(c("study_start","study_end","no_pat"),~as.numeric(.)) %>%  # suppress Warnings messes up the code...
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
#   mutate(cancer_group=case_when(cancer_group=="All"~"Cancer - General",
#                                 cancer_group=="NHL"~"Non-Hodgkin Lymphoma",
#                                 cancer_group=="HL"~"Hodgkin Lymphoma",
#                                 cancer_group=="Lymphoma"~"Lymphoma - General",
#                                 cancer_group=="Leukaemia"~"Leukaemia - General",
#                                 cancer_group=="Skin"~"Skin - General",
#                                 cancer_group=="Head and neck"~"Head and Neck - General",
#                                 T ~ cancer_group))
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
#                                  "cancer_group","summary","no_pat_org"
#                                  ),
#             relationship="many-to-many") %>%
#   full_join(.,cat_list[[3]],by=c("end_id","cov_id","author","title","journal",
#                                  "pub_year","study_start","study_end",
#                                  "no_pat","cancer_type","inequality_type",
#                                  "cancer_group","summary","no_pat_org"
#                                  ),
#             relationship="many-to-many") %>%
#   full_join(.,cat_list[[4]],by=c("end_id","cov_id","author","title","journal",
#                                  "pub_year","study_start","study_end",
#                                  "no_pat","cancer_type","inequality_type",
#                                  "cancer_group","summary","no_pat_org"
#                                  ),
#             relationship="many-to-many") %>%
#   full_join(.,cat_list[[5]],by=c("end_id","cov_id","author","title","journal",
#                                  "pub_year","study_start","study_end",
#                                  "no_pat","cancer_type","inequality_type",
#                                  "cancer_group","summary","no_pat_org"
#                                  ),
#             relationship="many-to-many") %>%
#   mutate(color=case_when(inequality=="SES/Ed/Employ"~"#2e008b",
#                               inequality=="Age"~"#e40074",
#                               inequality=="Sex"~"#00b6ed",
#                               inequality=="Ethnicity"~"#005870",
#                               inequality=="Comorbidity"~"#BB0071",
#                               inequality=="Geography"~"#2e2d2c",
#                               inequality=="Other"~"#9A9A9A",
#                               inequality=="LGBTQ"~"#710044"))
# 
# 
# 
# write_rds(final,"inst/processed_table2.rds")

# geo_1<-st_read("inst/geo/CTRY_DEC_2022_UK_BGC.shp")
# geo_2<-st_transform(geo, '+proj=longlat +datum=WGS84')
# geo_3<-ms_simplify(geo_2)
# write_rds(geo_3,"inst/geo.rds")








