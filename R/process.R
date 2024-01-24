# load <- read_xlsx("inst/table2.xlsx",skip=1) %>%
#   suppressMessages() %>%
#   rename(id=1,author=2,year=3) %>%
#   pivot_longer(-c(1:3),names_to="var",values_to="count") %>%
#   mutate(count=ifelse(is.na(count),0,count),
#          type=case_when(var%in%c("Bone/sarcoma","Brain","Breast",
#                                  "Cancer - general","Colorectal",
#                                  "Gynecological","Haematological",
#                                  "Head & neck","Liver","Lung",
#                                  "Oesophagogastic","Other...15",
#                                  "Other GI","Pancreatic","Prostate",
#                                  "Renal","Skin")~"cancer",
# 
#                         var%in%c("International","Regional","UK",
#                                  "Scotland","Wales","Northern Ireland",
#                                  "England")~"country",
# 
#                         var%in%c("Age","Comorbidity","SES_Ed_Employ",
#                                  "Ethnicity","Geography","LGBTQ","Other...34",
#                                  "Sex")~"inequality",
# 
#                         var%in%c("Access to care","Awareness","Diagnosis",
#                                  "Experience","Incidence","Mortality",
#                                  "Other...42","Prevalence","Quality of life",
#                                  "Risk","Screening...46","Survival",
#                                  "Treatment")~"poc",
# 
#                         var%in%c("Registry","Hosp_Discharge","Screening...51",
#                                  "Waits","Audit","EHR","Survey/question",
#                                  "Trial","Cohort","CPES","Civil registration",
#                                  "Primary care",
#                                  "Other...61")~"source"),
#          var=case_when(grepl("Other\\...",var)~"Other",
#                        grepl("Screening\\...",var)~"Screening",
#                        var=="SES_Ed_Employ"~"SES/Ed/Employ",
#                        var=="Hosp_Discharge"~"Hospital Discharge",
#                        T ~ var)) %>%
#   filter(count==1)
# 
# cat_list <- list()
# 
# type <- unique(load$type)
# 
# for (i in 1:length(type)) {
# 
#   cat_list[[i]] <- load %>%
#     filter(type==type[i]) %>%
#     rename_with(~type[i],var) %>%
#     dplyr::select(-c(type,count))
# 
# }
# 
# final <- cat_list[[1]] %>%
#   left_join(.,cat_list[[2]],by=c("id","year","author"),
#             relationship="many-to-many") %>%
#   left_join(.,cat_list[[3]],by=c("id","year","author"),
#             relationship="many-to-many") %>%
#   left_join(.,cat_list[[4]],by=c("id","year","author"),
#             relationship="many-to-many") %>%
#   left_join(.,cat_list[[5]],by=c("id","year","author"),
#             relationship="many-to-many")
# 
# write_rds(final,"inst/processed_table2.rds")









