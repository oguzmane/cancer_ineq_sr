
cancerStringClean <- function(df) {
  
  df %>% 
    mutate(cancer_group=gsub(".*sites - |\\.","",cancer_group),
           cancer_group=trimws(cancer_group,"left"),
           cancer_group=gsub("Childhood |Childhood - |Children and young people - ",
                             "",cancer_group),
           cancer_group=StrCap(cancer_group),
           #### WRONG ONES
           cancer_group=gsub("Breast and gynaecological","Breast, gynaecological",cancer_group),
           cancer_group=gsub("Breast or prostate","Breast, prostate",cancer_group),
           cancer_group=gsub("Appediceal","Appendiceal",cancer_group),
           cancer_group=gsub("Leukaemias","Leukaemia",cancer_group)) %>% 
    separate_rows(cancer_group,sep=",") %>% 
    mutate(cancer_group=gsub(" gynaecological","Gynaecological",cancer_group),
           cancer_group=gsub(" prostate","Prostate",cancer_group),
           cancer_group=gsub("Wilms'","Wilms tumour",cancer_group), 
           #########
           cancer_group=case_when(grepl("All",cancer_group)~"All",
                                  grepl("Anal",cancer_group)~"Anal",
                                  cancer_group%in%c("Biliary tract")~"Bile duct cancer",
                                  cancer_group%in%c("Bladder","Urological","Urothelial")~"Bladder",
                                  grepl("Bone|sarcoma",cancer_group)~"Bone/sarcoma",
                                  grepl("Bowel|Colorectal|Colon|Rectal|Rectum",cancer_group)~"Bowel",
                                  grepl("Brain|CNS|Nervous|nervous|Glio|Vestibular",cancer_group)~"Brain & CNS",
                                  grepl("Breast",cancer_group)~"Breast",
                                  grepl("Cerv",cancer_group)~"Cervical",
                                  grepl("Germ|germ",cancer_group)~"Germ cell tumours",
                                  grepl("Head and neck",cancer_group)~"Head and neck",
                                  grepl("Oral|oral",cancer_group)~"Lip and oral cavity",
                                  grepl("Oropharyn|oropharyn",cancer_group)~"Oropharyngeal",
                                  grepl("Pharyn|pharyn",cancer_group)~"Pharyngeal", # THIS HAS TO BE AFTER OROPHARYN
                                  grepl("Kidney|Renal",cancer_group)~"Kidney",
                                  grepl("Laryn",cancer_group)~"Laryngeal",
                                  grepl("Lymphoblastic|lymphoblastic",cancer_group)~"Lymphoblastic leukaemia",
                                  grepl("myeloid",cancer_group)~"Myeloid leukaemia",
                                  grepl("NHL|Non-Hod",cancer_group)~"NHL",
                                  grepl("Hod",cancer_group)~"HL",
                                  grepl("Lymphomas|Lymphoma \\(",cancer_group)~"Lymphoma",
                                  grepl("Hepa|Liver",cancer_group)~"Liver",
                                  grepl("Lung|lung",cancer_group)~"Lung",
                                  cancer_group%in%c("Malignant melanoma","Melanoma")~"Melanoma",
                                  cancer_group%in%c("Non-melanoma skin","Basal Cell Carcinoma")~"Non-melanoma",
                                  grepl("Myeloma|myeloma",cancer_group)~"Myeloma",
                                  grepl("Neuroblast|neuroblast",cancer_group)~"Neuroblastoma",
                                  grepl("Oeso",cancer_group)~"Oesophageal",
                                  grepl("Oral",cancer_group)~"Oral",
                                  grepl("Ovar",cancer_group)~"Ovarian",
                                  grepl("Pancrea",cancer_group)~"Pancreatic",
                                  grepl("Gast|Upper aero",cancer_group)~"Stomach",
                                  grepl("Testi",cancer_group)~"Testicular",
                                  grepl("Vulv|vulv",cancer_group)~"Vulval",
                                  grepl("Uteri|uteri|Uterus|Uterine|Endomet",cancer_group)~"Uterine",
                                  grepl("Other|other",cancer_group)~"Other",
                                  T~cancer_group
             
           ),
           cancer_group=case_when(cancer_group=="All"~"Cancer - general",
                                  cancer_group=="NHL"~"Non-Hodgkin lymphoma",
                                  cancer_group=="Gynaecological"~"Gynaecological - general",
                                  cancer_group=="Haematological"~"Haematological - general",
                                  cancer_group=="HL"~"Hodgkin lymphoma",
                                  cancer_group=="Lymphoma"~"Lymphoma - general",
                                  cancer_group=="Leukaemia"~"Leukaemia - general",
                                  cancer_group=="Skin"~"Skin - general",
                                  cancer_group=="Head and neck"~"Head and neck - general",
                                  T ~ cancer_group))
  
  
  # sort(unique(try$cancer_group))
  # sort(unique(try$cancer_group))[grepl("Bone|bone",sort(unique(try$cancer_group)))]

}

cancerFormalGroup <- function(df) {
  
  choices = list("Overall"=list("Cancer - general"),
                 "Breast"=list("Breast"),
                 "Digestive/Gastrointestinal"=list("Anal","Appendiceal","Bowel",
                                                   "Bile duct cancer","Liver",
                                                   "Oesophageal","Pancreatic","Stomach"),
                 "Endocrine"=list("Thyroid"),
                 "Eye"=list("Retinoblastoma"),
                 "Genitourinary"=list("Bladder","Kidney","Penile","Prostate",
                                      "Testicular","Wilms tumour"),
                 "Germ Cell"=list("Germ cell tumours"),
                 "Gynaecological"=list("Gynaecological - general",
                                       "Cervical","Ovarian","Uterine","Vulval"),
                 "Head and Neck"=list("Head and neck - general","Lip and oral cavity",
                                      "Laryngeal","Pharyngeal","Oropharyngeal",
                                      "Salivary gland"),
                 "Haematological"=list("Haematological - general",
                                       "Lymphoma - general","Hodgkin lymphoma",
                                       "Non-Hodgkin lymphoma","Leukaemia - general",
                                       "Lymphoblastic leukaemia",
                                       "Myeloid leukaemia",
                                       "Myeloma"),
                 "Musculoskeletal"=list("Bone/sarcoma"),
                 "Neurological"=list("Brain & CNS","Neuroblastoma"),
                 "Respiratory"=list("Lung","Mesothelioma"),
                 "Skin"=list("Skin - general","Melanoma","Non-melanoma"),
                 "Other"=list("Other"))
  
  # Initialize an empty list to store the rows
  data_list <- list()
  
  # Iterate over the nested list to populate the data list
  for (group in names(choices)) {
    for (cancer in choices[[group]]) {
      data_list <- append(data_list, list(c(cancer, group)))
    }
  }
  
  # Convert the list to a data frame
  cancer_df <- as.data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE)
  
  # Name the columns
  colnames(cancer_df) <- c("cancer_group", "cancer_broad")
  
  final <- left_join(df,cancer_df,by="cancer_group")
  
}

cancer_levelsFUN <- function(type) {
  
  # NEED TO KEEP LEVELS CONSISTENT WITH FUNCTION ABOVE!!!!
  
  if (type=="broad") {
    
    cancer_levels=c("Overall","Breast","Digestive/Gastrointestinal",
                    "Endocrine","Eye","Genitourinary","Germ Cell",
                    "Gynaecological","Head and Neck","Haematological",
                    "Musculoskeletal","Neurological","Respiratory",
                    "Skin","Other")
    
    
  } else if (type=="detailed") {
    
    cancer_levels=c("Cancer - general","Breast",
                    "Anal","Appendiceal","Bowel","Bile duct cancer","Liver","Oesophageal","Pancreatic","Stomach",
                    "Thyroid",
                    "Retinoblastoma",
                    "Bladder","Kidney","Penile","Prostate","Testicular","Wilms tumour",
                    "Germ cell tumours",
                    "Gynaecological - general","Cervical","Ovarian","Uterine","Vulval",
                    "Head and neck - general","Lip and oral cavity","Laryngeal","Pharyngeal","Oropharyngeal","Salivary gland",
                    "Haematological - general","Lymphoma - general","Hodgkin lymphoma","Non-Hodgkin lymphoma","Leukaemia - general","Lymphoblastic leukaemia","Myeloid leukaemia","Myeloma",
                    "Bone/sarcoma",
                    "Brain & CNS","Neuroblastoma",
                    "Lung","Mesothelioma",
                    "Skin - general","Melanoma","Non-melanoma",
                    "Other") 
    
  }
  
  return(cancer_levels)
  
}








