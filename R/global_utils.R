
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
           cancer_group=gsub("Wilms'","Wilms",cancer_group), 
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
                                  grepl("Head and neck|Oral|Pharyn|Oropharyn",cancer_group)~"Head and neck",
                                  grepl("Kidney|Renal",cancer_group)~"Kidney",
                                  grepl("Laryn",cancer_group)~"Laryngeal",
                                  grepl("Lymphoblastic|lymphoblastic",cancer_group)~"Lymphoblastic Leukaemia",
                                  grepl("myeloid",cancer_group)~"Myeloid Leukaemia",
                                  grepl("NHL|Non-Hod",cancer_group)~"NHL",
                                  grepl("Hod",cancer_group)~"HL",
                                  grepl("Lymphomas|Lymphoma \\(",cancer_group)~"Lymphoma",
                                  grepl("Hepa|Liver",cancer_group)~"Liver",
                                  grepl("Lung|lung",cancer_group)~"Lung",
                                  cancer_group%in%c("Malignant melanoma","Melanoma")~"Melanoma",
                                  cancer_group%in%c("Non-melanoma skin","Basal Cell Carcinoma")~"Non-melanoma",
                                  grepl("Myeloma|myeloma",cancer_group)~"Myeloma",
                                  grepl("Oeso",cancer_group)~"Oesophageal",
                                  grepl("Oral",cancer_group)~"Oral",
                                  grepl("Ovar",cancer_group)~"Ovarian",
                                  grepl("Pancrea",cancer_group)~"Pancreatic",
                                  grepl("Gast|Upper aero",cancer_group)~"Stomach",
                                  grepl("Testi",cancer_group)~"Testicular",
                                  grepl("Uteri|uteri|Uterus|Uterine|Endomet",cancer_group)~"Uterine",
                                  grepl("Other|other",cancer_group)~"Other",
                                  T~cancer_group
             
           ))
  
  
  # sort(unique(try$cancer_group))
  # sort(unique(try$cancer_group))[grepl("Bone|bone",sort(unique(try$cancer_group)))]

  }

