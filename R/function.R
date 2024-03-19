
treeFUN <- function(df,site_i,pub_year_i,study_start_i,
                    study_end_i,no_pat_i,poc_i,source_i,country_i) {
  
  # if (sum(site_i%in%"Overall"==T)>=1) {
  #   
  #   site_i <- unique(df$cancer)
  #   
  # }
  # 
  # if (sum(poc_i%in%"Overall"==T)>=1) {
  #   
  #   poc_i <- unique(df$poc)
  #   
  # }
  # 
  # if (sum(source_i%in%"Overall"==T)>=1) {
  #   
  #   source_i <- unique(df$source)
  #   
  # }
  # 
  # if (sum(no_pat_i%in%"Unrestricted"==T)>=1) {
  #   
  #   no_pat_i <- unique(df$no_pat)
  #   
  # }
  
  if (length(pub_year_i)>1) {
    
    pub_year_i <- seq(pub_year_i[1],pub_year_i[2])
    
  }
  
  if (length(study_start_i)>1) {
    
    study_start_i <- seq(study_start_i[1],study_start_i[2])
    
  }
  
  if (length(study_end_i)>1) {
    
    study_end_i <- seq(study_end_i[1],study_end_i[2])
    
  }
  
  load <- df %>% 
    filter(cancer_group%in%site_i,
           pub_year%in%pub_year_i,
           study_start%in%study_start_i,
           study_end%in%study_end_i,
           no_pat%in%no_pat_i,
           poc%in%poc_i,
           source%in%source_i,
           country%in%country_i
    ) %>% 
    mutate(cite=paste0(author," ",pub_year)) 
  
  
  parent <- load %>%
    dplyr::select(cov_id,inequality,cite,color) %>% 
    distinct() %>% 
    filter(!is.na(inequality)) %>%  # IMPORTANT
    dplyr::select(inequality,color) %>%
    count(inequality,color,name="count") 
  
  child1_list <- list()
  
  for (i in 1:nrow(parent)) {
    
    studies <- load %>% 
      filter(inequality==parent$inequality[i]) %>% 
      dplyr::select(cite) %>% 
      distinct() %>% 
      mutate(count=1)
    
    child1_list[[i]] <- tibble(name=studies$cite,
                               value=studies$count)
    
  }
  
  tree <- tibble(
    name = parent$inequality,
    value = parent$count,
    itemStyle = tibble(color=parent$color),
    children = child1_list
  )
  
  tree %>%
    e_charts() %>%
    e_treemap(
      leafDepth=1,
      nodeClick=T,
      drillDownIcon=F,
      label=list(fontSize=14,
                 fontWeight="bold",
                 fontFamily="Arial",
                 show=T),
      itemStyle=list(
        # borderWidth=2,
        # gapWidth=0,
        borderColor="#fff"
      ),
      upperLabel=list(show=T),
      levels=c(
        list(itemStyle=list(borderColor="#777",
                            borderWidth=0,
                            gapWidth=1)
        ),
        list(itemStyle=list(borderColor="#555",
                            borderWidth=5,
                            gapWidth=1))
      )) %>%
    e_tooltip(show=T) 
  
  
}

timeFUN <- function(df,site_i,pub_year_i,study_start_i,
                    study_end_i,no_pat_i,poc_i,source_i,country_i,inequality_select) {
  
  if (length(pub_year_i)>1) {
    
    pub_year_i <- seq(pub_year_i[1],pub_year_i[2])
    
  }
  
  if (length(study_start_i)>1) {
    
    study_start_i <- seq(study_start_i[1],study_start_i[2])
    
  }
  
  if (length(study_end_i)>1) {
    
    study_end_i <- seq(study_end_i[1],study_end_i[2])
    
  }
  
  load <- df %>% 
    filter(cancer_group%in%site_i,
           pub_year%in%pub_year_i,
           study_start%in%study_start_i,
           study_end%in%study_end_i,
           no_pat%in%no_pat_i,
           poc%in%poc_i,
           source%in%source_i,
           country%in%country_i) %>% 
    rename(date=pub_year) %>%  
    dplyr::select(cov_id,date,inequality,color) %>% 
    distinct() %>% 
    count(date,inequality,color,name="count") %>% 
    mutate(date=as.Date(paste0(date,"-01-01"))) %>% 
    filter(!is.na(inequality)) %>% 
    filter(inequality%in%inequality_select)
  
  for_col <- load %>% 
    dplyr::select(inequality,color) %>% 
    distinct() %>% 
    arrange(inequality)
  
  load %>% 
    group_by(inequality) %>% 
    e_charts(date) %>% 
    e_grid(top=90) %>% 
    e_line(count,
           symbolSize=7) %>% 
    e_tooltip(show=T) %>% 
    e_x_axis(name="Publication Year",
             nameLocation="middle",
             nameGap=40,
             nameTextStyle=list(fontWeight="bold")) %>% 
    e_y_axis(name="# Studies",
             nameGap=20,
             nameTextStyle=list(fontWeight="bold")) %>% 
    e_color(color=for_col$color) %>% 
    e_legend(textStyle=list(fontSize=13))
  
  
}

mapFUN <- function(df,df_geo,site_i,pub_year_i,study_start_i,
                   study_end_i,no_pat_i,poc_i,source_i,country_i,inequality_select) {
  
  if (length(pub_year_i)>1) {
    
    pub_year_i <- seq(pub_year_i[1],pub_year_i[2])
    
  }
  
  if (length(study_start_i)>1) {
    
    study_start_i <- seq(study_start_i[1],study_start_i[2])
    
  }
  
  if (length(study_end_i)>1) {
    
    study_end_i <- seq(study_end_i[1],study_end_i[2])
    
  }
  
  load <- df %>% 
    filter(cancer_group%in%site_i,
           pub_year%in%pub_year_i,
           study_start%in%study_start_i,
           study_end%in%study_end_i,
           no_pat%in%no_pat_i,
           poc%in%poc_i,
           source%in%source_i,
           country%in%country_i) %>% 
    dplyr::select(cov_id,country,inequality) %>% 
    distinct() %>% 
    count(country,inequality,name="count") %>% 
    filter(!is.na(country),
           !is.na(inequality))
  
  load_geo <- df_geo %>% 
    rename(country=CTRY22NM) %>% 
    left_join(.,load,by="country") %>% 
    filter(inequality==inequality_select) %>% 
    mutate(hover=paste0(country," - ",count," studies"))
  
  tmap_mode("view")
  
  tm_shape(load_geo) +
    tm_polygons("count",
                palette=c("#f9eaf3","#BB0071"),
                id="hover")
  
}

tableFUN <- function(df,site_i,pub_year_i,study_start_i,
                     study_end_i,no_pat_i,poc_i,source_i,country_i,
                     type,inequality_select) {
  
  # if (sum(site_i%in%"Overall"==T)>=1) {
  #   
  #   site_i <- unique(df$cancer)
  #   
  # }
  # 
  # if (sum(poc_i%in%"Overall"==T)>=1) {
  #   
  #   poc_i <- unique(df$poc)
  #   
  # }
  # 
  # if (sum(source_i%in%"Overall"==T)>=1) {
  #   
  #   source_i <- unique(df$source)
  #   
  # }
  # 
  # if (sum(no_pat_i%in%"Unrestricted"==T)>=1) {
  #   
  #   no_pat_i <- unique(df$no_pat)
  #   
  # }
  
  if (length(pub_year_i)>1) {
    
    pub_year_i <- seq(pub_year_i[1],pub_year_i[2])
    
  }
  
  if (length(study_start_i)>1) {
    
    study_start_i <- seq(study_start_i[1],study_start_i[2])
    
  }
  
  if (length(study_end_i)>1) {
    
    study_end_i <- seq(study_end_i[1],study_end_i[2])
    
  }
  
  load <- df %>% 
    filter(cancer_group%in%site_i,
           pub_year%in%pub_year_i,
           study_start%in%study_start_i,
           study_end%in%study_end_i,
           no_pat%in%no_pat_i,
           poc%in%poc_i,
           source%in%source_i,
           country%in%country_i
    ) %>% 
    mutate(cite=paste0(author," ",pub_year)) %>% 
    filter(inequality%in%inequality_select)
  
  if (type=="table") {
    
    pre_table1 <- load %>% 
      dplyr::select(cov_id,cite,title,journal,cancer_type,inequality,
                    inequality_type,summary,
                    study_start,study_end,
                    no_pat_org) %>% 
      distinct() 
    
    pre_table2 <- load %>% # need to join all countries in one row to avoid duplicate rows
      dplyr::select(cov_id,country) %>% 
      group_by(cov_id) %>%
      mutate(country=toString(unique(country)),
             country=case_when(country=="NA"~NA,
                               T~country)) %>% 
      ungroup() %>% 
      distinct()
    
    table <- pre_table1 %>% 
      left_join(.,pre_table2,by="cov_id") %>% 
      dplyr::select(-cov_id)
    
    reactable(table[1:5],
              height=500,
              searchable=T,
              pagination=F,
              bordered=T,
              style = list(fontFamily = 'Arial'),
              defaultColDef = colDef(format = colFormat(separators = F),
                                     align="center"),
              columns=list(
                cite=colDef(name="Study"),
                title=colDef(name="Title"),
                journal=colDef(name="Journal"),
                cancer_type=colDef(name="Cancers"),
                inequality=colDef(name="Inequality")
                # cov_id=colDef(name="Covidence ID"),
                # inequality=colDef(name="Inequality")
              ),
              details=function(index) {
                nest_table <- table %>%
                  slice(index) %>% 
                  dplyr::select(-c(cite,title,journal,inequality,
                                   cancer_type))
                htmltools::div(style = "padding: 1rem",
                               reactable(nest_table,
                                         defaultColDef = colDef(format = colFormat(separators = F),
                                                                align="center"),
                                         columns=list(
                                           summary=colDef(name="Summary"),
                                           study_start=colDef(name="Study Start"),
                                           study_end=colDef(name="Study End"),
                                           no_pat_org=colDef(name="Sample Size"),
                                           country=colDef(name="Country"),
                                           inequality_type=colDef(name="Measures")
                                         ))
                )
              }
    )
    
  } else if (type=="download") {
    
    load %>% 
      dplyr::select(cite,title,journal,cancer_type,
                    inequality_type,summary,
                    study_start,study_end,
                    no_pat_org,country) %>% 
      distinct() 
    
  }
  
}





