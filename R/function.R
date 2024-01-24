
treeFUN <- function(df,site_i,year_i,poc_i,source_i,
                    type) {
  
  if (sum(site_i%in%"Overall"==T)>=1) {
    
    site_i <- unique(df$cancer)
    
  }
  
  if (sum(poc_i%in%"Overall"==T)>=1) {
    
    poc_i <- unique(df$poc)
    
  }
  
  if (sum(source_i%in%"Overall"==T)>=1) {
    
    source_i <- unique(df$source)
    
  }
  
  if (length(year_i)>1) {
    
    year_i <- seq(year_i[1],year_i[2])
    
  }
  
  load <- df %>% 
    filter(cancer%in%site_i,
           year%in%year_i,
           poc%in%poc_i,
           source%in%source_i
    ) %>% 
    mutate(title=paste0(author," (",year,")")) %>% 
    dplyr::select(id,inequality,title) %>% 
    distinct() %>% 
    filter(!is.na(inequality)) # IMPORTANT
  
  if (type=="plot") {
    
    parent <- load %>%
      dplyr::select(inequality) %>%
      count(inequality,name="count") 
    
    
    child1_list <- list()
    
    for (i in 1:nrow(parent)) {
      
      studies <- load %>% 
        filter(inequality==parent$inequality[i]) %>% 
        dplyr::select(title) %>% 
        distinct() %>% 
        mutate(count=1)
      
      child1_list[[i]] <- tibble(name=studies$title,
                                 value=studies$count)
      
    }
    
    tree <- tibble(
      name = parent$inequality,
      value = parent$count,
      children = child1_list
    )
    
    tree %>%
      e_charts() %>%
      e_treemap(
        leafDepth=1,
        label=list(fontSize=14,
                   fontWeight="bold",
                   fontFamily="Arial",
                   show=T),
        itemStyle=list(
          # borderWidth=0,
          # gapWidth=1,
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
      e_tooltip(show=T) %>% 
      e_color(c("#2e008b",
                "#e40074",
                "#00b6ed",
                "#BB0071",
                "#005870",
                "#710044",
                "#2e008b",
                "#e40074"))
    
    # c("#2e008b",
    #   "#e40074",
    #   "#BB0071",
    #   "#710044",
    #   "#00b6ed",
    #   "#005870",
    #   "#575757",
    #   "#2e2d2c")
    
  } else if (type=="table") {
    
    load %>% 
      dplyr::select(title,id,inequality) %>% 
      reactable(.,
                height=400,
                pagination=F,
                bordered=T,
                style = list(fontFamily = 'Arial'),
                defaultColDef = colDef(format = colFormat(separators = F),
                                       align="center"),
                columns=list(
                  title=colDef(name="Study Author (year)"),
                  id=colDef(name="Covidence ID"),
                  inequality=colDef(name="Inequality")
                ))
    
  } else if (type=="download") {
    
    load %>% 
      dplyr::select(title,id,inequality)
    
  }
  
  
}

