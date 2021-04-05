# FUNCTIONS V2: Project Data Mining 2021


library(knitr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(mapsf)



tab_var <- function(data, codecom, list_mod, var, ...)
{
  tab <- data %>% 
    mutate(COM = substr(IRIS,1,5)) %>%
    filter(LPRM_1 %in% list_mod & COM == codecom) %>%
    group_by(...) %>% 
    count({{ var }}, wt=IPONDI) %>%  
    mutate(n=round(n,0), pct=prop.table(n)*100, pct=round(pct, 1))
  
  return(tab)
}


graph_baton1 <- function(data, codecom, list_mod, var, ..., var_x, var_y, nom_titre)
{
  tabvar <- tab_var(data = data, codecom = codecom, list_mod, var = {{ var }}, ...)
  
  graph <- tabvar %>%
    ggplot() + aes(x = {{ var_x }}, y = {{ var_y }}, fill = {{ var_x }}) + 
    geom_bar(stat="identity") + scale_fill_brewer(palette = "Set2") +
    geom_text(aes(y = {{ var_y }}, label = {{ var_y }}), vjust=1.4, color="gray30", size=3.5) + 
    labs(title = nom_titre, x="", y="") +  
    theme(legend.position = "none", axis.text.x = element_text(angle = 75, hjust=1))
  
  return(graph)
}



graph_baton3 <- function(data, codecom, list_mod, var, ..., var_x, var_y, var_z, nom_titre)
{
  tabvar <- tab_var(data = data, codecom = codecom, list_mod, var = {{ var }}, ...)
  
  graph <- tabvar %>%
    ggplot() + aes(x = {{ var_x }}, y = {{ var_y }}, fill = {{ var_z }}) + 
    geom_bar(stat="identity") + scale_fill_brewer(palette = "Set2") +
    labs(title = nom_titre, x="", y="") 
  
  return(graph)
}



mean_var <- function(data, codecom,list_mod, var, ..., nom_var)
{
  var_mean <- data %>% 
    mutate(COM = substr(IRIS,1,5)) %>%
    filter(LPRM_1 %in% list_mod & COM == codecom) %>%
    group_by(...) %>%
    summarise({{ nom_var }} := weighted.mean({{ var }}, IPONDI, na.rm=T))
  
  return(var_mean)
}


sum_var <- function(data, codecom,list_mod, ..., nom_var)
{
  var_sum <- data %>% 
    mutate(COM = substr(IRIS,1,5)) %>%
    filter(LPRM_1 %in% list_mod & COM == codecom) %>%
    group_by(...) %>%
    summarise({{ nom_var }} := sum(IPONDI))
  
  return(var_sum)
}

# ----------------------------------


tab_cont_iris <- function(data, map, codecom, list_mod, var)
{
  # Create tab
  tab <-tab_var(data,codecom, list_mod, {{var}}, IRIS)
  
  # Pivot count
  tab_raw <- pivot_wider(data = tab %>% select(-pct),
                         names_from = {{var}},
                         values_from = n,
                         values_fill = 0)
  names(tab_raw)[-1] <- paste("count_",names(tab_raw)[-1], sep="")
  
  # Pivot pct                
  tab_pct <- pivot_wider(data = tab %>% select(-n),
                         names_from = {{var}},
                         values_from = pct,
                         values_fill = 0)                      
  names(tab_pct)[-1] <- paste("pct_",names(tab_pct)[-1], sep="") 
  
  # join with geom
  tab_geo <- left_join(tab_raw, tab_pct) %>% 
    rename(CODE_IRIS=IRIS) %>%
    left_join(map) %>%
    st_as_sf()
  return(tab_geo)
}

# ----------------------------------

map_count_iris <- function (data,
                            map,
                            codecom,
                            list_mod,
                            var,
                            mod,
                            titre)
  
{
  
  mymap<-tab_cont_iris(data,map_iris,codecom,list_mod,{{var}})
  
  myvar <- paste("count_",mod, sep="")
  
  if (list_mod==0) {myleg = "nb. d'habitants"} else
  {myleg = "nb de ménages"}
  
  
  mf_map(x= mymap,
         type = "base",
         col="lightyellow",
         border="gray80",lwd=0.4)
  
  mf_map(x = mymap,
         var = myvar,
         type = "prop",
         col="blue",
         leg_title = myleg,
         inches = 0.06)
  
  mf_layout(title = titre, 
            frame = TRUE,
            credits ="INSEE, RP 2017, fichiers détail")
}


#-----------------------

map_pct_iris <- function (  data,
                            map,
                            codecom,
                            list_mod,
                            var,
                            mod,
                            titre)
  
{
  
  mymap<-tab_cont_iris(data,map, codecom,list_mod,{{var}})
  
  myvar <- paste("pct_",mod, sep="")
  
  if (list_mod==0) {myleg = "% des habitants"} else
  {myleg = "% des ménages"}
  
  
  
  huntsberger <- function(x) {round(1+(10/3)*log10(x))}
  nbc<-huntsberger(nrow(mymap))
  
  
  
  mf_map(x = mymap,
         var = myvar,
         type = "choro",
         nbreaks = nbc,
         leg_title = myleg)
  
  mf_layout(title = titre, 
            frame = TRUE,
            credits ="INSEE, RP 2017, fichiers détail")
  
}