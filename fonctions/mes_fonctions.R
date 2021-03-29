# FUNCTIONS : Project Data Mining 2021


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

