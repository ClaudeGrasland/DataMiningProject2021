# Fonctions de transformation des tableaux de contingence sociologiques et géographiques

library(sf)
library(dplyr)
library(knitr)


#### SELECTION ####

#### géographique
base_select_geo <- function(base = base,
                            var = i2,
                            sel = c("Geo1","Geo2","Geo3")) {
  b<-base
  b$geo<- b$geo %>% filter({{var}} %in% sel)
  b$tab<- b$tab %>% filter(i %in% b$geo$i)
  b$soc <- b$soc %>% filter(j %in% unique(b$tab$j))
  return(b)
}
  
#### sociologique

base_select_soc <- function(base = base,
                              var = j2,
                              sel = c("Cat1","Cat2","Cat3")) {
    b<-base
    b$soc<- b$soc %>% filter({{var}} %in% sel)
    b$tab<- b$tab %>% filter(j %in% b$soc$j)
    b$geo <- b$geo %>% filter(i %in% unique(b$tab$i))
    return(b)
}

#### AGREGATION ####

#### géographique

base_agreg_geo <- function(base = base,
                           var = i2,
                           lab = lab_i2) {
  b<-base
  key <- b$geo %>% select(i, {{var}})
  
  b$tab <-b$tab %>% left_join(key) %>% 
    mutate(i = as.factor({{var}}))%>%
    group_by(i,j) %>%
    summarize(Xij = sum(Xij)) %>%
    select(i,j,Xij)
  
  
  
  b$geo <- b$geo %>% st_transform(crs = 2154) %>%
    mutate(i = {{var}}, lab_i = {{lab}}) %>%
    group_by(i) %>%
    summarize(lab_i = unique(lab_i),
              lat = mean(lat),
              lng=mean(lng)) %>%
    ungroup() %>%
    st_transform(crs = 4326)%>%
    st_as_sf()
  
  return(b)
  
}

#### sociologique
base_agreg_soc <- function(base = base,
                           var = j2,
                           lab = lab_j2) {
  b<-base
  key <- b$soc %>% select(j, {{var}})
  
  b$tab <-b$tab %>% left_join(key) %>% 
    group_by(i,{{var}}) %>%
    mutate(i =i, j= as.factor({{var}}),Xij = sum(Xij)) %>%
    ungroup()%>%
    select(i,j,Xij)
  
  
  b$soc <- b$soc %>% mutate(j = {{var}}, lab_j = {{lab}}) %>%
    select(j,lab_j)
  
  
  return(b)
  
}

#### NORMALISATION ####

base_norm_geo <- function(base = base) {
  b <- base
  tab_i <- b$tab %>% group_by(i) %>%
    summarize(Xi = sum(Xij))
  b$tab <-b$tab %>% left_join(tab_i) %>% mutate(Pj_i = Xij/Xi)
  return(b)
}

#### géographique 
base_norm_soc <- function(base = base) {
  b <- base
  tab_j <- b$tab %>% group_by(j) %>%
    summarize(Xj = sum(Xij))
  b$tab <-b$tab %>% left_join(tab_j) %>% mutate(Pi_j = Xij/Xj)
  return(b)
}

#### sociologique
base_norm_stat <- function(base=base) {
  b<-base
  b<-base_norm_geo(b)
  b<-base_norm_soc(b)
  b$tab<-b$tab %>% mutate(Eij = Xi * Xj/sum(Xj),
                          Rij = Xij-Eij,
                          K2ij = (Rij**2)/Eij)
  return(b)
}
