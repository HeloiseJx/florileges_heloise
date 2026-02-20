###                   ###
# REFERENTIELS FLORILEGES
###                   ###


# IMPORT DATA ----

### Biogeoregions ----
invisible(capture.output({biogeoregions = st_read("maps/region_biogeo_fr/region_biogeographique.shp") %>%
  st_transform(crs = 4326) # passage du lambert 93 au WGS 84
}))

# certains sommets dupliqués : on corrige les erreurs
invalid_index <- which(!st_is_valid(biogeoregions))
biogeoregions[invalid_index, ] <- st_make_valid(biogeoregions[invalid_index, ])


invisible(capture.output({coord_epci <- st_read(here::here("maps/ADMIN-EXPRESS_3-2__SHP_WGS84G_FRA_2024-12-18/ADMIN-EXPRESS_3-2__SHP_WGS84G_FRA_2024-12-18/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-12-00243/ADE_3-2_SHP_WGS84G_FRA-ED2024-12-18/EPCI.shp"), quiet = TRUE)
}))

metro = coord_epci %>% filter(NATURE == "Métropole")

# choix car grosse participation en IdF --> découpage de la Metropole du grand dparis pour avoir un contexte très urbain
gd_paris = metro %>% 
  filter(NOM == "Métropole du Grand Paris") %>%
  rename(CODE = NATURE, N_DOMAINE = NOM) %>%
  select(CODE, N_DOMAINE)


biogeoreg_avec_paris =  st_difference(biogeoregions, gd_paris) %>%
  select(CODE, N_DOMAINE, geometry) %>%
  bind_rows(gd_paris)

## biogeoregions modifiées ----
dt_florileges %>%
  st_join(biogeoreg_avec_paris) %>%
  ggplot(aes(x = N_DOMAINE, fill = N_DOMAINE)) +
  geom_histogram(stat = "count", width = 0.5) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


### fonctions calcul référentiel ----
source(here::here("programs","fonctions_stat.R"))


### data paysage local pour un buffer de 1000m ----
dt_protoc_oso = readRDS("data/dt_florileges_oso_calcul_1000.rds")






# CALCULS DES REFERENTIELS ----

## SESSIONS ----

df_flori_ref_session = dt_protoc_oso %>%
  mutate(paysage_binaire = case_when(im > 0.6 ~ "urbain+",
                                     TRUE ~ "urbain-")) %>%
  st_join(biogeoreg_avec_paris) %>%
  select(session_id, paysage_binaire, N_DOMAINE)


### arbre de décision ----
dt_combi_ref = df_flori_ref_session %>%
  as.data.frame() %>%
  select(-geometry) %>%
  mutate(combi_CS_R = paste0(paysage_binaire, "---", N_DOMAINE)) %>%
  group_by(combi_CS_R) %>%
  mutate(n_CS_R = n_distinct(session_id)) %>%
  ungroup() %>%
  group_by(paysage_binaire) %>%
  mutate(n_CS = n_distinct(session_id)) %>%
  ungroup() %>%
  mutate(ref_choisi = case_when(n_CS_R > 49 ~ combi_CS_R,
                                TRUE ~ paysage_binaire),
         n_ref_choisi = case_when(n_CS_R > 49 ~ n_CS_R,
                                  TRUE ~ n_CS),
         n_critere = case_when(n_CS_R > 49 ~ 2,
                               TRUE ~ 1)) %>%
  group_by(ref_choisi, N_DOMAINE) %>%
  mutate(n_session_concernees = n_distinct(session_id)) %>%
  ungroup() 


## SITES ----

### gestion des sites dupliques ----

dt_site = dt_florileges %>%
  st_join(biogeoreg_avec_paris) %>%
  as.data.frame() %>%
  select(session_id, N_DOMAINE) %>%
  left_join(dt_protoc_oso)  %>%
  select(session_id, N_DOMAINE, paysage_local) %>% 
  left_join(dt_florileges_tot) %>%
  select(site_id, site, session_id, N_DOMAINE, paysage_local) %>%
  unique()

# SITES DUPLIQUES
temp =  dt_site %>% 
  select(site_id, site) %>% 
  group_by(site) %>% 
  mutate(nb_site_id = n_distinct(site_id)) %>% 
  ungroup() %>%
  unique() %>% 
  filter(nb_site_id == 2) %>% 
  left_join(dt_florileges_tot)

sites_dupliques = dt_protoc_oso %>% 
  select(session_id, geometry) %>% 
  right_join(temp) %>% 
  select(site_id, site, session_date, geometry) %>% 
  unique() %>% 
  arrange(site)

## on garde le dernier site_id et le dernier emplacement associé à un site 
sites_plus_dupliques <- sites_dupliques %>%
  group_by(site) %>%
  slice_max(order_by = session_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-session_date)

# SITES UNIQUES
temp1 =  dt_site %>% 
  select(site_id, site) %>% 
  group_by(site) %>% 
  mutate(nb_site_id = n_distinct(site_id)) %>% 
  ungroup() %>%
  unique() %>% 
  filter(nb_site_id == 1) %>% 
  select(site_id, site) %>% 
  left_join(dt_florileges_tot)

sites_uniques = dt_protoc_oso %>% 
  select(session_id, geometry) %>% 
  right_join(temp1) %>% 
  select(site_id, site, geometry) %>% 
  unique() %>% 
  arrange(site) %>%
  filter(!st_is_empty(geometry))

sites_clean = rbind(sites_uniques, sites_plus_dupliques)

# contient des infos pour lesquelles les coordonnées sont vides  
dt_florileges_tot_sites_clean = dt_florileges_tot %>%
  select(-site_id) %>% 
  left_join(sites_clean) %>%
  select(-geometry)

# on n'a pas fait de filtre sur les sessions pour lesquelles les coordonnées sont vides --> perte d'info trop grande
dt_florileges_sites_clean = dt_florileges_tot %>%
  select(session_id, session_date, structure_id, structure_nom, site, protocole) %>%
  unique() %>% 
  left_join(sites_clean) %>%
  rename(date = session_date, user_id = structure_id, user_pseudo = structure_nom) %>% 
  unique()  %>%
  filter(!st_is_empty(geometry))


### arbre de decision ----

dt_ref_site = dt_florileges_sites_clean %>%
  st_as_sf(crs = 4326) %>%
  st_join(biogeoreg_avec_paris) %>%
  as.data.frame() %>%
  select(session_id, N_DOMAINE) %>%
  left_join(dt_protoc_oso)  %>%
  mutate(paysage_binaire = case_when(im > 0.6 ~ "urbain+",
                                     TRUE ~ "urbain-")) %>%
  select(session_id, N_DOMAINE, paysage_binaire) %>% 
  left_join(dt_florileges_tot) %>%
  select(site_id, site, session_id, N_DOMAINE, paysage_binaire) %>%
  unique() %>%
  mutate(combi_CS_R = paste0(paysage_binaire, "---", N_DOMAINE)) %>%
  group_by(combi_CS_R) %>%
  mutate(n_CS_R = n_distinct(site_id)) %>%
  ungroup() %>%
  group_by(paysage_binaire) %>%
  mutate(n_CS = n_distinct(site_id)) %>%
  ungroup() %>%
  mutate(ref_choisi = case_when(n_CS_R > 49 ~ combi_CS_R,
                                TRUE ~ paysage_binaire),
         n_ref_choisi = case_when(n_CS_R > 49 ~ n_CS_R,
                                  TRUE ~ n_CS),
         n_critere = case_when(n_CS_R > 49 ~ 2,
                               TRUE ~ 1)) %>%
  group_by(ref_choisi, N_DOMAINE) %>%
  mutate(n_session_concernees = n_distinct(site_id)) %>%
  ungroup() 


## STRUCTURE ----

### buffer ----
func_stat_structures = function(dt_protocole, dt_protoc_tot){
  
  # calcul de la continuité pour chaque structure : quel est le maximum de nombre d'années qu'un site a été suivi pour chaque structure 
  df_continu = dt_protoc_tot %>%
    mutate(an = format(as.Date(session_date), "%Y")) %>%
    select(structure_id, site_id, session_id, an) %>%
    unique() %>% 
    group_by(structure_id, site_id, an) %>%
    reframe(nb_passage = n_distinct(session_id)) %>%
    ungroup() %>%
    # filter(nb_passage >= 3 & nb_passage < 15) %>% #(filtre grossier haut pour enlever les anomalies)
    group_by(structure_id, site_id) %>%
    reframe(nb_an_suivi = n_distinct(an)) %>%
    ungroup() %>%
    rename(user_id = structure_id) %>%
    unique()
  
  
  # calcul d'une coordonnée par structure
  df_oso = dt_protoc_oso %>%
    as.data.frame() %>%
    mutate(paysage_binaire = case_when(im > 0.6 ~ "urbain+",
                                       TRUE ~ "urbain-")) %>% 
    select(session_id, user_id, paysage_binaire) %>%
    unique()
  
  dt_centroide_structure = dt_protocole %>%
    group_by(user_id) %>%
    summarize(centroide = st_centroid(st_union(geometry))) %>%  # calcule le centroïde de cette géométrie)
    ungroup() #%>%
  # left_join(df_oso) 
  
  # boucle : calcul des structures dans un buffer de 300 km 
  dt_stat_struct_buffer = data.frame()
  
  for (structure in unique(dt_centroide_structure$user_id)) {
    
    zone_buffer <- dt_centroide_structure %>%
      filter(user_id == structure) %>%
      pull(centroide) %>%
      st_buffer(dist = 200000) # fabrique un cercle de 200 km de rayon autour du point
    
    # je récupère une coordonnée par structure = centroide des points
    struct_proches <- dt_centroide_structure %>%
      filter(user_id != structure) %>%
      st_intersection(zone_buffer) %>% # je récupère les points qui sont dans le buffer = structures dans le buffer
      left_join(df_oso)
    
    df_conti_buffer = df_continu %>%
      filter(user_id %in% unique(struct_proches$user_id))
    
    list_struct_proches = c(unique(struct_proches$user_id))
    
    
    dt_stat = tibble(user_id = structure, # structure concernée
                     liste_struct_proches = list(list_struct_proches), # liste des structures dans un buffer de 200km
                     nb_struct = n_distinct(list_struct_proches), # nombre de structures dans le buffer 
                     nb_sessions_buffer = n_distinct(struct_proches$session_id), # nombre de sessions réalisées dans le buffer (exclut les sessions de la structure concernée)
                     nb_sess_urbainplus_buffer = sum(struct_proches$paysage_binaire == "urbain+", na.rm = TRUE), # nombre de sessions dans un paysage très urbain pour les autres structures 
                     # nombre de sites dans le buffer
                     nb_sites_buffer = n_distinct(df_conti_buffer$site_id),
                     # maximum de continuité dans le buffer
                     max_conti_site_buffer = unique(max(df_conti_buffer$nb_an_suivi, na.rm = TRUE))
    ) %>% 
      # on récupère les mêmes infos pour ala structure concernée
      left_join(df_oso) %>%
      mutate(nb_sess_user_id = n_distinct(session_id), nb_sess_urbainplus_user_id = sum(paysage_binaire == "urbain+", na.rm = TRUE),
             nb_site_struct = sum(df_continu$user_id == structure, na.rm = TRUE),
             max_conti_site_struct = max(df_continu$nb_an_suivi[df_continu$user_id == structure], na.rm = TRUE)) %>%
      select(-session_id, -paysage_binaire) %>%
      unique()
    
    dt_stat_struct_buffer = dt_stat_struct_buffer %>%
      bind_rows(dt_stat)
  }
  
  
  dt_stat_struct_buffer = dt_stat_struct_buffer %>% 
    mutate(
      # pourcentage de sessions dans "urbain+" dans le buffer
      pourc_urbainplus_buffer = round(100 * nb_sess_urbainplus_buffer / nb_sessions_buffer),
      # pourcentage de sessions dans "urbain+" en local
      pourc_urbainplus_struct = round(100 * nb_sess_urbainplus_user_id / nb_sess_user_id),
      # moyenne du nombre de sessions par structure 
      moy_nb_sess_par_user_buffer = round(nb_sessions_buffer / nb_struct),
      # poids de la structure dans le buffer
      poids_struct_buffer = round(100 * nb_sess_user_id / (nb_sessions_buffer + nb_sess_user_id))) %>%
    # reordonnéer les colonnes
    select(user_id, pourc_urbainplus_buffer, pourc_urbainplus_struct,   poids_struct_buffer, nb_struct, moy_nb_sess_par_user_buffer, nb_sessions_buffer, nb_sess_user_id, nb_sess_urbainplus_buffer, nb_sess_urbainplus_user_id,max_conti_site_struct, max_conti_site_buffer, nb_site_struct,  liste_struct_proches )
  
  return(dt_stat_struct_buffer)
  
}

# dt_stat_struct_buffer = func_stat_structures(dt_florileges, dt_florileges_tot)


