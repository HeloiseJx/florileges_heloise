
# construction df_florileges : données nationales ----
source("programs/requete_florileges.R")

df_florileges = df_florileges %>%
  filter(session_date >= "2000-01-01") %>%
  mutate(across(starts_with("taxon_count_"), as.integer)) %>%
  mutate(session_year = as.numeric(year(as.Date(session_date))),
         session_year_factor_all = factor(session_year, levels = as.character(min(session_year, na.rm = T):max(session_year, na.rm = T))),
         taxon_presence = rowSums(select(., starts_with("taxon_count_Q")), na.rm=TRUE), # compter le nb d'occurence des sp
         taxon_presence_all = rowSums(select(., starts_with("taxon_count_")), na.rm=TRUE), # considération sp hors quadrat
         taxon_combine = if_else(is.na(taxon_nom_scientifique), taxon, taxon_nom_scientifique)) %>%
  filter(taxon_combine != "null")# %>% 
  # filter(taxon_presence > 0) # je ne filtre pas les sessions où il n'y a pas de plantes de la liste relevées --> même si en pratique il n'y a pas de session dans une plante de la liste

lst_florileges = sort(unique(df_florileges$taxon_nom_scientifique))

if (!exists("structure_name")) {
  structure_name = "EPT Plaine Commune"
}

df_florileges_struct = df_florileges %>%
  mutate(structure_nom = case_when(structure_nom == "Plaine commune - Grand Paris"~ "EPT Plaine Commune", 
                                   TRUE ~ structure_nom)) %>%
  filter(structure_nom == structure_name) %>%
  mutate(session_year_factor = factor(session_year, levels = as.character(min(session_year, na.rm = T):max(session_year, na.rm = T))))




# lecture BD couche admin ----
invisible(capture.output({coord_epci <- st_read(here::here("maps/ADMIN-EXPRESS_3-2__SHP_WGS84G_FRA_2024-12-18/ADMIN-EXPRESS_3-2__SHP_WGS84G_FRA_2024-12-18/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2024-12-00243/ADE_3-2_SHP_WGS84G_FRA-ED2024-12-18/EPCI.shp"), quiet = TRUE) }))

# différents référentiels
# coord_plaineco = coord_epci %>% filter(NOM == "Plaine Commune") 
# coord_terreenv = coord_epci %>% filter(NOM == "Paris Terres d'Envol") 
# coord_metro_lyon = coord_epci %>% filter(NOM == "Métropole de Lyon") 
coord_grand_paris = coord_epci %>% filter(NOM == "Métropole du Grand Paris")




# biogeoregions ----
invisible(capture.output({biogeoregions = st_read("maps/region_biogeo_fr/region_biogeographique.shp") %>%
  st_transform(crs = 4326) # passage du lambert 93 au WGS 84
}))

# certains sommets dupliqués : on corrige les erreurs
invalid_index <- which(!st_is_valid(biogeoregions))
biogeoregions[invalid_index, ] <- st_make_valid(biogeoregions[invalid_index, ])


# paysage local ----
dt_protoc_oso = readRDS("data/dt_florileges_oso_calcul_1000.rds") %>%
  select(-user_id)


# df de référence --> métropole GP ----
func_df_ref = function() {

  df_flori_ref = df_florileges %>%
    filter(!is.na(site_geometry)) %>%
    unique() %>%
    mutate(geometry = st_as_sfc(site_geometry, crs = 4326)) %>%
    select(-site_geometry) %>%
    st_as_sf() 
  
  # on modifie dans la mesure du possible les geometries invalides
  invalid_index <- which(!st_is_valid(df_flori_ref))
  df_flori_ref[invalid_index, ] <- st_make_valid(df_flori_ref[invalid_index, ])
  
  # on supprime les autres
  df_flori_ref <- df_flori_ref[st_is_valid(df_flori_ref), ]
  
  # choix référentiel
  df_flori_ref =  st_join(df_flori_ref, coord_grand_paris)  %>%
    as.data.frame() %>%
    select(-geometry) %>%
    filter(!is.na(NOM))
  
  return(df_flori_ref)
}

df_flori_ref = func_df_ref()







#######################################################
############## Répartition des taxons vus #############
#######################################################

ordre_taxonomique = read.csv2("data/doc_heloise/TAXREF_v18_2025/TAXREF_Plantae.csv", sep = ";")

df_obs_taxon = df_florileges_struct %>%
  group_by(taxon_combine) %>%
  summarise(nobs = n()) %>%
  left_join(ordre_taxonomique, by = c("taxon_combine" = "LB_NOM")) %>%
  filter(!is.na(REGNE), REGNE == "Plantae") %>%
  mutate(TRIBU_SAVE = TRIBU,
         TRIBU = if_else((SOUS_FAMILLE == "" & FAMILLE != ""),
                         "EFFACE", TRIBU),
         SOUS_FAMILLE = if_else(TRIBU == "EFFACE", TRIBU_SAVE, SOUS_FAMILLE),
         TRIBU = if_else(TRIBU == "EFFACE", "", TRIBU)) %>%
  arrange(ORDRE)

#######################################################
####################### Richesse ######################
#######################################################

df_richesse = df_florileges_struct %>%
  filter(taxon_presence_all != 0) %>%
  group_by(session_year, session_year_factor) %>%
  summarise(richesse = length(unique(taxon_combine)), .groups = 'drop')
df_richesse_5 = df_florileges_struct %>%
  filter(taxon_presence > 5) %>%
  group_by(session_year, session_year_factor) %>%
  summarise(richesse = length(unique(taxon_combine)), .groups = 'drop')

df_richesse_site = df_florileges_struct %>%
  filter(taxon_presence_all != 0) %>%
  group_by(session_year, session_year_factor, site_id, site) %>%
  summarise(richesse = length(unique(taxon_combine)), .groups = 'drop') %>%
  group_by(session_year) %>%
  mutate(rich_tot = sum(richesse),
         rich_moy = mean(richesse)) %>%
  select(session_year, session_year_factor, rich_moy) %>%
  unique()

#######################################################
################# Traits écologiques ##################
#######################################################

df_trait = read.csv2("data/SpeciesTrait2024.csv")

df_radar_all = df_florileges %>%
  select(taxon_nom_scientifique, taxon_combine, structure_id, structure_nom,
         session_id, site, site_id) %>%
  mutate(loca = str_locate(site, "\\(")[,1],
         site = if_else(!is.na(loca), str_sub(site, 1, loca-1), site)) %>%
  left_join(df_trait, by = c("taxon_nom_scientifique" = "Plant"))

df_radar_struct = df_radar_all %>%
  filter(structure_nom == structure_name)

min_nitro = min(df_radar_all$Nitrophilie, na.rm = T)
max_nitro = max(df_radar_all$Nitrophilie, na.rm = T)

min_entomo = min(df_radar_all$BioticVector, na.rm = T)
max_entomo = max(df_radar_all$BioticVector, na.rm = T)

min_vivace = min(df_radar_all$Vivace, na.rm = T)
max_vivace = max(df_radar_all$Vivace, na.rm = T)



########################################################
################# Calculs Heloise ######################
########################################################

nb_sp_pas_liste = df_florileges_struct %>%
  filter(taxon_liste_florilege == 0 & taxon_count_hors_quadrat == 0) %>%
  mutate(nb_sp = n_distinct(taxon)) %>%
  pull(nb_sp) %>%
  unique()

# poids des données dans le référentiel
poids = round(100* n_distinct(df_florileges_struct$session_id) / n_distinct(df_flori_ref$session_id) , 1)



# Fréquence d'espèces

func_freq_sp = function(dt_flori) {
  nb_tot_quadrats = 10*n_distinct(dt_flori$session_id)
  
  df_freq = dt_flori %>%
    filter(taxon_liste_florilege == 1) %>%
    select(session_id, observation_id, taxon, taxon_nom_scientifique, taxon_count_Q1, taxon_count_Q2,taxon_count_Q3,taxon_count_Q4,
           taxon_count_Q5,taxon_count_Q6,taxon_count_Q7,taxon_count_Q8,
           taxon_count_Q9,taxon_count_Q10) %>%
    group_by(observation_id) %>%
    mutate(somme = sum(taxon_count_Q1, taxon_count_Q2,taxon_count_Q3,taxon_count_Q4,
                       taxon_count_Q5,taxon_count_Q6,taxon_count_Q7,taxon_count_Q8,
                       taxon_count_Q9,taxon_count_Q10)) %>%
    ungroup() %>%
    group_by(taxon, taxon_nom_scientifique) %>%
    reframe(freq_recouvrement = mean(somme),   # en moyenne lorsque le taxon est observé, *
            #est-il observé en simultané sur plusieurs quadrats ou plutôt de façon isolée ?
            freq_quadrats = 100 * sum(somme)/nb_tot_quadrats) %>% # fréquence en abondance --> sur le nombre de quadrats analysés localement, sur combien a t-on trouvé le taxon ?
    ungroup()
  
  return(df_freq)
}

dt_freq_local = func_freq_sp(df_florileges_struct) %>%
  mutate(echelle = "local") %>%
  arrange(desc(freq_quadrats)) %>%
  slice_head(n = 5)

dt_freq_ref = func_freq_sp(df_flori_ref) %>%
  mutate(echelle = "ref") %>%
  filter(taxon %in% dt_freq_local$taxon)


dt_freq_combi =
  dt_freq_local %>%
  bind_rows(dt_freq_ref) %>%
  ggplot(aes(x = taxon, y = freq_quadrats, fill = echelle)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  coord_flip() +
  theme_minimal() +
  ylab("% de quadrats dans lesquels l'espèce est présente") +
  xlab("espèces")

  
# nombre moyen d'années de participation au programme des sites
func_nb_moy_an = function(dt_flori) {
  dt_nb_moy_an = dt_flori %>%
    mutate(annee = year(as.Date(session_date))) %>%
    select(user_id, site_id, annee) %>%
    distinct() %>%
    group_by(site_id, user_id) %>%
    summarize(nb_an = n_distinct(annee)) %>%
    ungroup() 
  
  nb_moy_an = round(mean(dt_nb_moy_an$nb_an), 1)
  
  return(nb_moy_an)
}

nb_moy_an_local = func_nb_moy_an(df_florileges_struct)
nb_moy_an_ref = func_nb_moy_an(df_flori_ref)

# approche par structure ? 

# Nombre de sessions annuelles : 
plot_nb_ses_an = df_florileges_struct %>%
  mutate(annee = year(as.Date(session_date))) %>%
  select(annee, session_id) %>%
  distinct() %>%
  group_by(annee) %>%
  mutate(nb_ses_an = n_distinct(session_id)) %>%
  ungroup() %>%
  ggplot(aes(x = annee, y = nb_ses_an)) +
  geom_bar(stat = "identity", width = 0.6, fill = "orange") +
  scale_x_continuous(breaks = seq(min(1), max(10000), 1)) +
  theme_minimal() +
  ylab("nombre de sessions annuelles")




# dates de passage : 

plot_pheno_participation = df_flori_ref %>% 
  mutate(echelle = case_when(structure_nom == structure_name ~ "local", 
                             TRUE ~ "reference")) %>% 
  select(session_date, session_id, echelle) %>%
  distinct() %>% 
  mutate(mois = month(as.Date(session_date))) %>%
  group_by(echelle, mois) %>%
  summarize(somme = n_distinct(session_id)) %>%
  ungroup() %>%
  group_by(echelle) %>%
  mutate(pourcent = round(100 * somme / sum(somme), 1)) %>%
  ungroup() %>%
  select(echelle, mois, pourcent) %>%
  distinct() %>%
  ggplot(aes(x = mois, y = pourcent, fill = echelle)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7 ) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  ylab("pourcentage de sessions réalisées (%)") +
  theme_minimal()

# richesse cumulee
# Comme il s'agit d'une liste fermée, on se contente de la richesse cumulée chronologiquement

func_riches_cum_tt_protoc = function(dt_flori, titre = NULL) {
  # richesse spécifique en fonction du nombre de parties, avec la ligne de jauge
  #df qui nous donne pour chaque date le nombre de parties cumulées à ce jour localement
  df_nb_partie_cum = dt_flori %>%
    select(session_date, session_id) %>% # la date indique-t-elle bien les jours ?
    unique() %>%
    arrange(session_date) %>%
    mutate(nb_session = row_number()) %>%
    select(-session_id) %>%
    group_by(session_date) %>%
    summarize(nb_partie = max(nb_session)) %>% 
    ungroup() 
  
  # on enlève les taxons non validés  
  plot_richesse_cum_partie <- dt_flori %>%
    # calcul de la première date d'observation de l'espèce
    select(session_id, taxon, session_date) %>%
    distinct() %>%
    group_by(taxon) %>%
    summarize(first_obs = min(session_date)) %>% 
    ungroup() %>%
    na.omit() %>%
    rename(session_date = first_obs) %>%
    # jointure avec la table liant date et nombre cumulé de parties jouées 
    left_join(df_nb_partie_cum) %>% 
    # calcul de la somme cumulée du nombre d'espèces en fonction du nombre de parties 
    # jouées, en conaissant les first_obs des espèces
    group_by(nb_partie) %>%
    summarize(nb_sp = n_distinct(taxon)) %>%
    ungroup() %>%
    mutate(richesse_cum = cumsum(nb_sp)) %>%  
    # graphe
    ggplot(aes(x = nb_partie, y = richesse_cum, group = 1)) +
    geom_line(linewidth = 0.8) +
    labs(x = "nombre de sessions", 
         y = "richesse en espèces cumulée", 
         title = titre) +
    # # ligne du nombre d'espèces de la liste
    # geom_hline(yintercept = 30, linetype = "dashed", color = "purple", linewidth = 0.8) +
    # geom_hline(yintercept = 15, linetype = "dashed", color = "lightblue", linewidth = 0.8) +
    # annotate("text", x = 2, y = 15, label = "50% des espèces de\nla liste observées", hjust = 0, size = 3, color = "lightblue") +
    theme_minimal()
  
  return(plot_richesse_cum_partie)
  
}


func_site_nbsession_nbsp = function(dt_flori) {
  # Nombre de sessions par site
  nb_ses_site = dt_flori %>% 
    filter(taxon_liste_florilege == 1 & taxon_count_hors_quadrat == 0 ) %>%
    select(session_date, session_id, structure_id, site_id) %>% # la date indique-t-elle bien les jours ?
    unique() %>%
    group_by(structure_id, site_id) %>%
    reframe(nb_session = n_distinct(session_id)) %>%
    ungroup() 
  
  # nombre d'especes observées par site, faisant partie du protocole et observées dans les quadrats
  df_nb_plante_site = dt_flori %>% 
    filter(taxon_liste_florilege == 1 & taxon_count_hors_quadrat == 0 ) %>%
    group_by(site_id) %>%
    summarize(nb_sp = n_distinct(taxon)) %>%
    ungroup() %>%
    left_join(nb_ses_site)
  
  return(df_nb_plante_site)
  
}


# calcul du nombre moyen de sessions moyen réalisées dans l'aire de ref
func_moy_nb_session = function(dt_flori) {
  # moyenne du nombre de sessions qu'il faut pour
  dat = func_site_nbsession_nbsp(dt_flori)
  
  moy_nb_ses = round(mean(dat$nb_session), 1)
  
  return(moy_nb_ses)
}



# calcul du nombre d'espèces moyen observées sur les sites dans l'aire de référence
func_nb_moy_sp = function(dt_flori) {
  # moyenne du nombre de sessions qu'il faut pour
  dat = func_site_nbsession_nbsp(dt_flori)
  
  moy_nb_sp = round(mean(dat$nb_sp), 1)
  
  return(moy_nb_sp)
}






# richesse moyenne par participation : 
riches_moy_local = df_florileges_struct %>%
  filter(taxon_liste_florilege == 1) %>%
  mutate(taxon_present_binaire = case_when(taxon_presence == 0 ~ 0, 
                                           TRUE ~1)) %>%
  group_by(session_id) %>%
  reframe(nb_sp = sum(taxon_present_binaire)) %>%
  ungroup() %>%
  mutate(mean_nb_sp = mean(nb_sp)) %>%
  pull(mean_nb_sp) %>%
  unique()


riches_moy_ref = df_flori_ref %>%
  filter(taxon_liste_florilege == 1) %>%
  mutate(taxon_present_binaire = case_when(taxon_presence == 0 ~ 0, 
                                           TRUE ~1)) %>%
  group_by(session_id) %>%
  reframe(nb_sp = sum(taxon_present_binaire)) %>%
  ungroup() %>%
  mutate(mean_nb_sp = mean(nb_sp)) %>%
  pull(mean_nb_sp) %>%
  unique()




# 
# library(vegan)
# library(tibble)
# 
# a = df_florileges_struct %>%
#   filter(taxon_liste_florilege == 1) %>%
#   select(session_id, session_date, taxon) %>%
#   distinct()
# 
# mat_presence <- a %>%
#   distinct(session_id, taxon) %>%  # une ligne par taxon observé dans un relevé
#   mutate(presence = 1) %>%
#   pivot_wider(names_from = taxon, values_from = presence, values_fill = 0) %>%
#   column_to_rownames("session_id")  # specaccum attend les lignes comme sites
# 
# # 2. Appliquer specaccum
# acc <- specaccum(mat_presence, method = "random", permutations = 100)
# 
# # 3. Tracer la courbe
# plot(acc, 
#      ci.type = "poly",      # intervalle de confiance en polygone
#      ci.col = "lightblue",  # couleur IC
#      col = "blue",          # couleur ligne
#      lwd = 2,                # épaisseur ligne
#      xlab = "Nombre de relevés", 
#      ylab = "Nombre cumulé d'espèces", 
#      main = "Courbe d'accumulation d'espèces")



### coordonnées nationales ----
df_flori_national_coord = df_florileges %>%
  filter(!is.na(site_geometry)) %>%
  unique() %>%
  mutate(geometry = st_as_sfc(site_geometry, crs = 4326)) %>%
  select(-site_geometry) %>%
  st_as_sf() 

# on modifie dans la mesure du possible les geometries invalides
invalid_index <- which(!st_is_valid(df_flori_national_coord))
df_flori_national_coord[invalid_index, ] <- st_make_valid(df_flori_national_coord[invalid_index, ])

# on supprime les autres
df_flori_national_coord <- df_flori_national_coord[st_is_valid(df_flori_national_coord), ] %>%
  select(session_id) %>% 
  unique()


