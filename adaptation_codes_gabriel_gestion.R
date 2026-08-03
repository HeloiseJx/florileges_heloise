# données du script parent
df_florilege  = df_florileges


# 0. IMPORTATION MODULES ET DONNEES ####
# is.spatial = T
# fonction de mise en forme des données de gestion
mise_en_forme_gestion <- function(df){
  
  ###### Type de fauche
  df_new = df %>%
    mutate(fauche = if_else(
      is.na(fauche), NA_character_,  # Conserve les NA
      case_when(
        fauche == "[]" ~ "PasFauche",
        TRUE ~sapply(str_extract_all(fauche, "broyage|faux|Pas de fauche|Je ne sais pas"), function(x) paste(sort(factor(x, levels = c("broyage", "faux", "Pas de fauche", "Je ne sais pas"))), collapse = " "))
      )
    ))
  
  df_new = df_new %>%
    mutate(fauche = if_else(
      is.na(fauche), NA_character_,  # Conserve les NA
      str_replace_all(fauche, c("broyage" = "FB", "faux" = "FT", "Pas de fauche" = "PF", "Je ne sais pas" = "NSP"))
    ))
  
  df_new$fauche = factor(df_new$fauche, levels = c("PF","FB","FT","NSP"))
  
  
  ###### Periode de fauche
  df_new = df_new %>%
    mutate(periodes_fauches = if_else(
      is.na(periodes_fauches), NA_character_,  # Conserve les NA
      case_when(
        periodes_fauches == "[]" ~ "PasFauche",
        TRUE ~sapply(str_extract_all(periodes_fauches, "estivale|precoce|tardive|je-ne-sais-pas"), function(x) paste(sort(factor(x, levels = c("precoce", "estivale", "tardive", "je-ne-sais-pas"))), collapse = " "))
      )
    ))
  # Condition verifie oubli aucune valeur du facteur
  df_new$periodes_fauches = factor(df_new$periodes_fauches, levels = c("PasFauche", "precoce","estivale","tardive","precoce estivale","precoce tardive", "estivale tardive", "precoce estivale tardive","je-ne-sais-pas","precoce je-ne-sais-pas","tardive je-ne-sais-pas"))
  
  df_new = df_new %>% 
    mutate(periodes_fauches = case_when(
      periodes_fauches == "PasFauche" ~ "NoF",
      periodes_fauches == "precoce" ~ "P",
      periodes_fauches == "estivale" ~ "E",
      periodes_fauches == "tardive" ~ "T",
      periodes_fauches == "precoce estivale" ~ "PE",
      periodes_fauches == "precoce tardive" ~ "PT",
      periodes_fauches == "estivale tardive" ~ "ET",
      periodes_fauches == "precoce estivale tardive" ~ "PET",
      periodes_fauches == "precoce je-ne-sais-pas" ~ "NSPP",
      periodes_fauches == "tardive je-ne-sais-pas" ~ "NSPT",
      periodes_fauches == "je-ne-sais-pas" ~ "NSP"
    ))
  
  df_new$periodes_fauches = factor(df_new$periodes_fauches, levels = c("NoF", "P","E","T","PE","PT", "ET", "PET","NSP","NSPP","NSPT"))
  
  
  ###### Frequence de fauche#
  df_new <- df_new %>%
    mutate(frequence_fauches = if_else(
      is.na(frequence_fauches), NA_character_,  # Conserve les NA
      str_replace(frequence_fauches, "Je ne sais pas", "NSP"))
    )
  
  df_new$frequence_fauches = factor(df_new$frequence_fauches,levels = c( "< 1/an","1/an", "2/an", "> 2/an","NSP"))
  
  df_new = df_new %>% 
    mutate(frequence_fauches = ifelse(is.na(frequence_fauches),
                                      NA_character_,
                                      case_when(
                                        frequence_fauches == "< 1/an" ~ "M1",
                                        frequence_fauches == "1/an" ~ "E1", 
                                        frequence_fauches == "2/an" ~ "E2", 
                                        frequence_fauches == "> 2/an" ~ "P2",
                                        frequence_fauches == "NSP" ~ "NSP"
                                      )))
  df_new$frequence_fauches = factor(df_new$frequence_fauches,levels = c( "M1","E1", "E2", "P2","NSP"))
  
  
  ###### Exportation des résidus
  df_new = df_new %>%
    mutate(exportation_residus = if_else(
      is.na(exportation_residus), NA_character_,  # Conserve les NA
      str_replace(exportation_residus, "Je ne sais pas", "NSP"))
    )
  
  df_new$exportation_residus = factor(df_new$exportation_residus, levels = c("Oui","Non","NSP"))
  
  
  ##### Paturage
  df_new = df_new %>%
    mutate(paturages = if_else(
      is.na(paturages), NA_character_,  # Conserve les NA
      case_when(
        paturages == "[]" ~ NA_character_,
        TRUE ~sapply(str_extract_all(paturages, "pas-de-paturage|bovin|ovin|caprin|equide|autre|je-ne-sais-pas"), function(x) paste(sort(factor(x, levels = c("pas-de-paturage", "bovin", "caprin", "equide", "ovin", "autre","je-ne-sais-pas"))), collapse = " ")))
    ))
  df_new = df_new %>%
    mutate(paturages = ifelse(is.na(paturages),
                              NA_character_,
                              case_when(
                                paturages == "pas-de-paturage" ~ "NP",
                                paturages == "bovin" ~ "Pb",
                                paturages == "ovin" ~ "Po",
                                paturages == "caprin" ~ "Pc",
                                paturages == "equide" ~ "Pe",
                                paturages == "bovin caprin" ~ "Pbc",
                                paturages == "bovin ovin" ~ "Pbo",
                                paturages == "bovin equide" ~ "Pbe",
                                paturages == "caprin ovin" ~ "Pco",
                                paturages == "caprin equide" ~ "Pce",
                                paturages == "equide ovin" ~ "Peo",
                                paturages == "bovin caprin ovin" ~ "Pbco",
                                paturages == "bovin caprin equide" ~ "Pbce",
                                paturages == "caprin equide ovin" ~ "Pceo",
                                paturages == "bovin equide ovin" ~ "Pbeo",
                                paturages == "bovin caprin equide ovin" ~ "Pbceo",
                                paturages == "autre" ~ "Pautre",
                                paturages == "je-ne-sais-pas" ~ "NSP",
                                TRUE ~ "Inconnu"
                              )))
  df_new$paturages = factor(df_new$paturages,levels = c( "NP","Pb", "Pc", "Pe","Po","Pbc","Pbe","Pbo","Pce","Pco","Peo","Pbce","Pbco","Pbeo","Pceo","Pbceo","Pautre","NSP","Inconnu"))
  
  
  df_new <- df_new %>%
    mutate(paturages_cat = case_when(
      paturages == "NP" ~ "NP",     # Pas de pâturage
      paturages == "NSP" ~ "NSP",   # Ne sait pas
      is.na(paturages) ~ NA_character_,  # Garde les NA
      startsWith(as.character(paturages), "P") ~ "P",  # Tous les pâturages
      TRUE ~ "Inconnu"  # Si une valeur inattendue est trouvée
    ))
  df_new <- df_new %>% 
    mutate(paturages_cat = factor(paturages_cat, levels = c("NP", "P", "NSP", "Inconnu")))
  
  ##### Paturages duree annuelle
  df_new = df_new %>% 
    mutate(duree_annuelle_paturage = case_when(
      duree_annuelle_paturage == "null" ~ NA,
      TRUE ~ as.integer(duree_annuelle_paturage)
    ))
  
  df_new = df_new %>% 
    mutate(duree_annuelle_paturage_catpat = case_when(
      is.na(duree_annuelle_paturage) ~ NA,
      duree_annuelle_paturage == 0 ~ "NP",
      duree_annuelle_paturage > 0 ~ "P"
    ))
  df_new = df_new %>% 
    mutate(duree_annuelle_paturage_catpat = factor(duree_annuelle_paturage_catpat, levels = c("NP","P")))
  
  df_new = df_new %>% 
    mutate(duree_annuelle_paturage_cat = case_when(
      is.na(duree_annuelle_paturage) ~ NA,
      duree_annuelle_paturage == 0 ~ "NP",
      duree_annuelle_paturage <= 3 ~ "M3m",
      duree_annuelle_paturage <= 6 ~ "M6m",
      duree_annuelle_paturage > 6 ~ "P6m",
    ))
  df_new = df_new %>% 
    mutate(duree_annuelle_paturage_cat = factor(duree_annuelle_paturage_cat, levels = c("NP","M3m","M6m","P6m")))
  
  ##### Paturages pression
  df_new = df_new %>% 
    mutate(pression_paturage = case_when(
      pression_paturage == "null" ~ NA,
      TRUE ~ as.integer(pression_paturage)
    ))
  
  df_new = df_new %>% 
    mutate(pression_paturage_catpat = case_when(
      is.na(pression_paturage) ~ NA,
      pression_paturage == 0 ~ "NP",
      TRUE ~ "P"
    ))
  df_new = df_new %>% 
    mutate(pression_paturage_catpat = factor(pression_paturage_catpat, levels = c("NP","P")))
  
  df_new = df_new %>% 
    mutate(pression_paturage_cat = case_when(
      is.na(pression_paturage) ~ NA,
      pression_paturage == 0 ~ "NP",
      pression_paturage <= 5 ~ "M5b",
      pression_paturage <= 10 ~ "M10b",
      pression_paturage <= 20 ~ "M20b",
      pression_paturage <= 40 ~ "M40b",
      pression_paturage > 40 ~ "P40b"
    ))
  df_new = df_new %>% 
    mutate(pression_paturage_cat = factor(pression_paturage_cat, levels = c("NP","M5b","M10b","M20b","M40b","P40b")))
  
  # df_new = df_new %>% 
  #   mutate(pression_paturage = droplevels(factor(pression_paturage, levels = seq(0,max(pression_paturage, na.rm = T)))))
  
  return(df_new)
}

# source("C:/Git/florileges code gabriel/florileges_HM/functions/functions_graph.R") # uniquement si utilise test graphiques

# fonctions de gestion des coordonnées
# pour partie 7 pour corriger les donnees de position
# source("C:/Git/florileges code gabriel/florileges_HM/functions/functions_positions_std_V1.1.R") 

# df reduit (sans les taxons)
df_florilege_red <- df_florilege %>% 
  select(-all_of(starts_with("taxon")),-observation_id,-cd_nom) %>% 
  select(-all_of(starts_with("tige")),-all_of(starts_with("date")),-all_of(starts_with("travaux")),-all_of(starts_with("frequence_t")),-all_of(starts_with("frequence_a")),-protocole,-site_zip_code) %>%
  distinct()

# données audrey pour compléter df
df_audrey2 = read_excel("C:/Git/florileges code gabriel/florileges_HM/data/rdata/df_florileges/SiteYearGestion20250330.xlsx")

df_gest <- mise_en_forme_gestion(df_florilege_red)





# 1. CORRECTION - EXPORT RESIDUS  ####

# RECUPERE INFOS EXPORT AUDREY
corr_export_res = df_audrey2 %>% 
  select(id_releve, ExportationResidus)

# Pour les session pour lesquelles il y a des infos de gestion erronées, on compare ce qui est renseigné dans le df d'audrey vs ce qui est renseigné sur la bd mosaic (colonne diff_export qui paste les export de fauche des 2 df pour une même gestion)
# les informations d'audrey prévalent par rapport à celles de la BD mosaic
df_gest = df_gest %>% 
  left_join(corr_export_res, by = c("session_id" = "id_releve" ))
df_gest = df_gest %>% 
  mutate(diff_export = paste(exportation_residus,ExportationResidus, sep = "_"))
unique(df_gest$diff_export)



# MODIFIE INFOS DANS NOTRE JEU DE DONNEES à partir de la colonne diff_export
df_gest_corrExp = df_gest %>%
  mutate(exportation_residus = case_when(
    diff_export == "Non_Oui" ~ "Oui",
    TRUE ~ exportation_residus
  ))

df_gest_corrExp = df_gest_corrExp %>% 
  select(-ExportationResidus,-diff_export)

# --> df avec les export corrigés
df_gest_corrExp = df_gest_corrExp %>% 
  mutate(exportation_residus = factor(exportation_residus, levels = c("Oui","Non","NSP")))






# 2. FORMATAGE GESTION ####

##### 2.1 FAUCHE #####
# indique si les combinaisons entre les variables décrivant la fauche sont ok ou pas
table_filtre_faucheNAper <- read_excel("C:/Git/florileges code gabriel/florileges_HM/data/rdata/tables_de_filtres/filtre_categorielle_fauche-ou-pas_NAper.xlsx")

###### 2.1.1 Creation des categories F ou PF pour toutes les variables ######
# "PRB" = Problème
df_gest2 = df_gest_corrExp %>% 
  mutate(TF_cat = case_when(
    fauche == "PF" ~ "PF",
    fauche == "FB" | fauche == "FT" ~ "F",
    fauche == "NSP" | is.na(fauche) ~ "NANSP",
    TRUE ~ "PRB"
  ),
  # categories F ou PF à partir des données de période de fauche
  Per_cat = case_when(
    periodes_fauches == "NSP" | periodes_fauches == "NoF"  ~ "NANSP",
    periodes_fauches %in% list("P","E","T","PE","PT","ET","PET") ~ "F",
    TRUE ~ "PRB"
  ),
  # categories F ou PF à partir des données de frequences de fauche
  Freq_cat = case_when(
    # frequence_fauches == "M1" ~ "PF",
    frequence_fauches == "M1" & TF_cat == "F" ~ "F", # cas des fauches bisannuelles
    frequence_fauches == "M1" & (TF_cat == "PF" | is.na(TF_cat) | TF_cat == "NANSP") ~ "PF",
    frequence_fauches == "NSP" | is.na(frequence_fauches) ~ "NANSP",
    frequence_fauches %in% list("E1","E2","P2") ~ "F",
    TRUE ~ "PRB"
  ),
  # categories F ou PF à partir des données d'e frequences de fauche'export des résidus
  Exp_cat = case_when(
    exportation_residus == "Non" ~ "NE",
    exportation_residus == "Oui" ~ "E",
    exportation_residus == "NSP" | is.na(exportation_residus) ~ "NANSP",
    TRUE ~ "PRB"
  ))


df_gest2 = df_gest2 %>% 
  # fixer ordre d'affichage des variables dans les graphiques grâce à levels
  mutate(
    TF_cat = factor(TF_cat, levels = c("PF","F","NANSP","PRB")),
    Per_cat = factor(Per_cat, levels = c("F","NANSP","PRB")),
    Freq_cat = factor(Freq_cat, levels = c("PF","F","NANSP","PRB")),
    Exp_cat = factor(Exp_cat, levels = c("NE","E","NANSP","PRB"))
  )



# Visionner les donnees problematiques (notamment -> periode NSP-P et NSP-T)
# df_gest2 %>%
#   filter(Per_cat == "PRB") %>%  View()


# CORRECTION DES DONNEES MAL RENSEIGNEES de périodes de fauche
df_gest2 = df_gest2 %>% 
  mutate(periodes_fauches = case_when(
    session_id == 26206 ~ 'P', # a la place NSPP
    session_id == 22944 ~ 'NSP', # a la place NSPT
    TRUE ~ periodes_fauches),
    Per_cat = case_when(
      session_id == 22944 ~ 'NANSP',
      session_id == 26206 ~ 'F',
      TRUE ~ Per_cat)
    ) 


###### 2.1.2 Application du filtre categoriel ######
# jpointure avec la table table_filtre_faucheNAper qui associe aux différentes combinaisons de valeurs de cellule de gestion un tag de validité des données
df_gest2 = df_gest2 %>% 
  #verification des combinaisons entre les différentes variables décrivant la fauche
  left_join(table_filtre_faucheNAper, by = c("TF_cat" = "typeF_cat", "Freq_cat" = "freq_cat", "Per_cat" = "per_cat", "Exp_cat" = "exp_cat"))



###### 2.1.3 Transformation des donnees frequence/periode si fauche averee######
df_gest2 = df_gest2 %>% 
  mutate(Freq_catinterne = case_when(
    (Fr_corr == "OK" & Per_corr == "OK") & frequence_fauches == "M1" ~ 1,
    (Fr_corr == "OK" & Per_corr == "OK") & frequence_fauches == "E1" ~ 1,
    (Fr_corr == "OK" & Per_corr == "OK") & frequence_fauches == "E2" ~ 2,
    (Fr_corr == "OK" & Per_corr == "OK") & frequence_fauches == "P2" ~ 3,
    TRUE ~ NA),
    Per_catinterne = case_when(
      (Fr_corr == "OK" & Per_corr == "OK") & periodes_fauches %in% list("P","E","T") ~ 1,
      (Fr_corr == "OK" & Per_corr == "OK") & periodes_fauches %in% list("PE","PT","ET") ~ 2,
      (Fr_corr == "OK" & Per_corr == "OK") & periodes_fauches == "PET" ~ 3,
      TRUE ~ NA
    )
  )

###### 2.1.4 Application du filtre sur la compatibilite frequence/periode ######      
df_gest2 = df_gest2 %>% 
  mutate(comp_PerFr = case_when(
    Freq_catinterne >= Per_catinterne ~ "Oui",
    Freq_catinterne < Per_catinterne ~ "Non",
    TRUE ~ NA_character_),
    comp_globale = paste(OK,comp_PerFr,sep= "_") # permet de coller 2 scores de validité des données : celui obtenu à partir de la table locale qui considère la combinaison des variables de gestion, et celui ci qui considère la cohérence entre fréquence de gestion et périodes de gestion renseignées
  )



###### 2.1.5 Creation des nouvelles variables de fauche a utiliser ######
df_gest2 = df_gest2 %>% 
  mutate(fauche_corr = case_when(
    (OK == "Oui" | OK == "Bof") & comp_globale != "Oui_Non" & TF_corr == "OK" ~ fauche,
    (OK == "Oui" | OK == "Bof") & TF_corr == "NA" ~ NA,
    TRUE ~ "EXCLURE"
  ),
  frequence_fauches_corr = case_when(
    (OK == "Oui" | OK == "Bof") & comp_globale != "Oui_Non" & Fr_corr == "OK" ~ frequence_fauches,
    (OK == "Oui" | OK == "Bof") & Fr_corr == "PF" ~ "PF",
    (OK == "Oui" | OK == "Bof") & Fr_corr == "NA" ~ NA,
    TRUE ~ "EXCLURE"
  ),
  periodes_fauches_corr = case_when(
    (OK == "Oui" | OK == "Bof") & comp_globale != "Oui_Non" & Per_corr == "OK" ~ periodes_fauches,
    (OK == "Oui" | OK == "Bof") & Per_corr == "PF" ~ "PF",
    (OK == "Oui" | OK == "Bof") & Per_corr == "NA" ~ NA,
    TRUE ~ "EXCLURE"
  ),
  exportation_residus_corr = case_when(
    (OK == "Oui" | OK == "Bof") & comp_globale != "Oui_Non" & Exp_corr == "OK" ~ exportation_residus,
    (OK == "Oui" | OK == "Bof") & Exp_corr == "PF" ~ "PF",
    (OK == "Oui" | OK == "Bof") & Exp_corr == "NA" ~ NA,
    TRUE ~ "EXCLURE"
  ) 
  )

df_gest2 = df_gest2 %>% 
  mutate(fauche_corr = factor(fauche_corr, levels = c('PF',"FB","FT","EXCLURE")),
         periodes_fauches_corr = factor(periodes_fauches_corr, levels = 
                                          c("PF","P","E","T","PE","PT","ET","PET","EXCLURE")),
         frequence_fauches_corr = factor(frequence_fauches_corr, levels = c("PF","M1", "E1","E2","P2","EXCLURE")),
         exportation_residus_corr = factor(exportation_residus_corr, levels = c("PF","Oui","Non","EXCLURE")))



###### 2.1.6. ISG FAUCHE :Variable recapitulative de la fauche ######
# --> colonne ModeGestion

# df_gest2 %>%
#   group_by(num,OK,TF_cat,Freq_cat,Per_cat,Exp_cat) %>%
#   summarise(nombre = n()) %>% View()
# 
# df_gest2 %>%
#   group_by(num,OK,TF_corr,Fr_corr,Per_corr,Exp_corr) %>%
#   summarise(nombre = n()) %>% View()

df_gest2 = df_gest2 %>% 
  mutate(ModeGestion = paste(fauche_corr,frequence_fauches_corr,periodes_fauches_corr,exportation_residus_corr, sep = "_"))

df_gest2 = df_gest2 %>% 
  mutate(ModeGestion = ifelse(ModeGestion == "PF_PF_PF_PF", "PF", ModeGestion))

# Nombre de donnees de fauche manquantes 
df_gest2 = df_gest2 %>% 
  mutate(nbNA = rowSums(is.na(across(c(fauche_corr, frequence_fauches_corr, periodes_fauches_corr, exportation_residus_corr)))))







##### 2.2. PATURAGE #####
# de même, table qui donne les combinaisons d'informations de paturage qui sont correctes et incorrectes
table_filtre_paturage <- read_excel("C:/Git/florileges code gabriel/florileges_HM/data/rdata/tables_de_filtres/filtre_categorielle_pat-ou-pas.xlsx")

###### 2.2.1 Creation des categories NP/P/NA ######
df_gest3 = df_gest2 %>% 
  mutate(paturages_cat = case_when(
    paturages_cat == "NP" ~ "NP",
    paturages_cat == "P" ~ "P",
    paturages_cat == "NSP" | is.na(paturages_cat) ~ "NANSP",
    TRUE ~ "PRB"
  ),
  duree_annuelle_paturage_catpat = case_when(
    is.na(duree_annuelle_paturage_catpat) ~ "NA", 
    TRUE ~ duree_annuelle_paturage_catpat
  ),
  pression_paturage_catpat = case_when(
    is.na(pression_paturage_catpat) ~ "NA",
    TRUE ~ pression_paturage_catpat
  )
  )


###### 2.2.2 Application du filtre categoriel ######

df_gest3 = df_gest3 %>% 
  left_join(table_filtre_paturage, by = c("paturages_cat" = "paturage_cat" , "duree_annuelle_paturage_catpat" = "duree_catpat" , "pression_paturage_catpat" = "pression_catpat")) %>% 
  rename(pression_catpat = "pression_paturage_catpat",
         duree_catpat="duree_annuelle_paturage_catpat")




###### 2.2.3 Creation des nouvelles variables de paturage ######
df_gest3 = df_gest3 %>% 
  mutate(paturages_corr = case_when(
    (OK_pat == "Oui") & pat_corr == "OK" ~ paturages,
    (OK_pat == "Oui" | OK_pat == "Bof") & pat_corr == "NP" ~ "NoPAT",
    (OK_pat == "Bof") & pat_corr == "NA" ~ NA,
    TRUE ~ "EXCLURE"
  ),
  paturage_cat_corr = case_when(
    (OK_pat == "Oui") & pat_corr == "OK" ~ "PAT",
    (OK_pat == "Oui" | OK_pat == "Bof") & pat_corr == "NP" ~ "NoPAT",
    (OK_pat == "Bof") & pat_corr == "NA" ~ NA,
    TRUE ~ "EXCLURE"
  ),
  duree_pat_corr = case_when(
    (OK_pat == "Oui" ) & duree_corr == "OK" ~ as.character(duree_annuelle_paturage),
    (OK_pat == "Oui" | OK_pat == "Bof") & duree_corr == "NP" ~ "NoPAT",
    (OK_pat == "Bof") & duree_corr == "NA" ~ NA,
    TRUE ~ "EXCLURE"
  ),
  pression_pat_corr = case_when(
    (OK_pat == "Oui" ) & pression_corr == "OK" ~ as.character(pression_paturage),
    (OK_pat == "Oui" | OK_pat == "Bof") & pression_corr == "NP" ~ "NoPAT",
    (OK_pat == "Bof") & pression_corr == "NA" ~ NA,
    TRUE ~ "EXCLURE"
  ) 
  )




###### 2.2.4 Correction a la main du paturage ######
# Partie arbitraire, a discuter

# Sites consideres comme patûres malgre non ou bof : 9513,9776,10406,10285,9992,9673,9945,10126,9648,9466
liste_session_pat = list(9513,9776,10406,10285,9992,9673,9945,10126,9648,9466)
df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = ifelse(session_id %in% liste_session_pat,"PAT",paturage_cat_corr))

# Sites consideres comme certainement patûres malgre non ou bof : 9958,9632,9939,9772
liste_session_problmnt_pat = list(9958,9632,9939,9772)
df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = ifelse(session_id %in% liste_session_problmnt_pat,"PAT",paturage_cat_corr))

# Sites consideres comme non-patures malgre non ou bof : 23043,25964,23238
liste_session_no_pat = list(23043,25964,23238)
df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = ifelse(session_id %in% liste_session_no_pat,"NoPAT",paturage_cat_corr))

# Corrige aussi les autres infos (discutable pour pression et duree)
df_gest3 = df_gest3 %>%
  mutate(paturages_corr = case_when(
    session_id %in% c(10126,9648,9466) ~ "Pe", # donnees a NA dans paturages, mais presente dans paturages_autre
    session_id %in% c(liste_session_pat,liste_session_problmnt_pat) ~ paturages,
    session_id %in% liste_session_no_pat ~ "NoPAT",
    TRUE ~ paturages_corr))
# Pourrait aussi mettre infos de pression et durre a NA quand valeur vaut 0 plutôt que garder 0
df_gest3 = df_gest3 %>% 
  mutate(
    duree_pat_corr = case_when(
      session_id %in% c(liste_session_pat,liste_session_problmnt_pat) & duree_annuelle_paturage != 0 ~ as.character(duree_annuelle_paturage),
      session_id %in% c(liste_session_pat,liste_session_problmnt_pat) ~ "0", # pourrait mettre NA
      session_id %in% liste_session_no_pat ~ "NoPAT",
      TRUE ~ duree_pat_corr),
    pression_pat_corr =  case_when(
      session_id %in% c(liste_session_pat,liste_session_problmnt_pat) & pression_paturage != 0 ~ as.character(pression_paturage),
      session_id %in% c(liste_session_pat,liste_session_problmnt_pat) ~ "0", # pourrait mettre NA
      session_id %in% liste_session_no_pat ~ "NoPAT",
      TRUE ~ pression_pat_corr)
  )



# Reformate bien les donnees
# HYPOTHESE FORTE : si pas infos de paturage, alors decide que il n'y a pas de paturage, sinon exclus trop de donnees

# paturage bovin, equin, caprin, ovin ou autre

df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = ifelse(is.na(paturage_cat_corr),"NoPAT",paturage_cat_corr),
         paturages_corr = ifelse(is.na(paturages_corr),"NoPAT",paturages_corr))
df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = factor(paturage_cat_corr,levels = c("PAT","NoPAT","EXCLURE")),
         paturages_corr = droplevels(factor(paturages_corr, levels = c("NoPAT","Pb","Pc","Pe" , "Po","Pbc","Pbe","Pbo","Pce","Pco","Peo","Pbce","Pbco","Pbeo","Pceo","Pbceo","Pautre", "EXCLURE"))))



###### 2.2.5 ISG fauche + paturage : Ajustement des ISG avec le paturage ######
# --> colonne ModeGestionP

df_gest3 = df_gest3 %>% 
  mutate(ModeGestionP = ifelse(paturage_cat_corr == "NoPAT", ModeGestion, paste(ModeGestion, paturage_cat_corr, sep = "_")))

df_gest3 = df_gest3 %>%
  mutate(ModeGestion_restr = case_when(
    fauche_corr == "EXCLURE" | paturage_cat_corr == "EXCLURE" ~ "Incoherent",
    nbNA >0 ~ "Incomplet",
    TRUE ~ ModeGestionP
  ))

df_paturage = df_gest3


df_gest4 = df_gest3 %>%  
  select(-c(duree_catpat,pression_catpat,TF_cat,Per_cat,Freq_cat,Exp_cat,TF_corr,Fr_corr,Per_corr,Exp_corr,OK,num,Freq_catinterne,Per_catinterne,comp_PerFr,pat_corr,duree_corr,pression_corr,num_pat))

rm(df_gest,df_gest2,df_gest_corrExp,corr_export_res,liste_session_no_pat,liste_session_pat,liste_session_problmnt_pat,table_filtre_paturage,table_filtre_faucheNAper)






# 3. FUSION SITES IDENTIQUES ####

##### 3.1 Ajout du nouvel identifant de site #####
# recupere table des sites identiques
table_sites_identiques = read.csv2("C:/Git/florileges code gabriel/florileges_HM/data/rdata/tables_de_filtres/sites_indentiques_sur_et_certain.csv") 

df_gest4 <- df_gest4 %>% 
# join les id corriges au df principal
  left_join(table_sites_identiques,by = c("site_id" = "Site_avant")) %>%
  mutate(site_id_corr = ifelse(is.na(Site_maintenant), site_id, Site_maintenant)) %>% 
  select(-Site_maintenant) %>%
# Erreur de saisie, est precise dans le commentaire du releve
  mutate(site_id_corr = ifelse(session_id == 20689, 739, site_id_corr))


##### 3.2 Ajout des nouvelles valeurs de position #####
# Ne modifie que la geometrie, car c'est cette info qui nous a permis de dire que les sites sont identiques, en revanche les autres infos (qui peuvent changer meme aussi pour un meme site au cours de plusieurs releves) sont laissees telles quelles.

# Recupere uniquement association initiale site_id/donnees de position
infos_geom_site = df_gest4 %>%
  select(site_id,site_geometry) %>% #,site_latitude,site_longitude
  rename(site_geometry_corr = "site_geometry"#,
         # site_longitude_corr = "site_longitude",
         # site_latitude_corr = "site_latitude"
         ) %>%
  distinct(.keep_all = T)
# Ajoute les nouvelles valeurs en fonction de l'identifiant du site corrige
df_gest4 <- df_gest4 %>%
  left_join(infos_geom_site,by = c("site_id_corr" = "site_id"))


# Essaye d'associer toute les valeurs du nouveau site, mais un mme id de site peut avoir des donnees differentes au cours du temps (nom, occupation, surface, etc.) -> ne peut pas l'automatiser.
site_infos = df_gest4 %>%
  select(site_id,site,site_geometry,site_departement,surface,objectifs,objectifs_autre,frequentation,occupations_sol_anterieures,occupations_sol_anterieures_autre,site_creation,amendements,amendements_autre) %>% distinct()
df_gest4 <- df_gest4 %>%
  left_join(site_infos, by = c("site_id_corr" = "site_id"), suffix = c("","_corr"))


# 4. FILTRAGE RELEVES REDONDANTS ####

# Une gestion a  modifier a la main (plus infos flore mais pas infos gestion)
cols_a_modifer = c("fauche","periodes_fauches","frequence_fauches","exportation_residus","paturages","paturages_cat","fauche_corr","periodes_fauches_corr","frequence_fauches_corr","exportation_residus_corr","paturages_corr","paturage_cat_corr","pression_pat_corr","duree_pat_corr","ModeGestion","ModeGestionP","ModeGestion_restr","comp_globale","OK_pat")
gestion_a_recup <- df_gest4 %>% 
  filter(session_id == 26234) %>% 
  select(all_of(cols_a_modifer))
df_gest4 <- df_gest4 %>% 
  mutate(across(all_of(cols_a_modifer),
                ~if_else(session_id == 23118, gestion_a_recup[[cur_column()]], .)))



table_releve_redondant_a_conserver = read_excel("C:/Git/florileges code gabriel/florileges_HM/data/rdata/tables_de_filtres/table_filtre-releves-redondants.xlsx")
# Ne garde que les variables utiles
table_releve_redondant_a_conserver = table_releve_redondant_a_conserver %>% 
  select(session_id,statut_redond) 
# Rassemble dans la table principale
df_gest5 <- df_gest4 %>% 
  left_join(table_releve_redondant_a_conserver, by = "session_id")
# Uniformise la colonne sur les releves redondants a conserver
df_gest5 <- df_gest5 %>% 
  mutate(statut_redond = case_when(
    statut_redond == "remove" ~  "remove",
    TRUE ~ "keep"
  ))




# 5. FILTRAGE DONNEES FLORISTIQUES  ####  

# ##### 5.1 Releves avec plus de 2 quadrats nuls #####
# # Somme par session_id pour chaque quadrat
# quad_sum <- df_florilege %>% 
#   filter(session_date >= 2014) %>% 
#   group_by(session_id) %>%
#   summarise(across(starts_with("taxon_count_Q"), sum))
# # Compte le nombre de quadrats vides
# quad_sum <- quad_sum %>%
#   rowwise() %>%
#   mutate(nb_quadrats_vides = sum(c_across(starts_with("taxon_count_Q")) == 0)) %>%
#   ungroup()
# nb_qdrt_vide <- quad_sum %>% 
#   select(session_id,nb_quadrats_vides)
# # Ajouter infos nombre de quadrats vides dans df de travail
# df_gest5 = df_gest5 %>% 
#   left_join(nb_qdrt_vide,by = "session_id")
# # Condition pour garder ou non releve 
# df_gest5 = df_gest5 %>% 
#   mutate(statut_quadrat_vide = ifelse(nb_quadrats_vides>=3, "remove","keep")) 
# if(test_table){
#   cat("Nombre de releves avec au moins 3 quadrats vides :",df_gest5 %>% filter(statut_quadrat_vide == "remove") %>% nrow) 
# }
# if(test_graph){
#   hist(df_gest5$nb_quadrats_vides, main = "Distribution du nombre de quadrat vides", xlab = "Nombre de quadrats vides", ylab = "Nombre de releves")
# }

##### 5.2 Releves sans donnees tiges ligneuses #####
# garder une ligne par session (la première)
df_florilege_red2 = df_florilege %>% 
  distinct(session_id, .keep_all = T)
# Calcule le nombre total de tiges par site
tiges_sum = df_florilege_red2%>% 
  filter(session_date >= 2014) %>% 
  group_by(session_id) %>%
  mutate(across(starts_with("tiges_ligneuses_nombre_Q"), as.numeric)) %>% 
  summarise(across(starts_with("tiges_ligneuses_nombre_Q"), sum)) %>% 
  rowwise() %>% 
  mutate(nb_tiges = sum(across(starts_with("tiges_ligneuses_nombre_Q")))) %>%
  ungroup() %>%
  filter(nb_tiges >= 0)
# Ajoute le filtre sur absence/presence de donnees
tiges_sum = tiges_sum %>% 
  mutate(statut_ligneux = ifelse(is.na(nb_tiges),"remove","keep"))
## Retire releve avec nombre de tige ligneuses negatif
tiges_sum = tiges_sum %>% 
  mutate(statut_ligneux = ifelse(session_id == 9983 ,"remove",statut_ligneux))
# Ajoute categorie sur nombre de tiges ligneuses
tiges_sum = tiges_sum %>% 
  mutate(nb_tiges_cat = case_when(
    nb_tiges == 0 ~ "None",
    nb_tiges <= 5 ~ "Less5",
    nb_tiges <=20 ~ "Less20",
    nb_tiges <=50 ~ "Less50",
    nb_tiges >50 ~ "More50",
    is.na(nb_tiges) ~ NA
  ))
tiges_sum = tiges_sum %>% 
  mutate(nb_tiges_cat = factor(nb_tiges_cat, levels = c("None","Less5","Less20","Less50","More50")))
# Joint les dataframes
df_gest5 = df_gest5 %>% 
  left_join(tiges_sum,by = "session_id")



# 7. FILTRAGE POSITION GEOGRAPHIQUE ####

##### 7.1 Correction de la geometrie #####
# necessite d'avoir les colonnes longitude et latitude --> à récupérer à terme
# recupere releves avec geometrie a probleme (ie qui ont une geometrie mais pas de latitude/longitude), ne veut pas corriger les autres
releves_prb_geometry = df_gest5 %>%
  filter(is.na(site_geometry_corr)) %>%
  select(session_id,site_geometry_corr)

# # transforme dans le bon format (sf)
# library(rlang)
# releves_prb_geometry = recuperation_position(df_sites = releves_prb_geometry,
#                                              sites_exclus = c(),
#                                              var_site_id = releves_prb_geometry$session_id,
#                                              var_site_coord = site_geometry_corr)

# releves_prb_geometry = correction_position(releves_prb_geometry)

# # calcul des barycentres
# releves_prb_geometry <- releves_prb_geometry %>% 
#   mutate(centroid = sf::st_centroid(releves_prb_geometry)$geometry) %>% 
#   mutate(site_latitude_corr2 = st_coordinates(centroid)[,2],
#          site_longitude_corr2 = st_coordinates(centroid)[,1],)

# # retransforme en df avant le join 
# releves_prb_geometry <- releves_prb_geometry %>%
#   mutate (site_geometry_corr2 = st_as_text(geometry))%>% 
#   st_drop_geometry() %>% 
#   select(nom,site_geometry_corr2,site_latitude_corr2,site_longitude_corr2)

# # ajout des nouvelles longitude/lattitude dans df principal
# df_gest6 <- df_gest5 %>% 
#   left_join(releves_prb_geometry,by = c("session_id"="nom"))
# df_gest6 <- df_gest6 %>% 
#   mutate(site_geometry_corr2 = ifelse(!is.na(site_geometry_corr2),site_geometry_corr2,site_geometry_corr),
#          site_latitude_corr2 = ifelse(!is.na(site_latitude_corr),site_latitude_corr,site_latitude_corr2),
#          site_longitude_corr2 = ifelse(!is.na(site_longitude_corr),site_longitude_corr,site_longitude_corr2)
#   )

##### 7.2 Filtre des sites avec coordonnees ##### 
# df_gest6 <- df_gest6 %>% 
#   mutate(statut_geom = ifelse(!is.na(site_geometry_corr2),"keep","remove"))
# # Site avec geometrie aberrante (polygone taille IDF)
# df_gest6 <- df_gest6 %>% 
#   mutate(statut_geom = if_else(site_id_corr == 1632, "remove", statut_geom))
# 
# if(test_graph){
#   plot_pie_1var(df_gest6, statut_geom,pct = T) + labs(title = "Repartition des releves ayant une information de position")
# }


# 8. data_ISG : APPLICATION DE TOUS LES FILTRES ####

# Enleves les objets devenus inutiles
rm(df_gest3,df_gest4,table_releve_redondant_a_conserver,table_sites_identiques,infos_geom_site,cols_a_modifer,gestion_a_recup,tiges_sum)

# Filtres
df_corr_final <- df_gest5 %>% 
  filter(ModeGestion_restr != "Incoherent") %>%  # Enleve gestion incoherente
  filter(statut_redond == "keep") # Enleve releves redondants


# Selection des variables
df_corr_final <- df_corr_final %>% 
  select(session_id,structure_id,user_id,site,site_geometry,site_departement,surface,objectifs,objectifs_autre,frequentation,occupations_sol_anterieures,occupations_sol_anterieures_autre,site_creation,amendements,amendements_autre,session_date,session_starting_time,session_ending_time,hauteur_vegetation,milieux,milieux_autre,semis_sursemis,fauche_corr,periodes_fauches_corr,frequence_fauches_corr,exportation_residus_corr,paturages_corr,paturage_cat_corr,pression_pat_corr,duree_pat_corr,ModeGestionP,ModeGestion_restr,traitements_phyto,traitements_phyto_autre,pressions,pressions_autre,commentaire,session_year,session_month,session_day,nbNA,site_id_corr, nb_tiges,nb_tiges_cat)%>% 
    rename(site_id = site_id_corr,
           fauche = fauche_corr,
           periodes_fauches = periodes_fauches_corr,
           frequence_fauches = frequence_fauches_corr,
           exportation_residus = exportation_residus_corr,
           paturages = paturages_corr,
           paturage_cat = paturage_cat_corr,
           pression_pat = pression_pat_corr,
           duree_pat = duree_pat_corr)

# sans les modes de gestion incohérents
# df_corr_final_restr <- df_corr_final %>% 
  # select(session_id,structure_id,user_id,site_id,site_departement,surface,frequentation,session_date,hauteur_vegetation,fauche,periodes_fauches,frequence_fauches,exportation_residus,paturages,paturage_cat,pression_pat,duree_pat,ModeGestionP,ModeGestion_restr,commentaire,session_year,session_month,session_day,nbNA,nb_tiges,nb_tiges_cat)

# caluls des scores de fauche et simplification des ISG
dt_ISG = df_corr_final %>% 
  select( ModeGestion_restr, fauche, frequence_fauches, periodes_fauches, exportation_residus, paturage_cat) %>%
  distinct() %>%
  filter() %>%
  # regroupement des categories de gestion
  mutate(freq_fauche_simple = case_when(frequence_fauches == "P2" ~ frequence_fauches, 
                                        is.na(frequence_fauches)  ~ frequence_fauches,
                                        fauche == "PF" ~ "PF",
                                        TRUE ~ "E12"),
         per_simple = case_when(periodes_fauches %in% c("E", "T", "ET" ) ~ "ET",
                                periodes_fauches %in% c("PE", "PT", "PET" , "P") ~ "P", 
                                TRUE ~ periodes_fauches )
         ) %>%
  mutate(ISG_new = paste0( freq_fauche_simple, "_", per_simple, "_", fauche, "_", exportation_residus)) %>%
  arrange(ISG_new) %>%
  # on considère que la fréq et la periode de fauche sont prioritaires et définissent le gradient d'intensité de gestion. le type de fauche et les exports sont des "bonus" en intensité, leur effet étant plus restreint.
    mutate(score_1 = case_when(fauche == "PF" ~ 1,
                               TRUE ~ 0),
           score_2 = case_when(freq_fauche_simple == "E12" & exportation_residus == "Non" ~ 2,
                               TRUE ~ 0),
           score_3 = case_when( freq_fauche_simple == "E12" & exportation_residus == "Oui" ~ 4,
                               TRUE ~ 0),
           score_4 = case_when( freq_fauche_simple == "P2" ~ 6,
                                TRUE ~ 0),
           bonus_score2 = case_when( freq_fauche_simple == "E12" & exportation_residus == "Non" & periodes_fauches == "P" ~ 1,
                              TRUE ~ 0),
           bonus_score3 = case_when( freq_fauche_simple == "E12" & exportation_residus == "Oui" & periodes_fauches == "P" ~ 1,
                               TRUE ~ 0),

           bonus_score4a = case_when( freq_fauche_simple == "P2"  & periodes_fauches == "P" ~ 1,
                                     TRUE ~ 0),
           bonus_score4b = case_when( freq_fauche_simple == "P2"  & exportation_residus == "Oui"  ~ 1,                                     TRUE ~ 0),
           bonus_score4c = case_when( freq_fauche_simple == "P2" & periodes_fauches == "P" & exportation_residus == "Oui" ~ 2,
                                      TRUE ~ 0),
           
           ) %>%
    mutate(score_fauche = score_1+score_2+score_3+score_4+bonus_score2 +bonus_score3 + bonus_score4a + bonus_score4b, 
           score_fauche_sansbonus = score_1+score_2+score_3+score_4) %>%
  #gestion des na 
  # si la frequence et la période sont des NA alors on considère que le score est na
  mutate(score_fauche = case_when(is.na(freq_fauche_simple) | is.na(per_simple) ~ NA,
                               # s'il n'y a pas de fauche
                               fauche == "PF" ~ 1,
                               TRUE ~ score_fauche),
         score_fauche_sansbonus = case_when(is.na(freq_fauche_simple) | is.na(per_simple) ~ NA,
                                  # s'il n'y a pas de fauche
                                  fauche == "PF" ~ 1,
                                  TRUE ~ score_fauche_sansbonus))
  




# df florileges complet avec ISG et gestion corrigée
cols = c(colnames(dt_ISG) , colnames(df_corr_final)) %>% unique() %>% setdiff("session_id")

data_ISG = df_corr_final %>% #modes de gestion associées aux sessions
  left_join(dt_ISG) %>% #scores associés aux modes de gestion
  left_join(df_florilege %>% select(-any_of(cols)) ) %>% # reste des infos du df florileges
  unique() %>%
  filter(taxon_liste_florilege == 1) %>% 
  select(-score_1, -score_2, -score_3, -score_4, -starts_with("bonus"), -ModeGestionP)
  


# GRAPHES ----

## etat des lieux des données de gestion ----

# df_florilege_ISG =
# data_ISG %>%
#   select(session_id, ISG_new, score_fauche, score_fauche_sansbonus) %>%
#   filter(!is.na(score_fauche)) %>%
#   distinct() %>%
#   ggplot(aes(
#     x = reorder(ISG_new, score_fauche),
#     fill = factor(score_fauche_sansbonus)
#   )) +
#   geom_bar(stat = "count") +
#   labs(x = "ISG simplifiés et classés par intensité de gestion (freq et période) croissante") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# 


data_ISG %>%
  select(site_id, ISG_new, score_fauche) %>%
  filter(!is.na(score_fauche)) %>%
  distinct() %>%
  ggplot(aes(
    x = factor(score_fauche),
    fill = factor(score_fauche)
  )) +
  geom_bar(stat = "count") +
  labs(x = "score d'intensité de la fauche", 
       y = "nombre de sites") +
  theme(legend.position = "none") +   
  scale_fill_manual(values = c("1" = "#10170D" , "2"= "#252916FF", "3" = "#244422FF","4" = "#225F2FFF", "5"="#3B7D31FF", "6" = "#5E9432FF","7" = "#88AB38FF", "8" =  "#B4BF3AFF", "9" = "#EBCF2EFF", "10" = "#F6E58D")) +
  theme_minimal() +
  theme(legend.position = "none")


data_ISG %>%
  select(site_id, ISG_new, score_fauche_sansbonus) %>%
  filter(!is.na(score_fauche_sansbonus)) %>%
  distinct() %>%
  ggplot(aes(
    x = factor(score_fauche_sansbonus),
    fill = factor(score_fauche_sansbonus)
  )) +
  geom_bar(stat = "count") +
  labs(x = "score d'intensité de la fauche", 
       y = "nombre de sites") +   
  scale_fill_manual(values = c("1" = "#10170D" , "2"= "#252916FF", "3" = "#244422FF","4" = "#225F2FFF", "5"="#3B7D31FF", "6" = "#5E9432FF","7" = "#88AB38FF", "8" =  "#B4BF3AFF", "9" = "#EBCF2EFF", "10" = "#F6E58D")) +
  theme_minimal() +
  theme(legend.position = "none")



### recap fauche ----
data_ISG |>
  select(session_id,
         fauche,
         freq_fauche_simple, 
         frequence_fauches,
         per_simple,
         periodes_fauches,
         exportation_residus#,
         # paturages
  ) %>%
  unique() |>
  select(-session_id) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "modalite"
  ) |>
  ggplot(aes(x = modalite, fill = modalite)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(y = "nombre de sessions")




data_ISG |>
  select(site_id,
         fauche,
         freq_fauche_simple, 
         frequence_fauches,
         per_simple,
         periodes_fauches,
         exportation_residus#,
         # paturages
  ) %>%
  unique() |>
  select(-site_id) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "modalite"
  ) |>
  ggplot(aes(x = modalite, fill = modalite)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(y = "nombre de sites")


### recap paturage ----
df_paturage |>
  select(paturages_cat,
         session_id) %>%
  unique() %>%
  
  select(-session_id) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "modalite") |>
  
  ggplot(aes(x = modalite, fill = modalite)) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "nombre de sites",
       title = "Paturage - florileges")




df_paturage |>
  filter(paturages_cat == "P") %>%
  select(paturages,
         session_id,
         duree_annuelle_paturage_cat,
         pression_paturage_cat) %>%
  unique() %>%
  
  select(-session_id) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "modalite") |>
  
  ggplot(aes(x = modalite, fill = modalite)) +
  geom_bar() +
  coord_flip() +
  facet_wrap( ~ variable, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "nombre de sites",
       title = "detail paturage - florileges")




## réponse des indicateurs de biodiversité florileges à la fauche----

# frequence des especes sur une année par site
temp_freq= data_ISG %>%
  select(session_id, taxon,  observation_id, taxon_presence, ISG_new, score_fauche, score_fauche_sansbonus,site_id, session_year, score_fauche, score_fauche_sansbonus) %>%
  unique() %>% 
  group_by(site_id, session_year) %>%
  mutate(freq = sum(taxon_presence)) %>%
  ungroup()


# richesse en especes sur une année par site
temp_richesse = data_ISG %>%
  select(session_id,score_fauche, taxon,  observation_id, taxon_presence, score_fauche_sansbonus,site_id, session_year, score_fauche, score_fauche_sansbonus) %>%
  # sessions non vides
  filter(taxon_presence != 0) %>%
  # cas des sites pour lesquels il y a eu un changement de gestion
  group_by(site_id, session_year) %>% 
  reframe(richesse = n_distinct(taxon)) %>%
  ungroup() 


dt_indic_biodiv_fauche = temp_freq %>%
  left_join(temp_richesse) %>%
  # select(-observation_id, -taxon, -taxon_presence) %>%
  mutate(richesse = replace_na(richesse, 0)) %>%
  unique()




### diversite beta ----

temp = data_ISG %>%
  
  # travailler sur les sites pour lesquels on a un seul score de gestion renseigné
  group_by(site_id) %>%
  mutate(nb_score_gestion = n_distinct(score_fauche)) %>%
  ungroup() %>%
  
  filter(nb_score_gestion == 1) %>%
  
  # calcul de l'abondance moyenne par taxon par site
  select(site_id, session_year, ISG_new, session_id, taxon, taxon_presence, score_fauche, score_fauche_sansbonus) %>%
  group_by(site_id) %>%
  mutate(nb_an = n_distinct(session_year)) %>%
  ungroup() %>%
  group_by(site_id, taxon, score_fauche) %>%
  reframe(freq_mean = mean(taxon_presence)) %>% # on fait une moyenne car tous les sites ne sont pas suivis le même nombre d'années
  ungroup() %>%
  mutate(site_id_fauche = paste0(site_id, "---", score_fauche)) %>%
  filter(!is.na(score_fauche)) %>%
  select(-site_id)



# verifications des données par taxon par session
a = temp |>
  dplyr::summarise(n = dplyr::n(), .by = c(site_id_fauche, taxon)) |>
  dplyr::filter(n > 1L)

# a$session_id %in% id_anciens_releves # sessions anciennes ont 2 valeurs d'abondance pour certains taxons



matrice = temp %>%
  select(-score_fauche) %>% 
  unique() %>%
  pivot_wider(
    names_from = taxon,
    values_from = freq_mean,
    values_fill = 0
  )

matrice_finale <- matrice %>%
  tibble::column_to_rownames(var = "site_id_fauche")


library(vegan)
# enlever sites vides
matrice_clean <- matrice_finale[rowSums(matrice_finale) > 0, ]


dist_matrix <- vegdist(matrice_clean, method = "bray")


# heatmap(matrice_dist)

nmds <- metaMDS(dist_matrix, k = 2)
# plot(nmds)

scores_nmds <- as.data.frame(scores(nmds))
scores_nmds$site_id <- rownames(scores_nmds)
scores_nmds <- scores_nmds %>%
  mutate(site_id_fauche = site_id) %>%
  separate(site_id,
           into = c("site_id", "score_fauche"),
           sep = "---") %>%
  select(-site_id)


# calcul centroide et ellipses 
scores_nmds$score_fauche <- factor(scores_nmds$score_fauche)
centroids <- scores_nmds %>%
  group_by(score_fauche) %>%
  summarise(
    NMDS1 = mean(NMDS1),
    NMDS2 = mean(NMDS2)
  )



# test statistique de l'effet du scoring sur la composition des communautés
res_adonis <- adonis2(dist_matrix ~ score_fauche, data = scores_nmds)
res_adonis
R2 <- round(res_adonis$R2[1], 3)
pval <- res_adonis$`Pr(>F)`[1]

# verifier que dispersion non significative
bd <- betadisper(dist_matrix, scores_nmds$score_fauche)
anova(bd)


ggplot(scores_nmds, aes(x = NMDS1, y = NMDS2, color = score_fauche)) +
  geom_point(size = 1) +
  # ellipses (≈ dispersion des groupes)
  stat_ellipse(type = "t", level = 0.68, linewidth = 1) +
  
  # centroïdes
  geom_point(data = centroids, size = 5, linewidth = 1.5) +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf,
           label = paste0("PERMANOVA\nR² = ", R2,
                          "\np = ", format.pval(pval, digits = 2)),
           hjust = 1.1, vjust = 1.5) +
  labs(title = "florileges - effet score de fauche sur composition des communautés (distances Bray-Curtis)")


### fonctionnalité - recuperation des données ----

dt_traits = read.csv2("data/SpeciesTrait2024.csv") %>% filter(ListeFlorilege == 1)

dt_fonctionnalite = dt_indic_biodiv_fauche %>%
  left_join(df_florileges %>% select(taxon, taxon_nom_scientifique, taxon_presence) %>% unique) %>%
  mutate(
    taxon_nom_scientifique = case_when(
      taxon_nom_scientifique == "Silene latifolia subsp. alba" ~ "Silene latifolia",
      taxon_nom_scientifique == "Cerastium fontanum subsp. vulgare" ~ "Cerastium fontanum",
      taxon_nom_scientifique == "Plantago major subsp. major" ~ "Plantago major",
      taxon_nom_scientifique == "Medicago sativa subsp. sativa" ~ "Medicago sativa",
      taxon_nom_scientifique == "Taraxacum sectionruderalia" ~ "Taraxacum section ruderalia",
      # taxon_nom_scientifique == "Festuca rubra Gr." ~ "Festuca rubra",
      TRUE ~ taxon_nom_scientifique
    )
  ) %>%
  left_join(dt_traits %>% rename(taxon_nom_scientifique = Plant))  %>%
  # travailler sur les sites pour lesquels on a un seul score de gestion renseigné
  group_by(site_id) %>%
  mutate(nb_score_gestion = n_distinct(score_fauche)) %>%
  ungroup() %>%
  filter(nb_score_gestion == 1) 




### fonctionnalité : floraison ----

# durée moyenne (en mois) de la période de floraison des espèces présentes dans la communauté. Un score faible indique une floraison courte pour la communauté et donc une spécialisation marquée de la phénologie à un instant donné de l’année tandis qu’une période de floraison plus étendue reflète le caractère opportuniste (généraliste) de la communauté.
# flowering_long

 # dt_floraison =
  dt_fonctionnalite %>%
  filter(!is.na(score_fauche) ) %>%
  filter(richesse != 0) %>%
  filter(taxon_presence != 0) %>%
  filter(!is.na(flw_long)) %>%
  
  # calcul de la part que represente la plante dans la communauté étudiée
  group_by(site_id, score_fauche, score_fauche_sansbonus) %>%
  mutate(sum_freq = sum(taxon_presence)) %>%
  ungroup() %>%
  group_by(site_id, taxon, score_fauche, score_fauche_sansbonus) %>%
  reframe(prop_taxon = sum(taxon_presence) / sum_freq,
         score_floraison_taxon = prop_taxon * flw_long) %>%
  ungroup() %>%
  unique() %>%

  # moyenne interannuelle par site de la floraison
  group_by(site_id, score_fauche, score_fauche_sansbonus) %>%
  mutate(ind_floraison_site = sum(score_floraison_taxon)) %>%
  ungroup() %>%
  unique() %>%

  ggplot(aes(x = factor(score_fauche_sansbonus), y = ind_floraison_site)) +
  geom_boxplot() +
  labs(title = "Floraison en fonction du score de fauche sans bonus", 
       y = "indice de floraison = sum(part de l'espèce * durée floraison espèce)", 
       x = "score_fauche_sansbonus")


### fonctionnalité : typicité prairiale ----

# proportion d'espèces caractéristiques des milieux prairiaux

# dt_typicite_prairie =
dt_fonctionnalite %>%
  filter(!is.na(score_fauche)) %>%
  filter(!is.na(HABITAT) ) %>%
  filter(richesse != 0) %>%
  filter(taxon_presence != 0) %>%
  
  # calcul de la part que represente la plante dans la communauté étudiée
  group_by(site_id, score_fauche, score_fauche_sansbonus) %>%
  mutate(sum_freq = sum(taxon_presence)) %>%
  ungroup() %>%
  group_by(site_id, HABITAT, score_fauche, score_fauche_sansbonus) %>%
  reframe(prop_habitat = sum(taxon_presence) / sum_freq) %>%
  ungroup() %>%
  unique() %>%
  # filter(HABITAT == "prairie") %>%
  
  ggplot(aes(x = factor(score_fauche), y = prop_habitat, fill = HABITAT)) +
  geom_boxplot() +
  labs(title = "Part d'espèces typiques en fonction de score de fauche", 
       y = "proportion especes typiques par site", 
       x = "score_fauche")


### fonctionnalité : entomogamie ----

# part des espèces pollinisées par des insectes, indicateur de la dépendance de la communauté aux pollinisateurs

dt_fonctionnalite %>%
  filter(!is.na(score_fauche)) %>%
  filter(!is.na(BioticVector) ) %>%
  filter(richesse != 0) %>%
  filter(taxon_presence != 0) %>%
  
  # calcul de la part que represente la plante dans la communauté étudiée
  group_by(site_id, score_fauche, score_fauche_sansbonus) %>%
  mutate(sum_freq = sum(taxon_presence)) %>%
  ungroup() %>%
  group_by(site_id, BioticVector, score_fauche, score_fauche_sansbonus) %>%
  reframe(prop_habitat = sum(taxon_presence) / sum_freq) %>%
  ungroup() %>%
  unique() %>%
  # filter(BioticVector != 0) %>%
  
  ggplot(aes(x = factor(score_fauche_sansbonus), y = prop_habitat, fill = factor(BioticVector))) +
  geom_boxplot() +
  labs(title = "Part d'espèces entomogames (1) en fonction\n de score de fauche sans bonus", 
       y = "proportion especes entomogames (1) par site", 
       x = "score_fauchesansbonus")



### fonctionnalité : vivace ----

# part de vivaces





