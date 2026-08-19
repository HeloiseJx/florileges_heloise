# données du script parent
df_florilege  = df_florileges


# 0. IMPORTATION MODULES ET DONNEES ####

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

rm(cols, dt_ISG, df_corr_final)
