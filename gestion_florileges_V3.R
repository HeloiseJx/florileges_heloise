# Données produites par le script parent : elles constituent le jeu de données d'entrée.
df_florilege  = df_florileges


# 0. PRÉPARATION DES DONNÉES ET FONCTION DE MISE EN FORME ####
# Cette section prépare les données de gestion avant les contrôles de cohérence.
# La fonction ci-dessous standardise les réponses brutes et crée des facteurs
# dont les modalités sont ensuite utilisées par les filtres de qualité.

# Fonction de nettoyage et de recodage des variables de gestion.
# Elle transforme les réponses brutes (listes, libellés longs, valeurs 'null', etc.)
# en modalités courtes et homogènes, plus faciles à filtrer et analyser.
mise_en_forme_gestion <- function(df){
  
  ###### Type de fauche
  # Nettoie les réponses concernant le type de fauche et les transforme en codes courts.
  df_new = df %>%
    mutate(fauche = if_else(
      is.na(fauche), NA_character_,  # Conserve les valeurs manquantes afin de ne pas les confondre avec une réponse négative.
      case_when(
        fauche == "[]" ~ "PasFauche",
        TRUE ~sapply(str_extract_all(fauche, "broyage|faux|Pas de fauche|Je ne sais pas"), function(x) paste(sort(factor(x, levels = c("broyage", "faux", "Pas de fauche", "Je ne sais pas"))), collapse = " "))
      )
    ))
  
  df_new = df_new %>%
    mutate(fauche = if_else(
      is.na(fauche), NA_character_,  # Conserve les valeurs manquantes afin de ne pas les confondre avec une réponse négative.
      str_replace_all(fauche, c("broyage" = "FB", "faux" = "FT", "Pas de fauche" = "PF", "Je ne sais pas" = "NSP"))
    ))
  
  df_new$fauche = factor(df_new$fauche, levels = c("PF","FB","FT","NSP"))
  
  
  ###### Période de fauche
  # Identifie les périodes renseignées et impose un ordre stable aux combinaisons possibles.
  df_new = df_new %>%
    mutate(periodes_fauches = if_else(
      is.na(periodes_fauches), NA_character_,  # Conserve les valeurs manquantes afin de ne pas les confondre avec une réponse négative.
      case_when(
        periodes_fauches == "[]" ~ "PasFauche",
        TRUE ~sapply(str_extract_all(periodes_fauches, "estivale|precoce|tardive|je-ne-sais-pas"), function(x) paste(sort(factor(x, levels = c("precoce", "estivale", "tardive", "je-ne-sais-pas"))), collapse = " "))
      )
    ))
  # Vérifie que toutes les combinaisons attendues sont bien présentes dans les niveaux du facteur.
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
  
  
  ###### Fréquence de fauche
  # Recodage des fréquences de fauche en modalités courtes utilisées dans les analyses.
  df_new <- df_new %>%
    mutate(frequence_fauches = if_else(
      is.na(frequence_fauches), NA_character_,  # Conserve les valeurs manquantes afin de ne pas les confondre avec une réponse négative.
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
  # Harmonise les réponses sur l'exportation des résidus et conserve les valeurs manquantes.
  df_new = df_new %>%
    mutate(exportation_residus = if_else(
      is.na(exportation_residus), NA_character_,  # Conserve les valeurs manquantes afin de ne pas les confondre avec une réponse négative.
      str_replace(exportation_residus, "Je ne sais pas", "NSP"))
    )
  
  df_new$exportation_residus = factor(df_new$exportation_residus, levels = c("Oui","Non","NSP"))
  
  
  ##### Pâturage
  # Nettoie les réponses de pâturage et transforme les combinaisons d'animaux en codes courts.
  df_new = df_new %>%
    mutate(paturages = if_else(
      is.na(paturages), NA_character_,  # Conserve les valeurs manquantes afin de ne pas les confondre avec une réponse négative.
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
  
  ##### Pâturage
  # Nettoie les réponses de pâturage et transforme les combinaisons d'animaux en codes courts.s duree annuelle
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
  
  ##### Pâturage
  # Nettoie les réponses de pâturage et transforme les combinaisons d'animaux en codes courts.s pression
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


# Réduit le jeu de données aux informations nécessaires à la gestion.
# Les observations taxonomiques et plusieurs variables techniques sont retirées
# pour éviter de dupliquer les informations lors des étapes de correction.
# Prépare un jeu de données allégé, centré sur les variables de gestion.
df_florilege_red <- df_florilege %>% 
  select(-all_of(starts_with("taxon")),-observation_id,-cd_nom) %>% 
  select(-all_of(starts_with("tige")),-all_of(starts_with("date")),-all_of(starts_with("travaux")),-all_of(starts_with("frequence_t")),-all_of(starts_with("frequence_a")),-protocole,-site_zip_code) %>%
  distinct()

# Importe le tableau de référence utilisé pour compléter/corriger certaines informations de gestion.
df_audrey2 = read_excel("C:/Git/florileges code gabriel/florileges_HM/data/rdata/df_florileges/SiteYearGestion20250330.xlsx")

# Applique le nettoyage initial à toutes les variables de gestion.
df_gest <- mise_en_forme_gestion(df_florilege_red)





# 1. CORRECTION DES INFORMATIONS D'EXPORTATION DES RÉSIDUS ####
# Compare les informations de la base principale avec le fichier de référence
# et applique les corrections lorsque la source de référence est considérée comme prioritaire.

# Récupère uniquement l'identifiant du relevé et l'information d'exportation des résidus.
corr_export_res = df_audrey2 %>% 
  select(id_releve, ExportationResidus)

# Compare, pour chaque relevé, la valeur provenant de la base principale et celle du fichier de référence.
# La valeur du fichier de référence est utilisée pour corriger les incohérences identifiées., on compare ce qui est renseigné dans le df d'audrey vs ce qui est renseigné sur la bd mosaic (colonne diff_export qui paste les export de fauche des 2 df pour une même gestion)
# les informations d'audrey prévalent par rapport à celles de la BD mosaic
df_gest = df_gest %>% 
  left_join(corr_export_res, by = c("session_id" = "id_releve" ))
df_gest = df_gest %>% 
  mutate(diff_export = paste(exportation_residus,ExportationResidus, sep = "_"))
unique(df_gest$diff_export)



# Applique les corrections identifiées à partir de la comparaison précédente.
df_gest_corrExp = df_gest %>%
  mutate(exportation_residus = case_when(
    diff_export == "Non_Oui" ~ "Oui",
    TRUE ~ exportation_residus
  ))

df_gest_corrExp = df_gest_corrExp %>% 
  select(-ExportationResidus,-diff_export)

# À ce stade, l'information d'exportation des résidus est corrigée et remise sous forme de facteur.
df_gest_corrExp = df_gest_corrExp %>% 
  mutate(exportation_residus = factor(exportation_residus, levels = c("Oui","Non","NSP")))






# 2. CONTRÔLE DE COHÉRENCE ET FORMATAGE DES DONNÉES DE GESTION ####
# Les réponses de fauche et de pâturage sont comparées à des tables de règles.
# L'objectif est de distinguer les combinaisons cohérentes, incomplètes ou incohérentes.

##### 2.1 Fauche #####
# Construction des catégories intermédiaires puis application des règles de cohérence.
# Table de référence indiquant si les différentes combinaisons de variables de fauche sont cohérentes.
table_filtre_faucheNAper <- read_excel("C:/Git/florileges code gabriel/florileges_HM/data/rdata/tables_de_filtres/filtre_categorielle_fauche-ou-pas_NAper.xlsx")

###### 2.1.1 Création de catégories simplifiées pour les variables de fauche ######
# Chaque variable est ramenée à quelques catégories communes : fauche (F), pas de fauche (PF),
# information absente/inconnue (NANSP) ou problème (PRB).
# PRB = combinaison ou valeur qui ne correspond à aucune modalité attendue.
df_gest2 = df_gest_corrExp %>% 
  mutate(TF_cat = case_when(
    fauche == "PF" ~ "PF",
    fauche == "FB" | fauche == "FT" ~ "F",
    fauche == "NSP" | is.na(fauche) ~ "NANSP",
    TRUE ~ "PRB"
  ),
  # Déduit la présence ou l'absence de fauche à partir des périodes renseignées.
  Per_cat = case_when(
    periodes_fauches == "NSP" | periodes_fauches == "NoF"  ~ "NANSP",
    periodes_fauches %in% list("P","E","T","PE","PT","ET","PET") ~ "F",
    TRUE ~ "PRB"
  ),
  # Déduit la présence ou l'absence de fauche à partir de la fréquence renseignée.
# Le cas '< 1/an' est traité en fonction du type de fauche déjà identifié.
  Freq_cat = case_when(
    # frequence_fauches == "M1" ~ "PF",
    frequence_fauches == "M1" & TF_cat == "F" ~ "F", # cas des fauches bisannuelles
    frequence_fauches == "M1" & (TF_cat == "PF" | is.na(TF_cat) | TF_cat == "NANSP") ~ "PF",
    frequence_fauches == "NSP" | is.na(frequence_fauches) ~ "NANSP",
    frequence_fauches %in% list("E1","E2","P2") ~ "F",
    TRUE ~ "PRB"
  ),
  # Catégorise l'exportation des résidus : exportés (E), non exportés (NE),
# inconnus/manquants (NANSP) ou problème (PRB).
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



# Corrections manuelles de deux relevés connus pour contenir une période de fauche mal renseignée.
# Ces corrections sont spécifiques aux identifiants indiqués et doivent être documentées si elles évoluent.
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


###### 2.1.2 Application du filtre catégoriel ######
# Joint la table de règles pour obtenir le statut de cohérence de chaque combinaison.
# Pour chaque combinaison de catégories, récupère le statut de validité défini dans la table de référence.
df_gest2 = df_gest2 %>% 
  # Vérifie la cohérence de la combinaison type / fréquence / période / exportation.
  left_join(table_filtre_faucheNAper, by = c("TF_cat" = "typeF_cat", "Freq_cat" = "freq_cat", "Per_cat" = "per_cat", "Exp_cat" = "exp_cat"))



###### 2.1.3 Transformation de la fréquence et de la période en valeurs internes ######
# Convertit les modalités de fréquence et de période en nombres permettant de comparer
# leur niveau de détail/intensité dans l'étape suivante.
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

###### 2.1.4 Contrôle de compatibilité entre fréquence et période ######
# Vérifie que le nombre de périodes renseignées est compatible avec la fréquence de fauche.      
df_gest2 = df_gest2 %>% 
  mutate(comp_PerFr = case_when(
    Freq_catinterne >= Per_catinterne ~ "Oui",
    Freq_catinterne < Per_catinterne ~ "Non",
    TRUE ~ NA_character_),
    comp_globale = paste(OK,comp_PerFr,sep= "_") # Combine le contrôle issu de la table de règles avec le contrôle fréquence/période.
  )



###### 2.1.5 Création des variables de fauche corrigées ######
# Conserve uniquement les informations jugées utilisables et remplace les modalités invalides
# par NA, PF ou EXCLURE selon le statut du contrôle.
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



###### 2.1.6. ISG fauche : variable récapitulative ######
# Construit un identifiant synthétique du mode de gestion à partir des quatre variables de fauche.
# --> colonne ModeGestion

df_gest2 = df_gest2 %>% 
  mutate(ModeGestion = paste(fauche_corr,frequence_fauches_corr,periodes_fauches_corr,exportation_residus_corr, sep = "_"))

df_gest2 = df_gest2 %>% 
  mutate(ModeGestion = ifelse(ModeGestion == "PF_PF_PF_PF", "PF", ModeGestion))

# Compte, pour chaque relevé, le nombre de variables de fauche corrigées qui restent manquantes. 
df_gest2 = df_gest2 %>% 
  mutate(nbNA = rowSums(is.na(across(c(fauche_corr, frequence_fauches_corr, periodes_fauches_corr, exportation_residus_corr)))))







##### 2.2. Pâturage #####
# Même principe que pour la fauche : catégorisation, contrôle par table de règles,
# puis création de variables corrigées utilisables dans l'analyse.#
# Table de référence indiquant quelles combinaisons de pâturage sont cohérentes.
table_filtre_paturage <- read_excel("C:/Git/florileges code gabriel/florileges_HM/data/rdata/tables_de_filtres/filtre_categorielle_pat-ou-pas.xlsx")

###### 2.2.1 Création des catégories pâturage / absence de pâturage / information manquante ######
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


###### 2.2.2 Application du filtre catégoriel ######
# Joint la table de règles afin d'obtenir le statut de cohérence de chaque relevé.

df_gest3 = df_gest3 %>% 
  left_join(table_filtre_paturage, by = c("paturages_cat" = "paturage_cat" , "duree_annuelle_paturage_catpat" = "duree_catpat" , "pression_paturage_catpat" = "pression_catpat")) %>% 
  rename(pression_catpat = "pression_paturage_catpat",
         duree_catpat="duree_annuelle_paturage_catpat")




###### 2.2.3 Création des variables de pâturage corrigées ######
# Les valeurs sont conservées, transformées en absence de pâturage, mises à NA ou exclues
# selon le résultat du contrôle de cohérence.
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




###### 2.2.4 Corrections manuelles du pâturage ######
# Ces corrections reposent sur une expertise ou sur des informations complémentaires au jeu de données.
# Elles constituent donc des choix métier à conserver/documenter explicitement.
# ATTENTION : cette section contient des décisions manuelles à discuter/valider.

# Relevés considérés comme pâturés malgré un statut initial 'Non' ou 'Bof'.
liste_session_pat = list(9513,9776,10406,10285,9992,9673,9945,10126,9648,9466)
df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = ifelse(session_id %in% liste_session_pat,"PAT",paturage_cat_corr))

# Relevés considérés comme probablement/certainement pâturés malgré un statut initial 'Non' ou 'Bof'.
liste_session_problmnt_pat = list(9958,9632,9939,9772)
df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = ifelse(session_id %in% liste_session_problmnt_pat,"PAT",paturage_cat_corr))

# Relevés considérés comme non pâturés malgré un statut initial ambigu.
liste_session_no_pat = list(23043,25964,23238)
df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = ifelse(session_id %in% liste_session_no_pat,"NoPAT",paturage_cat_corr))

# Corrige également les détails du pâturage pour les relevés concernés.
# La durée et la pression sont conservées à 0 dans certains cas : ce choix reste discutable.
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



# Finalise le format des variables de pâturage avant de construire l'ISG global.
# HYPOTHÈSE FORTE : en l'absence d'information sur le pâturage, on considère qu'il n'y a pas de pâturage.
# Cette règle évite de perdre trop de relevés, mais elle doit être gardée à l'esprit lors de l'interprétation.

# paturage bovin, equin, caprin, ovin ou autre

df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = ifelse(is.na(paturage_cat_corr),"NoPAT",paturage_cat_corr),
         paturages_corr = ifelse(is.na(paturages_corr),"NoPAT",paturages_corr))
df_gest3 = df_gest3 %>%
  mutate(paturage_cat_corr = factor(paturage_cat_corr,levels = c("PAT","NoPAT","EXCLURE")),
         paturages_corr = droplevels(factor(paturages_corr, levels = c("NoPAT","Pb","Pc","Pe" , "Po","Pbc","Pbe","Pbo","Pce","Pco","Peo","Pbce","Pbco","Pbeo","Pceo","Pbceo","Pautre", "EXCLURE"))))



###### 2.2.5 ISG fauche + pâturage : construction du mode de gestion global ######
# Ajoute l'information de pâturage au mode de gestion issu de la fauche.
# --> colonne ModeGestionP

df_gest3 = df_gest3 %>% 
  mutate(ModeGestionP = ifelse(paturage_cat_corr == "NoPAT", ModeGestion, paste(ModeGestion, paturage_cat_corr, sep = "_")))

df_gest3 = df_gest3 %>%
  mutate(ModeGestion_restr = case_when(
    fauche_corr == "EXCLURE" | paturage_cat_corr == "EXCLURE" ~ "Incoherent",
    nbNA >0 ~ "Incomplet",
    TRUE ~ ModeGestionP
  ))

# Sauvegarde intermédiaire du jeu de données de pâturage pour un usage ultérieur éventuel.
df_paturage = df_gest3


df_gest4 = df_gest3 %>%  
  select(-c(duree_catpat,pression_catpat,TF_cat,Per_cat,Freq_cat,Exp_cat,TF_corr,Fr_corr,Per_corr,Exp_corr,OK,num,Freq_catinterne,Per_catinterne,comp_PerFr,pat_corr,duree_corr,pression_corr,num_pat))

rm(df_gest,df_gest2,df_gest_corrExp,corr_export_res,liste_session_no_pat,liste_session_pat,liste_session_problmnt_pat,table_filtre_paturage,table_filtre_faucheNAper)





# 3. FUSION DES SITES IDENTIQUES ####
# Harmonise les identifiants de sites lorsqu'un même site apparaît sous plusieurs identifiants.

##### 3.1 Ajout du nouvel identifiant de site #####
# Remplace l'ancien identifiant par l'identifiant harmonisé lorsque celui-ci existe.
# Importe la table définissant les correspondances entre anciens et nouveaux identifiants.
table_sites_identiques = read.csv2("C:/Git/florileges code gabriel/florileges_HM/data/rdata/tables_de_filtres/sites_indentiques_sur_et_certain.csv") 

df_gest4 <- df_gest4 %>% 
  # join les id corriges au df principal
  left_join(table_sites_identiques,by = c("site_id" = "Site_avant")) %>%
  mutate(site_id_corr = ifelse(is.na(Site_maintenant), site_id, Site_maintenant)) %>% 
  select(-Site_maintenant) %>%
  # Correction ponctuelle d'une erreur de saisie documentée dans le commentaire du relevé.
  mutate(site_id_corr = ifelse(session_id == 20689, 739, site_id_corr))


##### 3.2 Ajout des nouvelles valeurs de position #####
# La géométrie est rattachée au nouvel identifiant de site.
# Les autres informations du site restent associées au relevé, car elles peuvent varier dans le temps.
# Ne modifie que la geometrie, car c'est cette info qui nous a permis de dire que les sites sont identiques, en revanche les autres infos (qui peuvent changer meme aussi pour un meme site au cours de plusieurs releves) sont laissees telles quelles.

# Conserve une correspondance unique entre l'ancien identifiant du site et sa géométrie.
infos_geom_site = df_gest4 %>%
  select(site_id,site_geometry) %>% #,site_latitude,site_longitude
  rename(site_geometry_corr = "site_geometry"#,
         # site_longitude_corr = "site_longitude",
         # site_latitude_corr = "site_latitude"
  ) %>%
  distinct(.keep_all = T)
# Rattache la géométrie correspondant au nouvel identifiant de site.
df_gest4 <- df_gest4 %>%
  left_join(infos_geom_site,by = c("site_id_corr" = "site_id"))


# Tente de récupérer les autres informations du site à partir du nouvel identifiant.
# ATTENTION : ces informations peuvent varier selon les relevés ; leur harmonisation n'est donc pas entièrement automatisable.
site_infos = df_gest4 %>%
  select(site_id,site,site_geometry,site_departement,surface,objectifs,objectifs_autre,frequentation,occupations_sol_anterieures,occupations_sol_anterieures_autre,site_creation,amendements,amendements_autre) %>% distinct()
df_gest4 <- df_gest4 %>%
  left_join(site_infos, by = c("site_id_corr" = "site_id"), suffix = c("","_corr"))


# 4. FILTRAGE DES RELEVÉS REDONDANTS ####
# Identifie les relevés redondants et conserve uniquement ceux autorisés par la table de référence.

# Cas particulier : transfère les informations de gestion d'un relevé de référence vers un relevé ciblé.
# Cette correction manuelle concerne un relevé dont les informations floristiques existent mais pas les informations de gestion.
cols_a_modifer = c("fauche","periodes_fauches","frequence_fauches","exportation_residus","paturages","paturages_cat","fauche_corr","periodes_fauches_corr","frequence_fauches_corr","exportation_residus_corr","paturages_corr","paturage_cat_corr","pression_pat_corr","duree_pat_corr","ModeGestion","ModeGestionP","ModeGestion_restr","comp_globale","OK_pat")
gestion_a_recup <- df_gest4 %>% 
  filter(session_id == 26234) %>% 
  select(all_of(cols_a_modifer))
df_gest4 <- df_gest4 %>% 
  mutate(across(all_of(cols_a_modifer),
                ~if_else(session_id == 23118, gestion_a_recup[[cur_column()]], .)))



table_releve_redondant_a_conserver = read_excel("C:/Git/florileges code gabriel/florileges_HM/data/rdata/tables_de_filtres/table_filtre-releves-redondants.xlsx")
# Ne conserve dans la table de référence que les colonnes nécessaires au filtrage.
table_releve_redondant_a_conserver = table_releve_redondant_a_conserver %>% 
  select(session_id,statut_redond) 
# Ajoute le statut de conservation de chaque relevé à la table principale.
df_gest5 <- df_gest4 %>% 
  left_join(table_releve_redondant_a_conserver, by = "session_id")
# Par défaut, les relevés sont conservés ; seuls ceux explicitement marqués 'remove' sont retirés.
df_gest5 <- df_gest5 %>% 
  mutate(statut_redond = case_when(
    statut_redond == "remove" ~  "remove",
    TRUE ~ "keep"
  ))




# 5. FILTRAGE DES DONNÉES FLORISTIQUES ####  

##### 5.2 Contrôle des relevés selon les données de tiges ligneuses #####
# Calcule le nombre total de tiges ligneuses par relevé pour identifier les relevés sans données exploitables.
# Réduit à une seule ligne par session afin d'éviter de compter plusieurs fois les mêmes informations de gestion.
df_florilege_red2 = df_florilege %>% 
  distinct(session_id, .keep_all = T)
# Convertit les variables de comptage en numérique et calcule le nombre total de tiges par session.
tiges_sum = df_florilege_red2%>% 
  filter(session_date >= 2014) %>% 
  group_by(session_id) %>%
  mutate(across(starts_with("tiges_ligneuses_nombre_Q"), as.numeric)) %>% 
  summarise(across(starts_with("tiges_ligneuses_nombre_Q"), sum)) %>% 
  rowwise() %>% 
  mutate(nb_tiges = sum(across(starts_with("tiges_ligneuses_nombre_Q")))) %>%
  ungroup() %>%
  filter(nb_tiges >= 0)
# Marque les sessions sans information exploitable sur les tiges ligneuses.
tiges_sum = tiges_sum %>% 
  mutate(statut_ligneux = ifelse(is.na(nb_tiges),"remove","keep"))
# Exclut explicitement le relevé connu pour contenir un nombre de tiges négatif.
tiges_sum = tiges_sum %>% 
  mutate(statut_ligneux = ifelse(session_id == 9983 ,"remove",statut_ligneux))
# Crée des classes de nombre de tiges pour faciliter les analyses et les graphiques.
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
# Ajoute les informations sur les tiges ligneuses au jeu de données de gestion.
df_gest5 = df_gest5 %>% 
  left_join(tiges_sum,by = "session_id")



# 7. CONTRÔLE DES INFORMATIONS DE POSITION GÉOGRAPHIQUE ####

##### 7.1 Contrôle de la géométrie #####
# Repère les relevés pour lesquels la géométrie est absente afin de préparer une éventuelle correction.
# À terme, la correction nécessitera également les coordonnées longitude/latitude.
# Identifie les relevés présentant un problème de géométrie ; aucune correction automatique n'est appliquée ici.
releves_prb_geometry = df_gest5 %>%
  filter(is.na(site_geometry_corr)) %>%
  select(session_id,site_geometry_corr)

# 8. CONSTRUCTION DE data_ISG : APPLICATION DES FILTRES FINAUX ####
# Assemble les corrections précédentes puis élimine les relevés incohérents ou redondants.

# Supprime les objets intermédiaires devenus inutiles pour libérer de la mémoire et clarifier l'environnement.
rm(df_gest3,df_gest4,table_releve_redondant_a_conserver,table_sites_identiques,infos_geom_site,cols_a_modifer,gestion_a_recup,tiges_sum)

# Conserve uniquement les relevés dont le mode de gestion est cohérent et qui ne sont pas redondants.
df_corr_final <- df_gest5 %>% 
  filter(ModeGestion_restr != "Incoherent") %>%  # Enleve gestion incoherente
  filter(statut_redond == "keep") # Enleve releves redondants


# Sélectionne les variables finales et renomme les variables corrigées avec leurs noms d'origine.
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


# Calcule le score d'intensité de gestion (ISG) associé à chaque combinaison de gestion.
# Les modalités détaillées de fréquence et de période sont d'abord regroupées en classes simplifiées.
dt_ISG = df_corr_final %>% 
  select( ModeGestion_restr, fauche, frequence_fauches, periodes_fauches, exportation_residus, paturage_cat) %>%
  distinct() %>%
  filter() %>%
  # Simplifie les catégories de fréquence et de période pour construire le gradient d'intensité.
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
  # Principe du score : la fréquence et la période définissent le gradient principal d'intensité.
# Le type de fauche et l'exportation des résidus ajoutent des points complémentaires (bonus).
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
  mutate(score_fauche = case_when(freq_fauche_simple == "E12" & is.na(exportation_residus) ~ NA, 
                                  TRUE ~ score_1+score_2+score_3+score_4+bonus_score2 +bonus_score3 + bonus_score4a + bonus_score4b), 
         score_fauche_sansbonus = case_when(freq_fauche_simple == "E12" & is.na(exportation_residus) ~ NA,
                                            TRUE ~ score_1+score_2+score_3+score_4)) %>%
  # Gestion des valeurs manquantes dans le calcul du score. 
  # Si fréquence ou période est inconnue, le score d'intensité ne peut pas être déterminé.
  mutate(score_fauche = case_when(is.na(freq_fauche_simple) | is.na(per_simple) ~ NA,
                                  # En l'absence de fauche, le score est fixé à 1.
                                  fauche == "PF" ~ 1,
                                  TRUE ~ score_fauche),
         score_fauche_sansbonus = case_when(is.na(freq_fauche_simple) | is.na(per_simple) ~ NA,
                                            # En l'absence de fauche, le score est fixé à 1.
                                            fauche == "PF" ~ 1,
                                            TRUE ~ score_fauche_sansbonus))




# Jeu final : combine les données floristiques complètes avec les variables de gestion corrigées et l'ISG.
cols = c(colnames(dt_ISG) , colnames(df_corr_final)) %>% unique() %>% setdiff("session_id")

# Fusionne les informations de gestion corrigées, les scores ISG et les données floristiques.
data_ISG = df_corr_final %>% # modes de gestion associées aux sessions
  left_join(dt_ISG) %>% #scores associés aux modes de gestion
  left_join(df_florilege %>% select(-any_of(cols)) ) %>% # reste des infos du df florileges
  unique() %>%
  filter(taxon_liste_florilege == 1) %>% 
  select(-score_1, -score_2, -score_3, -score_4, -starts_with("bonus"), -ModeGestionP)

rm(cols, dt_ISG, df_corr_final)
