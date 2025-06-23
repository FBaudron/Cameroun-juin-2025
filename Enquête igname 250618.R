#' ---
#' title: "Enquête igname"
#' author: "Frédéric Baudron"
#' date: "18 Juin 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# INSTALLING NECESSARY PACKAGES-------------------------------------------------

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('dplyr')) install.packages("dplyr")
if (!require('tidyr')) install.packages("tidyr")
if (!require('gtsummary')) install.packages("gtsummary")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggthemes')) install.packages("ggthemes")
if (!require('ggstats')) install.packages("ggstats")
if (!require('inspectdf')) install.packages("inspectdf")
if (!require('ggeffects')) install.packages("ggeffects")
if (!require('sjPlot')) install.packages("sjPlot")
if (!require('egg')) install.packages("egg")
if (!require('cowplot')) install.packages("cowplot")


# LOADING NECESSARY PACKAGES----------------------------------------------------


library(openxlsx)
library(dplyr)
library(tidyr)
library(gtsummary)
library(ggplot2)
library(ggthemes)
library(ggstats)
library(inspectdf)
library(ggeffects)
library(sjPlot)
library(egg)
library(cowplot)
library(geodata)
library(terra)
library(sf)
library(ggspatial)
library(tidyterra)


# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\Cameroun\\Igname\\")

path = 'D:\\Mes Donnees\\1. Cirad\\Cameroun\\Igname\\'

data = read.xlsx("2025 06 Variables igname_V3.xlsx", sheet = 1)


# DATA MANIPULATION-------------------------------------------------------------

data$Sexe_CM = ifelse(data$Sexe_CM == "femme", "Femme", data$Sexe_CM)
data$Sexe_CM = ifelse(data$Sexe_CM == "homme", "Homme", data$Sexe_CM)
data$Niveau_CM = ifelse(data$Niveau_CM == "aucun", "Aucun", data$Niveau_CM)
data$Niveau_CM = ifelse(data$Niveau_CM == "ecole_primaire", "Education primaire", data$Niveau_CM)
data$Niveau_CM = ifelse(data$Niveau_CM == "licence", "Licence", data$Niveau_CM)
data$Niveau_CM = ifelse(data$Niveau_CM == "master_maitrise", "Master/Maitrise", data$Niveau_CM)

data$Cult_ign_CM = ifelse(data$Cult_ign_CM == "chefmenage", "Chef du ménage", data$Cult_ign_CM)
data$Cult_ign_CM = ifelse(data$Cult_ign_CM == "enfant", "Enfant", data$Cult_ign_CM)
data$Cult_ign_CM = ifelse(data$Cult_ign_CM == "frere", "Frère", data$Cult_ign_CM)
data$Cult_ign_CM = ifelse(data$Cult_ign_CM == "mari", "Mari ou (co)épouse", data$Cult_ign_CM)
data$Sexe_Cult_ign = ifelse(data$Sexe_Cult_ign == "femme", "Femme", data$Sexe_CM)
data$Sexe_Cult_ign = ifelse(data$Sexe_Cult_ign == "homme", "Homme", data$Sexe_CM)
data$Niveau_Cult_Ign = ifelse(data$Niveau_Cult_Ign == "aucun", "Aucun", data$Niveau_Cult_Ign)
data$Niveau_Cult_Ign = ifelse(data$Niveau_Cult_Ign == "ecole_primaire", "Education primaire", data$Niveau_Cult_Ign)
data$Niveau_Cult_Ign = ifelse(data$Niveau_Cult_Ign == "licence", "Licence", data$Niveau_Cult_Ign)
data$Niveau_Cult_Ign = ifelse(data$Niveau_Cult_Ign == "master_maitrise", "Master/Maitrise", data$Niveau_Cult_Ign)

data$Acces_services = ifelse(data$Acces_services == "non_rarement", "Rarement", data$Acces_services)
data$Acces_services = ifelse(data$Acces_services == "non_service", "Non", data$Acces_services)

data$Type_Wdusol_1 = ifelse(data$Type_Wdusol_1 == "labourman", NA, data$Type_Wdusol_1)

data$Type_Wdusol_2 = ifelse(data$Type_Wdusol_2 == "0", "aucun", data$Type_Wdusol_2)

data$Parc_P_sup_igname = ifelse(data$Parc_P_sup_igname == "check", NA, data$Parc_P_sup_igname)

data$brulis = ifelse(data$brulis == "oui", TRUE, FALSE)


# ANALYSE DESCRIPTIVE-----------------------------------------------------------

# Chef de ménage----------------------------------------------------------------

cm = data[, c(1, 3:5)]

tb_cm = cm[, -c(1)] %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    digits = all_continuous() ~ 1,
    label = list(Sexe_CM ~ "Sexe du chef de ménage",
                 Age_CM ~ "Age du chef de ménage",
                 Niveau_CM ~ "Niveau d'éducation du chef de ménage")
  )

# write.xlsx(tb_cm, "Table chef de ménage.xlsx")


# CUlivateur igname-------------------------------------------------------------

cl = data[, c(1, 6:9)]

tb_cl = cl[, -c(1)] %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    digits = all_continuous() ~ 1,
    label = list(Cult_ign_CM ~ "Cultivateur d'igname",
                 Sexe_Cult_ign ~ "Sexe du cultivateur d'igname",
                 Age_Cult_ign ~ "Age du cultivateur d'igname",
                 Niveau_Cult_Ign ~ "Niveau d'éducation du cultivateur d'igname")
  )

# write.xlsx(tb_cl, "Table cultivateur igname.xlsx")


# Ménage------------------------------------------------------------------------

mn = data[, c(1, 10:18)]

tb_mn = mn[, -c(1)] %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    digits = all_continuous() ~ 1,
    type = list(Nb_parcelles ~ "continuous",
                Nb_parc_ign ~ "continuous"),
    label = list(Taille_menage ~ "Taille du ménage",
                 MO_agri_disp ~ "Main d'oeuvre agricole disponible",
                 Sup_cult ~ "Superficie cultivée (ha)",
                 Sup_cacao ~ "Superficie de cacao (ha)",
                 Sup_igname ~ "Superficie d'igname (ha)",
                 Nb_parcelles ~ "Nombre de parcelles",
                 Nb_parc_ign ~ "Nombre de parcelles d'igname",
                 UBT ~ "Bétail (UBT)",
                 Acces_services ~ "Accès aux services")
  )

# write.xlsx(tb_mn, "Table ménage.xlsx")


# Perceptions-------------------------------------------------------------------

pr = data[, c(1, 19:22)]

tb_pr = pr[, -c(1)] %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    digits = all_continuous() ~ 1,
    label = list(Percept_rdt ~ "Evolution des rendements en igname",
                 Percept_maladies ~ "Evolution des maladies de l'igname",
                 Percept_prix ~ "Evolutiond es prix d'igname",
                 Percept_conso ~ "Evolution de la consommatin d'igname")
  )

# write.xlsx(tb_pr, "Table perceptions.xlsx")


pr$Percept_rdt = factor(pr$Percept_rdt, levels = c("forteaugmentation", "augmentation", 
                                                    "const", "baisse", "fortebaisse"))

pr$Percept_rdt = factor(pr$Percept_rdt, labels = c("Forte augmentation", "Augmentation", 
                                                   "Constant", "Baisse", "Forte baisse"))

p1 = ggplot(data = pr, aes(x = "", fill = Percept_rdt)) +
  geom_bar(stat = "count", width = 0.6, color = "black") + 
  theme_few() + ylab("Evolution du rendement") +
  scale_fill_brewer(palette = "Set3", direction = 1) +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12,  face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.ticks.x = element_blank(),
        # legend.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) 


pr$Percept_maladies = factor(pr$Percept_maladies, levels = c("augmentation", "const", 
                                                   "baisse", "fortebaisse"))

pr$Percept_maladies = factor(pr$Percept_maladies, labels = c("Augmentation", "Constant", 
                                                   "Baisse", "Forte baisse"))

p2 = ggplot(data = pr, aes(x = "", fill = Percept_maladies)) +
  geom_bar(stat = "count", width = 0.6, color = "black") + 
  theme_few() + ylab("Evolution des maladies") +
  scale_fill_brewer(palette = "Set3", direction = 1) +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12,  face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.ticks.x = element_blank(),
        # legend.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) 


pr$Percept_prix = factor(pr$Percept_prix, levels = c("forteaugmentation", "augmentation",
                                                     "const"))

pr$Percept_prix = factor(pr$Percept_prix, labels = c("Forte augmentation", "Augmentation",
                                                     "Constant"))

p3 = ggplot(data = pr, aes(x = "", fill = Percept_prix)) +
  geom_bar(stat = "count", width = 0.6, color = "black") + 
  theme_few() + ylab("Evolution des prix") +
  scale_fill_brewer(palette = "Set3", direction = 1) +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12,  face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.ticks.x = element_blank(),
        # legend.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) 


pr$Percept_conso = factor(pr$Percept_conso, levels = c("augmentation", "const",
                                                     "baisse"))

pr$Percept_conso = factor(pr$Percept_conso, labels = c("Augmentation", "Constant",
                                                     "Baisse"))

p4 = ggplot(data = pr, aes(x = "", fill = Percept_conso)) +
  geom_bar(stat = "count", width = 0.6, color = "black") + 
  theme_few() + ylab("Evolution de la consommation") +
  scale_fill_brewer(palette = "Set3", direction = 1) +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12,  face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.ticks.x = element_blank(),
        # legend.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) 


# fig1 = ggarrange(p1, p2, p3, p4,   
#                  ncol = 2, nrow = 2, widths = c(1, 1), heights = c(1, 1))
# 
# ggdraw(fig1) +
#   theme(plot.background = element_rect(fill = "white"), 
#         plot.margin = margin(10, 10, 10, 10))
# 
# ggsave("Perceptions.png", units = "cm", width = 25, height = 20, dpi = 320)


# Parcelle----------------------------------------------------------------------

parc = data[, c(1, 23:28, 32:34, 38:39, 43:53, 55:56, 58:59, 63:64, 72:76)]

parc$Variete1 = ifelse(parc$Variete1 == "Igname  jaune", "Igname jaune", parc$Variete1)
parc$Variete1 = ifelse(parc$Variete1 == "Igname abomban", "Igname abomba", parc$Variete1)
parc$Variete1 = ifelse(parc$Variete1 == "Igname blanc", "Igname blanche", parc$Variete1)
parc$Variete2 = ifelse(is.na(parc$Variete2), '', parc$Variete2)
parc$Variete = paste(parc$Variete1, parc$Variete2, sep = " ")
parc$Variete = ifelse(parc$Variete == "Igname abomba ", "Igname abomba", parc$Variete)
parc$Variete = ifelse(parc$Variete == "Igname blanche ", "Igname blanche", parc$Variete)
parc$Variete = ifelse(parc$Variete == "Igname blanche Igname abomba", "Igname abomba Igname blanche", parc$Variete)
parc$Variete = ifelse(parc$Variete == "Igname jaune ", "Igname jaune", parc$Variete)
parc$Variete = ifelse(parc$Variete == "NA ", NA, parc$Variete)

parc$Decal_date_semis_1 = ifelse(parc$Decal_date_semis_1 == FALSE, NA, parc$Decal_date_semis_1)
parc$Decal_date_semis_1 = as.numeric(parc$Decal_date_semis_1)

parc$prov_semence_variete_2 = ifelse(is.na(parc$prov_semence_variete_2), '', parc$prov_semence_variete_2)
parc$Prov_semence = paste(parc$prov_semence_variete_1, parc$prov_semence_variete_2, sep = " ")
parc$Prov_semence = ifelse(parc$Prov_semence == "auto_produites ", "auto_produites", parc$Prov_semence)
parc$Prov_semence = ifelse(parc$Prov_semence == "magasin ", "magasin", parc$Prov_semence)
parc$Prov_semence = ifelse(parc$Prov_semence == "magasin magasin", "magasin", parc$Prov_semence)
parc$Prov_semence = ifelse(parc$Prov_semence == "NA ", NA, parc$Prov_semence)
parc$Prov_semence = ifelse(parc$Prov_semence == "ong ", "ong", parc$Prov_semence)
parc$Prov_semence = ifelse(parc$Prov_semence == "pepiniere ", "pepiniere", parc$Prov_semence)
parc$Prov_semence = ifelse(parc$Prov_semence == "voisin ", "voisin", parc$Prov_semence)

parc$Taille_semence = paste(parc$taille_semence_variete_1, parc$taille_semence_variete_2, sep = " ")
parc$Taille_semence = ifelse(parc$Taille_semence == "0 NA", NA, parc$Taille_semence)
parc$Taille_semence = ifelse(parc$Taille_semence == "demilamelle demilamelle", "demilamelle", parc$Taille_semence)
parc$Taille_semence = ifelse(parc$Taille_semence == "demilamelle NA", "demilamelle", parc$Taille_semence)
parc$Taille_semence = ifelse(parc$Taille_semence == "lamelle NA", "lamelle", parc$Taille_semence)
parc$Taille_semence = ifelse(parc$Taille_semence == "NA NA", NA, parc$Taille_semence)

parc$Recolte_variete_1 = ifelse(parc$Recolte_variete_1 == FALSE, NA, parc$Recolte_variete_1)
parc$Recolte_variete_1 = as.numeric(parc$Recolte_variete_1)
parc$Recolte_variete_1[is.na(parc$Recolte_variete_1)] = 0
parc$Recolte_variete_2[is.na(parc$Recolte_variete_2)] = 0
parc$Recolte = parc$Recolte_variete_1 + parc$Recolte_variete_2

parc$Rendement = parc$Recolte / parc$Sup_parcelle_igname


parc$Dureecycle_1 = ifelse(parc$Dureecycle_1 == FALSE, NA, parc$Dureecycle_1)
parc$Dureecycle_1 = as.numeric(parc$Dureecycle_1)

parc$Dureecycle_1 = ifelse(parc$Dureecycle_1 < 0, NA, parc$Dureecycle_1)
parc$Duree_cycle2 = ifelse(parc$Duree_cycle2 < 0, NA, parc$Duree_cycle2)

parc$Prix_vente_kg_1 = as.numeric(parc$Prix_vente_kg_1)
parc$Prix_vente_kg_2 = as.numeric(parc$Prix_vente_kg_2)
parc$Prix_vente_kg_1[is.na(parc$Prix_vente_kg_1)] = 0
parc$Prix_vente_kg_2[is.na(parc$Prix_vente_kg_2)] = 0

parc$Vente = ((parc$Recolte_variete_1 * parc$Prix_vente_kg_1) + (parc$Recolte_variete_2 * parc$Prix_vente_kg_2))/parc$Sup_parcelle_igname

parc$Vente = ifelse(parc$Vente == 0, NA, parc$Vente)


parc = parc[, c(1:17, 20, 27, 30:32, 35:40)]

tb_parc = parc[, -c(1)] %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    digits = all_continuous() ~ 1,
    type = list(Parc_nb_cult_association ~ "continuous",
                nb_sarclages ~ "continuous",
                Decal_date_semis_1 ~ "continuous"),
    label = list(Sup_parcelle_igname ~ "Superficie de la parcelle (ha)",
                 Parc_nb_cult_association ~ "Nombre de cultures associées",
                 Parc_P_sup_igname ~ "Proportion de la parcelle en igname (%)",
                 Parc_P_sup_cycl_court ~ "Proportion de la parcelle en cultures de cycle court (%)",
                 Parc_P_sup_cycl_long ~ "Proportion de la parcelle en cultures de cycle long (%)",
                 brulis ~ "Brûlis",
                 debrous_outil ~ "Outil de debroussaillage",
                 Type_Wdusol_1 ~ "Premier travail du sol",
                 Type_Wdusol_2 ~ "Second travail du sol",
                 Wdusol_outil ~ "Outil de travail du sol",
                 Wdusol_taille_butte ~ "Taille des buttes",
                 nb_sarclages ~ "Nombre de sarclages",
                 buttage_apres_semis ~ "Buttage après semis",
                 incidence_maladie ~ "Incidence des maladies",
                 incidence_ravageurs ~ "Incidence des ravageurs",
                 Decal_date_semis_1 ~ "Décallage de la date de semis (decades par rapport à la médiane)",
                 Dureecycle_1 ~ "Durée de cycle (jours)",
                 village ~ "Village",
                 tuteurage ~ "Tuteurage",
                 village ~ "Village",
                 Freq_rot ~ "Frequence de l'igname dans la rotation",
                 Variete ~ "Variété",
                 Prov_semence ~ "Provenance des semences",
                 Taille_semence ~ "Taille des semences",
                 Recolte ~ "Production (kg)",
                 Rendement ~ "Rendement (kg/ha)",
                 Vente ~ "Vente (XAF/ha)"))

write.xlsx(tb_parc, "Table parcelle.xlsx")


# Main d'oeuvre-----------------------------------------------------------------

mo = data[, c(1, 23, 29:30, 35:36, 40:41, 77:78, 80:81, 83:84, 86:87, 89:90)]

mo[, 2:18] = sapply(mo[, 2:18], as.numeric)
mo[, 2:18][is.na(mo[, 2:18])] = 0

mo$MO_fam_recolt = mo$MO_fam_recolt_1 + mo$MO_fam_recolt_2
mo$MO_sal_recolt = mo$MO_sal_recolt_1 + mo$MO_sal_recolt_2

mo = mo[, c(1:14, 19:20)]

# mo[, 3:16] = mo[, 3:16] / mo[, 2] 

mo$debrouss_nb_hj_MO = mo$debrouss_nb_hj_MO_fam + mo$debrouss_nb_hj_MO_sal
mo$Wdusol_nb_hj_MO = mo$Wdusol_nb_hj_MO_fam + mo$Wdusol_nb_hj_MO_sal
mo$semis_nbhj_MO = mo$semis_nbhj_MO_fam + mo$semis_nbhj_MO_sal
mo$sarclage_MO = mo$MO_fam_sarclage + mo$MO_sal_sarclage
mo$buttage_MO = mo$MO_fam_buttage + mo$MO_sal_buttage
mo$tute_MO = mo$MO_fam_tute + mo$MO_sal_tute
mo$recolt_MO = mo$MO_fam_recolt + mo$MO_sal_recolt

mo = mo[, c(1, 17:23)]

mo$sum = mo$debrouss_nb_hj_MO + mo$Wdusol_nb_hj_MO + mo$semis_nbhj_MO + mo$sarclage_MO +
  mo$buttage_MO + mo$tute_MO + mo$recolt_MO

mo_long = gather(mo[, -c(9)], Catégorie, MO, "debrouss_nb_hj_MO":"recolt_MO")

mo_long$Catégorie = as.factor(mo_long$Catégorie)

mo_long$MO = ifelse(is.na(mo_long$MO), 0, mo_long$MO)

mo_sum = aggregate(MO ~ id_menage, FUN = sum, na.rm = TRUE, data = mo_long)

m = median(mo_sum$MO)

mo_long$Catégorie = factor(mo_long$Catégorie, levels = c("sarclage_MO", "Wdusol_nb_hj_MO", "recolt_MO",
                                                         "semis_nbhj_MO", "tute_MO", "buttage_MO",
                                                         "debrouss_nb_hj_MO"))

mo_long$Catégorie = factor(mo_long$Catégorie, labels = c("Sarclage", "Travail du sol", "Récolte",
                                                         "Semis", "Tuteurage", "Buttage",
                                                         "Débroussaillage"))


ggplot(data = mo_long, aes(x = reorder(id_menage, -MO, sum), y = MO, fill = as.factor(Catégorie))) +
  geom_bar(stat = 'identity', width = 1, position = position_stack(reverse = TRUE), color = "black") + 
  geom_hline(yintercept = m, linetype = "dashed", color = "black", linewidth = 1) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set3", direction = 1) +
  xlab("Exploitations") + ylab("Main d'oeuvre (pers.jour/parcelle)") + 
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12,  face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.ticks.x = element_blank(),
        legend.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = c(0.9, 0.9), legend.justification = c(0.9, 0.9),
        plot.margin = margin(10, 10, 10, 10)) 

# ggsave("Main d'oeuvre finale.png", units = "cm", width = 25, height = 15, dpi = 320)



# Coût établissement------------------------------------------------------------

ct = data[, c(1, 23, 31, 37, 42, 54, 57)]

ct$cout_semence_var_1 = as.numeric(ct$cout_semence_var_1)
ct$cout_semence_var_2 = as.numeric(ct$cout_semence_var_2)

ct$cout_semence_var_1[is.na(ct$cout_semence_var_1)] = 0
ct$cout_semence_var_2[is.na(ct$cout_semence_var_2)] = 0

ct$cout_semence = ct$cout_semence_var_1 + ct$cout_semence_var_2

ct = ct[, c(1:5, 8)]

# ct[, 3:6] = ct[, 3:6] / ct[, 2] 

ct$sum = ct$debrouss_cout_MO_sal + ct$Wdusol_cout_MO_sal + ct$semis_cout_MO_sal + ct$cout_semence

ct_long = gather(ct[, -c(7)], Catégorie, Cout, "debrouss_cout_MO_sal":"cout_semence")

ct_sum = aggregate(Cout ~ id_menage, FUN = sum, na.rm = TRUE, data = ct_long)

m = median(ct_sum$Cout)

ct_long$Catégorie = as.factor(ct_long$Catégorie)

ct_long$Catégorie = factor(ct_long$Catégorie, levels = c("cout_semence", "Wdusol_cout_MO_sal", "debrouss_cout_MO_sal", "semis_cout_MO_sal"))

ct_long$Catégorie = factor(ct_long$Catégorie, labels = c("Semence", "Travail du sol", "Débroussaillage", "Semis"))


ggplot(data = ct_long, aes(x = reorder(id_menage, -Cout, sum), y = Cout, fill = as.factor(Catégorie))) +
  geom_bar(stat = 'identity', width = 1, position = position_stack(reverse = TRUE), color = "black") + 
  geom_hline(yintercept = m, linetype = "dashed", color = "black", linewidth = 1) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set3", direction = 1) +
  xlab("Exploitations") + ylab("Coût (XAF/parcelle)") + 
  theme_few() +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12,  face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 10, 0, 0)),
        axis.ticks.x = element_blank(),
        legend.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = c(0.9, 0.9), legend.justification = c(0.9, 0.9),
        plot.margin = margin(10, 10, 10, 10)) 

# ggsave("Coût final.png", units = "cm", width = 25, height = 15, dpi = 320)



# MODELES STATISTIQUE-----------------------------------------------------------

# FUNCTION FOR MODEL PERFORMANCE

resid.inspect = function(mod, col = "black"){
  resid = resid(mod)
  fitd = fitted(mod)
  par(mfrow = c(1, 3))
  hist(resid / sd(resid, na.rm = T), 30, main = "")
  plot(fitd, resid / sd(resid, na.rm = T), col = col)
  qqnorm(resid / sd(resid, na.rm = T))
  abline(coef = c(0, 1)) }


# MODELE RENDEMENT--------------------------------------------------------------

parc$Freq_rot = as.factor(parc$Freq_rot)

inspect_cat(parc) %>% show_plot()

parc_mod = parc[ , c(1:6, 12:14, 16:19, 21:24, 27:28)]

inspect_cat(parc_mod) %>% show_plot()


parc_mod$Freq_rot = as.factor(parc_mod$Freq_rot)
table(parc_mod$Freq_rot)

parc_mod$Freq_rot = ifelse(parc_mod$Freq_rot == "0", "0a1", parc_mod$Freq_rot)
parc_mod$Freq_rot = ifelse(parc_mod$Freq_rot == "2", "0a1", parc_mod$Freq_rot)
parc_mod$Freq_rot = ifelse(parc_mod$Freq_rot == "3", "2", parc_mod$Freq_rot)


parc_mod$Parc_P_sup_cycl_court = ifelse(parc_mod$Parc_P_sup_cycl_court == "40a60", "plus40", parc_mod$Parc_P_sup_cycl_court)
parc_mod$Parc_P_sup_cycl_court = ifelse(parc_mod$Parc_P_sup_cycl_court == "plus80", "plus40", parc_mod$Parc_P_sup_cycl_court)

parc_mod$Parc_P_sup_cycl_long = ifelse(parc_mod$Parc_P_sup_cycl_long == "0", "0a20", parc_mod$Parc_P_sup_cycl_long)
parc_mod$Parc_P_sup_cycl_long = ifelse(parc_mod$Parc_P_sup_cycl_long == "moins20", "0a20", parc_mod$Parc_P_sup_cycl_long)
parc_mod$Parc_P_sup_cycl_long = ifelse(parc_mod$Parc_P_sup_cycl_long == "40a60", "plus40", parc_mod$Parc_P_sup_cycl_long)
parc_mod$Parc_P_sup_cycl_long = ifelse(parc_mod$Parc_P_sup_cycl_long == "plus80", "plus40", parc_mod$Parc_P_sup_cycl_long)

parc_mod$Parc_P_sup_igname = ifelse(parc_mod$Parc_P_sup_igname == "0", "0a40", parc_mod$Parc_P_sup_igname)
parc_mod$Parc_P_sup_igname = ifelse(parc_mod$Parc_P_sup_igname == "20a40", "0a40", parc_mod$Parc_P_sup_igname)
parc_mod$Parc_P_sup_igname = ifelse(parc_mod$Parc_P_sup_igname == "60a80", "plus60", parc_mod$Parc_P_sup_igname)
parc_mod$Parc_P_sup_igname = ifelse(parc_mod$Parc_P_sup_igname == "plus80", "plus60", parc_mod$Parc_P_sup_igname)


parc_mod$Prov_semence
parc_mod$Prov_semence = ifelse(parc_mod$Prov_semence != "magasin", "autre",  parc_mod$Prov_semence)

parc$Variete = as.factor(parc$Variete)
parc_mod$Variete = ifelse(parc_mod$Variete == "Igname abomba Igname jaune", "Igname jaune seule ou associée",  parc_mod$Variete)
parc_mod$Variete = ifelse(parc_mod$Variete == "Igname blanche Igname jaune", "Igname jaune seule ou associée",  parc_mod$Variete)
parc_mod$Variete = ifelse(parc_mod$Variete == "Igname jaune", "Igname jaune seule ou associée",  parc_mod$Variete)
parc_mod$Variete = ifelse(parc_mod$Variete == "FALSE ", NA,  parc_mod$Variete)


parc_mod$Wdusol_taille_butte = ifelse(parc_mod$Wdusol_taille_butte == "0", "040cm",  parc_mod$Wdusol_taille_butte)
parc_mod$Wdusol_taille_butte = ifelse(parc_mod$Wdusol_taille_butte == "2040cm", "040cm",  parc_mod$Wdusol_taille_butte)
parc_mod$Wdusol_taille_butte = ifelse(parc_mod$Wdusol_taille_butte == "4060cm", "4080cm",  parc_mod$Wdusol_taille_butte)
parc_mod$Wdusol_taille_butte = ifelse(parc_mod$Wdusol_taille_butte == "6080cm", "4080cm",  parc_mod$Wdusol_taille_butte)

parc_mod$incidence_maladie = ifelse(parc_mod$incidence_maladie == "20a40", "plus20",  parc_mod$incidence_maladie)
parc_mod$incidence_maladie = ifelse(parc_mod$incidence_maladie == "40a60", "plus20",  parc_mod$incidence_maladie)
parc_mod$incidence_maladie = ifelse(parc_mod$incidence_maladie == "plus80", "plus20",  parc_mod$incidence_maladie)

parc_mod$incidence_ravageurs = ifelse(parc_mod$incidence_ravageurs == "40a60", "plus40",  parc_mod$incidence_ravageurs)
parc_mod$incidence_ravageurs = ifelse(parc_mod$incidence_ravageurs == "plus80", "plus40",  parc_mod$incidence_ravageurs)

inspect_cat(parc_mod) %>% show_plot()

parc_mod$buttage_apres_semis = as.factor(parc_mod$buttage_apres_semis)
parc_mod$Variete = as.factor(parc_mod$Variete)


# boxplot(parc_mod$Rendement)
parc_mod =  parc_mod[parc_mod$Rendement < 12000,]
# boxplot(parc_mod$Rendement)

parc_mod = na.omit(parc_mod)

mod_rdt = lm(Rendement ~ Sup_parcelle_igname + Parc_P_sup_igname + Parc_P_sup_cycl_court +
               Wdusol_taille_butte + Parc_P_sup_cycl_long + nb_sarclages +
               buttage_apres_semis + incidence_maladie + incidence_ravageurs +
               Variete + Decal_date_semis_1 + Dureecycle_1 + tuteurage + Freq_rot, data = parc_mod)

resid.inspect(mod_rdt)
summary(mod_rdt)

# write.xlsx(mod_rdt, "Model rendement.xlsx")

mod_rdta = step(mod_rdt)
summary(mod_rdta)
# write.xlsx(mod_rdta, "Model rendement réduit.xlsx")



ggcoef_model(mod_rdt)
# ggsave("Diagramme à moustaches model rendement.png", units = "cm", width = 25, height = 20, dpi = 320)


ggcoef_model(mod_rdta)
# ggsave("Diagramme à moustaches model rendement réduit.png", units = "cm", width = 25, height = 20, dpi = 320)


plot_model(mod_rdt, type = "pred", pred.type = 'fe', terms = "Variete")
predict_response(mod_rdt, terms = "Variete")


# MODELE VARIETES---------------------------------------------------------------

parc_var = parc_mod

parc_var$abomba = ifelse(parc_var$Variete == "Igname abomba", 1, 0)

mod_var_abomba = glm(abomba ~ Sup_parcelle_igname + Parc_P_sup_igname + Parc_P_sup_cycl_court +
                       Wdusol_taille_butte + Parc_P_sup_cycl_long + nb_sarclages +
                       buttage_apres_semis + incidence_maladie + incidence_ravageurs +
                       Decal_date_semis_1 + Dureecycle_1 + tuteurage + Freq_rot, 
                     data = parc_var, family = binomial(link = 'logit'))

resid.inspect(mod_var_abomba)
summary(mod_var_abomba)

# write.xlsx(mod_var_abomba, "Model abomba itinéraire technique.xlsx")

mod_var_abomba_a = step(mod_var_abomba)
summary(mod_var_abomba_a)

# write.xlsx(mod_var_abomba, "Model abomba itinéraire technique réduit.xlsx")


ggcoef_model(mod_var_abomba)
# ggsave("Diagramme à moustaches model abomba intinéraire technique.png", units = "cm", width = 25, height = 20, dpi = 320)

ggcoef_model(mod_var_abomba_a)
# ggsave("Diagramme à moustaches model abomba intinéraire technique réduit.png", units = "cm", width = 20, height = 15, dpi = 320)



plot_model(mod_var_abomba, type = "pred", pred.type = 'fe', terms = "Wdusol_taille_butte")
predict_response(mod_var_abomba, terms = "Wdusol_taille_butte")

plot_model(mod_var_abomba, type = "pred", pred.type = 'fe', terms = "buttage_apres_semis")
predict_response(mod_var_abomba, terms = "buttage_apres_semis")


# MODELES SOCIOECO--------------------------------------------------------------

socio = merge(parc_mod, mn, by = "id_menage", all.x = TRUE)
socio = merge(socio, cl, by = "id_menage", all.x = TRUE)

socio = socio[, c(1, 16, 18, 20:24, 27, 29:31)]

inspect_cat(socio) %>% show_plot()

socio$Cult_ign_CM = ifelse(socio$Cult_ign_CM != "Chef du ménage", "Autre", "Chef du ménage")

socio = na.omit(socio)

mod_rdt_socio = lm(Rendement ~ Taille_menage + MO_agri_disp + Sup_cult + 
                     Sup_cacao + Sup_igname + UBT + Cult_ign_CM + Sexe_Cult_ign +
                     Age_Cult_ign, data = socio)

resid.inspect(mod_rdt_socio)
summary(mod_rdt_socio)

# write.xlsx(mod_rdt_socio, "Model rendement variables socioeco.xlsx")


mod_rdt_socio_a = step(mod_rdt_socio)

summary(mod_rdt_socio_a)

socio$abomba = ifelse(socio$Variete == "Igname abomba", 1, 0)

mod_var_abomba_socio = glm(abomba ~ Taille_menage + MO_agri_disp + Sup_cult + 
                     Sup_cacao + Sup_igname + UBT + Cult_ign_CM + Sexe_Cult_ign +
                     Age_Cult_ign, data = socio, family = binomial(link = 'logit'))

resid.inspect(mod_var_abomba_socio)
summary(mod_var_abomba_socio)

# write.xlsx(mod_var_abomba_socio, "Model abomba variables socioeco.xlsx")

mod_var_abomba_socio_a = step(mod_var_abomba_socio)

summary(mod_var_abomba_socio_a)


# ANOVA et CHI SQUARED TESTS----------------------------------------------------

par(mfrow = c(1, 1))

#normality test
qqnorm(parc_mod$Rendement)
qqline(parc_mod$Rendement)

shapiro.test(parc_mod$Rendement) #non-normality if p-value lower than 0.05

# ANOVA (admettant une distribution normale)
M = aov(parc_mod$Rendement ~ parc_mod$Variete)
summary(M)

(M1.tukey = TukeyHSD(M))

# non parametric (comparision of medians)
kruskal.test(parc_mod$Rendement ~ parc_mod$Variete)

pairwise.wilcox.test(parc_mod$Rendement, parc_mod$Variete)


# Chi squared tests

summary(parc_mod$Variete)
parc_mod$tuteurage = as.factor(parc_mod$tuteurage)
tapply(parc_mod$tuteurage, parc_mod$Variete, summary) 

total = c(38, 10, 7, 6)
tuteur = c(13, 7, 2, 2)

prop.test(tuteur, total)


# CARTE-------------------------------------------------------------------------

# Country shapefiles
cmr0 = gadm(country='CMR', level = 0, path = "gadm")
cmr1 = gadm(country='CMR', level = 1, path = "gadm")
cmr2 = gadm(country='CMR', level = 2, path = "gadm")
cmr3 = gadm(country='CMR', level = 3, path = "gadm")

pos = terra::rast("satelligence_200428-deliverables-satelligence-cameroon-idh-wwf-zip_2020-04-28_1615\\200428 Deliverables Satelligence Cameroon IDH WWF\\CMR_GrandMbam_LULC.tif")

cmr3_df = terra::as.data.frame(cmr3)

ntui = terra::subset(cmr3, subset = cmr3$NAME_3 == "Ntui")

# terra::plot(ntui)
# terra::plot(pos)


pos_ntui = crop(pos, ntui)
pos_ntui = mask(pos_ntui, ntui)
terra::plot(pos_ntui)

pos_ntui_df = terra::as.data.frame(pos_ntui)
table(pos_ntui_df$CMR_GrandMbam_LULC)


cls = data.frame(id = c(10, 20, 30, 31, 40, 41, 500, 530, 600),
                 vegetation = c('Eau', 'Zone urbaine', 'Forêt primaire', 'Forêt secondaire', 
                                'Savane herbacée', 'Savane arbustive', 'Palmier à huile',
                                'Cacao', 'Cultures vivrières'))

levels(pos_ntui) = cls

spatial = data[, c(1, 68:69)]

parc_mod = merge(parc_mod, spatial, by = "id_menage", all.x = TRUE)

parc_sf = st_as_sf(parc_mod, coords = c("_gps_longitude", "_gps_latitude"), crs ="+proj=longlat + datum = WGS84 + no_defs")
parc_sf = st_jitter(parc_sf, factor = 0.05)
parc_sf = as(parc_sf, "Spatial")
parc_sf = vect(parc_sf)
parc_sf = project(parc_sf, "EPSG:4326")

# terra::plot(pos_ntui)
# terra::plot(parc_sf, add = T)

pos_ntui_df = terra::as.data.frame(pos_ntui, xy = TRUE)

ggplot() + theme_bw() +
  geom_raster(data = na.omit(pos_ntui_df), aes(x = x, y = y, fill = vegetation), alpha = 1) +
  geom_spatvector(data = parc_sf, shape = 19, color = "black", alpha = 1, size = 5) +
  geom_spatvector(data = ntui, color = "black", fill = NA, linewidth = 1) +
  scale_fill_manual(values = c("royalblue", "#e34a33", "#006837", "#31a354", "#d9f0a3","#78c679",
                               "#fee8c8", "#fdbb84","#ffffcc")) +
  # scale_color_gradient(low = "turquoise", high = "darkblue") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(fill = "Végétation", color = "Rendement") +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)) +
  annotation_scale(location = "tr", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 10))

# ggsave("Carte.png", units = "cm", width = 22, height = 25, dpi = 320)







