#' ---
#' title: "Formation R - bases"
#' author: "Frédéric Baudron"
#' date: "19-20 Juin 2025"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# INSTALLING NECESSARY PACKAGES-------------------------------------------------

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('dplyr')) install.packages("dplyr")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('GGally')) install.packages("GGally")
if (!require('gtsummary')) install.packages("gtsummary")
if (!require('emmeans')) install.packages("emmeans")
if (!require('multcomp')) install.packages("multcomp")
if (!require('agricolae')) install.packages("agricolae")
if (!require('egg')) install.packages("egg")
if (!require('cowplot')) install.packages("cowplot")


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(dplyr)
library(ggplot2)
library(GGally)
library(gtsummary)
library(emmeans)
library(multcomp)
library(agricolae)
library(egg)
library(cowplot)


#Ici je mets un dièse pour pouvoir insérer des commentaires, ce n'est pas du 
# code !

# SETTING UP THE DIRECTORY & LOADING THE DATA-----------------------------------

# Set your directory (where your input files are located, and where your output
# files will be save)

setwd("D:\\Mes Donnees\\1. Cirad\\Cameroun\\Formation\\")

# Load input files

data = read.xlsx("2025_06_Tableau_formation_R.xlsx", sheet = 1)


# DATAFRAMES AND VARIABLE-------------------------------------------------------

# dataframe (matrix)
data

# variable within a dataframe (vector)
data$Age_CM


#DEFINITION OF VARIABLES--------------------------------------------------------

# define variables; by default, numbers are read as continuous variables and 
# characters as factors. !! Cette étape est déterminante !!
str(data)
class(data$Age_CM)

class(data$Village)
data$Village = as.factor(data$Village)
class(data$Village)


# convert all character variables as factors
data = mutate_if(data, is.character, as.factor)
str(data)


#BASIC DATA MANIPULATION--------------------------------------------------------

# displays head
head(data)

# dislay variable names
names(data)

# display entire dataset
data

# retrieve the 24th row/observation point
data[24,]

# retrieve the 12th colum
data[, 12]

# retrieve the value in the 24th row of the 12th column
data[24, 12]

# retrieve several rows (24th and 36th). c() is a function used to create a 
# vector in R.
data[c(24, 36),] 

# selecting colums/variables 6 to 12.
data2 = data[, 6:12]

# selecting rows/entries 6 to 12
data3 = data[6:12, ]

# same thing as data2 above by dropping unwanted columns/variables
data4 = data[, -c(1:5, 13:23)]

# by default if no comma is used, the selection applies to columns/variables
data5 = data[6:12]

# subsetting using a factor for the rule
femme = data[which(data$Sexe_CM == "Femme"), ]

# removing data containing missing values
femme1 = na.omit(femme)

# removing data that doesn't have a GPS coordinate
femme2 = femme[!is.na(femme$`_Point.GPS_latitude`),]

# changing levels of a factorial variable
levels(data$Village) = c("Betamba", "Betamba", "Betamba", "Biagnimi", "Biagnimi", "Biatsota 1",
                       "Bidadjengué", "Bidadjengué", "Bilanga Kombe", "Bilanga Kombe", "Bilanga Kombe",
                       "Bindadjengué", "Bindadjengué", "Bindadjengué", "Biondjo", "Bipélé",           
                       "Bivouna", "Djame", "Djame", "Ehondo", "Nachtigal", "Nachtigal",
                       "Nachtigal", "Nachtigal", "Ndimi", "Djame", "Ndimi", "Nguette",         
                       "Nguette", "Nguette", "Nguila", "Ntui Centre", "Ossombe",
                       "Salakounou", "Yalongo")

table(data$Village)

# subsetting using a continuous variable for the rule
jeunes = data[data$Age_CM < 36,]
jeunes = na.omit(jeunes)

# subsetting using more than one rule (AND)
jeunes_femmes = data[(data$Age_CM < 36) & (data$Sexe_CM == "Femme"), ]
jeunes_femmes = na.omit(jeunes_femmes)

# subsetting using more than one rule (OR)
education_sup = data[(data$Educ_CM == "Lycée") | (data$Educ_CM == "Enseignement supérieur"), ]
education_sup = na.omit(education_sup)

# use of 'subset'
education_sup2 = subset(data, subset = Educ_CM == "Lycéé" | Educ_CM ==  "Enseignement supérieur")
education_sup2 = na.omit(education_sup2)


# CREATING NEW VARIABLES & SUMMARY STATS----------------------------------------

# create a new variable based on other variables in the dataset
data$Surf_cult_pp = data$Surf_cult / data$Taille_famille

# summary statistics for the entire dataset: minimum, maximum, quartiles, median
summary(data)

# summary statistics for one variable
summary(data$Surf_cult)
summary(data$Sexe_CM)

# mean and standard deviation
mean(data$Surf_cult) 
sd(data$Surf_cult)

# summary statistics by treatment groups
tapply(data$Surf_cult, data$Sexe_CM, summary) 

# ifelse function

data$Taille_famille_cat = ifelse(data$Taille_famille < 2, "Small", "Large")
data$Taille_famille_cat = as.factor(data$Taille_famille_cat)

summary(data$Taille_famille)
summary(data$Taille_famille_cat)


# PIVOT TABLES------------------------------------------------------------------

# from the package "dplyr"
village_pivot = group_by(data, Village)
village_pivot = summarise(village_pivot, 
                          meanAge_CM = mean(Age_CM),
                          maxAge_CM = max(Age_CM),
                          minAge_CM = min(Age_CM),
                          meanSurf_cult = mean(Surf_cult),
                          maxSurf_cult = max(Surf_cult),
                          minSurf_cult = min(Surf_cult))


# exclude missing values
village_pivot2 = group_by(data, Village)
village_pivot2 = summarise(village_pivot2, 
                          meanAge_CM = mean(Age_CM, na.rm = TRUE),
                          maxAge_CM = max(Age_CM, na.rm = TRUE),
                          minAge_CM = min(Age_CM, na.rm = TRUE),
                          sumSurf_cult = sum(Surf_cult, na.rm = TRUE),
                          meanSurf_cult = mean(Surf_cult, na.rm = TRUE),
                          maxSurf_cult = max(Surf_cult, na.rm = TRUE),
                          minSurf_cult = min(Surf_cult, na.rm = TRUE))


# BASIC GRAPHS------------------------------------------------------------------

# scatter plot
plot(data$Age_CM, data$Surf_cult) 

# boxplot
plot(data$Surf_cult ~ data$Sexe_CM) 
 
# histogram
hist(data$Surf_cult)

# pair plot
pairs(data[, c(10, 12:13, 21)],panel = panel.smooth)


# pie chart
pie(village_pivot2$sumSurf_cult, labels = village_pivot2$Village,
    main = "Distribution des surfaces cultivées par village")

# saving plot
jpeg("pie.jpg", unit = "cm", width = 20, height = 20, res = 320) # Open jpeg file
pie(village_pivot2$sumSurf_cult, labels = village_pivot2$Village,
    main = "Distribution des surfaces cultivées par village")
dev.off() # close the file

pdf("pie.pdf") # Open a pdf file
pie(village_pivot2$sumSurf_cult, labels = village_pivot2$Village,
    main = "Distribution des surfaces cultivées par village")
dev.off() # close the file


# SAME (BUT BETTER LOOKING) GRAPHS WITH GGPLOT2----------------------------------

# scatter plot
ggplot(data, aes(Age_CM, Surf_cult)) + 
  geom_point()

p1 = ggplot(data, aes(Age_CM, Surf_cult)) +
  geom_point(shape = 21, colour = "black", fill = "tomato", stroke = 1.2, size = 3) + 
  theme_bw() +
  xlab("Age du chef de ménage") + ylab("Surface cultivée (ha)") + 
  theme(axis.title = element_text(size = 16, face = "bold"), 
        axis.text = element_text(size = 12, face = "bold")) 

p1

# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# boxplot
p2 = ggplot(na.omit(data), aes(Sexe_CM, Surf_cult)) + 
  geom_boxplot(fill = "tomato") + 
  theme_bw() +
  xlab("") + ylab("Surface cultivée (ha)") + 
  theme(axis.title = element_text(size = 16, face = "bold"), 
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"))

p2

# histogram
p3 = ggplot(data, aes(Surf_cult)) + 
  geom_histogram(colour = "black", fill = "tomato") + 
  theme_bw() +
  xlab("Production (t)") + ylab("") + 
  theme(axis.title = element_text(size = 16, face = "bold"), 
        axis.text = element_text(size = 12, face = "bold"))

p3

# density plot
p4 = ggplot(data, aes(Surf_cult)) + 
  geom_density(colour = "black", fill = "tomato") + 
  theme_bw() +
  xlab("Production (t)") + ylab("") + 
  theme(axis.title = element_text(size = 16, face = "bold"), 
        axis.text = element_text(size = 12, face = "bold"))

p4

# pair plot from GGally
ggpairs(data[, c(10, 12:13, 21)],
        upper = list(continuous = wrap(ggally_cor, size = 5, color = "black")),
        diag = list(continuous = wrap("densityDiag", fill = "tomato", size = 1)),
        lower = list(continuous = wrap("points", alpha = 0.5, size = 4, color ="tomato"))) +
  theme_bw() + 
  theme(strip.text = element_text(size = 16),
        axis.text = element_text(size = 12)) 

# pie chart
p4 = ggplot(village_pivot2, aes(x = "", y = sumSurf_cult, fill = Village)) +
  geom_bar(width = 1, stat = "identity", color = "black")+
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.border = element_blank(), panel.grid = element_blank(),  
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.text = element_text(size = 16), legend.key.size = unit(1, 'lines'), 
        legend.title = element_blank(), legend.position = "bottom")  + 
  guides(fill = guide_legend(nrow = 4, byrow=TRUE))

# save a ggplot2
ggsave("pie ggplot2.png", units = "cm", width = 25, height = 20, dpi = 320)


# simple panel with facet
data_facet = data[!is.na(data$Sexe_CM),]

ggplot(data_facet, aes(Surf_cult, Rev_agri)) +
  geom_point(shape = 21, colour = "black", fill = "tomato", stroke = 1.2, size = 3) + 
  theme_bw() +
  xlab("Age du chef de ménage") + ylab("Surface cultivée (ha)") + 
  theme(axis.title = element_text(size = 16, face = "bold"), 
        axis.text = element_text(size = 12, face = "bold")) +
  facet_wrap(~ Sexe_CM, scales = "fixed")


fig1 = ggarrange(p1 + ggtitle("A"), p2 + ggtitle("B"), p3 + ggtitle("C"), p4 + ggtitle("D"),   
          ncol = 2, nrow = 2, widths = c(1, 1), heights = c(1, 1))

ggdraw(fig1) +
  theme(plot.background = element_rect(fill = "white"), 
        plot.margin = margin(10, 10, 10, 10))

ggsave("panel.png", units = "cm", width = 25, height = 20, dpi = 320)


# TABLES WITH GTSUMMARRY--------------------------------------------------------

names(data)

data_sexe = data[, c(9, 12:13, 15, 20:23)]

tbl = tbl_summary(data_sexe,
    by = Sexe_CM,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    digits = all_continuous() ~ 1)

tbl

# change labels
tbl = tbl_summary(data_sexe,
                  by = Sexe_CM,
                  statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{p}%"),
                  missing = "no",
                  digits = all_continuous() ~ 1,
                  label = list(Taille_famille ~ "Taille du foyer",
                               Surf_cult ~ "Surface cultivée (ha)",
                               Prop_vehicul_motor ~ "Propriété d'un véhicule motorisé",
                               Rev_elev ~ "Revenus de l'élevage (XAF/an)",
                               Rev_agri ~ "Revenus agricoles (XAF/an)",
                               UBT ~ "Animaux d'élevage (UBT)",
                               Rev_foret ~ "Revenus de la forêt"))

tbl

# add a column for overall sample
tbl = add_overall(tbl)

tbl

# saving
write.xlsx(tbl, "Table caractéristiques exploitation fonction sexe.xlsx")


#T TEST, ANOVA, CHI SQUARE, CORRELATION-----------------------------------------

#normality test
qqnorm(data$Age_CM)
qqline(data$Age_CM)

shapiro.test(data$Age_CM) #non-normality if p-value lower than 0.05

qqnorm(data$Surf_cult)
qqline(data$Surf_cult)
shapiro.test(data$Surf_cult)


# T test (1 sample per group, 2 groups). Assume normality.
t.test(data$Age_CM ~ data$Taille_famille_cat)

# non parametric (comparision of medians)
kruskal.test(data$Surf_cult ~ data$Taille_famille_cat)

# ANOVA
data$Educ_CM2 = data$Educ_CM
levels(data$Educ_CM2) = c("Aucun", "Secondaire ou plus", "Education primaire", "Secondaire ou plus", "Secondaire ou plus")

M = aov(data$Age_CM ~ data$Educ_CM2)
summary(M)

(M.tukey = TukeyHSD(M))

# from agrocolae
M.tukey = HSD.test(M, "Educ_CM2", group = TRUE, console = TRUE,
                   main = "Age du ched de menage en fonction de son niveau d'education")

plot(M.tukey)


# non parametric (comparision of medians)
kruskal.test(data$Surf_cult ~ data$Educ_CM2)

pairwise.wilcox.test(data$Surf_cult, data$Educ_CM2)

# Correltion tests
cor.test(data$Surf_cult, data$Age_CM)

# Chi squared tests

summary(data$Sexe_CM)
tapply(data$Appart_groupement, data$Sexe_CM, summary) 

total  = c(25, 106)
group  = c(9, 41)

prop.test(group, total)


# LINEAR MODELS & GENERALIZED LINEAR MODELS-------------------------------------

resid.inspect = function(mod, col = "black"){
  resid = resid(mod)
  fitd = fitted(mod)
  par(mfrow = c(1, 3))
  hist(resid / sd(resid, na.rm = T), 30, main = "")
  plot(fitd, resid / sd(resid, na.rm = T), col = col)
  qqnorm(resid / sd(resid, na.rm = T))
  abline(coef = c(0, 1)) }


# Simple GLM (no interaction)
M1 = glm(Rev_agri ~ Sexe_CM + Age_CM + Educ_CM + Taille_famille + Surf_cult + MO_salariee +
           Appart_groupement + UBT + P_sup_en_location + P_sup_cacao,
          data = data)

resid.inspect(M1)

boxplot(data$Rev_agri)

M1 = glm(Rev_agri ~ Sexe_CM + Age_CM + Educ_CM + Taille_famille + Surf_cult + MO_salariee +
           Appart_groupement + UBT + P_sup_en_location + P_sup_cacao,
         data = subset(data, Rev_agri < 50000000))

resid.inspect(M1)

summary(M1)

# saving output of the model
write.xlsx(M1, "Résultats modèle revenus agricoles.xlsx")


# Getting the least square means
# Least square means are means for groups that are adjusted for means of other
# factors in the model. 
means_sexe_M1 = emmeans(M1, ~ Sexe_CM)
cld(means_sexe_M1)

write.xlsx(data.frame(means_sexe_M1), "Moyennes sexe modèle revenus agricoles.xlsx")


# ajout d'interactions entre Age_CM et Educ_CM

M2 = glm(Rev_agri ~ Sexe_CM + Age_CM + Educ_CM + Taille_famille + Surf_cult + MO_salariee +
           Appart_groupement + UBT + P_sup_en_location * P_sup_cacao,
         data = subset(data, Rev_agri < 50000000))

resid.inspect(M2)

summary(M2)


# comparing moels with the Akaike information criterion

AIC(M1, M2)


# BINARY, PROPORTION & COUNT DATA------------------------------------------------

# Binary response variable

data$Ign_cult_01 = as.numeric(data$Ign_cult == "Oui")

M2 = glm(Ign_cult_01 ~ Sexe_CM + Age_CM + Educ_CM + Taille_famille + Surf_cult + MO_salariee +
            Appart_groupement + UBT + P_sup_en_location + P_sup_cacao, 
          family = binomial(link = 'logit'), data = data)

summary(M2)


# Proportion response variable

M3 = glm(P_sup_cacao ~ Sexe_CM + Age_CM + Educ_CM + Taille_famille + Surf_cult + MO_salariee +
            Appart_groupement + UBT + P_sup_en_location,
          family = binomial, data = data)

summary(M3)


# COunt data as response variable

levels(data$Educ_CM2) = c(0, 2, 1)

data$Educ_CM2 = as.numeric(data$Educ_CM2)

M4 = glm(Educ_CM2 ~ Sexe_CM + Age_CM + Taille_famille + Surf_cult + MO_salariee +
           Appart_groupement + UBT + P_sup_en_location + P_sup_cacao, 
          family = poisson(), data = data)

summary(M4)


