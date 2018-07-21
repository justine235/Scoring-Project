#===================================================================================
#                                          
#                                                                                   #
#                                                                                   #
#                                 SCORING                                           #    
#                                                                                   #  
#                                                                                   #
# MOTTIER MANON                                                                     #         
# CHARLEY JUSTINE                                                                   #
#===================================================================================


#*********   SOMMAIRE   *********#
# I   - DECOUVERTE DE LA TABLE
# II  - TRAITEMENT DE LA TABLE 
# III - MODELISTION
# IV  - RESULTAT


#====================================== 0. ENVIRONNEMENT 
#=== 0.1) Librairies
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

my_library<- c("tidyverse","scales","data.table","highcharter","FactoMineR","factoextra", "smbinning",
               "mice", "Amelia", "missForest", "stats", "VIM" ,"gridExtra", "corrplot", "ade4", "readxl")
ipak(my_library)


#=== 0.2) Chemin de travail
#setwd("C:/Users/charl/Desktop/scoring")
setwd("C:/Users/charl/Desktop/Final/Final/Basedonnees")


#=== 0.3) Importation de la table
auto <- fread("ResiliationContratAuto.txt", header=T, encoding = "UTF-8", verbose = T, stringsAsFactors=TRUE)


#====================================== I   - DECOUVERTE DE LA TABLE
#=== 1.1 quick view
glimpse(auto)


#=== 1.2) Problemes : certains NA ne sont pas lu pendant l'importation, il faut donc les definir manuellement
# Different de SAS qui comprend directement que ces modalites sont des NA
auto[, 1:length(auto)[1]][(auto[, 1:length(auto)[1]])== ""] <- NA

#=== 1.3) Visualisation des valeurs manquantes sur R
auto_missing <- bind_cols(sapply(auto, function(x) sum(is.na(x))) %>% 
                            as.data.frame() %>% 
                            dplyr::rename(Missing_Values_Per_Variable = ".") %>% 
                            mutate(Vars = rownames(.)) %>% 
                            dplyr::select(Vars, Missing_Values_Per_Variable) %>% 
                            arrange(desc(Missing_Values_Per_Variable)),
                          sapply(auto, function(x) percent((sum(is.na(x)) / length(x)))) %>% 
                            as.data.frame() %>% 
                            dplyr::rename(Proportion_Missing_Values = ".") %>% 
                            arrange(desc(Proportion_Missing_Values)))

names(auto_missing) <- c("Variable", "Valeurs manquantes", "Proportion")

hchart(auto_missing %>% arrange(desc(`Valeurs manquantes`)), "column", 
       hcaes(x = Variable, y = `Valeurs manquantes`)) %>% 
  hc_title(text = "Visualition des missings values") 

# On se rends compte qu'il faudra traiter les NA pour une trentaine de variables.
# Afin d'introduire un nouveau langage, nous utilisons SAS pour un maximum d'entres elles
# SAS est tres pratique pour gerer les dates et le traitement simultane de plusieurs variables


# 1.4) Renommage des variables par une suffixe + noms plus explicite (via SAS)
# 1.5) Les proc freq sur SAS afin d'observer les modalites sous representees



#========================================== II  - TRAITEMENT DE LA TABLE  
#=== 2.0) Importation de la table traite via SAS
auto1 <- fread("table4.txt", header=T, encoding = "UTF-8", verbose = T, stringsAsFactors=TRUE)
# Il reste 7 variables avec des valeurs manquantes (moins de 1%)
apply(auto1,2,function(x) sum(is.na(x)))


#=== 2.1) Pre-traitement via R des variables que nous n'avons pas reussi e traiter sous SAS
# suppression des variables qui ne seront pas exploitable (date & numero de contrat)

#=== 2.2 ) Suite des pre-traitement de SAS (impossible de realiser ces operations via SAS)
# Nombre de modalites par variables 
sapply(auto1 %>% select_if(is.factor), function(x) length(unique(x)))

# I_CDUSGAUT
sort(table(auto1$I_CDUSGAUT),decreasing=TRUE)[1:2]
auto1$I_CDUSGAUT <- auto1$I_CDUSGAUT %>% as.numeric()
auto1$I_CDUSGAUT[is.na(auto1$I_CDUSGAUT)] <- 611

auto1 <- auto1 %>% mutate(I_CDUSGAUT = ifelse(auto1$I_CDUSGAUT == 611, "a",
                     ifelse(auto1$I_CDUSGAUT == 603, "b",
                     ifelse(auto1$I_CDUSGAUT == 601, "c",
                     ifelse(auto1$I_CDUSGAUT == 613, "d",
                     ifelse(auto1$I_CDUSGAUT == 624, "e",
                     ifelse(auto1$I_CDUSGAUT == 609, "f", "g")))))))
         

# I_COEFPFLT 
auto1 <- auto1 %>% mutate(I_COEFPFLT_D = ifelse(auto1$I_COEFPFLT <= 0.86 , "a",
                                         ifelse(auto1$I_COEFPFLT > 0.86 & auto1$I_COEFPFLT <= 1.0281 , "b","c")))



# I_CDMARVEH / I_LBMDLVH bien qu'ayant bcp de modalites, seront traites dans la partie suivante

# I_MMJECHPP
# on extrait uniquement le mois afin de calculer la date anniversaire du contrat
auto1$I_MMJECHPP <- as.character(auto1$I_MMJECHPP)
str_sub(auto1$I_MMJECHPP, -2, 4) <- ""
auto1$I_MMJECHPP <- as.numeric(auto1$I_MMJECHPP)
auto1$I_MMJECHPP[is.na(auto1$I_MMJECHPP)] <- 1

#=== 2.3 ) remplacement des NA 

# METHODE MICE
# On garde mice 
auto1_partielle <- auto1 %>% select(c(o_age_circulation_veh,o_age_indi,o_age_permis,I_RN_VL_VH,o_duree_anciennete))

Countries_predictors <- quickpred(auto1_partielle,
                                  include = NULL,
                                  exclude = NULL,
                                  mincor = 0.1)

auto1_partielle <- mice(data = auto1_partielle,
                        predictorMatrix = Countries_predictors,
                        m = 5, 
                        axit = 100)

auto1_partielle <- mice::complete(auto1_partielle)

# controle du remplacement manuelle
densityplot(auto1_partielle$o_age_circulation_veh)
densityplot(auto1$o_age_circulation_veh)

auto1 <- auto1 %>% select(-c(o_age_circulation_veh,o_age_indi,o_age_permis,I_RN_VL_VH,o_duree_anciennete))
auto1 <- cbind(auto1,auto1_partielle)



# variables converties en factor / charactere
auto1$I_CDUSGAUT <- as.factor(auto1$I_CDUSGAUT)
auto1$I_DEPT <- as.factor(auto1$I_DEPT)
auto1$I_MMJECHPP <- as.factor(auto1$I_MMJECHPP)
auto1$I_RN_VL_VH <- as.factor(auto1$I_RN_VL_VH)
auto1$I_COEFPFLT_D <- as.factor(auto1$I_COEFPFLT_D)


#== 2.4 ) traitements des outliers
# Selection des variables pour le squish
auto2 <- auto1 %>% select(c(o_duree_anciennete,I_MTPAAREF,I_COEFCOMM,
                            I_COEFPFLT,I_CRM,o_age_circulation_veh,o_age_contrat,
                            o_age_indi,o_age_permis,I_U,I_RN_VL_VH, I_MTPAATTC))

auto3 <- auto1 %>% select(-c(o_duree_anciennete,I_MTPAAREF,I_COEFCOMM,
                             I_COEFPFLT,I_CRM,o_age_circulation_veh,o_age_contrat,
                             o_age_indi,o_age_permis,I_U,I_RN_VL_VH, I_MTPAATTC))

# Application de la fonction de remplacement
replace_extrem <- function(x){
  if(is.factor(x) == F){
    x <- squish(x, range = c(min(x[x >= quantile(x, 0.025)]), 
                             max(x[x <= quantile(x, 0.975)]))
    )
  }
  x
}


auto2 <- auto2 %>% mutate_if(is.numeric, replace_extrem)

# comparaison avant / apres
grid.arrange(boxplot(auto2$o_duree_anciennete),boxplot(auto1$o_duree_anciennete))
grid.arrange(boxplot(auto2$I_MTPAAREF),boxplot(auto1$I_MTPAAREF))
grid.arrange(boxplot(auto2$I_COEFCOMM),boxplot(auto1$I_COEFCOMM))
grid.arrange(boxplot(auto2$I_CRM),boxplot(auto1$I_CRM))
grid.arrange(boxplot(auto2$o_age_circulation_veh),boxplot(auto1$o_age_circulation_veh))
grid.arrange(boxplot(auto2$o_age_contrat),boxplot(auto1$o_age_contrat))
grid.arrange(boxplot(auto2$o_age_indi),boxplot(auto1$o_age_indi))
grid.arrange(boxplot(auto2$o_age_permis),boxplot(auto1$o_age_permis))
grid.arrange(boxplot(auto2$I_U),boxplot(auto1$I_U))
grid.arrange(boxplot(auto2$I_RN_VL_VH),boxplot(auto1$I_RN_VL_VH))
grid.arrange(boxplot(auto2$I_MTPAATTC),boxplot(auto1$I_MTPAATTC))


# Focus sur la duree d'anciennete
summary(auto2$o_duree_anciennete)
summary(auto1$o_duree_anciennete)

auto1 <- cbind(auto2,auto3)


#=== 2.5 ) Passage en logarithme 
# Afoin de stabiliser la variance, on passe en log certaines variables
auto1 <- auto1 %>% mutate(I_MTPAAREF = log(I_MTPAAREF))
auto1$I_MTPAATTC <- as.numeric(as.character(auto1$I_MTPAATTC))
auto1 <- auto1 %>% mutate(I_MTPAATTC = log(I_MTPAATTC))



#== 2.6 ) discretisation des variables restantes 
auto1 <- auto1 %>% mutate(Y = ifelse(I_CONTRAT == "RESIL",1,0 ))

smbinning(df = auto1,y = "Y",x = "o_age_indi")
smbinning(df = auto1,y = "Y",x = "o_age_circulation_veh")
smbinning(df = auto1,y = "Y",x = "o_age_contrat")
smbinning(df = auto1,y = "Y",x = "o_age_permis")
smbinning(df = auto1,y = "Y",x = "o_duree_anciennete")

#o_age_indi : <50 / 50-78 / >78 
#o_age_circulation_veh : <2 / 2-7 / 9-14 / >14
#o_age_contrat : <6 / 6-14 / >14
#o_age_permis : < 21 / 20-30 / 30-51 / >51
#o_duree_anciennete : <6 / 6-12 / 12-31 / >31

auto1 <- auto1 %>% mutate(o_age_indi_d = ifelse(auto1$o_age_indi <=50, "a",
                                         ifelse(auto1$o_age_indi >50 &auto1$o_age_indi<=78 , "b",
                                         ifelse(auto1$o_age_indi >78, "c","d"))))

auto1 <- auto1 %>% mutate(o_age_circulation_veh_d = ifelse(auto1$o_age_circulation_veh <=2, "a",
                                         ifelse(auto1$o_age_circulation_veh >2 & auto1$o_age_circulation_veh<=7 , "b",
                                         ifelse(auto1$o_age_circulation_veh >7 & auto1$o_age_circulation_veh<=9 , "c",
                                         ifelse(auto1$o_age_circulation_veh >9 & auto1$o_age_circulation_veh<=14 , "d",
                                         ifelse(auto1$o_age_circulation_veh >14, "e", "f"))))))

auto1 <- auto1 %>% mutate(o_age_contrat_d = ifelse(auto1$o_age_contrat <=6, "a",
                                         ifelse(auto1$o_age_contrat >6 & auto1$o_age_contrat<=14 , "b",
                                         ifelse(auto1$o_age_contrat >14, "c", "d"))))



auto1 <- auto1 %>% mutate(o_age_permis_d = ifelse(auto1$o_age_permis <=21, "a",
                                            ifelse(auto1$o_age_permis >21 & auto1$o_age_permis<=30 , "b",
                                            ifelse(auto1$o_age_permis >30 & auto1$o_age_permis<=51 , "c",
                                            ifelse(auto1$o_age_permis >51, "d", "e")))))

auto1 <- auto1 %>% mutate(o_duree_anciennete_d = ifelse(auto1$o_duree_anciennete <=6, "a",
                                           ifelse(auto1$o_duree_anciennete >6 & auto1$o_duree_anciennete<=12 , "b",
                                           ifelse(auto1$o_duree_anciennete >12 & auto1$o_duree_anciennete<=31 , "c",
                                           ifelse(auto1$o_duree_anciennete >31, "d" ,"e")))))

auto1$o_age_indi_d <- auto1$o_age_indi_d %>% as.factor()
auto1$o_age_circulation_veh_d <- auto1$o_age_circulation_veh_d %>% as.factor()
auto1$o_age_contrat_d <- auto1$o_age_contrat_d %>% as.factor()
auto1$o_age_permis_d <- auto1$o_age_permis_d %>% as.factor()
auto1$o_duree_anciennete_d <- auto1$o_duree_anciennete_d %>% as.factor()



#=== 2.7 ) autres transformations, variables derivees etc ...
# Marque 
auto1$I_CDMARVEH <- auto1$I_CDMARVEH %>% as.character() 
auto1$I_CDMARVEH <- tolower(auto1$I_CDMARVEH)
auto1$I_CDMARVEH <- gsub("[[:punct:]]+", "", auto1$I_CDMARVEH)

auto1$I_CDMARVEH <- ifelse(str_detect(auto1$I_CDMARVEH, "nault|rene|renu") == TRUE, "renault", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "alfa") == TRUE, "alfa", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "alpine") == TRUE, "alpina", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "kswa|wage|volk|kwag|vw|v w|vv|wols|wv") == TRUE, "volkswagen", 
                    
                    ifelse(str_detect(auto1$I_CDMARVEH, "audi") == TRUE, "audi", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "austin") == TRUE, "austin", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "bwm|bmw") == TRUE, "bmw", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "golf") == TRUE, "golf",    
                    
                    ifelse(str_detect(auto1$I_CDMARVEH, "citroen|citro|citoren|cit") == TRUE, "citroen", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "ford|frod") == TRUE, "ford", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "land") == TRUE, "land", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "datsun") == TRUE, "datsun",  
                    
                    
                    ifelse(str_detect(auto1$I_CDMARVEH, "simca") == TRUE, "simca", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "talbo") == TRUE, "talbot", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "toyota") == TRUE, "toyota", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "daf") == TRUE, "daff", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "honda") == TRUE, "honda", 
                    
                    ifelse(str_detect(auto1$I_CDMARVEH, "fiat") == TRUE, "fiat", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "gmc") == TRUE, "gme", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "iveco") == TRUE, "iveco", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "mazd") == TRUE, "mazda", 
                    
                    ifelse(str_detect(auto1$I_CDMARVEH, "merce|mercd") == TRUE, "mercedes", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "opel") == TRUE, "opel", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "peug") == TRUE, "peugeot", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "will") == TRUE, "willys", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "teihol|teilhol") == TRUE, "teihol", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "seat") == TRUE, "seat", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "roover|rover") == TRUE, "rover",
                    
                    ifelse(str_detect(auto1$I_CDMARVEH, "lada") == TRUE, "lada", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "opel") == TRUE, "opel", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "peug") == TRUE, "peugeot", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "will") == TRUE, "willys", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "teihol|teilhol") == TRUE, "teihol", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "seat") == TRUE, "seat", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "roover|rover") == TRUE, "rover",
                    ifelse(str_detect(auto1$I_CDMARVEH, "mitsu") == TRUE, "mitsubis", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "roover|rover|range") == TRUE, "rover",
                    ifelse(str_detect(auto1$I_CDMARVEH, "nissan") == TRUE, "nissan", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "porsche") == TRUE, "porsche",
                    ifelse(str_detect(auto1$I_CDMARVEH, "chrysler|chrisler") == TRUE, "chrysler", 
                    ifelse(str_detect(auto1$I_CDMARVEH, "porsche") == TRUE, "porsche",
                    auto1$I_CDMARVEH)))))))))))))))))))))))))))))))))))))))))



resilie <- auto1 %>% filter(I_CONTRAT == "RESIL")
resilie <- resilie %>% select(c(I_CONTRAT,I_CDMARVEH)) %>% group_by(I_CDMARVEH) %>% count(n = n())
resilie <- resilie %>% arrange(desc(n))
# Renault, peugeot et citroen

auto1 <- auto1 %>% mutate(I_CDMARVEH2 = ifelse(I_CDMARVEH == "renault", "a",
                                        ifelse(I_CDMARVEH == "peugeot", "b",
                                        ifelse(I_CDMARVEH == "citroen", "c","d"))))


# Departement 
resilie <- auto1 %>% filter(I_CONTRAT == "RESIL")
resilie <- resilie %>% select(c(I_CONTRAT,I_DEPT)) %>% group_by(I_DEPT) %>% count(n = n())
resilie <- resilie %>% arrange(desc(n))
# 59, 33, 6 sont les departements ayant le plus d'accident 

auto1 <- auto1 %>% mutate(I_DEPT2 = ifelse(I_DEPT == "59", "a",
                                    ifelse(I_DEPT == "33", "b",
                                    ifelse(I_DEPT == "6", "c","d"))))



# serie - variable 1
resilie <- auto1 %>% filter(I_CONTRAT == "RESIL")
resilie <- resilie %>% select(c(I_CONTRAT,I_LBMDLVH)) %>% group_by(I_LBMDLVH) %>% count(n = n())
resilie <- resilie %>% arrange(desc(n))
#205, CLIO, R 5 SUPERCINQ

auto1 <- auto1 %>% mutate(I_LBMDLVH2 = ifelse(I_LBMDLVH == "205", "a",
                                       ifelse(I_LBMDLVH == "CLIO", "b",
                                       ifelse(I_LBMDLVH == "R 5 SUPERCINQ", "c","d"))))




auto1 <- auto1 %>% mutate(o_marque =
                            ifelse(I_CDMARVEH == "renault"|I_CDMARVEH == "peugeot" |I_CDMARVEH == "citroen" |I_CDMARVEH == "fiat" |I_CDMARVEH == "opel", "a",
                            ifelse(I_CDMARVEH == "bmw"|I_CDMARVEH == "mercedes" |I_CDMARVEH == "audi","b",
                            ifelse(I_CDMARVEH == "porshe"|I_CDMARVEH == "ferrari","c", "d"
                                                  ))))





#=== 2.8) explorations visuelles
# graphique 1
t <- auto1 %>% select(I_MMJECHPP) %>% group_by(I_MMJECHPP) %>% count(n = n())
t$I_MMJECHPP <- as.numeric(as.character(t$I_MMJECHPP))

highchart() %>% 
  hc_yAxis_multiples(
    list(showLastLabel = FALSE, opposite = TRUE)) %>% 
  hc_add_series(t, "bar", color = "#CCCC99", name = "Mois d'anniversaire", hcaes(x = I_MMJECHPP, y = n)) %>% 
  hc_xAxis(categories = t$I_MMJECHPP) %>% 
  hc_title(text = "Mois d'anniversaire")


# graphique 2 
t <- auto1 %>% select(I_CDMARVEH2) %>% group_by(I_CDMARVEH2) %>% 
  count(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
highchart() %>% 
  hc_yAxis_multiples(
    list(showLastLabel = FALSE, opposite = TRUE)) %>% 
  hc_add_series(t, "bar", color = "#CCCC99", name = "Marques", hcaes(x = I_CDMARVEH2, y = n)) %>% 
  hc_xAxis(categories = t$I_CDMARVEH2) %>% 
  hc_title(text = "Les marques les plus representees")


# graphique 3
t <- auto1 %>% select(I_CONTRAT) %>% group_by(I_CONTRAT) %>% 
  count(n = n()) 

highchart() %>% 
  hc_chart(type = "pie") %>% 
  hc_add_series_labels_values(labels = t$I_CONTRAT , values = t$n)  


# graphique 4
t <- auto1 %>% select(o_nb_contrat_res) %>% group_by(o_nb_contrat_res) %>% count(n = n())

highchart() %>% 
  hc_yAxis_multiples(
    list(showLastLabel = FALSE, opposite = TRUE)) %>% 
  hc_add_series(t, "line", color = "#CCCC99", name = "Resiliation", hcaes(x = o_nb_contrat_res, y = n)) %>% 
  hc_xAxis(categories = t$o_nb_contrat_res) %>% 
  hc_title(text = "Nombre de contrat resilies")


# graphique 5
t <- as.data.frame(auto1$o_s_n + auto1$o_s_o)
colnames(t) <- "nb"
t <- t %>% group_by(nb) %>% count(n = n())

highchart() %>% 
  hc_yAxis_multiples(
    list(showLastLabel = FALSE, opposite = TRUE)) %>% 
  hc_add_series(t, "line", color = "#CCCC99", name = "Sinistres", hcaes(x = nb, y = n)) %>% 
  hc_xAxis(categories = t$nb) %>% 
  hc_title(text = "Nombre d'accident sur 3 ans")








#=== 2.9 ) Transformations des variables en dummy

# Passage en factor
auto1$I_CDMARVEH2 <- auto1$I_CDMARVEH2 %>% as.factor()
auto1$I_DEPT2 <- auto1$I_DEPT2 %>% as.factor()
auto1$I_LBMDLVH2 <- auto1$I_LBMDLVH2 %>% as.factor()
auto1$o_marque <- auto1$o_marque %>% as.factor()

# Suppression des variables inutiles
auto1$I_LBMDLVH <- NULL
auto1$I_NOTAREFF <- NULL
auto1$I_CLIACTIF <- NULL
auto1$I_CDSITFAM <- NULL

auto1$I_CDMARVEH <- NULL
auto1$I_ETAT <- NULL
auto1$I_DEPT <- NULL


factor <- auto1 %>% select_if(is.factor) %>% select(-I_CONTRAT) # 24
other <- auto1 %>% select_if(is.numeric) # 40
other2 <- auto1$I_CONTRAT %>% as.data.frame()#1
colnames(other2) <- "I_CONTRAT"
other <- cbind(other, other2) # 41

dichot <- acm.disjonctif(subset(factor, select = colnames(factor)))
auto1_d <- cbind(other, dichot) 

#=== 2.9 ) matrice de correlation
auto1_numeric <- auto1 %>% select_if(is.numeric) %>% select(-I_MTPAATTC) %>% na.omit()
hchart(cor(auto1_numeric))



#=== 2.10) Exportation et sauvegarde des resultats
write.csv(auto1_d,"auto5_d.csv")
saveRDS(auto1_d, "auto5_d.RDS")
#auto1_bis <- readRDS("auto5_d.RDS")




