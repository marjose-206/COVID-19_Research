library("data.table")
library("dplyr")
library("ISLR")
library("parallel") 

# ## one time code run below VVVVVVVVVV
# #read the original dataset
# Covid <- fread("covid_clean.txt")
# #remove the index column
# Covid <- Covid[,-1]
# 
# #attain the covid positive subset
# cov.pos3 <- filter(Covid, COVID_FLAG == 1)
# #see the Na in the cov.pos and remove the NA in Age
# colSums(is.na(cov.pos3))
# #remove 127 NA  in AGE
# cov.pos3 <- cov.pos3[!is.na(cov.pos3$AGE),]
# #removing the columns that would not work for random forest
# cov.pos3 <- cov.pos3[,-c("RACE_Caucasian",
#                    "REGION_Northeast", "DIVISION_Other/Unknown", "DIVISION_South_Atl/West_South_Crl",
#                    "DIVISION_East_North_Central", "DIVISION_East_South_Central", "DIVISION_Middle_Atlantic",
#                    "DIVISION_Mountain", "DIVISION_New_England", "DIVISION_Pacific", "DIVISION_West_North_Central",
#                    "ETHNICITY_Hispanic",
#                    "GENDER_Male", "POSITIVE_COUNT", "NEGATIVE_COUNT", "UNKNOWN_COUNT", "DATE_OF_DEATH", "BIRTH_YR")]
# #write out the final clean covid positive dataset
# write.csv(cov.pos3, file = "cov_pos.txt",row.names = FALSE)


#read in the covid positive dataset to attain the positive PTID
cov.pos <- fread("cov_pos.txt")

pos.PTID <- as.data.frame(cov.pos$PTID)
colnames(pos.PTID) <- c("PTID")

#read the posDiag.txt  created from unix word document 
posDiag <- fread("posDiag.txt")
#remove the date
posDiag <- posDiag[, -2]

n <- posDiag %>% count(DIAGNOSIS_CD, DIAGNOSIS_CD_TYPE, sort = TRUE)


#read the general diag 
nPosDiag <- fread("posDiag0_9.csv")
nPosDiag <- nPosDiag[!apply(nPosDiag == "", 1, all), ]    # remove all NA cell rows
#removed the other symptoms and factors health status
nPosDiag1 <- nPosDiag[!which(nPosDiag$General == "Factors_influencing_Health_status"), ]
nPosDiag1 <- nPosDiag1[!which(nPosDiag1$General == "Abnormal_clinical_Findings"), ]
#remove the other 
PosDiag <- nPosDiag1[!which(nPosDiag1$DIAGNOSIS_CD_TYPE == "OTHER"),]
total_n <- sum(PosDiag$n)
PosDiag$percentage <- PosDiag$n / total_n

top.500 <- PosDiag[1:500, ]
top.500$percentage <- top.500$n / sum(top.500$n)
#read the sub diag 
nPosDiagSub <- fread("posDiagSub.csv")
nPosDiagSub <- nPosDiagSub[!apply(nPosDiagSub == "", 1, all), ]    # remove all NA cell rows
nPosSub1 <- nPosDiagSub[!which(nPosDiagSub$General == "Factors_influencing_Health_status"), ]
nPosSub1 <- nPosSub1[!which(nPosSub1$General == "Abnormal_clinical_Findings"), ]
nPosSub1 <- nPosSub1[!which(nPosSub1$General == "External_causes_of_Morbidity"), ]
#the general with sub categories
PosDiagSub <- nPosSub1[!which(nPosSub1$DIAGNOSIS_CD_TYPE == "OTHER"),]
PosDiagSub <- nPosSub1[!which(nPosSub1$DIAGNOSIS_CD_TYPE == "SNOMED"),]
PosDiagSub <- nPosSub1[!which(nPosSub1$DIAGNOSIS_CD_TYPE == "UNKNOWN"),]
PosDiagSub <- nPosSub1[!which(nPosSub1$DIAGNOSIS_CD_TYPE == ""),]
PosDiagSub <- nPosSub1[!which(nPosSub1$General == ""),]
total_n <- sum(PosDiagSub$n)
PosDiagSub$percentage <- PosDiagSub$n / total_n

#get the top 500 with the sub categories 
top.500 <- PosDiagSub[1:500, ]
top.500$percentage <- top.500$n / sum(top.500$n)

#get the counts of each sub categories 
dat <- top.500 %>%  group_by(General, subCat) %>% 
  dplyr::summarise(COUNT = n(), SUM_OBS = sum(n))
dat$percentage <- dat$SUM_OBS / sum(dat$SUM_OBS)

cov.pos2 <- cov.pos 
diag.sub.names <- unique(dat$subCat)#get the colnames
diag.gen.names <- unique(dat$General)
#add columns to cov.pos of diagnosis 
cov.pos[, diag.sub.names] <- 0
cov.pos2[, diag.gen.names] <- 0


Anames <- as.list(unique(top.500$subCat))
#find diag that follow with the same sub cat names
namesDiag <- mapply(function(A)  as.vector(unique(top.500$DIAGNOSIS_CD[top.500$subCat == A])), Anames)
#find the PTIDS that are identified with same pre-existing diagnosis
idDiag <- mcmapply(function(B)  posDiag$PTID[posDiag$DIAGNOSIS_CD %in% B] , namesDiag)

#assign 1 for all PTIDS identified the 126 diagnosis colums
cov.pos[cov.pos$PTID %in% unlist(idDiag[1]),]$`Hypertensive diseases` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[2]),]$`Metabolic disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[3]),]$`Acute upper respiratory infections` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[4]),]$`Diseases of esophagus, stomach, and duodenum` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[5]),]$`Acute respiratory infections` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[6]),]$`Obesity and other hyperalimentation` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[7]),]$`Other metabolic and immunity disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[8]),]$`Rheumatism, excluding the back ` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[9]),]$`Cerebral palsy and other paralytic syndromes` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[10]),]$`Other dorsopathies` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[11]),]$`Neurotic, stress-related and somatoform disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[12]),]$`Provisional assignment of new diseases of uncertain etiology or emergency use` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[13]),]$`Mood [affective] disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[14]),]$`Diabetes mellitus` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[15]),]$`Other nutritional deficiencies` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[16]),]$`Dorsopathies` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[17]),]$`Neurotic disorders, personality disorders, and other nonpsychotic mental disorders ` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[18]),]$`Other diseases of the upper respiratory tract` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[19]),]$`Arthropathies and related disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[20]),]$`Other diseases of urinary system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[21]),]$`Chronic lower respiratory diseases` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[22]),]$`Diseases of other endocrine glands` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[23]),]$`Disorders of thyroid gland` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[24]),]$`Aplastic and other anaemias` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[25]),]$`Other acute lower respiratory infections` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[26]),]$`Episodic and paroxysmal disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[27]),]$`Hereditary and Degenerative diseases of the central nervous system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[28]),]$`Other joint disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[29]),]$`Chronic obstructive pulmonary disease and allied conditions` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[30]),]$`Nutritional deficiencies` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[31]),]$`Other diseases of intestines` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[32]),]$`Ischemic heart diseases` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[33]),]$`Other inflammatory conditions of skin and subcutaneous tissue` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[34]),]$`Anemia` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[35]),]$`Arthrosis` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[36]),]$`Mental and behavioural disorders due to psychoactive substance use` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[37]),]$`Other diseases of intestines and peritoneum` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[38]),]$`Diseases of the ear and mastoid process` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[39]),]$`Inflammatory diseases of the central nervous system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[40]),]$`Other disorders of the central nervous system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[41]),]$`Bacterial, viral and other infectious agents` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[42]),]$`Influenza and pneumonia` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[43]),]$`Noninfectious enteritis and colitis` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[44]),]$`Dermatitis and eczema` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[45]),]$`Noninflammatory disorders of female genital tract` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[46]),]$`Other viral diseases` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[47]),]$`Other soft tissue disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[48]),]$`Renal failure` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[49]),]$`Pneumonia and influenza` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[50]),]$`Inflammatory Diseases Of Female Pelvic Organs` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[51]),]$`Nutritional anaemias` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[52]),]$`Other disorders of female genital tract` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[53]),]$`Other forms of heart disease` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[54]),]$`Other disorders of the skin and subcutaneous tissue` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[55]),]$`Benign neoplasms` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[56]),]$`Other diseases of the respiratory system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[57]),]$`Other diseases of blood and blood-forming organs` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[58]),]$`Infections of the skin and subcutaneous tissue` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[59]),]$`Urolithiasis` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[60]),]$`Diseases of liver` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[61]),]$`Certain traumatic complications and unspecified injuries` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[62]),]$`Diseases of male genital organs` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[63]),]$`Injuries to the head` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[64]),]$`Other diseases of the skin and subcutaneous tissue` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[65]),]$`Disorders Of Breast` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[66]),]$`Hernia` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[67]),]$`Osteopathies, chondropathies, and acquired musculoskeletal deformities ` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[68]),]$`Chronic rheumatic heart disease` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[69]),]$`Disorders of bone density and structure` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[70]),]$`Other bacterial diseases` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[71]),]$`Spondylopathies` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[72]),]$`Neoplasms of uncertain behavior` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[73]),]$`Diseases of pulmonary circulation` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[74]),]$`Polyneuropathies and other disorders of the peripheral nervous system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[75]),]$`Mycoses` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[76]),]$`Sprains and strains of joints and adjacent muscles` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[77]),]$`Disorders of the peripheral nervous system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[78]),]$`Disorders of muscles` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[79]),]$`Injuries to unspecified part of trunk, limb or body region` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[80]),]$`Nephritis, nephrotic syndrome, and nephrosis ` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[81]),]$`Disorders of the eye and adnexa` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[82]),]$`Other diseases of the digestive system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[83]),]$`Injuries to the abdomen, lower back, lumbar spine and pelvis` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[84]),]$`Diseases of veins and lymphatics, and other diseases of circulatory system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[85]),]$`Other` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[86]),]$`Inflammatory polyarthropathies` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[87]),]$`Injuries to the neck` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[88]),]$`Diseases of external ear` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[89]),]$`Disorders of gallbladder, biliary tract and pancreas` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[90]),]$`Other and unspecified disorders of the circulatory system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[91]),]$`Diseases of arteries, arterioles, and capillaries ` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[92]),]$`Other disorders of kidney and ureter` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[93]),]$`Helminthiases` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[94]),]$`Coagulation defects, purpura and other haemorrhagic conditions` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[95]),]$`Other diseases of digestive system` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[96]),]$`Behavioural and emotional disorders with onset usually occurring in childhood and adolescence` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[97]),]$`Delivery` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[98]),]$`Disorders of skin appendages` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[99]),]$`Radiation-related disorders of the skin and subcutaneous tissue` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[100]),]$`Open wound of upper limb` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[101]),]$`Appendicitis` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[102]),]$`Other and unspecified effects of external causes` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[103]),]$`Normal delivery, and other indications for care in pregnancy, labor, and delivery ` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[104]),]$`Intestinal infectious diseases` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[105]),]$`Behavioural syndromes associated with physiological disturbances and physical factors` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[106]),]$`Urticaria and erythema` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[107]),]$`Diseases of middle ear and mastoid` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[108]),]$`Psychosis` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[109]),]$`Hernia of abdominal cavity` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[110]),]$`Cerebrovascular diseases` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[111]),]$`Disorders of conjunctiva` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[112]),]$`Other obstetric conditions, not elsewhere classified` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[113]),]$`Extrapyramidal and movement disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[114]),]$`Disorders of ocular muscles, binocular movement, accommodation and refraction` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[115]),]$`Viral infections characterized by skin and mucous membrane lesions` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[116]),]$`Other disorders of ear` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[117]),]$`Superficial injury` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[118]),]$`Open wound of head, neck, and trunk ` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[119]),]$`Visual disturbances and blindness` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[120]),]$`Other diseases of pleura` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[121]),]$`Disorders of lens` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[122]),]$`Contusion with intact skin surface` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[123]),]$`Disorders of eyelid, lacrimal system and orbit` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[124]),]$`Complications mainly related to pregnancy` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[125]),]$`Nerve, nerve root and plexus disorders` <- 1
cov.pos[cov.pos$PTID %in% unlist(idDiag[126]),]$`Other spirochetal diseases` <- 1




#subset by removing the covid flag and the PTID columns 
cov.sub <- subset(cov.pos, select = -c(PTID, COVID_FLAG))
########need to merge 2 similar columns into one 
#"Other diseases of the skin and subcutaneous tissue"  135                                         
#"Other disorders of the skin and subcutaneous tissue"   136         
cov.sub$`Other skin and subcutaneous tissue diseases` <- as.numeric(cov.sub$`Other diseases of the skin and subcutaneous tissue`) + as.numeric(cov.sub$`Other disorders of the skin and subcutaneous tissue`)
skinIndex <- which(cov.sub$`Other skin and subcutaneous tissue diseases` == 2)
cov.sub[skinIndex,"Other skin and subcutaneous tissue diseases"] <- 1
#remove the two columns that has been combined 
cov.sub <- cov.sub[, -c("Other diseases of the skin and subcutaneous tissue", "Other disorders of the skin and subcutaneous tissue")]
#cov.sub <- cov.sub %>% relocate(DECEASED_INDICATOR, .before = d_I10)
# pneumonia of ICD 9 and ICD 10 need to be merged 
cov.sub$`Influenza and Pneumonia` <- as.numeric(cov.sub$`Influenza and pneumonia`) + as.numeric(cov.sub$`Influenza and pneumonia`)
IP <- which(cov.sub$`Influenza and Pneumonia` == 2)
cov.sub[IP,"Influenza and Pneumonia" ] <- 1
cov.sub <- cov.sub[, -c("Influenza and pneumonia", "Pneumonia and influenza")]

#"Other diseases of the digestive system"  and "Other diseases of digestive system" 
cov.sub$`Other Diseases of the digestive system` <- as.numeric(cov.sub$`Other diseases of digestive system`) + as.numeric(cov.sub$`Other diseases of the digestive system`)
DS <- which(cov.sub$`Other Diseases of the digestive system` == 2)
cov.sub[DS,"Other Diseases of the digestive system"] <- 1
cov.sub <- cov.sub[, -c("Other diseases of digestive system", "Other diseases of the digestive system")]
#figure out rename Other as Other Neoplasms
names(cov.sub)[names(cov.sub) == "Other"] <- "Other Neoplasms"





#conduct the log model with dem and general diag
log.DiagDem <- glm(DECEASED_INDICATOR ~. , data = cov.sub2, family = "binomial")
summary(log.DiagDem)
library(plyr)
coef.sub <- summary(log.DiagDem)[12]
coef.sort.sub <- as.data.frame(coef.sub)
names(coef.sort.sub) <- c("Estimate","SE","Zval","Pval")
ans.sub <- arrange(coef.sort.sub,Pval)
coef.sort.sub$ad.Pval <-  p.adjust(coef.sort.sub$Pval, method = "BH")
categories_sig <- coef.sort.sub[which(coef.sort.sub$ad.Pval < 0.05), ]
################################General categories
Anames2 <- as.list(unique(top.500$General))
namesDiag2 <- mapply(function(A)  as.vector(unique(top.500$DIAGNOSIS_CD[top.500$General == A])), Anames2)
idDiag2 <- mcmapply(function(B)  posDiag$PTID[posDiag$DIAGNOSIS_CD %in% B] , namesDiag2)
#assign 1 for all PTIDS identified the 126 diagnosis colums
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[1]),]$`Circulatory` <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[2]),]$`Endocrine` <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[3]),]$Respiratory <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[4]),]$Digestive<- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[5]),]$Musculoskeletal <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[6]),]$`Nervous` <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[7]),]$Mental_and_Behavioral_Disorders <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[8]),]$Emergency <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[9]),]$`Genitourinary` <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[10]),]$`Neoplasms` <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[11]),]$`Skin` <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[12]),]$`Infectious_and_Parasitic` <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[13]),]$`Injury_Poisoning` <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[14]),]$`Eyes_and_ENT` <- 1
cov.pos2[cov.pos2$PTID %in% unlist(idDiag2[15]),]$`Preganacy_and_childbirth` <- 1

#conduct the log model with dem and general diag
log.DiagDemGen <- glm(DECEASED_INDICATOR ~. , data = cov.pos2, family = "binomial")
summary(log.DiagDemGen)
library(plyr)
coef <- summary(log.DiagDemGen)[12]
coef.sort <- as.data.frame(coef)
names(coef.sort) <- c("Estimate","SE","Zval","Pval")
coef.sort$ad.Pval <-  p.adjust(coef.sort$Pval, method = "BH")
gen_sig <- coef.sort[which(coef.sort$ad.Pval < 0.05), ]
arrange(coef.sort,Pval)


#General linear Hypothesis testing on the dem columns such as gender, race, ethnicity, and region
library(multcomp)
#gender
gen.fit <- glm(DECEASED_INDICATOR ~ GENDER_Female + GENDER_Unknown , data = cov.sub, family = "binomial")
# Matrix that represents each linear effect we want to estimate.
mMat <- matrix(0, 3, 138)
mMat[1:3,1] <- 1
mMat[2,3] <- 1
mMat[3,4] <- 1 
mMat <- rbind(c(1, 0, 0), c(1, 1, 0), c(1, 0, 1))
gTest <- glht(model = log.DiagDem , linfct = mMat)
summary(gTest)
#race
race.fit <- glm(DECEASED_INDICATOR ~ RACE_African_American + RACE_Asian + `RACE_Other/Unknown` , data = cov.sub, family = "binomial")
# Matrix that represents each linear effect we want to estimate.
mMat <- rbind(c(1, 0, 0, 0), c(1, 1, 0, 0), c(1, 0, 1, 0), c(1, 0, 0, 1) )
rTest <- glht(model = race.fit, linfct = mMat)
summary(rTest)

#ethnicity
eth.fit <- glm(DECEASED_INDICATOR ~ ETHNICITY_Unknown + ETHNICITY_Not_Hispanic , data = cov.sub, family = "binomial")
# Matrix that represents each linear effect we want to estimate.
mMat <- rbind(c(1, 0, 0), c(1, 1, 0), c(1, 0, 1))
eTest <- glht(model = eth.fit, linfct = mMat)
summary(eTest)
#Region
reg.fit <- glm(DECEASED_INDICATOR ~ REGION_Midwest + REGION_South + `REGION_Other/Unknown` + REGION_West , data = cov.sub, family = "binomial")
# Matrix that represents each linear effect we want to estimate.
mMat <- rbind(c(1, 0, 0, 0, 0), c(1, 1, 0, 0, 0), c(1, 0, 1, 0, 0), c(1, 0, 0, 1, 0), c(1, 0, 0, 0, 1) )
regTest <- glht(model = reg.fit, linfct = mMat)
summary(regTest)

#####Chi-sq test of homogeneity 
#get the  deceased and not deceased dataset 
deceased <-cov.sub[cov.sub[, which(cov.sub$DECEASED_INDICATOR == 1)], ]
not_deceased <-cov.sub[cov.sub[, which(cov.sub$DECEASED_INDICATOR == 0)], ]
## gender
gentab <- matrix(data = 0, nrow= 2, ncol = 2)
colnames(gentab) <- c("GENDER_Female", "GENDER_Unknown")
rownames(gentab) <- c("DECEASED", "NOT_DECEASED")
gentab[1,1] <- sum(deceased$GENDER_Female == 1)
gentab[1,2] <- sum(deceased$GENDER_Unknown == 1)

gentab[2,1] <- sum(not_deceased$GENDER_Female == 0)
gentab[2,2] <- sum(not_deceased$GENDER_Unknown == 0)
chisq.test(gentab, simulate.p.value = TRUE, B = 10000)

#region
regtab <-  matrix(data = 0, nrow= 2, ncol = 3)
colnames(regtab) <- c("REGION_Midwest", "REGION_South", "REGION_West")
rownames(regtab) <- c("DECEASED", "NOT_DECEASED")
regtab[1,1] <- sum(deceased$REGION_Midwest == 1)
regtab[1,2] <- sum(deceased$REGION_South == 1)
regtab[1,3] <- sum(deceased$REGION_West == 1)

regtab[2,1] <- sum(not_deceased$REGION_Midwest == 0)
regtab[2,2] <- sum(not_deceased$REGION_South == 0)
regtab[2,3] <- sum(not_deceased$REGION_West == 0)

chisq.test(regtab, simulate.p.value = FALSE)

#since rejected then 
regtab2 <-  matrix(data = 0, nrow= 2, ncol = 2)
colnames(regtab2) <- c("REGION_Midwest", "REGION_West")
rownames(regtab2) <- c("DECEASED", "NOT_DECEASED")
regtab2[1,1] <- sum(deceased$REGION_Midwest == 1)
regtab2[1,2] <- sum(deceased$REGION_West == 1)

regtab2[2,1] <- sum(not_deceased$REGION_Midwest == 0)
regtab2[2,2] <- sum(not_deceased$REGION_West == 0)

chisq.test(regtab2, simulate.p.value = FALSE, B = 10000)

#race and ethnicity
"RACE_African_American"                                                                        
"RACE_Asian"                                                                                   
"RACE_Other/Unknown"                                                                           
"ETHNICITY_Not_Hispanic"                                                                       
"ETHNICITY_Unknown"  

racetab <- matrix(data = 0, nrow= 2, ncol = 6)
colnames(racetab) <- c("RACE_African_American_&_His", "RACE_Asian_&_His", "RACE_Caucasian_&_His", 
                       "RACE_African_American_NOT_His", "RACE_Asian_NOT_His", "RACE_Caucasian_NOT_His")
rownames(racetab) <- c("DECEASED", "NOT_DECEASED")
test <- deceased[which( deceased$RACE_Asian == 1 &
                          deceased$ETHNICITY_Not_Hispanic == 0),]
racetab[1,1] <- length(deceased[which(deceased$RACE_African_American == 1 
                                      & deceased$ETHNICITY_Not_Hispanic == 0 ),]$RACE_African_American)
racetab[1,2] <- length(deceased[which(deceased$RACE_African_American == 0 & deceased$RACE_Asian == 1 &
                                        deceased$`RACE_Other/Unknown` == 0 & deceased$ETHNICITY_Not_Hispanic == 0 &
                                        deceased$ETHNICITY_Unknown == 0),]$RACE_Asian)
racetab[1,3] <- length(deceased[which(deceased$RACE_African_American == 0 & deceased$RACE_Asian == 0 &
                                        deceased$`RACE_Other/Unknown` == 0 & deceased$ETHNICITY_Not_Hispanic == 0 &
                                        deceased$ETHNICITY_Unknown == 0),]$ETHNICITY_Not_Hispanic)

racetab[1,4] <- length(deceased[which(deceased$RACE_African_American == 1 &
                                        deceased$ETHNICITY_Not_Hispanic == 1),]$RACE_African_American)
racetab[1,5] <- length(deceased[which(deceased$RACE_Asian == 1 &
                                        deceased$ETHNICITY_Not_Hispanic == 1),]$RACE_Asian)
racetab[1,6] <- length(deceased[which(deceased$RACE_Asian == 0 & deceased$RACE_African_American == 0 &
                                        deceased$`RACE_Other/Unknown` == 0 & deceased$ETHNICITY_Not_Hispanic == 1),]$ETHNICITY_Not_Hispanic)


racetab[2,1] <- length(not_deceased[which(not_deceased$RACE_African_American == 1 & not_deceased$RACE_Asian == 0 &
                                            not_deceased$`RACE_Other/Unknown` == 0 & not_deceased$ETHNICITY_Not_Hispanic == 0 &
                                            not_deceased$ETHNICITY_Unknown == 0),]$RACE_African_American)
racetab[2,2] <- length(not_deceased[which(not_deceased$RACE_African_American == 0 & not_deceased$RACE_Asian == 1 &
                                            not_deceased$`RACE_Other/Unknown` == 0 & not_deceased$ETHNICITY_Not_Hispanic == 0 &
                                            not_deceased$ETHNICITY_Unknown == 0),]$RACE_Asian)
racetab[2,3] <- length(not_deceased[which(not_deceased$RACE_African_American == 0 & not_deceased$RACE_Asian == 0 &
                                            not_deceased$`RACE_Other/Unknown` == 0 & not_deceased$ETHNICITY_Not_Hispanic == 0 &
                                            not_deceased$ETHNICITY_Unknown == 0),]$ETHNICITY_Not_Hispanic)

racetab[2,4] <- length(not_deceased[which(not_deceased$RACE_African_American == 1 &
                                            not_deceased$ETHNICITY_Not_Hispanic == 1),]$RACE_African_American)
racetab[2,5] <- length(not_deceased[which(not_deceased$RACE_Asian == 1 &
                                            not_deceased$ETHNICITY_Not_Hispanic == 1),]$RACE_Asian)
racetab[2,6] <- length(not_deceased[which(not_deceased$RACE_Asian == 0 & not_deceased$RACE_African_American == 0 &
                                            not_deceased$`RACE_Other/Unknown` == 0 & not_deceased$ETHNICITY_Not_Hispanic == 1),]$ETHNICITY_Not_Hispanic)

chisq.test(racetab, simulate.p.value = FALSE)

cauHisDied <- length(deceased[which(deceased$RACE_Caucasian & deceased$ETHNICITY_Not_Hispanic == 1),]$ETHNICITY_Not_Hispanic)










































































































































