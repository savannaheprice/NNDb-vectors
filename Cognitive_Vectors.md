Vectorsforassesments
================
Savannah Price
2024-06-05

``` r
FinalScores1 <- read.csv("~/NIH_Data/NIHScores.csv", quote = "")
NamesFinal <- c("NIH Toolbox Emotional Support FF Age 18+ v2.0", "NIH Toolbox Instrumental Support FF Age 18+ v2.0", "NIH Toolbox Friendship FF Age 18+ v2.0", "NIH Toolbox Loneliness FF Age 18+ v2.0", "NIH Toolbox Perceived Rejection FF Age 18+ v2.0", "NIH Toolbox Perceived Hostility FF Age 18+ v2.0", "NIH Toolbox Anger-Hostility FF Age 18+ v2.0", "NIH Toolbox Positive Affect CAT Age 18+ v2.0", "NIH Toolbox General Life Satisfaction CAT Age 18+ v2.0", "NIH Toolbox Meaning and Purpose CAT Age 18+ v2.0", "NIH Toolbox Self-Efficacy CAT Age 18+ v2.0", "NIH Toolbox Perceived Stress FF Age 18+ v2.0", "NIH Toolbox Fear-Affect CAT Age 18+ v2.0", "NIH Toolbox Fear-Somatic Arousal FF Age 18+ v2.0", "NIH Toolbox Sadness CAT Age 18+ v2.0", "NIH Toolbox Anger-Affect CAT Age 18+ v2.0", "NIH Toolbox Anger-Physical Aggression FF Age 18+ v2.0", "NIH Toolbox Dimensional Change Card Sort Test Age 12+ v2.1", "NIH Toolbox Flanker Inhibitory Control and Attention Test Age 12+ v2.1")
```

``` r
library(dplyr)
library(tidyr)
library(data.table)
FinalScores <- FinalScores1[1:9]
FinalScores$DeviceID <- NULL
FinalScores$Assessment.Name <- NULL
Finalnames <- (names(FinalScores))[2:7]
FinalScores <- FinalScores[FinalScores$Inst %in% NamesFinal, ]
FinalScores <- FinalScores[order(FinalScores$Inst), ]
FinalScores <- FinalScores[!is.na(FinalScores$RawScore), ]
FinalScores_wide <- FinalScores %>%
  group_by(Subject.ID) %>%
   mutate(nm1 = rowid(Subject.ID)) %>%
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = c(Finalnames))

FinalScoresNames <- c(names(FinalScores_wide))
FinalScoresNames <- FinalScoresNames[which(grepl("Inst_", FinalScoresNames, fixed = TRUE) == FALSE & grepl("ItmCnt", FinalScoresNames, fixed = TRUE) == FALSE)]
FinalScores_wide <- FinalScores_wide[FinalScoresNames]
```

``` r
Intermediatedata <- read.csv("~/NIH_Data/nih_toolbox_data.csv", quote = "")
Intermediatedata1 <- Intermediatedata[1:16]
Intermediatedata1$ItemID <- NULL
Intermediatedata1$Locale <- NULL
Intermediatedata1$DataType <- NULL
Intermediatedatanames <- (names(Intermediatedata1))[2:13]
Intermediatedata1 <- Intermediatedata1[Intermediatedata1$Inst %in% NamesFinal, ]
Intermediatedata1 <- Intermediatedata1[order(Intermediatedata1$Inst), ]
Intermediatedata1 <- Intermediatedata1 %>%
  mutate(id = row_number(), .by = c(Inst, sub.ID)) %>%
  tidyr::complete(id, sub.ID, Inst) %>%
  select(-id)
Intermediatedata1 <- Intermediatedata1 %>% arrange(Inst)
Intermediatedata1 <- Intermediatedata1 %>% arrange(sub.ID)
Intermediatedata_wide <- Intermediatedata1 %>%
  group_by(sub.ID) %>%
   mutate(nm1 = rowid(sub.ID)) %>%
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = c(Intermediatedatanames))

Intermediatedata_wide <- cbind(Intermediatedata_wide[1], Intermediatedata_wide[-1][order(as.numeric(regmatches(names(Intermediatedata_wide)[-1], regexpr("\\d+", names(Intermediatedata_wide)[-1]))))])

ScoresIntWide <- Intermediatedata_wide[which(grepl("^Score_", names(Intermediatedata_wide), fixed = FALSE) == TRUE | grepl("Inst_", names(Intermediatedata_wide), fixed = TRUE) == TRUE)]

ScoresIntWideS <- ScoresIntWide[which(grepl("^Score_", names(ScoresIntWide), fixed = FALSE) == TRUE)]
```

``` r
AngerAffect <- ScoresIntWideS[1:12]
AngerHostility <- ScoresIntWideS[39:43]
AngerPhysicalAggression <- ScoresIntWideS[77:81]
Card_Sort <- ScoresIntWideS[123:152]
EmotionalSupport <- ScoresIntWideS[153:160]
FearAffect <- ScoresIntWideS[191:202]
FearSomaticArousal <- ScoresIntWideS[229:234]
Flanker_Task <- ScoresIntWideS[271:290]
Friendship <- ScoresIntWideS[305:312]
GenLifeSatisfaction <- ScoresIntWideS[343:352]
InstrumentalSupport <- ScoresIntWideS[381:388]
Loneliness <- ScoresIntWideS[419:423]
MeaningPurpose <- ScoresIntWideS[457:468]
PerceivedHostility <- ScoresIntWideS[495:502]
PerceivedRejection <- ScoresIntWideS[533:540]
PerceivedStress <- ScoresIntWideS[571:580]
PositiveAffect <- ScoresIntWideS[609:620]
Sadness <- ScoresIntWideS[647:658]
SelfEfficacy <- ScoresIntWideS[685:694]
```

``` r
FinalScoresDF <- read.csv("~/NIH_Data/NIH_assessment_scores.csv", quote = "")
tasknames <- c("NIH Toolbox Flanker Inhibitory Control and Attention Test Age 12+ v2.1", "NIH Toolbox Dimensional Change Card Sort Test Age 12+ v2.1")
FinalScoresDF <- FinalScoresDF[FinalScoresDF$Inst %in% tasknames, ]
FinalScoresDF <- FinalScoresDF[is.na(FinalScoresDF$RawScore) == FALSE, ]

cardsort <- FinalScoresDF[FinalScoresDF$Inst=="NIH Toolbox Dimensional Change Card Sort Test Age 12+ v2.1", ]
flanker <- FinalScoresDF[FinalScoresDF$Inst=="NIH Toolbox Flanker Inhibitory Control and Attention Test Age 12+ v2.1", ]
cardsort <- cardsort[which(grepl("Score", names(cardsort), fixed = TRUE) == TRUE | grepl("National", names(cardsort), fixed = TRUE) == TRUE)]
cardsort <- cardsort %>% select_if(~ !all(is.na(.)))
flanker <- flanker[which(grepl("Score", names(flanker), fixed = TRUE) == TRUE | grepl("National", names(flanker), fixed = TRUE) == TRUE)]
flanker <- flanker %>% select_if(~ !all(is.na(.)))
```

``` r
angeraffectnames <- FinalScoresNames[which(grepl("_1$", FinalScoresNames, fixed = FALSE) == TRUE)]
angeraffect <- FinalScores_wide[angeraffectnames]
angerhostilitynames <- FinalScoresNames[which(grepl("_2$", FinalScoresNames, fixed = FALSE) == TRUE)]
angerhostility <- FinalScores_wide[angerhostilitynames]
angerphysaggnames <- FinalScoresNames[which(grepl("_3$", FinalScoresNames, fixed = FALSE) == TRUE)]
angerphysagg <- FinalScores_wide[angerphysaggnames]
emotionalsuppnames <- FinalScoresNames[which(grepl("_5$", FinalScoresNames, fixed = FALSE) == TRUE)]
emotionalsupp <- FinalScores_wide[emotionalsuppnames]
fearaffectnames <- FinalScoresNames[which(grepl("_6$", FinalScoresNames, fixed = FALSE) == TRUE)]
fearaffect <- FinalScores_wide[fearaffectnames]
fearsomanames <- FinalScoresNames[which(grepl("_7$", FinalScoresNames, fixed = FALSE) == TRUE)]
fearsoma <- FinalScores_wide[fearsomanames]
friendshipnames <- FinalScoresNames[which(grepl("_9$", FinalScoresNames, fixed = FALSE) == TRUE)]
friendship <- FinalScores_wide[friendshipnames]
genlifenames <- FinalScoresNames[which(grepl("_10$", FinalScoresNames, fixed = FALSE) == TRUE)]
genlife <- FinalScores_wide[genlifenames]
instrumentalnames <- FinalScoresNames[which(grepl("_11$", FinalScoresNames, fixed = FALSE) == TRUE)]
instrumental <- FinalScores_wide[instrumentalnames]
lonelinessnames <- FinalScoresNames[which(grepl("_12$", FinalScoresNames, fixed = FALSE) == TRUE)]
loneliness <- FinalScores_wide[lonelinessnames]
meaningpurposenames <- FinalScoresNames[which(grepl("_13$", FinalScoresNames, fixed = FALSE) == TRUE)]
meaningpurpose <- FinalScores_wide[meaningpurposenames]
hostilitynames <- FinalScoresNames[which(grepl("_14$", FinalScoresNames, fixed = FALSE) == TRUE)]
hostility <- FinalScores_wide[hostilitynames]
rejectionnames <- FinalScoresNames[which(grepl("_15$", FinalScoresNames, fixed = FALSE) == TRUE)]
rejection <- FinalScores_wide[rejectionnames]
stressnames <- FinalScoresNames[which(grepl("_16$", FinalScoresNames, fixed = FALSE) == TRUE)]
stress <- FinalScores_wide[stressnames]
posaffectnames <- FinalScoresNames[which(grepl("_17$", FinalScoresNames, fixed = FALSE) == TRUE)]
posaffect <- FinalScores_wide[posaffectnames]
sadnessnames <- FinalScoresNames[which(grepl("_18$", FinalScoresNames, fixed = FALSE) == TRUE)]
sadness <- FinalScores_wide[sadnessnames]
selfeffnames <- FinalScoresNames[which(grepl("_19$", FinalScoresNames, fixed = FALSE) == TRUE)]
selfeff <- FinalScores_wide[selfeffnames]
```

``` r
AngerAffect <- AngerAffect %>% replace(is.na(.), 0)
AngerHostility <- AngerHostility %>% replace(is.na(.), 0)
AngerPhysicalAggression <- AngerPhysicalAggression %>% replace(is.na(.), 0)
Card_Sort <- Card_Sort %>% replace(is.na(.), 0)
EmotionalSupport <- EmotionalSupport %>% replace(is.na(.), 0)
FearAffect <- FearAffect %>% replace(is.na(.), 0)
FearSomaticArousal <- FearSomaticArousal %>% replace(is.na(.), 0)
Flanker_Task <- Flanker_Task %>% replace(is.na(.), 0)
Friendship <- Friendship %>% replace(is.na(.), 0)
GenLifeSatisfaction <- GenLifeSatisfaction %>% replace(is.na(.), 0)
InstrumentalSupport <- InstrumentalSupport %>% replace(is.na(.), 0)
Loneliness <- Loneliness %>% replace(is.na(.), 0)
MeaningPurpose <- MeaningPurpose %>% replace(is.na(.), 0)
PerceivedHostility <- PerceivedHostility %>% replace(is.na(.), 0)
PerceivedRejection <- PerceivedRejection %>% replace(is.na(.), 0)
PerceivedStress <- PerceivedStress %>% replace(is.na(.), 0)
PositiveAffect <- PositiveAffect %>% replace(is.na(.), 0)
Sadness <- Sadness %>% replace(is.na(.), 0)
SelfEfficacy <- SelfEfficacy %>% replace(is.na(.), 0)
```

``` r
Make_subject_name <- function(x){
  name = deparse(substitute(x))
   for (u in 1:nrow(x)) {
    assign(paste0(name, "sub_", u, sep = ""), value = as.numeric(x[u,]), envir = .GlobalEnv)
}
}

Make_subject_name(PositiveAffect)
Make_subject_name(GenLifeSatisfaction)
Make_subject_name(MeaningPurpose)
Make_subject_name(SelfEfficacy)
Make_subject_name(PerceivedStress)
Make_subject_name(FearAffect)
Make_subject_name(FearSomaticArousal)
Make_subject_name(Sadness)
Make_subject_name(AngerAffect)
Make_subject_name(AngerPhysicalAggression)
Make_subject_name(EmotionalSupport)
Make_subject_name(InstrumentalSupport)
Make_subject_name(Friendship)
Make_subject_name(Loneliness)
Make_subject_name(PerceivedRejection)
Make_subject_name(PerceivedHostility)
Make_subject_name(AngerHostility)
Make_subject_name(Flanker_Task)
Make_subject_name(Card_Sort)
```

``` r
Make_summary <- function(x){
  name = deparse(substitute(x))
   for (u in 1:nrow(x)) {
    assign(paste0(name, "sub_", u, sep = ""), value = as.numeric(x[u,]), envir = .GlobalEnv)
}
}

Make_summary(angeraffect)
Make_summary(angerhostility)
Make_summary(angerphysagg)
Make_summary(emotionalsupp)
Make_summary(fearaffect)
Make_summary(fearsoma)
Make_summary(friendship)
Make_summary(genlife)
Make_summary(instrumental)
Make_summary(loneliness)
Make_summary(meaningpurpose)
Make_summary(hostility)
Make_summary(rejection)
Make_summary(stress)
Make_summary(posaffect)
Make_summary(sadness)
Make_summary(selfeff)
Make_summary(flanker)
Make_summary(cardsort)
```

``` r
FinalScores_wide <- cbind(FinalScores_wide[1], FinalScores_wide[-1][order(as.numeric(regmatches(names(FinalScores_wide)[-1], regexpr("\\d+", names(FinalScores_wide)[-1]))))])
```

``` r
Demographics <- read.csv("~/NIH_Data/Demographics2.csv")
Demographics <- data.frame(apply(Demographics, 2, function(x) gsub("^$|^ $", NA, x)))
Demographics$Handedness <- NULL
Demographics$Bids_number <- NULL
Demographics$subject.number.only <- NULL
for (i in 1:length(Demographics$Monolingual)) {
  if (Demographics$Monolingual[i] == "N/A") {
    Demographics$Monolingual[i] <- NA
  }
}
Demographics$Monolingual <- ifelse(Demographics$Monolingual=="yes",2,1)
Demographics$Gender <- ifelse(Demographics$Gender=="M",1,0)
Demographics$Movie_watched <- factor(Demographics$Movie_watched, levels = unique(Demographics$Movie_watched))
Demographics$Movie_watched <- as.integer(Demographics$Movie_watched)
Demographics$Ethnicity <- factor(Demographics$Ethnicity, levels = unique(Demographics$Ethnicity))
Demographics$Ethnicity <- as.integer(Demographics$Ethnicity)
Demographics <- Demographics %>% replace(is.na(.), 0)

Make_summary(Demographics)
```
