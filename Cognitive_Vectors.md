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

FinalScores_wide <- FinalScores_wide[names(FinalScores_wide)[which(grepl("Inst_", names(FinalScores_wide), fixed = TRUE) == FALSE & grepl("ItmCnt", names(FinalScores_wide), fixed = TRUE) == FALSE)]]

FinalScores_wide <- cbind(FinalScores_wide[1], FinalScores_wide[-1][order(as.numeric(regmatches(names(FinalScores_wide)[-1], regexpr("\\d+", names(FinalScores_wide)[-1]))))])
```

``` r
Intermediatedata <- read.csv("~/NIH_Data/nih_toolbox_data.csv", quote = "")
Intermediatedata1 <- Intermediatedata[1:16]
Intermediatedata1$ItemID <- NULL
Intermediatedata1$Locale <- NULL
Intermediatedata1$DataType <- NULL
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
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = (names(Intermediatedata1))[2:13])

Intermediatedata_wide <- cbind(Intermediatedata_wide[1], Intermediatedata_wide[-1][order(as.numeric(regmatches(names(Intermediatedata_wide)[-1], regexpr("\\d+", names(Intermediatedata_wide)[-1]))))])

ScoresIntWide <- Intermediatedata_wide[which(grepl("^Score_", names(Intermediatedata_wide), fixed = FALSE) == TRUE | grepl("Inst_", names(Intermediatedata_wide), fixed = TRUE) == TRUE)]

ScoresIntWideS <- ScoresIntWide[which(grepl("^Score_", names(ScoresIntWide), fixed = FALSE) == TRUE)]
ScoresIntWideS <- ScoresIntWideS %>% replace(is.na(.), 0)
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
split_final <- split(FinalScores, f = FinalScores$Inst)
names(split_final) <- c("angeraffect", "angerhostility", "angerphysagg", "cardsort", "emotionalsupp", "fearaffect", "fearsoma", "flanker", "friendship", "genlife", "instrumental", "loneliness", "meaningpurpose", "hostility", "rejection", "stress", "posaffect", "sadness", "selfeff")
list2env(split_final, envir=.GlobalEnv)
```

    ## <environment: R_GlobalEnv>

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
```

``` r
library(stats)

atlist <- c(Flanker_Task, Card_Sort)
fatlist <- c(flanker, cardsort)
emlist <- c(PositiveAffect, GenLifeSatisfaction, MeaningPurpose, SelfEfficacy, PerceivedStress, FearAffect, FearSomaticArousal, Sadness, AngerAffect, AngerPhysicalAggression)
femlist <- c(posaffect, genlife, meaningpurpose, selfeff, stress, fearaffect, fearsoma, sadness, angeraffect, angerphysagg)
solist <- c(EmotionalSupport, InstrumentalSupport, Friendship, Loneliness, PerceivedRejection, PerceivedHostility, AngerHostility)
fsolist <- c(emotionalsupp, instrumental, friendship, loneliness, rejection, hostility, angerhostility)
```

``` r
distAT <- data.matrix(dist(as.data.frame(atlist)))
distATF <- data.matrix(dist(as.data.frame(fatlist[which(grepl("^Computed.Score", names(fatlist), fixed = FALSE) == TRUE)])))

distET <- data.matrix(dist(as.data.frame(emlist)))
distETF <- data.matrix(dist(as.data.frame(femlist[which(grepl("^TScore_", names(femlist), fixed = FALSE) == TRUE)])))

distSO <- data.matrix(dist(as.data.frame(solist)))
distSOF <- data.matrix(dist(as.data.frame(fsolist[which(grepl("^TScore_", names(fsolist), fixed = FALSE) == TRUE)])))

distDE <- data.matrix(dist(Demographics[2:7]))
distAG <- data.matrix(dist(Demographics[2]))
distGE <- data.matrix(dist(Demographics[3]))
distEDU <- data.matrix(dist(Demographics[5:6]))
```

``` r
Make_summary <- function(x){
  name = deparse(substitute(x))
   for (u in 1:nrow(x)) {
    assign(paste0(name, "sub_", u, sep = ""), value = as.numeric(x[u,]), envir = .GlobalEnv)
}
}

fulllist <- c(Flanker_Task, Card_Sort, flanker, cardsort, PositiveAffect, GenLifeSatisfaction, MeaningPurpose, SelfEfficacy, PerceivedStress, FearAffect, FearSomaticArousal, Sadness, AngerAffect, AngerPhysicalAggression, posaffect, genlife, meaningpurpose, selfeff, stress, fearaffect, fearsoma, sadness, angeraffect, angerphysagg, EmotionalSupport, InstrumentalSupport, Friendship, Loneliness, PerceivedRejection, PerceivedHostility, AngerHostility, emotionalsupp, instrumental, friendship, loneliness, rejection, hostility, angerhostility)
```
