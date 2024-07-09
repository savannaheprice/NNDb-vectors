Vectorsforassesments
================
Savannah Price
2024-06-05

``` r
taskNamesFinal <- c("NIH Toolbox Emotional Support FF Age 18+ v2.0", "NIH Toolbox Instrumental Support FF Age 18+ v2.0", "NIH Toolbox Friendship FF Age 18+ v2.0", "NIH Toolbox Loneliness FF Age 18+ v2.0", "NIH Toolbox Perceived Rejection FF Age 18+ v2.0", "NIH Toolbox Perceived Hostility FF Age 18+ v2.0", "NIH Toolbox Anger-Hostility FF Age 18+ v2.0", "NIH Toolbox Positive Affect CAT Age 18+ v2.0", "NIH Toolbox General Life Satisfaction CAT Age 18+ v2.0", "NIH Toolbox Meaning and Purpose CAT Age 18+ v2.0", "NIH Toolbox Self-Efficacy CAT Age 18+ v2.0", "NIH Toolbox Perceived Stress FF Age 18+ v2.0", "NIH Toolbox Fear-Affect CAT Age 18+ v2.0", "NIH Toolbox Fear-Somatic Arousal FF Age 18+ v2.0", "NIH Toolbox Sadness CAT Age 18+ v2.0", "NIH Toolbox Anger-Affect CAT Age 18+ v2.0", "NIH Toolbox Anger-Physical Aggression FF Age 18+ v2.0", "NIH Toolbox Dimensional Change Card Sort Test Age 12+ v2.1", "NIH Toolbox Flanker Inhibitory Control and Attention Test Age 12+ v2.1")
```

``` r
library(dplyr)
library(tidyr)
library(data.table)
```

``` r
Intermediatedata <- read.csv("~/NIH_Data/nih_toolbox_data.csv", quote = "")
Intermediatedata1 <- Intermediatedata[1:16]
Intermediatedata1$ItemID <- NULL
Intermediatedata1$Locale <- NULL
Intermediatedata1$DataType <- NULL
Intermediatedata1 <- Intermediatedata1[Intermediatedata1$Inst %in% taskNamesFinal, ]
Intermediatedata1 <- Intermediatedata1[order(Intermediatedata1$Inst), ]
```

``` r
split_intermediate <- split(Intermediatedata1, f = Intermediatedata1$Inst)
names(split_intermediate) <- as.character(expression(AngerAffect, AngerHostility, AngerPhysicalAggression, Card_Sort, EmotionalSupport, FearAffect, FearSomaticArousal, Flanker_Task, Friendship, GenLifeSatisfaction, InstrumentalSupport, Loneliness, MeaningPurpose, PerceivedHostility, PerceivedRejection, PerceivedStress, PositiveAffect, Sadness, SelfEfficacy))

split_intermediate <- lapply(split_intermediate, function(x) x %>%
  group_by(sub.ID) %>%
   mutate(nm1 = rowid(sub.ID)) %>%
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = (names(x))[2:13]))

split_intermediate <- lapply(split_intermediate, function(x) x[which(grepl("^TScore_", names(x), fixed = FALSE) == TRUE | grepl("^Score_", names(x), fixed = FALSE) == TRUE)])

split_intermediate <- lapply(split_intermediate, function(x) cbind(x[1], x[-1][order(as.numeric(regmatches(names(x)[-1], regexpr("\\d+", names(x)[-1]))))]))

split_intermediate <- lapply(split_intermediate, function(x) x %>% replace(is.na(.), 0))

list2env(split_intermediate, envir=.GlobalEnv)
```

    ## <environment: R_GlobalEnv>

``` r
FinalScores1 <- read.csv("~/NIH_Data/NIHScores.csv", quote = "")
FinalScores <- FinalScores1[1:9]
FinalScores$DeviceID <- NULL
FinalScores$Assessment.Name <- NULL
taskFinalnames <- (names(FinalScores))[2:7]
FinalScores <- FinalScores[FinalScores$Inst %in% taskNamesFinal, ]
FinalScores <- FinalScores[order(FinalScores$Inst), ]
FinalScores <- FinalScores[!is.na(FinalScores$RawScore), ]
```

``` r
FinalScorest <- FinalScores
split_final <- split(FinalScorest, f = FinalScorest$Inst)
names(split_final) <- c("angeraffect", "angerhostility", "angerphysagg", "cardsort", "emotionalsupp", "fearaffect", "fearsoma", "flanker", "friendship", "genlife", "instrumental", "loneliness", "meaningpurpose", "perceivedhostility", "perceivedrejection", "perceivedstress", "posaffect", "sadness", "selfeff")

split_final <- lapply(split_final, function(x) x[which(grepl("^TScore", names(x), fixed = FALSE) == TRUE | grepl("^SE", names(x), fixed = FALSE) == TRUE | grepl("^Theta", names(x), fixed = FALSE) == TRUE)])

split_final <- lapply(split_final, function(x) x %>% replace(is.na(.), 0))

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
cardsort <- cardsort[which(grepl("Score", names(cardsort), fixed = TRUE) == TRUE | grepl("score", names(cardsort), fixed = TRUE) == TRUE)]
cardsort <- cardsort %>% select_if(~ !all(is.na(.)))
flanker <- flanker[which(grepl("Score", names(flanker), fixed = TRUE) == TRUE | grepl("score", names(flanker), fixed = TRUE) == TRUE)]
flanker <- flanker %>% select_if(~ !all(is.na(.)))
```

``` r
IntermediatedataDF <- read.csv("~/NIH_Data/nih_toolbox_data.csv", quote = "")
IntermediatedataDF <- IntermediatedataDF[IntermediatedataDF$Inst %in% tasknames, ]

Card_Sort <- IntermediatedataDF[IntermediatedataDF$Inst=="NIH Toolbox Dimensional Change Card Sort Test Age 12+ v2.1", ]
Card_Sort <- Card_Sort %>%
  group_by(sub.ID) %>%
   mutate(nm1 = rowid(sub.ID)) %>%
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = (names(Card_Sort))[2:16])

Flanker_Task <- IntermediatedataDF[IntermediatedataDF$Inst=="NIH Toolbox Flanker Inhibitory Control and Attention Test Age 12+ v2.1", ]
Flanker_Task <- Flanker_Task %>%
  group_by(sub.ID) %>%
   mutate(nm1 = rowid(sub.ID)) %>%
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = (names(Flanker_Task))[2:16])

Card_Sort <- Card_Sort[which(grepl("Score", names(Card_Sort), fixed = TRUE) == TRUE)]
Card_Sort <- Card_Sort %>% select_if(~ !all(is.na(.)))
Card_Sort <- Card_Sort[9:38]
Flanker_Task <- Flanker_Task[which(grepl("Score", names(Flanker_Task), fixed = TRUE) == TRUE)]
Flanker_Task <- Flanker_Task %>% select_if(~ !all(is.na(.)))
Flanker_Task <- Flanker_Task[5:24]
```

``` r
Flanker_Congruent <- Flanker_Task[, c(1, 2, 4, 6, 8, 9, 11, 12, 14, 16, 18, 19)]
Flanker_Incongruent <- Flanker_Task[, c(3, 5, 7, 10, 13, 15, 17, 20)]
Flanker_Task$Congruent <- rowSums(Flanker_Congruent)
Flanker_Task$Incongruent <- rowSums(Flanker_Incongruent)
CardSort_Repeat <- Card_Sort[, c(1, 2, 3, 4, 6, 7, 8, 10, 11, 12, 13, 14, 16, 17, 19, 20, 21, 23, 24, 25, 26, 28, 29)]
CardSort_Switch <- Card_Sort[, c(5, 9, 15, 18, 22, 27, 30)]
Card_Sort$Repeat <- rowSums(CardSort_Repeat)
Card_Sort$Switch <- rowSums(CardSort_Switch)

Flanker_Task <- Flanker_Task[21:22]
Card_Sort <- Card_Sort[31:32]
```

``` r
Demographics <- read.csv("~/NIH_Data/Demographics2.csv")
Demographics <- data.frame(apply(Demographics, 2, function(x) gsub("^$|^ $", NA, x)))
Demographics$Handedness <- NULL
Demographics$Bids_number <- NULL
Demographics$subject.number.only <- NULL
for (z in 1:length(Demographics$Monolingual)) {
  if (Demographics$Monolingual[z] == "N/A") {
    Demographics$Monolingual[z] <- NA
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
Leftout <- read.csv("~/NIH_Data/NIH_assessment_scores.csv", quote = "")
taskleftout <- c("Negative Affect Summary (18+)", "Social Satisfaction Summary (18+)", "Psychological Well Being Summary (18+)", "Cognition Fluid Composite v1.1")
Leftout <- Leftout[Leftout$Inst %in% taskleftout, ]
Leftout <- Leftout %>% arrange(Inst)
Leftout <- Leftout %>% replace(is.na(.), 0)
write.csv(Leftout, "~/NIH_Data/Leftout.csv", row.names = FALSE)
```

``` r
Negativeaffectsum <- Leftout[Leftout$Inst == "Negative Affect Summary (18+)", ]
Negativeaffectsum <- Negativeaffectsum[7]
Socialsatisfactionsum <- Leftout[Leftout$Inst == "Social Satisfaction Summary (18+)", ]
Socialsatisfactionsum <- Socialsatisfactionsum[7]
Psychologicalwellbeingsum <- Leftout[Leftout$Inst == "Psychological Well Being Summary (18+)", ]
Psychologicalwellbeingsum <- Psychologicalwellbeingsum[7]
Cognitionfluidsum <- Leftout[Leftout$Inst == "Cognition Fluid Composite v1.1", ]
Cognitionfluidsum <- Cognitionfluidsum[13:16]
Cognitionfluidsum$National.Percentile..age.adjusted. <- NULL
```

``` r
Make_summary <- function(x){
  name = deparse(substitute(x))
   for (u in 1:nrow(x)) {
    assign(paste0(name, "sub_", u, sep = ""), value = as.numeric(x[u,]), envir = .GlobalEnv)
}
}

Make_summary(PositiveAffect)
Make_summary(GenLifeSatisfaction)
Make_summary(MeaningPurpose)
Make_summary(SelfEfficacy)
Make_summary(PerceivedStress)
Make_summary(FearAffect)
Make_summary(FearSomaticArousal)
Make_summary(Sadness)
Make_summary(AngerAffect)
Make_summary(AngerPhysicalAggression)
Make_summary(EmotionalSupport)
Make_summary(InstrumentalSupport)
Make_summary(Friendship)
Make_summary(Loneliness)
Make_summary(PerceivedRejection)
Make_summary(PerceivedHostility)
Make_summary(AngerHostility)
Make_summary(Flanker_Task)
Make_summary(Card_Sort)

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
Make_summary(perceivedhostility)
Make_summary(perceivedrejection)
Make_summary(perceivedstress)
Make_summary(posaffect)
Make_summary(sadness)
Make_summary(selfeff)
Make_summary(flanker)
Make_summary(cardsort)
Make_summary(Demographics)

Make_summary(Negativeaffectsum)
Make_summary(Socialsatisfactionsum)
Make_summary(Psychologicalwellbeingsum)
Make_summary(Cognitionfluidsum)
```
