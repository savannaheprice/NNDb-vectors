Data_checking
================
Savannah Price
2024-06-20

``` r
library(dplyr)
library(tidyr)
library(data.table)

FinalScoresDF1 <- read.csv("~/NIH_Data/NIH_assessment_scores.csv", quote = "")
FinalScoresDF1 <- data.frame(apply(FinalScoresDF1, 2, function(x) gsub("^$|^ $", NA, x)))
FinalScoresDF1 <- FinalScoresDF1[is.na(FinalScoresDF1$RawScore) == FALSE, ]
FinalScoresDF1$DeviceID <- NULL
FinalScoresDF1$DateFinished <- NULL
FinalScoresDF1$Language <- NULL
FinalScoresDF1$Raw.Score.Right.Ear <- NULL
FinalScoresDF1$Threshold.Right.Ear <- NULL
FinalScoresDF1$Raw.Score.Left.Ear <- NULL
FinalScoresDF1$Threshold.Left.Ear <- NULL
FinalScoresDF1 <- FinalScoresDF1 %>% replace(is.na(.), 0)
NamesFinal <- c("NIH Toolbox Emotional Support FF Age 18+ v2.0", "NIH Toolbox Instrumental Support FF Age 18+ v2.0", "NIH Toolbox Friendship FF Age 18+ v2.0", "NIH Toolbox Loneliness FF Age 18+ v2.0", "NIH Toolbox Perceived Rejection FF Age 18+ v2.0", "NIH Toolbox Perceived Hostility FF Age 18+ v2.0", "NIH Toolbox Anger-Hostility FF Age 18+ v2.0", "NIH Toolbox Positive Affect CAT Age 18+ v2.0", "NIH Toolbox General Life Satisfaction CAT Age 18+ v2.0", "NIH Toolbox Meaning and Purpose CAT Age 18+ v2.0", "NIH Toolbox Self-Efficacy CAT Age 18+ v2.0", "NIH Toolbox Perceived Stress FF Age 18+ v2.0", "NIH Toolbox Fear-Affect CAT Age 18+ v2.0", "NIH Toolbox Fear-Somatic Arousal FF Age 18+ v2.0", "NIH Toolbox Sadness CAT Age 18+ v2.0", "NIH Toolbox Anger-Affect CAT Age 18+ v2.0", "NIH Toolbox Anger-Physical Aggression FF Age 18+ v2.0", "NIH Toolbox Dimensional Change Card Sort Test Age 12+ v2.1", "NIH Toolbox Flanker Inhibitory Control and Attention Test Age 12+ v2.1")
FinalScoresDF1 <- FinalScoresDF1[FinalScoresDF1$Inst %in% NamesFinal, ]
FinalScoresDF1 <- FinalScoresDF1[order(FinalScoresDF1$Inst), ]
FinalScoresDF1 <- FinalScoresDF1 %>% arrange(Inst)
FinalScoresDF1 <- FinalScoresDF1 %>% arrange(Subject.ID)
FinalScoresDF_wide <- FinalScoresDF1 %>%
  group_by(Subject.ID) %>%
   mutate(nm1 = rowid(Subject.ID)) %>%
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = c(names(FinalScoresDF1))[2:17])

FinalScoresDF_wide <- cbind(FinalScoresDF_wide[1], FinalScoresDF_wide[-1][order(as.numeric(regmatches(names(FinalScoresDF_wide)[-1], regexpr("\\d+", names(FinalScoresDF_wide)[-1]))))])
```

``` r
FinalScoresDFNames <- names(FinalScoresDF_wide)
angeraffectnames <- FinalScoresDFNames[which(grepl("_1$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
angeraffect <- FinalScoresDF_wide[angeraffectnames]
angerhostilitynames <- FinalScoresDFNames[which(grepl("_2$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
angerhostility <- FinalScoresDF_wide[angerhostilitynames]
angerphysaggnames <- FinalScoresDFNames[which(grepl("_3$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
angerphysagg <- FinalScoresDF_wide[angerphysaggnames]
cardsortnames <- FinalScoresDFNames[which(grepl("_4$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
cardsort <- FinalScoresDF_wide[cardsortnames]
emotionalsuppnames <- FinalScoresDFNames[which(grepl("_5$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
emotionalsupp <- FinalScoresDF_wide[emotionalsuppnames]
fearaffectnames <- FinalScoresDFNames[which(grepl("_6$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
fearaffect <- FinalScoresDF_wide[fearaffectnames]
fearsomanames <- FinalScoresDFNames[which(grepl("_7$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
fearsoma <- FinalScoresDF_wide[fearsomanames]
flankernames <- FinalScoresDFNames[which(grepl("_8$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
flanker <- FinalScoresDF_wide[flankernames]
friendshipnames <- FinalScoresDFNames[which(grepl("_9$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
friendship <- FinalScoresDF_wide[friendshipnames]
genlifenames <- FinalScoresDFNames[which(grepl("_10$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
genlife <- FinalScoresDF_wide[genlifenames]
instrumentalnames <- FinalScoresDFNames[which(grepl("_11$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
instrumental <- FinalScoresDF_wide[instrumentalnames]
lonelinessnames <- FinalScoresDFNames[which(grepl("_12$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
loneliness <- FinalScoresDF_wide[lonelinessnames]
meaningpurposenames <- FinalScoresDFNames[which(grepl("_13$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
meaningpurpose <- FinalScoresDF_wide[meaningpurposenames]
hostilitynames <- FinalScoresDFNames[which(grepl("_14$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
hostility <- FinalScoresDF_wide[hostilitynames]
rejectionnames <- FinalScoresDFNames[which(grepl("_15$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
rejection <- FinalScoresDF_wide[rejectionnames]
stressnames <- FinalScoresDFNames[which(grepl("_16$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
stress <- FinalScoresDF_wide[stressnames]
posaffectnames <- FinalScoresDFNames[which(grepl("_17$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
posaffect <- FinalScoresDF_wide[posaffectnames]
sadnessnames <- FinalScoresDFNames[which(grepl("_18$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
sadness <- FinalScoresDF_wide[sadnessnames]
selfeffnames <- FinalScoresDFNames[which(grepl("_19$", FinalScoresDFNames, fixed = FALSE) == TRUE)]
selfeff <- FinalScoresDF_wide[selfeffnames]
```

``` r
Intermediatedata <- read.csv("~/NIH_Data/nih_toolbox_data.csv", quote = "")
Intermediatedata1 <- Intermediatedata[1:16]
Intermediatedata1$Locale <- NULL
Intermediatedata1$DataType <- NULL
Intermediatedatanames <- (names(Intermediatedata1))[2:14]
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

AngerAffect <- ScoresIntWideS[1:12]
AngerHostility <- ScoresIntWideS[39:43]
AngerPhysicalAggression <- ScoresIntWideS[77:81]
Card_Sort <- ScoresIntWideS[123:152]
EmotionalSupport <- ScoresIntWideS[153:190]
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
aa <- c()
for (a in 1:12) {
  aa[[a]] <- paste0("_", a, "$")
}
ah <- c()
for (b in 39:43) {
  ah[[b]] <- paste0("_", b, "$")
}
ap <- c()
for (c in 77:81) {
  ap[[c]] <- paste0("_", c, "$")
}
cs <- c()
for (d in 123:152) {
  cs[[d]] <- paste0("_", d, "$")
}
es <- c()
for (e in 153:160) {
  es[[e]] <- paste0("_", e, "$")
}
fa <- c()
for (f in 191:202) {
  fa[[f]] <- paste0("_", f, "$")
}
fs <- c()
for (g in 229:234) {
  fs[[g]] <- paste0("_", g, "$")
}
ft <- c()
for (h in 271:290) {
  ft[[h]] <- paste0("_", h, "$")
}
fr <- c()
for (j in 305:312) {
  fr[[j]] <- paste0("_", j, "$")
}
gl <- c()
for (k in 343:352) {
  gl[[k]] <- paste0("_", k, "$")
}
is <- c()
for (l in 381:388) {
  is[[l]] <- paste0("_", l, "$")
}
lo <- c()
for (m in 419:423) {
  lo[[m]] <- paste0("_", m, "$")
}
mp <- c()
for (n in 457:468) {
  mp[[n]] <- paste0("_", n, "$")
}
ph <- c()
for (o in 495:502) {
  ph[[o]] <- paste0("_", o, "$")
}
pr <- c()
for (p in 533:540) {
  pr[[p]] <- paste0("_", p, "$")
}
ps <- c()
for (q in 571:580) {
  ps[[q]] <- paste0("_", q, "$")
}
pa <- c()
for (r in 609:620) {
  pa[[r]] <- paste0("_", r, "$")
}
sd <- c()
for (s in 647:658) {
  sd[[s]] <- paste0("_", s, "$")
}
se <- c()
for (t in 685:694) {
  se[[t]] <- paste0("_", t, "$")
}

Make_Full <- function(x, y){
  assign(deparse(substitute(y)), Intermediatedata_wide[(unique(grep(paste(x,collapse="|"), names(Intermediatedata_wide), value=TRUE)))], envir=.GlobalEnv)
}


Make_Full(pa, PositiveAffect)
Make_Full(gl, GenLifeSatisfaction)
Make_Full(mp, MeaningPurpose)
Make_Full(se, SelfEfficacy)
Make_Full(ps, PerceivedStress)
Make_Full(fa, FearAffect)
Make_Full(fs, FearSomaticArousal)
Make_Full(sd, Sadness)
Make_Full(aa, AngerAffect)
Make_Full(ap, AngerPhysicalAggression)
Make_Full(es, EmotionalSupport)
Make_Full(is, InstrumentalSupport)
Make_Full(fr, Friendship)
Make_Full(lo, Loneliness)
Make_Full(pr, PerceivedRejection)
Make_Full(ph, PerceivedHostility)
Make_Full(ah, AngerHostility)
Make_Full(ft, Flanker_Task)
Make_Full(cs, Card_Sort)

atlist <- c(Flanker_Task, flanker, Card_Sort, cardsort)
emlist <- c(PositiveAffect, posaffect, GenLifeSatisfaction, genlife, MeaningPurpose, meaningpurpose, SelfEfficacy, selfeff, PerceivedStress, stress, FearAffect, fearaffect, FearSomaticArousal, fearsoma, Sadness, sadness, AngerAffect, angeraffect, AngerPhysicalAggression, angerphysagg)
solist <- c(EmotionalSupport, emotionalsupp, InstrumentalSupport, instrumental, Friendship, friendship, Loneliness, loneliness, PerceivedRejection, rejection, PerceivedHostility, hostility, AngerHostility, angerhostility)

sub_ID <- 1:86
AttentionData <- as.data.frame(atlist)
AttentionData <-cbind(sub_ID, AttentionData)
AttentionData <- AttentionData %>% replace(is.na(.), 0)
AttentionData <- AttentionData[which(grepl("Inst_", names(AttentionData), fixed = TRUE) == FALSE)]
EmotionalData <- as.data.frame(emlist)
EmotionalData <-cbind(sub_ID, EmotionalData)
EmotionalData <- EmotionalData %>% replace(is.na(.), 0)
EmotionalData <- EmotionalData[which(grepl("Inst_", names(EmotionalData), fixed = TRUE) == FALSE)]
SocialityData <- as.data.frame(solist)
SocialityData <-cbind(sub_ID, SocialityData)
SocialityData <- SocialityData %>% replace(is.na(.), 0)
SocialityData <- SocialityData[which(grepl("Inst_", names(SocialityData), fixed = TRUE) == FALSE)]
```

``` r
write.csv(AttentionData, "~/NIH_Data/CreatedAttentionData.csv", row.names = FALSE)

write.csv(EmotionalData, "~/NIH_Data/CreatedEmotionalData.csv", row.names = FALSE)

write.csv(SocialityData, "~/NIH_Data/CreatedSocialityData.csv", row.names = FALSE)
```

``` r
ItemIDat <- AttentionData[which(grepl("^ItemID_", names(AttentionData), fixed = FALSE) == TRUE)][1:11, ]
ItemIDem <- EmotionalData[which(grepl("^ItemID_", names(EmotionalData), fixed = FALSE) == TRUE)]
ItemIDso <- SocialityData[which(grepl("^ItemID_", names(SocialityData), fixed = FALSE) == TRUE)][1:11, ]
ItemIDat_wide <- ItemIDat %>%
   mutate(nm1 = rowid(ItemID_271)) %>%
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = c(names(ItemIDat)))
ItemIDem_wide <- ItemIDem %>%
   mutate(nm1 = rowid(ItemID_609)) %>%
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = c(names(ItemIDem)))
ItemIDso_wide <- ItemIDso %>%
   mutate(nm1 = rowid(ItemID_153)) %>%
   pivot_wider(names_from = nm1, names_sort = TRUE, values_from = c(names(ItemIDso)))
```

``` r
write.csv(ItemIDat_wide, "~/NIH_Data/ItemIDat_wide.csv", row.names = FALSE)

write.csv(ItemIDem_wide, "~/NIH_Data/ItemIDem_wide.csv", row.names = FALSE)

write.csv(ItemIDso_wide, "~/NIH_Data/ItemIDso_wide.csv", row.names = FALSE)
```
