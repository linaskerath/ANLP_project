rm(list = ls())
library(ggplot2)
library(tidyverse)
library(psych)
library(xtable)
library(cowplot)

######## read data ###########
load("../primary data/eye tracking data/joint_data_l2_trimmed.rda")

###### remove outliers ###########

joint.data = joint.data[joint.data$dur > 80 | is.na(joint.data$dur)==T,]

crit = joint.data %>%
  group_by(lang, uniform_id) %>%
  summarise(dur.crit = quantile(dur, 0.99, na.rm = T))
             

m = merge(joint.data, crit, by.x = c("lang", "uniform_id"), by.y = c("lang", "uniform_id"))  
m = m[m$skip == 1 | (m$dur < m$dur.crit),]

m -> joint.data
rm(m)

joint.data = joint.data[joint.data$skip == 1 | joint.data$nfix <= quantile(joint.data$nfix, 0.99, na.rm = T), ]
joint.data$nfix.inc = 0 #fixing NAs in nfix for skipped words. Replacing by 0s.
joint.data[is.na(joint.data$nfix) == F,]$nfix.inc = joint.data[is.na(joint.data$nfix) == F,]$nfix
summary(joint.data$nfix.inc)


######### data frame of subject means ##########
by.sub = joint.data %>% group_by(uniform_id) %>% summarise(
  lang = unique(lang),
  skip = mean(skip, na.rm = T),
  nfix = mean(nfix, na.rm = T),
  dur = mean(dur, na.rm = T),
  reg.in = mean(reg.in, na.rm = T),
  firstfix.dur = mean(firstfix.dur, na.rm = T),
  firstrun.dur = mean(firstrun.dur, na.rm = T),
  refix = mean(refix, na.rm = T),
  reread = mean(reread, na.rm = T)
) 

table(by.sub$lang)


########## read reading rate ############
load("../primary data/eye tracking data/joint.l2.readrate.rda") #reading rate broken down by story and participant
u -> readrate_full
rm(u)
readrate_full <- readrate_full %>% filter(rate < 1000)


readrate <- readrate_full %>% group_by(uniform_id) %>% dplyr::summarize(rate = mean(rate))

m = merge(by.sub, readrate)
m -> by.sub
rm(m, readrate)

##### create table of means #####
describeBy(x = by.sub[, 2:ncol(by.sub)], group = "lang") -> descr
data.frame(do.call(rbind, descr)) -> descr
substr(rownames(descr), 1, 2) -> descr$lang
substr(rownames(descr), 4, nchar(rownames(descr))) -> descr$var

descr <- descr[, c("var", "lang", "n", "mean", "sd", "se",  "median", "min", "max")]
descr = descr[-grep("lang", rownames(descr)),]
descr = descr[order(descr$var, descr$lang),]

for (i in 3:ncol(descr)) {
  descr[,i] = round(descr[,i], 2)
}

rownames(descr) = 1:nrow(descr)
descr = droplevels(descr)

load("../primary data/comprehension data/joint.comp.l2.rda") #reading comprehension accuracy data.
joint.comp -> joint.acc
rm(joint.comp)
joint.acc$lang = as.factor(joint.acc$lang)

describeBy(joint.acc[, c("accuracy")], group = joint.acc$lang) -> desc_acc
data.frame(do.call(rbind, desc_acc)) -> desc_acc
substr(rownames(desc_acc), 1, 2) -> desc_acc$lang
substr(rownames(desc_acc), 4, nchar(rownames(desc_acc))) -> desc_acc$var
desc_acc <- desc_acc[, c("var", "lang", "n", "mean", "sd", "se",  "median", "min", "max")]
desc_acc = desc_acc[order(desc_acc$var, desc_acc$lang),]

for (i in 3:ncol(desc_acc)) {
  desc_acc[,i] = round(desc_acc[,i], 2)
}

rownames(desc_acc) = 1:nrow(desc_acc)
desc_acc = droplevels(desc_acc)
desc_acc$var = "acc"

load("../primary data/individual differences data/joint.ind.diff.l2.rda") ## reading individual differences data
colnames(joint_id)[7] = "towre: swe"
colnames(joint_id)[8] = "towre: pde"

#note: here only use vocab 2-5: renamed simply as vocabulary
colnames(joint_id)[10] = "vocabulary"
colnames(joint_id)[9] = "vocab.old.all"

colnames(joint_id)[11] = "cft"
colnames(joint_id)[4] = "motivation"

describeBy(joint_id[, -c(1,2,5)], group = joint_id$lang) -> desc_ind
data.frame(do.call(rbind, desc_ind)) -> desc_ind

substr(rownames(desc_ind), 1, 2) -> desc_ind$lang
substr(rownames(desc_ind), 4, nchar(rownames(desc_ind))) -> desc_ind$var

desc_ind <- desc_ind[, c("var", "lang", "n", "mean", "sd", "se",  "median", "min", "max")]
desc_ind = desc_ind[order(desc_ind$var, desc_ind$lang),]

for (i in 3:ncol(desc_ind)) {
  desc_ind[,i] = round(desc_ind[,i], 2)
}

rownames(desc_ind) = 1:nrow(desc_ind)
desc_ind = droplevels(desc_ind)


descr <- data.frame(rbind(descr, desc_acc), stringsAsFactors = F)
rm(desc_acc)
descr$lang = as.factor(descr$lang)

## write auxiliary files
# write.csv(descr, "../auxiliary files/descriptive stats/descriptive_eyemove_language.csv", quote = F, row.names = F)
# 
# write.csv(desc_ind, "../auxiliary files/descriptive stats/descriptive_ind_language.csv", quote = F, row.names = F)



##### descriptive stats for read rate and acc by passage #####
fun25 = function(x) {
  return(quantile(x, 0.25))
}
fun75 = function(x) {
  return(quantile(x, 0.75))
}

describeBy(readrate_full$rate, group = list(readrate_full$trialid, readrate_full$lang), mat = T) -> descr_rate_story
head(descr_rate_story)
colnames(descr_rate_story)[2:4] = c("trialid", "lang", "var")
descr_rate_story <- data.frame(descr_rate_story[, c("var", "lang", "trialid", "n", "mean", "sd", "se",  "median", "min", "max")], stringsAsFactors = F)

readrate_full %>% group_by(lang, trialid) %>% dplyr::summarize(q1 = fun25(rate), q3 = fun75(rate)) -> tmp_rate

descr_rate_story = cbind(descr_rate_story, tmp_rate[, 3:4])
descr_rate_story$var = "rate"
for (i in 5:8) {
  descr_rate_story[,i] = round(descr_rate_story[,i])
}
head(descr_rate_story)

## write auxiliary file
# write.csv(descr_rate_story, file = "../auxiliary files/descriptive stats/descriptive_rate_passage.csv", quote = F, row.names = F)

load("../primary data/comprehension data/joint_l2_acc_breakdown.rda")

describeBy(joint.br.acc$ACCURACY, group = list(joint.br.acc$number, joint.br.acc$lang), mat = T) -> descr_acc_story
head(descr_acc_story)
colnames(descr_acc_story)[2:4] = c("trialid", "lang", "var")
descr_acc_story <- data.frame(descr_acc_story[, c("var", "lang", "trialid", "n", "mean", "sd", "se",  "median", "min", "max")], stringsAsFactors = F)

joint.br.acc %>% group_by(lang, number) %>% dplyr::summarize(q1 = fun25(ACCURACY), q3 = fun75(ACCURACY)) -> tmp_acc
descr_acc_story = cbind(descr_acc_story, tmp_acc[, 3:4])
descr_acc_story$var = "acc"
for (i in 5:8) {
  descr_acc_story[,i] = round(descr_acc_story[,i],2)
}
head(descr_acc_story)

## write auxiliary file 
# write.csv(descr_acc_story, file = "../auxiliary files/descriptive stats/descriptive_acc_passage.csv", quote = F, row.names = F)


##### plot means #####
ggplot(aes(x = lang, y = mean, col = lang), data = descr) + geom_point(stat = "identity") + geom_errorbar(aes(ymin=mean - se, ymax=mean+se)) + facet_wrap(~var, scales = "free") + theme(axis.text=element_text(size=5)) + labs(col = "sample (L1)") 

desc_ind_plot <- desc_ind
table(desc_ind_plot$var)
desc_ind_plot = subset(desc_ind_plot, var != "vocab.old.all") #remove old vocab measure: plot only performance at thousands 2-5
desc_ind_plot$var = gsub("\\*", "", desc_ind_plot$var)

ggplot(aes(x = lang, y = mean, col = lang), data = desc_ind_plot) + geom_point(stat = "identity") + geom_errorbar(aes(ymin=mean - se, ymax=mean+se)) + facet_wrap(~var, scales = "free") + theme(axis.text=element_text(size=5)) + labs(col = "sample (L1)")  


######## reliability (supplementary materials) ########

joint.data$split = as.factor(ifelse(joint.data$ianum %% 2 == 1, "o", "e"))


# reliability at the level of subjects
by.sub.split = joint.data %>% group_by(uniform_id, lang, split) %>% summarise(
  skip = mean(skip, na.rm = T),
  nfix = mean(nfix, na.rm = T),
  tft = mean(dur, na.rm = T),
  reg = mean(reg.in, na.rm = T),
  ffd = mean(firstfix.dur, na.rm = T),
  gd = mean(firstrun.dur, na.rm = T),
  reread = mean(reread, na.rm = T)
) 

head(by.sub.split)

langs = unique(by.sub.split$lang)
vars = colnames(by.sub.split[4:ncol(by.sub.split)])

n = length(langs)*length(vars)
d = data.frame(lang = character(n), var = character(n), split.half = numeric(n), stringsAsFactors = F)

count = 1
for (lang in langs) {
  for (var in vars) { 
    cor.tmp = cor.test(pull(by.sub.split[by.sub.split$lang == lang & by.sub.split$split == "e", var]), pull(by.sub.split[by.sub.split$lang == lang & by.sub.split$split == "o", var]))
    cor.tmp = cor.tmp$estimate
    
    d$lang[count] = lang
    d$var[count] = var
    d$split.half[count] = as.numeric(cor.tmp)
    
    count = count + 1
  }
}
d
d$split.half.corrected = (2*d$split.half) / (1 + d$split.half)
aggregate(data = d, split.half ~ lang, FUN = mean)
aggregate(data = d, split.half.corrected ~ lang, FUN = mean)

#long to wide
library(reshape2)
rel.sub.wide.uncor = dcast(d, lang ~ var, value.var="split.half")
head(rel.sub.wide.uncor)

library(psych)
rel.sub.wide.uncor$mean.r = fisherz2r(rowMeans(fisherz(rel.sub.wide.uncor[,2:ncol(rel.sub.wide.uncor)]))) #note: mean based on fisher tranformation


rel.sub.wide.uncor[,2:ncol(rel.sub.wide.uncor)] = round(rel.sub.wide.uncor[2:ncol(rel.sub.wide.uncor)],2)

rel.sub.wide.cor = dcast(d, lang ~ var, value.var="split.half.corrected")

rel.sub.wide.cor$mean.r = fisherz2r(rowMeans(fisherz(rel.sub.wide.cor[,2:ncol(rel.sub.wide.cor)])))
rel.sub.wide.cor[,2:ncol(rel.sub.wide.cor)] = round(rel.sub.wide.cor[2:ncol(rel.sub.wide.cor)],2)

values.uncor = as.matrix(rel.sub.wide.uncor[,2:ncol(rel.sub.wide.uncor)])
values.cor = as.matrix(rel.sub.wide.cor[,2:ncol(rel.sub.wide.cor)])

rel.sub.sum = matrix(paste(values.uncor, values.cor, sep = ", "), nrow=nrow(values.uncor), dimnames=dimnames(values.uncor))

rel.sub.sum = as.data.frame(rel.sub.sum)
rel.sub.sum$lang = rel.sub.wide.cor$lang ## rel.sub.sum: table of subject-level reliability (uncorrected and corrected)



# reliability at the level of words 
joint.data$split.sub = as.factor(ifelse(as.numeric(as.factor(joint.data$subid)) %% 2 == 0, "e", "o"))


table(joint.data$lang, joint.data$split.sub)

joint.data$word.run.num = paste(joint.data$lang, joint.data$trialid, joint.data$ianum, sep = "_")

by.sub.split.word = joint.data %>% group_by(word.run.num, lang, split.sub) %>% summarise(
  skip = mean(skip, na.rm = T),
  nfix = mean(nfix, na.rm = T),
  tft = mean(dur, na.rm = T),
  reg = mean(reg.in, na.rm = T),
  ffd = mean(firstfix.dur, na.rm = T),
  gd = mean(firstrun.dur, na.rm = T),
  reread = mean(reread, na.rm = T)
) 

head(by.sub.split.word)


langs = unique(by.sub.split.word$lang)
vars = colnames(by.sub.split.word[4:ncol(by.sub.split.word)])

n = length(langs)*length(vars)
d.split.half.words = data.frame(lang = character(n), var = character(n), split.half = numeric(n), stringsAsFactors = F)

count = 1
for (lang in langs) {
  for (var in vars) { 
    #two datasets: one even and one odd.
    even = by.sub.split.word[by.sub.split.word$lang == lang & by.sub.split.word$split.sub == "e", ]
    odd =  by.sub.split.word[by.sub.split.word$lang == lang & by.sub.split.word$split.sub == "o", ]
    
    #subset to intersect (in cases where number is different due to missing)
    even <- even[even$word.run.num %in% intersect(even$word.run.num, odd$word.run.num), ]
    odd <- odd[odd$word.run.num %in% intersect(even$word.run.num, odd$word.run.num), ]
    
    cor.tmp = cor.test(pull(even[, var]), pull(odd[, var]))
    cor.tmp = cor.tmp$estimate
    
    d.split.half.words$lang[count] = lang
    d.split.half.words$var[count] = var
    d.split.half.words$split.half[count] = as.numeric(cor.tmp)
    
    count = count + 1
  }
}

d.split.half.words$split.half.corrected = (2*d.split.half.words$split.half)/(1+d.split.half.words$split.half)
aggregate(data = d.split.half.words, split.half ~ lang, FUN = mean)
aggregate(data = d.split.half.words, split.half.corrected ~ lang, FUN = mean)
aggregate(data = d.split.half.words, split.half.corrected ~ var, FUN = mean)

library(reshape2)
rel.item.wide.uncor = dcast(d.split.half.words, lang ~ var, value.var="split.half")
head(rel.item.wide.uncor)

rel.item.wide.uncor$mean.r = fisherz2r(rowMeans(fisherz(rel.item.wide.uncor[,2:ncol(rel.item.wide.uncor)])))

rel.item.wide.uncor[,2:ncol(rel.item.wide.uncor)] = round(rel.item.wide.uncor[2:ncol(rel.item.wide.uncor)],2)

rel.item.wide.cor = dcast(d.split.half.words, lang ~ var, value.var="split.half.corrected")
head(rel.item.wide.cor)

rel.item.wide.cor$mean.r = fisherz2r(rowMeans(fisherz(rel.item.wide.cor[,2:ncol(rel.item.wide.cor)])))

rel.item.wide.cor[,2:ncol(rel.item.wide.cor)] = round(rel.item.wide.cor[2:ncol(rel.item.wide.cor)],2)


values.uncor = as.matrix(rel.item.wide.uncor[,2:ncol(rel.item.wide.uncor)])
values.cor = as.matrix(rel.item.wide.cor[,2:ncol(rel.item.wide.cor)])

rel.item.sum = matrix(paste(values.uncor, values.cor, sep = ", "), nrow=nrow(values.uncor), dimnames=dimnames(values.uncor))  ## rel.item.sum: table of item-level reliability (uncorrected and corrected)

rel.item.sum = as.data.frame(rel.item.sum)
rel.item.sum$lang = rel.item.wide.cor$lang

###### ICC for reading rate
library(psych)
library(reshape2)

load("../primary data/eye tracking data/joint.l2.readrate.rda")

length(unique(u$trialid))

u.wide = dcast(data = u, uniform_id + lang ~ trialid, value.var="rate")

#all languages
psych::ICC(x = u.wide[,3:ncol(u.wide)], missing = T, lmer = T)

#ICC by language 
d.icc = data.frame(lang = character(length(unique(u.wide$lang))), icc2k = numeric(length(unique(u.wide$lang))), stringsAsFactors = F)

count = 1
for (lang.curr in unique(u$lang)) { 
  d.icc$lang[count] = lang.curr
  d.curr = u.wide[u.wide$lang == lang.curr,]
  
  #note: outlier removal for ICC: if > 500 words per minute delete. 
  to.icc = d.curr[,3:ncol(u.wide)]
  to.icc[to.icc > 500] = NA
  
  icc.curr = psych::ICC(x = to.icc, missing = T, lmer = T)
  
  d.icc$icc2k[count] = icc.curr$results$ICC[5]
  
  count = count + 1
}

d.icc$icc2k = round(d.icc$icc2k, 2)


##### correlation table #####
merge(by.sub, joint.acc[, c("uniform_id", "accuracy")], by.x = "uniform_id", by.y = "uniform_id", all = T) -> by.sub1

by.sub1$accuracy -> by.sub1$acc
by.sub1$accuracy <- NULL

### write subject means to file: to be used later. 
# save(by.sub1, file = "../auxiliary files/by-sub-means/by.sub.l2.rda")

by.sub.ind = merge(by.sub1[,-2], joint_id, by.x = "uniform_id", by.y = "uniform_id")

library(Hmisc)

colnames(by.sub.ind)
rcorr(as.matrix(by.sub.ind[, -c(1,12,15,19)])) -> rc 

mat_corr = matrix(nrow = nrow(rc$r), ncol = ncol(rc$r), "")
mat_corr[upper.tri(mat_corr)] <- round(rc$r[upper.tri(rc$r)],2)
mat_corr[lower.tri(mat_corr)] <- round(rc$P[lower.tri(rc$P)],3)
data.frame(mat_corr) -> mat_corr
colnames(mat_corr) = rownames(mat_corr) = colnames(rc$r)

mat_corr #correlation table
# write.csv(mat_corr, file = "../../SSLA Revision 1/tables/cor.table.csv")

######### read in L1 and L2 subject-level data ########
rm(list = ls())
load("../auxiliary files/by-sub-means/by.sub.l2.rda")
by.sub1 -> by.sub.l2
colnames(by.sub.l2) = paste0("l2_", colnames(by.sub.l2))

load("../auxiliary files/by-sub-means/by.sub.l1.rda")
by.sub1 -> by.sub.l1
colnames(by.sub.l1) = paste0("l1_", colnames(by.sub.l1))
rm(by.sub1)

by.sub <- inner_join(by.sub.l1, by.sub.l2, by = c("l1_uniform_id" = "l2_uniform_id"))
table(by.sub$l1_lang) #number of subjects by site from data on both L1 and L2

####### L1-L2 reading rate correlation #######
#scale reading rate within L1 
by.sub.small <- 
  by.sub %>%
  group_by(l1_lang) %>%
  mutate(l1_rate_s = scale(l1_rate)) 

#remove english for plot (only L2 readers)
by.sub.small.no.eng <- by.sub.small[by.sub.small$l1_lang != "en",]
cor.test(by.sub.small.no.eng$l1_rate_s, by.sub.small.no.eng$l2_rate) #even slightly higher 0.75

ggplot(data = by.sub.small.no.eng, aes(x = l1_rate_s, y = l2_rate)) +
  geom_point(size = 2) + 
  geom_smooth(method = lm, se = F) +
  theme_bw(base_size = 18) + 
  xlab("L1 reading rate (scaled)") + 
  ylab("L2 reading rate")

############ predicting L2 behavior from L1 behavior , individual differences and sample ############
rm(list = ls())
load("../auxiliary files/by-sub-means/by.sub.l2.rda")
by.sub1 -> by.sub.l2
var.short = colnames(by.sub.l2) 
colnames(by.sub.l2) = paste0("l2_", colnames(by.sub.l2))

load("../auxiliary files/by-sub-means/by.sub.l1.rda")
by.sub1 -> by.sub.l1
colnames(by.sub.l1) = paste0("l1_", colnames(by.sub.l1))
rm(by.sub1)

by.sub.l1$l1_cft_score = NULL
by.sub.l1$l1_acc_matched = NULL

#scale all variables within language in L1 
by.sub.l1.s <- 
  by.sub.l1 %>%
  group_by(l1_lang) %>%
  mutate(l1_skip = scale(l1_skip),
         l1_nfix = scale(l1_nfix),
         l1_dur = scale(l1_dur),
         l1_reg.in = scale(l1_reg.in),
         l1_firstfix.dur = scale(l1_firstfix.dur),
         l1_firstrun.dur = scale(l1_firstrun.dur),
         l1_refix = scale(l1_refix),
         l1_reread = scale(l1_reread),
         l1_rate = scale(l1_rate),
         l1_acc = scale(l1_acc)) 

by.sub.l1 <- by.sub.l1.s
rm(by.sub.l1.s)

by.sub <- inner_join(by.sub.l1, by.sub.l2, by = c("l1_uniform_id" = "l2_uniform_id"))

rm(by.sub.l1, by.sub.l2)
colnames(by.sub)[1] = "uniform_id"

load("../primary data/individual differences data/joint.ind.diff.l2.rda")
joint_id$TOWRE_nonword = as.numeric(joint_id$TOWRE_nonword)
joint_id$TOWRE_word = as.numeric(joint_id$TOWRE_word)
colnames(joint_id)

by.sub.ind = merge(by.sub[,-2], joint_id[,-1], by = "uniform_id")

ind.vars = colnames(joint_id)[-c(1,2,5,9)]
u <- colnames(by.sub.ind)[13:22]
u.l1 <- gsub("l2", "l1", u)

by.sub.ind$is.l1 = ifelse(by.sub.ind$lang == "en", "l1", "l2")
by.sub.ind$is.l1 = as.factor(by.sub.ind$is.l1)

## remove L1 sample 
by.sub.ind = by.sub.ind[by.sub.ind$is.l1 == "l2",]
by.sub.ind = droplevels(by.sub.ind)


######## hierarchical regression #########
r2_full = step1_l1_em = step2_skill_tests = step3_l2_sample = vector()

for (i in 1:length(u)) {
  by.sub.ind$dv = by.sub.ind[,u[i]]
  
  
  #step1 model: l1 var
  mod.step1 =lm(formula(paste("dv", "~", u.l1[i])), data = by.sub.ind)
  
  step1_l1_em[i] = round(100*summary(mod.step1)$adj.r.squared,2)
  
  #step2 model: skill tests
  mod.step2 = lm(formula(paste("dv", "~", u.l1[i],"+", paste(ind.vars, collapse = " + "))), data = by.sub.ind)
  step2_skill_tests[i] = round(100*summary(mod.step2)$adj.r.squared,2) - round(100*summary(mod.step1)$adj.r.squared,2)  ##NOTE: this is already a difference from previous step 
  
  #step3 model: l2 sample 
  mod.step3 = lm(formula(paste("dv", "~", "lang", "+", u.l1[i],"+", paste(ind.vars, collapse = " + "))), data = by.sub.ind)
  step3_l2_sample[i] = round(100*summary(mod.step3)$adj.r.squared,2) - round(100*summary(mod.step2)$adj.r.squared,2)  ##NOTE: this is already a difference from previous step 
  
  #full model (same as last step): for sorting plot
  r2_full[i] = round(100*summary(mod.step3)$adj.r.squared,2)
}  



d = data.frame(cbind(step1_l1_em, step2_skill_tests, step3_l2_sample, r2_full))

d$var = u

d.plot <- d

d.plot.2 <- d.plot[,c(5,1,2,3,4)]

colnames(d.plot.2)

d.plot.long = gather(d.plot.2, type, r2, step1_l1_em:step3_l2_sample, factor_key=TRUE)

d.plot.long$type = gsub("step1_", "step1: ", d.plot.long$type)
d.plot.long$type = gsub("step2_", "step2: ", d.plot.long$type)
d.plot.long$type = gsub("step3_", "step3: ", d.plot.long$type)

d.plot.long$type = gsub("_", " ", d.plot.long$type)

d.plot.long$type = gsub("step1: l1 em", "step1: l1 reading", d.plot.long$type)

d.plot.long$type = gsub("l1", "L1", d.plot.long$type)
d.plot.long$type = gsub("l2", "L2", d.plot.long$type)

d.plot.long$type = as.factor(d.plot.long$type)
d.plot.long$type = factor(d.plot.long$type, levels = c('step3: L2 sample', 'step2: skill tests', 'step1: L1 reading')) #flip for plot 

ggplot(data = d.plot.long, aes(x = reorder(var, -r2_full), y = r2, fill = type)) + 
  geom_bar(position = "stack", stat = "identity") + 
  coord_cartesian(ylim=c(0, 100)) + 
  scale_fill_manual(values = c("steelblue4", "red", "orange")) +
  theme_bw(base_size = 18) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab("L2 reading measure") + 
  ylab("percent variance explained") + 
  theme(legend.title=element_blank()) 

####### partioning of variance (non-hierarchical) ########
r2_full = l1_var = skill_tests = which_l2 = overlap = vector()

for (i in 1:length(u)) {
  by.sub.ind$dv = by.sub.ind[,u[i]]

  mod.full = lm(formula(paste("dv", "~", "lang", "+", u.l1[i],"+", paste(ind.vars, collapse = " + "))), data = by.sub.ind) #full model. same as lang + ind.diff.
  r2_full[i] = round(100*summary(mod.full)$adj.r.squared,2)

  #comparison for l1_var
  mod.comp.l1_var = lm(formula(paste("dv", "~", "lang","+", paste(ind.vars, collapse = " + "))), data = by.sub.ind) #no l1_var
  l1_var[i] = round(100*summary(mod.full)$adj.r.squared,2) - round(100*summary(mod.comp.l1_var)$adj.r.squared,2)


  #comparison for ind-diff
  mod.comp.ind.diff = lm(formula(paste("dv", "~", "lang", "+", u.l1[i])), data = by.sub.ind) #no ind diff
  skill_tests[i] = round(100*summary(mod.full)$adj.r.squared,2) - round(100*summary(mod.comp.ind.diff)$adj.r.squared,2)

  #comparison for lang
  mod.comp.lang = lm(formula(paste("dv","~", u.l1[i], "+", paste(ind.vars, collapse = " + "))), data = by.sub.ind)
  which_l2[i] = round(100*summary(mod.full)$adj.r.squared,2) - round(100*summary(mod.comp.lang)$adj.r.squared,2)

  #overlap: part in full that is not unique to any model
  overlap[i] = r2_full[i] - l1_var[i] - skill_tests[i] - which_l2[i]


}



d = data.frame(cbind(r2_full, l1_var, skill_tests, which_l2, overlap))

d$var = u

d.plot <- d

d.plot.2 <- d.plot[,c(6,2,3,4,5,1)]

colnames(d.plot.2) = c("var", "l1_reading", "skill tests", "L2 sample", "shared", "r2_full")

d.plot.long = gather(d.plot.2, type, r2, l1_reading:shared, factor_key=TRUE)

d.plot.long$type = gsub("_", " ", d.plot.long$type)
d.plot.long$type = gsub("l1", "L1", d.plot.long$type)
d.plot.long$type = gsub("l2", "L2", d.plot.long$type)

d.plot.long$type = as.factor(d.plot.long$type)
d.plot.long$type = factor(d.plot.long$type, levels = c('shared',  'L2 sample', 'skill tests', 'L1 reading'))

ggplot(data = d.plot.long, aes(x = reorder(var, -r2_full), y = r2, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_manual(values = c("purple", "steelblue4", "red",  "orange")) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("L2 reading measure") +
  ylab("percent variance explained") +
  theme(legend.title=element_blank())

