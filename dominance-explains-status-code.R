#######################################################################################################################
##### Dominance is necessary to explain human status hierarchies (extended online version & supplemental): R Code #####
#######################################################################################################################

## Script prepared by: Joey Cheng
## Instructions: https://github.com/joeytcheng/Dominance-Necessary-to-Explain-Status/
## Email: chengjt@yorku.ca
## Last updated: Dec 1, 2020 

#########################
##### Load packages #####
#########################

# Load packages (after ensuring that the packages specified below have been installed)

libs <-
  c(
    "brms",
    "car",
    "dplyr",
    "factoextra",
    "faux",
    "GGally",
    "ggcorrplot",
    "ggdendro",
    "ggplot2",
    "ggpubr",
    "ggrepel",
    "ggsignif",
    "Hmisc",
    "jtools",
    "psych",
    "RColorBrewer",
    "readxl",
    "rstatix",
    "sjPlot",
    "stargazer",
    "stringr",
    "tidybayes",
    "tidyr",
    "viridis"
  )
lapply(libs, require, character.only = TRUE)
options(scipen = 999)



# Set working directory (to where the data files are stored)

setwd(
  "C:/Dropbox/Research/Current Work/Manuscripts/PNAS Durkee Commentary (with Jess & Joe)/Manuscript/GitHub Materials/Process/Test My R Code Posted 12.02.20"
)

###################################################################################
##### S1. Collinearity limits inferences about the importance of dominance ########
###################################################################################

######################################
## S1.1 The problem of collinearity ##
######################################

# Load Durkee et al.'s original data & data wrangling (code adapted from Durkee et al.)

status_df <- read.csv("LongStatusData.csv")

# convert status ratings to long format for averaging
status_long <- 
  status_df %>% 
  gather(key = item_id, value = rating, -c(p_id:ses)) %>% 
  separate(item_id, into = c("prompt", "item_id")) %>% 
  mutate(item_id = as.numeric(item_id),
         p_id = paste("STATUS", p_id, sep = ""))

# get rid of weird NA that popped up
status_long <- status_long[!is.na(status_long$item_id),] 

status_agg <- 
  status_long %>% 
  group_by(Country, TargetSex, item_id) %>% 
  summarise(Status = mean(rating, na.rm = T)) %>% 
  # spread(key = Country, value = Status) %>% 
  ungroup
  # warnings()

status_agg[status_agg == "NaN"] <- NA

BGA <- read.csv("Mturk_BG-A.csv") # benefit generation ability
BGA$prompt <- "BGA" # column indicating which prompt
nrow(BGA)

BGW <- read.csv("Mturk_BG-W.csv") # benefit generation willingness
BGW$prompt <- "BGW" # column indicating which prompt
nrow(BGW)

CIA <- read.csv("Mturk_CI-A.csv") #cost infliction ability
CIA$prompt <- "CIA" # column indicating which prompt
nrow(CIA)

CIW <- read.csv("Mturk_CI-W.csv") #cost infliction willingness
CIW$prompt <- "CIW" # column indicating which prompt
nrow(CIW)

# row bind files into one dataset
inference_raw <- rbind(BGA, BGW, CIA, CIW)
full_inference_n <- nrow(inference_raw)

# get only needed columns
inference_df <- inference_raw[, -c(1:17)] # removing qualtrics metadata
#make names match status data
demo_names <- c("sex", "gender", "age", "countryReside", "countryOrigin", "ses")
m_names <- paste("Man_", 1:240, sep = "")
M_attention_names <- paste("M.att.", 1:5, sep = "")
f_names <- paste("Woman_", 1:240, sep = "")
F_attention_names <- paste("F.att.", 1:5, sep = "")
full_names <- c(demo_names, m_names, M_attention_names, 
                f_names, F_attention_names, "p_id", "prompt")

colnames(inference_df) <- full_names

#Make attention check column of sums (correct answers = 1, else = 0)
inference_df$attention <- 
  rowSums(inference_df[, c(which(colnames(inference_df) == "M.att.1"):
                             which(colnames(inference_df) == "M.att.5"),
                           which(colnames(inference_df) == "F.att.1"):
                             which(colnames(inference_df) == "F.att.5"))], 
          na.rm = T)
#select only respondents who answered at least 3/5 correctly
include <- subset(inference_df, attention >= 3)
n_excluded <- full_inference_n - nrow(include)

include %>% group_by(prompt, sex) %>% 
  summarise(n = n())

mean(include$age, na.rm = T)
sd(include$age, na.rm = T)

inference_df <- 
  include %>% 
  select(-attention, -gender, -countryReside, -countryOrigin,
         M.att.1:M.att.5, F.att.1:F.att.5) %>% 
  gather(key = targetsex_item, value = rating, 
         -sex, -age,  -ses, -prompt, -p_id) %>% 
  separate(targetsex_item, 
           into = c("TargetSex", "item_id")) %>% 
  mutate(p_id = paste(prompt, p_id, sep = ""),
         item_id = as.numeric(item_id),
         TargetSex = as.factor(TargetSex),
         RaterSex = ifelse(sex == 1, "Man", 
                           ifelse(sex == 2, "Woman", "Other")))

# get rid of weird NA that popped up
inference_df <- inference_df[!is.na(inference_df$item_id),] 

inference_agg <- 
  inference_df %>% 
  group_by(prompt, item_id, TargetSex) %>% 
  summarise(rating = mean(rating, na.rm = T)) %>% 
  spread(key = prompt, value = rating)

agg_df <- 
  status_agg %>% 
  group_by(Country, TargetSex) %>%
  mutate(Status = scale(Status)) %>%
  ungroup() %>%
  full_join(inference_agg) %>% 
  group_by(TargetSex) %>% 
  mutate(BGA = scale(BGA),
         BGW = scale(BGW), 
         CIA = scale(CIA),
         CIW = scale(CIW))

# glimpse(agg_df)


# Figure 1 | A pairs plot showing pairwise associations between the four predictors in Durkee et al.'s data

ggscatmat(agg_df, columns = 5:8, color = "TargetSex", alpha = .02)

# Get VIF among Durkee et al.'s predictors

fit.4.predictors <- lm(Status ~
                         BGA + BGW + CIA + CIW,
                       data = agg_df)
vif(fit.4.predictors)



##################################
## S1.2 Simulating collinearity ##
##################################

# Simulate 5 datasets that vary in degree of collinearity

set.seed(48096325)

# Case 1. No collinearity

cmatCase1 <-
  c(1, .5, .5, .4, .4,  # corrs between DV & 4 predictors
    .5, 1, 0, 0, 0,
    .5, 0, 1, 0, 0,
    .4, 0, 0, 1, 0,
    .4, 0, 0, 0, 1)
Case1.df <- # sim 240 samples, 5 vars, M = 0, SD = 1
  rnorm_multi( 
    240,
    5,
    0,
    1,
    
    cmatCase1,
    varnames = c( # variable names
      "Status",
      "BGA",
      "BGW",
      "CIA",
      "CIW"),
    empirical = TRUE
  )

ggpairs(Case1.df, # get scatterplot matrix
        lower = list(continuous = wrap(ggally_points,
                                       alpha = .2)))


# Case 2. Low collinearity

cmatCase2 <- c(1, .5, .5, .4, .4,
                 .5, 1, .2, .1, .1,
                 .5, .2, 1, .1, .1,
                 .4, .1, .1, 1, .2,
                 .4, .1, .1, .2, 1)
Case2.df <- rnorm_multi(240, 5, 0, 1,
  cmatCase2,
  varnames = c("Status", "BGA", "BGW", "CIA", "CIW"),
  empirical = TRUE)

ggpairs(Case2.df,
        lower = list(continuous = wrap(ggally_points,
                                       alpha = .2)))
        
# Case 3. Moderate collinearity

cmatCase3 <- c(1, .5, .5, .4, .4,
                 .5, 1, .6, .5, .5,
                 .5, .6, 1, .5, .5,
                 .4, .5, .5, 1, .6,
                 .4, .5, .5, .6, 1)
Case3.df <- rnorm_multi(240, 5, 0, 1,
  cmatCase3,
  varnames = c("Status", "BGA", "BGW", "CIA", "CIW"),
  empirical = TRUE)
 
ggpairs(Case3.df,
        lower = list(continuous = wrap(ggally_points,
                                       alpha = .2)))

# Case 4. High collinearity

cmatCase4 <- c(1, .5, .5, .4, .4,
                 .5, 1, .8, .7, .7,
                 .5, .8, 1, .7, .7,
                 .4, .7, .7, 1, .8,
                 .4, .7, .7, .8, 1)
Case4.df <- rnorm_multi(240, 5, 0, 1,
  cmatCase4,
  varnames = c("Status", "BGA", "BGW", "CIA", "CIW"),
  empirical = TRUE)

ggpairs(Case4.df,
        lower = list(continuous = wrap(ggally_points,
                                       alpha = .2)))

# Case 5. Severe collinearity

cmatCase5 <- c(1, .5, .5, .4, .4,
                 .5, 1, .9, .8, .8,
                 .5, .9, 1, .8, .8,
                 .4, .8, .8, 1, .9,
                 .4, .8, .8, .9, 1)
Case5.df <- rnorm_multi(240, 5, 0, 1,
  cmatCase5,
  varnames = c("Status", "BGA", "BGW", "CIA", "CIW"),
  empirical = TRUE)

ggpairs(Case5.df,
        lower = list(continuous = wrap(ggally_points,
                                       alpha = .2)))

# Run Bayesian models on the 5 simulated datasets above

brmmax_Case1.df <-
  brm(Status ~ BGA + BGW + CIA + CIW,
       data = Case1.df,
       iter = 4000)
summary(brmmax_Case1.df)

brmmax_Case2.df <-
  brm(Status ~ BGA + BGW + CIA + CIW,
       data = Case2.df,
       iter = 4000)
summary(brmmax_Case2.df)

brmmax_Case3.df <-
  brm(Status ~ BGA + BGW + CIA + CIW,
       data = Case3.df,
       iter = 4000)
summary(brmmax_Case3.df)

brmmax_Case4.df <-
  brm(Status ~ BGA + BGW + CIA + CIW,
       data = Case4.df,
       iter = 4000)
summary(brmmax_Case4.df)

brmmax_Case5.df <-
  brm(Status ~ BGA + BGW + CIA + CIW,
       data = Case5.df,
       iter = 4000)
summary(brmmax_Case5.df)

# Table 1 | Print results from Bayesian models above into a table

tab_model(
  brmmax_Case1.df,
  brmmax_Case2.df,
  brmmax_Case3.df,
  brmmax_Case4.df,
  brmmax_Case5.df,
  collapse.se = TRUE,
  collapse.ci = TRUE,
  p.val = "kr",
  show.zeroinf = T,
  show.se = T,
  show.df = T,
  show.icc = F,
  show.re.var = T,
  dv.labels = c("Case.1",
                "Case.2",
                "Case.3",
                "Case.4",
                "Case.5"),
  string.est = " ",
  string.pred = " ",
  title = "Table 1 | Data simulations show that model estimates are severely negatively biased and imprecisely estimated under high collinearity (Bayesian linear regression).",
  order.terms = c(2, 3, 4, 5, 1),
  CSS = list(css.depvarheadnodv = 'font-style: normal;', css.caption = 'font-weight: normal;')
)

# Run OLS regression models on the 5 simulated datasets above

ols_Case1.df <- lm(Status ~ BGA + BGW + CIA + CIW, data = Case1.df)
summ(ols_Case1.df, vifs = TRUE)

ols_Case2.df <- lm(Status ~ BGA + BGW + CIA + CIW, data = Case2.df)
summ(ols_Case2.df, vifs = TRUE)

ols_Case3.df <- lm(Status ~ BGA + BGW + CIA + CIW, data = Case3.df)
summ(ols_Case3.df, vifs = TRUE)

ols_Case4.df <- lm(Status ~ BGA + BGW + CIA + CIW, data = Case4.df)
summ(ols_Case4.df, vifs = TRUE)

ols_Case5.df <- lm(Status ~ BGA + BGW + CIA + CIW, data = Case5.df)
summ(ols_Case5.df, vifs = TRUE)


# Table 2 | Print results from OLS regression models above into a table

stargazer(
  ols_Case1.df,
  ols_Case2.df,
  ols_Case3.df,
  ols_Case4.df,
  ols_Case5.df,
  title = "Table 2 | Data simulations show that model estimates are severely negatively biased and imprecisely estimated under high collinearity (ordinary least squares regression).",
  type = 'text',
  intercept.bottom = T,
  intercept.top = F,
  ci = F,
  se = NULL,
  digits = 2,
  notes = "OLS, standard errors in parentheses",
  single.row = F,
  dep.var.caption = "Simulated Dataset",
  object.names = FALSE,
  dep.var.labels.include = FALSE,
  column.labels=c("Case.1", "Case.2", "Case.3", "Case.4", "Case.5"),
  model.numbers = FALSE,
  covariate.labels = c(
    "Benefit-Generation Ability (BGA)",
    "Benefit-Generation Willingness (BGW)",
    "Cost-Infliction Ability (CIA)",
    "Cost-Infliction Willingness (CIW)",
    "Intercept"
  )
)

##########################################################################
## S1.3 Partially correcting for collinearity in Durkee et al.’s models ##
##########################################################################

# Reproduce Durkee et al.'s results (code adapted from Durkee et al.)

brmmax_men <-
  brm(Status ~ BGA + BGW + CIA + CIW +
         (1 + BGA + BGW + CIA + CIW | Country) +
         (1 | item_id),
       data = agg_df %>% filter(TargetSex == "Man"),
       iter = 4000)
summary(brmmax_men)

brmmax_women <-
  brm(Status ~ BGA + BGW + CIA + CIW +
         (1 + BGA + BGW + CIA + CIW | Country) +
         (1 | item_id),
       data = agg_df %>% filter(TargetSex == "Woman"),
      iter = 4000)
summary(brmmax_women)


# Compute benefits and costs composite variables

agg_df$BGcomposite <- rowMeans(subset(agg_df, select = c(BGA, BGW)))
agg_df$CIcomposite <- rowMeans(subset(agg_df, select = c(CIA, CIW)))

# Standardize composite variables within target sex

agg_df <- agg_df %>% 
  group_by(TargetSex) %>% 
  mutate(BGcomposite = scale(BGcomposite),
         CIcomposite = scale(CIcomposite))

# Run Bayesian models using benefits and costs composites as predictors

brmmax_men_composite <-
  brm(
    Status ~ BGcomposite + CIcomposite +
      (1 + BGcomposite + CIcomposite | Country) +
      (1 | item_id),
    data = agg_df %>% filter(TargetSex == "Man"),
    iter = 4000,
    control = list(adapt_delta = 0.99) # increase adapt_delta
  )
summary(brmmax_men_composite)

brmmax_women_composite <-
  brm(
    Status ~ BGcomposite + CIcomposite +
      (1 + BGcomposite + CIcomposite | Country) +
      (1 | item_id),
    data = agg_df %>% filter(TargetSex == "Woman"),
    iter = 4000)
summary(brmmax_women_composite)


# Table 3 | Print results from Bayesian models above to a table (models 1 & 2 = all 4 predictors as in Durkee et al.; models 3 & 4 = composite predictors only)

tab_model(
  brmmax_men,
  brmmax_women,
  brmmax_men_composite,
  brmmax_women_composite,
  collapse.se = TRUE,
  collapse.ci = TRUE,
  p.val = "kr",
  show.zeroinf = T,
  show.se = T,
  show.df = T,
  show.re.var = F,
  show.icc = F,
  dv.labels = c(
    "All.Predictors: Men",
    "All.Predictors: Women",
    "Composites: Men",
    "Composites: Women"
  ),
  title = "Table 3 | Perceived status impact is unrelated to cost-infliction when all four collinear predictors are included, but is positively predicted by cost-infliction when benefits and costs composites are included as predictors instead.",
  order.terms = c(2, 3, 4, 5, 6, 7, 1),
  CSS = list(css.depvarheadnodv = 'font-style: normal;', css.caption = 'font-weight: normal;')
)

# See associations between benefits and costs composite variables

ggscatmat(agg_df, columns = 9:10, color = "TargetSex", alpha = .02)

# Get VIF among benefits and costs composite variables in male vs. female targets

ols.composite.men <-
  lm(Status ~ BGcomposite + CIcomposite,
     data = agg_df %>% filter(TargetSex == "Man"))
vif(ols.composite.men)   # get vif in male targets

ols.composite.women <-
  lm(Status ~ BGcomposite + CIcomposite,
     data = agg_df %>% filter(TargetSex == "Woman"))
vif(ols.composite.women) # get vif in female targets


# Run Bayesian models using ability predictors only (BGA & CIA)

brmmax_men_ability <-
  brm(Status ~ BGA + CIA +
         (1 + BGA + CIA | Country) +
         (1 | item_id),
       data = agg_df %>% filter(TargetSex == "Man"),
       iter = 4000)
summary(brmmax_men_ability)

brmmax_women_ability <-
  brm(Status ~ BGA + CIA +
         (1 + BGA + CIA | Country) +
         (1 | item_id),
       data = agg_df %>% filter(TargetSex == "Woman"),
      iter = 4000)
summary(brmmax_women_ability)

# Run Bayesian models using willingness predictors only (BGW & CIW)

brmmax_men_willingness <-
  brm(Status ~ BGW + CIW +
         (1 + BGW + CIW | Country) +
         (1 | item_id),
       data = agg_df %>% filter(TargetSex == "Man"),
       iter = 4000)
summary(brmmax_men_willingness)

brmmax_women_willingness <-
  brm(Status ~ BGW + CIW +
         (1 + BGW + CIW | Country) +
         (1 | item_id),
       data = agg_df %>% filter(TargetSex == "Woman"),
      iter = 4000)
summary(brmmax_women_willingness)


# Table 4 | Print results from Bayesian models above to a table (models 1 & 2 = ability predictors only; models 3 & 4 = willingness predictors only)

tab_model(
  brmmax_men_ability,
  brmmax_women_ability,
  brmmax_men_willingness,
  brmmax_women_willingness,
  collapse.se = TRUE,
  collapse.ci = TRUE,
  p.val = "kr",
  show.zeroinf = T,
  show.se = T,
  show.df = T,
  show.re.var = F,
  show.icc = F,
  dv.labels = c(
    "Ability: Men",
    "Ability: Women",
    "Willingness: Men",
    "Willingess: Women"
  ),
  title = "Table 4 | Perceived status impact increases with cost-infliction for both male and female targets.",
  order.terms = c(2, 3, 4, 5, 1),
  CSS = list(css.depvarheadnodv = 'font-style: normal;', css.caption = 'font-weight: normal;')
)


####################################################################################
##### S2. Problems with operationalization and measurement of social status ########
####################################################################################

##################################################################################
## S2.1 Measures of (projected) status are biased to primarily capture prestige ##
##################################################################################

# Table 5 | Print prompts used by Durkee et al. to measured projected social status across sites

translated.prompts.df <- data.frame(image="![](durkeeTranslatedPrompts.png)")
translated.prompts.mat <- as.matrix(translated.prompts.df)
colnames(translated.prompts.mat) <- NULL
knitr::kable(translated.prompts.mat, caption="Table 5 | DLB's prompts used to measure participants' perceptions of status impact across sampling sites (English translations added here).")

####################################################################################################################################
## S2.2 Evidence of substantial semantic overlap between Durkee et al.'s measure of status ('status and reputation') and prestige ##
####################################################################################################################################

# Load Study 1 (Semantic Similarity Study) data mean dissimilarity scores

distanceMatrix <-
  read_excel(
    "study-1-semantic-similarity-study.xlsx",
    sheet = "Matrix (High Score = DISsimilar"
  )

distanceMatrix <- select(distanceMatrix,-c(1)) # drop first column which is the row label

# Coerce data frame to a distance matrix

distanceMatrix <- as.dist(distanceMatrix, diag = TRUE)


# Figure 2 | Hierarchical cluster analysis dendrogram (Ward method)

fit <- hclust(distanceMatrix, method="ward.D")
fviz_dend(
  fit,
  k = 3,
  # cut in three groups
  cex = 1,
  main = "",
  horiz = TRUE,
  k_colors = c("#2ca02c", "#ff7f0e", "mediumpurple"),
  rect = TRUE,
  rect_border = c("#2ca02c", "#ff7f0e", "mediumpurple"),
  rect_fill = TRUE,
  labels_track_height = 10,
  label_cols = c(
    "#2ca02c",
    "#2ca02c",
    "#2ca02c",
    "#2ca02c",
    "#2ca02c",
    "#2ca02c",
    "#2ca02c",
    "#ff7f0e",
    "#ff7f0e",
    "#ff7f0e",
    "#ff7f0e",
    "#ff7f0e",
    "#ff7f0e",
    "#ff7f0e",
    "mediumpurple",
    "mediumpurple",
    "mediumpurple",
    "mediumpurple",
    "red2",
    "red2",
    "mediumpurple",
    "mediumpurple",
    "mediumpurple",
    "mediumpurple",
    "mediumpurple",
    "mediumpurple",
    "mediumpurple"
  ),
  ggtheme = theme_classic()
)

# Load Study 1 (Semantic Similarity Study) data similarity ratings

AllClustersRatingsAvgAcrossSubjects <- read_excel("study-1-semantic-similarity-study.xlsx", sheet = "WordsAllClusters")

# Figure 3| Rated semantic similarity among status words

simRatingsFig <-
  ggplot(data = AllClustersRatingsAvgAcrossSubjects) +
  stat_summary(
    geom = "pointrange",
    fun.data = mean_cl_boot,
    mapping = aes(
      x = factor(Status.Word.Rated),
      y = Similarity.Rating.Between.Word.Pair,
      color = Word.Cluster
    ),
    position = position_dodge(width = 0.8),
    size = 1.5,
    alpha = 0.4
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme(text = element_text(size = 20)) +
  xlab("\nStatus Word Rated") + ylab("Word Pair Rated Similarity\n")

simRatingsFigText <-
  simRatingsFig + geom_text_repel(
    data = AllClustersRatingsAvgAcrossSubjects,
    mapping = aes(
      x = Status.Word.Rated,
      y = Similarity.Rating.Between.Word.Pair,
      label =
        Cluster.Word.Rated,
      colour = factor(Word.Cluster)
    ),
    size = 5,
    position = position_dodge(width = 0.8)
  ) + scale_color_discrete(name = "Word Cluster") +
  scale_color_brewer(palette = "Dark2")

AllClustersRatingsAvgAcrossSubjects$Word.Cluster <-
  as.factor(AllClustersRatingsAvgAcrossSubjects$Word.Cluster)

stat.test1 <- AllClustersRatingsAvgAcrossSubjects %>%
  group_by(Status.Word.Rated) %>%
  t_test(Similarity.Rating.Between.Word.Pair ~ Word.Cluster) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")

stat.test1 <- stat.test1 %>%
  add_xy_position(x = "Status.Word.Rated", dodge = 0.8)

simRatingsFigText.stat1 <- simRatingsFigText + 
  stat_pvalue_manual(
    stat.test1,
    label = "p.adj.signif",
    tip.length = 0,
    bracket.nudge.y = + 0.25
  )
simRatingsFigText.stat1


###################################################################################################################
## S2.4 Evidence that any results concerning the importance of dominance are sensitive to how status is measured ##
###################################################################################################################

# Load Study 2 Swapping Status Measure Study data

itemDataFrame <-
  read_excel("study-2-swapping-status-measure-study.xlsx")

# Compute benefits and costs composites

itemDataFrame$BGcomposite <- rowMeans(subset(itemDataFrame, select = c(BGA, BGW)))
itemDataFrame$CIcomposite <- rowMeans(subset(itemDataFrame, select = c(CIA, CIW)))

# Standardize variables to mean = 0, std = 1

itemDataFrame <- itemDataFrame %>% mutate_each_(
  funs(scale),
  vars = c(
    "statusReputationImpact",
    "influenceDecisionMakingImpact",
    "influenceImpact",
    "decisionMakingImpact",
    "attendedToImpact",
    "beingLeaderImpact",
    "inChargeImpact",
    "authorityImpact",
    "controlImpact",
    "dominanceImpact",
    "BGA",
    "BGW",
    "CIA",
    "CIW",
    "BGcomposite",
    "CIcomposite"
  )
) 


# Figure 4 | Correlation matrix of variables

itemDataFrameCorrMat <-
  itemDataFrame %>% select(-item,-countryImpactRatings)

corr <- round(cor(itemDataFrameCorrMat), 2)
p.mat <- cor_pmat(itemDataFrameCorrMat)

ggcorrplot(corr,
           title = "",
           hc.order = FALSE,
           outline.col = "white",
           type = "lower",
           tl.cex = 16,
           lab = TRUE,
           digits = 2)


# Run Bayesian models on the 10 different status DVs

statusReputation.brmmax.composite <- 
  brm(statusReputationImpact ~ BGcomposite + CIcomposite,
      data = itemDataFrame,
      iter = 4000)
# summary(statusReputation.brmmax.composite)

influenceDecisionMaking.brmmax.composite <-
  brm(
    influenceDecisionMakingImpact ~ BGcomposite + CIcomposite,
    data = itemDataFrame,
    iter = 4000)

influence.brmmax.composite <-
  brm(influenceImpact ~ BGcomposite + CIcomposite,
      data = itemDataFrame,
      iter = 4000)

decisionMaking.brmmax.composite <-
  brm(decisionMakingImpact ~ BGcomposite + CIcomposite,
      data = itemDataFrame,
      iter = 4000)

attendedTo.brmmax.composite <-
  brm(attendedToImpact ~ BGcomposite + CIcomposite,
      data = itemDataFrame,
      iter = 4000)

beingLeader.brmmax.composite <-
  brm(beingLeaderImpact ~ BGcomposite + CIcomposite,
      data = itemDataFrame,
      iter = 4000)

inCharge.brmmax.composite <-
  brm(inChargeImpact ~ BGcomposite + CIcomposite,
      data = itemDataFrame,
      iter = 4000)

authority.brmmax.composite <-
  brm(authorityImpact ~ BGcomposite + CIcomposite,
      data = itemDataFrame,
      iter = 4000)

control.brmmax.composite <-
  brm(controlImpact ~ BGcomposite + CIcomposite,
      data = itemDataFrame,
      iter = 4000)

dominance.brmmax.composite <-
  brm(dominanceImpact ~ BGcomposite + CIcomposite,
      data = itemDataFrame,
      iter = 4000)


# Figure 5 | Plot estimates from Bayesian models above visually

post1 <- posterior_samples(statusReputation.brmmax.composite) # save the posterior draws for each model as separate data frames
post2 <- posterior_samples(influenceDecisionMaking.brmmax.composite)
post3 <- posterior_samples(influence.brmmax.composite)
post4 <- posterior_samples(decisionMaking.brmmax.composite)
post5 <- posterior_samples(attendedTo.brmmax.composite)
post6 <- posterior_samples(beingLeader.brmmax.composite)
post7 <- posterior_samples(inCharge.brmmax.composite)
post8 <- posterior_samples(authority.brmmax.composite)
post9 <- posterior_samples(control.brmmax.composite)
post10 <- posterior_samples(dominance.brmmax.composite)

posts <-
  bind_rows(post1,
            post2,
            post3,
            post4,
            post5,
            post6,
            post7,
            post8,
            post9,
            post10) %>%
  mutate(
    DV = c(
      "Status & Reputation (DLB)",
      "Influence & Decision-Making Impact",
      "Influence",
      "Decision-Making Impact",
      "Being Attended to",
      "Being the Leader",
      "Being in Charge",
      "Authority",
      "Control",
      "Dominance"
    ) %>% rep(., each = 8000)
  )

posts <- select(posts, -c(b_Intercept, sigma, lp__))

long_posts <-
  posts %>% gather(key = predictor, value = Estimate, b_BGcomposite:b_CIcomposite) %>%
  mutate(predictor = str_remove(predictor, "b_"))

long_posts$DV = with(long_posts, reorder(DV, Estimate, median))

facet.labels <-
  c(BGcomposite = "Benefit-Generation Composite", CIcomposite = "Cost-Infliction Composite")

long_posts %>%
  ggplot(aes(x = Estimate,
             y = DV,
             fill = DV)) +
  stat_halfeye(alpha = .5) +
  ylab(NULL) +
  facet_wrap( ~ predictor, ncol = 1) +
  viridis::scale_fill_viridis(discrete = T) +
  facet_wrap( ~ predictor,
              ncol = 1,
              labeller = labeller(predictor = facet.labels)) +
  labs(fill = "Model DV (Status Prompt Used) \n") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold")
  )

# Figure 6 | Plot estimates from OLS regression models above visually

statusReputationImpactCompositeFit <- lm(statusReputationImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(statusReputationImpactCompositeFit, vifs = TRUE)

influenceDecisionMakingImpactCompositeFit <- lm(influenceDecisionMakingImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(influenceDecisionMakingImpactCompositeFit, vifs = TRUE)

influenceImpactCompositeFit <- lm(influenceImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(influenceImpactCompositeFit, vifs = TRUE)

decisionMakingImpactCompositeFit <- lm(decisionMakingImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(decisionMakingImpactCompositeFit, vifs = TRUE)

attendedToImpactCompositeFit <- lm(attendedToImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(attendedToImpactCompositeFit, vifs = TRUE)

beingLeaderImpactCompositeFit <- lm(beingLeaderImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(beingLeaderImpactCompositeFit, vifs = TRUE)

inChargeImpactCompositeFit <- lm(inChargeImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(inChargeImpactCompositeFit, vifs = TRUE)

authorityImpactCompositeFit <- lm(authorityImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(authorityImpactCompositeFit, vifs = TRUE)

controlImpactCompositeFit <- lm(controlImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(controlImpactCompositeFit, vifs = TRUE)

dominanceImpactCompositeFit <- lm(dominanceImpact ~ BGcomposite + CIcomposite, data = itemDataFrame)
summ(dominanceImpactCompositeFit, vifs = TRUE)

plot_summs(
  statusReputationImpactCompositeFit,
  influenceDecisionMakingImpactCompositeFit,
  influenceImpactCompositeFit,
  decisionMakingImpactCompositeFit,
  attendedToImpactCompositeFit,
  beingLeaderImpactCompositeFit,
  inChargeImpactCompositeFit,
  authorityImpactCompositeFit,
  controlImpactCompositeFit,
  dominanceImpactCompositeFit,
  scale = FALSE,
  robust = TRUE,
  point.shape = TRUE,
  plot.distributions = FALSE,
  colors = "Rainbow",
  legend.title = "Model DV (Status Prompt Used)",
  model.names = c(
    "Status & Reputation (DLB)",
    "Influence & Decision-Making Impact",
    "Influence",
    "Decision-Making Impact",
    "Being Attended to",
    "Being the Leader",
    "Being in Charge",
    "Authority",
    "Control",
    "Dominance"
  ),
  coefs = c(
    "Benefit-Generation Composite" = "BGcomposite",
    "Cost-Infliction Composite" = "CIcomposite"
  )
)

# Print session info

devtools::session_info()



















