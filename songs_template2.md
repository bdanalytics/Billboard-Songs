# Billboard Top10: Top10 classification:: template2
bdanalytics  

**  **    
**Date: (Sun) Jun 28, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/songs.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

Regression results:
First run:
    <glb_sel_mdl_id>: 
        OOB_RMSE=<0.4f>; new_RMSE=<0.4f>; <feat1>=<imp>; <feat2>=<imp>

Classification results:
First run:
    <glb_sel_mdl_id>: Leaderboard: <accuracy>
        newobs_tbl=[0=, 1=]; submit_filename=
        OOB_conf_mtrx=[YN=, NY=]=; max.Accuracy.OOB=; opt.prob.threshold.OOB=
            <feat1>=<imp>; <feat1>=<imp>; <feat1>=<imp>; 
            <txt.feat1>=<imp>; <txt.feat1>=<imp>; <txt.feat1>=<imp>; 

### Prediction Accuracy Enhancement Options:
- import.data chunk:
    - which obs should be in fit vs. OOB (currently dirty.0 vs .1 is split 50%)
    
- inspect.data chunk:
    - For date variables
        - Appropriate factors ?
        - Different / More last* features ?
        
- scrub.data chunk:        
- transform.data chunk:
    - derive features from multiple features
    
- manage.missing.data chunk:
    - Not fill missing vars
    - Fill missing numerics with a different algorithm
    - Fill missing chars with data based on clusters 
    
- extract.features chunk:
    - Text variables: move to date extraction chunk ???
        - Mine acronyms
        - Mine places

- Review set_global_options chunk after features are finalized

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")
#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/songs.csv"
glb_newdt_url <- "<newdt_url>"
glb_out_pfx <- "template2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newobs_dataset <- FALSE    # or TRUE
    glb_split_entity_newobs_datasets <- TRUE   # or FALSE
    glb_split_newdata_method <- "condition"          # "condition" or "sample" or "copy"
    glb_split_newdata_condition <- "year >= 2010"
    glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
    glb_split_sample.seed <- 123               # or any integer

glb_max_fitobs <- NULL # or any integer                         
glb_is_regression <- FALSE; glb_is_classification <- !glb_is_regression; 
    glb_is_binomial <- TRUE # or TRUE or FALSE

glb_rsp_var_raw <- "Top10"

# for classification, the response variable has to be a factor
glb_rsp_var <- "Top10.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"), 
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) {
#     return(log(raw))
    ret_vals <- rep_len(NA, length(raw)); ret_vals[!is.na(raw)] <- ifelse(raw[!is.na(raw)] == 1, "Y", "N"); return(relevel(as.factor(ret_vals), ref="N"))
#     #as.factor(paste0("B", raw))
#     #as.factor(gsub(" ", "\\.", raw))    
}
glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA))
```

```
## [1] Y    Y    N    N    <NA>
## Levels: N Y
```

```r
glb_map_rsp_var_to_raw <- function(var) {
#     return(exp(var))
    as.numeric(var) - 1
#     #as.numeric(var)
#     #gsub("\\.", " ", levels(var)[as.numeric(var)])
#     c("<=50K", " >50K")[as.numeric(var)]
#     #c(FALSE, TRUE)[as.numeric(var)]
}
glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA)))
```

```
## [1]  1  1  0  0 NA
```

```r
if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# year = the year the song was released
# songtitle = the title of the song
# artistname = the name of the artist of the song
# songID and artistID = identifying variables for the song and artist
# timesignature and timesignature_confidence = a variable estimating the time signature of the song, and the confidence in the estimate
# loudness = a continuous variable indicating the average amplitude of the audio in decibels
# tempo and tempo_confidence = a variable indicating the estimated beats per minute of the song, and the confidence in the estimate
# key and key_confidence = a variable with twelve levels indicating the estimated key of the song (C, C#, . . ., B), and the confidence in the estimate
# energy = a variable that represents the overall acoustic energy of the song, using a mix of features such as loudness
# pitch = a continuous variable that indicates the pitch of the song
# timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, . . . , timbre_11_min, and timbre_11_max = variables that indicate the minimum/maximum values over all segments for each of the twelve values in the timbre vector (resulting in 24 continuous variables)
# Top10 = a binary variable indicating whether or not the song made it to the Top 10 of the Billboard Hot 100 Chart (1 if it was in the top 10, and 0 if it was not)

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- NULL #c("songID")
glb_category_vars <- NULL # or c("<var1>", "<var2>")
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_map_vars <- NULL # or c("<var1>", "<var2>")
glb_map_urls <- list();
# glb_map_urls[["<var1>"]] <- "<var1.url>"

glb_assign_pairs_lst <- NULL; 
# glb_assign_pairs_lst[["<var1>"]] <- list(from=c(NA),
#                                            to=c("NA.my"))
glb_assign_vars <- names(glb_assign_pairs_lst)

# Derived features
glb_derive_lst <- NULL;

# Add logs of numerics that are not distributed normally ->  do automatically ???

# glb_derive_lst[["<feat.sfx>"]] <- list(
#     mapfn=function(Rasmussen) { return(ifelse(sign(Rasmussen) >= 0, 1, 0)) }
#     mapfn=function(PropR) { return(as.factor(ifelse(PropR >= 0.5, "Y", "N"))) }
#     mapfn=function(Week) { return(substr(Week, 1, 10)) }
#     mapfn=function(raw) { tfr_raw <- as.character(cut(raw, 5)); 
#                           tfr_raw[is.na(tfr_raw)] <- "NA.my";
#                           return(as.factor(tfr_raw)) }
#     , args=c("raw"))
#     mapfn=function(PTS, oppPTS) { return(PTS - oppPTS) }
#     , args=c("PTS", "oppPTS"))

# # If glb_allobs_df is not sorted in the desired manner
#     mapfn=function(Week) { return(coredata(lag(zoo(orderBy(~Week, glb_allobs_df)$ILI), -2, na.pad=TRUE))) }
#     mapfn=function(ILI) { return(coredata(lag(zoo(ILI), -2, na.pad=TRUE))) }
#     mapfn=function(ILI.2.lag) { return(log(ILI.2.lag)) }

# glb_derive_lst[["<txt_var>.niso8859.log"]] <- list(
#     mapfn=function(<txt_var>) { match_lst <- gregexpr("&#[[:digit:]]{3};", <txt_var>)
#                         match_num_vctr <- unlist(lapply(match_lst, 
#                                                         function(elem) length(elem)))
#                         return(log(1 + match_num_vctr)) }
#     , args=c("<txt_var>"))

#     mapfn=function(raw) { mod_raw <- raw;
#         mod_raw <- gsub("&#[[:digit:]]{3};", " ", mod_raw);
#         # Modifications for this exercise only
#         mod_raw <- gsub("\\bgoodIn ", "good In", mod_raw);
#                           return(mod_raw)

#         # Create user-specified pattern vectors 
# #sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
#         if (txt_var %in% c("Snippet", "Abstract")) {
#             txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
#                 as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
#                                                    glb_allobs_df[, txt_var]))
#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])

# glb_derive_lst[["<var1>"]] <- glb_derive_lst[["<var2>"]]

glb_derive_vars <- names(glb_derive_lst)
# tst <- "PropR.fctr"; args_lst <- NULL; for (arg in glb_derive_lst[[tst]]$args) args_lst[[arg]] <- glb_allobs_df[, arg]; print(head(args_lst[[arg]])); print(head(drv_vals <- do.call(glb_derive_lst[[tst]]$mapfn, args_lst))); 
# print(which_ix <- which(args_lst[[arg]] == 0.75)); print(drv_vals[which_ix]); 

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# User-specified exclusions  
glb_exclude_vars_as_features <- c("songID", "songtitle", "artistID", "artistname", "year") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](songs_template2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 8.268  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
#glb_chunks_df <- myadd_chunk(NULL, "import.data")

glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/songs.csv..."
## [1] "dimensions of data in ./data/songs.csv: 7,574 rows x 39 cols"
##   year                          songtitle        artistname
## 1 2010 This Is the House That Doubt Built A Day to Remember
## 2 2010                    Sticks & Bricks A Day to Remember
## 3 2010                         All I Want A Day to Remember
## 4 2010                   It's Complicated A Day to Remember
## 5 2010                          2nd Sucks A Day to Remember
## 6 2010                Better Off This Way A Day to Remember
##               songID           artistID timesignature
## 1 SOBGGAB12C5664F054 AROBSHL1187B9AFB01             3
## 2 SOPAQHU1315CD47F31 AROBSHL1187B9AFB01             4
## 3 SOOIZOU1376E7C6386 AROBSHL1187B9AFB01             4
## 4 SODRYWD1315CD49DBE AROBSHL1187B9AFB01             4
## 5 SOICMQB1315CD46EE3 AROBSHL1187B9AFB01             4
## 6 SOCEYON1315CD4A23E AROBSHL1187B9AFB01             4
##   timesignature_confidence loudness   tempo tempo_confidence key
## 1                    0.853   -4.262  91.525            0.953  11
## 2                    1.000   -4.051 140.048            0.921  10
## 3                    1.000   -3.571 160.512            0.489   2
## 4                    1.000   -3.815  97.525            0.794   1
## 5                    0.788   -4.707 140.053            0.286   6
## 6                    1.000   -3.807 160.366            0.347   4
##   key_confidence    energy pitch timbre_0_min timbre_0_max timbre_1_min
## 1          0.453 0.9666556 0.024        0.002       57.342       -6.496
## 2          0.469 0.9847095 0.025        0.000       57.414      -37.351
## 3          0.209 0.9899004 0.026        0.003       57.422      -17.222
## 4          0.632 0.9392072 0.013        0.000       57.765      -32.083
## 5          0.483 0.9877376 0.063        0.000       56.872     -223.922
## 6          0.627 0.9799530 0.038        0.000       57.083      -40.408
##   timbre_1_max timbre_2_min timbre_2_max timbre_3_min timbre_3_max
## 1      171.093      -81.664       95.117     -285.049      259.426
## 2      171.130     -149.589      180.334     -380.143      384.166
## 3      171.060      -72.912      157.925     -203.984      251.258
## 4      220.895     -138.596      173.365      -73.490      373.492
## 5      171.130     -147.153      166.008     -128.082      389.419
## 6      174.002      -83.829      126.879     -100.119      173.607
##   timbre_4_min timbre_4_max timbre_5_min timbre_5_max timbre_6_min
## 1      -40.385       73.630     -104.683      183.089      -88.771
## 2      -48.662      100.414      -87.267       42.798      -86.895
## 3      -66.044      152.095      -98.673      141.365      -88.874
## 4      -55.607      119.158      -77.515      141.178      -70.790
## 5      -43.908       99.332      -96.147       38.303     -110.757
## 6      -33.789       66.904      -84.451       47.268      -71.219
##   timbre_6_max timbre_7_min timbre_7_max timbre_8_min timbre_8_max
## 1       73.549      -71.127       82.475      -52.025       39.116
## 2       75.455      -65.807      106.918      -61.320       35.378
## 3       66.504      -67.433       80.621      -59.773       45.979
## 4       64.540      -63.667       96.675      -78.660       41.088
## 5       72.391      -55.935      110.332      -56.450       37.555
## 6       71.239      -79.948       91.117      -54.378       53.808
##   timbre_9_min timbre_9_max timbre_10_min timbre_10_max timbre_11_min
## 1      -35.368       71.642      -126.440        18.658       -44.770
## 2      -81.928       74.574      -103.808       121.935       -38.892
## 3      -46.293       59.904      -108.313        33.300       -43.733
## 4      -49.194       95.440      -102.676        46.422       -59.439
## 5      -48.588       67.570       -52.796        22.888       -50.414
## 6      -33.183       54.657       -64.478        34.522       -40.922
##   timbre_11_max Top10
## 1        25.989     0
## 2        22.513     0
## 3        25.744     0
## 4        37.082     0
## 5        32.758     0
## 6        36.453     0
##      year                    songtitle    artistname             songID
## 262  2010 When Love Gets a Hold of You          Reba SOCZKPW12C5641317E
## 1154 2008               Take You There Sean Kingston SORKRBM1374015BCDA
## 3857 2002                          One    Faith Hill SOSOGQP135C392FC94
## 5511 1998            Domestic Violence           RZA SOCZFNY1311AFE8F95
## 5569 1998                Feel This Way     Zebrahead SOGZAIO13129A92246
## 7495 1990                 Liberty Ship      The La's SOWDRVI12A8C13C63C
##                artistID timesignature timesignature_confidence loudness
## 262  ARPH1W61187B9B4CD8             4                    0.655   -4.984
## 1154 AR9LJ9D1187FB4303D             4                    1.000  -12.654
## 3857 ARVNGA71187FB3C107             4                    1.000   -3.493
## 5511 AROUGGS1187FB51CA1             4                    0.937   -7.731
## 5569 ARCBQRB1187FB52F88             4                    1.000   -6.590
## 7495 AR8V85J1187B9ACB4A             4                    1.000   -8.529
##        tempo tempo_confidence key key_confidence    energy pitch
## 262  128.090            1.000   0          0.660 0.7772171 0.020
## 1154 116.024            0.419   8          0.237 0.6961294 0.009
## 3857  79.001            0.601   6          0.254 0.7157751 0.005
## 5511  90.840            0.754   1          0.060 0.6650303 0.003
## 5569 111.221            1.000   6          0.295 0.9551664 0.004
## 7495  99.405            0.820  11          0.019 0.7127939 0.030
##      timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 262         0.020       58.303     -129.068      170.769     -124.348
## 1154       34.605       50.296      -42.595      233.750     -142.882
## 3857        0.000       59.939     -132.546      243.926     -193.551
## 5511        0.000       54.832      -64.913      243.222     -159.325
## 5569        0.000       55.962     -238.622      180.486     -127.988
## 7495        0.006       55.901      -51.119      230.989     -117.601
##      timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 262       174.242     -157.578       95.009      -56.617       79.562
## 1154      127.075     -132.503       96.467      -66.977      118.690
## 3857      167.289     -273.823      183.082      -57.079      165.864
## 5511      142.525     -156.074      319.301     -119.629      119.534
## 5569      122.048     -102.617       93.045      -71.845      113.590
## 7495      100.957     -171.453       95.348     -106.919       57.326
##      timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 262       -85.451       57.943      -77.515       61.073      -67.306
## 1154      -74.795      110.296      -90.218       63.028     -103.846
## 3857     -112.776      199.830     -105.671       79.788     -105.861
## 5511     -129.610      274.626     -102.462       56.879     -124.531
## 5569     -123.461       45.145      -84.796       97.613      -82.657
## 7495      -82.783      185.709     -106.765       58.805      -80.145
##      timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 262        59.944      -83.330       44.507      -45.331       49.539
## 1154       94.827      -63.906       39.722      -49.856       74.920
## 3857       95.457      -70.141       75.744      -64.718       68.458
## 5511       99.836      -81.727       74.712      -72.099       84.240
## 5569       65.853      -45.191       80.223      -81.819       73.628
## 7495       55.620      -98.838       30.321      -55.246       55.172
##      timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 262        -83.405        50.168       -59.199        33.327     0
## 1154      -103.825        82.409       -63.028        42.040     1
## 3857       -99.661        72.336       -67.201        65.759     0
## 5511       -97.461        85.892       -59.753        56.363     0
## 5569       -76.362        49.676       -71.164        35.919     0
## 7495       -50.047        32.024       -36.554        63.253     0
##      year              songtitle artistname             songID
## 7569 1990        Red Hot & Ready        Y&T SOHVWXR12A6D4FC59C
## 7570 1990             She's Gone        Y&T SOSIEQB12A6D4FC59D
## 7571 1990             Let It Out        Y&T SOIGIQI12A6D4FC59E
## 7572 1990             Ten Lovers        Y&T SOLNEQO12A6D4FC59F
## 7573 1990 Goin' Off The Deep End        Y&T SOEMJEP12A58A7E7B5
## 7574 1990              Surrender        Y&T SOLBQSX12A6D4FC5A0
##                artistID timesignature timesignature_confidence loudness
## 7569 ARGQANQ11F50C4769E             4                    1.000  -10.574
## 7570 ARGQANQ11F50C4769E             4                    1.000  -10.197
## 7571 ARGQANQ11F50C4769E             4                    1.000  -12.392
## 7572 ARGQANQ11F50C4769E             4                    0.984  -10.304
## 7573 ARGQANQ11F50C4769E             4                    0.907   -9.295
## 7574 ARGQANQ11F50C4769E             4                    0.987   -9.762
##        tempo tempo_confidence key key_confidence    energy pitch
## 7569 137.872            1.000   2          0.006 0.9736458 0.030
## 7570  93.140            0.859   5          0.889 0.9429917 0.016
## 7571  79.858            0.196   9          0.149 0.8124216 0.012
## 7572  91.760            0.592   2          0.077 0.7368709 0.016
## 7573 110.907            0.838   9          0.621 0.9900532 0.061
## 7574 139.650            0.781   6          0.193 0.9448227 0.027
##      timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 7569            0       51.847      -18.801      203.289     -141.762
## 7570            0       53.462      -22.878      202.424      -77.467
## 7571            0       51.354      -91.916      202.639      -76.736
## 7572            0       53.358      -10.087      202.877      -48.768
## 7573            0       52.928      -15.289      175.845     -119.044
## 7574            0       52.989      -32.907      195.601     -146.973
##      timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 7569      157.204     -120.035      364.391      -53.240       99.468
## 7570      174.663     -170.802      201.677      -45.807      109.383
## 7571      165.643     -186.653      166.200      -63.367      132.495
## 7572      194.665     -201.008      132.174      -55.916      129.674
## 7573      164.155     -162.944      391.622      -42.728      137.159
## 7574      150.596      -79.356      307.651      -39.215       68.426
##      timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 7569      -82.554      115.220      -91.407       62.922      -57.069
## 7570      -75.011      103.305     -130.215       59.775      -61.197
## 7571     -115.231       86.509      -83.905      102.373      -66.416
## 7572      -82.989      166.003      -83.246       62.951      -69.512
## 7573      -80.171       92.551      -64.419       74.428      -38.794
## 7574      -77.927      202.200      -55.617       98.615      -71.984
##      timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 7569       96.283      -56.468       34.205      -50.403       51.369
## 7570       38.120      -66.638       50.105      -40.692       43.763
## 7571       83.454      -83.707       44.124      -51.628       69.929
## 7572      103.413     -101.464       36.152      -45.387       48.352
## 7573      108.688      -55.893       42.222      -76.631       68.336
## 7574       87.098      -48.440       52.198      -60.673       48.418
##      timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 7569       -63.965        82.665       -47.429        58.158     0
## 7570       -59.707        49.414       -53.970        68.303     0
## 7571       -97.153        36.745       -61.243        56.902     0
## 7572       -57.103        67.641       -53.729        65.176     0
## 7573       -83.284        56.476       -51.687        59.427     0
## 7574      -120.625        49.593       -47.656        70.005     0
## 'data.frame':	7574 obs. of  20 variables:
##  $ year                    : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
##  $ songtitle               : chr  "This Is the House That Doubt Built" "Sticks & Bricks" "All I Want" "It's Complicated" ...
##  $ artistname              : chr  "A Day to Remember" "A Day to Remember" "A Day to Remember" "A Day to Remember" ...
##  $ songID                  : chr  "SOBGGAB12C5664F054" "SOPAQHU1315CD47F31" "SOOIZOU1376E7C6386" "SODRYWD1315CD49DBE" ...
##  $ artistID                : chr  "AROBSHL1187B9AFB01" "AROBSHL1187B9AFB01" "AROBSHL1187B9AFB01" "AROBSHL1187B9AFB01" ...
##  $ timesignature           : int  3 4 4 4 4 4 4 4 4 4 ...
##  $ timesignature_confidence: num  0.853 1 1 1 0.788 1 0.968 0.861 0.622 0.938 ...
##  $ loudness                : num  -4.26 -4.05 -3.57 -3.81 -4.71 ...
##  $ tempo                   : num  91.5 140 160.5 97.5 140.1 ...
##  $ tempo_confidence        : num  0.953 0.921 0.489 0.794 0.286 0.347 0.273 0.83 0.018 0.929 ...
##  $ key                     : int  11 10 2 1 6 4 10 5 9 11 ...
##  $ key_confidence          : num  0.453 0.469 0.209 0.632 0.483 0.627 0.715 0.423 0.751 0.602 ...
##  $ energy                  : num  0.967 0.985 0.99 0.939 0.988 ...
##  $ pitch                   : num  0.024 0.025 0.026 0.013 0.063 0.038 0.026 0.033 0.027 0.004 ...
##  $ timbre_0_min            : num  0.002 0 0.003 0 0 ...
##  $ timbre_0_max            : num  57.3 57.4 57.4 57.8 56.9 ...
##  $ timbre_1_min            : num  -6.5 -37.4 -17.2 -32.1 -223.9 ...
##  $ timbre_1_max            : num  171 171 171 221 171 ...
##  $ timbre_2_min            : num  -81.7 -149.6 -72.9 -138.6 -147.2 ...
##  $ timbre_2_max            : num  95.1 180.3 157.9 173.4 166 ...
## NULL
## 'data.frame':	7574 obs. of  21 variables:
##  $ timbre_2_min : num  -81.7 -149.6 -72.9 -138.6 -147.2 ...
##  $ timbre_2_max : num  95.1 180.3 157.9 173.4 166 ...
##  $ timbre_3_min : num  -285 -380.1 -204 -73.5 -128.1 ...
##  $ timbre_3_max : num  259 384 251 373 389 ...
##  $ timbre_4_min : num  -40.4 -48.7 -66 -55.6 -43.9 ...
##  $ timbre_4_max : num  73.6 100.4 152.1 119.2 99.3 ...
##  $ timbre_5_min : num  -104.7 -87.3 -98.7 -77.5 -96.1 ...
##  $ timbre_5_max : num  183.1 42.8 141.4 141.2 38.3 ...
##  $ timbre_6_min : num  -88.8 -86.9 -88.9 -70.8 -110.8 ...
##  $ timbre_6_max : num  73.5 75.5 66.5 64.5 72.4 ...
##  $ timbre_7_min : num  -71.1 -65.8 -67.4 -63.7 -55.9 ...
##  $ timbre_7_max : num  82.5 106.9 80.6 96.7 110.3 ...
##  $ timbre_8_min : num  -52 -61.3 -59.8 -78.7 -56.5 ...
##  $ timbre_8_max : num  39.1 35.4 46 41.1 37.6 ...
##  $ timbre_9_min : num  -35.4 -81.9 -46.3 -49.2 -48.6 ...
##  $ timbre_9_max : num  71.6 74.6 59.9 95.4 67.6 ...
##  $ timbre_10_min: num  -126.4 -103.8 -108.3 -102.7 -52.8 ...
##  $ timbre_10_max: num  18.7 121.9 33.3 46.4 22.9 ...
##  $ timbre_11_min: num  -44.8 -38.9 -43.7 -59.4 -50.4 ...
##  $ timbre_11_max: num  26 22.5 25.7 37.1 32.8 ...
##  $ Top10        : int  0 0 0 0 0 0 0 0 0 1 ...
## NULL
```

```
## Warning in myprint_str_df(df): [list output truncated]
```

```r
# glb_trnobs_df <- read.delim("data/hygiene.txt", header=TRUE, fill=TRUE, sep="\t",
#                             fileEncoding='iso-8859-1')
# glb_trnobs_df <- read.table("data/hygiene.dat.labels", col.names=c("dirty"),
#                             na.strings="[none]")
# glb_trnobs_df$review <- readLines("data/hygiene.dat", n =-1)
# comment(glb_trnobs_df) <- "glb_trnobs_df"                                

# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
# glb_trnobs_df <- 
#     glb_trnobs_df %>% dplyr::filter(Year >= 1999)
                                
if (glb_is_separate_newobs_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newobs_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newobs_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
}         
```

```
##   year                          songtitle        artistname
## 1 2010 This Is the House That Doubt Built A Day to Remember
## 2 2010                    Sticks & Bricks A Day to Remember
## 3 2010                         All I Want A Day to Remember
## 4 2010                   It's Complicated A Day to Remember
## 5 2010                          2nd Sucks A Day to Remember
## 6 2010                Better Off This Way A Day to Remember
##               songID           artistID timesignature
## 1 SOBGGAB12C5664F054 AROBSHL1187B9AFB01             3
## 2 SOPAQHU1315CD47F31 AROBSHL1187B9AFB01             4
## 3 SOOIZOU1376E7C6386 AROBSHL1187B9AFB01             4
## 4 SODRYWD1315CD49DBE AROBSHL1187B9AFB01             4
## 5 SOICMQB1315CD46EE3 AROBSHL1187B9AFB01             4
## 6 SOCEYON1315CD4A23E AROBSHL1187B9AFB01             4
##   timesignature_confidence loudness   tempo tempo_confidence key
## 1                    0.853   -4.262  91.525            0.953  11
## 2                    1.000   -4.051 140.048            0.921  10
## 3                    1.000   -3.571 160.512            0.489   2
## 4                    1.000   -3.815  97.525            0.794   1
## 5                    0.788   -4.707 140.053            0.286   6
## 6                    1.000   -3.807 160.366            0.347   4
##   key_confidence    energy pitch timbre_0_min timbre_0_max timbre_1_min
## 1          0.453 0.9666556 0.024        0.002       57.342       -6.496
## 2          0.469 0.9847095 0.025        0.000       57.414      -37.351
## 3          0.209 0.9899004 0.026        0.003       57.422      -17.222
## 4          0.632 0.9392072 0.013        0.000       57.765      -32.083
## 5          0.483 0.9877376 0.063        0.000       56.872     -223.922
## 6          0.627 0.9799530 0.038        0.000       57.083      -40.408
##   timbre_1_max timbre_2_min timbre_2_max timbre_3_min timbre_3_max
## 1      171.093      -81.664       95.117     -285.049      259.426
## 2      171.130     -149.589      180.334     -380.143      384.166
## 3      171.060      -72.912      157.925     -203.984      251.258
## 4      220.895     -138.596      173.365      -73.490      373.492
## 5      171.130     -147.153      166.008     -128.082      389.419
## 6      174.002      -83.829      126.879     -100.119      173.607
##   timbre_4_min timbre_4_max timbre_5_min timbre_5_max timbre_6_min
## 1      -40.385       73.630     -104.683      183.089      -88.771
## 2      -48.662      100.414      -87.267       42.798      -86.895
## 3      -66.044      152.095      -98.673      141.365      -88.874
## 4      -55.607      119.158      -77.515      141.178      -70.790
## 5      -43.908       99.332      -96.147       38.303     -110.757
## 6      -33.789       66.904      -84.451       47.268      -71.219
##   timbre_6_max timbre_7_min timbre_7_max timbre_8_min timbre_8_max
## 1       73.549      -71.127       82.475      -52.025       39.116
## 2       75.455      -65.807      106.918      -61.320       35.378
## 3       66.504      -67.433       80.621      -59.773       45.979
## 4       64.540      -63.667       96.675      -78.660       41.088
## 5       72.391      -55.935      110.332      -56.450       37.555
## 6       71.239      -79.948       91.117      -54.378       53.808
##   timbre_9_min timbre_9_max timbre_10_min timbre_10_max timbre_11_min
## 1      -35.368       71.642      -126.440        18.658       -44.770
## 2      -81.928       74.574      -103.808       121.935       -38.892
## 3      -46.293       59.904      -108.313        33.300       -43.733
## 4      -49.194       95.440      -102.676        46.422       -59.439
## 5      -48.588       67.570       -52.796        22.888       -50.414
## 6      -33.183       54.657       -64.478        34.522       -40.922
##   timbre_11_max Top10
## 1        25.989     0
## 2        22.513     0
## 3        25.744     0
## 4        37.082     0
## 5        32.758     0
## 6        36.453     0
##     year                          songtitle        artistname
## 1   2010 This Is the House That Doubt Built A Day to Remember
## 66  2010                          King Zero     Drowning Pool
## 144 2010                         Do-Wah-Doo         Kate Nash
## 146 2010                  I've Got A Secret         Kate Nash
## 149 2010                           Later On         Kate Nash
## 172 2010                        Bad Romance         Lady Gaga
##                 songID           artistID timesignature
## 1   SOBGGAB12C5664F054 AROBSHL1187B9AFB01             3
## 66  SORDZHA1375842B93A ARVSQHB1187B992FF0             4
## 144 SOBSGPF12AB018A2EC ARUZIPI1187FB5454A             4
## 146 SOOTJPM1315CD4E37E ARUZIPI1187FB5454A             4
## 149 SORAMKD131634AA473 ARUZIPI1187FB5454A             4
## 172 SOWPBHM13740123DE4 ARX6TAQ11C8A415850             4
##     timesignature_confidence loudness   tempo tempo_confidence key
## 1                      0.853   -4.262  91.525            0.953  11
## 66                     0.507   -4.482 170.038            0.347   4
## 144                    1.000   -4.140  75.187            0.096   5
## 146                    0.838   -7.300 134.372            0.300   7
## 149                    0.928   -5.855 142.996            0.402   0
## 172                    0.766   -5.280 121.123            0.757   9
##     key_confidence    energy pitch timbre_0_min timbre_0_max timbre_1_min
## 1            0.453 0.9666556 0.024        0.002       57.342       -6.496
## 66           0.008 0.9789121 0.034        0.000       56.858      -32.765
## 144          0.276 0.8982172 0.040        3.548       57.736      -65.857
## 146          0.174 0.6976624 0.008       18.959       55.941     -123.354
## 149          1.000 0.8296503 0.034        4.724       56.585     -110.203
## 172          0.206 0.9646445 0.010       28.933       57.919      -52.930
##     timbre_1_max timbre_2_min timbre_2_max timbre_3_min timbre_3_max
## 1        171.093      -81.664       95.117     -285.049      259.426
## 66       236.966     -114.972      145.832     -291.344      249.504
## 144      209.740      -54.648      148.757     -110.410      235.860
## 146      150.717     -181.426      130.265     -144.576      351.420
## 149      143.525     -101.047      102.746     -215.517      126.843
## 172      188.946     -121.424      194.654     -319.608      124.892
##     timbre_4_min timbre_4_max timbre_5_min timbre_5_max timbre_6_min
## 1        -40.385       73.630     -104.683      183.089      -88.771
## 66       -64.450       61.272      -89.033      166.842      -54.385
## 144      -25.883      125.275      -71.218      220.054      -78.952
## 146      -51.180      124.562     -119.357      207.993     -106.233
## 149      -44.579      142.076      -75.550      266.427      -56.031
## 172      -88.386      119.030     -112.415      217.453      -82.697
##     timbre_6_max timbre_7_min timbre_7_max timbre_8_min timbre_8_max
## 1         73.549      -71.127       82.475      -52.025       39.116
## 66        60.711      -53.279       54.733      -52.828       38.089
## 144       68.543      -64.882      114.825      -75.785       23.124
## 146       86.776      -94.747      117.902      -70.564       38.605
## 149       62.192      -90.309       64.590      -88.586       41.525
## 172      101.543      -99.651       89.983      -89.644       41.225
##     timbre_9_min timbre_9_max timbre_10_min timbre_10_max timbre_11_min
## 1        -35.368       71.642      -126.440        18.658       -44.770
## 66       -33.359       80.422      -202.256       135.774       -48.136
## 144      -63.403       64.000       -57.716        80.260       -33.235
## 146      -76.984       58.775      -131.919        49.218       -35.680
## 149      -50.205       80.019       -66.911        38.834       -50.193
## 172      -54.354       76.439       -86.599        48.143       -47.715
##     timbre_11_max Top10
## 1          25.989     0
## 66         46.067     0
## 144        33.499     0
## 146        54.745     0
## 149        36.077     0
## 172        56.982     1
##     year            songtitle      artistname             songID
## 368 2010       Mighty to Save Various Artists SOZHGUW137375147FE
## 369 2010 All Because of Jesus Various Artists SOQNGML13DC8B9A8A5
## 370 2010            Your Name Various Artists SOXGDEA1375A1AD2F8
## 371 2010            You Reign Various Artists SOEHNSK13DC4A18344
## 372 2010     New Song We Sing Various Artists SOQOCRO13DC469AD34
## 373 2010              BedRock     Young Money SOPQHEH1374011CFB8
##               artistID timesignature timesignature_confidence loudness
## 368 ARAGWS81187FB3F768             4                    0.465  -11.768
## 369 ARAGWS81187FB3F768             4                    0.851   -6.896
## 370 ARAGWS81187FB3F768             4                    0.937   -7.695
## 371 ARAGWS81187FB3F768             4                    0.581  -13.761
## 372 ARAGWS81187FB3F768             4                    0.869  -12.884
## 373 ARFNQBX122BCFCC40F             4                    0.230   -5.860
##       tempo tempo_confidence key key_confidence    energy pitch
## 368 147.962            0.455   6          0.715 0.7076861 0.004
## 369 132.014            0.897   5          0.593 0.8218041 0.002
## 370  79.990            0.345   3          0.512 0.5038695 0.003
## 371  75.019            0.171   2          0.689 0.3286537 0.002
## 372 120.074            0.710   9          0.664 0.3213989 0.002
## 373 148.328            0.266   8          0.458 0.7864918 0.005
##     timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 368       13.320       51.565     -112.753      190.046     -109.409
## 369        0.000       55.832     -253.356      171.130     -113.093
## 370        0.008       57.291     -137.997      182.508     -108.564
## 371        0.000       52.977     -120.113      179.066      -99.847
## 372        0.000       50.539     -160.382      186.927     -135.012
## 373       33.782       56.072      -40.255      309.536     -131.923
##     timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 368      118.560     -144.309      125.791      -54.146      132.050
## 369      108.598     -152.949       94.648      -50.066      142.013
## 370      100.557     -230.832       74.961      -40.792       94.952
## 371      143.332     -209.714      131.918      -37.509      132.303
## 372      196.199     -182.354      229.050      -33.858      128.405
## 373      126.088     -159.948      169.983      -54.751      148.642
##     timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 368      -88.464       77.963      -69.467       69.589      -58.550
## 369      -80.187       42.780      -53.975       70.978     -113.356
## 370      -95.304      157.062      -84.841       73.270      -92.767
## 371      -86.539      176.159      -85.282       96.528     -123.525
## 372      -87.455      158.989     -135.819       69.098      -94.179
## 373     -111.185      125.831     -111.483       72.581      -73.553
##     timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 368       86.250      -72.253       61.809      -55.720       66.168
## 369      122.253      -50.909       61.278      -65.765       61.128
## 370       91.915      -78.383       29.942      -62.093       47.771
## 371      100.910     -100.195       50.555      -64.052       60.195
## 372       99.038      -53.112       42.895      -64.875       99.467
## 373       88.488      -70.974       51.550      -51.106       85.348
##     timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 368       -63.432        44.960       -61.604        48.130     0
## 369       -54.461        32.120       -46.549        32.486     0
## 370       -49.670        51.155       -44.724        41.997     0
## 371       -83.234        51.323       -59.022        63.993     0
## 372      -111.697        49.754       -39.289        69.057     0
## 373       -84.959        74.872       -57.014        51.651     1
## 'data.frame':	373 obs. of  39 variables:
##  $ year                    : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
##  $ songtitle               : chr  "This Is the House That Doubt Built" "Sticks & Bricks" "All I Want" "It's Complicated" ...
##  $ artistname              : chr  "A Day to Remember" "A Day to Remember" "A Day to Remember" "A Day to Remember" ...
##  $ songID                  : chr  "SOBGGAB12C5664F054" "SOPAQHU1315CD47F31" "SOOIZOU1376E7C6386" "SODRYWD1315CD49DBE" ...
##  $ artistID                : chr  "AROBSHL1187B9AFB01" "AROBSHL1187B9AFB01" "AROBSHL1187B9AFB01" "AROBSHL1187B9AFB01" ...
##  $ timesignature           : int  3 4 4 4 4 4 4 4 4 4 ...
##  $ timesignature_confidence: num  0.853 1 1 1 0.788 1 0.968 0.861 0.622 0.938 ...
##  $ loudness                : num  -4.26 -4.05 -3.57 -3.81 -4.71 ...
##  $ tempo                   : num  91.5 140 160.5 97.5 140.1 ...
##  $ tempo_confidence        : num  0.953 0.921 0.489 0.794 0.286 0.347 0.273 0.83 0.018 0.929 ...
##  $ key                     : int  11 10 2 1 6 4 10 5 9 11 ...
##  $ key_confidence          : num  0.453 0.469 0.209 0.632 0.483 0.627 0.715 0.423 0.751 0.602 ...
##  $ energy                  : num  0.967 0.985 0.99 0.939 0.988 ...
##  $ pitch                   : num  0.024 0.025 0.026 0.013 0.063 0.038 0.026 0.033 0.027 0.004 ...
##  $ timbre_0_min            : num  0.002 0 0.003 0 0 ...
##  $ timbre_0_max            : num  57.3 57.4 57.4 57.8 56.9 ...
##  $ timbre_1_min            : num  -6.5 -37.4 -17.2 -32.1 -223.9 ...
##  $ timbre_1_max            : num  171 171 171 221 171 ...
##  $ timbre_2_min            : num  -81.7 -149.6 -72.9 -138.6 -147.2 ...
##  $ timbre_2_max            : num  95.1 180.3 157.9 173.4 166 ...
##  $ timbre_3_min            : num  -285 -380.1 -204 -73.5 -128.1 ...
##  $ timbre_3_max            : num  259 384 251 373 389 ...
##  $ timbre_4_min            : num  -40.4 -48.7 -66 -55.6 -43.9 ...
##  $ timbre_4_max            : num  73.6 100.4 152.1 119.2 99.3 ...
##  $ timbre_5_min            : num  -104.7 -87.3 -98.7 -77.5 -96.1 ...
##  $ timbre_5_max            : num  183.1 42.8 141.4 141.2 38.3 ...
##  $ timbre_6_min            : num  -88.8 -86.9 -88.9 -70.8 -110.8 ...
##  $ timbre_6_max            : num  73.5 75.5 66.5 64.5 72.4 ...
##  $ timbre_7_min            : num  -71.1 -65.8 -67.4 -63.7 -55.9 ...
##  $ timbre_7_max            : num  82.5 106.9 80.6 96.7 110.3 ...
##  $ timbre_8_min            : num  -52 -61.3 -59.8 -78.7 -56.5 ...
##  $ timbre_8_max            : num  39.1 35.4 46 41.1 37.6 ...
##  $ timbre_9_min            : num  -35.4 -81.9 -46.3 -49.2 -48.6 ...
##  $ timbre_9_max            : num  71.6 74.6 59.9 95.4 67.6 ...
##  $ timbre_10_min           : num  -126.4 -103.8 -108.3 -102.7 -52.8 ...
##  $ timbre_10_max           : num  18.7 121.9 33.3 46.4 22.9 ...
##  $ timbre_11_min           : num  -44.8 -38.9 -43.7 -59.4 -50.4 ...
##  $ timbre_11_max           : num  26 22.5 25.7 37.1 32.8 ...
##  $ Top10                   : int  0 0 0 0 0 0 0 0 0 1 ...
##  - attr(*, "comment")= chr "glb_newobs_df"
##     year              songtitle artistname             songID
## 374 2009    The Awkward Goodbye    Athlete SOUALGK12AB017FC37
## 375 2009           Rubik's Cube    Athlete SOGPIQC12AB0182B15
## 376 2009       Superhuman Touch    Athlete SOBNYZN13774E81F76
## 377 2009            The Getaway    Athlete SOHFEOA1366EE931DD
## 378 2009        Black Swan Song    Athlete SOXXSMX12AB017F7B3
## 379 2009 Don't Hold Your Breath    Athlete SOOEDWA12AB017FC13
##               artistID timesignature timesignature_confidence loudness
## 374 ARDW3YJ1187FB4CCE5             3                    0.732   -6.320
## 375 ARDW3YJ1187FB4CCE5             3                    0.906   -9.541
## 376 ARDW3YJ1187FB4CCE5             4                    0.987   -4.842
## 377 ARDW3YJ1187FB4CCE5             4                    0.822   -5.272
## 378 ARDW3YJ1187FB4CCE5             4                    0.983   -6.233
## 379 ARDW3YJ1187FB4CCE5             4                    1.000   -6.793
##       tempo tempo_confidence key key_confidence    energy pitch
## 374  89.614            0.652   1          0.773 0.5985294 0.004
## 375 117.742            0.542   0          0.722 0.3633990 0.006
## 376 119.018            0.838   6          0.106 0.7601515 0.003
## 377  71.479            0.613   4          0.781 0.7550336 0.014
## 378  77.492            0.740   8          0.552 0.5236583 0.008
## 379  81.859            0.821   9          0.218 0.6546091 0.012
##     timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 374        0.000       57.831      -62.306      285.818      -81.802
## 375        0.739       57.059     -220.205      241.091      -96.833
## 376        0.000       57.815     -189.660      187.282     -139.053
## 377        0.000       58.330     -113.885      171.130      -71.640
## 378        0.000       57.643     -160.579      216.778      -79.456
## 379        0.000       57.389     -103.691      227.209     -155.016
##     timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 374      211.084     -217.025      203.151      -55.874       97.646
## 375      214.510     -201.889      124.200      -52.389      131.859
## 376      134.508     -116.316       94.698      -55.617       79.292
## 377      194.788     -276.297      146.268      -59.374      121.707
## 378      114.093     -183.559      108.719      -31.922      169.734
## 379      174.758     -386.464      185.756      -69.700      103.106
##     timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 374      -62.492       82.169      -82.129       59.197     -109.384
## 375      -73.875       73.628      -63.496       70.133      -90.092
## 376      -73.474       41.025      -41.489       62.759      -69.311
## 377      -71.135       39.607      -77.786       94.525      -69.088
## 378      -73.004      233.930      -76.026       58.016      -78.803
## 379      -75.267      184.309      -62.755       45.283      -61.878
##     timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 374       70.975      -71.776       58.432      -53.816       88.571
## 375      112.879      -64.470       58.086      -76.937       74.441
## 376       90.400      -52.459       40.679      -50.408       58.811
## 377       93.373      -55.811       78.963      -51.504       70.455
## 378      100.766      -61.392       50.309      -62.994       96.837
## 379       89.443      -70.718       50.515      -54.980       80.278
##     timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 374       -89.816        38.026       -52.075        52.827     0
## 375       -88.244        42.209       -66.812        40.749     0
## 376       -78.239        35.264       -54.200        46.490     0
## 377       -74.928        30.839       -51.377        27.768     0
## 378       -90.397        60.549       -52.122        48.059     0
## 379       -70.317        54.084       -43.651        43.261     0
##      year                           songtitle     artistname
## 2726 2005                       American Guns    Transplants
## 3640 2003 Don't Let the World Get in Your Way   The Jayhawks
## 5012 1999                           Even Odds      Jeff Beck
## 5465 1998                     Phone Call Skit  Queen Latifah
## 7226 1991                          Ellen West Throwing Muses
## 7323 1990                                Epic  Faith No More
##                  songID           artistID timesignature
## 2726 SOASWBQ13134394F22 ARF4WYR1187FB3838D             4
## 3640 SORBFQV1313438E57F AR4TV7J1187FB400C9             4
## 5012 SOFNLJE12AB01801EB ARHI20L1187FB4C997             4
## 5465 SOFLVRM1315CD46AA2 ARELPXQ1187FB384FD             3
## 7226 SOQJGUA1315CD40B11 ARWV4ZQ1187FB4A44F             4
## 7323 SODICGU1373F1D2CE3 AR0AJG11187FB51EFC             4
##      timesignature_confidence loudness   tempo tempo_confidence key
## 2726                    1.000   -5.635  93.369            1.000   5
## 3640                    0.947   -5.965  83.460            0.792   4
## 5012                    0.364   -7.764  91.902            0.975   0
## 5465                    0.750  -16.109  90.736            0.854   1
## 7226                    0.988  -11.877 121.219            0.824   4
## 7323                    1.000   -7.368  87.187            0.914   4
##      key_confidence    energy pitch timbre_0_min timbre_0_max timbre_1_min
## 2726          0.991 0.9742504 0.038        1.530       56.356      -33.893
## 3640          0.400 0.3074298 0.005        0.991       58.787     -208.433
## 5012          0.370 0.8056250 0.014        0.951       55.084        1.988
## 5465          0.254 0.3204597 0.021        8.616       45.455     -270.967
## 7226          0.458 0.4965909 0.007        0.000       50.967     -108.305
## 7323          0.809 0.8953855 0.012        0.000       55.115     -130.600
##      timbre_1_max timbre_2_min timbre_2_max timbre_3_min timbre_3_max
## 2726      174.678      -61.115      174.919     -129.347      117.431
## 3640      185.035     -159.470       88.993     -175.936       92.497
## 5012      217.262     -144.212      101.045     -254.606      285.427
## 5465      198.236     -162.788      121.035     -125.385      172.790
## 7226      171.130     -175.175       98.137     -261.209      287.941
## 7323      230.373     -183.707      110.840     -239.676      143.577
##      timbre_4_min timbre_4_max timbre_5_min timbre_5_max timbre_6_min
## 2726      -30.230       88.339      -73.830      144.070      -62.843
## 3640      -76.347      131.807      -72.179       97.136      -97.176
## 5012      -47.381      178.111      -96.249      191.226     -121.316
## 5465      -90.194      130.643      -78.386       75.061      -74.612
## 7226      -68.878       95.331     -111.167      186.491      -86.676
## 7323      -64.810      131.815     -117.699      162.767      -88.636
##      timbre_6_max timbre_7_min timbre_7_max timbre_8_min timbre_8_max
## 2726       39.098      -62.324       71.844      -71.110       44.070
## 3640       51.147      -77.978       86.337      -45.057       52.735
## 5012       61.408     -111.579       99.271      -37.422       58.007
## 5465       93.092      -66.750       88.664      -76.198       37.251
## 7226       64.456      -60.156      116.897      -75.238       47.249
## 7323       71.788      -64.151      146.595      -79.778       49.692
##      timbre_9_min timbre_9_max timbre_10_min timbre_10_max timbre_11_min
## 2726      -33.131       49.908       -95.315        33.230       -29.731
## 3640      -55.272       52.952       -66.075        31.141       -58.252
## 5012      -28.135       82.095      -113.360       119.897       -52.073
## 5465      -45.053       84.435       -75.624        65.293       -22.330
## 7226      -42.790       37.726      -113.553        62.800       -44.657
## 7323      -50.031       88.352      -122.997        81.541       -49.090
##      timbre_11_max Top10
## 2726        27.310     0
## 3640        45.012     0
## 5012        38.193     0
## 5465        43.326     0
## 7226        57.052     0
## 7323        74.035     1
##      year              songtitle artistname             songID
## 7569 1990        Red Hot & Ready        Y&T SOHVWXR12A6D4FC59C
## 7570 1990             She's Gone        Y&T SOSIEQB12A6D4FC59D
## 7571 1990             Let It Out        Y&T SOIGIQI12A6D4FC59E
## 7572 1990             Ten Lovers        Y&T SOLNEQO12A6D4FC59F
## 7573 1990 Goin' Off The Deep End        Y&T SOEMJEP12A58A7E7B5
## 7574 1990              Surrender        Y&T SOLBQSX12A6D4FC5A0
##                artistID timesignature timesignature_confidence loudness
## 7569 ARGQANQ11F50C4769E             4                    1.000  -10.574
## 7570 ARGQANQ11F50C4769E             4                    1.000  -10.197
## 7571 ARGQANQ11F50C4769E             4                    1.000  -12.392
## 7572 ARGQANQ11F50C4769E             4                    0.984  -10.304
## 7573 ARGQANQ11F50C4769E             4                    0.907   -9.295
## 7574 ARGQANQ11F50C4769E             4                    0.987   -9.762
##        tempo tempo_confidence key key_confidence    energy pitch
## 7569 137.872            1.000   2          0.006 0.9736458 0.030
## 7570  93.140            0.859   5          0.889 0.9429917 0.016
## 7571  79.858            0.196   9          0.149 0.8124216 0.012
## 7572  91.760            0.592   2          0.077 0.7368709 0.016
## 7573 110.907            0.838   9          0.621 0.9900532 0.061
## 7574 139.650            0.781   6          0.193 0.9448227 0.027
##      timbre_0_min timbre_0_max timbre_1_min timbre_1_max timbre_2_min
## 7569            0       51.847      -18.801      203.289     -141.762
## 7570            0       53.462      -22.878      202.424      -77.467
## 7571            0       51.354      -91.916      202.639      -76.736
## 7572            0       53.358      -10.087      202.877      -48.768
## 7573            0       52.928      -15.289      175.845     -119.044
## 7574            0       52.989      -32.907      195.601     -146.973
##      timbre_2_max timbre_3_min timbre_3_max timbre_4_min timbre_4_max
## 7569      157.204     -120.035      364.391      -53.240       99.468
## 7570      174.663     -170.802      201.677      -45.807      109.383
## 7571      165.643     -186.653      166.200      -63.367      132.495
## 7572      194.665     -201.008      132.174      -55.916      129.674
## 7573      164.155     -162.944      391.622      -42.728      137.159
## 7574      150.596      -79.356      307.651      -39.215       68.426
##      timbre_5_min timbre_5_max timbre_6_min timbre_6_max timbre_7_min
## 7569      -82.554      115.220      -91.407       62.922      -57.069
## 7570      -75.011      103.305     -130.215       59.775      -61.197
## 7571     -115.231       86.509      -83.905      102.373      -66.416
## 7572      -82.989      166.003      -83.246       62.951      -69.512
## 7573      -80.171       92.551      -64.419       74.428      -38.794
## 7574      -77.927      202.200      -55.617       98.615      -71.984
##      timbre_7_max timbre_8_min timbre_8_max timbre_9_min timbre_9_max
## 7569       96.283      -56.468       34.205      -50.403       51.369
## 7570       38.120      -66.638       50.105      -40.692       43.763
## 7571       83.454      -83.707       44.124      -51.628       69.929
## 7572      103.413     -101.464       36.152      -45.387       48.352
## 7573      108.688      -55.893       42.222      -76.631       68.336
## 7574       87.098      -48.440       52.198      -60.673       48.418
##      timbre_10_min timbre_10_max timbre_11_min timbre_11_max Top10
## 7569       -63.965        82.665       -47.429        58.158     0
## 7570       -59.707        49.414       -53.970        68.303     0
## 7571       -97.153        36.745       -61.243        56.902     0
## 7572       -57.103        67.641       -53.729        65.176     0
## 7573       -83.284        56.476       -51.687        59.427     0
## 7574      -120.625        49.593       -47.656        70.005     0
## 'data.frame':	7201 obs. of  39 variables:
##  $ year                    : int  2009 2009 2009 2009 2009 2009 2009 2009 2009 2009 ...
##  $ songtitle               : chr  "The Awkward Goodbye" "Rubik's Cube" "Superhuman Touch" "The Getaway" ...
##  $ artistname              : chr  "Athlete" "Athlete" "Athlete" "Athlete" ...
##  $ songID                  : chr  "SOUALGK12AB017FC37" "SOGPIQC12AB0182B15" "SOBNYZN13774E81F76" "SOHFEOA1366EE931DD" ...
##  $ artistID                : chr  "ARDW3YJ1187FB4CCE5" "ARDW3YJ1187FB4CCE5" "ARDW3YJ1187FB4CCE5" "ARDW3YJ1187FB4CCE5" ...
##  $ timesignature           : int  3 3 4 4 4 4 4 4 4 4 ...
##  $ timesignature_confidence: num  0.732 0.906 0.987 0.822 0.983 1 0.821 0.997 0.816 1 ...
##  $ loudness                : num  -6.32 -9.54 -4.84 -5.27 -6.23 ...
##  $ tempo                   : num  89.6 117.7 119 71.5 77.5 ...
##  $ tempo_confidence        : num  0.652 0.542 0.838 0.613 0.74 0.821 0.912 0.609 0.786 0.27 ...
##  $ key                     : int  1 0 6 4 8 9 6 9 0 9 ...
##  $ key_confidence          : num  0.773 0.722 0.106 0.781 0.552 0.218 0.275 0.333 0.634 0.578 ...
##  $ energy                  : num  0.599 0.363 0.76 0.755 0.524 ...
##  $ pitch                   : num  0.004 0.006 0.003 0.014 0.008 0.012 0.002 0.003 0.001 0.006 ...
##  $ timbre_0_min            : num  0 0.739 0 0 0 ...
##  $ timbre_0_max            : num  57.8 57.1 57.8 58.3 57.6 ...
##  $ timbre_1_min            : num  -62.3 -220.2 -189.7 -113.9 -160.6 ...
##  $ timbre_1_max            : num  286 241 187 171 217 ...
##  $ timbre_2_min            : num  -81.8 -96.8 -139.1 -71.6 -79.5 ...
##  $ timbre_2_max            : num  211 215 135 195 114 ...
##  $ timbre_3_min            : num  -217 -202 -116 -276 -184 ...
##  $ timbre_3_max            : num  203.2 124.2 94.7 146.3 108.7 ...
##  $ timbre_4_min            : num  -55.9 -52.4 -55.6 -59.4 -31.9 ...
##  $ timbre_4_max            : num  97.6 131.9 79.3 121.7 169.7 ...
##  $ timbre_5_min            : num  -62.5 -73.9 -73.5 -71.1 -73 ...
##  $ timbre_5_max            : num  82.2 73.6 41 39.6 233.9 ...
##  $ timbre_6_min            : num  -82.1 -63.5 -41.5 -77.8 -76 ...
##  $ timbre_6_max            : num  59.2 70.1 62.8 94.5 58 ...
##  $ timbre_7_min            : num  -109.4 -90.1 -69.3 -69.1 -78.8 ...
##  $ timbre_7_max            : num  71 112.9 90.4 93.4 100.8 ...
##  $ timbre_8_min            : num  -71.8 -64.5 -52.5 -55.8 -61.4 ...
##  $ timbre_8_max            : num  58.4 58.1 40.7 79 50.3 ...
##  $ timbre_9_min            : num  -53.8 -76.9 -50.4 -51.5 -63 ...
##  $ timbre_9_max            : num  88.6 74.4 58.8 70.5 96.8 ...
##  $ timbre_10_min           : num  -89.8 -88.2 -78.2 -74.9 -90.4 ...
##  $ timbre_10_max           : num  38 42.2 35.3 30.8 60.5 ...
##  $ timbre_11_min           : num  -52.1 -66.8 -54.2 -51.4 -52.1 ...
##  $ timbre_11_max           : num  52.8 40.7 46.5 27.8 48.1 ...
##  $ Top10                   : int  0 0 0 0 0 0 0 0 0 0 ...
```

```r
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Combine trnent & newobs into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"

# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Train"))
    glb_newobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Test"))    
    glb_id_var <- ".rownames"
}
```

```
## Warning: using .rownames as identifiers for observations
```

```r
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 8.268 9.832   1.564
## 2 inspect.data          2          0 9.832    NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Loading required package: reshape2
```

![](songs_template2_files/figure-html/inspect.data-1.png) 

```
##       Top10.0 Top10.1
## Test      314      59
## Train    6141    1060
##         Top10.0   Top10.1
## Test  0.8418231 0.1581769
## Train 0.8527982 0.1472018
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##            timesignature timesignature_confidence                    tempo 
##                       10                      157                       13 
##         tempo_confidence                      key           key_confidence 
##                       85                      803                      422 
##                    pitch             timbre_0_min                    Top10 
##                      108                     3114                     6455 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##  songtitle artistname     songID   artistID  .rownames 
##          0          0          0          0          0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##   Top10 Top10.fctr   .n
## 1     0          N 6455
## 2     1          Y 1119
```

![](songs_template2_files/figure-html/inspect.data-2.png) 

```
##       Top10.fctr.N Top10.fctr.Y
## Test           314           59
## Train         6141         1060
##       Top10.fctr.N Top10.fctr.Y
## Test     0.8418231    0.1581769
## Train    0.8527982    0.1472018
```

```r
# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: timesignature"
```

![](songs_template2_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: timesignature_confidence"
```

![](songs_template2_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: loudness"
```

![](songs_template2_files/figure-html/inspect.data-5.png) 

```
## [1] "feat: tempo"
```

![](songs_template2_files/figure-html/inspect.data-6.png) 

```
## [1] "feat: tempo_confidence"
```

![](songs_template2_files/figure-html/inspect.data-7.png) 

```
## [1] "feat: key"
```

![](songs_template2_files/figure-html/inspect.data-8.png) 

```
## [1] "feat: key_confidence"
```

![](songs_template2_files/figure-html/inspect.data-9.png) 

```
## [1] "feat: energy"
```

![](songs_template2_files/figure-html/inspect.data-10.png) 

```
## [1] "feat: pitch"
```

![](songs_template2_files/figure-html/inspect.data-11.png) 

```
## [1] "feat: timbre_0_min"
```

![](songs_template2_files/figure-html/inspect.data-12.png) 

```
## [1] "feat: timbre_0_max"
```

![](songs_template2_files/figure-html/inspect.data-13.png) 

```
## [1] "feat: timbre_1_min"
```

![](songs_template2_files/figure-html/inspect.data-14.png) 

```
## [1] "feat: timbre_1_max"
```

![](songs_template2_files/figure-html/inspect.data-15.png) 

```
## [1] "feat: timbre_2_min"
```

![](songs_template2_files/figure-html/inspect.data-16.png) 

```
## [1] "feat: timbre_2_max"
```

![](songs_template2_files/figure-html/inspect.data-17.png) 

```
## [1] "feat: timbre_3_min"
```

![](songs_template2_files/figure-html/inspect.data-18.png) 

```
## [1] "feat: timbre_3_max"
```

![](songs_template2_files/figure-html/inspect.data-19.png) 

```
## [1] "feat: timbre_4_min"
```

![](songs_template2_files/figure-html/inspect.data-20.png) 

```
## [1] "feat: timbre_4_max"
```

![](songs_template2_files/figure-html/inspect.data-21.png) 

```
## [1] "feat: timbre_5_min"
```

![](songs_template2_files/figure-html/inspect.data-22.png) 

```
## [1] "feat: timbre_5_max"
```

![](songs_template2_files/figure-html/inspect.data-23.png) 

```
## [1] "feat: timbre_6_min"
```

![](songs_template2_files/figure-html/inspect.data-24.png) 

```
## [1] "feat: timbre_6_max"
```

![](songs_template2_files/figure-html/inspect.data-25.png) 

```
## [1] "feat: timbre_7_min"
```

![](songs_template2_files/figure-html/inspect.data-26.png) 

```
## [1] "feat: timbre_7_max"
```

![](songs_template2_files/figure-html/inspect.data-27.png) 

```
## [1] "feat: timbre_8_min"
```

![](songs_template2_files/figure-html/inspect.data-28.png) 

```
## [1] "feat: timbre_8_max"
```

![](songs_template2_files/figure-html/inspect.data-29.png) 

```
## [1] "feat: timbre_9_min"
```

![](songs_template2_files/figure-html/inspect.data-30.png) 

```
## [1] "feat: timbre_9_max"
```

![](songs_template2_files/figure-html/inspect.data-31.png) 

```
## [1] "feat: timbre_10_min"
```

![](songs_template2_files/figure-html/inspect.data-32.png) 

```
## [1] "feat: timbre_10_max"
```

![](songs_template2_files/figure-html/inspect.data-33.png) 

```
## [1] "feat: timbre_11_min"
```

![](songs_template2_files/figure-html/inspect.data-34.png) 

```
## [1] "feat: timbre_11_max"
```

![](songs_template2_files/figure-html/inspect.data-35.png) 

```
## [1] "feat: .rnorm"
```

![](songs_template2_files/figure-html/inspect.data-36.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5) +
#         geom_vline(xintercept=84))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0  9.832 27.612   17.78
## 3   scrub.data          2          1 27.612     NA      NA
```

### Step `2.1: scrub data`

```r
mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##            timesignature timesignature_confidence                    tempo 
##                       10                      157                       13 
##         tempo_confidence                      key           key_confidence 
##                       85                      803                      422 
##                    pitch             timbre_0_min                    Top10 
##                      108                     3114                     6455 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##  songtitle artistname     songID   artistID  .rownames 
##          0          0          0          0          0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

# sel_obs <- function(Popular=NULL, 
#                     NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
#         Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
#         Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
#         perl=FALSE) {
sel_obs <- function(vars_lst) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 27.612 31.328   3.716
## 4 transform.data          2          2 31.328     NA      NA
```

```r
### Mapping dictionary
#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}

### Forced Assignments
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}

### Derivations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (new_feat in glb_derive_vars) {
    print(sprintf("Creating new feature: %s...", new_feat))
    args_lst <- NULL 
    for (arg in glb_derive_lst[[new_feat]]$args) 
        args_lst[[arg]] <- glb_allobs_df[, arg]
    glb_allobs_df[, new_feat] <- do.call(glb_derive_lst[[new_feat]]$mapfn, args_lst)
}
```

## Step `2.2: transform data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 4   transform.data          2          2 31.328 31.388    0.06
## 5 extract.features          3          0 31.389     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 31.395  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

#stop(here"); sav_allobs_df <- glb_allobs_df #; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    for (sfx in c("", ".POSIX"))
        glb_exclude_vars_as_features <- 
            union(glb_exclude_vars_as_features, 
                    paste(glb_date_vars, sfx, sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last2 <- as.numeric(merge(z-lag(z, -2), b, all=TRUE)); last2[is.na(last2)] <- 0
        glb_allobs_df[, paste0(feat, ".last2.log")] <- log(1 + last2)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last2.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last2.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}
rm(last1, last10, last100)
```

```
## Warning in rm(last1, last10, last100): object 'last1' not found
```

```
## Warning in rm(last1, last10, last100): object 'last10' not found
```

```
## Warning in rm(last1, last10, last100): object 'last100' not found
```

```r
#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 31.395 31.409
## 2 extract.features_factorize.str.vars          2          0 31.409     NA
##   elapsed
## 1   0.014
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##    songtitle   artistname       songID     artistID         .src 
##  "songtitle" "artistname"     "songID"   "artistID"       ".src" 
##    .rownames 
##  ".rownames"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               c(glb_exclude_vars_as_features, glb_txt_vars))) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- 
            relevel(factor(glb_allobs_df[, var]),
                    names(which.max(table(glb_allobs_df[, var], useNA = "ifany"))))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}

if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(rex_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(rex_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }

#     match_lst <- gregexpr("\\bok(?!ay)", txt_vctr[746], ignore.case = FALSE, perl=TRUE); print(match_lst)
    dsp_pattern <- function(rex_str, ignore.case=TRUE, print.all=TRUE) {
        match_lst <- gregexpr(rex_str, txt_vctr, ignore.case = ignore.case, perl=TRUE)
        match_lst <- regmatches(txt_vctr, match_lst)
        match_df <- data.frame(matches=sapply(match_lst, 
                                              function (elems) paste(elems, collapse="#")))
        match_df <- subset(match_df, matches != "")
        if (print.all)
            print(match_df)
        return(match_df)
    }
    
    dsp_matches <- function(rex_str, ix) {
        print(match_pos <- gregexpr(rex_str, txt_vctr[ix], perl=TRUE))
        print(str_sub(txt_vctr[ix], (match_pos[[1]] / 100) *  99 +   0, 
                                    (match_pos[[1]] / 100) * 100 + 100))        
    }

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], 
                        glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#chk.equal( 1, 100)
#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[163, "rex_str"])
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining OK in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "(?<!(BO|HO|LO))OK(?!(E\\!|ED|IE|IN|S ))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "Ok(?!(a\\.|ay|in|ra|um))", ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "(?<!( b| B| c| C| g| G| j| M| p| P| w| W| r| Z|\\(b|ar|bo|Bo|co|Co|Ew|gk|go|ho|ig|jo|kb|ke|Ke|ki|lo|Lo|mo|mt|no|No|po|ra|ro|sm|Sm|Sp|to|To))ok(?!(ay|bo|e |e\\)|e,|e\\.|eb|ed|el|en|er|es|ey|i |ie|in|it|ka|ke|ki|ly|on|oy|ra|st|u |uc|uy|yl|yo))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))
    }    
    # txt_vctr <- glb_txt_lst[[glb_txt_vars[1]]]
    # print(chk_pattern_freq(rex_str <- "(?<!( b| c| C| p|\\(b|bo|co|lo|Lo|Sp|to|To))ok(?!(ay|e |e\\)|e,|e\\.|ed|el|en|es|ey|ie|in|on|ra))", ignore.case=FALSE))
    # print(chk_pattern_freq(rex_str <- "ok(?!(ay|el|on|ra))", ignore.case=FALSE))
    # dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
    # dsp_matches(rex_str, ix=8)
    # substr(txt_vctr[86], 5613, 5620)
    # substr(glb_allobs_df[301, "review"], 550, 650)

#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "([[:upper:]]\\.( *)){2,}", ignore.case=FALSE))
        
        # Check for names
        print(subset(chk_pattern_freq(rex_str <- "(([[:upper:]]+)\\.( *)){1}",
                                      ignore.case=FALSE),
                     .n > 1))
        # dsp_pattern(rex_str="(OK\\.( *)){1}", ignore.case=FALSE)
        # dsp_matches(rex_str="(OK\\.( *)){1}", ix=557)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)(\\B)", ix=461)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)", ix=461)        
        #print(str_sub(txt_vctr[676], 10100, 10200))
        #print(str_sub(txt_vctr[74], 1, -1))        
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 31.409 31.428
## 3                extract.features_end          3          0 31.428     NA
##   elapsed
## 2   0.019
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 31.409 31.428
## 1                extract.features_bgn          1          0 31.395 31.409
##   elapsed duration
## 2   0.019    0.019
## 1   0.014    0.014
## [1] "Total Elapsed Time: 31.428 secs"
```

![](songs_template2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](songs_template2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 31.389 32.797   1.408
## 6     cluster.data          4          0 32.798     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 32.798 33.095   0.298
## 7 manage.missing.data          4          1 33.096     NA      NA
```

```r
# If mice crashes with error: Error in get(as.character(FUN), mode = "function", envir = envir) : object 'State' of mode 'function' was not found
#   consider excluding 'State' as a feature

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##            timesignature timesignature_confidence                    tempo 
##                       10                      157                       13 
##         tempo_confidence                      key           key_confidence 
##                       85                      803                      422 
##                    pitch             timbre_0_min                    Top10 
##                      108                     3114                     6455 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##  songtitle artistname     songID   artistID  .rownames 
##          0          0          0          0          0
```

```r
# glb_allobs_df <- na.omit(glb_allobs_df)

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col],
                                                       inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    
    # complete(mice()) changes attributes of factors even though values don't change
    for (col in ret_vars) {
        if (inherits(out_impent_df[, col], "factor")) {
            if (identical(as.numeric(out_impent_df[, col]), 
                          as.numeric(inp_impent_df[, col])))
                ret_vars <- setdiff(ret_vars, col)
        }
    }
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
    
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##            timesignature timesignature_confidence                    tempo 
##                       10                      157                       13 
##         tempo_confidence                      key           key_confidence 
##                       85                      803                      422 
##                    pitch             timbre_0_min                    Top10 
##                      108                     3114                     6455 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##  songtitle artistname     songID   artistID  .rownames 
##          0          0          0          0          0
```

## Step `4.1: manage missing data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Last call for data modifications 
#stop(here") # sav_allobs_df <- glb_allobs_df
# glb_allobs_df[(glb_allobs_df$PropR == 0.75) & (glb_allobs_df$State == "Hawaii"), "PropR.fctr"] <- "N"

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 7 manage.missing.data          4          1 33.096 33.219   0.124
## 8     select.features          5          0 33.220     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                                id        cor.y
## Top10                                       Top10  1.000000000
## timbre_6_min                         timbre_6_min -0.216378268
## timbre_4_max                         timbre_4_max  0.197034666
## timbre_11_min                       timbre_11_min -0.193049686
## timbre_8_max                         timbre_8_max  0.162722687
## timbre_7_min                         timbre_7_min -0.155645339
## timbre_0_max                         timbre_0_max -0.152171355
## timbre_9_max                         timbre_9_max  0.145756048
## timbre_11_max                       timbre_11_max  0.143718363
## timbre_2_min                         timbre_2_min -0.136085935
## pitch                                       pitch -0.135723510
## timbre_5_min                         timbre_5_min -0.123590779
## timbre_6_max                         timbre_6_max  0.123432076
## energy                                     energy -0.120897670
## year                                         year -0.106988968
## timbre_1_max                         timbre_1_max  0.106338252
## timbre_10_max                       timbre_10_max  0.104441775
## timbre_8_min                         timbre_8_min -0.101131084
## timbre_2_max                         timbre_2_max  0.096545551
## timbre_9_min                         timbre_9_min -0.088167503
## loudness                                 loudness -0.086546031
## tempo_confidence                 tempo_confidence  0.086315020
## timbre_5_max                         timbre_5_max  0.076923330
## timbre_0_min                         timbre_0_min  0.068825164
## timesignature_confidence timesignature_confidence  0.063023674
## timesignature                       timesignature  0.046630910
## timbre_7_max                         timbre_7_max  0.045966630
## timbre_3_max                         timbre_3_max -0.026744876
## timbre_3_min                         timbre_3_min -0.026442066
## key                                           key  0.025326063
## .rnorm                                     .rnorm  0.011773171
## key_confidence                     key_confidence  0.010021403
## timbre_4_min                         timbre_4_min -0.006010064
## timbre_10_min                       timbre_10_min  0.005257590
## timbre_1_min                         timbre_1_min  0.003659089
## tempo                                       tempo -0.003577709
##                          exclude.as.feat   cor.y.abs
## Top10                                  1 1.000000000
## timbre_6_min                           0 0.216378268
## timbre_4_max                           0 0.197034666
## timbre_11_min                          0 0.193049686
## timbre_8_max                           0 0.162722687
## timbre_7_min                           0 0.155645339
## timbre_0_max                           0 0.152171355
## timbre_9_max                           0 0.145756048
## timbre_11_max                          0 0.143718363
## timbre_2_min                           0 0.136085935
## pitch                                  0 0.135723510
## timbre_5_min                           0 0.123590779
## timbre_6_max                           0 0.123432076
## energy                                 0 0.120897670
## year                                   1 0.106988968
## timbre_1_max                           0 0.106338252
## timbre_10_max                          0 0.104441775
## timbre_8_min                           0 0.101131084
## timbre_2_max                           0 0.096545551
## timbre_9_min                           0 0.088167503
## loudness                               0 0.086546031
## tempo_confidence                       0 0.086315020
## timbre_5_max                           0 0.076923330
## timbre_0_min                           0 0.068825164
## timesignature_confidence               0 0.063023674
## timesignature                          0 0.046630910
## timbre_7_max                           0 0.045966630
## timbre_3_max                           0 0.026744876
## timbre_3_min                           0 0.026442066
## key                                    0 0.025326063
## .rnorm                                 0 0.011773171
## key_confidence                         0 0.010021403
## timbre_4_min                           0 0.006010064
## timbre_10_min                          0 0.005257590
## timbre_1_min                           0 0.003659089
## tempo                                  0 0.003577709
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## [1] "cor(loudness, timbre_0_max)=0.9059"
## [1] "cor(Top10.fctr, loudness)=-0.0865"
## [1] "cor(Top10.fctr, timbre_0_max)=-0.1522"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified loudness as highly correlated with timbre_0_max
```

```
##                          id        cor.y exclude.as.feat   cor.y.abs
## 35                    Top10  1.000000000               1 1.000000000
## 21             timbre_4_max  0.197034666               0 0.197034666
## 29             timbre_8_max  0.162722687               0 0.162722687
## 31             timbre_9_max  0.145756048               0 0.145756048
## 15            timbre_11_max  0.143718363               0 0.143718363
## 25             timbre_6_max  0.123432076               0 0.123432076
## 11             timbre_1_max  0.106338252               0 0.106338252
## 13            timbre_10_max  0.104441775               0 0.104441775
## 17             timbre_2_max  0.096545551               0 0.096545551
## 8          tempo_confidence  0.086315020               0 0.086315020
## 23             timbre_5_max  0.076923330               0 0.076923330
## 10             timbre_0_min  0.068825164               0 0.068825164
## 34 timesignature_confidence  0.063023674               0 0.063023674
## 33            timesignature  0.046630910               0 0.046630910
## 27             timbre_7_max  0.045966630               0 0.045966630
## 3                       key  0.025326063               0 0.025326063
## 1                    .rnorm  0.011773171               0 0.011773171
## 4            key_confidence  0.010021403               0 0.010021403
## 14            timbre_10_min  0.005257590               0 0.005257590
## 12             timbre_1_min  0.003659089               0 0.003659089
## 7                     tempo -0.003577709               0 0.003577709
## 22             timbre_4_min -0.006010064               0 0.006010064
## 20             timbre_3_min -0.026442066               0 0.026442066
## 19             timbre_3_max -0.026744876               0 0.026744876
## 5                  loudness -0.086546031               0 0.086546031
## 32             timbre_9_min -0.088167503               0 0.088167503
## 30             timbre_8_min -0.101131084               0 0.101131084
## 36                     year -0.106988968               1 0.106988968
## 2                    energy -0.120897670               0 0.120897670
## 24             timbre_5_min -0.123590779               0 0.123590779
## 6                     pitch -0.135723510               0 0.135723510
## 18             timbre_2_min -0.136085935               0 0.136085935
## 9              timbre_0_max -0.152171355               0 0.152171355
## 28             timbre_7_min -0.155645339               0 0.155645339
## 16            timbre_11_min -0.193049686               0 0.193049686
## 26             timbre_6_min -0.216378268               0 0.216378268
##      cor.high.X  freqRatio percentUnique zeroVar   nzv myNearZV
## 35         <NA>   5.793396    0.02777392   FALSE FALSE    FALSE
## 21         <NA>  80.333333   93.51478961   FALSE FALSE    FALSE
## 29         <NA>   1.250000   92.32051104   FALSE FALSE    FALSE
## 31         <NA>   1.000000   95.04235523   FALSE FALSE    FALSE
## 15         <NA>   1.333333   91.86224136   FALSE FALSE    FALSE
## 25         <NA>   1.000000   94.11192890   FALSE FALSE    FALSE
## 11         <NA> 126.571429   84.90487432   FALSE FALSE    FALSE
## 13         <NA>   1.000000   94.55631162   FALSE FALSE    FALSE
## 17         <NA>   1.000000   97.51423413   FALSE FALSE    FALSE
## 8          <NA>   6.554217   13.67865574   FALSE FALSE    FALSE
## 23         <NA>   1.000000   97.97250382   FALSE FALSE    FALSE
## 10         <NA>  21.099291   37.77253159   FALSE FALSE    FALSE
## 34         <NA>  18.928571   11.22066380   FALSE FALSE    FALSE
## 33         <NA>  13.717622    0.08332176   FALSE FALSE    FALSE
## 27         <NA>   1.000000   96.05610332   FALSE FALSE    FALSE
## 3          <NA>   1.052696    0.16664352   FALSE FALSE    FALSE
## 1          <NA>   1.000000  100.00000000   FALSE FALSE    FALSE
## 4          <NA>   4.093750   13.59533398   FALSE FALSE    FALSE
## 14         <NA>   1.000000   95.82002500   FALSE FALSE    FALSE
## 12         <NA>   1.000000   98.23635606   FALSE FALSE    FALSE
## 7          <NA>   2.600000   92.90376337   FALSE FALSE    FALSE
## 22         <NA>   1.333333   94.72295515   FALSE FALSE    FALSE
## 20         <NA>   1.000000   98.09748646   FALSE FALSE    FALSE
## 19         <NA>   1.000000   98.55575614   FALSE FALSE    FALSE
## 5  timbre_0_max   1.000000   76.30884599   FALSE FALSE    FALSE
## 32         <NA>   1.000000   94.08415498   FALSE FALSE    FALSE
## 30         <NA>   5.500000   93.05651993   FALSE FALSE    FALSE
## 36         <NA>   1.200772    0.27773920   FALSE FALSE    FALSE
## 2          <NA>   1.000000   99.63893904   FALSE FALSE    FALSE
## 24         <NA>   2.250000   95.25065963   FALSE FALSE    FALSE
## 6          <NA>   1.007987    1.33314817   FALSE FALSE    FALSE
## 18         <NA>   1.666667   97.62532982   FALSE FALSE    FALSE
## 9          <NA>   1.166667   72.68434940   FALSE FALSE    FALSE
## 28         <NA>   1.000000   95.88945980   FALSE FALSE    FALSE
## 16         <NA>   1.000000   92.04277184   FALSE FALSE    FALSE
## 26         <NA>   1.000000   95.16733787   FALSE FALSE    FALSE
##    is.cor.y.abs.low
## 35            FALSE
## 21            FALSE
## 29            FALSE
## 31            FALSE
## 15            FALSE
## 25            FALSE
## 11            FALSE
## 13            FALSE
## 17            FALSE
## 8             FALSE
## 23            FALSE
## 10            FALSE
## 34            FALSE
## 33            FALSE
## 27            FALSE
## 3             FALSE
## 1             FALSE
## 4              TRUE
## 14             TRUE
## 12             TRUE
## 7              TRUE
## 22             TRUE
## 20            FALSE
## 19            FALSE
## 5             FALSE
## 32            FALSE
## 30            FALSE
## 36            FALSE
## 2             FALSE
## 24            FALSE
## 6             FALSE
## 18            FALSE
## 9             FALSE
## 28            FALSE
## 16            FALSE
## 26            FALSE
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

```
## Warning: Removed 28 rows containing missing values (geom_point).
```

![](songs_template2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
## named integer(0)
## [1] "numeric data w/ 0s in : "
##            timesignature timesignature_confidence                    tempo 
##                       10                      157                       13 
##         tempo_confidence                      key           key_confidence 
##                       85                      803                      422 
##                    pitch             timbre_0_min                    Top10 
##                      108                     3114                     6455 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
##  songtitle artistname     songID   artistID  .rownames 
##          0          0          0          0          0
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 33.220 35.356   2.136
## 9 partition.data.training          6          0 35.356     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## [1] "Newdata contains non-NA data for Top10.fctr; setting OOB to Newdata"
```

```r
if (!is.null(glb_max_fitobs) && (nrow(glb_fitobs_df) > glb_max_fitobs)) {
    warning("glb_fitobs_df restricted to glb_max_fitobs: ", 
            format(glb_max_fitobs, big.mark=","))
    org_fitobs_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitobs_df[split <- sample.split(org_fitobs_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitobs), ]
    org_fitobs_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newobs_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 36 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                    id exclude.as.feat rsp_var
## Top10.fctr Top10.fctr            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                    id cor.y exclude.as.feat cor.y.abs cor.high.X freqRatio
## 35              Top10     1            TRUE         1       <NA>  5.793396
## Top10.fctr Top10.fctr    NA            TRUE        NA       <NA>        NA
##            percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 35            0.02777392   FALSE FALSE    FALSE            FALSE
## Top10.fctr            NA      NA    NA       NA               NA
##            interaction.feat rsp_var_raw rsp_var
## 35                       NA        TRUE      NA
## Top10.fctr               NA          NA    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 7574   44
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 7201   43
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 7201   43
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 373  43
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 373  43
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)
```

```
## Warning in rm(split): object 'split' not found
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 35.356 35.689   0.333
## 10              fit.models          7          0 35.689     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[glb_feats_df$id == max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_feats_df$id == glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a higher correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl(model_id="Baseline", 
                         model_method="mybaseln_classfr",
                        indep_vars_vctr=glb_Baseline_mdl_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##         N         Y 
## 0.8527982 0.1472018 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8527982 0.1472018
## 2 0.8527982 0.1472018
## 3 0.8527982 0.1472018
## 4 0.8527982 0.1472018
## 5 0.8527982 0.1472018
## 6 0.8527982 0.1472018
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.MFO.myMFO_classfr.N
## 1          N                                   6141
## 2          Y                                   1060
##          Prediction
## Reference    N    Y
##         N 6141    0
##         Y 1060    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.527982e-01   0.000000e+00   8.444027e-01   8.609070e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   5.081939e-01  4.439170e-232 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8527982 0.1472018
## 2 0.8527982 0.1472018
## 3 0.8527982 0.1472018
## 4 0.8527982 0.1472018
## 5 0.8527982 0.1472018
## 6 0.8527982 0.1472018
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.MFO.myMFO_classfr.N
## 1          N                                    314
## 2          Y                                     59
##          Prediction
## Reference   N   Y
##         N 314   0
##         Y  59   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.418231e-01   0.000000e+00   8.007587e-01   8.773551e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   5.346786e-01   4.320720e-14 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.318                 0.003         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8527982
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8444027              0.860907             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8418231
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8007587             0.8773551             0
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Random.myrandom_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      table      numeric  
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in Random.Classifier$prob"
```

![](songs_template2_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.2566275
## 2        0.1 0.2566275
## 3        0.2 0.1483932
## 4        0.3 0.1483932
## 5        0.4 0.1483932
## 6        0.5 0.1483932
## 7        0.6 0.1483932
## 8        0.7 0.1483932
## 9        0.8 0.1483932
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](songs_template2_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.Random.myrandom_classfr.Y
## 1          N                                         6141
## 2          Y                                         1060
##          Prediction
## Reference    N    Y
##         N    0 6141
##         Y    0 1060
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.1472018      0.0000000      0.1390930      0.1555973      0.8527982 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.0000000 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](songs_template2_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.2731481
## 2        0.1 0.2731481
## 3        0.2 0.1864407
## 4        0.3 0.1864407
## 5        0.4 0.1864407
## 6        0.5 0.1864407
## 7        0.6 0.1864407
## 8        0.7 0.1864407
## 9        0.8 0.1864407
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](songs_template2_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.Random.myrandom_classfr.Y
## 1          N                                          314
## 2          Y                                           59
##          Prediction
## Reference   N   Y
##         N   0 314
##         Y   0  59
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.581769e-01   0.000000e+00   1.226449e-01   1.992413e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   7.995559e-70 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.235                 0.002   0.5008601
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.1       0.2566275        0.1472018
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.139093             0.1555973             0   0.5167872
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.2731481        0.1581769
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.1226449             0.1992413             0
```

```r
# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: timbre_6_min, timbre_4_max"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.00108 on full training set
```

```
## Loading required package: rpart.plot
```

![](songs_template2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 7201 
## 
##            CP nsplit rel error
## 1 0.001078167      0         1
## 
## Node number 1: 7201 observations
##   predicted class=N  expected loss=0.1472018  P(node) =1
##     class counts:  6141  1060
##    probabilities: 0.853 0.147 
## 
## n= 7201 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 7201 1060 N (0.8527982 0.1472018) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1          N                                      6141
## 2          Y                                      1060
##          Prediction
## Reference    N    Y
##         N 6141    0
##         Y 1060    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.527982e-01   0.000000e+00   8.444027e-01   8.609070e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   5.081939e-01  4.439170e-232 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1          N                                       314
## 2          Y                                        59
##          Prediction
## Reference   N   Y
##         N 314   0
##         Y  59   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.418231e-01   0.000000e+00   8.007587e-01   8.773551e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   5.346786e-01   4.320720e-14 
##               model_id model_method                      feats
## 1 Max.cor.Y.cv.0.rpart        rpart timbre_6_min, timbre_4_max
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.843                 0.124
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1         0.5                    0.5               0        0.8527982
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8444027              0.860907             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8418231
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8007587             0.8773551             0
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: timbre_6_min, timbre_4_max"
## Fitting cp = 0 on full training set
```

```
## Warning: labs do not fit even at cex 0.15, there may be some overplotting
```

![](songs_template2_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 7201 
## 
##              CP nsplit rel error
## 1  0.0010781671      0 1.0000000
## 2  0.0009433962     35 0.9509434
## 3  0.0008086253     50 0.9358491
## 4  0.0006738544     59 0.9283019
## 5  0.0006289308     74 0.9103774
## 6  0.0005390836     78 0.9075472
## 7  0.0004716981     99 0.8924528
## 8  0.0003144654    137 0.8650943
## 9  0.0002358491    143 0.8632075
## 10 0.0001347709    155 0.8603774
## 11 0.0000000000    162 0.8594340
## 
## Variable importance
## timbre_6_min timbre_4_max 
##           55           45 
## 
## Node number 1: 7201 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.1472018  P(node) =1
##     class counts:  6141  1060
##    probabilities: 0.853 0.147 
##   left son=2 (4382 obs) right son=3 (2819 obs)
##   Primary splits:
##       timbre_6_min < -86.184   to the right, improve=63.85999, (0 missing)
##       timbre_4_max < 90.6385   to the left,  improve=57.81832, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 111.935   to the left,  agree=0.68, adj=0.182, (0 split)
## 
## Node number 2: 4382 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.09379279  P(node) =0.6085266
##     class counts:  3971   411
##    probabilities: 0.906 0.094 
##   left son=4 (1731 obs) right son=5 (2651 obs)
##   Primary splits:
##       timbre_4_max < 87.318    to the left,  improve=17.36542, (0 missing)
##       timbre_6_min < -64.667   to the right, improve=13.80606, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -60.748   to the right, agree=0.658, adj=0.135, (0 split)
## 
## Node number 3: 2819 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.2302235  P(node) =0.3914734
##     class counts:  2170   649
##    probabilities: 0.770 0.230 
##   left son=6 (898 obs) right son=7 (1921 obs)
##   Primary splits:
##       timbre_4_max < 109.353   to the left,  improve=18.257260, (0 missing)
##       timbre_6_min < -110.867  to the right, improve= 9.658228, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -86.831   to the right, agree=0.683, adj=0.003, (0 split)
## 
## Node number 4: 1731 observations
##   predicted class=N  expected loss=0.03870595  P(node) =0.2403833
##     class counts:  1664    67
##    probabilities: 0.961 0.039 
## 
## Node number 5: 2651 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.1297624  P(node) =0.3681433
##     class counts:  2307   344
##    probabilities: 0.870 0.130 
##   left son=10 (701 obs) right son=11 (1950 obs)
##   Primary splits:
##       timbre_6_min < -64.667   to the right, improve=8.922924, (0 missing)
##       timbre_4_max < 189.9985  to the left,  improve=1.376515, (0 missing)
## 
## Node number 6: 898 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.1469933  P(node) =0.1247049
##     class counts:   766   132
##    probabilities: 0.853 0.147 
##   left son=12 (366 obs) right son=13 (532 obs)
##   Primary splits:
##       timbre_4_max < 91.4875   to the left,  improve=3.259933, (0 missing)
##       timbre_6_min < -96.5325  to the right, improve=2.574484, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -86.4725  to the right, agree=0.598, adj=0.014, (0 split)
## 
## Node number 7: 1921 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.2691307  P(node) =0.2667685
##     class counts:  1404   517
##    probabilities: 0.731 0.269 
##   left son=14 (1522 obs) right son=15 (399 obs)
##   Primary splits:
##       timbre_6_min < -112.424  to the right, improve=5.549425, (0 missing)
##       timbre_4_max < 135.6385  to the left,  improve=2.789776, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 109.422   to the right, agree=0.793, adj=0.003, (0 split)
## 
## Node number 10: 701 observations
##   predicted class=N  expected loss=0.06134094  P(node) =0.09734759
##     class counts:   658    43
##    probabilities: 0.939 0.061 
## 
## Node number 11: 1950 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.154359  P(node) =0.2707957
##     class counts:  1649   301
##    probabilities: 0.846 0.154 
##   left son=22 (1928 obs) right son=23 (22 obs)
##   Primary splits:
##       timbre_4_max < 189.9985  to the left,  improve=1.949059, (0 missing)
##       timbre_6_min < -85.6325  to the left,  improve=1.388632, (0 missing)
## 
## Node number 12: 366 observations,    complexity param=0.0001347709
##   predicted class=N  expected loss=0.09562842  P(node) =0.05082627
##     class counts:   331    35
##    probabilities: 0.904 0.096 
##   left son=24 (92 obs) right son=25 (274 obs)
##   Primary splits:
##       timbre_4_max < 67.6285   to the left,  improve=0.9761157, (0 missing)
##       timbre_6_min < -86.478   to the left,  improve=0.7113886, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -119.2955 to the left,  agree=0.754, adj=0.022, (0 split)
## 
## Node number 13: 532 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.1823308  P(node) =0.07387863
##     class counts:   435    97
##    probabilities: 0.818 0.182 
##   left son=26 (522 obs) right son=27 (10 obs)
##   Primary splits:
##       timbre_4_max < 91.831    to the right, improve=3.555789, (0 missing)
##       timbre_6_min < -110.0435 to the right, improve=2.459997, (0 missing)
## 
## Node number 14: 1522 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.2496715  P(node) =0.2113595
##     class counts:  1142   380
##    probabilities: 0.750 0.250 
##   left son=28 (1514 obs) right son=29 (8 obs)
##   Primary splits:
##       timbre_4_max < 109.674   to the right, improve=2.265854, (0 missing)
##       timbre_6_min < -90.2195  to the left,  improve=1.591299, (0 missing)
## 
## Node number 15: 399 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3433584  P(node) =0.05540897
##     class counts:   262   137
##    probabilities: 0.657 0.343 
##   left son=30 (385 obs) right son=31 (14 obs)
##   Primary splits:
##       timbre_6_min < -112.758  to the left,  improve=1.50941, (0 missing)
##       timbre_4_max < 124.0415  to the left,  improve=1.36934, (0 missing)
## 
## Node number 22: 1928 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.151971  P(node) =0.2677406
##     class counts:  1635   293
##    probabilities: 0.848 0.152 
##   left son=44 (51 obs) right son=45 (1877 obs)
##   Primary splits:
##       timbre_6_min < -85.6325  to the left,  improve=1.332038, (0 missing)
##       timbre_4_max < 87.524    to the right, improve=1.075013, (0 missing)
## 
## Node number 23: 22 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.3636364  P(node) =0.003055131
##     class counts:    14     8
##    probabilities: 0.636 0.364 
##   left son=46 (7 obs) right son=47 (15 obs)
##   Primary splits:
##       timbre_4_max < 204.9105  to the right, improve=2.715152, (0 missing)
##       timbre_6_min < -76.618   to the right, improve=2.548485, (0 missing)
## 
## Node number 24: 92 observations
##   predicted class=N  expected loss=0.0326087  P(node) =0.012776
##     class counts:    89     3
##    probabilities: 0.967 0.033 
## 
## Node number 25: 274 observations,    complexity param=0.0001347709
##   predicted class=N  expected loss=0.1167883  P(node) =0.03805027
##     class counts:   242    32
##    probabilities: 0.883 0.117 
##   left son=50 (253 obs) right son=51 (21 obs)
##   Primary splits:
##       timbre_4_max < 71.4975   to the right, improve=2.132926, (0 missing)
##       timbre_6_min < -86.704   to the left,  improve=1.396616, (0 missing)
## 
## Node number 26: 522 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.1743295  P(node) =0.07248993
##     class counts:   431    91
##    probabilities: 0.826 0.174 
##   left son=52 (434 obs) right son=53 (88 obs)
##   Primary splits:
##       timbre_6_min < -110.0435 to the right, improve=2.049576, (0 missing)
##       timbre_4_max < 108.1615  to the right, improve=1.330437, (0 missing)
## 
## Node number 27: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.001388696
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 28: 1514 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.2476882  P(node) =0.2102486
##     class counts:  1139   375
##    probabilities: 0.752 0.248 
##   left son=56 (895 obs) right son=57 (619 obs)
##   Primary splits:
##       timbre_4_max < 139.04    to the left,  improve=2.337676, (0 missing)
##       timbre_6_min < -90.081   to the left,  improve=1.600316, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -110.69   to the right, agree=0.598, adj=0.018, (0 split)
## 
## Node number 29: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001110957
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 30: 385 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3350649  P(node) =0.0534648
##     class counts:   256   129
##    probabilities: 0.665 0.335 
##   left son=60 (179 obs) right son=61 (206 obs)
##   Primary splits:
##       timbre_4_max < 135.5785  to the left,  improve=1.6826600, (0 missing)
##       timbre_6_min < -121.5495 to the right, improve=0.6027706, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -117.412  to the right, agree=0.595, adj=0.128, (0 split)
## 
## Node number 31: 14 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001944174
##     class counts:     6     8
##    probabilities: 0.429 0.571 
## 
## Node number 44: 51 observations
##   predicted class=N  expected loss=0.03921569  P(node) =0.00708235
##     class counts:    49     2
##    probabilities: 0.961 0.039 
## 
## Node number 45: 1877 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.1550346  P(node) =0.2606582
##     class counts:  1586   291
##    probabilities: 0.845 0.155 
##   left son=90 (1847 obs) right son=91 (30 obs)
##   Primary splits:
##       timbre_6_min < -85.353   to the right, improve=3.658963, (0 missing)
##       timbre_4_max < 87.524    to the right, improve=1.051434, (0 missing)
## 
## Node number 46: 7 observations
##   predicted class=N  expected loss=0  P(node) =0.0009720872
##     class counts:     7     0
##    probabilities: 1.000 0.000 
## 
## Node number 47: 15 observations
##   predicted class=Y  expected loss=0.4666667  P(node) =0.002083044
##     class counts:     7     8
##    probabilities: 0.467 0.533 
## 
## Node number 50: 253 observations,    complexity param=0.0001347709
##   predicted class=N  expected loss=0.09881423  P(node) =0.03513401
##     class counts:   228    25
##    probabilities: 0.901 0.099 
##   left son=100 (242 obs) right son=101 (11 obs)
##   Primary splits:
##       timbre_6_min < -86.704   to the left,  improve=1.6130080, (0 missing)
##       timbre_4_max < 90.1555   to the right, improve=0.6646697, (0 missing)
## 
## Node number 51: 21 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.002916262
##     class counts:    14     7
##    probabilities: 0.667 0.333 
## 
## Node number 52: 434 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.1543779  P(node) =0.06026941
##     class counts:   367    67
##    probabilities: 0.846 0.154 
##   left son=104 (98 obs) right son=105 (336 obs)
##   Primary splits:
##       timbre_4_max < 105.5845  to the right, improve=1.7419350, (0 missing)
##       timbre_6_min < -96.5325  to the right, improve=0.6145578, (0 missing)
## 
## Node number 53: 88 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.2727273  P(node) =0.01222052
##     class counts:    64    24
##    probabilities: 0.727 0.273 
##   left son=106 (9 obs) right son=107 (79 obs)
##   Primary splits:
##       timbre_4_max < 107.9985  to the right, improve=1.4913690, (0 missing)
##       timbre_6_min < -110.881  to the left,  improve=0.9090909, (0 missing)
## 
## Node number 56: 895 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.224581  P(node) =0.1242883
##     class counts:   694   201
##    probabilities: 0.775 0.225 
##   left son=112 (31 obs) right son=113 (864 obs)
##   Primary splits:
##       timbre_4_max < 137.6295  to the right, improve=1.6454820, (0 missing)
##       timbre_6_min < -110.683  to the left,  improve=0.8060126, (0 missing)
## 
## Node number 57: 619 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.2810985  P(node) =0.08596028
##     class counts:   445   174
##    probabilities: 0.719 0.281 
##   left son=114 (525 obs) right son=115 (94 obs)
##   Primary splits:
##       timbre_6_min < -90.091   to the left,  improve=2.806318, (0 missing)
##       timbre_4_max < 224.1235  to the right, improve=1.280816, (0 missing)
## 
## Node number 60: 179 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.2849162  P(node) =0.02485766
##     class counts:   128    51
##    probabilities: 0.715 0.285 
##   left son=120 (9 obs) right son=121 (170 obs)
##   Primary splits:
##       timbre_4_max < 134.8965  to the right, improve=1.538547, (0 missing)
##       timbre_6_min < -117.385  to the left,  improve=1.167730, (0 missing)
## 
## Node number 61: 206 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3786408  P(node) =0.02860714
##     class counts:   128    78
##    probabilities: 0.621 0.379 
##   left son=122 (198 obs) right son=123 (8 obs)
##   Primary splits:
##       timbre_4_max < 137.1985  to the right, improve=2.295675, (0 missing)
##       timbre_6_min < -120.7485 to the right, improve=1.017479, (0 missing)
## 
## Node number 90: 1847 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.1510558  P(node) =0.2564922
##     class counts:  1568   279
##    probabilities: 0.849 0.151 
##   left son=180 (1175 obs) right son=181 (672 obs)
##   Primary splits:
##       timbre_6_min < -78.8265  to the right, improve=0.7298795, (0 missing)
##       timbre_4_max < 141.9505  to the left,  improve=0.4721727, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 87.4895   to the right, agree=0.638, adj=0.004, (0 split)
## 
## Node number 91: 30 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.4  P(node) =0.004166088
##     class counts:    18    12
##    probabilities: 0.600 0.400 
##   left son=182 (19 obs) right son=183 (11 obs)
##   Primary splits:
##       timbre_6_min < -85.494   to the left,  improve=1.94067, (0 missing)
##       timbre_4_max < 128.539   to the right, improve=0.90000, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 126.7505  to the right, agree=0.667, adj=0.091, (0 split)
## 
## Node number 100: 242 observations,    complexity param=0.0001347709
##   predicted class=N  expected loss=0.08677686  P(node) =0.03360644
##     class counts:   221    21
##    probabilities: 0.913 0.087 
##   left son=200 (138 obs) right son=201 (104 obs)
##   Primary splits:
##       timbre_6_min < -96.956   to the right, improve=0.8347476, (0 missing)
##       timbre_4_max < 90.1555   to the right, improve=0.5157493, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 90.576    to the left,  agree=0.595, adj=0.058, (0 split)
## 
## Node number 101: 11 observations
##   predicted class=N  expected loss=0.3636364  P(node) =0.001527566
##     class counts:     7     4
##    probabilities: 0.636 0.364 
## 
## Node number 104: 98 observations
##   predicted class=N  expected loss=0.07142857  P(node) =0.01360922
##     class counts:    91     7
##    probabilities: 0.929 0.071 
## 
## Node number 105: 336 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.1785714  P(node) =0.04666019
##     class counts:   276    60
##    probabilities: 0.821 0.179 
##   left son=210 (250 obs) right son=211 (86 obs)
##   Primary splits:
##       timbre_4_max < 102.463   to the left,  improve=0.9952425, (0 missing)
##       timbre_6_min < -95.538   to the right, improve=0.7739358, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -109.6235 to the right, agree=0.747, adj=0.012, (0 split)
## 
## Node number 106: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.001249826
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 107: 79 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.3037975  P(node) =0.0109707
##     class counts:    55    24
##    probabilities: 0.696 0.304 
##   left son=214 (53 obs) right son=215 (26 obs)
##   Primary splits:
##       timbre_4_max < 104.347   to the left,  improve=2.983759, (0 missing)
##       timbre_6_min < -110.881  to the left,  improve=1.100261, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -110.6185 to the left,  agree=0.684, adj=0.038, (0 split)
## 
## Node number 112: 31 observations
##   predicted class=N  expected loss=0.06451613  P(node) =0.004304958
##     class counts:    29     2
##    probabilities: 0.935 0.065 
## 
## Node number 113: 864 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.2303241  P(node) =0.1199833
##     class counts:   665   199
##    probabilities: 0.770 0.230 
##   left son=226 (371 obs) right son=227 (493 obs)
##   Primary splits:
##       timbre_4_max < 121.5665  to the left,  improve=1.0317510, (0 missing)
##       timbre_6_min < -102.7925 to the right, improve=0.6989913, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -110.7055 to the left,  agree=0.574, adj=0.008, (0 split)
## 
## Node number 114: 525 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.2609524  P(node) =0.07290654
##     class counts:   388   137
##    probabilities: 0.739 0.261 
##   left son=228 (25 obs) right son=229 (500 obs)
##   Primary splits:
##       timbre_6_min < -91.2335  to the right, improve=2.563048, (0 missing)
##       timbre_4_max < 224.1235  to the right, improve=1.106398, (0 missing)
## 
## Node number 115: 94 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.393617  P(node) =0.01305374
##     class counts:    57    37
##    probabilities: 0.606 0.394 
##   left son=230 (62 obs) right son=231 (32 obs)
##   Primary splits:
##       timbre_6_min < -89.0365  to the right, improve=2.767502, (0 missing)
##       timbre_4_max < 141.9595  to the right, improve=2.221178, (0 missing)
## 
## Node number 120: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.001249826
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 121: 170 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3  P(node) =0.02360783
##     class counts:   119    51
##    probabilities: 0.700 0.300 
##   left son=242 (97 obs) right son=243 (73 obs)
##   Primary splits:
##       timbre_6_min < -117.385  to the left,  improve=1.2488910, (0 missing)
##       timbre_4_max < 124.0415  to the left,  improve=0.6783251, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 110.5345  to the right, agree=0.594, adj=0.055, (0 split)
## 
## Node number 122: 198 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3636364  P(node) =0.02749618
##     class counts:   126    72
##    probabilities: 0.636 0.364 
##   left son=244 (23 obs) right son=245 (175 obs)
##   Primary splits:
##       timbre_6_min < -134.7425 to the left,  improve=1.1131340, (0 missing)
##       timbre_4_max < 180.7105  to the right, improve=0.7053292, (0 missing)
## 
## Node number 123: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001110957
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## Node number 180: 1175 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.1404255  P(node) =0.1631718
##     class counts:  1010   165
##    probabilities: 0.860 0.140 
##   left son=360 (1118 obs) right son=361 (57 obs)
##   Primary splits:
##       timbre_6_min < -65.368   to the left,  improve=0.9203472, (0 missing)
##       timbre_4_max < 119.0535  to the left,  improve=0.5208527, (0 missing)
## 
## Node number 181: 672 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.1696429  P(node) =0.09332037
##     class counts:   558   114
##    probabilities: 0.830 0.170 
##   left son=362 (614 obs) right son=363 (58 obs)
##   Primary splits:
##       timbre_6_min < -79.48    to the left,  improve=1.9351540, (0 missing)
##       timbre_4_max < 93.8785   to the left,  improve=0.5562932, (0 missing)
## 
## Node number 182: 19 observations
##   predicted class=N  expected loss=0.2631579  P(node) =0.002638522
##     class counts:    14     5
##    probabilities: 0.737 0.263 
## 
## Node number 183: 11 observations
##   predicted class=Y  expected loss=0.3636364  P(node) =0.001527566
##     class counts:     4     7
##    probabilities: 0.364 0.636 
## 
## Node number 200: 138 observations
##   predicted class=N  expected loss=0.05072464  P(node) =0.019164
##     class counts:   131     7
##    probabilities: 0.949 0.051 
## 
## Node number 201: 104 observations,    complexity param=0.0001347709
##   predicted class=N  expected loss=0.1346154  P(node) =0.01444244
##     class counts:    90    14
##    probabilities: 0.865 0.135 
##   left son=402 (27 obs) right son=403 (77 obs)
##   Primary splits:
##       timbre_4_max < 88.1385   to the right, improve=1.3216780, (0 missing)
##       timbre_6_min < -103.526  to the left,  improve=0.4986264, (0 missing)
## 
## Node number 210: 250 observations
##   predicted class=N  expected loss=0.156  P(node) =0.0347174
##     class counts:   211    39
##    probabilities: 0.844 0.156 
## 
## Node number 211: 86 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.244186  P(node) =0.01194279
##     class counts:    65    21
##    probabilities: 0.756 0.244 
##   left son=422 (26 obs) right son=423 (60 obs)
##   Primary splits:
##       timbre_6_min < -98.182   to the left,  improve=2.0852120, (0 missing)
##       timbre_4_max < 102.639   to the right, improve=0.5181463, (0 missing)
## 
## Node number 214: 53 observations
##   predicted class=N  expected loss=0.2075472  P(node) =0.007360089
##     class counts:    42    11
##    probabilities: 0.792 0.208 
## 
## Node number 215: 26 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.5  P(node) =0.00361061
##     class counts:    13    13
##    probabilities: 0.500 0.500 
##   left son=430 (9 obs) right son=431 (17 obs)
##   Primary splits:
##       timbre_6_min < -116.569  to the left,  improve=2.1241830, (0 missing)
##       timbre_4_max < 107.254   to the left,  improve=0.3611111, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 105.1215  to the left,  agree=0.769, adj=0.333, (0 split)
## 
## Node number 226: 371 observations,    complexity param=0.0008086253
##   predicted class=N  expected loss=0.2021563  P(node) =0.05152062
##     class counts:   296    75
##    probabilities: 0.798 0.202 
##   left son=452 (129 obs) right son=453 (242 obs)
##   Primary splits:
##       timbre_4_max < 117.926   to the right, improve=1.9588230, (0 missing)
##       timbre_6_min < -110.6995 to the left,  improve=0.9265499, (0 missing)
## 
## Node number 227: 493 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.2515213  P(node) =0.06846271
##     class counts:   369   124
##    probabilities: 0.748 0.252 
##   left son=454 (391 obs) right son=455 (102 obs)
##   Primary splits:
##       timbre_4_max < 125.184   to the right, improve=1.3337150, (0 missing)
##       timbre_6_min < -89.5705  to the left,  improve=0.7310924, (0 missing)
## 
## Node number 228: 25 observations
##   predicted class=N  expected loss=0.04  P(node) =0.00347174
##     class counts:    24     1
##    probabilities: 0.960 0.040 
## 
## Node number 229: 500 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.272  P(node) =0.0694348
##     class counts:   364   136
##    probabilities: 0.728 0.272 
##   left son=458 (8 obs) right son=459 (492 obs)
##   Primary splits:
##       timbre_6_min < -111.831  to the left,  improve=1.202992, (0 missing)
##       timbre_4_max < 224.1235  to the right, improve=1.202992, (0 missing)
## 
## Node number 230: 62 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3064516  P(node) =0.008609915
##     class counts:    43    19
##    probabilities: 0.694 0.306 
##   left son=460 (55 obs) right son=461 (7 obs)
##   Primary splits:
##       timbre_4_max < 142.605   to the right, improve=2.624969, (0 missing)
##       timbre_6_min < -88.4605  to the left,  improve=2.275406, (0 missing)
## 
## Node number 231: 32 observations,    complexity param=0.001078167
##   predicted class=Y  expected loss=0.4375  P(node) =0.004443827
##     class counts:    14    18
##    probabilities: 0.438 0.562 
##   left son=462 (20 obs) right son=463 (12 obs)
##   Primary splits:
##       timbre_4_max < 162.371   to the left,  improve=1.3500000, (0 missing)
##       timbre_6_min < -89.69    to the left,  improve=0.3906926, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -90.062   to the right, agree=0.688, adj=0.167, (0 split)
## 
## Node number 242: 97 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.2474227  P(node) =0.01347035
##     class counts:    73    24
##    probabilities: 0.753 0.247 
##   left son=484 (75 obs) right son=485 (22 obs)
##   Primary splits:
##       timbre_6_min < -130.7685 to the right, improve=1.487348, (0 missing)
##       timbre_4_max < 124.1495  to the left,  improve=1.235154, (0 missing)
## 
## Node number 243: 73 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.369863  P(node) =0.01013748
##     class counts:    46    27
##    probabilities: 0.630 0.370 
##   left son=486 (25 obs) right son=487 (48 obs)
##   Primary splits:
##       timbre_6_min < -114.1775 to the right, improve=1.2823970, (0 missing)
##       timbre_4_max < 113.2695  to the left,  improve=0.4126432, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 110.273   to the left,  agree=0.699, adj=0.12, (0 split)
## 
## Node number 244: 23 observations
##   predicted class=N  expected loss=0.2173913  P(node) =0.003194001
##     class counts:    18     5
##    probabilities: 0.783 0.217 
## 
## Node number 245: 175 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3828571  P(node) =0.02430218
##     class counts:   108    67
##    probabilities: 0.617 0.383 
##   left son=490 (87 obs) right son=491 (88 obs)
##   Primary splits:
##       timbre_6_min < -120.7485 to the right, improve=1.8194000, (0 missing)
##       timbre_4_max < 169.243   to the left,  improve=0.8804762, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 156.5615  to the right, agree=0.577, adj=0.149, (0 split)
## 
## Node number 360: 1118 observations,    complexity param=0.0003144654
##   predicted class=N  expected loss=0.1359571  P(node) =0.1552562
##     class counts:   966   152
##    probabilities: 0.864 0.136 
##   left son=720 (38 obs) right son=721 (1080 obs)
##   Primary splits:
##       timbre_6_min < -65.843   to the right, improve=0.9457575, (0 missing)
##       timbre_4_max < 119.0535  to the left,  improve=0.6736932, (0 missing)
## 
## Node number 361: 57 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.2280702  P(node) =0.007915567
##     class counts:    44    13
##    probabilities: 0.772 0.228 
##   left son=722 (45 obs) right son=723 (12 obs)
##   Primary splits:
##       timbre_6_min < -65.235   to the right, improve=3.8368420, (0 missing)
##       timbre_4_max < 110.9155  to the right, improve=0.7368421, (0 missing)
## 
## Node number 362: 614 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.1579805  P(node) =0.08526594
##     class counts:   517    97
##    probabilities: 0.842 0.158 
##   left son=724 (551 obs) right son=725 (63 obs)
##   Primary splits:
##       timbre_4_max < 95.1505   to the right, improve=0.5794584, (0 missing)
##       timbre_6_min < -81.6945  to the left,  improve=0.5584624, (0 missing)
## 
## Node number 363: 58 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.2931034  P(node) =0.008054437
##     class counts:    41    17
##    probabilities: 0.707 0.293 
##   left son=726 (45 obs) right son=727 (13 obs)
##   Primary splits:
##       timbre_6_min < -79.4275  to the right, improve=2.017389, (0 missing)
##       timbre_4_max < 94.066    to the left,  improve=1.367816, (0 missing)
## 
## Node number 402: 27 observations
##   predicted class=N  expected loss=0  P(node) =0.003749479
##     class counts:    27     0
##    probabilities: 1.000 0.000 
## 
## Node number 403: 77 observations,    complexity param=0.0001347709
##   predicted class=N  expected loss=0.1818182  P(node) =0.01069296
##     class counts:    63    14
##    probabilities: 0.818 0.182 
##   left son=806 (52 obs) right son=807 (25 obs)
##   Primary splits:
##       timbre_4_max < 84.6245   to the left,  improve=2.3506290, (0 missing)
##       timbre_6_min < -103.534  to the left,  improve=0.5374693, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -98.749   to the left,  agree=0.688, adj=0.04, (0 split)
## 
## Node number 422: 26 observations
##   predicted class=N  expected loss=0.07692308  P(node) =0.00361061
##     class counts:    24     2
##    probabilities: 0.923 0.077 
## 
## Node number 423: 60 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.3166667  P(node) =0.008332176
##     class counts:    41    19
##    probabilities: 0.683 0.317 
##   left son=846 (52 obs) right son=847 (8 obs)
##   Primary splits:
##       timbre_6_min < -96.3905  to the right, improve=3.4666670, (0 missing)
##       timbre_4_max < 102.7715  to the right, improve=0.6205128, (0 missing)
## 
## Node number 430: 9 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.001249826
##     class counts:     7     2
##    probabilities: 0.778 0.222 
## 
## Node number 431: 17 observations
##   predicted class=Y  expected loss=0.3529412  P(node) =0.002360783
##     class counts:     6    11
##    probabilities: 0.353 0.647 
## 
## Node number 452: 129 observations
##   predicted class=N  expected loss=0.1317829  P(node) =0.01791418
##     class counts:   112    17
##    probabilities: 0.868 0.132 
## 
## Node number 453: 242 observations,    complexity param=0.0008086253
##   predicted class=N  expected loss=0.2396694  P(node) =0.03360644
##     class counts:   184    58
##    probabilities: 0.760 0.240 
##   left son=906 (217 obs) right son=907 (25 obs)
##   Primary splits:
##       timbre_4_max < 116.948   to the left,  improve=1.4333700, (0 missing)
##       timbre_6_min < -110.6995 to the left,  improve=0.8281343, (0 missing)
## 
## Node number 454: 391 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2327366  P(node) =0.05429801
##     class counts:   300    91
##    probabilities: 0.767 0.233 
##   left son=908 (18 obs) right son=909 (373 obs)
##   Primary splits:
##       timbre_4_max < 125.5545  to the left,  improve=1.1846900, (0 missing)
##       timbre_6_min < -102.915  to the right, improve=0.8799361, (0 missing)
## 
## Node number 455: 102 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3235294  P(node) =0.0141647
##     class counts:    69    33
##    probabilities: 0.676 0.324 
##   left son=910 (19 obs) right son=911 (83 obs)
##   Primary splits:
##       timbre_6_min < -104.638  to the left,  improve=1.281174, (0 missing)
##       timbre_4_max < 124.123   to the left,  improve=1.024837, (0 missing)
## 
## Node number 458: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.001110957
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 459: 492 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.2764228  P(node) =0.06832384
##     class counts:   356   136
##    probabilities: 0.724 0.276 
##   left son=918 (473 obs) right son=919 (19 obs)
##   Primary splits:
##       timbre_6_min < -110.8735 to the right, improve=2.468288, (0 missing)
##       timbre_4_max < 224.1235  to the right, improve=1.242760, (0 missing)
## 
## Node number 460: 55 observations,    complexity param=0.0006289308
##   predicted class=N  expected loss=0.2545455  P(node) =0.007637828
##     class counts:    41    14
##    probabilities: 0.745 0.255 
##   left son=920 (14 obs) right son=921 (41 obs)
##   Primary splits:
##       timbre_6_min < -88.4635  to the left,  improve=1.2594870, (0 missing)
##       timbre_4_max < 147.1105  to the left,  improve=0.7363636, (0 missing)
## 
## Node number 461: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.0009720872
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 462: 20 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.45  P(node) =0.002777392
##     class counts:    11     9
##    probabilities: 0.550 0.450 
##   left son=924 (7 obs) right son=925 (13 obs)
##   Primary splits:
##       timbre_4_max < 147.704   to the right, improve=0.5813187, (0 missing)
##       timbre_6_min < -89.461   to the left,  improve=0.3646465, (0 missing)
## 
## Node number 463: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001666435
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## Node number 484: 75 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2  P(node) =0.01041522
##     class counts:    60    15
##    probabilities: 0.800 0.200 
##   left son=968 (17 obs) right son=969 (58 obs)
##   Primary splits:
##       timbre_6_min < -119.76   to the right, improve=0.8762677, (0 missing)
##       timbre_4_max < 117.565   to the right, improve=0.8696398, (0 missing)
## 
## Node number 485: 22 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.4090909  P(node) =0.003055131
##     class counts:    13     9
##    probabilities: 0.591 0.409 
##   left son=970 (15 obs) right son=971 (7 obs)
##   Primary splits:
##       timbre_4_max < 126.721   to the left,  improve=1.9125540, (0 missing)
##       timbre_6_min < -135.49   to the left,  improve=0.6534577, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -131.8545 to the left,  agree=0.773, adj=0.286, (0 split)
## 
## Node number 486: 25 observations
##   predicted class=N  expected loss=0.24  P(node) =0.00347174
##     class counts:    19     6
##    probabilities: 0.760 0.240 
## 
## Node number 487: 48 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.4375  P(node) =0.006665741
##     class counts:    27    21
##    probabilities: 0.562 0.438 
##   left son=974 (28 obs) right son=975 (20 obs)
##   Primary splits:
##       timbre_4_max < 121.1635  to the right, improve=1.8107140, (0 missing)
##       timbre_6_min < -116.3835 to the left,  improve=0.8101852, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -117.071  to the right, agree=0.646, adj=0.15, (0 split)
## 
## Node number 490: 87 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3103448  P(node) =0.01208166
##     class counts:    60    27
##    probabilities: 0.690 0.310 
##   left son=980 (65 obs) right son=981 (22 obs)
##   Primary splits:
##       timbre_4_max < 167.48    to the left,  improve=1.2245960, (0 missing)
##       timbre_6_min < -119.501  to the left,  improve=0.7969349, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -120.65   to the right, agree=0.759, adj=0.045, (0 split)
## 
## Node number 491: 88 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.4545455  P(node) =0.01222052
##     class counts:    48    40
##    probabilities: 0.545 0.455 
##   left son=982 (53 obs) right son=983 (35 obs)
##   Primary splits:
##       timbre_6_min < -125.0005 to the left,  improve=0.9064445, (0 missing)
##       timbre_4_max < 145.766   to the right, improve=0.7627850, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 140.308   to the right, agree=0.682, adj=0.2, (0 split)
## 
## Node number 720: 38 observations
##   predicted class=N  expected loss=0.02631579  P(node) =0.005277045
##     class counts:    37     1
##    probabilities: 0.974 0.026 
## 
## Node number 721: 1080 observations,    complexity param=0.0003144654
##   predicted class=N  expected loss=0.1398148  P(node) =0.1499792
##     class counts:   929   151
##    probabilities: 0.860 0.140 
##   left son=1442 (885 obs) right son=1443 (195 obs)
##   Primary splits:
##       timbre_6_min < -68.5755  to the left,  improve=0.9552393, (0 missing)
##       timbre_4_max < 119.0535  to the left,  improve=0.6692646, (0 missing)
## 
## Node number 722: 45 observations
##   predicted class=N  expected loss=0.1333333  P(node) =0.006249132
##     class counts:    39     6
##    probabilities: 0.867 0.133 
## 
## Node number 723: 12 observations
##   predicted class=Y  expected loss=0.4166667  P(node) =0.001666435
##     class counts:     5     7
##    probabilities: 0.417 0.583 
## 
## Node number 724: 551 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.1506352  P(node) =0.07651715
##     class counts:   468    83
##    probabilities: 0.849 0.151 
##   left son=1448 (340 obs) right son=1449 (211 obs)
##   Primary splits:
##       timbre_6_min < -81.6945  to the left,  improve=0.7998523, (0 missing)
##       timbre_4_max < 156.9115  to the left,  improve=0.4770407, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 95.713    to the right, agree=0.619, adj=0.005, (0 split)
## 
## Node number 725: 63 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.2222222  P(node) =0.008748785
##     class counts:    49    14
##    probabilities: 0.778 0.222 
##   left son=1450 (51 obs) right son=1451 (12 obs)
##   Primary splits:
##       timbre_4_max < 93.8785   to the left,  improve=5.856209, (0 missing)
##       timbre_6_min < -83.969   to the right, improve=1.162945, (0 missing)
## 
## Node number 726: 45 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2222222  P(node) =0.006249132
##     class counts:    35    10
##    probabilities: 0.778 0.222 
##   left son=1452 (24 obs) right son=1453 (21 obs)
##   Primary splits:
##       timbre_6_min < -79.1945  to the left,  improve=0.9722222, (0 missing)
##       timbre_4_max < 132.956   to the right, improve=0.5020796, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 129.0205  to the left,  agree=0.756, adj=0.476, (0 split)
## 
## Node number 727: 13 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.001805305
##     class counts:     6     7
##    probabilities: 0.462 0.538 
## 
## Node number 806: 52 observations
##   predicted class=N  expected loss=0.09615385  P(node) =0.007221219
##     class counts:    47     5
##    probabilities: 0.904 0.096 
## 
## Node number 807: 25 observations,    complexity param=0.0001347709
##   predicted class=N  expected loss=0.36  P(node) =0.00347174
##     class counts:    16     9
##    probabilities: 0.640 0.360 
##   left son=1614 (12 obs) right son=1615 (13 obs)
##   Primary splits:
##       timbre_6_min < -103.9    to the left,  improve=1.7251280, (0 missing)
##       timbre_4_max < 86.511    to the left,  improve=0.4611765, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 85.8535   to the left,  agree=0.64, adj=0.25, (0 split)
## 
## Node number 846: 52 observations
##   predicted class=N  expected loss=0.25  P(node) =0.007221219
##     class counts:    39    13
##    probabilities: 0.750 0.250 
## 
## Node number 847: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001110957
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## Node number 906: 217 observations,    complexity param=0.0008086253
##   predicted class=N  expected loss=0.2211982  P(node) =0.0301347
##     class counts:   169    48
##    probabilities: 0.779 0.221 
##   left son=1812 (43 obs) right son=1813 (174 obs)
##   Primary splits:
##       timbre_4_max < 115.6655  to the right, improve=1.762037, (0 missing)
##       timbre_6_min < -102.3525 to the right, improve=1.140433, (0 missing)
## 
## Node number 907: 25 observations,    complexity param=0.0008086253
##   predicted class=N  expected loss=0.4  P(node) =0.00347174
##     class counts:    15    10
##    probabilities: 0.600 0.400 
##   left son=1814 (11 obs) right son=1815 (14 obs)
##   Primary splits:
##       timbre_6_min < -97.937   to the left,  improve=1.870130, (0 missing)
##       timbre_4_max < 117.486   to the right, improve=1.333333, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 117.1815  to the left,  agree=0.64, adj=0.182, (0 split)
## 
## Node number 908: 18 observations
##   predicted class=N  expected loss=0.05555556  P(node) =0.002499653
##     class counts:    17     1
##    probabilities: 0.944 0.056 
## 
## Node number 909: 373 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2412869  P(node) =0.05179836
##     class counts:   283    90
##    probabilities: 0.759 0.241 
##   left son=1818 (366 obs) right son=1819 (7 obs)
##   Primary splits:
##       timbre_4_max < 125.7355  to the right, improve=1.555094, (0 missing)
##       timbre_6_min < -102.915  to the right, improve=1.013744, (0 missing)
## 
## Node number 910: 19 observations
##   predicted class=N  expected loss=0.1578947  P(node) =0.002638522
##     class counts:    16     3
##    probabilities: 0.842 0.158 
## 
## Node number 911: 83 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3614458  P(node) =0.01152618
##     class counts:    53    30
##    probabilities: 0.639 0.361 
##   left son=1822 (72 obs) right son=1823 (11 obs)
##   Primary splits:
##       timbre_6_min < -101.1935 to the right, improve=1.916788, (0 missing)
##       timbre_4_max < 124.599   to the left,  improve=1.916788, (0 missing)
## 
## Node number 918: 473 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.2663848  P(node) =0.06568532
##     class counts:   347   126
##    probabilities: 0.734 0.266 
##   left son=1836 (10 obs) right son=1837 (463 obs)
##   Primary splits:
##       timbre_6_min < -110.4595 to the left,  improve=1.449870, (0 missing)
##       timbre_4_max < 144.1545  to the right, improve=1.223526, (0 missing)
## 
## Node number 919: 19 observations
##   predicted class=Y  expected loss=0.4736842  P(node) =0.002638522
##     class counts:     9    10
##    probabilities: 0.474 0.526 
## 
## Node number 920: 14 observations
##   predicted class=N  expected loss=0.07142857  P(node) =0.001944174
##     class counts:    13     1
##    probabilities: 0.929 0.071 
## 
## Node number 921: 41 observations,    complexity param=0.0006289308
##   predicted class=N  expected loss=0.3170732  P(node) =0.005693654
##     class counts:    28    13
##    probabilities: 0.683 0.317 
##   left son=1842 (10 obs) right son=1843 (31 obs)
##   Primary splits:
##       timbre_4_max < 147.1105  to the left,  improve=1.246420, (0 missing)
##       timbre_6_min < -88.079   to the right, improve=1.092232, (0 missing)
## 
## Node number 924: 7 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.0009720872
##     class counts:     5     2
##    probabilities: 0.714 0.286 
## 
## Node number 925: 13 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.001805305
##     class counts:     6     7
##    probabilities: 0.462 0.538 
## 
## Node number 968: 17 observations
##   predicted class=N  expected loss=0.05882353  P(node) =0.002360783
##     class counts:    16     1
##    probabilities: 0.941 0.059 
## 
## Node number 969: 58 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2413793  P(node) =0.008054437
##     class counts:    44    14
##    probabilities: 0.759 0.241 
##   left son=1938 (30 obs) right son=1939 (28 obs)
##   Primary splits:
##       timbre_6_min < -124.752  to the left,  improve=1.450903, (0 missing)
##       timbre_4_max < 117.565   to the right, improve=1.018123, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 123.654   to the left,  agree=0.621, adj=0.214, (0 split)
## 
## Node number 970: 15 observations
##   predicted class=N  expected loss=0.2666667  P(node) =0.002083044
##     class counts:    11     4
##    probabilities: 0.733 0.267 
## 
## Node number 971: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.0009720872
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 974: 28 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3214286  P(node) =0.003888349
##     class counts:    19     9
##    probabilities: 0.679 0.321 
##   left son=1948 (20 obs) right son=1949 (8 obs)
##   Primary splits:
##       timbre_4_max < 130.9325  to the left,  improve=2.0642860, (0 missing)
##       timbre_6_min < -115.6625 to the right, improve=0.2610693, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -117.0565 to the right, agree=0.857, adj=0.5, (0 split)
## 
## Node number 975: 20 observations,    complexity param=0.001078167
##   predicted class=Y  expected loss=0.4  P(node) =0.002777392
##     class counts:     8    12
##    probabilities: 0.400 0.600 
##   left son=1950 (8 obs) right son=1951 (12 obs)
##   Primary splits:
##       timbre_6_min < -116.3835 to the left,  improve=1.350000, (0 missing)
##       timbre_4_max < 113.5975  to the left,  improve=0.632967, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 112.2645  to the left,  agree=0.7, adj=0.25, (0 split)
## 
## Node number 980: 65 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2615385  P(node) =0.009026524
##     class counts:    48    17
##    probabilities: 0.738 0.262 
##   left son=1960 (16 obs) right son=1961 (49 obs)
##   Primary splits:
##       timbre_4_max < 158.734   to the right, improve=1.6816720, (0 missing)
##       timbre_6_min < -118.351  to the right, improve=0.5464678, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -114.312  to the right, agree=0.769, adj=0.063, (0 split)
## 
## Node number 981: 22 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.4545455  P(node) =0.003055131
##     class counts:    12    10
##    probabilities: 0.545 0.455 
##   left son=1962 (11 obs) right son=1963 (11 obs)
##   Primary splits:
##       timbre_4_max < 180.2065  to the right, improve=1.454545, (0 missing)
##       timbre_6_min < -116.6205 to the left,  improve=1.051948, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -114.0915 to the left,  agree=0.682, adj=0.364, (0 split)
## 
## Node number 982: 53 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3962264  P(node) =0.007360089
##     class counts:    32    21
##    probabilities: 0.604 0.396 
##   left son=1964 (32 obs) right son=1965 (21 obs)
##   Primary splits:
##       timbre_6_min < -130.002  to the right, improve=2.135276, (0 missing)
##       timbre_4_max < 146.959   to the right, improve=1.600915, (0 missing)
## 
## Node number 983: 35 observations,    complexity param=0.001078167
##   predicted class=Y  expected loss=0.4571429  P(node) =0.004860436
##     class counts:    16    19
##    probabilities: 0.457 0.543 
##   left son=1966 (24 obs) right son=1967 (11 obs)
##   Primary splits:
##       timbre_6_min < -123.566  to the right, improve=2.432035, (0 missing)
##       timbre_4_max < 147.26    to the left,  improve=1.136134, (0 missing)
## 
## Node number 1442: 885 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1299435  P(node) =0.1228996
##     class counts:   770   115
##    probabilities: 0.870 0.130 
##   left son=2884 (514 obs) right son=2885 (371 obs)
##   Primary splits:
##       timbre_4_max < 119.0535  to the left,  improve=0.5634018, (0 missing)
##       timbre_6_min < -74.981   to the right, improve=0.4190415, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -78.0445  to the right, agree=0.592, adj=0.027, (0 split)
## 
## Node number 1443: 195 observations,    complexity param=0.0003144654
##   predicted class=N  expected loss=0.1846154  P(node) =0.02707957
##     class counts:   159    36
##    probabilities: 0.815 0.185 
##   left son=2886 (182 obs) right son=2887 (13 obs)
##   Primary splits:
##       timbre_6_min < -68.378   to the right, improve=3.4879120, (0 missing)
##       timbre_4_max < 110.349   to the left,  improve=0.7506216, (0 missing)
## 
## Node number 1448: 340 observations
##   predicted class=N  expected loss=0.1294118  P(node) =0.04721566
##     class counts:   296    44
##    probabilities: 0.871 0.129 
## 
## Node number 1449: 211 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.1848341  P(node) =0.02930149
##     class counts:   172    39
##    probabilities: 0.815 0.185 
##   left son=2898 (123 obs) right son=2899 (88 obs)
##   Primary splits:
##       timbre_6_min < -80.7415  to the right, improve=1.768267, (0 missing)
##       timbre_4_max < 156.9115  to the left,  improve=1.105860, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 96.6865   to the right, agree=0.592, adj=0.023, (0 split)
## 
## Node number 1450: 51 observations
##   predicted class=N  expected loss=0.1176471  P(node) =0.00708235
##     class counts:    45     6
##    probabilities: 0.882 0.118 
## 
## Node number 1451: 12 observations
##   predicted class=Y  expected loss=0.3333333  P(node) =0.001666435
##     class counts:     4     8
##    probabilities: 0.333 0.667 
## 
## Node number 1452: 24 observations
##   predicted class=N  expected loss=0.125  P(node) =0.00333287
##     class counts:    21     3
##    probabilities: 0.875 0.125 
## 
## Node number 1453: 21 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3333333  P(node) =0.002916262
##     class counts:    14     7
##    probabilities: 0.667 0.333 
##   left son=2906 (14 obs) right son=2907 (7 obs)
##   Primary splits:
##       timbre_4_max < 120.2585  to the right, improve=1.1904760, (0 missing)
##       timbre_6_min < -79.0535  to the right, improve=0.3888889, (0 missing)
## 
## Node number 1614: 12 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.001666435
##     class counts:    10     2
##    probabilities: 0.833 0.167 
## 
## Node number 1615: 13 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.001805305
##     class counts:     6     7
##    probabilities: 0.462 0.538 
## 
## Node number 1812: 43 observations
##   predicted class=N  expected loss=0.09302326  P(node) =0.005971393
##     class counts:    39     4
##    probabilities: 0.907 0.093 
## 
## Node number 1813: 174 observations,    complexity param=0.0008086253
##   predicted class=N  expected loss=0.2528736  P(node) =0.02416331
##     class counts:   130    44
##    probabilities: 0.747 0.253 
##   left son=3626 (81 obs) right son=3627 (93 obs)
##   Primary splits:
##       timbre_4_max < 113.117   to the left,  improve=1.3887030, (0 missing)
##       timbre_6_min < -102.898  to the right, improve=0.7858861, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -91.4585  to the right, agree=0.563, adj=0.062, (0 split)
## 
## Node number 1814: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.001527566
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 1815: 14 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001944174
##     class counts:     6     8
##    probabilities: 0.429 0.571 
## 
## Node number 1818: 366 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2349727  P(node) =0.05082627
##     class counts:   280    86
##    probabilities: 0.765 0.235 
##   left son=3636 (14 obs) right son=3637 (352 obs)
##   Primary splits:
##       timbre_4_max < 126.312   to the left,  improve=1.607427, (0 missing)
##       timbre_6_min < -102.915  to the right, improve=1.074406, (0 missing)
## 
## Node number 1819: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 1822: 72 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.3194444  P(node) =0.009998611
##     class counts:    49    23
##    probabilities: 0.681 0.319 
##   left son=3644 (62 obs) right son=3645 (10 obs)
##   Primary splits:
##       timbre_4_max < 124.599   to the left,  improve=3.36362, (0 missing)
##       timbre_6_min < -95.6255  to the left,  improve=1.46746, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -100.4785 to the right, agree=0.875, adj=0.1, (0 split)
## 
## Node number 1823: 11 observations
##   predicted class=Y  expected loss=0.3636364  P(node) =0.001527566
##     class counts:     4     7
##    probabilities: 0.364 0.636 
## 
## Node number 1836: 10 observations
##   predicted class=N  expected loss=0  P(node) =0.001388696
##     class counts:    10     0
##    probabilities: 1.000 0.000 
## 
## Node number 1837: 463 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.2721382  P(node) =0.06429663
##     class counts:   337   126
##    probabilities: 0.728 0.272 
##   left son=3674 (8 obs) right son=3675 (455 obs)
##   Primary splits:
##       timbre_4_max < 224.1235  to the right, improve=1.205782, (0 missing)
##       timbre_6_min < -92.902   to the left,  improve=1.205195, (0 missing)
## 
## Node number 1842: 10 observations
##   predicted class=N  expected loss=0.1  P(node) =0.001388696
##     class counts:     9     1
##    probabilities: 0.900 0.100 
## 
## Node number 1843: 31 observations,    complexity param=0.0006289308
##   predicted class=N  expected loss=0.3870968  P(node) =0.004304958
##     class counts:    19    12
##    probabilities: 0.613 0.387 
##   left son=3686 (23 obs) right son=3687 (8 obs)
##   Primary splits:
##       timbre_6_min < -87.8225  to the right, improve=1.2205470, (0 missing)
##       timbre_4_max < 173.0225  to the right, improve=0.6894754, (0 missing)
## 
## Node number 1938: 30 observations
##   predicted class=N  expected loss=0.1333333  P(node) =0.004166088
##     class counts:    26     4
##    probabilities: 0.867 0.133 
## 
## Node number 1939: 28 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3571429  P(node) =0.003888349
##     class counts:    18    10
##    probabilities: 0.643 0.357 
##   left son=3878 (21 obs) right son=3879 (7 obs)
##   Primary splits:
##       timbre_4_max < 117.6805  to the right, improve=0.8571429, (0 missing)
##       timbre_6_min < -123.025  to the right, improve=0.4571429, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -120.4125 to the left,  agree=0.821, adj=0.286, (0 split)
## 
## Node number 1948: 20 observations
##   predicted class=N  expected loss=0.2  P(node) =0.002777392
##     class counts:    16     4
##    probabilities: 0.800 0.200 
## 
## Node number 1949: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001110957
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 1950: 8 observations
##   predicted class=N  expected loss=0.375  P(node) =0.001110957
##     class counts:     5     3
##    probabilities: 0.625 0.375 
## 
## Node number 1951: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001666435
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## Node number 1960: 16 observations
##   predicted class=N  expected loss=0.0625  P(node) =0.002221914
##     class counts:    15     1
##    probabilities: 0.937 0.062 
## 
## Node number 1961: 49 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3265306  P(node) =0.00680461
##     class counts:    33    16
##    probabilities: 0.673 0.327 
##   left son=3922 (42 obs) right son=3923 (7 obs)
##   Primary splits:
##       timbre_4_max < 157.723   to the left,  improve=0.9795918, (0 missing)
##       timbre_6_min < -119.501  to the left,  improve=0.5510204, (0 missing)
## 
## Node number 1962: 11 observations
##   predicted class=N  expected loss=0.2727273  P(node) =0.001527566
##     class counts:     8     3
##    probabilities: 0.727 0.273 
## 
## Node number 1963: 11 observations
##   predicted class=Y  expected loss=0.3636364  P(node) =0.001527566
##     class counts:     4     7
##    probabilities: 0.364 0.636 
## 
## Node number 1964: 32 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.28125  P(node) =0.004443827
##     class counts:    23     9
##    probabilities: 0.719 0.281 
##   left son=3928 (25 obs) right son=3929 (7 obs)
##   Primary splits:
##       timbre_4_max < 146.904   to the right, improve=1.5089290, (0 missing)
##       timbre_6_min < -126.631  to the left,  improve=0.2867063, (0 missing)
## 
## Node number 1965: 21 observations,    complexity param=0.001078167
##   predicted class=Y  expected loss=0.4285714  P(node) =0.002916262
##     class counts:     9    12
##    probabilities: 0.429 0.571 
##   left son=3930 (14 obs) right son=3931 (7 obs)
##   Primary splits:
##       timbre_4_max < 152.7635  to the right, improve=1.7142860, (0 missing)
##       timbre_6_min < -133.339  to the left,  improve=0.4285714, (0 missing)
## 
## Node number 1966: 24 observations,    complexity param=0.001078167
##   predicted class=N  expected loss=0.4166667  P(node) =0.00333287
##     class counts:    14    10
##    probabilities: 0.583 0.417 
##   left son=3932 (12 obs) right son=3933 (12 obs)
##   Primary splits:
##       timbre_6_min < -121.989  to the left,  improve=1.3333330, (0 missing)
##       timbre_4_max < 147.26    to the left,  improve=0.3333333, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 156.103   to the left,  agree=0.625, adj=0.25, (0 split)
## 
## Node number 1967: 11 observations
##   predicted class=Y  expected loss=0.1818182  P(node) =0.001527566
##     class counts:     2     9
##    probabilities: 0.182 0.818 
## 
## Node number 2884: 514 observations
##   predicted class=N  expected loss=0.114786  P(node) =0.07137898
##     class counts:   455    59
##    probabilities: 0.885 0.115 
## 
## Node number 2885: 371 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1509434  P(node) =0.05152062
##     class counts:   315    56
##    probabilities: 0.849 0.151 
##   left son=5770 (360 obs) right son=5771 (11 obs)
##   Primary splits:
##       timbre_4_max < 119.9485  to the right, improve=2.089794, (0 missing)
##       timbre_6_min < -78.103   to the left,  improve=1.761006, (0 missing)
## 
## Node number 2886: 182 observations
##   predicted class=N  expected loss=0.1593407  P(node) =0.02527427
##     class counts:   153    29
##    probabilities: 0.841 0.159 
## 
## Node number 2887: 13 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.001805305
##     class counts:     6     7
##    probabilities: 0.462 0.538 
## 
## Node number 2898: 123 observations
##   predicted class=N  expected loss=0.1300813  P(node) =0.01708096
##     class counts:   107    16
##    probabilities: 0.870 0.130 
## 
## Node number 2899: 88 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.2613636  P(node) =0.01222052
##     class counts:    65    23
##    probabilities: 0.739 0.261 
##   left son=5798 (81 obs) right son=5799 (7 obs)
##   Primary splits:
##       timbre_6_min < -80.8265  to the left,  improve=1.4622820, (0 missing)
##       timbre_4_max < 125.668   to the right, improve=0.4402042, (0 missing)
## 
## Node number 2906: 14 observations
##   predicted class=N  expected loss=0.2142857  P(node) =0.001944174
##     class counts:    11     3
##    probabilities: 0.786 0.214 
## 
## Node number 2907: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 3626: 81 observations,    complexity param=0.0003144654
##   predicted class=N  expected loss=0.1851852  P(node) =0.01124844
##     class counts:    66    15
##    probabilities: 0.815 0.185 
##   left son=7252 (11 obs) right son=7253 (70 obs)
##   Primary splits:
##       timbre_6_min < -89.441   to the right, improve=0.8730159, (0 missing)
##       timbre_4_max < 110.7735  to the right, improve=0.6463675, (0 missing)
## 
## Node number 3627: 93 observations,    complexity param=0.0008086253
##   predicted class=N  expected loss=0.311828  P(node) =0.01291487
##     class counts:    64    29
##    probabilities: 0.688 0.312 
##   left son=7254 (66 obs) right son=7255 (27 obs)
##   Primary splits:
##       timbre_6_min < -102.816  to the right, improve=1.338221, (0 missing)
##       timbre_4_max < 113.891   to the right, improve=1.292766, (0 missing)
## 
## Node number 3636: 14 observations
##   predicted class=N  expected loss=0  P(node) =0.001944174
##     class counts:    14     0
##    probabilities: 1.000 0.000 
## 
## Node number 3637: 352 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2443182  P(node) =0.0488821
##     class counts:   266    86
##    probabilities: 0.756 0.244 
##   left son=7274 (254 obs) right son=7275 (98 obs)
##   Primary splits:
##       timbre_6_min < -102.915  to the right, improve=1.0375330, (0 missing)
##       timbre_4_max < 127.3095  to the right, improve=0.7744509, (0 missing)
## 
## Node number 3644: 62 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.2580645  P(node) =0.008609915
##     class counts:    46    16
##    probabilities: 0.742 0.258 
##   left son=7288 (21 obs) right son=7289 (41 obs)
##   Primary splits:
##       timbre_4_max < 123.629   to the right, improve=0.8429808, (0 missing)
##       timbre_6_min < -98.7895  to the right, improve=0.4803970, (0 missing)
## 
## Node number 3645: 10 observations
##   predicted class=Y  expected loss=0.3  P(node) =0.001388696
##     class counts:     3     7
##    probabilities: 0.300 0.700 
## 
## Node number 3674: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.001110957
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 3675: 455 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.2769231  P(node) =0.06318567
##     class counts:   329   126
##    probabilities: 0.723 0.277 
##   left son=7350 (401 obs) right son=7351 (54 obs)
##   Primary splits:
##       timbre_6_min < -92.902   to the left,  improve=1.5362490, (0 missing)
##       timbre_4_max < 144.1545  to the right, improve=0.8936559, (0 missing)
## 
## Node number 3686: 23 observations,    complexity param=0.0006289308
##   predicted class=N  expected loss=0.3043478  P(node) =0.003194001
##     class counts:    16     7
##    probabilities: 0.696 0.304 
##   left son=7372 (14 obs) right son=7373 (9 obs)
##   Primary splits:
##       timbre_4_max < 151.452   to the right, improve=1.866115, (0 missing)
##       timbre_6_min < -86.75    to the left,  improve=0.310559, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -87.318   to the left,  agree=0.652, adj=0.111, (0 split)
## 
## Node number 3687: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001110957
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 3878: 21 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2857143  P(node) =0.002916262
##     class counts:    15     6
##    probabilities: 0.714 0.286 
##   left son=7756 (12 obs) right son=7757 (9 obs)
##   Primary splits:
##       timbre_6_min < -122.317  to the right, improve=2.2936510, (0 missing)
##       timbre_4_max < 124.098   to the left,  improve=0.9603175, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 131.217   to the right, agree=0.667, adj=0.222, (0 split)
## 
## Node number 3879: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 3922: 42 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.005832523
##     class counts:    30    12
##    probabilities: 0.714 0.286 
## 
## Node number 3923: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 3928: 25 observations
##   predicted class=N  expected loss=0.2  P(node) =0.00347174
##     class counts:    20     5
##    probabilities: 0.800 0.200 
## 
## Node number 3929: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 3930: 14 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.001944174
##     class counts:     8     6
##    probabilities: 0.571 0.429 
## 
## Node number 3931: 7 observations
##   predicted class=Y  expected loss=0.1428571  P(node) =0.0009720872
##     class counts:     1     6
##    probabilities: 0.143 0.857 
## 
## Node number 3932: 12 observations
##   predicted class=N  expected loss=0.25  P(node) =0.001666435
##     class counts:     9     3
##    probabilities: 0.750 0.250 
## 
## Node number 3933: 12 observations
##   predicted class=Y  expected loss=0.4166667  P(node) =0.001666435
##     class counts:     5     7
##    probabilities: 0.417 0.583 
## 
## Node number 5770: 360 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1416667  P(node) =0.04999306
##     class counts:   309    51
##    probabilities: 0.858 0.142 
##   left son=11540 (35 obs) right son=11541 (325 obs)
##   Primary splits:
##       timbre_6_min < -78.103   to the left,  improve=1.5561540, (0 missing)
##       timbre_4_max < 124.9375  to the right, improve=0.4514314, (0 missing)
## 
## Node number 5771: 11 observations
##   predicted class=N  expected loss=0.4545455  P(node) =0.001527566
##     class counts:     6     5
##    probabilities: 0.545 0.455 
## 
## Node number 5798: 81 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.2345679  P(node) =0.01124844
##     class counts:    62    19
##    probabilities: 0.765 0.235 
##   left son=11596 (58 obs) right son=11597 (23 obs)
##   Primary splits:
##       timbre_6_min < -81.383   to the right, improve=0.8240509, (0 missing)
##       timbre_4_max < 103.923   to the right, improve=0.6244479, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 98.883    to the right, agree=0.728, adj=0.043, (0 split)
## 
## Node number 5799: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 7252: 11 observations
##   predicted class=N  expected loss=0  P(node) =0.001527566
##     class counts:    11     0
##    probabilities: 1.000 0.000 
## 
## Node number 7253: 70 observations,    complexity param=0.0003144654
##   predicted class=N  expected loss=0.2142857  P(node) =0.009720872
##     class counts:    55    15
##    probabilities: 0.786 0.214 
##   left son=14506 (46 obs) right son=14507 (24 obs)
##   Primary splits:
##       timbre_6_min < -93.8255  to the left,  improve=1.0351970, (0 missing)
##       timbre_4_max < 110.7735  to the right, improve=0.7142857, (0 missing)
## 
## Node number 7254: 66 observations,    complexity param=0.0008086253
##   predicted class=N  expected loss=0.2575758  P(node) =0.009165394
##     class counts:    49    17
##    probabilities: 0.742 0.258 
##   left son=14508 (26 obs) right son=14509 (40 obs)
##   Primary splits:
##       timbre_6_min < -95.7075  to the left,  improve=1.7347320, (0 missing)
##       timbre_4_max < 114.5965  to the right, improve=0.9231935, (0 missing)
## 
## Node number 7255: 27 observations,    complexity param=0.0008086253
##   predicted class=N  expected loss=0.4444444  P(node) =0.003749479
##     class counts:    15    12
##    probabilities: 0.556 0.444 
##   left son=14510 (17 obs) right son=14511 (10 obs)
##   Primary splits:
##       timbre_6_min < -105.5865 to the left,  improve=2.0745100, (0 missing)
##       timbre_4_max < 114.859   to the right, improve=0.5333333, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 113.6145  to the right, agree=0.741, adj=0.3, (0 split)
## 
## Node number 7274: 254 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2204724  P(node) =0.03527288
##     class counts:   198    56
##    probabilities: 0.780 0.220 
##   left son=14548 (195 obs) right son=14549 (59 obs)
##   Primary splits:
##       timbre_4_max < 134.869   to the left,  improve=1.585400, (0 missing)
##       timbre_6_min < -101.875  to the left,  improve=1.117786, (0 missing)
## 
## Node number 7275: 98 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3061224  P(node) =0.01360922
##     class counts:    68    30
##    probabilities: 0.694 0.306 
##   left son=14550 (87 obs) right son=14551 (11 obs)
##   Primary splits:
##       timbre_4_max < 127.5305  to the right, improve=1.419487, (0 missing)
##       timbre_6_min < -108.277  to the left,  improve=1.090139, (0 missing)
## 
## Node number 7288: 21 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.002916262
##     class counts:    18     3
##    probabilities: 0.857 0.143 
## 
## Node number 7289: 41 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.3170732  P(node) =0.005693654
##     class counts:    28    13
##    probabilities: 0.683 0.317 
##   left son=14578 (18 obs) right son=14579 (23 obs)
##   Primary splits:
##       timbre_6_min < -96.115   to the left,  improve=0.5773536, (0 missing)
##       timbre_4_max < 122.467   to the left,  improve=0.2360976, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 122.499   to the left,  agree=0.683, adj=0.278, (0 split)
## 
## Node number 7350: 401 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.2618454  P(node) =0.05568671
##     class counts:   296   105
##    probabilities: 0.738 0.262 
##   left son=14700 (22 obs) right son=14701 (379 obs)
##   Primary splits:
##       timbre_6_min < -93.8975  to the right, improve=1.3602740, (0 missing)
##       timbre_4_max < 187.1575  to the left,  improve=0.6537155, (0 missing)
## 
## Node number 7351: 54 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.3888889  P(node) =0.007498958
##     class counts:    33    21
##    probabilities: 0.611 0.389 
##   left son=14702 (44 obs) right son=14703 (10 obs)
##   Primary splits:
##       timbre_4_max < 141.9325  to the right, improve=2.3757580, (0 missing)
##       timbre_6_min < -92.3515  to the right, improve=0.7843137, (0 missing)
## 
## Node number 7372: 14 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.001944174
##     class counts:    12     2
##    probabilities: 0.857 0.143 
## 
## Node number 7373: 9 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.001249826
##     class counts:     4     5
##    probabilities: 0.444 0.556 
## 
## Node number 7756: 12 observations
##   predicted class=N  expected loss=0.08333333  P(node) =0.001666435
##     class counts:    11     1
##    probabilities: 0.917 0.083 
## 
## Node number 7757: 9 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.001249826
##     class counts:     4     5
##    probabilities: 0.444 0.556 
## 
## Node number 11540: 35 observations
##   predicted class=N  expected loss=0  P(node) =0.004860436
##     class counts:    35     0
##    probabilities: 1.000 0.000 
## 
## Node number 11541: 325 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1569231  P(node) =0.04513262
##     class counts:   274    51
##    probabilities: 0.843 0.157 
##   left son=23082 (314 obs) right son=23083 (11 obs)
##   Primary splits:
##       timbre_6_min < -77.8695  to the right, improve=2.0170080, (0 missing)
##       timbre_4_max < 120.4265  to the left,  improve=0.4558715, (0 missing)
## 
## Node number 11596: 58 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.1896552  P(node) =0.008054437
##     class counts:    47    11
##    probabilities: 0.810 0.190 
##   left son=23192 (24 obs) right son=23193 (34 obs)
##   Primary splits:
##       timbre_6_min < -81.14    to the left,  improve=1.7932720, (0 missing)
##       timbre_4_max < 155.0395  to the left,  improve=0.9088187, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 110.414   to the left,  agree=0.603, adj=0.042, (0 split)
## 
## Node number 11597: 23 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.3478261  P(node) =0.003194001
##     class counts:    15     8
##    probabilities: 0.652 0.348 
##   left son=23194 (16 obs) right son=23195 (7 obs)
##   Primary splits:
##       timbre_6_min < -81.452   to the left,  improve=1.0062110, (0 missing)
##       timbre_4_max < 125.668   to the right, improve=0.8454969, (0 missing)
## 
## Node number 14506: 46 observations
##   predicted class=N  expected loss=0.1521739  P(node) =0.006388002
##     class counts:    39     7
##    probabilities: 0.848 0.152 
## 
## Node number 14507: 24 observations,    complexity param=0.0003144654
##   predicted class=N  expected loss=0.3333333  P(node) =0.00333287
##     class counts:    16     8
##    probabilities: 0.667 0.333 
##   left son=29014 (17 obs) right son=29015 (7 obs)
##   Primary splits:
##       timbre_4_max < 111.553   to the right, improve=1.1204480, (0 missing)
##       timbre_6_min < -90.4815  to the left,  improve=0.1792717, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -93.2075  to the right, agree=0.792, adj=0.286, (0 split)
## 
## Node number 14508: 26 observations
##   predicted class=N  expected loss=0.1153846  P(node) =0.00361061
##     class counts:    23     3
##    probabilities: 0.885 0.115 
## 
## Node number 14509: 40 observations,    complexity param=0.0008086253
##   predicted class=N  expected loss=0.35  P(node) =0.005554784
##     class counts:    26    14
##    probabilities: 0.650 0.350 
##   left son=29018 (30 obs) right son=29019 (10 obs)
##   Primary splits:
##       timbre_4_max < 113.5145  to the right, improve=1.6666670, (0 missing)
##       timbre_6_min < -92.434   to the right, improve=0.6533333, (0 missing)
## 
## Node number 14510: 17 observations
##   predicted class=N  expected loss=0.2941176  P(node) =0.002360783
##     class counts:    12     5
##    probabilities: 0.706 0.294 
## 
## Node number 14511: 10 observations
##   predicted class=Y  expected loss=0.3  P(node) =0.001388696
##     class counts:     3     7
##    probabilities: 0.300 0.700 
## 
## Node number 14548: 195 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.1897436  P(node) =0.02707957
##     class counts:   158    37
##    probabilities: 0.810 0.190 
##   left son=29096 (47 obs) right son=29097 (148 obs)
##   Primary splits:
##       timbre_4_max < 133.052   to the right, improve=1.3560420, (0 missing)
##       timbre_6_min < -100.7345 to the left,  improve=0.7141533, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -86.286   to the right, agree=0.764, adj=0.021, (0 split)
## 
## Node number 14549: 59 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3220339  P(node) =0.008193306
##     class counts:    40    19
##    probabilities: 0.678 0.322 
##   left son=29098 (28 obs) right son=29099 (31 obs)
##   Primary splits:
##       timbre_6_min < -93.3205  to the right, improve=1.2373660, (0 missing)
##       timbre_4_max < 137.2545  to the right, improve=0.7185942, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 137.2775  to the right, agree=0.61, adj=0.179, (0 split)
## 
## Node number 14550: 87 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2758621  P(node) =0.01208166
##     class counts:    63    24
##    probabilities: 0.724 0.276 
##   left son=29100 (24 obs) right son=29101 (63 obs)
##   Primary splits:
##       timbre_6_min < -108.277  to the left,  improve=0.7903667, (0 missing)
##       timbre_4_max < 130.4925  to the left,  improve=0.6499250, (0 missing)
## 
## Node number 14551: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.001527566
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 14578: 18 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.002499653
##     class counts:    14     4
##    probabilities: 0.778 0.222 
## 
## Node number 14579: 23 observations,    complexity param=0.0009433962
##   predicted class=N  expected loss=0.3913043  P(node) =0.003194001
##     class counts:    14     9
##    probabilities: 0.609 0.391 
##   left son=29158 (14 obs) right son=29159 (9 obs)
##   Primary splits:
##       timbre_6_min < -93.072   to the right, improve=2.242236, (0 missing)
##       timbre_4_max < 122.9475  to the right, improve=1.294983, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 122.9475  to the right, agree=0.652, adj=0.111, (0 split)
## 
## Node number 14700: 22 observations
##   predicted class=N  expected loss=0.09090909  P(node) =0.003055131
##     class counts:    20     2
##    probabilities: 0.909 0.091 
## 
## Node number 14701: 379 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.2717678  P(node) =0.05263158
##     class counts:   276   103
##    probabilities: 0.728 0.272 
##   left son=29402 (357 obs) right son=29403 (22 obs)
##   Primary splits:
##       timbre_4_max < 187.1575  to the left,  improve=0.8808681, (0 missing)
##       timbre_6_min < -100.37   to the left,  improve=0.7616642, (0 missing)
## 
## Node number 14702: 44 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.3181818  P(node) =0.006110262
##     class counts:    30    14
##    probabilities: 0.682 0.318 
##   left son=29404 (15 obs) right son=29405 (29 obs)
##   Primary splits:
##       timbre_4_max < 150.4675  to the left,  improve=2.879415, (0 missing)
##       timbre_6_min < -92.3515  to the right, improve=1.286988, (0 missing)
## 
## Node number 14703: 10 observations
##   predicted class=Y  expected loss=0.3  P(node) =0.001388696
##     class counts:     3     7
##    probabilities: 0.300 0.700 
## 
## Node number 23082: 314 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1464968  P(node) =0.04360505
##     class counts:   268    46
##    probabilities: 0.854 0.146 
##   left son=46164 (76 obs) right son=46165 (238 obs)
##   Primary splits:
##       timbre_6_min < -76.0415  to the left,  improve=0.5932793, (0 missing)
##       timbre_4_max < 121.5315  to the right, improve=0.4026349, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 185.1995  to the right, agree=0.764, adj=0.026, (0 split)
## 
## Node number 23083: 11 observations
##   predicted class=N  expected loss=0.4545455  P(node) =0.001527566
##     class counts:     6     5
##    probabilities: 0.545 0.455 
## 
## Node number 23192: 24 observations
##   predicted class=N  expected loss=0.04166667  P(node) =0.00333287
##     class counts:    23     1
##    probabilities: 0.958 0.042 
## 
## Node number 23193: 34 observations,    complexity param=0.0005390836
##   predicted class=N  expected loss=0.2941176  P(node) =0.004721566
##     class counts:    24    10
##    probabilities: 0.706 0.294 
##   left son=46386 (27 obs) right son=46387 (7 obs)
##   Primary splits:
##       timbre_6_min < -81.1045  to the right, improve=3.1123560, (0 missing)
##       timbre_4_max < 142.4145  to the left,  improve=0.3187053, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 147.683   to the left,  agree=0.912, adj=0.571, (0 split)
## 
## Node number 23194: 16 observations
##   predicted class=N  expected loss=0.25  P(node) =0.002221914
##     class counts:    12     4
##    probabilities: 0.750 0.250 
## 
## Node number 23195: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 29014: 17 observations
##   predicted class=N  expected loss=0.2352941  P(node) =0.002360783
##     class counts:    13     4
##    probabilities: 0.765 0.235 
## 
## Node number 29015: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 29018: 30 observations
##   predicted class=N  expected loss=0.2666667  P(node) =0.004166088
##     class counts:    22     8
##    probabilities: 0.733 0.267 
## 
## Node number 29019: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.001388696
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 29096: 47 observations
##   predicted class=N  expected loss=0.08510638  P(node) =0.006526871
##     class counts:    43     4
##    probabilities: 0.915 0.085 
## 
## Node number 29097: 148 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.222973  P(node) =0.0205527
##     class counts:   115    33
##    probabilities: 0.777 0.223 
##   left son=58194 (73 obs) right son=58195 (75 obs)
##   Primary splits:
##       timbre_4_max < 129.356   to the left,  improve=1.505519, (0 missing)
##       timbre_6_min < -99.202   to the left,  improve=1.035019, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -93.21    to the right, agree=0.595, adj=0.178, (0 split)
## 
## Node number 29098: 28 observations
##   predicted class=N  expected loss=0.2142857  P(node) =0.003888349
##     class counts:    22     6
##    probabilities: 0.786 0.214 
## 
## Node number 29099: 31 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.4193548  P(node) =0.004304958
##     class counts:    18    13
##    probabilities: 0.581 0.419 
##   left son=58198 (19 obs) right son=58199 (12 obs)
##   Primary splits:
##       timbre_4_max < 135.773   to the right, improve=1.0529150, (0 missing)
##       timbre_6_min < -100.434  to the right, improve=0.4182028, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -93.773   to the left,  agree=0.677, adj=0.167, (0 split)
## 
## Node number 29100: 24 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.00333287
##     class counts:    20     4
##    probabilities: 0.833 0.167 
## 
## Node number 29101: 63 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3174603  P(node) =0.008748785
##     class counts:    43    20
##    probabilities: 0.683 0.317 
##   left son=58202 (18 obs) right son=58203 (45 obs)
##   Primary splits:
##       timbre_4_max < 130.4925  to the left,  improve=1.1460320, (0 missing)
##       timbre_6_min < -107.592  to the right, improve=0.3386243, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -107.671  to the left,  agree=0.746, adj=0.111, (0 split)
## 
## Node number 29158: 14 observations
##   predicted class=N  expected loss=0.2142857  P(node) =0.001944174
##     class counts:    11     3
##    probabilities: 0.786 0.214 
## 
## Node number 29159: 9 observations
##   predicted class=Y  expected loss=0.3333333  P(node) =0.001249826
##     class counts:     3     6
##    probabilities: 0.333 0.667 
## 
## Node number 29402: 357 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.2633053  P(node) =0.04957645
##     class counts:   263    94
##    probabilities: 0.737 0.263 
##   left son=58804 (288 obs) right son=58805 (69 obs)
##   Primary splits:
##       timbre_6_min < -96.643   to the left,  improve=0.8388772, (0 missing)
##       timbre_4_max < 144.1545  to the right, improve=0.6154952, (0 missing)
## 
## Node number 29403: 22 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.4090909  P(node) =0.003055131
##     class counts:    13     9
##    probabilities: 0.591 0.409 
##   left son=58806 (15 obs) right son=58807 (7 obs)
##   Primary splits:
##       timbre_4_max < 195.063   to the right, improve=1.912554, (0 missing)
##       timbre_6_min < -99.7295  to the right, improve=1.063714, (0 missing)
## 
## Node number 29404: 15 observations
##   predicted class=N  expected loss=0.06666667  P(node) =0.002083044
##     class counts:    14     1
##    probabilities: 0.933 0.067 
## 
## Node number 29405: 29 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.4482759  P(node) =0.004027218
##     class counts:    16    13
##    probabilities: 0.552 0.448 
##   left son=58810 (8 obs) right son=58811 (21 obs)
##   Primary splits:
##       timbre_4_max < 174.053   to the right, improve=2.309113, (0 missing)
##       timbre_6_min < -92.3515  to the right, improve=1.952671, (0 missing)
## 
## Node number 46164: 76 observations
##   predicted class=N  expected loss=0.09210526  P(node) =0.01055409
##     class counts:    69     7
##    probabilities: 0.908 0.092 
## 
## Node number 46165: 238 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1638655  P(node) =0.03305097
##     class counts:   199    39
##    probabilities: 0.836 0.164 
##   left son=92330 (204 obs) right son=92331 (34 obs)
##   Primary splits:
##       timbre_6_min < -75.1125  to the right, improve=1.3459380, (0 missing)
##       timbre_4_max < 120.4265  to the left,  improve=0.5023302, (0 missing)
## 
## Node number 46386: 27 observations
##   predicted class=N  expected loss=0.1851852  P(node) =0.003749479
##     class counts:    22     5
##    probabilities: 0.815 0.185 
## 
## Node number 46387: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.0009720872
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 58194: 73 observations
##   predicted class=N  expected loss=0.1506849  P(node) =0.01013748
##     class counts:    62    11
##    probabilities: 0.849 0.151 
## 
## Node number 58195: 75 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2933333  P(node) =0.01041522
##     class counts:    53    22
##    probabilities: 0.707 0.293 
##   left son=116390 (67 obs) right son=116391 (8 obs)
##   Primary splits:
##       timbre_4_max < 129.608   to the right, improve=3.735124, (0 missing)
##       timbre_6_min < -95.8915  to the left,  improve=1.548592, (0 missing)
## 
## Node number 58198: 19 observations
##   predicted class=N  expected loss=0.3157895  P(node) =0.002638522
##     class counts:    13     6
##    probabilities: 0.684 0.316 
## 
## Node number 58199: 12 observations
##   predicted class=Y  expected loss=0.4166667  P(node) =0.001666435
##     class counts:     5     7
##    probabilities: 0.417 0.583 
## 
## Node number 58202: 18 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.002499653
##     class counts:    15     3
##    probabilities: 0.833 0.167 
## 
## Node number 58203: 45 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3777778  P(node) =0.006249132
##     class counts:    28    17
##    probabilities: 0.622 0.378 
##   left son=116406 (29 obs) right son=116407 (16 obs)
##   Primary splits:
##       timbre_4_max < 132.712   to the right, improve=3.034866, (0 missing)
##       timbre_6_min < -104.2985 to the left,  improve=0.384127, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -107.516  to the right, agree=0.667, adj=0.062, (0 split)
## 
## Node number 58804: 288 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2465278  P(node) =0.03999445
##     class counts:   217    71
##    probabilities: 0.753 0.247 
##   left son=117608 (9 obs) right son=117609 (279 obs)
##   Primary splits:
##       timbre_6_min < -97.0515  to the right, improve=1.1292560, (0 missing)
##       timbre_4_max < 144.1545  to the right, improve=0.7162698, (0 missing)
## 
## Node number 58805: 69 observations,    complexity param=0.0006738544
##   predicted class=N  expected loss=0.3333333  P(node) =0.009582002
##     class counts:    46    23
##    probabilities: 0.667 0.333 
##   left son=117610 (55 obs) right son=117611 (14 obs)
##   Primary splits:
##       timbre_6_min < -96.346   to the right, improve=5.097835, (0 missing)
##       timbre_4_max < 140.693   to the left,  improve=1.731183, (0 missing)
## 
## Node number 58806: 15 observations
##   predicted class=N  expected loss=0.2666667  P(node) =0.002083044
##     class counts:    11     4
##    probabilities: 0.733 0.267 
## 
## Node number 58807: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.0009720872
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 58810: 8 observations
##   predicted class=N  expected loss=0.125  P(node) =0.001110957
##     class counts:     7     1
##    probabilities: 0.875 0.125 
## 
## Node number 58811: 21 observations,    complexity param=0.0006738544
##   predicted class=Y  expected loss=0.4285714  P(node) =0.002916262
##     class counts:     9    12
##    probabilities: 0.429 0.571 
##   left son=117622 (12 obs) right son=117623 (9 obs)
##   Primary splits:
##       timbre_6_min < -92.31    to the right, improve=1.3412700, (0 missing)
##       timbre_4_max < 160.1845  to the right, improve=0.4285714, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 163.2825  to the left,  agree=0.667, adj=0.222, (0 split)
## 
## Node number 92330: 204 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1421569  P(node) =0.0283294
##     class counts:   175    29
##    probabilities: 0.858 0.142 
##   left son=184660 (12 obs) right son=184661 (192 obs)
##   Primary splits:
##       timbre_4_max < 120.8475  to the left,  improve=0.5153186, (0 missing)
##       timbre_6_min < -70.573   to the right, improve=0.2428566, (0 missing)
## 
## Node number 92331: 34 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.2941176  P(node) =0.004721566
##     class counts:    24    10
##    probabilities: 0.706 0.294 
##   left son=184662 (13 obs) right son=184663 (21 obs)
##   Primary splits:
##       timbre_6_min < -75.681   to the left,  improve=0.8282698, (0 missing)
##       timbre_4_max < 151.6005  to the right, improve=0.4033613, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 147.3055  to the right, agree=0.706, adj=0.231, (0 split)
## 
## Node number 116390: 67 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.238806  P(node) =0.009304263
##     class counts:    51    16
##    probabilities: 0.761 0.239 
##   left son=232780 (22 obs) right son=232781 (45 obs)
##   Primary splits:
##       timbre_6_min < -95.8915  to the left,  improve=2.449118, (0 missing)
##       timbre_4_max < 130.831   to the left,  improve=1.306738, (0 missing)
## 
## Node number 116391: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001110957
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## Node number 116406: 29 observations
##   predicted class=N  expected loss=0.2413793  P(node) =0.004027218
##     class counts:    22     7
##    probabilities: 0.759 0.241 
## 
## Node number 116407: 16 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.002221914
##     class counts:     6    10
##    probabilities: 0.375 0.625 
## 
## Node number 117608: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.001249826
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 117609: 279 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2544803  P(node) =0.03874462
##     class counts:   208    71
##    probabilities: 0.746 0.254 
##   left son=235218 (272 obs) right son=235219 (7 obs)
##   Primary splits:
##       timbre_6_min < -97.331   to the left,  improve=1.4425810, (0 missing)
##       timbre_4_max < 144.1545  to the right, improve=0.6455453, (0 missing)
## 
## Node number 117610: 55 observations
##   predicted class=N  expected loss=0.2363636  P(node) =0.007637828
##     class counts:    42    13
##    probabilities: 0.764 0.236 
## 
## Node number 117611: 14 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.001944174
##     class counts:     4    10
##    probabilities: 0.286 0.714 
## 
## Node number 117622: 12 observations
##   predicted class=N  expected loss=0.4166667  P(node) =0.001666435
##     class counts:     7     5
##    probabilities: 0.583 0.417 
## 
## Node number 117623: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.001249826
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 184660: 12 observations
##   predicted class=N  expected loss=0  P(node) =0.001666435
##     class counts:    12     0
##    probabilities: 1.000 0.000 
## 
## Node number 184661: 192 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1510417  P(node) =0.02666296
##     class counts:   163    29
##    probabilities: 0.849 0.151 
##   left son=369322 (185 obs) right son=369323 (7 obs)
##   Primary splits:
##       timbre_4_max < 121.6295  to the right, improve=1.1191200, (0 missing)
##       timbre_6_min < -70.573   to the right, improve=0.3047094, (0 missing)
## 
## Node number 184662: 13 observations
##   predicted class=N  expected loss=0.1538462  P(node) =0.001805305
##     class counts:    11     2
##    probabilities: 0.846 0.154 
## 
## Node number 184663: 21 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.3809524  P(node) =0.002916262
##     class counts:    13     8
##    probabilities: 0.619 0.381 
##   left son=369326 (14 obs) right son=369327 (7 obs)
##   Primary splits:
##       timbre_6_min < -75.4695  to the right, improve=0.7619048, (0 missing)
##       timbre_4_max < 131.742   to the right, improve=0.7619048, (0 missing)
##   Surrogate splits:
##       timbre_4_max < 137.361   to the left,  agree=0.714, adj=0.143, (0 split)
## 
## Node number 232780: 22 observations
##   predicted class=N  expected loss=0.04545455  P(node) =0.003055131
##     class counts:    21     1
##    probabilities: 0.955 0.045 
## 
## Node number 232781: 45 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3333333  P(node) =0.006249132
##     class counts:    30    15
##    probabilities: 0.667 0.333 
##   left son=465562 (38 obs) right son=465563 (7 obs)
##   Primary splits:
##       timbre_4_max < 132.4285  to the left,  improve=2.406015, (0 missing)
##       timbre_6_min < -94.821   to the right, improve=1.655405, (0 missing)
## 
## Node number 235218: 272 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2463235  P(node) =0.03777253
##     class counts:   205    67
##    probabilities: 0.754 0.246 
##   left son=470436 (9 obs) right son=470437 (263 obs)
##   Primary splits:
##       timbre_6_min < -97.639   to the right, improve=1.1295290, (0 missing)
##       timbre_4_max < 144.1545  to the right, improve=0.6675752, (0 missing)
## 
## Node number 235219: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 369322: 185 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1405405  P(node) =0.02569088
##     class counts:   159    26
##    probabilities: 0.859 0.141 
##   left son=738644 (16 obs) right son=738645 (169 obs)
##   Primary splits:
##       timbre_4_max < 123.7945  to the left,  improve=0.6918919, (0 missing)
##       timbre_6_min < -74.8875  to the left,  improve=0.4176062, (0 missing)
## 
## Node number 369323: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 369326: 14 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.001944174
##     class counts:    10     4
##    probabilities: 0.714 0.286 
## 
## Node number 369327: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.0009720872
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 465562: 38 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2631579  P(node) =0.005277045
##     class counts:    28    10
##    probabilities: 0.737 0.263 
##   left son=931124 (12 obs) right son=931125 (26 obs)
##   Primary splits:
##       timbre_4_max < 130.831   to the left,  improve=1.1342780, (0 missing)
##       timbre_6_min < -89.7845  to the right, improve=0.4722267, (0 missing)
## 
## Node number 465563: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.0009720872
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 470436: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.001249826
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 470437: 263 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2547529  P(node) =0.03652271
##     class counts:   196    67
##    probabilities: 0.745 0.255 
##   left son=940874 (243 obs) right son=940875 (20 obs)
##   Primary splits:
##       timbre_6_min < -109.2095 to the right, improve=0.9133236, (0 missing)
##       timbre_4_max < 144.1545  to the right, improve=0.6886413, (0 missing)
## 
## Node number 738644: 16 observations
##   predicted class=N  expected loss=0  P(node) =0.002221914
##     class counts:    16     0
##    probabilities: 1.000 0.000 
## 
## Node number 738645: 169 observations,    complexity param=0.0002358491
##   predicted class=N  expected loss=0.1538462  P(node) =0.02346896
##     class counts:   143    26
##    probabilities: 0.846 0.154 
##   left son=1477290 (161 obs) right son=1477291 (8 obs)
##   Primary splits:
##       timbre_4_max < 124.9935  to the right, improve=3.728261, (0 missing)
##       timbre_6_min < -74.8875  to the left,  improve=0.450000, (0 missing)
## 
## Node number 931124: 12 observations
##   predicted class=N  expected loss=0.08333333  P(node) =0.001666435
##     class counts:    11     1
##    probabilities: 0.917 0.083 
## 
## Node number 931125: 26 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3461538  P(node) =0.00361061
##     class counts:    17     9
##    probabilities: 0.654 0.346 
##   left son=1862250 (13 obs) right son=1862251 (13 obs)
##   Primary splits:
##       timbre_4_max < 131.897   to the right, improve=1.9230770, (0 missing)
##       timbre_6_min < -89.763   to the right, improve=0.2136752, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -90.37    to the right, agree=0.654, adj=0.308, (0 split)
## 
## Node number 940874: 243 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2427984  P(node) =0.03374531
##     class counts:   184    59
##    probabilities: 0.757 0.243 
##   left son=1881748 (38 obs) right son=1881749 (205 obs)
##   Primary splits:
##       timbre_6_min < -107.1325 to the left,  improve=1.1143640, (0 missing)
##       timbre_4_max < 144.1545  to the right, improve=0.8998324, (0 missing)
## 
## Node number 940875: 20 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.4  P(node) =0.002777392
##     class counts:    12     8
##    probabilities: 0.600 0.400 
##   left son=1881750 (9 obs) right son=1881751 (11 obs)
##   Primary splits:
##       timbre_4_max < 149.5945  to the left,  improve=1.0343430, (0 missing)
##       timbre_6_min < -110.0255 to the left,  improve=0.1454545, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -110.1945 to the right, agree=0.65, adj=0.222, (0 split)
## 
## Node number 1477290: 161 observations
##   predicted class=N  expected loss=0.1304348  P(node) =0.02235801
##     class counts:   140    21
##    probabilities: 0.870 0.130 
## 
## Node number 1477291: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001110957
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 1862250: 13 observations
##   predicted class=N  expected loss=0.1538462  P(node) =0.001805305
##     class counts:    11     2
##    probabilities: 0.846 0.154 
## 
## Node number 1862251: 13 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.001805305
##     class counts:     6     7
##    probabilities: 0.462 0.538 
## 
## Node number 1881748: 38 observations
##   predicted class=N  expected loss=0.1315789  P(node) =0.005277045
##     class counts:    33     5
##    probabilities: 0.868 0.132 
## 
## Node number 1881749: 205 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2634146  P(node) =0.02846827
##     class counts:   151    54
##    probabilities: 0.737 0.263 
##   left son=3763498 (182 obs) right son=3763499 (23 obs)
##   Primary splits:
##       timbre_6_min < -106.1775 to the right, improve=2.3916400, (0 missing)
##       timbre_4_max < 143.4215  to the right, improve=0.7451589, (0 missing)
## 
## Node number 1881750: 9 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.001249826
##     class counts:     7     2
##    probabilities: 0.778 0.222 
## 
## Node number 1881751: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.001527566
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 3763498: 182 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2362637  P(node) =0.02527427
##     class counts:   139    43
##    probabilities: 0.764 0.236 
##   left son=7526996 (141 obs) right son=7526997 (41 obs)
##   Primary splits:
##       timbre_6_min < -99.4305  to the left,  improve=1.1713720, (0 missing)
##       timbre_4_max < 146.1955  to the left,  improve=0.3386245, (0 missing)
## 
## Node number 3763499: 23 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.4782609  P(node) =0.003194001
##     class counts:    12    11
##    probabilities: 0.522 0.478 
##   left son=7526998 (10 obs) right son=7526999 (13 obs)
##   Primary splits:
##       timbre_4_max < 153.3585  to the right, improve=1.1244150, (0 missing)
##       timbre_6_min < -106.3975 to the left,  improve=0.5282609, (0 missing)
##   Surrogate splits:
##       timbre_6_min < -106.347  to the left,  agree=0.652, adj=0.2, (0 split)
## 
## Node number 7526996: 141 observations
##   predicted class=N  expected loss=0.2056738  P(node) =0.01958061
##     class counts:   112    29
##    probabilities: 0.794 0.206 
## 
## Node number 7526997: 41 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.3414634  P(node) =0.005693654
##     class counts:    27    14
##    probabilities: 0.659 0.341 
##   left son=15053994 (33 obs) right son=15053995 (8 obs)
##   Primary splits:
##       timbre_6_min < -99.171   to the right, improve=1.598115, (0 missing)
##       timbre_4_max < 144.1625  to the right, improve=1.598115, (0 missing)
## 
## Node number 7526998: 10 observations
##   predicted class=N  expected loss=0.3  P(node) =0.001388696
##     class counts:     7     3
##    probabilities: 0.700 0.300 
## 
## Node number 7526999: 13 observations
##   predicted class=Y  expected loss=0.3846154  P(node) =0.001805305
##     class counts:     5     8
##    probabilities: 0.385 0.615 
## 
## Node number 15053994: 33 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.2727273  P(node) =0.004582697
##     class counts:    24     9
##    probabilities: 0.727 0.273 
##   left son=30107988 (8 obs) right son=30107989 (25 obs)
##   Primary splits:
##       timbre_6_min < -98.8505  to the left,  improve=1.5709090, (0 missing)
##       timbre_4_max < 169.9855  to the right, improve=0.4609091, (0 missing)
## 
## Node number 15053995: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001110957
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 30107988: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.001110957
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 30107989: 25 observations,    complexity param=0.0004716981
##   predicted class=N  expected loss=0.36  P(node) =0.00347174
##     class counts:    16     9
##    probabilities: 0.640 0.360 
##   left son=60215978 (17 obs) right son=60215979 (8 obs)
##   Primary splits:
##       timbre_6_min < -98.4525  to the right, improve=1.6523530, (0 missing)
##       timbre_4_max < 154.3515  to the right, improve=0.3511688, (0 missing)
## 
## Node number 60215978: 17 observations
##   predicted class=N  expected loss=0.2352941  P(node) =0.002360783
##     class counts:    13     4
##    probabilities: 0.765 0.235 
## 
## Node number 60215979: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001110957
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## n= 7201 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##        1) root 7201 1060 N (0.85279822 0.14720178)  
##          2) timbre_6_min>=-86.184 4382  411 N (0.90620721 0.09379279)  
##            4) timbre_4_max< 87.318 1731   67 N (0.96129405 0.03870595) *
##            5) timbre_4_max>=87.318 2651  344 N (0.87023765 0.12976235)  
##             10) timbre_6_min>=-64.667 701   43 N (0.93865906 0.06134094) *
##             11) timbre_6_min< -64.667 1950  301 N (0.84564103 0.15435897)  
##               22) timbre_4_max< 189.9985 1928  293 N (0.84802905 0.15197095)  
##                 44) timbre_6_min< -85.6325 51    2 N (0.96078431 0.03921569) *
##                 45) timbre_6_min>=-85.6325 1877  291 N (0.84496537 0.15503463)  
##                   90) timbre_6_min>=-85.353 1847  279 N (0.84894423 0.15105577)  
##                    180) timbre_6_min>=-78.8265 1175  165 N (0.85957447 0.14042553)  
##                      360) timbre_6_min< -65.368 1118  152 N (0.86404293 0.13595707)  
##                        720) timbre_6_min>=-65.843 38    1 N (0.97368421 0.02631579) *
##                        721) timbre_6_min< -65.843 1080  151 N (0.86018519 0.13981481)  
##                         1442) timbre_6_min< -68.5755 885  115 N (0.87005650 0.12994350)  
##                           2884) timbre_4_max< 119.0535 514   59 N (0.88521401 0.11478599) *
##                           2885) timbre_4_max>=119.0535 371   56 N (0.84905660 0.15094340)  
##                             5770) timbre_4_max>=119.9485 360   51 N (0.85833333 0.14166667)  
##                              11540) timbre_6_min< -78.103 35    0 N (1.00000000 0.00000000) *
##                              11541) timbre_6_min>=-78.103 325   51 N (0.84307692 0.15692308)  
##                                23082) timbre_6_min>=-77.8695 314   46 N (0.85350318 0.14649682)  
##                                  46164) timbre_6_min< -76.0415 76    7 N (0.90789474 0.09210526) *
##                                  46165) timbre_6_min>=-76.0415 238   39 N (0.83613445 0.16386555)  
##                                    92330) timbre_6_min>=-75.1125 204   29 N (0.85784314 0.14215686)  
##                                     184660) timbre_4_max< 120.8475 12    0 N (1.00000000 0.00000000) *
##                                     184661) timbre_4_max>=120.8475 192   29 N (0.84895833 0.15104167)  
##                                       369322) timbre_4_max>=121.6295 185   26 N (0.85945946 0.14054054)  
##                                         738644) timbre_4_max< 123.7945 16    0 N (1.00000000 0.00000000) *
##                                         738645) timbre_4_max>=123.7945 169   26 N (0.84615385 0.15384615)  
##                                          1477290) timbre_4_max>=124.9935 161   21 N (0.86956522 0.13043478) *
##                                          1477291) timbre_4_max< 124.9935 8    3 Y (0.37500000 0.62500000) *
##                                       369323) timbre_4_max< 121.6295 7    3 N (0.57142857 0.42857143) *
##                                    92331) timbre_6_min< -75.1125 34   10 N (0.70588235 0.29411765)  
##                                     184662) timbre_6_min< -75.681 13    2 N (0.84615385 0.15384615) *
##                                     184663) timbre_6_min>=-75.681 21    8 N (0.61904762 0.38095238)  
##                                       369326) timbre_6_min>=-75.4695 14    4 N (0.71428571 0.28571429) *
##                                       369327) timbre_6_min< -75.4695 7    3 Y (0.42857143 0.57142857) *
##                                23083) timbre_6_min< -77.8695 11    5 N (0.54545455 0.45454545) *
##                             5771) timbre_4_max< 119.9485 11    5 N (0.54545455 0.45454545) *
##                         1443) timbre_6_min>=-68.5755 195   36 N (0.81538462 0.18461538)  
##                           2886) timbre_6_min>=-68.378 182   29 N (0.84065934 0.15934066) *
##                           2887) timbre_6_min< -68.378 13    6 Y (0.46153846 0.53846154) *
##                      361) timbre_6_min>=-65.368 57   13 N (0.77192982 0.22807018)  
##                        722) timbre_6_min>=-65.235 45    6 N (0.86666667 0.13333333) *
##                        723) timbre_6_min< -65.235 12    5 Y (0.41666667 0.58333333) *
##                    181) timbre_6_min< -78.8265 672  114 N (0.83035714 0.16964286)  
##                      362) timbre_6_min< -79.48 614   97 N (0.84201954 0.15798046)  
##                        724) timbre_4_max>=95.1505 551   83 N (0.84936479 0.15063521)  
##                         1448) timbre_6_min< -81.6945 340   44 N (0.87058824 0.12941176) *
##                         1449) timbre_6_min>=-81.6945 211   39 N (0.81516588 0.18483412)  
##                           2898) timbre_6_min>=-80.7415 123   16 N (0.86991870 0.13008130) *
##                           2899) timbre_6_min< -80.7415 88   23 N (0.73863636 0.26136364)  
##                             5798) timbre_6_min< -80.8265 81   19 N (0.76543210 0.23456790)  
##                              11596) timbre_6_min>=-81.383 58   11 N (0.81034483 0.18965517)  
##                                23192) timbre_6_min< -81.14 24    1 N (0.95833333 0.04166667) *
##                                23193) timbre_6_min>=-81.14 34   10 N (0.70588235 0.29411765)  
##                                  46386) timbre_6_min>=-81.1045 27    5 N (0.81481481 0.18518519) *
##                                  46387) timbre_6_min< -81.1045 7    2 Y (0.28571429 0.71428571) *
##                              11597) timbre_6_min< -81.383 23    8 N (0.65217391 0.34782609)  
##                                23194) timbre_6_min< -81.452 16    4 N (0.75000000 0.25000000) *
##                                23195) timbre_6_min>=-81.452 7    3 Y (0.42857143 0.57142857) *
##                             5799) timbre_6_min>=-80.8265 7    3 Y (0.42857143 0.57142857) *
##                        725) timbre_4_max< 95.1505 63   14 N (0.77777778 0.22222222)  
##                         1450) timbre_4_max< 93.8785 51    6 N (0.88235294 0.11764706) *
##                         1451) timbre_4_max>=93.8785 12    4 Y (0.33333333 0.66666667) *
##                      363) timbre_6_min>=-79.48 58   17 N (0.70689655 0.29310345)  
##                        726) timbre_6_min>=-79.4275 45   10 N (0.77777778 0.22222222)  
##                         1452) timbre_6_min< -79.1945 24    3 N (0.87500000 0.12500000) *
##                         1453) timbre_6_min>=-79.1945 21    7 N (0.66666667 0.33333333)  
##                           2906) timbre_4_max>=120.2585 14    3 N (0.78571429 0.21428571) *
##                           2907) timbre_4_max< 120.2585 7    3 Y (0.42857143 0.57142857) *
##                        727) timbre_6_min< -79.4275 13    6 Y (0.46153846 0.53846154) *
##                   91) timbre_6_min< -85.353 30   12 N (0.60000000 0.40000000)  
##                    182) timbre_6_min< -85.494 19    5 N (0.73684211 0.26315789) *
##                    183) timbre_6_min>=-85.494 11    4 Y (0.36363636 0.63636364) *
##               23) timbre_4_max>=189.9985 22    8 N (0.63636364 0.36363636)  
##                 46) timbre_4_max>=204.9105 7    0 N (1.00000000 0.00000000) *
##                 47) timbre_4_max< 204.9105 15    7 Y (0.46666667 0.53333333) *
##          3) timbre_6_min< -86.184 2819  649 N (0.76977652 0.23022348)  
##            6) timbre_4_max< 109.353 898  132 N (0.85300668 0.14699332)  
##             12) timbre_4_max< 91.4875 366   35 N (0.90437158 0.09562842)  
##               24) timbre_4_max< 67.6285 92    3 N (0.96739130 0.03260870) *
##               25) timbre_4_max>=67.6285 274   32 N (0.88321168 0.11678832)  
##                 50) timbre_4_max>=71.4975 253   25 N (0.90118577 0.09881423)  
##                  100) timbre_6_min< -86.704 242   21 N (0.91322314 0.08677686)  
##                    200) timbre_6_min>=-96.956 138    7 N (0.94927536 0.05072464) *
##                    201) timbre_6_min< -96.956 104   14 N (0.86538462 0.13461538)  
##                      402) timbre_4_max>=88.1385 27    0 N (1.00000000 0.00000000) *
##                      403) timbre_4_max< 88.1385 77   14 N (0.81818182 0.18181818)  
##                        806) timbre_4_max< 84.6245 52    5 N (0.90384615 0.09615385) *
##                        807) timbre_4_max>=84.6245 25    9 N (0.64000000 0.36000000)  
##                         1614) timbre_6_min< -103.9 12    2 N (0.83333333 0.16666667) *
##                         1615) timbre_6_min>=-103.9 13    6 Y (0.46153846 0.53846154) *
##                  101) timbre_6_min>=-86.704 11    4 N (0.63636364 0.36363636) *
##                 51) timbre_4_max< 71.4975 21    7 N (0.66666667 0.33333333) *
##             13) timbre_4_max>=91.4875 532   97 N (0.81766917 0.18233083)  
##               26) timbre_4_max>=91.831 522   91 N (0.82567050 0.17432950)  
##                 52) timbre_6_min>=-110.0435 434   67 N (0.84562212 0.15437788)  
##                  104) timbre_4_max>=105.5845 98    7 N (0.92857143 0.07142857) *
##                  105) timbre_4_max< 105.5845 336   60 N (0.82142857 0.17857143)  
##                    210) timbre_4_max< 102.463 250   39 N (0.84400000 0.15600000) *
##                    211) timbre_4_max>=102.463 86   21 N (0.75581395 0.24418605)  
##                      422) timbre_6_min< -98.182 26    2 N (0.92307692 0.07692308) *
##                      423) timbre_6_min>=-98.182 60   19 N (0.68333333 0.31666667)  
##                        846) timbre_6_min>=-96.3905 52   13 N (0.75000000 0.25000000) *
##                        847) timbre_6_min< -96.3905 8    2 Y (0.25000000 0.75000000) *
##                 53) timbre_6_min< -110.0435 88   24 N (0.72727273 0.27272727)  
##                  106) timbre_4_max>=107.9985 9    0 N (1.00000000 0.00000000) *
##                  107) timbre_4_max< 107.9985 79   24 N (0.69620253 0.30379747)  
##                    214) timbre_4_max< 104.347 53   11 N (0.79245283 0.20754717) *
##                    215) timbre_4_max>=104.347 26   13 N (0.50000000 0.50000000)  
##                      430) timbre_6_min< -116.569 9    2 N (0.77777778 0.22222222) *
##                      431) timbre_6_min>=-116.569 17    6 Y (0.35294118 0.64705882) *
##               27) timbre_4_max< 91.831 10    4 Y (0.40000000 0.60000000) *
##            7) timbre_4_max>=109.353 1921  517 N (0.73086934 0.26913066)  
##             14) timbre_6_min>=-112.424 1522  380 N (0.75032852 0.24967148)  
##               28) timbre_4_max>=109.674 1514  375 N (0.75231176 0.24768824)  
##                 56) timbre_4_max< 139.04 895  201 N (0.77541899 0.22458101)  
##                  112) timbre_4_max>=137.6295 31    2 N (0.93548387 0.06451613) *
##                  113) timbre_4_max< 137.6295 864  199 N (0.76967593 0.23032407)  
##                    226) timbre_4_max< 121.5665 371   75 N (0.79784367 0.20215633)  
##                      452) timbre_4_max>=117.926 129   17 N (0.86821705 0.13178295) *
##                      453) timbre_4_max< 117.926 242   58 N (0.76033058 0.23966942)  
##                        906) timbre_4_max< 116.948 217   48 N (0.77880184 0.22119816)  
##                         1812) timbre_4_max>=115.6655 43    4 N (0.90697674 0.09302326) *
##                         1813) timbre_4_max< 115.6655 174   44 N (0.74712644 0.25287356)  
##                           3626) timbre_4_max< 113.117 81   15 N (0.81481481 0.18518519)  
##                             7252) timbre_6_min>=-89.441 11    0 N (1.00000000 0.00000000) *
##                             7253) timbre_6_min< -89.441 70   15 N (0.78571429 0.21428571)  
##                              14506) timbre_6_min< -93.8255 46    7 N (0.84782609 0.15217391) *
##                              14507) timbre_6_min>=-93.8255 24    8 N (0.66666667 0.33333333)  
##                                29014) timbre_4_max>=111.553 17    4 N (0.76470588 0.23529412) *
##                                29015) timbre_4_max< 111.553 7    3 Y (0.42857143 0.57142857) *
##                           3627) timbre_4_max>=113.117 93   29 N (0.68817204 0.31182796)  
##                             7254) timbre_6_min>=-102.816 66   17 N (0.74242424 0.25757576)  
##                              14508) timbre_6_min< -95.7075 26    3 N (0.88461538 0.11538462) *
##                              14509) timbre_6_min>=-95.7075 40   14 N (0.65000000 0.35000000)  
##                                29018) timbre_4_max>=113.5145 30    8 N (0.73333333 0.26666667) *
##                                29019) timbre_4_max< 113.5145 10    4 Y (0.40000000 0.60000000) *
##                             7255) timbre_6_min< -102.816 27   12 N (0.55555556 0.44444444)  
##                              14510) timbre_6_min< -105.5865 17    5 N (0.70588235 0.29411765) *
##                              14511) timbre_6_min>=-105.5865 10    3 Y (0.30000000 0.70000000) *
##                        907) timbre_4_max>=116.948 25   10 N (0.60000000 0.40000000)  
##                         1814) timbre_6_min< -97.937 11    2 N (0.81818182 0.18181818) *
##                         1815) timbre_6_min>=-97.937 14    6 Y (0.42857143 0.57142857) *
##                    227) timbre_4_max>=121.5665 493  124 N (0.74847870 0.25152130)  
##                      454) timbre_4_max>=125.184 391   91 N (0.76726343 0.23273657)  
##                        908) timbre_4_max< 125.5545 18    1 N (0.94444444 0.05555556) *
##                        909) timbre_4_max>=125.5545 373   90 N (0.75871314 0.24128686)  
##                         1818) timbre_4_max>=125.7355 366   86 N (0.76502732 0.23497268)  
##                           3636) timbre_4_max< 126.312 14    0 N (1.00000000 0.00000000) *
##                           3637) timbre_4_max>=126.312 352   86 N (0.75568182 0.24431818)  
##                             7274) timbre_6_min>=-102.915 254   56 N (0.77952756 0.22047244)  
##                              14548) timbre_4_max< 134.869 195   37 N (0.81025641 0.18974359)  
##                                29096) timbre_4_max>=133.052 47    4 N (0.91489362 0.08510638) *
##                                29097) timbre_4_max< 133.052 148   33 N (0.77702703 0.22297297)  
##                                  58194) timbre_4_max< 129.356 73   11 N (0.84931507 0.15068493) *
##                                  58195) timbre_4_max>=129.356 75   22 N (0.70666667 0.29333333)  
##                                   116390) timbre_4_max>=129.608 67   16 N (0.76119403 0.23880597)  
##                                     232780) timbre_6_min< -95.8915 22    1 N (0.95454545 0.04545455) *
##                                     232781) timbre_6_min>=-95.8915 45   15 N (0.66666667 0.33333333)  
##                                       465562) timbre_4_max< 132.4285 38   10 N (0.73684211 0.26315789)  
##                                         931124) timbre_4_max< 130.831 12    1 N (0.91666667 0.08333333) *
##                                         931125) timbre_4_max>=130.831 26    9 N (0.65384615 0.34615385)  
##                                          1862250) timbre_4_max>=131.897 13    2 N (0.84615385 0.15384615) *
##                                          1862251) timbre_4_max< 131.897 13    6 Y (0.46153846 0.53846154) *
##                                       465563) timbre_4_max>=132.4285 7    2 Y (0.28571429 0.71428571) *
##                                   116391) timbre_4_max< 129.608 8    2 Y (0.25000000 0.75000000) *
##                              14549) timbre_4_max>=134.869 59   19 N (0.67796610 0.32203390)  
##                                29098) timbre_6_min>=-93.3205 28    6 N (0.78571429 0.21428571) *
##                                29099) timbre_6_min< -93.3205 31   13 N (0.58064516 0.41935484)  
##                                  58198) timbre_4_max>=135.773 19    6 N (0.68421053 0.31578947) *
##                                  58199) timbre_4_max< 135.773 12    5 Y (0.41666667 0.58333333) *
##                             7275) timbre_6_min< -102.915 98   30 N (0.69387755 0.30612245)  
##                              14550) timbre_4_max>=127.5305 87   24 N (0.72413793 0.27586207)  
##                                29100) timbre_6_min< -108.277 24    4 N (0.83333333 0.16666667) *
##                                29101) timbre_6_min>=-108.277 63   20 N (0.68253968 0.31746032)  
##                                  58202) timbre_4_max< 130.4925 18    3 N (0.83333333 0.16666667) *
##                                  58203) timbre_4_max>=130.4925 45   17 N (0.62222222 0.37777778)  
##                                   116406) timbre_4_max>=132.712 29    7 N (0.75862069 0.24137931) *
##                                   116407) timbre_4_max< 132.712 16    6 Y (0.37500000 0.62500000) *
##                              14551) timbre_4_max< 127.5305 11    5 Y (0.45454545 0.54545455) *
##                         1819) timbre_4_max< 125.7355 7    3 Y (0.42857143 0.57142857) *
##                      455) timbre_4_max< 125.184 102   33 N (0.67647059 0.32352941)  
##                        910) timbre_6_min< -104.638 19    3 N (0.84210526 0.15789474) *
##                        911) timbre_6_min>=-104.638 83   30 N (0.63855422 0.36144578)  
##                         1822) timbre_6_min>=-101.1935 72   23 N (0.68055556 0.31944444)  
##                           3644) timbre_4_max< 124.599 62   16 N (0.74193548 0.25806452)  
##                             7288) timbre_4_max>=123.629 21    3 N (0.85714286 0.14285714) *
##                             7289) timbre_4_max< 123.629 41   13 N (0.68292683 0.31707317)  
##                              14578) timbre_6_min< -96.115 18    4 N (0.77777778 0.22222222) *
##                              14579) timbre_6_min>=-96.115 23    9 N (0.60869565 0.39130435)  
##                                29158) timbre_6_min>=-93.072 14    3 N (0.78571429 0.21428571) *
##                                29159) timbre_6_min< -93.072 9    3 Y (0.33333333 0.66666667) *
##                           3645) timbre_4_max>=124.599 10    3 Y (0.30000000 0.70000000) *
##                         1823) timbre_6_min< -101.1935 11    4 Y (0.36363636 0.63636364) *
##                 57) timbre_4_max>=139.04 619  174 N (0.71890145 0.28109855)  
##                  114) timbre_6_min< -90.091 525  137 N (0.73904762 0.26095238)  
##                    228) timbre_6_min>=-91.2335 25    1 N (0.96000000 0.04000000) *
##                    229) timbre_6_min< -91.2335 500  136 N (0.72800000 0.27200000)  
##                      458) timbre_6_min< -111.831 8    0 N (1.00000000 0.00000000) *
##                      459) timbre_6_min>=-111.831 492  136 N (0.72357724 0.27642276)  
##                        918) timbre_6_min>=-110.8735 473  126 N (0.73361522 0.26638478)  
##                         1836) timbre_6_min< -110.4595 10    0 N (1.00000000 0.00000000) *
##                         1837) timbre_6_min>=-110.4595 463  126 N (0.72786177 0.27213823)  
##                           3674) timbre_4_max>=224.1235 8    0 N (1.00000000 0.00000000) *
##                           3675) timbre_4_max< 224.1235 455  126 N (0.72307692 0.27692308)  
##                             7350) timbre_6_min< -92.902 401  105 N (0.73815461 0.26184539)  
##                              14700) timbre_6_min>=-93.8975 22    2 N (0.90909091 0.09090909) *
##                              14701) timbre_6_min< -93.8975 379  103 N (0.72823219 0.27176781)  
##                                29402) timbre_4_max< 187.1575 357   94 N (0.73669468 0.26330532)  
##                                  58804) timbre_6_min< -96.643 288   71 N (0.75347222 0.24652778)  
##                                   117608) timbre_6_min>=-97.0515 9    0 N (1.00000000 0.00000000) *
##                                   117609) timbre_6_min< -97.0515 279   71 N (0.74551971 0.25448029)  
##                                     235218) timbre_6_min< -97.331 272   67 N (0.75367647 0.24632353)  
##                                       470436) timbre_6_min>=-97.639 9    0 N (1.00000000 0.00000000) *
##                                       470437) timbre_6_min< -97.639 263   67 N (0.74524715 0.25475285)  
##                                         940874) timbre_6_min>=-109.2095 243   59 N (0.75720165 0.24279835)  
##                                          1881748) timbre_6_min< -107.1325 38    5 N (0.86842105 0.13157895) *
##                                          1881749) timbre_6_min>=-107.1325 205   54 N (0.73658537 0.26341463)  
##                                            3763498) timbre_6_min>=-106.1775 182   43 N (0.76373626 0.23626374)  
##                                              7526996) timbre_6_min< -99.4305 141   29 N (0.79432624 0.20567376) *
##                                              7526997) timbre_6_min>=-99.4305 41   14 N (0.65853659 0.34146341)  
##                                               15053994) timbre_6_min>=-99.171 33    9 N (0.72727273 0.27272727)  
##                                                 30107988) timbre_6_min< -98.8505 8    0 N (1.00000000 0.00000000) *
##                                                 30107989) timbre_6_min>=-98.8505 25    9 N (0.64000000 0.36000000)  
##                                                   60215978) timbre_6_min>=-98.4525 17    4 N (0.76470588 0.23529412) *
##                                                   60215979) timbre_6_min< -98.4525 8    3 Y (0.37500000 0.62500000) *
##                                               15053995) timbre_6_min< -99.171 8    3 Y (0.37500000 0.62500000) *
##                                            3763499) timbre_6_min< -106.1775 23   11 N (0.52173913 0.47826087)  
##                                              7526998) timbre_4_max>=153.3585 10    3 N (0.70000000 0.30000000) *
##                                              7526999) timbre_4_max< 153.3585 13    5 Y (0.38461538 0.61538462) *
##                                         940875) timbre_6_min< -109.2095 20    8 N (0.60000000 0.40000000)  
##                                          1881750) timbre_4_max< 149.5945 9    2 N (0.77777778 0.22222222) *
##                                          1881751) timbre_4_max>=149.5945 11    5 Y (0.45454545 0.54545455) *
##                                     235219) timbre_6_min>=-97.331 7    3 Y (0.42857143 0.57142857) *
##                                  58805) timbre_6_min>=-96.643 69   23 N (0.66666667 0.33333333)  
##                                   117610) timbre_6_min>=-96.346 55   13 N (0.76363636 0.23636364) *
##                                   117611) timbre_6_min< -96.346 14    4 Y (0.28571429 0.71428571) *
##                                29403) timbre_4_max>=187.1575 22    9 N (0.59090909 0.40909091)  
##                                  58806) timbre_4_max>=195.063 15    4 N (0.73333333 0.26666667) *
##                                  58807) timbre_4_max< 195.063 7    2 Y (0.28571429 0.71428571) *
##                             7351) timbre_6_min>=-92.902 54   21 N (0.61111111 0.38888889)  
##                              14702) timbre_4_max>=141.9325 44   14 N (0.68181818 0.31818182)  
##                                29404) timbre_4_max< 150.4675 15    1 N (0.93333333 0.06666667) *
##                                29405) timbre_4_max>=150.4675 29   13 N (0.55172414 0.44827586)  
##                                  58810) timbre_4_max>=174.053 8    1 N (0.87500000 0.12500000) *
##                                  58811) timbre_4_max< 174.053 21    9 Y (0.42857143 0.57142857)  
##                                   117622) timbre_6_min>=-92.31 12    5 N (0.58333333 0.41666667) *
##                                   117623) timbre_6_min< -92.31 9    2 Y (0.22222222 0.77777778) *
##                              14703) timbre_4_max< 141.9325 10    3 Y (0.30000000 0.70000000) *
##                        919) timbre_6_min< -110.8735 19    9 Y (0.47368421 0.52631579) *
##                  115) timbre_6_min>=-90.091 94   37 N (0.60638298 0.39361702)  
##                    230) timbre_6_min>=-89.0365 62   19 N (0.69354839 0.30645161)  
##                      460) timbre_4_max>=142.605 55   14 N (0.74545455 0.25454545)  
##                        920) timbre_6_min< -88.4635 14    1 N (0.92857143 0.07142857) *
##                        921) timbre_6_min>=-88.4635 41   13 N (0.68292683 0.31707317)  
##                         1842) timbre_4_max< 147.1105 10    1 N (0.90000000 0.10000000) *
##                         1843) timbre_4_max>=147.1105 31   12 N (0.61290323 0.38709677)  
##                           3686) timbre_6_min>=-87.8225 23    7 N (0.69565217 0.30434783)  
##                             7372) timbre_4_max>=151.452 14    2 N (0.85714286 0.14285714) *
##                             7373) timbre_4_max< 151.452 9    4 Y (0.44444444 0.55555556) *
##                           3687) timbre_6_min< -87.8225 8    3 Y (0.37500000 0.62500000) *
##                      461) timbre_4_max< 142.605 7    2 Y (0.28571429 0.71428571) *
##                    231) timbre_6_min< -89.0365 32   14 Y (0.43750000 0.56250000)  
##                      462) timbre_4_max< 162.371 20    9 N (0.55000000 0.45000000)  
##                        924) timbre_4_max>=147.704 7    2 N (0.71428571 0.28571429) *
##                        925) timbre_4_max< 147.704 13    6 Y (0.46153846 0.53846154) *
##                      463) timbre_4_max>=162.371 12    3 Y (0.25000000 0.75000000) *
##               29) timbre_4_max< 109.674 8    3 Y (0.37500000 0.62500000) *
##             15) timbre_6_min< -112.424 399  137 N (0.65664160 0.34335840)  
##               30) timbre_6_min< -112.758 385  129 N (0.66493506 0.33506494)  
##                 60) timbre_4_max< 135.5785 179   51 N (0.71508380 0.28491620)  
##                  120) timbre_4_max>=134.8965 9    0 N (1.00000000 0.00000000) *
##                  121) timbre_4_max< 134.8965 170   51 N (0.70000000 0.30000000)  
##                    242) timbre_6_min< -117.385 97   24 N (0.75257732 0.24742268)  
##                      484) timbre_6_min>=-130.7685 75   15 N (0.80000000 0.20000000)  
##                        968) timbre_6_min>=-119.76 17    1 N (0.94117647 0.05882353) *
##                        969) timbre_6_min< -119.76 58   14 N (0.75862069 0.24137931)  
##                         1938) timbre_6_min< -124.752 30    4 N (0.86666667 0.13333333) *
##                         1939) timbre_6_min>=-124.752 28   10 N (0.64285714 0.35714286)  
##                           3878) timbre_4_max>=117.6805 21    6 N (0.71428571 0.28571429)  
##                             7756) timbre_6_min>=-122.317 12    1 N (0.91666667 0.08333333) *
##                             7757) timbre_6_min< -122.317 9    4 Y (0.44444444 0.55555556) *
##                           3879) timbre_4_max< 117.6805 7    3 Y (0.42857143 0.57142857) *
##                      485) timbre_6_min< -130.7685 22    9 N (0.59090909 0.40909091)  
##                        970) timbre_4_max< 126.721 15    4 N (0.73333333 0.26666667) *
##                        971) timbre_4_max>=126.721 7    2 Y (0.28571429 0.71428571) *
##                    243) timbre_6_min>=-117.385 73   27 N (0.63013699 0.36986301)  
##                      486) timbre_6_min>=-114.1775 25    6 N (0.76000000 0.24000000) *
##                      487) timbre_6_min< -114.1775 48   21 N (0.56250000 0.43750000)  
##                        974) timbre_4_max>=121.1635 28    9 N (0.67857143 0.32142857)  
##                         1948) timbre_4_max< 130.9325 20    4 N (0.80000000 0.20000000) *
##                         1949) timbre_4_max>=130.9325 8    3 Y (0.37500000 0.62500000) *
##                        975) timbre_4_max< 121.1635 20    8 Y (0.40000000 0.60000000)  
##                         1950) timbre_6_min< -116.3835 8    3 N (0.62500000 0.37500000) *
##                         1951) timbre_6_min>=-116.3835 12    3 Y (0.25000000 0.75000000) *
##                 61) timbre_4_max>=135.5785 206   78 N (0.62135922 0.37864078)  
##                  122) timbre_4_max>=137.1985 198   72 N (0.63636364 0.36363636)  
##                    244) timbre_6_min< -134.7425 23    5 N (0.78260870 0.21739130) *
##                    245) timbre_6_min>=-134.7425 175   67 N (0.61714286 0.38285714)  
##                      490) timbre_6_min>=-120.7485 87   27 N (0.68965517 0.31034483)  
##                        980) timbre_4_max< 167.48 65   17 N (0.73846154 0.26153846)  
##                         1960) timbre_4_max>=158.734 16    1 N (0.93750000 0.06250000) *
##                         1961) timbre_4_max< 158.734 49   16 N (0.67346939 0.32653061)  
##                           3922) timbre_4_max< 157.723 42   12 N (0.71428571 0.28571429) *
##                           3923) timbre_4_max>=157.723 7    3 Y (0.42857143 0.57142857) *
##                        981) timbre_4_max>=167.48 22   10 N (0.54545455 0.45454545)  
##                         1962) timbre_4_max>=180.2065 11    3 N (0.72727273 0.27272727) *
##                         1963) timbre_4_max< 180.2065 11    4 Y (0.36363636 0.63636364) *
##                      491) timbre_6_min< -120.7485 88   40 N (0.54545455 0.45454545)  
##                        982) timbre_6_min< -125.0005 53   21 N (0.60377358 0.39622642)  
##                         1964) timbre_6_min>=-130.002 32    9 N (0.71875000 0.28125000)  
##                           3928) timbre_4_max>=146.904 25    5 N (0.80000000 0.20000000) *
##                           3929) timbre_4_max< 146.904 7    3 Y (0.42857143 0.57142857) *
##                         1965) timbre_6_min< -130.002 21    9 Y (0.42857143 0.57142857)  
##                           3930) timbre_4_max>=152.7635 14    6 N (0.57142857 0.42857143) *
##                           3931) timbre_4_max< 152.7635 7    1 Y (0.14285714 0.85714286) *
##                        983) timbre_6_min>=-125.0005 35   16 Y (0.45714286 0.54285714)  
##                         1966) timbre_6_min>=-123.566 24   10 N (0.58333333 0.41666667)  
##                           3932) timbre_6_min< -121.989 12    3 N (0.75000000 0.25000000) *
##                           3933) timbre_6_min>=-121.989 12    5 Y (0.41666667 0.58333333) *
##                         1967) timbre_6_min< -123.566 11    2 Y (0.18181818 0.81818182) *
##                  123) timbre_4_max< 137.1985 8    2 Y (0.25000000 0.75000000) *
##               31) timbre_6_min>=-112.758 14    6 Y (0.42857143 0.57142857) *
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_0-7.png) 

```
##    threshold    f.score
## 1        0.0 0.25662753
## 2        0.1 0.37657961
## 3        0.2 0.47030879
## 4        0.3 0.47032222
## 5        0.4 0.46315789
## 6        0.5 0.44954683
## 7        0.6 0.31837916
## 8        0.7 0.15709459
## 9        0.8 0.02782931
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1          N                                           5848
## 2          Y                                            644
##   Top10.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            293
## 2                                            416
##          Prediction
## Reference    N    Y
##         N 5848  293
##         Y  644  416
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.698792e-01   3.994621e-01   8.618892e-01   8.775683e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   1.745099e-05   2.827705e-30 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_0-9.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.33207547
## 3        0.2 0.27272727
## 4        0.3 0.13043478
## 5        0.4 0.13333333
## 6        0.5 0.14117647
## 7        0.6 0.08333333
## 8        0.7 0.02985075
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1          N                                            152
## 2          Y                                             15
##   Top10.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            162
## 2                                             44
##          Prediction
## Reference   N   Y
##         N 152 162
##         Y  15  44
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.254692e-01   1.142520e-01   4.734236e-01   5.771074e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   5.094861e-28 
##                    model_id model_method                      feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart timbre_6_min, timbre_4_max
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.559                 0.113
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7925561                    0.3       0.4703222        0.8698792
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8618892             0.8775683     0.3994621   0.6506801
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.3320755        0.5254692
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.4734236             0.5771074      0.114252
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: timbre_6_min, timbre_4_max"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00108 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](songs_template2_files/figure-html/fit.models_0-11.png) ![](songs_template2_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 7201 
## 
##            CP nsplit rel error
## 1 0.001078167      0         1
## 
## Node number 1: 7201 observations
##   predicted class=N  expected loss=0.1472018  P(node) =1
##     class counts:  6141  1060
##    probabilities: 0.853 0.147 
## 
## n= 7201 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 7201 1060 N (0.8527982 0.1472018) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.Max.cor.Y.rpart.N
## 1          N                                 6141
## 2          Y                                 1060
##          Prediction
## Reference    N    Y
##         N 6141    0
##         Y 1060    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.527982e-01   0.000000e+00   8.444027e-01   8.609070e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   5.081939e-01  4.439170e-232 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.Max.cor.Y.rpart.N
## 1          N                                  314
## 2          Y                                   59
##          Prediction
## Reference   N   Y
##         N 314   0
##         Y  59   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.418231e-01   0.000000e+00   8.007587e-01   8.773551e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   5.346786e-01   4.320720e-14 
##          model_id model_method                      feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart timbre_6_min, timbre_4_max               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.685                 0.126         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8328024
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8444027              0.860907    0.04053746         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8418231
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8007587             0.8773551             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.00978916      0.02058608
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.glm"
## [1] "    indep_vars: timbre_6_min, timbre_4_max"
## Aggregating results
## Fitting final model on full training set
```

![](songs_template2_files/figure-html/fit.models_0-13.png) ![](songs_template2_files/figure-html/fit.models_0-14.png) ![](songs_template2_files/figure-html/fit.models_0-15.png) ![](songs_template2_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4102  -0.6047  -0.4623  -0.3333   2.6044  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -4.999029   0.175097 -28.550   <2e-16 ***
## timbre_6_min -0.023036   0.001835 -12.551   <2e-16 ***
## timbre_4_max  0.011122   0.001125   9.891   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 5580.7  on 7198  degrees of freedom
## AIC: 5586.7
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_0-17.png) 

```
##    threshold    f.score
## 1        0.0 0.25662753
## 2        0.1 0.32419929
## 3        0.2 0.33236785
## 4        0.3 0.19446233
## 5        0.4 0.07582140
## 6        0.5 0.01109057
## 7        0.6 0.00000000
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_0-18.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.Max.cor.Y.glm.N
## 1          N                               4898
## 2          Y                                601
##   Top10.fctr.predict.Max.cor.Y.glm.Y
## 1                               1243
## 2                                459
##          Prediction
## Reference    N    Y
##         N 4898 1243
##         Y  601  459
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.439245e-01   1.844047e-01   7.336766e-01   7.539745e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.193705e-50 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_0-19.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.33121019
## 3        0.2 0.33112583
## 4        0.3 0.27906977
## 5        0.4 0.06557377
## 6        0.5 0.00000000
## 7        0.6 0.00000000
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_0-20.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.Max.cor.Y.glm.N
## 1          N                                111
## 2          Y                                  7
##   Top10.fctr.predict.Max.cor.Y.glm.Y
## 1                                203
## 2                                 52
##          Prediction
## Reference   N   Y
##         N 111 203
##         Y   7  52
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   4.369973e-01   9.998621e-02   3.859956e-01   4.890069e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.828054e-41 
##        model_id model_method                      feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm timbre_6_min, timbre_4_max               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.322                 0.129   0.6982092
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.3323678        0.8514097
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7336766             0.7539745   0.005126623   0.7064126
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.3312102        0.4369973
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.3859956             0.4890069    0.09998621    5586.714
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.001019932     0.008109304
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.glm"
## [1] "    indep_vars: timbre_6_min, timbre_4_max, timbre_6_min:timbre_0_max"
## Aggregating results
## Fitting final model on full training set
```

![](songs_template2_files/figure-html/fit.models_0-21.png) ![](songs_template2_files/figure-html/fit.models_0-22.png) ![](songs_template2_files/figure-html/fit.models_0-23.png) ![](songs_template2_files/figure-html/fit.models_0-24.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2516  -0.5873  -0.4454  -0.3202   2.6533  
## 
## Coefficients:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                 -4.9234298  0.1771093  -27.80   <2e-16 ***
## timbre_6_min                -0.0720751  0.0048023  -15.01   <2e-16 ***
## timbre_4_max                 0.0119132  0.0011413   10.44   <2e-16 ***
## `timbre_6_min:timbre_0_max`  0.0009463  0.0000858   11.03   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 5461.3  on 7197  degrees of freedom
## AIC: 5469.3
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_0-25.png) 

```
##    threshold     f.score
## 1        0.0 0.256627527
## 2        0.1 0.336928226
## 3        0.2 0.355390335
## 4        0.3 0.257401813
## 5        0.4 0.148262548
## 6        0.5 0.076388889
## 7        0.6 0.041780200
## 8        0.7 0.009319664
## 9        0.8 0.001881468
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](songs_template2_files/figure-html/fit.models_0-26.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.Interact.High.cor.Y.glm.N
## 1          N                                         4989
## 2          Y                                          582
##   Top10.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                         1152
## 2                                          478
##          Prediction
## Reference    N    Y
##         N 4989 1152
##         Y  582  478
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.592001e-01   2.154280e-01   7.491514e-01   7.690385e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.658676e-42 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_0-27.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.35738832
## 3        0.2 0.38518519
## 4        0.3 0.27848101
## 5        0.4 0.12500000
## 6        0.5 0.06557377
## 7        0.6 0.03333333
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.Interact.High.cor.Y.glm.N
## 1          N                                          264
## 2          Y                                           33
##   Top10.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                           50
## 2                                           26
##          Prediction
## Reference   N   Y
##         N 264  50
##         Y  33  26
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.77747989     0.25196318     0.73180986     0.81868312     0.84182306 
## AccuracyPValue  McnemarPValue 
##     0.99955937     0.07904946 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                   feats max.nTuningRuns
## 1 timbre_6_min, timbre_4_max, timbre_6_min:timbre_0_max               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.447                 0.143   0.7238499
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.3553903        0.8525202
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7491514             0.7690385    0.05800737   0.7395552
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3851852        0.7774799
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7318099             0.8186831     0.2519632    5469.265
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.001478443      0.01759949
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.glm"
## [1] "    indep_vars: timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min"
## Aggregating results
## Fitting final model on full training set
```

![](songs_template2_files/figure-html/fit.models_0-29.png) ![](songs_template2_files/figure-html/fit.models_0-30.png) ![](songs_template2_files/figure-html/fit.models_0-31.png) ![](songs_template2_files/figure-html/fit.models_0-32.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1030  -0.5591  -0.3601  -0.1900   3.3191  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -2.239e+00  7.466e-01  -2.999 0.002708 ** 
## timbre_4_max              6.324e-03  1.533e-03   4.125 3.70e-05 ***
## timbre_8_max              6.413e-03  2.950e-03   2.174 0.029707 *  
## timbre_9_max              3.516e-03  2.377e-03   1.479 0.139078    
## timbre_11_max             1.823e-02  3.342e-03   5.454 4.92e-08 ***
## timbre_6_max              3.849e-03  2.157e-03   1.785 0.074294 .  
## timbre_1_max             -8.007e-04  7.068e-04  -1.133 0.257241    
## timbre_10_max             7.397e-03  1.731e-03   4.272 1.94e-05 ***
## timbre_2_max              4.088e-04  8.967e-04   0.456 0.648456    
## tempo_confidence          5.491e-01  1.407e-01   3.901 9.57e-05 ***
## timbre_5_max              7.019e-04  7.808e-04   0.899 0.368666    
## timbre_0_min              2.475e-02  4.241e-03   5.836 5.35e-09 ***
## timesignature_confidence  6.879e-01  1.923e-01   3.576 0.000348 ***
## timesignature             1.617e-01  8.739e-02   1.850 0.064313 .  
## timbre_7_max             -3.118e-03  1.811e-03  -1.722 0.085134 .  
## key                       1.736e-02  1.026e-02   1.693 0.090518 .  
## .rnorm                    3.092e-02  3.757e-02   0.823 0.410421    
## key_confidence            2.949e-01  1.395e-01   2.115 0.034469 *  
## timbre_10_min             2.963e-03  1.804e-03   1.642 0.100564    
## timbre_1_min              7.136e-03  7.711e-04   9.255  < 2e-16 ***
## tempo                     5.717e-04  1.665e-03   0.343 0.731358    
## timbre_4_min              9.084e-03  1.953e-03   4.652 3.28e-06 ***
## timbre_3_min              6.512e-04  5.949e-04   1.095 0.273713    
## timbre_3_max             -2.469e-03  5.677e-04  -4.350 1.36e-05 ***
## timbre_9_min             -3.823e-04  2.956e-03  -0.129 0.897106    
## timbre_8_min              4.462e-03  2.810e-03   1.588 0.112321    
## energy                    1.830e-01  2.608e-01   0.701 0.482994    
## timbre_5_min             -5.646e-03  1.255e-03  -4.498 6.86e-06 ***
## pitch                    -5.146e+01  6.855e+00  -7.507 6.06e-14 ***
## timbre_2_min             -1.564e-03  1.109e-03  -1.410 0.158465    
## timbre_0_max             -1.008e-01  1.178e-02  -8.557  < 2e-16 ***
## timbre_7_min             -5.082e-03  1.755e-03  -2.896 0.003781 ** 
## timbre_11_min            -2.836e-02  3.631e-03  -7.811 5.65e-15 ***
## timbre_6_min             -1.612e-02  2.235e-03  -7.213 5.49e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4871.2  on 7167  degrees of freedom
## AIC: 4939.2
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_0-33.png) 

```
##    threshold     f.score
## 1        0.0 0.256627527
## 2        0.1 0.392010536
## 3        0.2 0.451410658
## 4        0.3 0.446480231
## 5        0.4 0.355500311
## 6        0.5 0.258684405
## 7        0.6 0.160000000
## 8        0.7 0.073542601
## 9        0.8 0.022367195
## 10       0.9 0.005644403
## 11       1.0 0.000000000
```

![](songs_template2_files/figure-html/fit.models_0-34.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.Low.cor.X.glm.N
## 1          N                               4978
## 2          Y                                412
##   Top10.fctr.predict.Low.cor.X.glm.Y
## 1                               1163
## 2                                648
##          Prediction
## Reference    N    Y
##         N 4978 1163
##         Y  412  648
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.812804e-01   3.262998e-01   7.715504e-01   7.907822e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.179575e-79 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_0-35.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.45414847
## 3        0.2 0.50649351
## 4        0.3 0.49056604
## 5        0.4 0.44186047
## 6        0.5 0.30555556
## 7        0.6 0.21212121
## 8        0.7 0.12698413
## 9        0.8 0.03333333
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.Low.cor.X.glm.N
## 1          N                                258
## 2          Y                                 20
##   Top10.fctr.predict.Low.cor.X.glm.Y
## 1                                 56
## 2                                 39
##          Prediction
## Reference   N   Y
##         N 258  56
##         Y  20  39
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.962466e-01   3.868316e-01   7.517471e-01   8.359713e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   9.920270e-01   5.950244e-05 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                              feats
## 1 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.727                 0.863
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8004704                    0.2       0.4514107        0.8604359
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7715504             0.7907822     0.2083714    0.841574
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.5064935        0.7962466
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7517471             0.8359713     0.3868316    4939.171
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0008623889     0.005280039
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 35.689 85.439   49.75
## 11 fit.models          7          1 85.439     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 96.739  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 96.739 96.754   0.015
## 2 fit.models_1_glm          2          0 96.754     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min"
## Aggregating results
## Fitting final model on full training set
```

![](songs_template2_files/figure-html/fit.models_1-1.png) ![](songs_template2_files/figure-html/fit.models_1-2.png) ![](songs_template2_files/figure-html/fit.models_1-3.png) ![](songs_template2_files/figure-html/fit.models_1-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9200  -0.5392  -0.3454  -0.1859   3.0866  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.471e+01  1.807e+00   8.144 3.82e-16 ***
## timbre_4_max              6.134e-03  1.551e-03   3.955 7.65e-05 ***
## timbre_8_max              4.000e-03  3.003e-03   1.332 0.182808    
## timbre_9_max              1.582e-03  2.434e-03   0.650 0.515675    
## timbre_11_max             1.960e-02  3.387e-03   5.787 7.16e-09 ***
## timbre_6_max              3.696e-03  2.190e-03   1.688 0.091377 .  
## timbre_1_max             -2.629e-04  7.156e-04  -0.367 0.713392    
## timbre_10_max             5.857e-03  1.770e-03   3.310 0.000934 ***
## timbre_2_max              6.782e-04  9.068e-04   0.748 0.454477    
## tempo_confidence          4.726e-01  1.422e-01   3.324 0.000887 ***
## timbre_5_max              8.640e-05  7.936e-04   0.109 0.913299    
## timbre_0_min              2.310e-02  4.258e-03   5.425 5.79e-08 ***
## timesignature_confidence  7.445e-01  1.952e-01   3.814 0.000137 ***
## timesignature             1.258e-01  8.678e-02   1.450 0.147119    
## timbre_7_max             -3.729e-03  1.833e-03  -2.035 0.041834 *  
## key                       1.587e-02  1.039e-02   1.527 0.126677    
## .rnorm                    3.377e-02  3.808e-02   0.887 0.375170    
## key_confidence            3.081e-01  1.412e-01   2.183 0.029048 *  
## timbre_10_min             4.093e-03  1.839e-03   2.225 0.026075 *  
## timbre_1_min              5.872e-03  7.799e-04   7.529 5.13e-14 ***
## tempo                     3.876e-04  1.692e-03   0.229 0.818807    
## timbre_4_min              1.036e-02  1.986e-03   5.217 1.82e-07 ***
## timbre_3_min              6.929e-04  5.985e-04   1.158 0.246945    
## timbre_3_max             -2.975e-03  5.818e-04  -5.114 3.15e-07 ***
## loudness                  3.001e-01  2.918e-02  10.287  < 2e-16 ***
## timbre_9_min              1.413e-03  2.999e-03   0.471 0.637501    
## timbre_8_min              3.880e-03  2.851e-03   1.361 0.173553    
## energy                   -1.501e+00  3.100e-01  -4.843 1.28e-06 ***
## timbre_5_min             -5.603e-03  1.277e-03  -4.388 1.15e-05 ***
## pitch                    -4.485e+01  6.833e+00  -6.564 5.24e-11 ***
## timbre_2_min             -2.109e-03  1.126e-03  -1.872 0.061152 .  
## timbre_0_max             -3.313e-01  2.570e-02 -12.890  < 2e-16 ***
## timbre_7_min             -4.533e-03  1.782e-03  -2.545 0.010942 *  
## timbre_11_min            -2.625e-02  3.694e-03  -7.105 1.20e-12 ***
## timbre_6_min             -1.685e-02  2.264e-03  -7.444 9.77e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4758.4  on 7166  degrees of freedom
## AIC: 4828.4
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_1-5.png) 

```
##    threshold    f.score
## 1        0.0 0.25662753
## 2        0.1 0.40895591
## 3        0.2 0.46669017
## 4        0.3 0.45406949
## 5        0.4 0.39522388
## 6        0.5 0.28981349
## 7        0.6 0.19614148
## 8        0.7 0.12780656
## 9        0.8 0.05469462
## 10       0.9 0.01683817
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_1-6.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.All.X.glm.N Top10.fctr.predict.All.X.glm.Y
## 1          N                           5026                           1115
## 2          Y                            398                            662
##          Prediction
## Reference    N    Y
##         N 5026 1115
##         Y  398  662
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.898903e-01   3.461100e-01   7.802951e-01   7.992502e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.144941e-75 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_1-7.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.42975207
## 3        0.2 0.52229299
## 4        0.3 0.47706422
## 5        0.4 0.44705882
## 6        0.5 0.32432432
## 7        0.6 0.23880597
## 8        0.7 0.15625000
## 9        0.8 0.03333333
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.All.X.glm.N Top10.fctr.predict.All.X.glm.Y
## 1          N                            257                             57
## 2          Y                             18                             41
##          Prediction
## Reference   N   Y
##         N 257  57
##         Y  18  41
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.989276e-01   4.047492e-01   7.546059e-01   8.384302e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   9.886242e-01   1.144703e-05 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      5.848                 0.886
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8139835                    0.2       0.4666902         0.860436
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7802951             0.7992502     0.2286063   0.8421138
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2        0.522293        0.7989276
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7546059             0.8384302     0.4047492    4828.367
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.001667005      0.01770052
##                   label step_major step_minor     bgn     end elapsed
## 2      fit.models_1_glm          2          0  96.754 109.988  13.234
## 3 fit.models_1_bayesglm          3          0 109.989      NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min"
```

```
## Loading required package: arm
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: Matrix
## Loading required package: lme4
## 
## arm (Version 1.8-5, built: 2015-05-13)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Assignments/HW3_Billboard_Songs
```

![](songs_template2_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9147  -0.5393  -0.3464  -0.1866   3.0803  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.439e+01  1.790e+00   8.041 8.88e-16 ***
## timbre_4_max              6.128e-03  1.547e-03   3.963 7.41e-05 ***
## timbre_8_max              4.059e-03  2.996e-03   1.355 0.175449    
## timbre_9_max              1.625e-03  2.427e-03   0.670 0.502949    
## timbre_11_max             1.951e-02  3.380e-03   5.771 7.86e-09 ***
## timbre_6_max              3.710e-03  2.184e-03   1.698 0.089428 .  
## timbre_1_max             -2.664e-04  7.139e-04  -0.373 0.709069    
## timbre_10_max             5.864e-03  1.766e-03   3.321 0.000898 ***
## timbre_2_max              6.697e-04  9.045e-04   0.740 0.459054    
## tempo_confidence          4.733e-01  1.419e-01   3.336 0.000850 ***
## timbre_5_max              9.650e-05  7.916e-04   0.122 0.902972    
## timbre_0_min              2.309e-02  4.251e-03   5.430 5.62e-08 ***
## timesignature_confidence  7.408e-01  1.947e-01   3.805 0.000142 ***
## timesignature             1.262e-01  8.659e-02   1.457 0.145150    
## timbre_7_max             -3.718e-03  1.828e-03  -2.034 0.041942 *  
## key                       1.585e-02  1.037e-02   1.528 0.126484    
## .rnorm                    3.367e-02  3.802e-02   0.885 0.375896    
## key_confidence            3.067e-01  1.409e-01   2.177 0.029511 *  
## timbre_10_min             4.078e-03  1.833e-03   2.225 0.026071 *  
## timbre_1_min              5.860e-03  7.774e-04   7.538 4.78e-14 ***
## tempo                     3.900e-04  1.688e-03   0.231 0.817295    
## timbre_4_min              1.030e-02  1.981e-03   5.199 2.01e-07 ***
## timbre_3_min              6.921e-04  5.971e-04   1.159 0.246444    
## timbre_3_max             -2.954e-03  5.792e-04  -5.100 3.40e-07 ***
## loudness                  2.947e-01  2.887e-02  10.210  < 2e-16 ***
## timbre_9_min              1.399e-03  2.991e-03   0.468 0.639990    
## timbre_8_min              3.858e-03  2.844e-03   1.357 0.174909    
## energy                   -1.474e+00  3.081e-01  -4.783 1.73e-06 ***
## timbre_5_min             -5.583e-03  1.273e-03  -4.385 1.16e-05 ***
## pitch                    -4.456e+01  6.785e+00  -6.568 5.12e-11 ***
## timbre_2_min             -2.103e-03  1.123e-03  -1.874 0.060997 .  
## timbre_0_max             -3.267e-01  2.545e-02 -12.838  < 2e-16 ***
## timbre_7_min             -4.527e-03  1.777e-03  -2.548 0.010846 *  
## timbre_11_min            -2.622e-02  3.686e-03  -7.112 1.15e-12 ***
## timbre_6_min             -1.685e-02  2.262e-03  -7.450 9.34e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4758.4  on 7166  degrees of freedom
## AIC: 4828.4
## 
## Number of Fisher Scoring iterations: 7
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_1-9.png) 

```
##    threshold    f.score
## 1        0.0 0.25662753
## 2        0.1 0.40866591
## 3        0.2 0.46506704
## 4        0.3 0.45523810
## 5        0.4 0.39450090
## 6        0.5 0.28879310
## 7        0.6 0.19516129
## 8        0.7 0.12780656
## 9        0.8 0.05296804
## 10       0.9 0.01683817
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.All.X.bayesglm.N
## 1          N                                5026
## 2          Y                                 401
##   Top10.fctr.predict.All.X.bayesglm.Y
## 1                                1115
## 2                                 659
##          Prediction
## Reference    N    Y
##         N 5026 1115
##         Y  401  659
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.894737e-01   3.442135e-01   7.798718e-01   7.988406e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   6.617306e-75 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_1-11.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.42975207
## 3        0.2 0.52229299
## 4        0.3 0.47706422
## 5        0.4 0.44705882
## 6        0.5 0.30136986
## 7        0.6 0.23880597
## 8        0.7 0.15625000
## 9        0.8 0.03333333
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.All.X.bayesglm.N
## 1          N                                 257
## 2          Y                                  18
##   Top10.fctr.predict.All.X.bayesglm.Y
## 1                                  57
## 2                                  41
##          Prediction
## Reference   N   Y
##         N 257  57
##         Y  18  41
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.989276e-01   4.047492e-01   7.546059e-01   8.384302e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   9.886242e-01   1.144703e-05 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      4.142                 1.068
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8139566                    0.2        0.465067         0.860575
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7798718             0.7988406     0.2256497   0.8423297
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2        0.522293        0.7989276
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7546059             0.8384302     0.4047492    4828.409
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.001725688      0.01758228
##                   label step_major step_minor     bgn     end elapsed
## 3 fit.models_1_bayesglm          3          0 109.989 119.852   9.863
## 4    fit.models_1_rpart          4          0 119.853      NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0217 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](songs_template2_files/figure-html/fit.models_1-13.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 7201 
## 
##           CP nsplit rel error
## 1 0.02169811      0         1
## 
## Node number 1: 7201 observations
##   predicted class=N  expected loss=0.1472018  P(node) =1
##     class counts:  6141  1060
##    probabilities: 0.853 0.147 
## 
## n= 7201 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 7201 1060 N (0.8527982 0.1472018) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.All.X.no.rnorm.rpart.N
## 1          N                                      6141
## 2          Y                                      1060
##          Prediction
## Reference    N    Y
##         N 6141    0
##         Y 1060    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.527982e-01   0.000000e+00   8.444027e-01   8.609070e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   5.081939e-01  4.439170e-232 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.All.X.no.rnorm.rpart.N
## 1          N                                       314
## 2          Y                                        59
##          Prediction
## Reference   N   Y
##         N 314   0
##         Y  59   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.418231e-01   0.000000e+00   8.007587e-01   8.773551e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   5.346786e-01   4.320720e-14 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      4.588                 0.865
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1         0.5                    0.5               0        0.8527983
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8444027              0.860907             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8418231
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8007587             0.8773551             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0002050944               0
##                label step_major step_minor     bgn     end elapsed
## 4 fit.models_1_rpart          4          0 119.853 126.233    6.38
## 5    fit.models_1_rf          5          0 126.234      NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](songs_template2_files/figure-html/fit.models_1-14.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 17 on full training set
```

![](songs_template2_files/figure-html/fit.models_1-15.png) ![](songs_template2_files/figure-html/fit.models_1-16.png) 

```
##                 Length Class      Mode     
## call                4  -none-     call     
## type                1  -none-     character
## predicted        7201  factor     numeric  
## err.rate         1500  -none-     numeric  
## confusion           6  -none-     numeric  
## votes           14402  matrix     numeric  
## oob.times        7201  -none-     numeric  
## classes             2  -none-     character
## importance         33  -none-     numeric  
## importanceSD        0  -none-     NULL     
## localImportance     0  -none-     NULL     
## proximity           0  -none-     NULL     
## ntree               1  -none-     numeric  
## mtry                1  -none-     numeric  
## forest             14  -none-     list     
## y                7201  factor     numeric  
## test                0  -none-     NULL     
## inbag               0  -none-     NULL     
## xNames             33  -none-     character
## problemType         1  -none-     character
## tuneValue           1  data.frame list     
## obsLevels           2  -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.2566275
## 2        0.1 0.7282721
## 3        0.2 0.9774089
## 4        0.3 0.9971778
## 5        0.4 0.9990575
## 6        0.5 0.9990557
## 7        0.6 0.9990557
## 8        0.7 0.8248337
## 9        0.8 0.3161239
## 10       0.9 0.0674567
## 11       1.0 0.0000000
```

![](songs_template2_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.All.X.no.rnorm.rf.N
## 1          N                                   6139
## 2          Y                                     NA
##   Top10.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                      2
## 2                                   1060
##          Prediction
## Reference    N    Y
##         N 6139    2
##         Y    0 1060
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.9997223      0.9988946      0.9989971      0.9999664      0.8527982 
## AccuracyPValue  McnemarPValue 
##      0.0000000      0.4795001 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_1-19.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.37113402
## 3        0.2 0.47674419
## 4        0.3 0.46464646
## 5        0.4 0.32876712
## 6        0.5 0.20895522
## 7        0.6 0.15625000
## 8        0.7 0.03333333
## 9        0.8 0.03333333
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_1-20.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.All.X.no.rnorm.rf.N
## 1          N                                    242
## 2          Y                                     18
##   Top10.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                     72
## 2                                     41
##          Prediction
## Reference   N   Y
##         N 242  72
##         Y  18  41
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.587131e-01   3.394593e-01   7.119951e-01   8.012709e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   9.999879e-01   2.314376e-08 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     76.855                25.191
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9999997                    0.4       0.9990575        0.8647411
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9989971             0.9999664     0.1813316   0.8103206
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.4767442        0.7587131
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7119951             0.8012709     0.3394593
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.001022467      0.01236912
```

```r
# User specified
#   Ensure at least 2 vars in each regression; else varImp crashes
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df

    # easier to exclude features
require(gdata) # needed for trim
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```r
model_id <- "SongsLog1";
indep_vars_vctr <- head(subset(glb_models_df, grepl("All\\.X\\.", model_id), select=feats)
                        , 1)[, "feats"]
indep_vars_vctr <- trim(unlist(strsplit(indep_vars_vctr, "[,]")))
indep_vars_vctr <- setdiff(indep_vars_vctr, ".rnorm")

    # easier to include features
#model_id <- "Rank9.2"; indep_vars_vctr <- c(NULL
#    ,"<feat1>"
#    ,"<feat1>*<feat2>"
#    ,"<feat1>:<feat2>"
#                                            )
for (method in c("glm")) {
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    csm_mdl_id <- paste0(model_id, ".", method)
    csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
}
```

```
## [1] "fitting model: SongsLog1.glm"
## [1] "    indep_vars: timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min"
## Aggregating results
## Fitting final model on full training set
```

![](songs_template2_files/figure-html/fit.models_1-21.png) ![](songs_template2_files/figure-html/fit.models_1-22.png) ![](songs_template2_files/figure-html/fit.models_1-23.png) ![](songs_template2_files/figure-html/fit.models_1-24.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9220  -0.5399  -0.3459  -0.1845   3.0770  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.470e+01  1.806e+00   8.138 4.03e-16 ***
## timbre_4_max              6.110e-03  1.550e-03   3.942 8.10e-05 ***
## timbre_8_max              4.011e-03  3.003e-03   1.336 0.181620    
## timbre_9_max              1.603e-03  2.434e-03   0.659 0.510188    
## timbre_11_max             1.967e-02  3.385e-03   5.811 6.21e-09 ***
## timbre_6_max              3.668e-03  2.190e-03   1.675 0.093875 .  
## timbre_1_max             -2.449e-04  7.152e-04  -0.342 0.732087    
## timbre_10_max             5.825e-03  1.769e-03   3.292 0.000995 ***
## timbre_2_max              6.586e-04  9.066e-04   0.726 0.467571    
## tempo_confidence          4.732e-01  1.422e-01   3.329 0.000873 ***
## timbre_5_max              7.736e-05  7.935e-04   0.097 0.922337    
## timbre_0_min              2.316e-02  4.256e-03   5.441 5.29e-08 ***
## timesignature_confidence  7.450e-01  1.953e-01   3.815 0.000136 ***
## timesignature             1.264e-01  8.674e-02   1.457 0.145050    
## timbre_7_max             -3.774e-03  1.832e-03  -2.060 0.039408 *  
## key                       1.588e-02  1.039e-02   1.529 0.126349    
## key_confidence            3.087e-01  1.412e-01   2.187 0.028760 *  
## timbre_10_min             4.126e-03  1.839e-03   2.244 0.024852 *  
## timbre_1_min              5.881e-03  7.798e-04   7.542 4.64e-14 ***
## tempo                     3.634e-04  1.691e-03   0.215 0.829889    
## timbre_4_min              1.040e-02  1.985e-03   5.237 1.63e-07 ***
## timbre_3_min              6.920e-04  5.985e-04   1.156 0.247583    
## timbre_3_max             -2.967e-03  5.815e-04  -5.103 3.34e-07 ***
## loudness                  2.999e-01  2.917e-02  10.282  < 2e-16 ***
## timbre_9_min              1.367e-03  2.998e-03   0.456 0.648356    
## timbre_8_min              3.911e-03  2.851e-03   1.372 0.170123    
## energy                   -1.502e+00  3.099e-01  -4.847 1.25e-06 ***
## timbre_5_min             -5.598e-03  1.277e-03  -4.385 1.16e-05 ***
## pitch                    -4.491e+01  6.835e+00  -6.570 5.02e-11 ***
## timbre_2_min             -2.127e-03  1.126e-03  -1.889 0.058843 .  
## timbre_0_max             -3.310e-01  2.569e-02 -12.882  < 2e-16 ***
## timbre_7_min             -4.549e-03  1.781e-03  -2.554 0.010661 *  
## timbre_11_min            -2.625e-02  3.693e-03  -7.108 1.18e-12 ***
## timbre_6_min             -1.686e-02  2.264e-03  -7.445 9.66e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4759.2  on 7167  degrees of freedom
## AIC: 4827.2
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_1-25.png) 

```
##    threshold    f.score
## 1        0.0 0.25662753
## 2        0.1 0.41122775
## 3        0.2 0.47046339
## 4        0.3 0.45700713
## 5        0.4 0.39689181
## 6        0.5 0.28939828
## 7        0.6 0.19386107
## 8        0.7 0.12769629
## 9        0.8 0.05818182
## 10       0.9 0.01683817
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_1-26.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.SongsLog1.glm.N
## 1          N                               5039
## 2          Y                                395
##   Top10.fctr.predict.SongsLog1.glm.Y
## 1                               1102
## 2                                665
##          Prediction
## Reference    N    Y
##         N 5039 1102
##         Y  395  665
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.921122e-01   3.510460e-01   7.825528e-01   8.014345e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.181971e-74 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_1-27.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.44628099
## 3        0.2 0.51612903
## 4        0.3 0.49090909
## 5        0.4 0.43678161
## 6        0.5 0.30136986
## 7        0.6 0.23880597
## 8        0.7 0.15625000
## 9        0.8 0.03333333
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_1-28.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.SongsLog1.glm.N
## 1          N                                258
## 2          Y                                 19
##   Top10.fctr.predict.SongsLog1.glm.Y
## 1                                 56
## 2                                 40
##          Prediction
## Reference   N   Y
##         N 258  56
##         Y  19  40
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.989276e-01   3.982189e-01   7.546059e-01   8.384302e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   9.886242e-01   3.225641e-05 
##        model_id model_method
## 1 SongsLog1.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      3.527                 0.827
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8138747                    0.2       0.4704634        0.8609916
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7825528             0.8014345     0.2284371   0.8425996
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2        0.516129        0.7989276
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7546059             0.8384302     0.3982189    4827.154
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002130866      0.01576222
##               importance
## timbre_0_max   100.00000
## loudness        79.66080
## timbre_1_min    58.22552
## timbre_6_min    57.47385
## timbre_11_min   54.83557
## pitch           50.62914
```

```r
model_id <- "SongsLog2"; indep_vars_vctr <- setdiff(indep_vars_vctr, "loudness")
for (method in c("glm")) {
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    csm_mdl_id <- paste0(model_id, ".", method)
    csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
}
```

```
## [1] "fitting model: SongsLog2.glm"
## [1] "    indep_vars: timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min"
## Aggregating results
## Fitting final model on full training set
```

![](songs_template2_files/figure-html/fit.models_1-29.png) ![](songs_template2_files/figure-html/fit.models_1-30.png) ![](songs_template2_files/figure-html/fit.models_1-31.png) ![](songs_template2_files/figure-html/fit.models_1-32.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0983  -0.5607  -0.3602  -0.1902   3.3107  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -2.241e+00  7.465e-01  -3.002 0.002686 ** 
## timbre_4_max              6.306e-03  1.532e-03   4.115 3.87e-05 ***
## timbre_8_max              6.423e-03  2.950e-03   2.177 0.029497 *  
## timbre_9_max              3.525e-03  2.377e-03   1.483 0.138017    
## timbre_11_max             1.829e-02  3.341e-03   5.476 4.34e-08 ***
## timbre_6_max              3.814e-03  2.157e-03   1.768 0.076982 .  
## timbre_1_max             -7.830e-04  7.064e-04  -1.108 0.267650    
## timbre_10_max             7.367e-03  1.731e-03   4.255 2.09e-05 ***
## timbre_2_max              3.889e-04  8.964e-04   0.434 0.664427    
## tempo_confidence          5.497e-01  1.407e-01   3.906 9.40e-05 ***
## timbre_5_max              6.937e-04  7.807e-04   0.889 0.374256    
## timbre_0_min              2.479e-02  4.240e-03   5.847 5.01e-09 ***
## timesignature_confidence  6.885e-01  1.924e-01   3.578 0.000346 ***
## timesignature             1.625e-01  8.734e-02   1.860 0.062873 .  
## timbre_7_max             -3.158e-03  1.811e-03  -1.744 0.081090 .  
## key                       1.740e-02  1.026e-02   1.697 0.089740 .  
## key_confidence            2.954e-01  1.394e-01   2.118 0.034163 *  
## timbre_10_min             2.993e-03  1.804e-03   1.660 0.097004 .  
## timbre_1_min              7.143e-03  7.710e-04   9.265  < 2e-16 ***
## tempo                     5.521e-04  1.665e-03   0.332 0.740226    
## timbre_4_min              9.115e-03  1.952e-03   4.670 3.02e-06 ***
## timbre_3_min              6.500e-04  5.949e-04   1.093 0.274524    
## timbre_3_max             -2.462e-03  5.674e-04  -4.339 1.43e-05 ***
## timbre_9_min             -4.282e-04  2.955e-03  -0.145 0.884792    
## timbre_8_min              4.488e-03  2.810e-03   1.597 0.110254    
## energy                    1.813e-01  2.608e-01   0.695 0.486991    
## timbre_5_min             -5.641e-03  1.255e-03  -4.495 6.95e-06 ***
## pitch                    -5.150e+01  6.857e+00  -7.511 5.87e-14 ***
## timbre_2_min             -1.579e-03  1.109e-03  -1.424 0.154531    
## timbre_0_max             -1.007e-01  1.178e-02  -8.551  < 2e-16 ***
## timbre_7_min             -5.102e-03  1.755e-03  -2.907 0.003644 ** 
## timbre_11_min            -2.837e-02  3.630e-03  -7.815 5.48e-15 ***
## timbre_6_min             -1.612e-02  2.235e-03  -7.214 5.45e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4871.8  on 7168  degrees of freedom
## AIC: 4937.8
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_1-33.png) 

```
##    threshold     f.score
## 1        0.0 0.256627527
## 2        0.1 0.392096597
## 3        0.2 0.450871080
## 4        0.3 0.448192771
## 5        0.4 0.357986327
## 6        0.5 0.258112094
## 7        0.6 0.164588529
## 8        0.7 0.075201432
## 9        0.8 0.024186047
## 10       0.9 0.005644403
## 11       1.0 0.000000000
```

![](songs_template2_files/figure-html/fit.models_1-34.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.SongsLog2.glm.N
## 1          N                               4978
## 2          Y                                413
##   Top10.fctr.predict.SongsLog2.glm.Y
## 1                               1163
## 2                                647
##          Prediction
## Reference    N    Y
##         N 4978 1163
##         Y  413  647
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.811415e-01   3.256685e-01   7.714094e-01   7.906455e-01   8.527982e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.129046e-79 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_1-35.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.45814978
## 3        0.2 0.51612903
## 4        0.3 0.49056604
## 5        0.4 0.44186047
## 6        0.5 0.30555556
## 7        0.6 0.21212121
## 8        0.7 0.12698413
## 9        0.8 0.03333333
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_1-36.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.SongsLog2.glm.N
## 1          N                                258
## 2          Y                                 19
##   Top10.fctr.predict.SongsLog2.glm.Y
## 1                                 56
## 2                                 40
##          Prediction
## Reference   N   Y
##         N 258  56
##         Y  19  40
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.989276e-01   3.982189e-01   7.546059e-01   8.384302e-01   8.418231e-01 
## AccuracyPValue  McnemarPValue 
##   9.886242e-01   3.225641e-05 
##        model_id model_method
## 1 SongsLog2.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                      feats
## 1 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.575                 0.803
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8003625                    0.2       0.4508711        0.8601582
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7714094             0.7906455     0.2067826   0.8430854
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2        0.516129        0.7989276
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7546059             0.8384302     0.3982189    4937.849
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0006681795     0.005733383
##               importance
## timbre_1_min   100.00000
## timbre_0_max    92.16662
## timbre_11_min   84.10097
## pitch           80.76217
## timbre_6_min    77.50214
## timbre_0_min    62.51903
```

```r
model_id <- "SongsLog3"; 
indep_vars_vctr <- head(subset(glb_models_df, grepl("All\\.X\\.", model_id), select=feats)
                        , 1)[, "feats"]
indep_vars_vctr <- trim(unlist(strsplit(indep_vars_vctr, "[,]")))
indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm", "energy"))
for (method in c("glm")) {
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    csm_mdl_id <- paste0(model_id, ".", method)
    csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
}
```

```
## [1] "fitting model: SongsLog3.glm"
## [1] "    indep_vars: timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min"
## Aggregating results
## Fitting final model on full training set
```

![](songs_template2_files/figure-html/fit.models_1-37.png) ![](songs_template2_files/figure-html/fit.models_1-38.png) ![](songs_template2_files/figure-html/fit.models_1-39.png) ![](songs_template2_files/figure-html/fit.models_1-40.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9182  -0.5417  -0.3481  -0.1874   3.4171  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.196e+01  1.714e+00   6.977 3.01e-12 ***
## timbre_4_max              6.467e-03  1.541e-03   4.196 2.72e-05 ***
## timbre_8_max              4.658e-03  2.988e-03   1.559 0.119022    
## timbre_9_max              1.342e-03  2.424e-03   0.554 0.579900    
## timbre_11_max             1.984e-02  3.365e-03   5.896 3.74e-09 ***
## timbre_6_max              3.447e-03  2.182e-03   1.580 0.114203    
## timbre_1_max             -5.115e-04  7.110e-04  -0.719 0.471928    
## timbre_10_max             5.793e-03  1.759e-03   3.294 0.000988 ***
## timbre_2_max              4.119e-04  9.020e-04   0.457 0.647915    
## tempo_confidence          3.841e-01  1.398e-01   2.747 0.006019 ** 
## timbre_5_max              2.979e-04  7.855e-04   0.379 0.704526    
## timbre_0_min              2.205e-02  4.239e-03   5.200 1.99e-07 ***
## timesignature_confidence  7.143e-01  1.946e-01   3.670 0.000242 ***
## timesignature             1.151e-01  8.726e-02   1.319 0.187183    
## timbre_7_max             -3.394e-03  1.820e-03  -1.865 0.062208 .  
## key                       1.649e-02  1.035e-02   1.593 0.111056    
## key_confidence            3.394e-01  1.409e-01   2.409 0.015984 *  
## timbre_10_min             4.050e-03  1.827e-03   2.217 0.026637 *  
## timbre_1_min              5.416e-03  7.643e-04   7.086 1.38e-12 ***
## tempo                    -6.460e-04  1.665e-03  -0.388 0.698107    
## timbre_4_min              1.105e-02  1.978e-03   5.585 2.34e-08 ***
## timbre_3_min              3.179e-04  5.869e-04   0.542 0.588083    
## timbre_3_max             -2.964e-03  5.758e-04  -5.147 2.64e-07 ***
## loudness                  2.306e-01  2.528e-02   9.120  < 2e-16 ***
## timbre_9_min             -9.318e-05  2.957e-03  -0.032 0.974859    
## timbre_8_min              3.686e-03  2.833e-03   1.301 0.193229    
## timbre_5_min             -5.135e-03  1.269e-03  -4.046 5.21e-05 ***
## pitch                    -5.328e+01  6.733e+00  -7.914 2.49e-15 ***
## timbre_2_min             -2.254e-03  1.120e-03  -2.012 0.044190 *  
## timbre_0_max             -3.105e-01  2.537e-02 -12.240  < 2e-16 ***
## timbre_7_min             -5.128e-03  1.768e-03  -2.900 0.003733 ** 
## timbre_11_min            -2.638e-02  3.683e-03  -7.162 7.96e-13 ***
## timbre_6_min             -1.784e-02  2.246e-03  -7.945 1.94e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4782.7  on 7168  degrees of freedom
## AIC: 4848.7
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.models_1-41.png) 

```
##    threshold    f.score
## 1        0.0 0.25662753
## 2        0.1 0.40791269
## 3        0.2 0.45992300
## 4        0.3 0.46445498
## 5        0.4 0.38008500
## 6        0.5 0.28034682
## 7        0.6 0.18800648
## 8        0.7 0.12000000
## 9        0.8 0.04044118
## 10       0.9 0.01312090
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_1-42.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.SongsLog3.glm.N
## 1          N                               5581
## 2          Y                                570
##   Top10.fctr.predict.SongsLog3.glm.Y
## 1                                560
## 2                                490
##          Prediction
## Reference    N    Y
##         N 5581  560
##         Y  570  490
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.8430774      0.3725277      0.8344684      0.8514076      0.8527982 
## AccuracyPValue  McnemarPValue 
##      0.9900387      0.7889042 
## [1] "    calling mypredict_mdl for OOB:"
```

![](songs_template2_files/figure-html/fit.models_1-43.png) 

```
##    threshold    f.score
## 1        0.0 0.27314815
## 2        0.1 0.45188285
## 3        0.2 0.49350649
## 4        0.3 0.50877193
## 5        0.4 0.47191011
## 6        0.5 0.35135135
## 7        0.6 0.21212121
## 8        0.7 0.15625000
## 9        0.8 0.03333333
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](songs_template2_files/figure-html/fit.models_1-44.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Top10.fctr Top10.fctr.predict.SongsLog3.glm.N
## 1          N                                288
## 2          Y                                 30
##   Top10.fctr.predict.SongsLog3.glm.Y
## 1                                 26
## 2                                 29
##          Prediction
## Reference   N   Y
##         N 288  26
##         Y  30  29
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.8498660      0.4202931      0.8095092      0.8845549      0.8418231 
## AccuracyPValue  McnemarPValue 
##      0.3667675      0.6884997 
##        model_id model_method
## 1 SongsLog3.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.638                 0.812
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8115728                    0.3        0.464455         0.860575
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8344684             0.8514076     0.2189265    0.848969
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.5087719         0.849866
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8095092             0.8845549     0.4202931    4848.719
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.000611418      0.01842614
##               importance
## timbre_0_max   100.00000
## loudness        74.44375
## timbre_6_min    64.81746
## pitch           64.56445
## timbre_11_min   58.40369
## timbre_1_min    57.78607
```

```r
# Ntv.1.lm <- lm(reformulate(indep_vars_vctr, glb_rsp_var), glb_trnobs_df); print(summary(Ntv.1.lm))

#print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
## SongsLog1.glm                         SongsLog1.glm              glm
## SongsLog2.glm                         SongsLog2.glm              glm
## SongsLog3.glm                         SongsLog3.glm              glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                       .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                      timbre_6_min, timbre_4_max
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                 timbre_6_min, timbre_4_max
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                           timbre_6_min, timbre_4_max
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                             timbre_6_min, timbre_4_max
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                        timbre_6_min, timbre_4_max, timbre_6_min:timbre_0_max
## Low.cor.X.glm                       timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## All.X.glm                 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## All.X.bayesglm            timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## All.X.no.rnorm.rpart              timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## All.X.no.rnorm.rf                 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## SongsLog1.glm                     timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## SongsLog2.glm                               timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## SongsLog3.glm                             timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.318
## Random.myrandom_classfr                 0                      0.235
## Max.cor.Y.cv.0.rpart                    0                      0.843
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.559
## Max.cor.Y.rpart                         3                      1.685
## Max.cor.Y.glm                           1                      1.322
## Interact.High.cor.Y.glm                 1                      1.447
## Low.cor.X.glm                           1                      2.727
## All.X.glm                               1                      5.848
## All.X.bayesglm                          1                      4.142
## All.X.no.rnorm.rpart                    3                      4.588
## All.X.no.rnorm.rf                       3                     76.855
## SongsLog1.glm                           1                      3.527
## SongsLog2.glm                           1                      2.575
## SongsLog3.glm                           1                      2.638
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.003   0.5000000
## Random.myrandom_classfr                   0.002   0.5008601
## Max.cor.Y.cv.0.rpart                      0.124   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.113   0.7925561
## Max.cor.Y.rpart                           0.126   0.5000000
## Max.cor.Y.glm                             0.129   0.6982092
## Interact.High.cor.Y.glm                   0.143   0.7238499
## Low.cor.X.glm                             0.863   0.8004704
## All.X.glm                                 0.886   0.8139835
## All.X.bayesglm                            1.068   0.8139566
## All.X.no.rnorm.rpart                      0.865   0.5000000
## All.X.no.rnorm.rf                        25.191   0.9999997
## SongsLog1.glm                             0.827   0.8138747
## SongsLog2.glm                             0.803   0.8003625
## SongsLog3.glm                             0.812   0.8115728
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2566275
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.3       0.4703222
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.2       0.3323678
## Interact.High.cor.Y.glm                      0.2       0.3553903
## Low.cor.X.glm                                0.2       0.4514107
## All.X.glm                                    0.2       0.4666902
## All.X.bayesglm                               0.2       0.4650670
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.4       0.9990575
## SongsLog1.glm                                0.2       0.4704634
## SongsLog2.glm                                0.2       0.4508711
## SongsLog3.glm                                0.3       0.4644550
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                0.8527982             0.8444027
## Random.myrandom_classfr          0.1472018             0.1390930
## Max.cor.Y.cv.0.rpart             0.8527982             0.8444027
## Max.cor.Y.cv.0.cp.0.rpart        0.8698792             0.8618892
## Max.cor.Y.rpart                  0.8328024             0.8444027
## Max.cor.Y.glm                    0.8514097             0.7336766
## Interact.High.cor.Y.glm          0.8525202             0.7491514
## Low.cor.X.glm                    0.8604359             0.7715504
## All.X.glm                        0.8604360             0.7802951
## All.X.bayesglm                   0.8605750             0.7798718
## All.X.no.rnorm.rpart             0.8527983             0.8444027
## All.X.no.rnorm.rf                0.8647411             0.9989971
## SongsLog1.glm                    0.8609916             0.7825528
## SongsLog2.glm                    0.8601582             0.7714094
## SongsLog3.glm                    0.8605750             0.8344684
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.8609070   0.000000000   0.5000000
## Random.myrandom_classfr               0.1555973   0.000000000   0.5167872
## Max.cor.Y.cv.0.rpart                  0.8609070   0.000000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8775683   0.399462097   0.6506801
## Max.cor.Y.rpart                       0.8609070   0.040537462   0.5000000
## Max.cor.Y.glm                         0.7539745   0.005126623   0.7064126
## Interact.High.cor.Y.glm               0.7690385   0.058007365   0.7395552
## Low.cor.X.glm                         0.7907822   0.208371432   0.8415740
## All.X.glm                             0.7992502   0.228606299   0.8421138
## All.X.bayesglm                        0.7988406   0.225649656   0.8423297
## All.X.no.rnorm.rpart                  0.8609070   0.000000000   0.5000000
## All.X.no.rnorm.rf                     0.9999664   0.181331612   0.8103206
## SongsLog1.glm                         0.8014345   0.228437117   0.8425996
## SongsLog2.glm                         0.7906455   0.206782636   0.8430854
## SongsLog3.glm                         0.8514076   0.218926492   0.8489690
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2731481
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.1       0.3320755
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.1       0.3312102
## Interact.High.cor.Y.glm                      0.2       0.3851852
## Low.cor.X.glm                                0.2       0.5064935
## All.X.glm                                    0.2       0.5222930
## All.X.bayesglm                               0.2       0.5222930
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.2       0.4767442
## SongsLog1.glm                                0.2       0.5161290
## SongsLog2.glm                                0.2       0.5161290
## SongsLog3.glm                                0.3       0.5087719
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                0.8418231             0.8007587
## Random.myrandom_classfr          0.1581769             0.1226449
## Max.cor.Y.cv.0.rpart             0.8418231             0.8007587
## Max.cor.Y.cv.0.cp.0.rpart        0.5254692             0.4734236
## Max.cor.Y.rpart                  0.8418231             0.8007587
## Max.cor.Y.glm                    0.4369973             0.3859956
## Interact.High.cor.Y.glm          0.7774799             0.7318099
## Low.cor.X.glm                    0.7962466             0.7517471
## All.X.glm                        0.7989276             0.7546059
## All.X.bayesglm                   0.7989276             0.7546059
## All.X.no.rnorm.rpart             0.8418231             0.8007587
## All.X.no.rnorm.rf                0.7587131             0.7119951
## SongsLog1.glm                    0.7989276             0.7546059
## SongsLog2.glm                    0.7989276             0.7546059
## SongsLog3.glm                    0.8498660             0.8095092
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.8773551    0.00000000
## Random.myrandom_classfr               0.1992413    0.00000000
## Max.cor.Y.cv.0.rpart                  0.8773551    0.00000000
## Max.cor.Y.cv.0.cp.0.rpart             0.5771074    0.11425198
## Max.cor.Y.rpart                       0.8773551    0.00000000
## Max.cor.Y.glm                         0.4890069    0.09998621
## Interact.High.cor.Y.glm               0.8186831    0.25196318
## Low.cor.X.glm                         0.8359713    0.38683163
## All.X.glm                             0.8384302    0.40474924
## All.X.bayesglm                        0.8384302    0.40474924
## All.X.no.rnorm.rpart                  0.8773551    0.00000000
## All.X.no.rnorm.rf                     0.8012709    0.33945929
## SongsLog1.glm                         0.8384302    0.39821886
## SongsLog2.glm                         0.8384302    0.39821886
## SongsLog3.glm                         0.8845549    0.42029307
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                 0.0097891603     0.020586079          NA
## Max.cor.Y.glm                   0.0010199318     0.008109304    5586.714
## Interact.High.cor.Y.glm         0.0014784431     0.017599489    5469.265
## Low.cor.X.glm                   0.0008623889     0.005280039    4939.171
## All.X.glm                       0.0016670046     0.017700517    4828.367
## All.X.bayesglm                  0.0017256879     0.017582275    4828.409
## All.X.no.rnorm.rpart            0.0002050944     0.000000000          NA
## All.X.no.rnorm.rf               0.0010224674     0.012369122          NA
## SongsLog1.glm                   0.0021308656     0.015762223    4827.154
## SongsLog2.glm                   0.0006681795     0.005733383    4937.849
## SongsLog3.glm                   0.0006114180     0.018426137    4848.719
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 5  fit.models_1_rf          5          0 126.234 239.861 113.627
## 6 fit.models_1_end          6          0 239.862      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1  85.439 239.869  154.43
## 12 fit.models          7          2 239.869      NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id",
                                    grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df),
                                    grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
## SongsLog1.glm                         SongsLog1.glm              glm
## SongsLog2.glm                         SongsLog2.glm              glm
## SongsLog3.glm                         SongsLog3.glm              glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                       .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                      timbre_6_min, timbre_4_max
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                 timbre_6_min, timbre_4_max
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                           timbre_6_min, timbre_4_max
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                             timbre_6_min, timbre_4_max
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                        timbre_6_min, timbre_4_max, timbre_6_min:timbre_0_max
## Low.cor.X.glm                       timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## All.X.glm                 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## All.X.bayesglm            timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, .rnorm, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## All.X.no.rnorm.rpart              timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## All.X.no.rnorm.rf                 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## SongsLog1.glm                     timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## SongsLog2.glm                               timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, timbre_9_min, timbre_8_min, energy, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
## SongsLog3.glm                             timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##                           max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                       0   0.5000000
## Random.myrandom_classfr                 0   0.5008601
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.7925561
## Max.cor.Y.rpart                         3   0.5000000
## Max.cor.Y.glm                           1   0.6982092
## Interact.High.cor.Y.glm                 1   0.7238499
## Low.cor.X.glm                           1   0.8004704
## All.X.glm                               1   0.8139835
## All.X.bayesglm                          1   0.8139566
## All.X.no.rnorm.rpart                    3   0.5000000
## All.X.no.rnorm.rf                       3   0.9999997
## SongsLog1.glm                           1   0.8138747
## SongsLog2.glm                           1   0.8003625
## SongsLog3.glm                           1   0.8115728
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2566275
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.3       0.4703222
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.2       0.3323678
## Interact.High.cor.Y.glm                      0.2       0.3553903
## Low.cor.X.glm                                0.2       0.4514107
## All.X.glm                                    0.2       0.4666902
## All.X.bayesglm                               0.2       0.4650670
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.4       0.9990575
## SongsLog1.glm                                0.2       0.4704634
## SongsLog2.glm                                0.2       0.4508711
## SongsLog3.glm                                0.3       0.4644550
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                0.8527982   0.000000000   0.5000000
## Random.myrandom_classfr          0.1472018   0.000000000   0.5167872
## Max.cor.Y.cv.0.rpart             0.8527982   0.000000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart        0.8698792   0.399462097   0.6506801
## Max.cor.Y.rpart                  0.8328024   0.040537462   0.5000000
## Max.cor.Y.glm                    0.8514097   0.005126623   0.7064126
## Interact.High.cor.Y.glm          0.8525202   0.058007365   0.7395552
## Low.cor.X.glm                    0.8604359   0.208371432   0.8415740
## All.X.glm                        0.8604360   0.228606299   0.8421138
## All.X.bayesglm                   0.8605750   0.225649656   0.8423297
## All.X.no.rnorm.rpart             0.8527983   0.000000000   0.5000000
## All.X.no.rnorm.rf                0.8647411   0.181331612   0.8103206
## SongsLog1.glm                    0.8609916   0.228437117   0.8425996
## SongsLog2.glm                    0.8601582   0.206782636   0.8430854
## SongsLog3.glm                    0.8605750   0.218926492   0.8489690
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2731481
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.1       0.3320755
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.1       0.3312102
## Interact.High.cor.Y.glm                      0.2       0.3851852
## Low.cor.X.glm                                0.2       0.5064935
## All.X.glm                                    0.2       0.5222930
## All.X.bayesglm                               0.2       0.5222930
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.2       0.4767442
## SongsLog1.glm                                0.2       0.5161290
## SongsLog2.glm                                0.2       0.5161290
## SongsLog3.glm                                0.3       0.5087719
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                0.8418231    0.00000000
## Random.myrandom_classfr          0.1581769    0.00000000
## Max.cor.Y.cv.0.rpart             0.8418231    0.00000000
## Max.cor.Y.cv.0.cp.0.rpart        0.5254692    0.11425198
## Max.cor.Y.rpart                  0.8418231    0.00000000
## Max.cor.Y.glm                    0.4369973    0.09998621
## Interact.High.cor.Y.glm          0.7774799    0.25196318
## Low.cor.X.glm                    0.7962466    0.38683163
## All.X.glm                        0.7989276    0.40474924
## All.X.bayesglm                   0.7989276    0.40474924
## All.X.no.rnorm.rpart             0.8418231    0.00000000
## All.X.no.rnorm.rf                0.7587131    0.33945929
## SongsLog1.glm                    0.7989276    0.39821886
## SongsLog2.glm                    0.7989276    0.39821886
## SongsLog3.glm                    0.8498660    0.42029307
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                         3.14465409          333.33333333
## Random.myrandom_classfr                   4.25531915          500.00000000
## Max.cor.Y.cv.0.rpart                      1.18623962            8.06451613
## Max.cor.Y.cv.0.cp.0.rpart                 1.78890877            8.84955752
## Max.cor.Y.rpart                           0.59347181            7.93650794
## Max.cor.Y.glm                             0.75642965            7.75193798
## Interact.High.cor.Y.glm                   0.69108500            6.99300699
## Low.cor.X.glm                             0.36670334            1.15874855
## All.X.glm                                 0.17099863            1.12866817
## All.X.bayesglm                            0.24142926            0.93632959
## All.X.no.rnorm.rpart                      0.21795990            1.15606936
## All.X.no.rnorm.rf                         0.01301152            0.03969672
## SongsLog1.glm                             0.28352708            1.20918984
## SongsLog2.glm                             0.38834951            1.24533001
## SongsLog3.glm                             0.37907506            1.23152709
##                            inv.aic.fit
## MFO.myMFO_classfr                   NA
## Random.myrandom_classfr             NA
## Max.cor.Y.cv.0.rpart                NA
## Max.cor.Y.cv.0.cp.0.rpart           NA
## Max.cor.Y.rpart                     NA
## Max.cor.Y.glm             0.0001789961
## Interact.High.cor.Y.glm   0.0001828399
## Low.cor.X.glm             0.0002024631
## All.X.glm                 0.0002071093
## All.X.bayesglm            0.0002071076
## All.X.no.rnorm.rpart                NA
## All.X.no.rnorm.rf                   NA
## SongsLog1.glm             0.0002071614
## SongsLog2.glm             0.0002025173
## SongsLog3.glm             0.0002062400
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 15. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 4 rows containing missing values (geom_path).
```

```
## Warning: Removed 132 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 15. Consider specifying shapes manually if you must have them.
```

![](songs_template2_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## Warning: max.AccuracyUpper.fit already exists in glb_models_df
```

```
## [1] "var:max.KappaSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

![](songs_template2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 15             SongsLog3.glm        0.8498660   0.8489690    0.42029307
## 1          MFO.myMFO_classfr        0.8418231   0.5000000    0.00000000
## 3       Max.cor.Y.cv.0.rpart        0.8418231   0.5000000    0.00000000
## 5            Max.cor.Y.rpart        0.8418231   0.5000000    0.00000000
## 11      All.X.no.rnorm.rpart        0.8418231   0.5000000    0.00000000
## 14             SongsLog2.glm        0.7989276   0.8430854    0.39821886
## 13             SongsLog1.glm        0.7989276   0.8425996    0.39821886
## 10            All.X.bayesglm        0.7989276   0.8423297    0.40474924
## 9                  All.X.glm        0.7989276   0.8421138    0.40474924
## 8              Low.cor.X.glm        0.7962466   0.8415740    0.38683163
## 7    Interact.High.cor.Y.glm        0.7774799   0.7395552    0.25196318
## 12         All.X.no.rnorm.rf        0.7587131   0.8103206    0.33945929
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.5254692   0.6506801    0.11425198
## 6              Max.cor.Y.glm        0.4369973   0.7064126    0.09998621
## 2    Random.myrandom_classfr        0.1581769   0.5167872    0.00000000
##    min.aic.fit opt.prob.threshold.OOB
## 15    4848.719                    0.3
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 11          NA                    0.5
## 14    4937.849                    0.2
## 13    4827.154                    0.2
## 10    4828.409                    0.2
## 9     4828.367                    0.2
## 8     4939.171                    0.2
## 7     5469.265                    0.2
## 12          NA                    0.2
## 4           NA                    0.1
## 6     5586.714                    0.1
## 2           NA                    0.1
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 15. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 56 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 15. Consider specifying shapes manually if you must have them.
```

![](songs_template2_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.auc.OOB - max.Kappa.OOB + min.aic.fit - 
##     opt.prob.threshold.OOB
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: SongsLog3.glm"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
#     if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
#         warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
#         glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
#     }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](songs_template2_files/figure-html/fit.models_2-4.png) ![](songs_template2_files/figure-html/fit.models_2-5.png) ![](songs_template2_files/figure-html/fit.models_2-6.png) ![](songs_template2_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9182  -0.5417  -0.3481  -0.1874   3.4171  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.196e+01  1.714e+00   6.977 3.01e-12 ***
## timbre_4_max              6.467e-03  1.541e-03   4.196 2.72e-05 ***
## timbre_8_max              4.658e-03  2.988e-03   1.559 0.119022    
## timbre_9_max              1.342e-03  2.424e-03   0.554 0.579900    
## timbre_11_max             1.984e-02  3.365e-03   5.896 3.74e-09 ***
## timbre_6_max              3.447e-03  2.182e-03   1.580 0.114203    
## timbre_1_max             -5.115e-04  7.110e-04  -0.719 0.471928    
## timbre_10_max             5.793e-03  1.759e-03   3.294 0.000988 ***
## timbre_2_max              4.119e-04  9.020e-04   0.457 0.647915    
## tempo_confidence          3.841e-01  1.398e-01   2.747 0.006019 ** 
## timbre_5_max              2.979e-04  7.855e-04   0.379 0.704526    
## timbre_0_min              2.205e-02  4.239e-03   5.200 1.99e-07 ***
## timesignature_confidence  7.143e-01  1.946e-01   3.670 0.000242 ***
## timesignature             1.151e-01  8.726e-02   1.319 0.187183    
## timbre_7_max             -3.394e-03  1.820e-03  -1.865 0.062208 .  
## key                       1.649e-02  1.035e-02   1.593 0.111056    
## key_confidence            3.394e-01  1.409e-01   2.409 0.015984 *  
## timbre_10_min             4.050e-03  1.827e-03   2.217 0.026637 *  
## timbre_1_min              5.416e-03  7.643e-04   7.086 1.38e-12 ***
## tempo                    -6.460e-04  1.665e-03  -0.388 0.698107    
## timbre_4_min              1.105e-02  1.978e-03   5.585 2.34e-08 ***
## timbre_3_min              3.179e-04  5.869e-04   0.542 0.588083    
## timbre_3_max             -2.964e-03  5.758e-04  -5.147 2.64e-07 ***
## loudness                  2.306e-01  2.528e-02   9.120  < 2e-16 ***
## timbre_9_min             -9.318e-05  2.957e-03  -0.032 0.974859    
## timbre_8_min              3.686e-03  2.833e-03   1.301 0.193229    
## timbre_5_min             -5.135e-03  1.269e-03  -4.046 5.21e-05 ***
## pitch                    -5.328e+01  6.733e+00  -7.914 2.49e-15 ***
## timbre_2_min             -2.254e-03  1.120e-03  -2.012 0.044190 *  
## timbre_0_max             -3.105e-01  2.537e-02 -12.240  < 2e-16 ***
## timbre_7_min             -5.128e-03  1.768e-03  -2.900 0.003733 ** 
## timbre_11_min            -2.638e-02  3.683e-03  -7.162 7.96e-13 ***
## timbre_6_min             -1.784e-02  2.246e-03  -7.945 1.94e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4782.7  on 7168  degrees of freedom
## AIC: 4848.7
## 
## Number of Fisher Scoring iterations: 6
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); #sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                          importance SongsLog3.glm.importance
## timbre_0_max             100.000000               100.000000
## loudness                  74.443754                74.443754
## timbre_6_min              64.817463                64.817463
## pitch                     64.564454                64.564454
## timbre_11_min             58.403687                58.403687
## timbre_1_min              57.786074                57.786074
## timbre_11_max             48.031092                48.031092
## timbre_4_min              45.486626                45.486626
## timbre_0_min              42.334718                42.334718
## timbre_3_max              41.903716                41.903716
## timbre_4_max              34.107417                34.107417
## timbre_5_min              32.883797                32.883797
## timesignature_confidence  29.803231                29.803231
## timbre_10_max             26.721367                26.721367
## timbre_7_min              23.494570                23.494570
## tempo_confidence          22.240124                22.240124
## key_confidence            19.475923                19.475923
## timbre_10_min             17.899251                17.899251
## timbre_2_min              16.224160                16.224160
## timbre_7_max              15.016247                15.016247
## key                       12.793693                12.793693
## timbre_6_max              12.679951                12.679951
## timbre_8_max              12.510488                12.510488
## timesignature             10.545236                10.545236
## timbre_8_min              10.398850                10.398850
## timbre_1_max               5.633911                 5.633911
## timbre_9_max               4.275753                 4.275753
## timbre_3_min               4.178160                 4.178160
## timbre_2_max               3.482307                 3.482307
## tempo                      2.918906                 2.918906
## timbre_5_max               2.847983                 2.847983
## timbre_9_min               0.000000                 0.000000
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
    
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))

#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_OOBobs_df, mdl_id =
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 32
```

![](songs_template2_files/figure-html/fit.models_2-8.png) ![](songs_template2_files/figure-html/fit.models_2-9.png) ![](songs_template2_files/figure-html/fit.models_2-10.png) ![](songs_template2_files/figure-html/fit.models_2-11.png) ![](songs_template2_files/figure-html/fit.models_2-12.png) 

```
## [1] "Min/Max Boundaries: "
##       .rownames Top10.fctr Top10.fctr.predict.SongsLog3.glm.prob
## 73051     73051          N                            0.11552825
## 73711     73711          Y                            0.79838164
## 74931     74931          N                            0.02205776
##       Top10.fctr.predict.SongsLog3.glm
## 73051                                N
## 73711                                Y
## 74931                                N
##       Top10.fctr.predict.SongsLog3.glm.accurate
## 73051                                      TRUE
## 73711                                      TRUE
## 74931                                      TRUE
##       Top10.fctr.predict.SongsLog3.glm.error .label
## 73051                                      0  73051
## 73711                                      0  73711
## 74931                                      0  74931
## [1] "Inaccurate: "
##       .rownames Top10.fctr Top10.fctr.predict.SongsLog3.glm.prob
## 72271     72271          Y                            0.03793471
## 72251     72251          Y                            0.05658360
## 73721     73721          Y                            0.06631789
## 73701     73701          Y                            0.07250412
## 75211     75211          Y                            0.09579406
## 75361     75361          Y                            0.10175173
##       Top10.fctr.predict.SongsLog3.glm
## 72271                                N
## 72251                                N
## 73721                                N
## 73701                                N
## 75211                                N
## 75361                                N
##       Top10.fctr.predict.SongsLog3.glm.accurate
## 72271                                     FALSE
## 72251                                     FALSE
## 73721                                     FALSE
## 73701                                     FALSE
## 75211                                     FALSE
## 75361                                     FALSE
##       Top10.fctr.predict.SongsLog3.glm.error
## 72271                             -0.2620653
## 72251                             -0.2434164
## 73721                             -0.2336821
## 73701                             -0.2274959
## 75211                             -0.2042059
## 75361                             -0.1982483
##       .rownames Top10.fctr Top10.fctr.predict.SongsLog3.glm.prob
## 75361     75361          Y                             0.1017517
## 73691     73691          Y                             0.1597822
## 74101     74101          Y                             0.1927471
## 75331     75331          Y                             0.2814858
## 73001     73001          N                             0.3216391
## 73031     73031          N                             0.3564767
##       Top10.fctr.predict.SongsLog3.glm
## 75361                                N
## 73691                                N
## 74101                                N
## 75331                                N
## 73001                                Y
## 73031                                Y
##       Top10.fctr.predict.SongsLog3.glm.accurate
## 75361                                     FALSE
## 73691                                     FALSE
## 74101                                     FALSE
## 75331                                     FALSE
## 73001                                     FALSE
## 73031                                     FALSE
##       Top10.fctr.predict.SongsLog3.glm.error
## 75361                            -0.19824827
## 73691                            -0.14021778
## 74101                            -0.10725295
## 75331                            -0.01851418
## 73001                             0.02163909
## 73031                             0.05647668
##       .rownames Top10.fctr Top10.fctr.predict.SongsLog3.glm.prob
## 75441     75441          N                             0.4207847
## 74441     74441          N                             0.4573506
## 73891     73891          N                             0.4914757
## 75291     75291          N                             0.4962977
## 74411     74411          N                             0.5298895
## 74171     74171          N                             0.5439753
##       Top10.fctr.predict.SongsLog3.glm
## 75441                                Y
## 74441                                Y
## 73891                                Y
## 75291                                Y
## 74411                                Y
## 74171                                Y
##       Top10.fctr.predict.SongsLog3.glm.accurate
## 75441                                     FALSE
## 74441                                     FALSE
## 73891                                     FALSE
## 75291                                     FALSE
## 74411                                     FALSE
## 74171                                     FALSE
##       Top10.fctr.predict.SongsLog3.glm.error
## 75441                              0.1207847
## 74441                              0.1573506
## 73891                              0.1914757
## 75291                              0.1962977
## 74411                              0.2298895
## 74171                              0.2439753
```

![](songs_template2_files/figure-html/fit.models_2-13.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
# FN_OOB_ids <- c(4721, 4020, 693, 92)
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_feats_df$id[1:5]])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

# write.csv(glb_chunks_df, paste0(glb_out_pfx, tail(glb_chunks_df, 1)$label, "_",
#                                 tail(glb_chunks_df, 1)$step_minor,  "_chunks1.csv"),
#           row.names=FALSE)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 12 fit.models          7          2 239.869 262.786  22.918
## 13 fit.models          7          3 262.787      NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "Top10.fctr.predict.SongsLog3.glm.prob"    
## [2] "Top10.fctr.predict.SongsLog3.glm"         
## [3] "Top10.fctr.predict.SongsLog3.glm.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](songs_template2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn    end elapsed
## 13        fit.models          7          3 262.787 267.96   5.174
## 14 fit.data.training          8          0 267.961     NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## [1] "fitting model: Final.glm"
## [1] "    indep_vars: timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min"
## Aggregating results
## Fitting final model on full training set
```

![](songs_template2_files/figure-html/fit.data.training_0-1.png) ![](songs_template2_files/figure-html/fit.data.training_0-2.png) ![](songs_template2_files/figure-html/fit.data.training_0-3.png) ![](songs_template2_files/figure-html/fit.data.training_0-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9182  -0.5417  -0.3481  -0.1874   3.4171  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               1.196e+01  1.714e+00   6.977 3.01e-12 ***
## timbre_4_max              6.467e-03  1.541e-03   4.196 2.72e-05 ***
## timbre_8_max              4.658e-03  2.988e-03   1.559 0.119022    
## timbre_9_max              1.342e-03  2.424e-03   0.554 0.579900    
## timbre_11_max             1.984e-02  3.365e-03   5.896 3.74e-09 ***
## timbre_6_max              3.447e-03  2.182e-03   1.580 0.114203    
## timbre_1_max             -5.115e-04  7.110e-04  -0.719 0.471928    
## timbre_10_max             5.793e-03  1.759e-03   3.294 0.000988 ***
## timbre_2_max              4.119e-04  9.020e-04   0.457 0.647915    
## tempo_confidence          3.841e-01  1.398e-01   2.747 0.006019 ** 
## timbre_5_max              2.979e-04  7.855e-04   0.379 0.704526    
## timbre_0_min              2.205e-02  4.239e-03   5.200 1.99e-07 ***
## timesignature_confidence  7.143e-01  1.946e-01   3.670 0.000242 ***
## timesignature             1.151e-01  8.726e-02   1.319 0.187183    
## timbre_7_max             -3.394e-03  1.820e-03  -1.865 0.062208 .  
## key                       1.649e-02  1.035e-02   1.593 0.111056    
## key_confidence            3.394e-01  1.409e-01   2.409 0.015984 *  
## timbre_10_min             4.050e-03  1.827e-03   2.217 0.026637 *  
## timbre_1_min              5.416e-03  7.643e-04   7.086 1.38e-12 ***
## tempo                    -6.460e-04  1.665e-03  -0.388 0.698107    
## timbre_4_min              1.105e-02  1.978e-03   5.585 2.34e-08 ***
## timbre_3_min              3.179e-04  5.869e-04   0.542 0.588083    
## timbre_3_max             -2.964e-03  5.758e-04  -5.147 2.64e-07 ***
## loudness                  2.306e-01  2.528e-02   9.120  < 2e-16 ***
## timbre_9_min             -9.318e-05  2.957e-03  -0.032 0.974859    
## timbre_8_min              3.686e-03  2.833e-03   1.301 0.193229    
## timbre_5_min             -5.135e-03  1.269e-03  -4.046 5.21e-05 ***
## pitch                    -5.328e+01  6.733e+00  -7.914 2.49e-15 ***
## timbre_2_min             -2.254e-03  1.120e-03  -2.012 0.044190 *  
## timbre_0_max             -3.105e-01  2.537e-02 -12.240  < 2e-16 ***
## timbre_7_min             -5.128e-03  1.768e-03  -2.900 0.003733 ** 
## timbre_11_min            -2.638e-02  3.683e-03  -7.162 7.96e-13 ***
## timbre_6_min             -1.784e-02  2.246e-03  -7.945 1.94e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 6017.5  on 7200  degrees of freedom
## Residual deviance: 4782.7  on 7168  degrees of freedom
## AIC: 4848.7
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](songs_template2_files/figure-html/fit.data.training_0-5.png) 

```
##    threshold    f.score
## 1        0.0 0.25662753
## 2        0.1 0.40791269
## 3        0.2 0.45992300
## 4        0.3 0.46445498
## 5        0.4 0.38008500
## 6        0.5 0.28034682
## 7        0.6 0.18800648
## 8        0.7 0.12000000
## 9        0.8 0.04044118
## 10       0.9 0.01312090
## 11       1.0 0.00000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Top10.fctr Top10.fctr.predict.Final.glm.N Top10.fctr.predict.Final.glm.Y
## 1          N                           5581                            560
## 2          Y                            570                            490
##          Prediction
## Reference    N    Y
##         N 5581  560
##         Y  570  490
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.8430774      0.3725277      0.8344684      0.8514076      0.8527982 
## AccuracyPValue  McnemarPValue 
##      0.9900387      0.7889042
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](songs_template2_files/figure-html/fit.data.training_0-6.png) 

```
##    model_id model_method
## 1 Final.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 timbre_4_max, timbre_8_max, timbre_9_max, timbre_11_max, timbre_6_max, timbre_1_max, timbre_10_max, timbre_2_max, tempo_confidence, timbre_5_max, timbre_0_min, timesignature_confidence, timesignature, timbre_7_max, key, key_confidence, timbre_10_min, timbre_1_min, tempo, timbre_4_min, timbre_3_min, timbre_3_max, loudness, timbre_9_min, timbre_8_min, timbre_5_min, pitch, timbre_2_min, timbre_0_max, timbre_7_min, timbre_11_min, timbre_6_min
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.575                 0.815
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8115728                    0.3        0.464455         0.860575
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit min.aic.fit
## 1             0.8344684             0.8514076     0.2189265    4848.719
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.000611418      0.01842614
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 267.961 277.061     9.1
## 15 fit.data.training          8          1 277.062      NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id, : Using default probability threshold: 0.3
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                          SongsLog3.glm.importance importance
## timbre_0_max                           100.000000 100.000000
## loudness                                74.443754  74.443754
## timbre_6_min                            64.817463  64.817463
## pitch                                   64.564454  64.564454
## timbre_11_min                           58.403687  58.403687
## timbre_1_min                            57.786074  57.786074
## timbre_11_max                           48.031092  48.031092
## timbre_4_min                            45.486626  45.486626
## timbre_0_min                            42.334718  42.334718
## timbre_3_max                            41.903716  41.903716
## timbre_4_max                            34.107417  34.107417
## timbre_5_min                            32.883797  32.883797
## timesignature_confidence                29.803231  29.803231
## timbre_10_max                           26.721367  26.721367
## timbre_7_min                            23.494570  23.494570
## tempo_confidence                        22.240124  22.240124
## key_confidence                          19.475923  19.475923
## timbre_10_min                           17.899251  17.899251
## timbre_2_min                            16.224160  16.224160
## timbre_7_max                            15.016247  15.016247
## key                                     12.793693  12.793693
## timbre_6_max                            12.679951  12.679951
## timbre_8_max                            12.510488  12.510488
## timesignature                           10.545236  10.545236
## timbre_8_min                            10.398850  10.398850
## timbre_1_max                             5.633911   5.633911
## timbre_9_max                             4.275753   4.275753
## timbre_3_min                             4.178160   4.178160
## timbre_2_max                             3.482307   3.482307
## tempo                                    2.918906   2.918906
## timbre_5_max                             2.847983   2.847983
## timbre_9_min                             0.000000   0.000000
##                          Final.glm.importance
## timbre_0_max                       100.000000
## loudness                            74.443754
## timbre_6_min                        64.817463
## pitch                               64.564454
## timbre_11_min                       58.403687
## timbre_1_min                        57.786074
## timbre_11_max                       48.031092
## timbre_4_min                        45.486626
## timbre_0_min                        42.334718
## timbre_3_max                        41.903716
## timbre_4_max                        34.107417
## timbre_5_min                        32.883797
## timesignature_confidence            29.803231
## timbre_10_max                       26.721367
## timbre_7_min                        23.494570
## tempo_confidence                    22.240124
## key_confidence                      19.475923
## timbre_10_min                       17.899251
## timbre_2_min                        16.224160
## timbre_7_max                        15.016247
## key                                 12.793693
## timbre_6_max                        12.679951
## timbre_8_max                        12.510488
## timesignature                       10.545236
## timbre_8_min                        10.398850
## timbre_1_max                         5.633911
## timbre_9_max                         4.275753
## timbre_3_min                         4.178160
## timbre_2_max                         3.482307
## tempo                                2.918906
## timbre_5_max                         2.847983
## timbre_9_min                         0.000000
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 32
```

![](songs_template2_files/figure-html/fit.data.training_1-1.png) ![](songs_template2_files/figure-html/fit.data.training_1-2.png) ![](songs_template2_files/figure-html/fit.data.training_1-3.png) ![](songs_template2_files/figure-html/fit.data.training_1-4.png) ![](songs_template2_files/figure-html/fit.data.training_1-5.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames Top10.fctr Top10.fctr.predict.Final.glm.prob
## 2147      2147          N                      6.851974e-12
## 2307      2307          N                      1.215963e-02
## 559        559          N                      3.593032e-02
##      Top10.fctr.predict.Final.glm Top10.fctr.predict.Final.glm.accurate
## 2147                            N                                  TRUE
## 2307                            N                                  TRUE
## 559                             N                                  TRUE
##      Top10.fctr.predict.Final.glm.error .label
## 2147                                  0   2147
## 2307                                  0   2307
## 559                                   0    559
## [1] "Inaccurate: "
##      .rownames Top10.fctr Top10.fctr.predict.Final.glm.prob
## 5071      5071          Y                        0.00291371
## 7485      7485          Y                        0.01365728
## 1124      1124          Y                        0.01445043
## 6869      6869          Y                        0.01842675
## 4901      4901          Y                        0.01871727
## 2214      2214          Y                        0.01901933
##      Top10.fctr.predict.Final.glm Top10.fctr.predict.Final.glm.accurate
## 5071                            N                                 FALSE
## 7485                            N                                 FALSE
## 1124                            N                                 FALSE
## 6869                            N                                 FALSE
## 4901                            N                                 FALSE
## 2214                            N                                 FALSE
##      Top10.fctr.predict.Final.glm.error
## 5071                         -0.2970863
## 7485                         -0.2863427
## 1124                         -0.2855496
## 6869                         -0.2815732
## 4901                         -0.2812827
## 2214                         -0.2809807
##      .rownames Top10.fctr Top10.fctr.predict.Final.glm.prob
## 909        909          Y                        0.08826412
## 6194      6194          Y                        0.08860972
## 1825      1825          Y                        0.15018048
## 1359      1359          N                        0.31637964
## 4752      4752          N                        0.36445023
## 1471      1471          N                        0.36488861
##      Top10.fctr.predict.Final.glm Top10.fctr.predict.Final.glm.accurate
## 909                             N                                 FALSE
## 6194                            N                                 FALSE
## 1825                            N                                 FALSE
## 1359                            Y                                 FALSE
## 4752                            Y                                 FALSE
## 1471                            Y                                 FALSE
##      Top10.fctr.predict.Final.glm.error
## 909                         -0.21173588
## 6194                        -0.21139028
## 1825                        -0.14981952
## 1359                         0.01637964
## 4752                         0.06445023
## 1471                         0.06488861
##      .rownames Top10.fctr Top10.fctr.predict.Final.glm.prob
## 5253      5253          N                         0.8057353
## 6974      6974          N                         0.8071618
## 1278      1278          N                         0.8134451
## 3940      3940          N                         0.8213381
## 6206      6206          N                         0.8394882
## 3680      3680          N                         0.8411511
##      Top10.fctr.predict.Final.glm Top10.fctr.predict.Final.glm.accurate
## 5253                            Y                                 FALSE
## 6974                            Y                                 FALSE
## 1278                            Y                                 FALSE
## 3940                            Y                                 FALSE
## 6206                            Y                                 FALSE
## 3680                            Y                                 FALSE
##      Top10.fctr.predict.Final.glm.error
## 5253                          0.5057353
## 6974                          0.5071618
## 1278                          0.5134451
## 3940                          0.5213381
## 6206                          0.5394882
## 3680                          0.5411511
```

![](songs_template2_files/figure-html/fit.data.training_1-6.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

# print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])

print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "Top10.fctr.predict.Final.glm.prob" "Top10.fctr.predict.Final.glm"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](songs_template2_files/figure-html/fit.data.training_1-7.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn end elapsed
## 15 fit.data.training          8          1 277.062 288  10.938
## 16  predict.data.new          9          0 288.000  NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
# sav_newobs_df <- glb_newobs_df
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(glb_newobs_df, mdl_id = glb_fin_mdl_id,
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.3
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_newobs_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 32
```

![](songs_template2_files/figure-html/predict.data.new-1.png) ![](songs_template2_files/figure-html/predict.data.new-2.png) ![](songs_template2_files/figure-html/predict.data.new-3.png) ![](songs_template2_files/figure-html/predict.data.new-4.png) ![](songs_template2_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##       .rownames Top10.fctr Top10.fctr.predict.Final.glm.prob
## 73051     73051          N                        0.11552825
## 73711     73711          Y                        0.79838164
## 74931     74931          N                        0.02205776
##       Top10.fctr.predict.Final.glm Top10.fctr.predict.Final.glm.accurate
## 73051                            N                                  TRUE
## 73711                            Y                                  TRUE
## 74931                            N                                  TRUE
##       Top10.fctr.predict.Final.glm.error .label
## 73051                                  0  73051
## 73711                                  0  73711
## 74931                                  0  74931
## [1] "Inaccurate: "
##       .rownames Top10.fctr Top10.fctr.predict.Final.glm.prob
## 72271     72271          Y                        0.03793471
## 72251     72251          Y                        0.05658360
## 73721     73721          Y                        0.06631789
## 73701     73701          Y                        0.07250412
## 75211     75211          Y                        0.09579406
## 75361     75361          Y                        0.10175173
##       Top10.fctr.predict.Final.glm Top10.fctr.predict.Final.glm.accurate
## 72271                            N                                 FALSE
## 72251                            N                                 FALSE
## 73721                            N                                 FALSE
## 73701                            N                                 FALSE
## 75211                            N                                 FALSE
## 75361                            N                                 FALSE
##       Top10.fctr.predict.Final.glm.error
## 72271                         -0.2620653
## 72251                         -0.2434164
## 73721                         -0.2336821
## 73701                         -0.2274959
## 75211                         -0.2042059
## 75361                         -0.1982483
##       .rownames Top10.fctr Top10.fctr.predict.Final.glm.prob
## 72271     72271          Y                        0.03793471
## 73101     73101          Y                        0.13016697
## 75221     75221          Y                        0.22328723
## 73931     73931          N                        0.31237727
## 72591     72591          N                        0.32940462
## 75291     75291          N                        0.49629767
##       Top10.fctr.predict.Final.glm Top10.fctr.predict.Final.glm.accurate
## 72271                            N                                 FALSE
## 73101                            N                                 FALSE
## 75221                            N                                 FALSE
## 73931                            Y                                 FALSE
## 72591                            Y                                 FALSE
## 75291                            Y                                 FALSE
##       Top10.fctr.predict.Final.glm.error
## 72271                        -0.26206529
## 73101                        -0.16983303
## 75221                        -0.07671277
## 73931                         0.01237727
## 72591                         0.02940462
## 75291                         0.19629767
##       .rownames Top10.fctr Top10.fctr.predict.Final.glm.prob
## 75441     75441          N                         0.4207847
## 74441     74441          N                         0.4573506
## 73891     73891          N                         0.4914757
## 75291     75291          N                         0.4962977
## 74411     74411          N                         0.5298895
## 74171     74171          N                         0.5439753
##       Top10.fctr.predict.Final.glm Top10.fctr.predict.Final.glm.accurate
## 75441                            Y                                 FALSE
## 74441                            Y                                 FALSE
## 73891                            Y                                 FALSE
## 75291                            Y                                 FALSE
## 74411                            Y                                 FALSE
## 74171                            Y                                 FALSE
##       Top10.fctr.predict.Final.glm.error
## 75441                          0.1207847
## 74441                          0.1573506
## 73891                          0.1914757
## 75291                          0.1962977
## 74411                          0.2298895
## 74171                          0.2439753
```

![](songs_template2_files/figure-html/predict.data.new-6.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
#     submit_df <- glb_newobs_df[, c(paste0(glb_rsp_var_out, glb_fin_mdl_id)), FALSE]
#     names(submit_df)[1] <- "BDscience"
#     submit_df$BDscience <- as.numeric(submit_df$BDscience) - 1
#     #submit_df <-rbind(submit_df, data.frame(bdanalytics=c(" ")))
#     print("Submission Stats:")
#     print(table(submit_df$BDscience, useNA = "ifany"))
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
submit_fname <- paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
                    "_submit.csv")
write.csv(submit_df, submit_fname, quote=FALSE, row.names=FALSE)
#cat(" ", "\n", file=submit_fn, append=TRUE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
```

```
## [1] 0.3
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: SongsLog3.glm"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.glm"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 7201   43
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 15             SongsLog3.glm        0.8498660   0.8489690    0.42029307
## 1          MFO.myMFO_classfr        0.8418231   0.5000000    0.00000000
## 3       Max.cor.Y.cv.0.rpart        0.8418231   0.5000000    0.00000000
## 5            Max.cor.Y.rpart        0.8418231   0.5000000    0.00000000
## 11      All.X.no.rnorm.rpart        0.8418231   0.5000000    0.00000000
## 14             SongsLog2.glm        0.7989276   0.8430854    0.39821886
## 13             SongsLog1.glm        0.7989276   0.8425996    0.39821886
## 10            All.X.bayesglm        0.7989276   0.8423297    0.40474924
## 9                  All.X.glm        0.7989276   0.8421138    0.40474924
## 8              Low.cor.X.glm        0.7962466   0.8415740    0.38683163
## 7    Interact.High.cor.Y.glm        0.7774799   0.7395552    0.25196318
## 12         All.X.no.rnorm.rf        0.7587131   0.8103206    0.33945929
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.5254692   0.6506801    0.11425198
## 6              Max.cor.Y.glm        0.4369973   0.7064126    0.09998621
## 2    Random.myrandom_classfr        0.1581769   0.5167872    0.00000000
##    min.aic.fit opt.prob.threshold.OOB
## 15    4848.719                    0.3
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 11          NA                    0.5
## 14    4937.849                    0.2
## 13    4827.154                    0.2
## 10    4828.409                    0.2
## 9     4828.367                    0.2
## 8     4939.171                    0.2
## 7     5469.265                    0.2
## 12          NA                    0.2
## 4           NA                    0.1
## 6     5586.714                    0.1
## 2           NA                    0.1
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_vars)) {
        stop("not implemented yet")
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
            pred_stats_df <- 
                mypredict_mdl(mdl=glb_models_lst[[glb_fin_mdl_id]], 
                              df=glb_newobs_df, 
                              rsp_var=glb_rsp_var, 
                              rsp_var_out=glb_rsp_var_out, 
                              model_id_method=glb_fin_mdl_id, 
                              label="new",
						      model_summaryFunction=glb_sel_mdl$control$summaryFunction, 
						      model_metric=glb_sel_mdl$metric,
						      model_metric_maximize=glb_sel_mdl$maximize,
						      ret_type="stats")        
            print(sprintf("%s prediction stats for glb_newobs_df:", glb_fin_mdl_id))
            print(pred_stats_df)
    }    
}    
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
        print(sprintf("%s new confusion matrix & accuracy: ", glb_fin_mdl_id))
        print(t(confusionMatrix(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)], 
                                glb_newobs_df[, glb_rsp_var])$table))
    }    

}    
```

```
## [1] "SongsLog3.glm OOB confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 288  26
##         Y  30  29
## [1] "Final.glm new confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 288  26
##         Y  30  29
```

```r
dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

# if (glb_is_classification) {
#     print("FN_OOB_ids:")
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         glb_txt_vars])
#     print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
#                                 union(myfind_chr_cols_df(glb_OOBobs_df),
#                     grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
# }

dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##                          SongsLog3.glm.importance importance
## timbre_0_max                           100.000000 100.000000
## loudness                                74.443754  74.443754
## timbre_6_min                            64.817463  64.817463
## pitch                                   64.564454  64.564454
## timbre_11_min                           58.403687  58.403687
## timbre_1_min                            57.786074  57.786074
## timbre_11_max                           48.031092  48.031092
## timbre_4_min                            45.486626  45.486626
## timbre_0_min                            42.334718  42.334718
## timbre_3_max                            41.903716  41.903716
## timbre_4_max                            34.107417  34.107417
## timbre_5_min                            32.883797  32.883797
## timesignature_confidence                29.803231  29.803231
## timbre_10_max                           26.721367  26.721367
## timbre_7_min                            23.494570  23.494570
## tempo_confidence                        22.240124  22.240124
## key_confidence                          19.475923  19.475923
## timbre_10_min                           17.899251  17.899251
## timbre_2_min                            16.224160  16.224160
## timbre_7_max                            15.016247  15.016247
## key                                     12.793693  12.793693
## timbre_6_max                            12.679951  12.679951
## timbre_8_max                            12.510488  12.510488
## timesignature                           10.545236  10.545236
## timbre_8_min                            10.398850  10.398850
## timbre_1_max                             5.633911   5.633911
## timbre_9_max                             4.275753   4.275753
## timbre_3_min                             4.178160   4.178160
## timbre_2_max                             3.482307   3.482307
## tempo                                    2.918906   2.918906
## timbre_5_max                             2.847983   2.847983
## timbre_9_min                             0.000000   0.000000
##                          Final.glm.importance
## timbre_0_max                       100.000000
## loudness                            74.443754
## timbre_6_min                        64.817463
## pitch                               64.564454
## timbre_11_min                       58.403687
## timbre_1_min                        57.786074
## timbre_11_max                       48.031092
## timbre_4_min                        45.486626
## timbre_0_min                        42.334718
## timbre_3_max                        41.903716
## timbre_4_max                        34.107417
## timbre_5_min                        32.883797
## timesignature_confidence            29.803231
## timbre_10_max                       26.721367
## timbre_7_min                        23.494570
## tempo_confidence                    22.240124
## key_confidence                      19.475923
## timbre_10_min                       17.899251
## timbre_2_min                        16.224160
## timbre_7_max                        15.016247
## key                                 12.793693
## timbre_6_max                        12.679951
## timbre_8_max                        12.510488
## timesignature                       10.545236
## timbre_8_min                        10.398850
## timbre_1_max                         5.633911
## timbre_9_max                         4.275753
## timbre_3_min                         4.178160
## timbre_2_max                         3.482307
## tempo                                2.918906
## timbre_5_max                         2.847983
## timbre_9_min                         0.000000
```

```r
# players_df <- data.frame(id=c("Chavez", "Giambi", "Menechino", "Myers", "Pena"),
#                          OBP=c(0.338, 0.391, 0.369, 0.313, 0.361),
#                          SLG=c(0.540, 0.450, 0.374, 0.447, 0.500),
#                         cost=c(1400000, 1065000, 295000, 800000, 300000))
# players_df$RS.predict <- predict(glb_models_lst[[csm_mdl_id]], players_df)
# print(orderBy(~ -RS.predict, players_df))

if (length(diff <- setdiff(names(glb_trnobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

if (length(diff <- setdiff(names(glb_fitobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
if (length(diff <- setdiff(names(glb_OOBobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
if (length(diff <- setdiff(names(glb_newobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor     bgn     end elapsed
## 16     predict.data.new          9          0 288.000 296.548   8.548
## 17 display.session.info         10          0 296.549      NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor     bgn     end elapsed
## 11              fit.models          7          1  85.439 239.869 154.430
## 10              fit.models          7          0  35.689  85.439  49.750
## 12              fit.models          7          2 239.869 262.786  22.918
## 2             inspect.data          2          0   9.832  27.612  17.780
## 15       fit.data.training          8          1 277.062 288.000  10.938
## 14       fit.data.training          8          0 267.961 277.061   9.100
## 16        predict.data.new          9          0 288.000 296.548   8.548
## 13              fit.models          7          3 262.787 267.960   5.174
## 3               scrub.data          2          1  27.612  31.328   3.716
## 8          select.features          5          0  33.220  35.356   2.136
## 1              import.data          1          0   8.268   9.832   1.564
## 5         extract.features          3          0  31.389  32.797   1.408
## 9  partition.data.training          6          0  35.356  35.689   0.333
## 6             cluster.data          4          0  32.798  33.095   0.298
## 7      manage.missing.data          4          1  33.096  33.219   0.124
## 4           transform.data          2          2  31.328  31.388   0.060
##    duration
## 11  154.430
## 10   49.750
## 12   22.917
## 2    17.780
## 15   10.938
## 14    9.100
## 16    8.548
## 13    5.173
## 3     3.716
## 8     2.136
## 1     1.564
## 5     1.408
## 9     0.333
## 6     0.297
## 7     0.123
## 4     0.060
## [1] "Total Elapsed Time: 296.548 secs"
```

![](songs_template2_files/figure-html/display.session.info-1.png) 

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] tcltk     grid      parallel  stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1        randomForest_4.6-10 arm_1.8-5          
##  [4] lme4_1.1-8          Matrix_1.2-1        MASS_7.3-41        
##  [7] rpart.plot_1.5.2    rpart_4.1-9         ROCR_1.0-7         
## [10] gplots_2.17.0       dplyr_0.4.2         plyr_1.8.3         
## [13] sqldf_0.4-10        RSQLite_1.0.0       DBI_0.3.1          
## [16] gsubfn_0.6-6        proto_0.3-10        reshape2_1.4.1     
## [19] doMC_1.3.3          iterators_1.0.7     foreach_1.4.2      
## [22] doBy_4.5-13         survival_2.38-2     caret_6.0-47       
## [25] ggplot2_1.0.1       lattice_0.20-31    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.6         class_7.3-12        gtools_3.5.0       
##  [4] assertthat_0.1      digest_0.6.8        R6_2.0.1           
##  [7] BradleyTerry2_1.0-6 chron_2.3-47        coda_0.17-1        
## [10] evaluate_0.7        e1071_1.6-4         lazyeval_0.1.10    
## [13] minqa_1.2.4         SparseM_1.6         car_2.0-25         
## [16] nloptr_1.0.4        rmarkdown_0.7       labeling_0.3       
## [19] splines_3.2.0       stringr_1.0.0       munsell_0.4.2      
## [22] compiler_3.2.0      mgcv_1.8-6          htmltools_0.2.6    
## [25] nnet_7.3-9          codetools_0.2-11    brglm_0.5-9        
## [28] bitops_1.0-6        nlme_3.1-120        gtable_0.1.2       
## [31] magrittr_1.5        formatR_1.2         scales_0.2.5       
## [34] KernSmooth_2.23-14  stringi_0.5-2       RColorBrewer_1.1-2 
## [37] tools_3.2.0         abind_1.4-3         pbkrtest_0.4-2     
## [40] yaml_2.1.13         colorspace_1.2-6    caTools_1.17.1     
## [43] knitr_1.10.5        quantreg_5.11
```
