source("util.R")

theme_set(theme_bw(20))
########################################################
############### LOAD IN YOUGOV DATA ####################
########################################################
yougov_data <- data.table(fromJSON('../data/all_yougov_tweetdata_updated.json', flatten = TRUE))
yougov_data$created_at <- NULL
#yougov_data$created_at <- parse_date_time(yougov_data$created_at,'%Y-%m-%d %H:%M:%S', tz = "America/New_York")
yougov_data[,`_highlightResult.text.value` := NULL]
yougov_data[,`_highlightResult.text.matchLevel` := NULL]
yougov_data[,`_highlightResult.text.matchedWords` := NULL]
yougov_data <- yougov_data[!is.na(objectID) & !yougov_data$objectID == "NA"]
yougov_data[,`data.All.base` := NULL]
yougov_data[,`data.Democrat.base` := NULL]
yougov_data[,`data.Republican.base` := NULL]
yougov_data[,`data.Independent.base` := NULL]

dim(yougov_data)
####vGet rid of multi-tweet stuff 
yougov_data[, group_len := sapply(group,length)]
yougov_data[,group_text := sapply(group,paste,collapse= " ")]
yougov_data <- yougov_data[!duplicated(group_text)]

dim(yougov_data)

#### Generate scoring data
gen_scores <- function(l,label){
  res <- rbindlist(
    lapply(l,
           function(x){data.table(great=x[1],good=x[2],ok=x[3],bad=x[4],terrible=x[5])}))
  setnames(res, paste(label,names(res),sep="_"))
}
d_scores <- gen_scores(yougov_data$data.Democrat.data,"d")
r_scores <- gen_scores(yougov_data$data.Republican.data,"r")
i_scores <- gen_scores(yougov_data$data.Independent.data,"i")
scores <- cbind(d_scores,r_scores,i_scores)
yougov_data <- cbind(yougov_data,scores)


# Analyze scoring data
d_s <- gen_scores(yougov_data$data.Democrat.data,"")
d_s$typ <- "Democrat"
d_s$v <- 1:nrow(d_s)
r_s <- gen_scores(yougov_data$data.Republican.data,"")
r_s$typ <- "Republican"
r_s$v <- 1:nrow(d_s)
i_s <- gen_scores(yougov_data$data.Independent.data,"")
i_s$typ <- "Indep"
i_s$v <- 1:nrow(d_s)
s <- rbind(d_s,r_s,i_s)
setnames(s, sub("_","",names(s)))
s$text <- yougov_data$text
k <- melt(s, id=c("typ","v","text"))
k[, variable := factor(variable, levels=c("terrible","bad","ok","good","great"))]

# fairly clear patterns in the scorings -> dems think things are OK or terrible, reps think things are great or ok -ish
p1 <- ggplot(k, aes(variable,value,group=v)) + geom_line(alpha=.05) + facet_wrap(~typ) + stat_summary(aes(group=NA),fun.y="mean",color='red',geom='line',size=2) 
p1 <- p1 + scale_y_continuous("Percent of Respondents", labels=percent) + xlab("Likert Option")
ggsave( "../img/likert_updated.pdf", p1, h=5,w=10)




########################################################
############### LOAD IN RAW TWEET DATA TO RETICULATE ####################
########################################################

#use_python("/Users/kennyjoseph/anaconda/bin/python")
twitter_dm <- reticulate::import("twitter_dm")
source("~/git/r_util/kenny_util.R")

u <- twitter_dm$TwitterUser()
u$populate_tweets_from_file("../data/trump_tweets_updated.json",do_parse_created_at=F)
length(u$tweets)

# get general tweet info
tweet_data <- rbindlist(lapply(u$tweets, function(t){
  data.table(tid=t$id_str,
             created_at=t$created_at,
             is_reply=!is.null(t$in_reply_to_status_id),
             rt_count=t$retweet_count,
             fav_count=t$favorited_count,
             reply_count=t$reply_count,
             #quote_count=t$quote_count,
             is_quote=!is.null(t$quoted_tweet),
             is_rt=!is.null(t$retweeted_tweet),
             link_types=lapply(t$media, function(l){l$type}),
             n_hashtags=length(t$entities$hashtags),
             n_mentions=length(t$entities$mentions),
             n_urls=length(t$urls),
             n_media = length(t$media),
             text_length=length(t$text),
             token_len=length(t$tokens),
             source= t$source)}))
tweet_data <- tweet_data[!duplicated(tid)]



########################################################
############### LOAD PANEL DATA ####################
########################################################


########### NOTE: TO PROTECT THE IDENTITY OF THE MEMBERS OF OUR PANEL, 
########### WE DO NOTE RELEASE THE INDIVIDUAL LEVEL PANEL DATA,
########### ONLY AGGREGATE STATISTICS ON MORE RECENT (AND POPULAR) TWEETS
# panel_info <- fread("panel_demographics.csv")
# panel_info$twProfileID <- as.character(panel_info$twProfileID)
# n_rep <- panel_info[,.N,by=party][party == "Republican"]$N
# n_dem <- panel_info[,.N,by=party][party == "Democrat"]$N
# panel_trump_stats <- load_panel_stats("../data/panel_trump_rt_updated.csv",
#                                      "../data/panel_trump_reply_min_updated.csv",
#                                      panel_info, smoother=10)
# panel_trump_stats[, in_yougov_data := tid %in% yougov_data$objectID]
#write.csv(panel_trump_stats[,.(in_yougov_data,log_smoothed_perc_dem_rt, log_smoothed_perc_rep_rt, log_smoothed_perc_dem_reply, log_smoothed_perc_rep_reply) ],
#           "../data/data_for_figure_2.csv",row.names=F)
#write.csv(panel_trump_stats[in_yougov_data==T,.(tid,d_rt, r_rt, d_reply,r_reply) ],
#           "../data/data_for_hits.csv",row.names=F)

panel_trump_stats <- fread("../data/data_for_figure_2.csv")
gen_panel_stat_plot(panel_trump_stats,"in_yougov_data","trump")


########################################################
############### FINALIZE LOADING DATA ####################
########################################################
BUILTINS = reticulate::import_builtins(convert = FALSE)
vader <- reticulate::import("vaderSentiment")

panel_trump_stats <- fread("../data/data_for_hits.csv")
panel_trump_stats$tid <- as.character(panel_trump_stats$tid)
hits <- copy(yougov_data)
hits <- merge(hits, panel_trump_stats, by.y="tid",by.x="objectID",all.x=T)
hits <- merge(hits, tweet_data,by.x="objectID",by.y="tid",all.x=T)

# stuff from december we don't have twitter data for
hits <- hits[!is.na(d_rt)]

# remove deleted tweets
hits$has_url <- hits$n_urls > 0 | hits$n_media > 0
hits <- hits[!is.na(has_url)]

print(dim(hits))
# Sentiment
s_scores <- lapply(hits$text, function(l){vader$vaderSentiment$sentiment(BUILTINS$str(l)$decode('utf-8'))$compound})

hits$sentiment_scores <- unlist(s_scores)
hits[, sentiment := ifelse(sentiment_scores < -.1, "-", ifelse(sentiment_scores < .1, "0","+"))]
hits$sentiment <- factor(hits$sentiment, levels=c("-","0","+"))

# load in NYT Data
nytimes <- fread("../data/nytimes_attacks_updated.csv", encoding="UTF-8",header=T)
setnames(nytimes, c("name", "description","text_ref","status"))
nytimes[, status := sub("https://twitter.com/realDonaldTrump/status/","",status,fixed=T)]
nytimes[, status := sub("http://twitter.com/realDonaldTrump/status/","",status,fixed=T)]
nytimes[, name := tolower(name)]
nytimes <- nytimes[!duplicated(nytimes[,.(name,text_ref,status)])]
hits[, has_insult := if_else(objectID %in% nytimes$status,"Insult Tweet","No Insult")]
hits[, has_insult_int := ifelse(has_insult == "No Insult",0,1)]

hits[, has_boring := grepl("(^| )(law enforcement|safe|hero|first responder|disaster|congrat|victim|tragic|bless|storm|evacua|serv|pray|hurricane|symp|happy|condol|brave)",text, ignore.case = T)]

wapo_data <- fread("../data/wapo_tid_mapping.csv")
wapo_data <- wapo_data[!duplicated(tid)]
wapo_data$tid <- as.character(wapo_data$tid)
hits <- merge(hits, wapo_data, by.x="objectID",by.y="tid",all.x=T)
hits[is.na(wapo_isfalse)]$wapo_isfalse <- 0
hits$ind <- 1:nrow(hits)


# Rename scores
setnames(hits,c("data.Democrat.score","data.Republican.score","data.Independent.score"),
         c("dem_survey","rep_survey","ind_survey"))

cor(log(hits$d_rt+1),hits$dem_survey)
cor(log(hits$d_reply+1),hits$dem_survey)
cor(log(hits$r_rt+1),hits$rep_survey)
cor(log(hits$r_reply+1),hits$rep_survey)

######################################################################################
################### Twitter Metric ######################
######################################################################################





# Step 1: Verify RTs and Replies are a good idea
# Step 2: Smoothing term
df <- data.frame()
cf <- data.frame()
for(rt in 1:50){
  print(rt)
  
  dem_rt <- cor(log(hits$d_rt + rt), hits$dem_survey)
  rep_rt <- cor(log(hits$r_rt+rt), hits$rep_survey)
  dem_reply <- cor(log(hits$d_reply + rt), hits$dem_survey)
  rep_reply <- cor(log(hits$r_reply +rt), hits$rep_survey)
  cf <- rbind(cf, data.frame(rt=rt, dem_rt=dem_rt,rep_rt=rep_rt, dem_reply=dem_reply,rep_reply=rep_reply))
  for(reply in 1:50){
    hits[, tw_score_dem := log( (d_rt+rt)/(d_reply+reply))]
    hits[, tw_score_rep := log( (r_rt+rt)/(r_reply+reply))]
    dem <- cor(hits$tw_score_dem,hits$dem_survey)
    rep <- cor(hits$tw_score_rep,hits$rep_survey)
    df <- rbind(df, data.frame(rt=rt,reply=reply, dem=dem,rep=rep))
  }
}



df <- data.table(df)
m_rt_dem <- get_1se(df,"rt","dem")
m_reply_dem <-  get_1se(df,"reply","dem")
m_rt_rep <-  get_1se(df,"rt","rep")
m_reply_rep <-  get_1se(df,"reply","rep")

print(m_rt_dem)
print(m_rt_rep)
print(m_reply_rep)
print(m_reply_dem)
a<- ggplot(df, aes(reply,dem, color=reply==m_reply_dem)) +  geom_hline(yintercept = min(cf$dem_reply)*-1,color='red') + stat_summary(fun.data="mean_cl_boot")
b <- ggplot(df, aes(reply,rep, color=reply==m_reply_rep))  +  geom_hline(yintercept = min(cf$rep_reply)*-1,color='red') + stat_summary(fun.data="mean_cl_boot")
c <- ggplot(df, aes(rt,rep, color=rt==m_rt_rep))  +  geom_hline(yintercept = max(cf$dem_rt),color='red') + stat_summary(fun.data="mean_cl_boot")
d <- ggplot(df, aes(rt,dem, color=rt==m_rt_dem))  +  geom_hline(yintercept = max(cf$rep_rt),color='red') + stat_summary(fun.data="mean_cl_boot")
a <- a + theme(axis.title.x=element_text(size=25)) + scale_color_manual(values=c("grey","black")) + theme(legend.position = "none") + ylab("") + xlab(expression(lambda[list(Reply,D)]))
b <- b + theme(axis.title.x=element_text(size=25)) + scale_color_manual(values=c("grey","black")) + theme(legend.position = "none") + ylab("") + xlab(expression(lambda[list(Reply,R)]))
c <- c + theme(axis.title.x=element_text(size=25)) + scale_color_manual(values=c("grey","black")) + theme(legend.position = "none") + ylab("") +    xlab(expression(lambda[list(RT,R)]))
d <- d + theme(axis.title.x=element_text(size=25)) + scale_color_manual(values=c("grey","black")) + theme(legend.position = "none") + ylab("") +    xlab(expression(lambda[list(RT,D)]))
plt <- grid.arrange(a,b,d,c,left=textGrob("Correlation", rot=90,vjust=1,gp=gpar(fontsize=20)),padding=unit(.1,'line'),nrow=2)

ggsave("../img/smoothing_param.pdf",plt,h=6,w=7)


# Construct Score
hits[, raw_tw_dem := log( (d_rt+m_rt_dem)/(d_reply+m_reply_dem))]
hits[, raw_tw_rep := log( (r_rt+m_rt_rep)/(r_reply+m_reply_rep))]
hits[, dem_survey_scaled := scale(c(dem_survey,rep_survey))[,][1:nrow(hits)]]
hits[, rep_survey_scaled := scale(c(rep_survey,dem_survey))[,][1:nrow(hits)]]
hits[, dem_twitter_scaled := scale(c(raw_tw_dem,raw_tw_rep))[,][1:nrow(hits)]]
hits[, rep_twitter_scaled := scale(c(raw_tw_rep,raw_tw_dem))[,][1:nrow(hits)]]

hits[, dem_survey_own_scale := scale( dem_survey)[,]]
hits[, rep_survey_own_scale := scale( rep_survey)[,]]
hits[, dem_twitter_own_scale := scale(raw_tw_dem)[,]]
hits[, rep_twitter_own_scale := scale(raw_tw_rep)[,]]


############## RQ1 & 2: polarization and "true" polarization############

p2 <- ggplot(melt(hits, id=c("objectID"),measure=c("raw_tw_dem","raw_tw_rep")), 
              aes(value,fill=variable)) + geom_histogram(position='dodge')
p2 <- p2 + scale_fill_manual(values=c("blue","red")) + theme(legend.position = "none") 
p2 <- p2 + ylab("Number of Tweets") + xlab("Raw Twitter Measure")
ggsave("../img/tweet_score_histo_updated.pdf",h=5,w=5)

p3 <- ggplot(hits, aes(dem_twitter_scaled,rep_twitter_scaled,size=total_panel)) + geom_point(alpha=.3)+ theme(legend.position = "none")+ xlab("Democrat Support") + ylab("Republican Support")
ggsave("../img/tweet_score_scatter_updated.pdf",h=5,w=5)



p2 <- ggplot(melt(hits, id=c("objectID"),measure=c("dem_survey","rep_survey")), aes(value,fill=variable)) + geom_histogram(position='dodge')
p2 <- p2 + scale_fill_manual(values=c("blue","red")) + theme(legend.position = "none") 
p2 <- p2 + ylab("Number of Tweets") + xlab("Raw YouGov Measure")
ggsave("../img/yougov_score_histo_updated.pdf",h=5,w=5)

p3 <- ggplot(hits, aes(dem_survey_scaled,rep_survey_scaled)) + geom_point(alpha=.3)  + xlab("Democrat Support") + ylab("Republican Support")
ggsave("../img/yougov_score_scatter_updated.pdf",h=5,w=5)

# Polarization measure

smean.cl.boot(hits$dem_twitter_scaled-hits$rep_twitter_scaled)
smean.cl.boot(hits$dem_survey_scaled-hits$rep_survey_scaled)
# Correlations 
cor(hits$dem_survey_scaled, hits$rep_survey_scaled)
cor(hits$rep_twitter_scaled,hits$dem_twitter_scaled)

# Note SD Differences

sd(hits$raw_tw_dem)
sd(hits$raw_tw_rep)
sd(hits$dem_survey)
sd(hits$rep_survey)

#############################################
##### Does it hold with other politicians?
#############################################

#NOTE:We have also chosen not to release this data, given that the counts of these behaviors was sometimes low enough
# on all of Twitter, relevant to the panel, that we believed even a pseudoanomimization would not have been enough.
# however, you can find the plots from this analysis in the img/ directory

# m_rt <- 3
# m_reply <- 1
# 
# other_pol_stat_df <- data.table()
# for(person in c("HillaryClinton","BarackObama","VP","NancyPelosi","Mcconell")){
#   stats <- load_panel_stats(paste0("../data/",person,"_rt.csv"),
#                             paste0("../data/",person,"_reply_min_updated.csv"),
#                             panel_info, smoother=0)
#   stats <- stats[total_panel > 10]
#   stats[, raw_tw_dem := log( (d_rt+m_rt)/(d_reply+m_reply))]
#   stats[, raw_tw_rep := log( (r_rt+m_rt)/(r_reply+m_reply))]
# 
#   m <- lm(raw_tw_dem~raw_tw_rep, stats)
#   other_pol_stat_df <- rbind(other_pol_stat_df,
#                              data.table(p=person, med_dem=median(stats$raw_tw_dem), med_rep=median(stats$raw_tw_rep),
#                                                cor=coef(m)[2],
#                                                p=z$coefficients[2,4]))
#   
#   
#   p3 <- ggplot(stats, aes(raw_tw_dem,raw_tw_rep,size=total_panel)) + geom_point(alpha=.3)+ theme(legend.position = "none")+ xlab("Centered Democrat Support") + ylab("Centered Republican Support")
#   ggsave(paste0("../img/corr_",person, ".pdf"),p3 + stat_smooth(method='lm'),h=4,w=4)
#   
#   p2 <- ggplot(melt(stats, id=c("tid"),measure=c("raw_tw_dem","raw_tw_rep")), aes(value,fill=variable)) + geom_histogram(position='dodge')
#   p2 <- p2 + scale_fill_manual(values=c("blue","red")) + theme(legend.position = "none") 
#   p2 <- p2 + ylab("Number of Tweets") + xlab("Raw Twitter Measure")
#   ggsave(paste0("../img/polarization_",person, ".pdf"),p2,h=4,w=4)
# }


##############################################
###### Why/What can we say? ##################################
#############################################

###################f###### INSULT/Sentiment Comparison #############################
pk <- small_multiples_plot(hits,"has_insult")
ggsave("../img/insult_updated.pdf",pk,w=12,h=3.5)

pk1 <- small_multiples_plot(hits,"sentiment")
ggsave("../img/sentiment_updated.pdf",pk1,w=12,h=3.5)

hits[, wapo_false_text := factor(ifelse(wapo_isfalse==1, "False\nClaim", "No False\nClaim"),
                                 levels=c("No False\nClaim","False\nClaim"))]

pk2 <- small_multiples_plot(hits,"wapo_false_text")
ggsave("../img/false_updated.pdf",pk2,w=12,h=3.5)

hits[, sc_text := factor(ifelse(has_boring==F, "No S&C\nLanguage", "S&C\nLanguage"),
                                 levels=c("No S&C\nLanguage","S&C\nLanguage"))]

pk3 <- small_multiples_plot(hits,"sc_text")
ggsave("../img/tragedy_updated.pdf",pk3,w=12,h=3.5)

hits[, url_text := factor(ifelse(has_url ==1, "Has URL", "No URL"),
                                 levels=c("No URL","Has URL"))]
pk4 <- small_multiples_plot(hits,"url_text")

ggsave("../img/url_updated.pdf",pk4,w=12,h=3.5)

mt2 <- melt(hits, id=c("objectID","has_insult","sentiment","has_url","wapo_isfalse",
                       "has_boring"),
            measure=c("rep_twitter_scaled","dem_twitter_scaled",
                      "rep_survey_scaled","dem_survey_scaled"))
mt2[, met := ifelse(variable %in% c("rep_survey_scaled","dem_survey_scaled"),
                    "Survey","Platform")]
mt2[, d:= ifelse(variable %in% c("rep_survey_scaled","rep_twitter_scaled"),
                 "Republican","Democrat")]
mt2[, has_insult := relevel(factor(has_insult),ref="No Insult")]
mt2[, sentiment := relevel(factor(sentiment),ref="0")]

mod <-  lm(value~has_boring*d*met + has_insult*d*met + d*met*sentiment + has_url*d*met + wapo_isfalse*d*met ,mt2)


mod_tab <- data.table(name=names(coef(mod)), coef = coef(mod),lower=confint(mod)[,1],upper=confint(mod)[,2])
sig <- mod_tab[ lower*upper > 0]
sig <- sig[2:nrow(sig),]
sig[, text := mapvalues(name,c("has_boringTRUE",
                               "has_boringTRUE:dRepublican:metSurvey",
                               "has_boringTRUE:metSurvey",
                               "has_insultInsult Tweet",
                               "dRepublican",
                               "metSurvey",
                               "sentiment-",
                               "sentiment+",
                               "has_urlTRUE",
                               "wapo_isfalse",
                               "dRepublican:has_insultInsult Tweet",
                               "dRepublican:wapo_isfalse",
                               "dRepublican:metSurvey",
                               "dRepublican:sentiment-",
                               "dRepublican:sentiment+",
                               "dRepublican:has_urlTRUE",
                               "metSurvey:has_urlTRUE",
                               "dRepublican:metSurvey:has_urlTRUE"
),
c("Main Effect",
  "Republican * Survey",
  "Survey",
  "Main Effect",
  "Republican",
  "Survey",
  "Main Effect (-)",
  "Main Effect (+)",
  "Main Effect",
  "Main Effect",
  "Republican",
  "Republican",
  "Republican * Survey",
  "- * Republican",
  "+ * Republican",
  "Republican",
  "Survey",
  "Republican * Survey")
)]


sig[, t:= ifelse(grepl("has_boring",name), "Support and Condolence",
                 ifelse(grepl("sentiment",name), "Sentiment",
                        ifelse(grepl("wapo_",name), "False Claim",
                               ifelse(grepl("has_url",name),"URL", 
                                      ifelse(grepl("insult",name), "Insult", "Party & Measure Type")))))]

px <- ggplot(sig, aes(reorder(text,-coef), coef, ymin=lower,ymax=upper))+ geom_pointrange() + coord_flip()
px <- px + geom_abline(slope=0, color='red') + xlab("") + ylab("Coefficient") + facet_wrap(~t,scales="free",nrow=3)
ggsave("../img/reg_mod.pdf",px + theme_bw(12),h=5,w=7)



######################### PEOPLE  ################


# load in person map
person_data <- fread("../data/person_data.csv")

# connect times data to persons
times_insult_data <- merge(nytimes[!duplicated(nytimes[,.(name,status)])], person_data[,.(my_party,my_race,my_type,gender.x,name)], by="name")
times_insult_data <- merge(times_insult_data,hits,by.x="status",by.y="objectID")
times_insult_data <- times_insult_data[!duplicated(times_insult_data[,.(status,name)])]
times_insult_data[, my_party := ifelse(is.na(my_party),"none",my_party)]
times_insult_data[, my_type_sm := ifelse(my_type %in% c("news","media","tv","author"), "media",
                                         ifelse(my_type %in% c("sports","celeb","business","X"), "X",
                                                ifelse(my_type %in% c("govt","trump"),"govt",
                                                       ifelse(my_type %in% c("rep","sen","politician"),"politician",
                                                              ifelse(my_type %in% c("mueller","military","family","citizen","religous"), "X", my_type)))))]

times_insult_data[, my_party := relevel(factor(my_party), ref="r")]
times_insult_data[, my_race := relevel(factor(my_race), ref="w")]
times_insult_data[, my_gender := relevel(factor(gender.x), ref="male")]
times_insult_data[, my_type_sm := relevel(factor(my_type_sm), ref="politician")]

cor(times_insult_data$dem_twitter_scaled,times_insult_data$rep_twitter_scaled)
cor(times_insult_data$dem_survey_scaled, times_insult_data$rep_survey_scaled)

mod_dz <- lm( dem_survey_own_scale~my_party+my_race+my_type_sm+my_gender, times_insult_data)
mod_rz <- lm( rep_survey_own_scale~my_party+my_race+my_type_sm+my_gender, times_insult_data)
mod_dt <- lm(dem_twitter_own_scale~my_party+my_race+my_type_sm+my_gender, times_insult_data)
mod_rt <-lm( rep_twitter_own_scale~my_party+my_race+my_type_sm+my_gender, times_insult_data)

library(magrittr)
p <- {dwplot(list(mod_dz,mod_dt,mod_rz,mod_rt),
             vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
             dot_args=list(size=3),
             line_args=list(size=3),
             dodge_size=.6) %>% # plot line at zero _behind_ coefs
    relabel_predictors(c(my_partyd  = "Democrat\n(Ref. Republican)",                       # relabel predictors
                         my_partynone = "No Political Affiliation\n(Ref. Republican)",
                         my_raceb = "Black\n(Ref. White)", 
                         my_racem = "Middle Eastern\n(Ref. White)",
                         my_racea = "Asian\n(Ref. White)",
                         `my_type_smforeign leader` ="Foreign Leader\n(Ref. Politician)",
                         my_type_smgovt ="Gov't Employee\n(Ref. Politician)",
                         my_type_smmedia ="Media\n(Ref. Politician)",
                         my_type_smX ="Other\n(Ref. Politician)",
                         my_type_smpolitician="Politician\n(Ref. Politician)",
                         my_genderfemale="Female\n(Ref. Male)",
                         "(Intercept)"="Intercept")) +
    theme_bw(20) + xlab("Coefficient Estimate") + ylab("") +
    scale_color_manual(labels=
                         c(dem_off_name,rep_off_name,dem_on_name,rep_on_name),
                       values=c("blue","blue","red","red")) +
     aes(linetype=model) + scale_linetype_manual(values=c("dotted","solid","dotted","solid"))}

ggsave("../img/person_reg_updated.pdf",p,h=10,w=8)

summary(mod_dt)$adj.r.squared
summary(mod_rt)$adj.r.squared
summary(mod_dz)$adj.r.squared
summary(mod_rz)$adj.r.squared

#################################################

