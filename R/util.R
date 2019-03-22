require(jsonlite)
library(ggplot2)
require(lubridate)
require(reshape2)
require(dplyr)
require(plotly)
require(glmnet)
library(bit64)
library(GGally)
library(rvest)
library(quanteda)
library(reticulate)
library(dotwhisker)
library(gridExtra)
library(grid)
library(data.table)
library(scales)
library(bit64)

dem_off_name <- "Survey Dem."
rep_off_name <- "Survey Rep."
dem_on_name <- "Platform Dem."
rep_on_name <- "Platform Rep."

my_bin <- function(data, mapping, ..., low = "#132B43", high = "#56B1F7") {
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha=.3) + stat_smooth(method='gam') + geom_abline(slope=1,color='red')
}

run_single_regression <- function(data,dep_col,feature_cols){
  # create feature matrix
  X <- data[,feature_cols, with=F]
  #lapply(X, function(l){sum(is.na(l))})
  fill_datatable_na_with_zero(X)
  X <- as.matrix(X)
  # outcome var
  y <- data[,get(dep_col)]
  
  # run regression
  D_predict <- cv.glmnet(X,y)
  
  D_coef <- as.data.frame(as.matrix(coef(D_predict, s = D_predict$lambda.min)))
  D_coef$term <- rownames(D_coef)
  D_coef <- data.table(D_coef)
  setnames(D_coef, c("v","t"))
  D_coef[order(-abs(v))]
  D_coef$mod <- dep_col
  
  pt <- data.table(x=predict(D_predict,X,s = D_predict$lambda.min )[,1],y=y)
  return(list(coefs=D_coef,
              cor_df= data.table(mod=dep_col,corv=cor(pt$x,pt$y)^2)))
}

run_all_regressions <- function(data, feature_cols){
  dz <- run_single_regression(data,"dem_survey_scaled",feature_cols )
  rz <- run_single_regression(data,"rep_survey_scaled",feature_cols )
  dtw <- run_single_regression(data,"dem_twitter_scaled",feature_cols )
  rtw <- run_single_regression(data,"rep_twitter_scaled",feature_cols )
  
  cors <- rbind(dz[[2]],rz[[2]],dtw[[2]],rtw[[2]])
  coefs <-  rbind(dz[[1]],rz[[1]],dtw[[1]],rtw[[1]])
  coefs <- spread(coefs,mod,v)
  return(list(cors=cors,coefs=coefs))
}


load_counts <- function(fil,panel,n_dem,n_rep){
  panel_ct <- fread(fil)
  setnames(panel_ct, c("panel_id","tid","date"))
  panel_ct$panel_id <- as.character(panel_ct$panel_id)
  ct_pan <- merge(panel_ct, panel, by.x="panel_id",by.y="twProfileID")
  ct <- spread(ct_pan[, .N, by=.(tid, party)], party, N, fill=0)
  ct[, perc_dem := Democrat/n_dem]
  ct[, perc_rep := Republican/n_rep]
  ct$tid <- as.character(ct$tid)
  ct <- ct[Democrat > 0 | Republican > 0 ]
  return(ct)
}


load_panel_stats <- function(rt_file,reply_file,panel_info, smoother){
  n_rep <- panel_info[,.N,by=party][party == "Republican"]$N
  n_dem <- panel_info[,.N,by=party][party == "Democrat"]$N
  
  rts <- load_counts(rt_file,panel_info,n_dem,n_rep)
  setnames(rts, c("perc_dem","perc_rep","Democrat","Republican"),c("perc_dem_rt","perc_rep_rt","d_rt","r_rt"))
  
  replies <- load_counts(reply_file, panel_info,n_dem,n_rep)
  setnames(replies, c("perc_dem","perc_rep","Democrat","Republican"),c("perc_dem_reply","perc_rep_reply","d_reply","r_reply"))
  
  
  stats <- merge(rts[,.(tid,perc_dem_rt,perc_rep_rt,d_rt,r_rt)],
                 replies[,.(tid,perc_dem_reply,perc_rep_reply,d_reply,r_reply)], by="tid",all.x=T,all.y=T)
  # This is when people start a tweet with an @ but not in reply to any tweet in particular
  stats <- stats[!tid==""]
  fill_datatable_na_with_zero(stats)
  stats[, total_panel := d_rt+r_rt+d_reply+r_reply]
  stats[, log_smoothed_perc_dem_rt :=    log(perc_dem_rt + smoother/n_dem)]
  stats[, log_smoothed_perc_rep_rt :=    log(perc_rep_rt + smoother/n_rep)]
  stats[, log_smoothed_perc_dem_reply := log(perc_dem_reply + smoother/n_dem)]
  stats[, log_smoothed_perc_rep_reply := log(perc_rep_reply + smoother/n_rep)]
  
  return(stats)
  
}

get_label <- function(axis_name){
  party <- ifelse(grepl("_rep_",axis_name),"Republicans","Democrats")
  t <- ifelse(grepl("_rt", axis_name), "Retweeting","Replying To")
  return(paste0("Log %\n",party, " ", t))
}

gen_panel_stat_plot_single <- function(stats,x_axis,y_axis,color_var){
  p1 <- ggplot(stats, aes_string(x_axis, y_axis)) + geom_point(alpha=.5)
  if(! is.null(color_var)){
    p1 <- p1 + aes_string(color=color_var)
    p1 <- p1 + scale_color_manual(values=c("grey","darkgreen"))+ theme(legend.position = 'none');
  } else{
    p1 <- p1 + geom_density2d()
  }
  p1 <- p1 + geom_abline(slope=1,color='black')
  p1 <- p1 + ylab(get_label(y_axis)) + xlab(get_label(x_axis))
  return(p1)
}

gen_panel_stat_plot <- function(stats, color_var,output_prefix){
  
  p1 <- gen_panel_stat_plot_single(stats, "log_smoothed_perc_rep_rt","log_smoothed_perc_dem_rt",color_var)
  p2 <- gen_panel_stat_plot_single(stats, "log_smoothed_perc_rep_reply","log_smoothed_perc_dem_reply",color_var)
  p3 <- gen_panel_stat_plot_single(stats, "log_smoothed_perc_rep_rt","log_smoothed_perc_rep_reply",color_var)
  p4 <- gen_panel_stat_plot_single(stats, "log_smoothed_perc_dem_rt","log_smoothed_perc_dem_reply",color_var)
  p <- grid.arrange(p1,p2,p3,p4,ncol=2)
  
  ggsave(paste0("../img/",output_prefix,"_tweet_cors_updated.pdf"),p,h=9,w=9)
  p
}


get_1se <- function(df,rt_or_reply, rep_or_dem){
  k <- df[, list(Mean=mean(get(rep_or_dem)),Upper=mean(get(rep_or_dem))+sd(get(rep_or_dem))),by=rt_or_reply]
  max_v <- max(k$Mean)
  max_smooth <- k[order(-Mean)][,get(rt_or_reply)][1]
  return(k[order(get(rt_or_reply))][Upper >= max_v][,get(rt_or_reply)][1])
}


small_multiples_plot <- function(hits, varname){
  
  
  
  mt <- melt(hits, id=c("objectID",varname),measure=c("rep_twitter_own_scale","dem_twitter_own_scale","dem_survey_own_scale","rep_survey_own_scale"))
  mt[, met := ifelse(variable %in% c("rep_survey_own_scale","dem_survey_own_scale"),"Survey","Platform")]
  mt[, d:= ifelse(variable %in% c("rep_survey_own_scale","rep_twitter_own_scale"),"Republican","Democrat")]
  mt$variable <- factor(mt$variable, levels=c("rep_twitter_own_scale","dem_twitter_own_scale","rep_survey_own_scale","dem_survey_own_scale"))
  
  mt[, name := paste(met, d)]
  pk <- ggplot(mt, aes_string(varname, "value",linetype="met",color="d",group=1))
  pk <- pk + stat_summary(fun.data="mean_cl_boot")
  pk <- pk + stat_summary(fun.y="mean",position=position_dodge(.5),geom='line',size=1.1)
  pk <- pk + xlab("") + ylab("Scaled-And-Centered\nSupport Measure")
  pk <- pk + scale_color_manual(values=c("blue","red"))
  pk + facet_wrap(~name,nrow=1) + theme(legend.position='none')
}