# Introduction

This repository contains the replication materials for the following paper (please reference if you use the code or data from this repository):

```
@inproceedings{joseph_polarized_2019,
  title = {Polarized, Together: Comparing Partisan Support for Trump's Tweets Using Survey and Platform-based Measures},
  booktitle = {Proceedings of the 13th {{International AAAI Conference}} on {{Weblogs}} and {{Social Media}} ({{ICWSM}})},
  author = {Joseph, Kenneth and Swire-Thompson, Briony and Masuga, Hannah and Baum, Matthew A. and Lazer, David},
  year = {2019}
}

```

# Requirements

In addition to the R packages listed at the top of ```R/util.R```, you'll need to have the [twitter_dm](https://github.com/kennyjoseph/twitter_dm) python library installed, as well as [my fork of the VADER](https://github.com/kennyjoseph/vaderSentiment) tool

# Replicating the paper

The main replication file is ```R/analyses.R```.  The script takes in data from the following files in the ```data``` folder:

- ```data/all_yougov_tweetdata_updated.json``` - This is the data pulled down from YouGov's site.  The data was pulled using the script at ```python/01_compile_all_tweets_from_yougov.ipynb```
- ```data/trump_tweets_updated.json``` - This contains all of the tweets from Trump that were retweeted, replied to or mentioned by at least one panel member. The script used to collect the data is at ```python/02_collect_and_tag_tweets.ipynb```.
- ```data/nytimes_attacks_updated.csv``` - This contains all of the insults in Trump's tweets, according to the New York Times.  We pulled down and saved a static version of the page, in ```data/nyt_trump_updated/pg.htm``` and extracted the csv using the script at ```python/03_collect_nytimes_attacks.ipynb```.  Note that we compressed the non-html portions of the page in a zip file, if you want to look at the page closer to how it looked on the times, unzip ```data/nyt_trump_updated/pg_files.zip``` before opening up ```data/nyt_trump_updated/pg.htm``` in a browser.
- ```data/wapo_tid_mapping.csv``` - A listing of the tweet IDs from the Washington Post article on *Tracking all of President Trump’s false or misleading claims*.  Again, we have stored a static screenshot of the story, at ```data/wapo_truth/Tracking all of President Trump’s false or misleading claims - Washington Post.htm```. 
- ```data/person_data.csv``` - A mapping of individuals mentioned in Trumps tweets to demographic information about them.  This data was manually labeled, however, those manual labels were heavily bootstrapped by data from Wikipedia and the ```genderizeR``` package in R.  See ```R/person_bootstrap.R```, which also calls ```python/wikifier.py```, for how that process was carried out. Note that the labeling was performed in a private google sheet accessed from ```person_bootstrap.R```, the data is replicated in ```data/person_data.csv```.  Also note that ```data/person_data.csv``` contains additional people not in the NYT data from preliminary analyses.
- ```data/data_for_figure_2.csv``` - Smoothed (logged) percentages of replies and retweets to Trumps tweets (data for Figure 2). Note that due to privacy restrictions, we do not include the data used to construct this plot directly from the panel (i.e. exactly who retweeted/replied to each tweet)
- ```../data/data_for_hits.csv``` - Counts of Dem/Rep RTs/Replies, only for Trump's tweets since 2017.