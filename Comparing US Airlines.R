################################################################################
#                           Data Massaging                                     #
################################################################################
#Loading required packages
library(tm)
library(twitteR)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(cld2)
library(ggplot2)
library(tidyr)
library(scales) 
library(igraph)
library(ggraph)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#Applying tokens
consumer_key <- 'nJh20uG1RWBTOzDQpj5T684P6'
consumer_secret <- 'FaIYyBNcLDgWGP7QcLsRPTKYcUuZQvErHCpjaZx3at31bLcor3'
access_token <- '1367228710408114176-fyOG67xPsUJU8IjO7y0RvtyR9Jc3nt'
access_secret <- 'MspBewzVx90ZsceFSLOhcpn5bK9N1YG6bqXW203gzOQux'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


########################## Importing data from Twitter #########################
#Delta airline
delta_data <- twitteR::searchTwitter('delta airline', n = 1000, since = '2012-01-01', retryOnRateLimit = 1e3)
delta = twitteR::twListToDF(delta_data)

#United airline
united_data <- twitteR::searchTwitter('united airline', n = 1000, since = '2012-01-01', retryOnRateLimit = 1e3)
united = twitteR::twListToDF(united_data)

#Southwest airline
southwest_data <- twitteR::searchTwitter('southwest airline', n = 1000, since = '2012-01-01', retryOnRateLimit = 1e3)
southwest = twitteR::twListToDF(southwest_data) 

#Adding "company" column 
delta$company <- c("delta")
united$company <- c("united")
southwest$company <- c("southwest")

#Creating companies data set
companies <- rbind(delta, united, southwest)

#Filtering non-English reviews
companies_df <- companies %>% 
  mutate(review_language = cld2::detect_language(text,plain_text = TRUE))%>%
  filter(review_language == "en") #89 observations were removed

#Removing unnecessary information 
companies_df$text <- gsub('#\\S+', '', companies_df$text) #Removing hashtags
companies_df$text <- gsub('@\\S+', '', companies_df$text) #Removing mentions
companies_df$text <- gsub('http\\S+\\s*', '', companies_df$text) #Removing url

#Creating new stopwords
word <- c("airline","airlines","https","rt","like","t.co","delta","southwest","united","flight","like")
lexicon <-  rep("custom", times=length(word))

cus_stops <- data.frame(word, lexicon)
names(cus_stops) <- c("word", "lexicon")
stop_words <-  dplyr::bind_rows(stop_words, cus_stops)

View(stop_words)

################################################################################
#                              Tokenizing dataframe                            #
################################################################################
#Tokenizing delta
delta_df <- companies_df %>%
  filter(company== "delta")

tidy_delta <- delta_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tokenizing united
united_df <- companies_df %>%
  filter(company== "united")

tidy_united <- united_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


#Tokenizing southwest
southwest <- companies_df %>%
  filter(company== "southwest")

tidy_southwest <- southwest %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

################################################################################
#              Combining datasets and creating frequencies                     #
################################################################################
frequency <- bind_rows(mutate(tidy_delta, company="delta"),
                       mutate(tidy_united, company= "united"),
                       mutate(tidy_southwest, company="southwest")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>% 
  count(company, word) %>%
  group_by(company) %>%
  mutate(proportion = n/sum(n))%>% 
  select(-n) %>%
  spread(company, proportion) %>%
  gather(company, proportion, `delta`, `southwest`)

################################################################################
#                         Framework 1: Correlogram                             #
################################################################################
ggplot(frequency, aes(x=proportion, y=`united`, 
                      color = abs(`united`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+ 
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+ 
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~company, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "united", x=NULL)

#Conducting the Correlation test
cor.test(data=frequency[frequency$company == "delta",],
         ~proportion + `united`) #0.6406

cor.test(data=frequency[frequency$company == "southwest",],
         ~proportion + `united`) #0.4926

################################################################################
#                         Framework 2: TF-IDF                                  #
################################################################################
#Grouping by company 
companies_token <- companies_df %>%
  unnest_tokens(word, text) %>%
  count(company, word, sort=TRUE) %>%
  ungroup()

total_words <- companies_token %>%
  group_by(company) %>%
  summarise(total=sum(n))

company_words <- left_join(companies_token, total_words)%>%
  filter(company %in% c("delta", "united", "southwest"))


ggplot(company_words, aes(n/total, fill = company))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~company, ncol=2, scales="free_y")

#ZIPF's law
freq_by_rank <- company_words %>%
  group_by(company) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank 

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=company))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

#TF-IDF
airline_words <- company_words  %>%
  bind_tf_idf(word, company, n)

airline_words  # we get all the zeros because we are looking at stop words ... too common

airline_words  %>%
  arrange(desc(tf_idf))

#Creating the graphical approach
airline_words  %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(company) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=company))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~company, ncol=2, scales="free")+
  coord_flip()


################################################################################
#                         Framework 3: Sentiment Analysis                      #
################################################################################
afinn <- get_sentiments("afinn") 
nrc <- get_sentiments("nrc") 
bing <- get_sentiments("bing") 

sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)


companies_token <- companies_df %>%
  unnest_tokens(word, text)

companies_token = as.data.frame(companies_token)

nrctrust <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")

companies_token %>%
  filter(company == "united") %>%
  inner_join(nrctrust) %>%
  count(word, sort=T)


################################################################################
#                       Comparing different sentiments                         #
################################################################################
################################### United #####################################
united_token <- companies_token %>%
  filter(company == "united")

afinn_united <- united_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN") #positive sentiment 

bing_and_nrc_united <- bind_rows(
  united_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  united_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

##for side by side comparison 
bind_rows(afinn_united, bing_and_nrc_united) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

################################## Delta #######################################
delta_token <- companies_token %>%
  filter(company == "delta")

afinn_delta <- delta_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN") #least positive sentiment 

bing_and_nrc_delta <- bind_rows(
  delta_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  delta_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_delta, bing_and_nrc_delta) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

###################################### Southwest ################################
southwest_token <- companies_token %>%
  filter(company == "southwest")

afinn_southwest <- southwest_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN") 

bing_and_nrc_southwest <- bind_rows(
  southwest_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  southwest_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_southwest, bing_and_nrc_southwest) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")


################################################################################
#              Most common positive and negative words                         #
################################################################################
###################################### United ##################################
bing_counts_united <- united_token %>%
  inner_join(get_sentiments("bing")) %>% #selected bing since it has the least positive sentiment
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_united  

bing_counts_united%>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

################################# Delta #######################################
bing_counts_delta <- delta_token %>%
  inner_join(get_sentiments("bing")) %>% #selected bing since it has the least positive sentiment
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_delta  

bing_counts_delta%>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

####################################### Southwest ##############################
bing_counts_southwest <- southwest_token %>%
  inner_join(get_sentiments("bing")) %>% #selected bing since it has the least positive sentiment
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_southwest  

bing_counts_southwest%>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


