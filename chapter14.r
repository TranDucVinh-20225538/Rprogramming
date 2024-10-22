epa.df <- readRDS(gzcon(url("https://goo.gl/s5vjWz")))

#Q1
bytes.agg <- aggregate(bytes ~ host, data=epa.df, sum)       
bytes.agg <- bytes.agg[rev(order(bytes.agg$bytes)), ]        
hist(bytes.agg$bytes)
hist(log(bytes.agg$bytes[bytes.agg$bytes > 0]), breaks = 100)

#Q2
# that value we can often see is 0
#there are some reasons:
#some bots or web crawlers may send requests without downloading any content
#Many requests may come from hosts that do not download any actual content

#Q3
epa.df <- readRDS(gzcon(url("https://goo.gl/s5vjWz")))
#get time differences between rows in minutes
epa.ordered <- epa.df[order(epa.df$host, epa.df$datetime), ]
epa.ordered$time.diff <- 
  c(NA, as.numeric(difftime(epa.ordered$datetime[-1], 
                             epa.ordered$datetime[-nrow(epa.ordered)], 
                             units="mins")))
session.threshold <- 15 
epa.ordered$is_new_session <- NA  
epa.ordered$is_new_session[1] <- TRUE  
for (i in 2:nrow(epa.ordered)) {
  epa.ordered$is_new_session[i] <- 
    epa.ordered$host[i] != epa.ordered$host[i - 1] || 
    epa.ordered$time.diff[i] >= session.threshold
}
epa.ordered$session_id <- cumsum(epa.ordered$is_new_session)
epa.ordered$time.diff[epa.ordered$is_new_session] <- NA  
most_frequent_pages <- names(head(sort(table(epa.df$page[epa.df$pagetype=="html"]), 
                                        decreasing = TRUE), 20))
epa.html <- subset(epa.ordered, pagetype == "html" & page %in% most_frequent_pages)
epa.sessions <- split(epa.html, epa.html$session_id)
valid_session_lengths <- sapply(epa.sessions, nrow) > 1
epa.sessions <- epa.sessions[valid_session_lengths]

#convert to a sequence of pages for each user
epa.streamE <- unlist(lapply(epa.sessions, 
                              function(session) {
                                host <- unique(session$host)
                                pages <- paste(session$page, collapse = ",")
                                return(paste0(host, ",", pages))
                              }))
head(epa.streamE) 

any(grepl("ii", epa.streamE))  
epa.streamE <- gsub("/", "ii", epa.streamE)
epa.streamE <- gsub("\\.html$", "", epa.streamE)
head(epa.streamE)  

install.packages("clickstream")
install.packages("igraph")
library(clickstream)  
click.tempfile <- tempfile()
writeLines(epa.streamE, click.tempfile)  
epa.transE <- readClickstreams(click.tempfile, header = TRUE) 

head(epa.streamE)  
head(epa.transE)  
tail(epa.streamE)  
tail(epa.transE)  

#fit Markov chain to the transactions
epa.mcE <- fitMarkovChain(epa.transE, order = 1)
transitions_matrix <- epa.mcE@transitions
transitions_matrix  

#visualizing by heatmap
epa.mc.matE <- t(transitions_matrix[[1]])  
dimnames(epa.mc.matE)[[1]] <- gsub("ii", "/", dimnames(epa.mc.matE)[[1]])
dimnames(epa.mc.matE)[[2]] <- gsub("ii", "/", dimnames(epa.mc.matE)[[2]])
superheat(epa.mc.matE,
          bottom.label.size = 0.8,
          bottom.label.text.size = 2.5,
          bottom.label.text.angle = 270,
          left.label.size = 0.4,
          left.label.text.size = 2.5,
          heat.col.scheme = "red", 
          n.clusters.rows = 5, 
          n.clusters.cols = 5,
          left.label = "variable", bottom.label = "variable",
          title = "Page transition heatmap for top 20 pages")

#visualizing by graph model
set.seed(1404) 
plot(epa.mcE, minProbability = 0.15) 


#Q4
superheat(epa.mc.matE,
          bottom.label.size = 0.6,
          bottom.label.text.size = 2.5,
          left.label.size = 0.4,
          left.label.text.size = 3,
          bottom.label.text.angle = 270,
          heat.col.scheme = "red", 
          n.clusters.rows = 5, n.clusters.cols = 5,
          left.label = "variable", bottom.label = "variable", 
          title = "Page transition heatmap for top 40 pages")
#adjust the session splitting to make sure that the sessions are split based on new dataset


plot(epa.mcE, minProbability = 0.1)  # Điều chỉnh ngưỡng xác suất nếu cần
#adjust the minprobability parameter to filter out less significant transitions

#Q5
#i wish to have these information:
#time on page that the amount of time users spend on each page provides insight into their level of interest and engagement with the content
#device type that knowing whether users access the site via a computer, smartphone, or tablet can help optimize the website's design for each device
#content interaction that tracking user interactions with content like clicking links, downloading documents can offer insights into how well the content is received

#Q6
sdsc.df <- read.csv("https://goo.gl/jpWMVh")
head(sdsc.df)

names(sdsc.df) <- c("raw_data")
install.packages("dplyr")
library(dplyr)
library(tidyr)
sdsc.df <- sdsc.df %>% 
  mutate(raw_data = as.character(V1))
sdsc.clean.df <- sdsc.df %>%
  mutate(
    id = sub("^(\\S+).*", "\\1", raw_data),
    datetime = sub(".*?\\s+(Tue|Wed|Thu|Fri|Sat|Sun)\\s+(\\w+)\\s+(\\d+)\\s+(\\d+:\\d+:\\d+)\\s+(\\d{4}).*", "\\1 \\2 \\3 \\4 \\5", raw_data),
    url = sub(".*?\\((/[^)]+)\\):.*", "\\1", raw_data),
    status = sub(".*?: (Sent \\w+:).*", "\\1", raw_data)
  ) %>%
  select(id, datetime, url, status) 
head(sdsc.clean.df)

library(dplyr)
library(superheat)
valid_urls <- sdsc.clean.df[!grepl("access denied|file does not exist", sdsc.clean.df$url), ]
top_urls <- table(valid_urls$url) %>%
  as.data.frame() %>%
  arrange(desc(Freq))
top_20_urls <- head(top_urls, 20)
valid_top_20 <- valid_urls %>%
  filter(url %in% top_20_urls$Var1 & !is.na(url) & url != "")  
  transitions_top_20 <- table(valid_top_20$url[-nrow(valid_top_20)], 
                               valid_top_20$url[-1])
  
  transitions_matrix_top_20 <- prop.table(transitions_top_20, margin = 1)
    
set.seed(1406) 
superheat(transitions_matrix_top_20,
            bottom.label.size = 1,
            bottom.label.text.size = 2.5,
            left.label.size = 1,bottom.label.text.angle = 270,
            left.label.text.size = 2.5,
            heat.col.scheme = "red", 
            title = "Page transition heatmap for top 40 pages")
  

most_common_transitions_df <- as.data.frame(as.table(transitions_matrix_top_20))
colnames(most_common_transitions_df) <- c("Source", "Target", "Frequency")
most_common_transitions_df <- most_common_transitions_df[order(-most_common_transitions_df$Frequency), ]
top_10_transitions <- head(most_common_transitions_df, 10)
top_10_transitions



