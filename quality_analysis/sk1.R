df <- read.csv("restaurantCA.csv")

names(df)

head(df$yelp, 2000)

yp <- subset(df, !is.na(df$website))
nrow(df[!is.na(df[[addr.street]]),])
nrow(df[!is.na(df$addr.street),])

str(yp)

head(yp$website, 100)

myvars <-c("X", "X.id", "cuisine", "created_by", "source", "name", "addr.county", "addr.postcode", "addr.city", "addr.street", "addr.housenumber", "website", "phone", "email", "yelp", "coords.x1", "coords.x2")
rest <-df[myvars]

library("ggplot2")


pr <- lapply(rest, function(x) length(x[!is.na(x)]))
pdf <- do.call(rbind.data.frame, pr)

colnames(pdf)[1] <- "valid.count"
pdf$names <- c("X", "X.id", "cuisine", "created_by", "source", "name", "addr.county", "addr.postcode", "addr.city", "addr.street", "addr.housenumber", "website", "phone", "email", "yelp", "coords.x1", "coords.x2")


pdf <- pdf[-c(1, 2, 16, 17), ]

ggplot(pdf, aes(names, (valid.count/7188)*100)) +
  geom_bar(stat="identity") +
  xlab("Data columns") + 
  ylab("Valid sample percentage") + 
  ggtitle("Valid data percentage by columns")


summary(rest$source)

#regex matching
mail_reg <- "^[[:alnum:].-]+@[[:alnum:]]+.[[:alnum:].-]+$"
web_reg <- "^[^.]+[.][^.]+"

ma <- grep(web_reg, df$website, value = T)
grep(reg, str, value=T)
pdf[pdf$names == "website", 1] <- 1182


#subseting quality sample with regex and ploting heatmap
sub_df <- subset(rest, !is.na(rest$cuisine) & !is.na(rest$name) & grepl(web_reg, website) & !is.na(rest$phone) & !is.na(rest$addr.city) & !is.na(rest$addr.street) & !is.na(rest$addr.housenumber))
library(ggmap)
map <- get_map(location = 'California', zoom = 7)
mapPoints <- ggmap(map, extent='panel') +
  geom_point(aes(x = coords.x1, y = coords.x2), data = df, alpha = .35, color = 'red') +
  geom_point(aes(x = coords.x1, y = coords.x2), data = sub_df, alpha = .35, color = 'blue') +
  scale_colour_manual(name = 'Legend', 
                      values =c('blue'='blue','red'='red'), labels = c('Quality sample','Whole sample'))

#heat map for san francisco - sacramento area
head(df$coords.x1)
head(df$coords.x2)
#subset(df, coords.x1< -120 & coords.x1 > -124 & coords.x2 >36 & coords.x2 < 40)
map <- get_map(location = c(-124, 36, -120, 40), zoom = 8)
mapPoints <- ggmap(map, extent='panel') +
  geom_point(aes(x = coords.x1, y = coords.x2), data = subset(df, coords.x1< -120 & coords.x1 > -124 & coords.x2 >36 & coords.x2 < 40), alpha = .55, color = 'red') +
  geom_point(aes(x = coords.x1, y = coords.x2), data = subset(sub_df, coords.x1< -120 & coords.x1 > -124 & coords.x2 >36 & coords.x2 < 40), alpha = .55, color = 'yellow') +
  scale_colour_manual(name = 'Legend', 
                      values =c('blue'='blue','red'='red'), labels = c('Quality sample','Whole sample'))

mapPoints


#heat map for san diego - los angeles area
map <- get_map(location = c(-120, 32, -114, 36), zoom = 8)
mapPoints <- ggmap(map, extent='panel') +
  geom_point(aes(x = coords.x1, y = coords.x2), data = subset(df, coords.x1< -114 & coords.x1 > -120 & coords.x2 >32 & coords.x2 < 36), alpha = .55, color = 'red') +
  geom_point(aes(x = coords.x1, y = coords.x2), data = subset(sub_df, coords.x1< -114 & coords.x1 > -120 & coords.x2 >32 & coords.x2 < 36), alpha = .55, color = 'blue') +
  scale_colour_manual(name = 'Legend', 
                      values =c('blue'='blue','red'='red'), labels = c('Quality sample','Whole sample'))

mapPoints


#aggregate(X ~ source, rest, function(x) length(unique(x)))

#quality sample ploting, by source
summary(sub_df$source)

names <- c("City of Redwood City, CA 1013", "Local knowledge", "SanGIS", "Website", "Customer", "Yelp", "Facebook", "Owner", "Photo", "Survey")
counts <- c(22, 4, 2, 13, 1, 2, 3, 1, 1, 3)

sub.source.counts <- data.frame(names, counts);
library(scales)
ggplot(sub.source.counts, aes(names, (counts/327))) +
  geom_bar(stat="identity", fill="#56B4E9", colour="black") +
  scale_y_continuous(labels = percent) +
  xlab("Source name") + 
  ylab("Record percentage") + 
  ggtitle("Precentages of records per source in quality sample")


#whole sample ploting, by source
summary(df$source)
counts <- c(48,53,45,28,21,4,3,2,65,378)
df.source.counts <- data.frame(names, (counts)

ggplot(df.source.counts, aes(names, (counts/7188))) +
  geom_bar(stat="identity", fill="red", colour="black") +
  scale_y_continuous(labels = percent) +
  xlab("Source name") + 
  ylab("Record percentage") + 
  ggtitle("Percentage of records per source in whole sample")


#geo quality sample subseting and ploting, by source
geo.vars <- subset(df, !is.na(addr.postcode) & !is.na(addr.city) & !is.na(addr.street) & !is.na(addr.housenumber))

summary(geo.vars$source)

counts <- c(40,4,34,16,7,4,3,1,3,40)
geo.source.counts <- data.frame(names, counts)

ggplot(geo.source.counts, aes(names, (counts/971))) +
  geom_bar(stat="identity", fill="#993300", colour="black") +
  scale_y_continuous(labels = percent) +
  xlab("Source name") + 
  ylab("Record percentage") + 
  ggtitle("Percentages of records per source in geo quality sample")


#CUISINE PLOTING

#whole sample ploting, by cuisine type
summary(df$cuisine)
#2951 NA's
cuisine.names <- c("mexican", "pizza", "american", "chinese", "italian", "japanese", "thai", "burger", "sushi", "sandwich")
cuisine.ws.counts <- c(599,422,425,318,303,234,210,168,149,151)
df.cuisine.counts <- data.frame(cuisine.names, cuisine.ws.counts)
ggplot(df.cuisine.counts, aes(cuisine.names, (cuisine.ws.counts/7188))) +
  geom_bar(stat="identity", fill="red", colour="black") +
  scale_y_continuous(labels = percent) +
  xlab("Cuisine name") + 
  ylab("Record percentage") + 
  ggtitle("Percentage of records per cuisine in whole sample")


#quality sample ploting, by cuisine type
summary(sub_df$cuisine)
cuisine.qs.counts <- c(34,33,26,16,32,15,15,13,12,14)
qs.cuisine.counts <- data.frame(cuisine.names, cuisine.qs.counts)
ggplot(qs.cuisine.counts, aes(cuisine.names, (cuisine.qs.counts/327))) +
  geom_bar(stat="identity", fill="#56B4E9", colour="black") +
  scale_y_continuous(labels = percent) +
  xlab("Cuisine name") + 
  ylab("Record percentage") + 
  ggtitle("Precentages of records per cuisine in quality sample")


#geo quality sample ploting, by cuisine
summary(geo.vars$cuisine)
#192 NA's
cuisine.geo.qs.counts <- c(98,61,71,46,56,40,35,38,23,33)
geo.qs.cuisine.counts <- data.frame(cuisine.names, cuisine.geo.qs.counts)
ggplot(geo.qs.cuisine.counts, aes(cuisine.names, (cuisine.geo.qs.counts/971))) +
  geom_line(stat="identity") +
  scale_y_continuous(labels = percent) +
  xlab("Cuisine name") + 
  ylab("Record percentage") + 
  ggtitle("Percentages of records per cuisine in geo quality sample")




#grouping all cuisine graphs on one multiple line graph
geo.qs.cuisine.counts["sample.type"] <- c("Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample")
qs.cuisine.counts["sample.type"] <- c("Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample")
df.cuisine.counts["sample.type"] <- c("Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample")
colnames(geo.qs.cuisine.counts)[2] <- "cusine.count"
colnames(df.cuisine.counts)[2] <- "cusine.count"
merged.cuisine.counts <- rbind(df.cuisine.counts, qs.cuisine.counts, geo.qs.cuisine.counts) 


merged.cuisine.counts$pctgs <- 0
merged.cuisine.counts$pctgs[merged.cuisine.counts$sample.type == "Whole sample"] <- merged.cuisine.counts$cusine.count/7188
merged.cuisine.counts$pctgs[merged.cuisine.counts$sample.type == "Geo quality sample"] <- merged.cuisine.counts$cusine.count[merged.cuisine.counts$sample.type == "Geo quality sample"]/971
merged.cuisine.counts$pctgs[merged.cuisine.counts$sample.type == "Quality sample"] <- merged.cuisine.counts$cusine.count[merged.cuisine.counts$sample.type == "Quality sample"]/327
ggplot(merged.cuisine.counts, aes(cuisine.names, pctgs)) + 
  geom_line(aes(group = sample.type, color = sample.type)) +
  scale_y_continuous(labels = percent) +
  xlab("Cuisine name") + 
  ylab("Record percentage") + 
  ggtitle("Percentages of records per cuisine")


#grouping all source graphs on one multiple line graph
geo.source.counts["sample.type"] <- c("Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample", "Geo quality sample")
sub.source.counts["sample.type"] <- c("Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample", "Quality sample")
df.source.counts["sample.type"] <- c("Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample", "Whole sample")
merged.source.counts <- rbind(df.source.counts, sub.source.counts, geo.source.counts)
merged.source.counts$pctgs <- 0

merged.source.counts$pctgs[merged.source.counts$sample.type == "Whole sample"] <- merged.source.counts$counts[merged.source.counts$sample.type == "Whole sample"]/7188
merged.source.counts$pctgs[merged.source.counts$sample.type == "Geo quality sample"] <- merged.source.counts$counts[merged.cuisine.counts$sample.type == "Geo quality sample"]/971
merged.source.counts$pctgs[merged.source.counts$sample.type == "Quality sample"] <- merged.source.counts$counts[merged.cuisine.counts$sample.type == "Quality sample"]/327

ggplot(merged.source.counts, aes(names, pctgs)) + 
  geom_line(aes(group = sample.type, color = sample.type)) +
  scale_y_continuous(labels = percent) +
  xlab("Source name") + 
  ylab("Record percentage") + 
  ggtitle("Percentages of records per source")