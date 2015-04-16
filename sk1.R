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

ggplot(pdf, aes(names, (valid.count/7188)*100)) +
  geom_bar(stat="identity") +
  xlab("Data columns") + 
  ylab("Valid sample percentage") + 
  ggtitle("Valid data percentage by columns")


summary(rest$source)



library(ggmap)

install.packages('rworldmap')


get_openstreetmap(bbox= c(left = -143, bottom = 40, right = -50, top = 71),
                             scale = 69885283, format = "png", urlonly = TRUE)
?get_openstreetmap




mail_reg <- "^[[:alnum:].-]+@[[:alnum:]]+.[[:alnum:].-]+$"
web_reg <- "^[^.]+[.][^.]+"

ma <- grep(web_reg, df$website, value = T)
grep(reg, str, value=T)
pdf[pdf$names == "website", 1] <- 1182



sub_df <- subset(rest, !is.na(rest$cuisine) & !is.na(rest$name) & grepl(web_reg, website) & !is.na(rest$phone) & !is.na(rest$addr.city) & !is.na(rest$addr.street) & !is.na(rest$addr.housenumber))

map <- get_map(location = 'California', zoom = 6)
mapPoints <- ggmap(map) +
  geom_point(aes(x = coords.x1, y = coords.x2), data = df, alpha = .35, color = 'red') +
  geom_point(aes(x = coords.x1, y = coords.x2), data = sub_df, alpha = .35, color = 'blue')
mapPoints


aggregate(X ~ source, rest, function(x) length(unique(x)))
