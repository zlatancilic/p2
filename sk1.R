df <- read.csv("restaurantCA.csv")

names(df)

head(df$yelp, 2000)

yp <- subset(df, !is.na(df$coords.x1))
nrow(df[!is.na(df[[addr.street]]),])
nrow(df[!is.na(df$addr.street),])

str(yp)

head(yp$source_ref, 100)

myvars <-c("X", "X.id", "cuisine", "created_by", "source", "name", "addr.county", "addr.postcode", "addr.city", "addr.street", "addr.housenumber", "website", "phone", "email", "yelp")
rest <-df[myvars]

library("ggplot2")


pr <- lapply(rest, function(x) length(x[!is.na(x)]))
pdf <- do.call(rbind.data.frame, pr)

colnames(pdf)[1] <- "valid.count"
pdf$names <- c("X", "X.id", "cuisine", "created_by", "source", "name", "addr.county", "addr.postcode", "addr.city", "addr.street", "addr.housenumber", "website", "phone", "email", "yelp")

ggplot(pdf, aes(names, (valid.count/7188)*100)) +
  geom_bar(stat="identity") +
  xlab("Data columns") + 
  ylab("Valid sample percentage") + 
  ggtitle("Valid data percentage by columns")


summary(rest$source)

ggplot(df, aes(x = coords.x1, y = coords.x2)) + 
  geom_point() + 
  stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) +
  scale_fill_gradient2(low = "white", high = "red")



library(ggmap)

install.packages('rworldmap')


get_openstreetmap(bbox= c(left = -143, bottom = 40, right = -50, top = 71),
                             scale = 69885283, format = "png", urlonly = TRUE)
?get_openstreetmap




newmap <- getMap(resolution = "high")
plot(newmap, xlim = c(-124, -80), ylim = c(33, 42), asp = 1)

map <- get_map(location = 'California', zoom = 6)
mapPoints <- ggmap(map) +
   geom_point(aes(x = coords.x1, y = coords.x2), data = df, alpha = .25, color = 'red')

mapPoints
