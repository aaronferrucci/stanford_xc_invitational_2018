library(ggplot2)
library(gridExtra)
library(scales)

read_data <- function(file) {
  data <- read.fwf(file, c(3, 27, 3, 22, 8, 4, 4), col.names=c("rank", "name", "year", "school", "rawtime", "hnum", "points"), skip=10, stringsAsFactors=F, strip.white=T)

  return(data)
}

clean <- function(data) {
  data <- data[data$year != "",]
  
  return(data)
}

encode <- function(data, gender) {
  
  data$year <- factor(data$year, levels=c("FR", "SO", "JR", "SR"), labels=c("freshman", "sophomore", "junior", "senior"))
  
  data$time <- sapply(data$rawtime, rawtimeToSeconds)
  data$gender <- gender
  return(data)
}

timeToStr <- function(seconds) {
  minutes <- as.integer(seconds / 60)
  seconds <- round(seconds - minutes * 60)
  time <- sprintf("%02d' %02d\"", minutes, seconds)
  
  return(time)
}

# input: 29:56, 1:07:48, etc
# output: time in seconds
rawtimeToSeconds <- function(time) {
  nums <- sapply(strsplit(time, ":", fixed=T), FUN=as.numeric)
  exps <- length(nums):1 - 1
  mults <- 60^exps
  s_list <- nums * mults
  seconds <- sum(s_list)
  return(seconds)
}

boys <- encode(clean(read_data("boys_5k_d4.txt")), "M")
girls <- encode(clean(read_data("girls_5k_d4.txt")), "F")

scboys <- boys[boys$school == "Santa Cruz",]
scgirls <- girls[girls$school == "Santa Cruz",]

bbreaks <- seq(round(min(boys$time), -2), round(max(boys$time), -2), 100)
pb <- ggplot(boys, aes(year, time)) +
  ggtitle("Boys 5k Run CC D 4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_jitter(width=0.05, height=0, color = "#00CCFF") +
  geom_point(data=scboys, aes(year, time), color="blue") +
  scale_y_continuous(breaks=bbreaks, labels=timeToStr(bbreaks), name="Elapsed Time (mm:ss)")

gbreaks <- seq(round(min(girls$time), -2), round(max(girls$time), -2), 100)
pg <- ggplot(girls, aes(year, time)) +
  ggtitle("Girls 5k Run CC D 4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_jitter(width=0.05, height=0, color = "#00CCFF") +
  geom_point(data=scgirls, aes(year, time), color="blue") +
  scale_y_continuous(breaks=gbreaks, labels=timeToStr(gbreaks), name="Elapsed Time (mm:ss)")

grid.arrange(pb, pg)

all <- rbind(boys, girls)
all$gender <- factor(all$gender)
sc <- all[all$school == "Santa Cruz",]

pal <- hue_pal()(2)
all$color <- ifelse(all$gender == "M", pal[2], pal[1])
scpal <- hue_pal(l=90)(2)
sc$color <- ifelse(sc$gender == "M", scpal[2], scpal[1])

allbreaks <- seq(round(min(all$time), -2), round(max(all$time), -2), 100)

p <- ggplot() +
  ggtitle("5k Run CC D 4") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.1)) +
  geom_jitter(data=all, color=all$color, aes(year, time), width=0.1, height=0) +
  scale_y_continuous(breaks=allbreaks, labels=timeToStr(allbreaks), name="Elapsed Time (mm:ss)") +
  geom_jitter(data=sc, width=.1, height=0, shape=21, size=2, fill=sc$color, color="black", aes(year, time))
print(p)

p2 <- ggplot() +
  ggtitle("5k Run CC D 4") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_boxplot(data=all, aes(x=year, y=time, fill=gender, color=gender), alpha=0.1, outlier.shape=NA) +
  geom_point(data=all, aes(year, time, fill=gender, color=gender), position=position_jitterdodge()) +
  geom_point(data=sc, aes(year, time, fill=gender, color=color), shape=21, size=2, color="black", position=position_jitterdodge()) +
  scale_y_continuous(breaks=allbreaks, labels=timeToStr(allbreaks), name="Elapsed Time (mm:ss)")
print(p2)
