library(ggplot2)
library(gridExtra)

read_data <- function(file) {
  data <- read.fwf(file, c(3, 27, 3, 22, 8, 4, 4), col.names=c("rank", "name", "year", "school", "rawtime", "hnum", "points"), skip=10, stringsAsFactors=F, strip.white=T)

  return(data)
}

clean <- function(data) {
  data <- data[data$year != "",]
  
  return(data)
}

encode <- function(data) {
  
  data$year <- factor(data$year, levels=c("FR", "SO", "JR", "SR"))
  
  data$time <- sapply(data$rawtime, rawtimeToSeconds)
  # data$color <- ifelse(data$school == "Santa Cruz", "blue", "lightblue")
  return(data)
}

timeToStr <- function(seconds) {
  minutes <- as.integer(seconds / 60)
  seconds <- round(seconds - minutes * 60)
  
  # minute_prefix <- ifelse(minutes < 10, "0", "")
  # minutes <- paste0(minute_prefix, minutes)
  # second_prefix <- ifelse(seconds < 10, "0", "")
  # seconds <- paste0(second_prefix, seconds)
  
  # time <- paste(minutes, seconds, sep=":")
  time <- sprintf("%02d:%02d", minutes, seconds)
  
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

#boys <- read.fwf("boys_5k_d4.txt", c(3, 27, 3, 22, 8, 4, 4), col.names=c("rank", "name", "year", "school", "time", "hnum", "points"), skip=10, stringsAsFactors=F, strip.white=T)
boys <- encode(clean(read_data("boys_5k_d4.txt")))
girls <- encode(clean(read_data("girls_5k_d4.txt")))

scboys <- boys[boys$school == "Santa Cruz",]
scgirls <- girls[girls$school == "Santa Cruz",]
breaks <- seq(round(min(boys$time), -2), round(max(boys$time), -2), 100)
pb <- ggplot(boys, aes(year, time)) +
  ggtitle("Boys 5k Run CC D 4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_jitter(width=0.05, color = "#00CCFF") +
  geom_point(data=scboys, aes(year, time), color="blue") +
  scale_y_continuous(breaks=breaks, labels=timeToStr(breaks), name="Elapsed Time (mm:ss)")

breaks <- seq(round(min(girls$time), -2), round(max(girls$time), -2), 100)
pg <- ggplot(girls, aes(year, time)) +
  ggtitle("Girls 5k Run CC D 4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_jitter(width=0.05, color = "#00CCFF") +
  geom_point(data=scgirls, aes(year, time), color="blue") +
  scale_y_continuous(breaks=breaks, labels=timeToStr(breaks), name="Elapsed Time (mm:ss)")

grid.arrange(pb, pg)
