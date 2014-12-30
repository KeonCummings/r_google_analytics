library(RGoogleAnalytics)

save.token <- function() {
	token <- Auth("635724585936-c491melt43ktgdpsb9md09011q6ugih1.apps.googleusercontent.com", "ujtf4LufJvK-bK7yMeSbBP0U")
	save(token,file="./token_file")
}

load.token <- function() {
	return(load("token_file"))
}
get.token <- function() {
	return(Auth("635724585936-c491melt43ktgdpsb9md09011q6ugih1.apps.googleusercontent.com", "ujtf4LufJvK-bK7yMeSbBP0U"))
}

get.profiles <- function(){
	token <- Auth("635724585936-c491melt43ktgdpsb9md09011q6ugih1.apps.googleusercontent.com", "ujtf4LufJvK-bK7yMeSbBP0U")
	return(GetProfiles(token))
}

sources<- function(start.date, end.date, id){
	token <- Auth("635724585936-c491melt43ktgdpsb9md09011q6ugih1.apps.googleusercontent.com", "ujtf4LufJvK-bK7yMeSbBP0U")
	query.list <- Init(
	start.date = start.date,
	end.date = end.date,
	dimensions = "ga:source",
	metrics = "ga:sessions, ga:uniquePageviews, ga:pageviews, ga:avgSessionDuration, ga:bounceRate, ga:percentNewSessions",
	max.results = 1000,
	table.id = paste("ga:",id, sep=""))

	ga.query <- QueryBuilder(query.list)
	ga.df <- GetReportData(ga.query, token)
	return(ga.df)
}

pageviews <- function(start.date, end.date, id){
	token <- Auth("635724585936-c491melt43ktgdpsb9md09011q6ugih1.apps.googleusercontent.com", "ujtf4LufJvK-bK7yMeSbBP0U")
	query.list <- Init(
	start.date = start.date,
	end.date = end.date,
	dimensions = "ga:date",
	metrics = "ga:sessions, ga:uniquePageviews, ga:pageviews, ga:avgSessionDuration, ga:bounceRate, ga:percentNewSessions",
	max.results = 1000,
	table.id = paste("ga:",id, sep=""))

	ga.query <- QueryBuilder(query.list)
	ga.df <- GetReportData(ga.query, token)
	return(ga.df)
}
mobile <- function(start.date, end.date, id){
	token <- Auth("635724585936-c491melt43ktgdpsb9md09011q6ugih1.apps.googleusercontent.com", "ujtf4LufJvK-bK7yMeSbBP0U")
	query.list <- Init(
	start.date = start.date,
	end.date = end.date,
	dimensions = "ga:mobileDeviceInfo",
	metrics = "ga:sessions, ga:uniquePageviews, ga:pageviews, ga:avgSessionDuration, ga:bounceRate, ga:percentNewSessions, ga:pageviewsPerVisit",
	max.results = 1000,
	table.id = paste("ga:",id, sep=""))

	ga.query <- QueryBuilder(query.list)
	ga.df <- GetReportData(ga.query, token)
	return(ga.df)
}

mobile.sum <- function(start.date, end.date, id){
	mobile = data.frame(matrix(NA, 1, 3))
	info = mobile(start.date, end.date, id)
	colnames(mobile) <- c("duration", "bounceRate", "pagesPerSession")
	mobile <- rbind(mobile, data.frame(duration= mean(info$avgSessionDuration), bounceRate=mean(as.numeric(info$bounceRate)), pagesPerSession=mean(as.numeric(info$pageviewsPerVisit))))
	mobile <- mobile[-1,]
	return(mobile)
}

device.info <- function(start.date, end.date, id) {
	devices = data.frame(matrix(NA, 1, 2))
	colnames(devices) <- c("device", "sessions")
	mobile.info <- mobile(start.date, end.date, id)
	devices <- rbind(devices, data.frame(device = "Galaxy", sessions= sum(mobile.info[grepl("Galaxy", mobile.info$mobileDeviceInfo),]$sessions)))
	devices <- rbind(devices, data.frame(device = "Motorola", sessions= sum(mobile.info[grepl("Motorola", mobile.info$mobileDeviceInfo),]$sessions)))
	devices <- rbind(devices, data.frame(device = "iPhone", sessions= sum(mobile.info[grepl("iPhone", mobile.info$mobileDeviceInfo),]$sessions)))
	devices <- rbind(devices, data.frame(device = "iPad", sessions= sum(mobile.info[grepl("iPad", mobile.info$mobileDeviceInfo),]$sessions)))
	devices <- rbind(devices, data.frame(device = "iPod", sessions= sum(mobile.info[grepl("iPod", mobile.info$mobileDeviceInfo),]$sessions)))
	devices <- rbind(devices, data.frame(device = "HTC", sessions= sum(mobile.info[grepl("HTC", mobile.info$mobileDeviceInfo),]$sessions)))
	devices <- rbind(devices, data.frame(device = "Nexus", sessions= sum(mobile.info[grepl("Nexus", mobile.info$mobileDeviceInfo),]$sessions)))
	devices <- rbind(devices, data.frame(device = "Kindle", sessions= sum(mobile.info[grepl("Kindle", mobile.info$mobileDeviceInfo),]$sessions)))
	return(na.omit(devices))
}

device.category <- function(start.date, end.date, id){
	token <- Auth("635724585936-c491melt43ktgdpsb9md09011q6ugih1.apps.googleusercontent.com", "ujtf4LufJvK-bK7yMeSbBP0U")
	query.list <- Init(
	start.date = start.date,
	end.date = end.date,
	dimensions = "ga:deviceCategory",
	metrics = "ga:sessions, ga:uniquePageviews, ga:pageviews, ga:avgSessionDuration, ga:bounceRate, ga:percentNewSessions, ga:pageviewsPerVisit",
	max.results = 1000,
	table.id = paste("ga:",id, sep=""))

	ga.query <- QueryBuilder(query.list)
	ga.df <- GetReportData(ga.query, token)
	return(ga.df)
}

vect <<- NULL

viewsvsmobile <- function(start.date, end.date, id){
	token <- Auth("635724585936-c491melt43ktgdpsb9md09011q6ugih1.apps.googleusercontent.com", "ujtf4LufJvK-bK7yMeSbBP0U")
	#query info from google analytics using date as the dimesion
	query.date <- Init(
		start.date = start.date,
		end.date = end.date,
		dimensions = "ga:date",
		metrics = "ga:sessions, ga:uniquePageviews, ga:pageviews, ga:avgSessionDuration, ga:bounceRate, ga:percentNewSessions",
		filters = "ga:deviceCategory%3D%3Ddesktop",
		max.results = 1000,
		table.id = paste("ga:",id, sep=""))
	#query info from google analytics using date as the dimension but filtering by mobile device
	query.mobile <- Init(
		start.date = start.date,
		end.date = end.date,
		dimensions = "ga:date",
		metrics = "ga:sessions, ga:uniquePageviews, ga:pageviews, ga:avgSessionDuration, ga:bounceRate, ga:percentNewSessions",
		filters = "ga:deviceCategory%3D%3Dmobile",
		max.results = 1000,
		table.id = paste("ga:",id, sep=""))
	#query info from google analytics using date as the dimension but filtering by mobile device
	query.tablet <- Init(
		start.date = start.date,
		end.date = end.date,
		dimensions = "ga:date",
		metrics = "ga:sessions, ga:uniquePageviews, ga:pageviews, ga:avgSessionDuration, ga:bounceRate, ga:percentNewSessions",
		filters = "ga:deviceCategory%3D%3Dtablet",
		max.results = 1000,
		table.id = paste("ga:",id, sep=""))
	#build queries for filtered and unfiltered parameters 
	ga.date <- QueryBuilder(query.date)
	ga.mobile <- QueryBuilder(query.mobile)
	ga.tablet <- QueryBuilder(query.tablet)
	#put report data into a data frame
	ga.dateDf <- cbind(GetReportData(ga.date, token)[,1:2], device= "Desktop")
	ga.mobileDf <- cbind(GetReportData(ga.mobile, token)[,1:2], device = "Mobile")
	ga.tabletDf <- cbind(GetReportData(ga.tablet, token)[,1:2], device = "Tablet")
	#combine report data and assign column names to combined data frame
	combine <- rbind(ga.dateDf, ga.mobileDf, ga.tabletDf)
	# colnames(combine) <- c(colnames(combine[1:7]), "mobileDate", "mobileSessions", "mobileUniques", "mobilePageViews", "mobileavgSession", "mobileBounceRate", "mobilePercentNewSessions")
	# data <- data.frame(cbind(combine$date, combine$sessions, combine$mobileSessions))
	combine[,1] <- as.Date(combine[,1], "%Y%m%d")
	combine[,2] <- as.numeric(as.character(combine[,2]))
	# #put percent of mobile users into an empty vector
	# for (i in 1:length(data[,1])) {
	# 	sum <- as.numeric(data[i,2]) + as.numeric(data[i,3])
	# 	vect <<- c(vect, as.numeric(data[i,3])/sum)
	# 	} 
	# #combine desktop and mobile visitors
	# desktop <- data.frame(date=data$X1, visits=data$X2, device='Desktop')
	# mobile <- data.frame(date=data$X1, visits=data$X3, device='Mobile')
	# allData <- rbind(desktop,mobile)
	return(combine)
}