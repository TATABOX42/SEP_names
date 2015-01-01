######################################################################
######################################################################
# AUTHOR: BENJAMIN TOVAR 
# JULY, 2014
######################################################################
######################################################################

# ***********************
# LOAD DATABASE
# ***********************

sep.file <- "censocompleto2013v1.csv"
sep <- read.delim(sep.file,header=TRUE,sep=",")

# ***********************
# FIRST NAME FREQUENCIES ONLY
# ***********************

# get the names
persona_responsable.full.name <- as.character(sep$persona_responsable[])
# remove rows with empty values
persona_responsable.full.name <- persona_responsable.full.name[persona_responsable.full.name != ""]
# to get only the first name
persona_responsable.name <- unlist(lapply(strsplit(persona_responsable.full.name," "), function(x) x[1]))
# remove empty entries
persona_responsable.name <- persona_responsable.name[persona_responsable.name != ""]
# change some usual names like MA. to MARIA
persona_responsable.name[persona_responsable.name == "MA."] <- "MARIA"
persona_responsable.name[persona_responsable.name == "MA"] <- "MARIA"
persona_responsable.name[persona_responsable.name == "J."] <- "JOSE"

# ***********************
# WORDCLOUD: FIRST NAME FREQUENCIES ONLY
# ***********************

# load libraries
require(tm)
require(wordcloud)

pdf("wordcloud_responsible_name_only.pdf",width=6,height=6)
	wordcloud(names(table(persona_responsable.name)),
			  as.numeric(table(persona_responsable.name)), 
			 scale=c(8,0.2),min.freq=20, max.words=Inf,
			  random.order=FALSE, rot.per=.15,
			  col=brewer.pal(8,"Dark2") )
			  # col=rainbow(100))
title("Wordcloud of responsible name (name only)")
dev.off()

# *******************
# compute the percentage 
# of each name 
# *******************

persona_responsable.name.scaled <- (table(persona_responsable.name)/sum(table(persona_responsable.name)))*100

# sort(persona_responsable.name.scaled,decreasing=TRUE)[1:20]
# persona_responsable.name
#      MARIA       JOSE       JUAN        ANA     MARTHA       ROSA  FRANCISCO 
# 11.1212262  3.6335210  1.7089027  1.3784027  1.2957777  1.2027627  0.8534618 
#       LUIS      JESUS      JORGE      LAURA     MIGUEL  GUADALUPE    CLAUDIA 
#  0.8203129  0.7916168  0.7401617  0.7357088  0.6990966  0.6802957  0.6704005 
#     CARLOS      NORMA   PATRICIA       ALMA      JUANA     SILVIA 
#  0.6506100  0.6476415  0.6179558  0.6055868  0.5862912  0.5848069 

# the name MARIA represents the 11% of all names!

# ******************************
# COMPUTE THE TOP 30 MORE COMMON NAMES
# ******************************

n <- 30
top.names <- sort(table(persona_responsable.name),decreasing=TRUE)[1:n]
top.names.per <- sort(persona_responsable.name.scaled,decreasing=TRUE)[1:n]

require(ggplot2)

pdf("barplot_national_name_frequency.pdf",width=9,height=8)
	# BARPLOT OF COUNTS
	ggplot(data.frame(name=names(top.names),
					  frequency=as.numeric(top.names))) +
		  aes(x=name,y=frequency) + 
		  labs(title="Top 30 national name (frequency)") +
		  geom_bar(stat="identity",aes(fill=name,colour=name),alpha=0.3)  + 
		  coord_flip() + theme(legend.position="none")
dev.off()

pdf("barplot_national_name_percentage.pdf",width=9,height=8)
	# BARPLOT OF PERCENTAGE
	ggplot(data.frame(name=names(top.names.per),
					  percentage=as.numeric(top.names.per))) +
		  aes(x=name,y=percentage) + ylim(0,12) +
		  labs(title="Top 30 national name (percentage)") +
		  geom_bar(stat="identity",aes(fill=name,colour=name),alpha=0.3)  + 
		  coord_flip() + theme(legend.position="none")
dev.off()

# ******************************
# top first names per state
# ******************************

# get the names
persona_responsable.full.name <- as.character(sep$persona_responsable[])
# to get only the name
persona_responsable.name <- unlist(lapply(strsplit(persona_responsable.full.name," "), function(x) x[1]))
# join the values
persona_responsable.name.state <- data.frame(edo_en_mapa=sep$edo_en_mapa,
											 name=persona_responsable.name)

# > persona_responsable.name.state[1:5,]
#      edo_en_mapa    name
# 1 Aguascalientes   MARIA
# 2 Aguascalientes    JUAN
# 3 Aguascalientes ROSALES
# 4 Aguascalientes   JORGE
# 5 Aguascalientes   RAMON

# remove rows with empty first names (to remove states without complete data)
persona_responsable.name.state <- persona_responsable.name.state[persona_responsable.name.state[,2] != "",]
# remove rows with empty first names (just the vector of first names)
persona_responsable.name <- persona_responsable.name[persona_responsable.name != ""]
# change some usual names like MA. to MARIA
persona_responsable.name[persona_responsable.name == "MA."] <- "MARIA"
persona_responsable.name[persona_responsable.name == "MA"] <- "MARIA"
persona_responsable.name[persona_responsable.name == "J."] <- "JOSE"
# merge the data (with the curated first names)
persona_responsable.name.state[,2] <- persona_responsable.name
# clean the data
persona_responsable.name.state <- persona_responsable.name.state[complete.cases(persona_responsable.name.state),]

# > dim(persona_responsable.name.state)
# [1] 202118      2

# ******************************
# top 20 national first names
# ******************************

# extract the top nation names
n <- 20
top.names <- names(sort(table(persona_responsable.name.state[,2]),decreasing=TRUE))[1:n]
# extract the name of each state
state_name <- levels(sep$edo_en_mapa)
state_and_top_names <- matrix(0,nr=length(state_name),nc=n)
rownames(state_and_top_names) <- state_name
colnames(state_and_top_names) <- top.names
# populate the matrix
for (i in 1:length(state_name)) {
	for (j in 1:n) {
		state_and_top_names[i,j] <- length(persona_responsable.name.state[as.character(persona_responsable.name.state[,1]) == state_name[i] & persona_responsable.name.state[,2] == top.names[j],1] )
	}
	cat("state",i,"\n")
}

# ***************
# plot a heatmap of the results
# ***************

library(pheatmap)

pdf("heatmap_state_and_top_names.pdf",width=9,height=8)
	# this plot help us to study the distribution of the 20 most common names
	# and their distribution across the 32 states
	pheatmap(as.matrix(state_and_top_names),col=color,
					 main="heatmap of distribution of top 20\nnational names per state",
					 cexCol=0.7,cexRow=0.7)	
dev.off()

# *****************************************
# STATE VS STATE CLUSTERING TOP 1000 first NAMES
# *****************************************

# extract the top nation names
n <- 1000
top.names <- names(sort(table(persona_responsable.name.state[,2]),decreasing=TRUE))[1:n]
# extract the name of each state
state_name <- levels(sep$edo_en_mapa)
state_and_top_names.big <- matrix(0,nr=length(state_name),nc=n)
rownames(state_and_top_names.big) <- state_name
colnames(state_and_top_names.big) <- top.names
# populate the matrix
for (i in 1:length(state_name)) {
	for (j in 1:n) {
		state_and_top_names.big[i,j] <- length(persona_responsable.name.state[as.character(persona_responsable.name.state[,1]) == state_name[i] & persona_responsable.name.state[,2] == top.names[j],1] )
	}
	cat("state",i,"\n")
}

# *************************
# Scale the numbers
# *************************

state_and_top_names.big.scaled <- apply(state_and_top_names.big,1, function(x) x/sum(x) )
state_and_top_names.big.scaled <- t(state_and_top_names.big.scaled)

# *************************
# STATE VS STATE CLUSTERING
# *************************

state_and_top_names.d <- dist(state_and_top_names.big.scaled)
state_and_top_names.d <- as.matrix(state_and_top_names.d)
# scale the distance values
state_and_top_names.d <- state_and_top_names.d/max(state_and_top_names.d)

# save the objects
# save(list=c("state_and_top_names.big","state_and_top_names.d"),file="state_and_top_names.RData")

# load the region data
name_region <- read.delim("state_region.csv",header=TRUE)
region <- data.frame(Region=name_region$region)
rownames(region) <- rownames(state_and_top_names.d)

# ***************
# plot a heatmap
# ***************

library(pheatmap)

pdf("heatmap_state_distance_1000_top_names.pdf",width=9,height=8)
	# this plot help us to identify clusters of similarities among the states
	# to share the same quantities of names, in other words, cluster the states
	# in terms of similarity of names distribution
	pheatmap(as.matrix(state_and_top_names.d),
					 main="heatmap of state-state distance of top 1000 national names",
					 cexCol=0.7,cexRow=0.7,annotation=region)
dev.off()

# #######################
# #######################
# COMPLETE NAME ANALYSIS
# #######################
# #######################

# crawl the names
persona_responsable.full.name <- as.character(sep$persona_responsable[])
# remove the empty names
persona_responsable.full.name <- persona_responsable.full.name[persona_responsable.full.name != ""]

# split the strings TO GET THE FULL NAME
persona_responsable.full.name <- unlist(strsplit(persona_responsable.full.name," "))
# remove empty entries
persona_responsable.full.name <- persona_responsable.full.name[persona_responsable.full.name != ""]
# remove composed names with words like DE, DEL, LA
persona_responsable.full.name <- persona_responsable.full.name[persona_responsable.full.name != "DE"]
persona_responsable.full.name <- persona_responsable.full.name[persona_responsable.full.name != "DEL"]
persona_responsable.full.name <- persona_responsable.full.name[persona_responsable.full.name != "LA"]
# change some usual names like MA. to MARIA
persona_responsable.full.name[persona_responsable.full.name == "MA."] <- "MARIA"
persona_responsable.full.name[persona_responsable.full.name == "MA"] <- "MARIA"
persona_responsable.full.name[persona_responsable.full.name == "J."] <- "JOSE"


# load libraries
require(tm)
require(wordcloud)

pdf("wordcloud_responsible_name.pdf",width=8,height=8)
	wordcloud(names(table(persona_responsable.full.name)),
			  as.numeric(table(persona_responsable.full.name)), 
			 scale=c(8,0.2),min.freq=50, max.words=Inf,
			  random.order=FALSE, rot.per=.15,
			  col=brewer.pal(8,"Dark2") )
			  # col=rainbow(100))
title("Wordcloud of responsible name (complete name)")
dev.off()


# *******************
# compute the percentage of each name 
# *******************

persona_responsable.full.name.scaled <- (table(persona_responsable.full.name)/sum(table(persona_responsable.full.name)))*100

# sort(persona_responsable.full.name.scaled,decreasing=TRUE)[1:20]
# persona_responsable.full.name.scaled
#     MARIA HERNANDEZ    GARCIA  MARTINEZ     LOPEZ  GONZALEZ      JOSE RODRIGUEZ 
# 3.7961524 2.0771773 1.6185485 1.4873302 1.2833696 1.2281420 1.1438176 1.0993248 
#     PEREZ   SANCHEZ GUADALUPE      CRUZ   RAMIREZ     JESUS    FLORES      JUAN 
# 1.0109043 0.9552530 0.8757310 0.8720585 0.8579338 0.6613181 0.6490296 0.5538292 
#     GOMEZ    CARMEN     REYES      ROSA 
# 0.5351846 0.5279810 0.4925280 0.4868782 

# the name MARIA represents the 4% of all words in responsible names!

# ******************************
# top 30 most frequent words in the complete name columns
# ******************************
n <- 30
top.names <- sort(table(persona_responsable.full.name),decreasing=TRUE)[1:n]
top.names.per <- sort(persona_responsable.full.name.scaled,decreasing=TRUE)[1:n]

pdf("barplot_national_complete_name_frequency.pdf",width=9,height=8)
	# BARPLOT OF COUNTS
	ggplot(data.frame(name=names(top.names),
					  frequency=as.numeric(top.names))) +
		  aes(x=name,y=frequency) + 
		  labs(title="Top 30 national complete name words (frequency)") +
		  geom_bar(stat="identity",aes(fill=name,colour=name),alpha=0.3)  + 
		  coord_flip() + theme(legend.position="none")
dev.off()

pdf("barplot_national_complete_name_percentage.pdf",width=9,height=8)
	# BARPLOT OF PERCENTAGE
	ggplot(data.frame(name=names(top.names.per),
					  percentage=as.numeric(top.names.per))) +
		  aes(x=name,y=percentage) + ylim(0,12) +
		  labs(title="Top 30 national complete name words (percentage)") +
		  geom_bar(stat="identity",aes(fill=name,colour=name),alpha=0.3)  + 
		  coord_flip() + theme(legend.position="none")
dev.off()


