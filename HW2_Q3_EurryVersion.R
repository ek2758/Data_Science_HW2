#Load JSON package.
require(RJSONIO)
require(foreach)
require(plyr)

#Define facets to search.
facets=list("Arts","Business","Obituaries","Sports","World")
queries <- list()

#Define parameters.
api <- "687d601e0e5d64f3ea200c0257002ebf:1:44679323" #Eurry's API-key
api <- "edf9f44cfdde2bc23a53be2830873ce7:11:62471970" #Jed's API-key
facet <- vector("list",length(facets))
records <- 2000
os <- 0:(records/10-1)
fields <- list("url%2Ctitle%2Cbody")
rank <- "newest"

#Set up query output table.
articles <- list()

#Check for missing bodys in articles. 
addBody <- c("body",NA)
fixRow <- function(x) {
    inAdeq <- ldply(x[[rowNum]])
    rePlace <- t(rbind(addBody, inAdeq))
    toList <- rePlace[-1,1:3]
    names(toList) <- c("body","title","url")
    return(toList)
}

#Loop to insert facets.
query <- foreach(i= 1:length(facets))  %do%
    sprintf("nytd_section_facet%%3A%%5B%s%%5D", facets[[i]])
Queries <- ldply(query) #hold query sections for use in URI construction.

# Gather All articles and transform them into dataframes.
for(j in 1:5){
    for (i in 1:length(os)) {
        uri <- paste ("http://api.nytimes.com/svc/search/v1/article?format=json&query=", Queries[j,],
                      "&offset=", os[i], 
                      "&fields=", fields,
                      "&rank=", rank,
                      "&api-key=", api,
                      sep="")
        data <- readLines(uri, warn="F") # get them
        trnslt  <- fromJSON(data) # tokenize
        rowNum <- which(sapply(trnslt$results,length)<3, arr.ind=TRUE) #returns row w/ missing body.
        #Run fixRow function to insert NA in missing body column.
        if(sum(rowNum, na.rm=TRUE)>0) {
            z<-fixRow(trnslt$results)
            trnslt$results[[rowNum]] <- z}
        articles <- append(articles, unlist(trnslt$results))
        Sys.sleep(.1)
    }
    assign(paste("articles_",facets[[j]],sep=""),data.frame(matrix(unlist(articles), nrow=2000,byrow=T))) 
    rm(articles)
    articles <- list()
}

#Add descriptive row to articles
articles_Arts$section <- apply(articles_Arts,1,function(row) "Arts")
articles_Business$section <- apply(articles_Business,1,function(row) "Business")
articles_Obituaries$section <- apply(articles_Obituaries,1,function(row) "Obituaries")
articles_Sports$section <- apply(articles_Sports,1,function(row) "Sports")
articles_World$section <- apply(articles_World,1,function(row) "World")

library('RTextTools')
library('e1071')
library('SparseM')
library('tm')
library('ggplot2')

#Combines the articles into one table.
#We need to do this because we are training the 
#machine to compare articles to eachother.
articles_All <- rbind(articles_Arts,articles_Business,
                      articles_Obituaries,articles_Sports,articles_World)

#Next we do this cute trick to sample two random and independent sections of the data.
#I also took sample t as a subset of the test data so I wouldn't have to wait so long
#for it to run.
s <- sample(10000, 5000)
t <- sample(5000, 100)
train <- articles_All[s,] 
test <- articles_All[-s,] 
littletest <- test[t,]

#Creates the training matrix with the listed specifications

trainmatrix <- create_matrix(cbind(train["X1"],train["X2"]), language="english",
                             removeNumbers=TRUE, removePunctuation=TRUE, 
                             removeStopwords=TRUE,
                             stemWords=TRUE, removeSparseTerms = .998)

#Creates the test matrix with the listed specifications

testmatrix <- create_matrix(cbind(test["X1"],test["X2"]), language="english",
                            removeNumbers=TRUE, removePunctuation=TRUE,
                            removeStopwords=TRUE,
                            stemWords=TRUE, removeSparseTerms = .998)

#Creates a little test matrix

little_test_matrix <- create_matrix(cbind(littletest["X1"],littletest["X2"]), language="english",
                            removeNumbers=TRUE, removePunctuation=TRUE,
                            stemWords=TRUE, removeSparseTerms = .998)

#This trains the model using Naive Bayes and 5000 test rows.
#model <- naiveBayes(as.matrix(trainmatrix),as.factor(train$newcol))
model <- naiveBayes(as.matrix(trainmatrix), as.factor(train$section))
summary(model)

#This provides the probabilities for each section using test data.
#To run for full results instead of the small set, rename 
#little_test_matrix to testmatrix
#WARNING THIS TAKES A LONG TIME AND MAKES YOUR COMPUTER GET REALLY HOT

detailed_results <- predict(model,as.matrix(testmatrix),type="raw")
View(detailed_results)

#This provides only the most likely section for each row of your data
results <- predict(model,as.matrix(testmatrix))
View(results)

#This creates a confusion table that looks at the actual data and compares
#it to the predicted class
comparison_table <- table(results, test$section)
comparison_table

#This plots the confusion table in a heatmap using ggplots.
#Names the rows and columns
colnames(input.matrix.normalized) = c("Arts", "Business", "Obituaries", "Sports", "World")
rownames(input.matrix.normalized) = colnames(input.matrix.normalized)

#Turns the table to various data frames
#this one is the raw table numbers
confusion <-as.data.frame(table(results,test$section))
#this one is normalized based on the number of projections in each category
confusion_normalized_projected <- as.data.frame(prop.table(table(results,test$section),1))
#this one is normalized based on the actual number in each category
confusion_normalized_actual <- as.data.frame(prop.table(table(results,test$section),2))
#plots the data frame, in this case for confusion_normalized_projected
postscript(file="Confusion_NYT.eps", #Save graph to EPS file.
           onefile=FALSE, 
           width=3,
           height=3,
           horizontal=FALSE)
    plot <- ggplot(confusion_normalized_projected)
    plot + geom_tile(aes(x=Var2, y=results, fill=Freq)) + 
        scale_x_discrete(name="Actual Class") + 
        scale_y_discrete(name="Predicted Class") +
        scale_fill_gradient(breaks=seq(from=-0, to=.8, by=.1)) + 
        labs(fill="Normalized\nFrequency")
dev.off()
