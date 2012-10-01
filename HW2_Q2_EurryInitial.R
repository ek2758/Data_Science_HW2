####2. NYC Housing: Linear regression.
write.table(mnhttn_sls_clean, "mnhttn_sls_clean.csv", sep=",")
mnhttn_sls_clean <- read.csv("mnhttn_sls_clean.csv", stringsAsFactors=FALSE)
colnames(mnhttn_sls_clean)
summary(mnhttn_sls_clean)
getwd()

mnhttn_sls_clean$nhood <- gsub(" ","", mnhttn_sls_clean$NEIGHBORHOOD, fixed=TRUE)

#Code categorical variables.
mnhttn_sls_clean <- within(mnhttn_sls_clean, {
    nhood <- factor(nhood)
    BUILDING.CLASS.CATEGORY <- factor(BUILDING.CLASS.CATEGORY)
    TAX.CLASS.AT.PRESENT <- factor(TAX.CLASS.AT.PRESENT)
    BUILDING.CLASS.AT.TIME.OF.SALE <- factor(BUILDING.CLASS.AT.TIME.OF.SALE)
    ZIP.CODE <- factor(ZIP.CODE)
    month_yr <- factor(month_yr, levels=c('82011','92011','102011','112011','122011',
                                          '12012','22012','32012','42012','52012',
                                          '62012','72012'))
})

#Find potential predictors.
require(ggplot2)
require(scales) #To customize axes labels.

#Create subsets of data to separate units information.
subset_totunits <- subset(mnhttn_sls_clean, 
                          is.na(TOTAL.UNITS)==FALSE,
                          select = c(SALE.PRICE, TOTAL.UNITS))
subset_resunits <- subset(mnhttn_sls_clean, 
                          is.na(RESIDENTIAL.UNITS)==FALSE,
                          select = c(SALE.PRICE, RESIDENTIAL.UNITS))
subset_communits <- subset(mnhttn_sls_clean, 
                          is.na(COMMERCIAL.UNITS)==FALSE,
                          select = c(SALE.PRICE, COMMERCIAL.UNITS))
summary(subset_totunits)
colnames(subset_resunits)

#Merge subsets of data.
units_subsets <- cbind(subset_totunits, subset_resunits[,2], subset_communits[,2])
colnames(units_subsets) <- c("saleprc","tot_units","res_units","comm_units")
units_subsets$rownum=as.numeric(row.names(units_subsets))
summary(units_subsets)

require(reshape2)
units_subsets_org <- melt(units_subsets, 
                          id=c("rownum","saleprc"))


#Linear regression: SALE.PRICE as a function of TOTAL.UNITS
mnhttn_sls_lm <- lm(log(SALE.PRICE) ~ log(TOTAL.UNITS),
                    data = subset(mnhttn_sls_clean, SALE.PRICE>0 & TOTAL.UNITS>0))
coef(mnhttn_sls_lm)
summary(mnhttn_sls_lm) #Coefficient is 12 SEs away from zero -- reasonable relation.

#Linear regression: SALE.PRICE as a function of RESIDENTIAL.UNITS
mnhttn_sls_resunits_lm <- lm(log(SALE.PRICE) ~ log(RESIDENTIAL.UNITS),
                    data = subset(mnhttn_sls_clean, SALE.PRICE>0 & RESIDENTIAL.UNITS>0))
coef(mnhttn_sls_resunits_lm)
summary(mnhttn_sls_resunits_lm) #Coefficient is 13.7 SEs away from zero -- reasonable relation.

#Linear regression: SALE.PRICE as a function of COMMERCIAL.UNITS
mnhttn_sls_communits_lm <- lm(log(SALE.PRICE) ~ log(COMMERCIAL.UNITS),
                             data = subset(mnhttn_sls_clean, SALE.PRICE>0 & COMMERCIAL.UNITS>0))
coef(mnhttn_sls_communits_lm)
summary(mnhttn_sls_communits_lm) #Coefficient is 21 SEs away from zero -- best relation so far!

#Label Units.
unit_label <- function(var, value){
    value <- as.character(value)
    if (var=="variable") { 
        value[value=="comm_units"] <- "Commercial Units"
        value[value=="res_units"] <- "Residential Units"
        value[value=="tot_units"] <- "Total Units"
    }
    return(value)
}

#Draw scatterplot of commercial, residential, and total units against log of sales prices.
units_subsets_org$variable <- factor(units_subsets_org$variable, 
                                     levels = c("comm_units",
                                                "res_units",
                                                "tot_units")) 

getwd()
postscript(file="MnhttnSls_3UnitTyps_Scttr.eps", #Save graph to EPS file.
           onefile=FALSE, 
           width=6,
           height=6,
           horizontal=FALSE)
ggplot(subset(units_subsets_org, log(value)>0 & log(saleprc)>0), 
       aes(x=log(value), y=log(saleprc))) +
           geom_point(shape=1) +
           geom_smooth(method='lm', se=FALSE) +
           facet_grid(variable ~ ., labeller=unit_label) +
           scale_x_log10(name="Log(Units in Property)",
                              breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", math_format())) +
           scale_y_log10(name="Log(Sale Price)",
                         breaks = trans_breaks("log10", function(x) 10^x),
                         labels = trans_format("log10", math_format()))
dev.off()

#Create subsets of data to separate square footage information.
subset_landsqft <- subset(mnhttn_sls_clean, 
                          is.na(LAND.SQUARE.FEET)==FALSE,
                          select = c(SALE.PRICE, LAND.SQUARE.FEET))

#Draw scatterplot of land square feet against sale price.
postscript(file="MnhttnSls_LandSqFt_Scttr.eps", #Save graph to EPS file.
           onefile=FALSE, 
           width=6,
           height=2,
           horizontal=FALSE)
ggplot(subset(subset_landsqft, log(LAND.SQUARE.FEET)>0 & log(SALE.PRICE)>0), 
       aes(x=log(LAND.SQUARE.FEET), y=log(SALE.PRICE))) +
           geom_point(shape=1) +
           geom_smooth(method='lm', se=FALSE) +
           scale_x_log10(name="Log(Land, Ft^2)",
                         breaks = trans_breaks("log10", function(x) 10^x),
                         labels = trans_format("log10", math_format())) +
           scale_y_log10(name="Log(Sale Price)",
                         breaks = trans_breaks("log10", function(x) 10^x),
                         labels = trans_format("log10", math_format()))
dev.off()

#Linear regression: SALE.PRICE as a function of LAND.SQUARE.FEET
mnhttn_sls_landsqft_lm <- lm(log(SALE.PRICE) ~ log(LAND.SQUARE.FEET),
                    data = subset(mnhttn_sls_clean, SALE.PRICE>0 & LAND.SQUARE.FEET>0))
coef(mnhttn_sls_landsqft_lm)
summary(mnhttn_sls_landsqft_lm) #adj R^2 .228 <- Larger! But bad predictor!
#Just barely above 3. Not confident in relations.

#Linear regression: SALE.PRICE as a function of LAND.SQUARE.FEET
mnhttn_sls_zip_lm <- lm(log(SALE.PRICE) ~ ZIP.CODE,
                             data = subset(mnhttn_sls_clean, SALE.PRICE>0))
coef(mnhttn_sls_zip_lm)
summary(mnhttn_sls_zip_lm) #adj R^2 .42 <- zip code is not bad!

#Linear regression: SALE.PRICE as a function of YEAR.BUILT
mnhttn_sls_yrblt_lm <- lm(log(SALE.PRICE) ~ YEAR.BUILT,
                        data = subset(mnhttn_sls_clean, SALE.PRICE>0))
coef(mnhttn_sls_yrblt_lm)
summary(mnhttn_sls_yrblt_lm) #adj R^2 .11 <- Meh -- not the best, but 45 t-value!

#Draw bargraph of SALE.PRICE by YEAR.BUILT
postscript(file="MnhttnSls_YrBlt_Bar.eps", #Save graph to EPS file.
           onefile=FALSE, 
           width=6,
           height=2,
           horizontal=FALSE)
ggplot(subset(mnhttn_sls_clean, YEAR.BUILT>1900 & log(SALE.PRICE)>0), 
       aes(x=YEAR.BUILT, y=log(SALE.PRICE))) +
           geom_bar(stat="identity") +
           scale_x_continuous(name="Year Built") +
           scale_y_log10(name="Log(Sale Price)",
                         breaks = trans_breaks("log10", function(x) 10^x),
                         labels = trans_format("log10", math_format()))
dev.off()

#Using most confident correlation (Commercial Units and Sales Price), 
#how does the addition of zip code fare? Year Built?
mnhttn_sls_multi_lm <- lm(log(SALE.PRICE) ~ log(COMMERCIAL.UNITS) + 
                                              ZIP.CODE + 
                                              YEAR.BUILT,
                              data = subset(mnhttn_sls_clean, 
                                            SALE.PRICE>0 & COMMERCIAL.UNITS>0 & !is.na(YEAR.BUILT>0)))
coef(mnhttn_sls_multi_lm)
summary(mnhttn_sls_multi_lm)
#BLOCK was not significant and R^2 value only by .001
#ZIP.CODE was a better predictor than NEIGHBORHOOD.

#Visualize residuals of regression.
postscript(file="MnhttnSls_MultiLM.eps", #Save graph to EPS file.
           onefile=FALSE, 
           width=12,
           height=12,
           horizontal=FALSE)
op <- par(mfrow = c(2, 2), 
          pty = "s")  
plot(mnhttn_sls_multi_lm)
dev.off()

####2. NYC Housing Data: K-nearest neighbors.
require(class)
require(MASS)

#Subset columns for training and testing samples.
subset_mnhttn <- subset(mnhttn_sls_clean, 
                        is.na(COMMERCIAL.UNITS)==FALSE &
                        is.na(YEAR.BUILT)==FALSE &
                        is.na(ZIP.CODE)==FALSE &
                        is.na(nhood)==FALSE,    
                        select=c("COMMERCIAL.UNITS",
                                 "YEAR.BUILT",
                                 "ZIP.CODE",
                                 "nhood"))
head(subset_mnhttn)
nrow(subset_mnhttn)/2

#Implement random number generator to create training and testing samples.
s <- sample(7006, 3503)
train_mnhttn <- subset_mnhttn[s,] 
test_mnhttn <- subset_mnhttn[-s,]

train_mnhttn_vbls <- train_mnhttn[,1:2]
test_mnhttn_vbls <- test_mnhttn[,1:2]
train_mnhttn_nhood <- train_mnhttn[,3]
test_mnhttn_nhood <- test_mnhttn[,3]

#Implement K-nearest neighbors.
predict_nhood <- knn(train_mnhttn_vbls, test_mnhttn_vbls, train_mnhttn_nhood, k=5)

sum(predict_nhood != test_mnhttn_nhood)/length(test_mnhttn_nhood)

#LDA analysis.
nhood_lda = lda(nhood ~ .,data=subset_mnhttn)
pred_lda = predict(nhood_lda,subset_mnhttn)
names(pred_lda)
confusion_nhood <- table(subset_mnhttn$nhood,pred_lda$class)
confusion_nhood_projected <- as.data.frame(prop.table(table(subset_mnhttn$nhood,
                                                            pred_lda$class),1))
colnames(confusion_nhood_projected)
postscript(file="Confusion_nhood.eps", #Save graph to EPS file.
           onefile=FALSE, 
           width=3,
           height=3,
           horizontal=FALSE)
plot <- ggplot(confusion_nhood_projected)
plot + geom_tile(aes(x=Var2, y=Var1, fill=Freq)) + 
    scale_x_discrete(name="Actual Class") + 
    scale_y_discrete(name="Predicted Class") +
    scale_fill_gradient(breaks=seq(from=-0, to=.8, by=.1)) + 
    labs(fill="Normalized\nFrequency")
dev.off()
