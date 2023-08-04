library(tm)

lit <- read.csv(file.choose())

names(lit)
lit_abs <- lit["Abstract"]
length(t(lit_abs))

corp <- VCorpus((VectorSource(t(lit_abs))))



tdm <- TermDocumentMatrix(corp, control = list(removePunctuation = TRUE, stopwords = TRUE))
dtm <- DocumentTermMatrix(corp, control = list(removePunctuation = TRUE, stopwords = TRUE))
dtm <- DocumentTermMatrix(corp,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))
dim(tm::inspect(dtm))
hcdf <- data.frame(tm::inspect(dtm))
class(hcdf)
head(hcdf)
is.data.frame(hcdf)

# library(wordcloud)
# freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
# wordcloud(names(freq), freq, min.freq=400, max.words=Inf, random.order=FALSE, colors=brewer.pal(8, "Accent"), scale=c(7,.4), rot.per=0)

names(hcdf)

library(lavaan)
library(semPlot)
library(semptools)

names(hcdf)

model <- '
        # covid =~ sars.cov.2 
        # healthcare =~ health + care 
        # immunity =~ vaccine + vaccination
        
        # direct effect
              sars.cov.2 ~ c*health
                 
        # mediator
              vaccine ~ a*health
              sars.cov.2 ~ b*vaccine
              
        # indirect effect (a*b)
              ab := a*b
        # total effect
              total := c + (a*b)
'

fit <- sem(model, hcdf)
summary(fit, fit.measures = TRUE)
write.csv(parameterEstimates(fit), 'covid_med_model.csv')
write.csv(fitmeasures(fit), 'fit_measures_covid_med_model.csv')

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
semPaths(fit)
p_pa <- semPaths(fit, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = 1.15,
                 style = "ram",
                 nCharNodes = 0, nCharEdges = 0) 
p_pa2 <- mark_sig(p_pa, fit)
plot(p_pa2)