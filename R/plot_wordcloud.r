#With thanks to Rui Miguel Forte, Mastering Predictive Analytics with R - Page 336 

plot_wordcloud <- function(model, myDtm, k, numTerms) {
	require(wordcloud)
	require(RColorBrewer)
	par(mfrow=c(round(k/3),3))
	for (index in 1:k) {
		model_terms <- terms(model, numTerms)
		model_topics <- topics(model)
		terms_i <- model_terms[,index]
		topic_i <- model_topics == index
		dtm_i <- myDtm[topic_i, terms_i]
		frequencies_i <- colSums(as.matrix(dtm_i))
		pal2 <- brewer.pal(8,"Dark2")
		wordcloud(terms_i, frequencies_i, min.freq = 0, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
	}
	par(mfrow=c(1,1))
}
