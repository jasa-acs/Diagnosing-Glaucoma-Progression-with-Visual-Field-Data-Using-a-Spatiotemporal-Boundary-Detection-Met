###Obtain Boolean of Unique Edge Locations
obtainBooleanEdges<-function(W) {
	W==1&!lower.tri(W)
}

###Metric Distance Function
pdist<-function(x,y) {
	pmin(abs(x-y),(360/scale_degree-pmax(x,y)+pmin(x,y)))
}

###Compute Dissimilarity Metric at Unique Edges
obtainDissimilarityMetric<-function(D,W) {
	
	##Compute Grid
	temp<-expand.grid(D,D)
	
	##Calculate Dissimilarity metric
	z<-pdist(temp[,1],temp[,2])
	z<-matrix(z,nrow=dim(W)[1],ncol=dim(W)[1],byrow=TRUE)
	keep<-obtainBooleanEdges(W)
	z<-z[keep]
	return(z)
}

