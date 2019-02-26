###Function to Create W(alpha)
W.alpha<-function(alpha) {
		
	##Compute Proposal Adjacency Matrix		
	UpdatedAdjacentEdges<-1*(exp(-Z*alpha)>=0.5) #Lee and Mitchell 2011
	Wnew<-Wfixed
	Wnew[AdjacentEdgesBoolean]<-UpdatedAdjacentEdges
	WnewTranspose<-t(Wnew)
	WnewTranspose[AdjacentEdgesBoolean]<-UpdatedAdjacentEdges
	return(WnewTranspose)
		
}