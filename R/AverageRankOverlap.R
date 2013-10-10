# Average Rank Overlap
#
# This implementation is a modified version of the rank overlap metric
# defined in the paper "A Similarity Measure for Indefinite Rankings"
# by Webber et al. You can find the paper at this url:
# http://www.umiacs.umd.edu/~wew/papers/wmz10_tois.pdf
#
# Author: Fatih Sunor

###############################################################################

score <- function(l1,l2,depth=length(l1)){
	
	if(length(l1)==0){
		print("First list is empty")
		return(NA)
	}
	
	if(length(l2)==0){
		print("Second list is empty")
		return(NA)
	}
	
	if(length(l1)!=length(l2)){	
		print("Lists must be equal size")
		return(NA)
	}
	
	if(depth>length(l1)){	
		print("Depth must be less than or equal to the length of either list")
		return(NA)
	}
	
	sum<-0
	
	depthlevels<-seq(1,depth)
	
	for(i in depthlevels){	
		sum = sum + length(intersect(l1[1:i],l2[1:i]))/i
	}
	
	return(sum/depth)
}



avgrankoverlap<-function(items,settings=items[,2],key=items[,1]){
	
	meanOverlap<-matrix(0,nrow=length(settings),ncol=length(settings))
	
	for(k in unique(key)){
		for(s in 1:length(unique(settings))){
			for(j in 1:s){
				if(s==j){
					meanOverlap[s,j]<-meanOverlap[s,j]+1
				}else{
					sitems<-items[items[,2]==settings[s]&items[,1]==k,3:dim(items)[2]]
					jitems<-items[items[,2]==settings[j]&items[,1]==k,3:dim(items)[2]]
					scoresj<-score(sitems,jitems)
					meanOverlap[s,j]<-meanOverlap[s,j]+scoresj
					meanOverlap[j,s]<-meanOverlap[j,s]+scoresj
				}
			}
		}
	}
	
	meanOverlap<-meanOverlap/length(unique(key))
	colnames(meanOverlap)<-settings
	rownames(meanOverlap)<-settings
	return(meanOverlap)
	
}