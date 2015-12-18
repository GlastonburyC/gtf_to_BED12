
# convert 
# Setup dataframe columns for new variables
exon.lengths=NULL
exon.starts=NULL
meta.exons$exons.start=rep(NA)
meta.exons$No.exons=rep(NA)
meta.exons$exon.lengths=rep(NA)
gene.id=NULL

for(i in 1:nrow(meta.exons)){
	# If it's the first entry, record the gene.id
	if(i ==1){gene.id=meta.exons$V10[i]}

	if(meta.exons$V10[i]==gene.id){

	# set the first exon start site to zero for each gene
	if(length(exon.starts) < 1){ exon.starts <- append(exon.starts,'0')}

	# calculate the length of each exon
	exon.lengths <- append(exon.lengths,as.character((meta.exons$V5[i]-meta.exons$V4[i])))
	# record the start site of each exon
	exon.starts <- append(exon.starts,meta.exons$V4[i])

	} else{

	# when the gene.id changes, its a new gene - so summerize the previous genes information
	meta.exons$No.exons[i-1] <- as.character(length(exon.lengths))
	meta.exons$exon.lengths[i-1]<- paste(as.character(exon.lengths),collapse=", ")
	exon.starts<-exon.starts[-2]
	meta.exons$exons.start[i-1] <- paste(as.character(exon.starts),collapse=", ")
	# For the next gene set all vectors to NULL
	exon.starts=NULL; exon.lengths=NULL;
	gene.id=meta.exons$V10[i]
	# set gene.id to the new gene
	if(meta.exons$V10[i]==gene.id){
    # for the first entry - set the exon length, no of exons and start to 0.
	if(length(exon.starts) < 1){ exon.starts <- append(exon.starts,'0')}
	exon.starts <- append(exon.starts,meta.exons$V4[i])
	exon.lengths <- append(exon.lengths,as.character((meta.exons$V5[i]-meta.exons$V4[i])))

	if(i == nrow(meta.exons)){
		exon.starts = NULL
		exon.lengths = NULL
		exon.starts <- append(exon.starts,meta.exons$V4[i])
		exon.lengths <- append(exon.lengths,as.character((meta.exons$V5[i]-meta.exons$V4[i])))
		meta.exons$No.exons[i] <- as.character(length(exon.lengths))
		meta.exons$exon.lengths[i]<- paste(as.character(exon.lengths),collapse=", ")
		exon.starts<-exon.starts[-2]
		meta.exons$exons.start[i] <- paste(as.character(exon.starts),collapse=", ")

	}
}
}
}
