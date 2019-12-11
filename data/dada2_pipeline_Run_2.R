#This pipeline assumes paired-end fastq files have been demultiplexed by sample
#Barcodes/adapters should have already been removed

library(dada2)

#Directory containing the fastq files
path <- "Desktop/stripsR/sequence-data/KMou_STRIPS_Run_2"
list.files(path)

#Forward and reverse fastq filenames have format: SAMPLENAME_R1_001.fastq and SAMPLENAME_R2_001.fastq
fnFs <- sort(list.files(path, pattern = "_R1_001.fastq", full.names = TRUE))
fnRs <- sort(list.files(path, pattern = "_R2_001.fastq", full.names = TRUE))

#Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
#Removes everything after SAMPLENAME
sample.names <- sapply(strsplit(basename(fnFs), "_"), function(x) paste(x[1], x[2], sep = "_"))

#Inspect read quality profiles
#Heat maps of the frequency of each quality score at each base position, median quality score shown by green line
plotQualityProfile(fnFs[1:2])
plotQualityProfile(fnRs[1:2])

#Filter and trim
#Place filtered files in filtered/subdirectory
filtFs <- file.path(path, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(path, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))

names(filtFs) <- sample.names
names(filtRs) <- sample.names

#Use standard filtering parameters: maxN = 0, truncQ = 2, rm.phix = TRUE, and maxEE = 2
#Truncate reads at 160 and 240
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen = c(240,160),
                     maxN = 0, maxEE = c(2,2), truncQ = 2, rm.phix = TRUE, compress = TRUE, multithread = TRUE)

#Save 'out' file as excel file
write.csv(out, file = "Desktop/stripsR/STRIPS_Run_2_out.csv")

#On Windows set multithread = FALSE

#Filter anything less than 2000 reads and greater than 80000 based on out table produced (based on 'out' file)
filtFs2 <- filtFs[out[,2] >= 2000 & out[,2] <= 80000]
filtRs2 <- filtRs[out[,2] >= 2000 & out[,2] <= 80000]

#Learns error model from the data by estimating the error rates and inferenec of sample composition
#until they converge on a jointly consistent solution
errF <- learnErrors(filtFs2, multithread = TRUE)
#100974720 total bases in 420728 reads from 21 samples will be used for learning the error rates

errR <- learnErrors(filtRs2, multithread = TRUE)
#100240800 total bases in 626504 reads from 30 samples will be used for learning the error rates

#Visualize estimated error rates
#Error rates for each possible transition (A->C, A->G, ...) are shown
#Black line shows the estimated error rates after convergence of the machine-learning algorithm
#Red line shows the error rates expected under the nominal definition of the Q-score
#Estimated error rates (black line) are a good fit to the observed rates (points)
#Error rates drop with increased quality as expected
plotErrors(errF, nominalQ = TRUE)

# ######https://github.com/benjjneb/dada2/issues/710 trouble shooting: Jared added
# exists <- file.exists(filtFs)
# exists
# filtFs <- filtFs[exists]
# filtFs

#Sample Inference
#Apply the core sample inferene algorithm to the filtered and trimmed sequence data
dadaFs <- dada(filtFs2, err=errF, multithread=TRUE)

#Inspecting the returned dada-class object
dadaFs[[1]]
#In the first sample DADA2 inferred 1219 sequence variants from 9657 unique sequences

dadaRs <- dada(filtRs2, err=errR, multithread=TRUE)

#Inspecting the returned dada-class object
dadaRs[[1]]
#In the first sample DADA2 inferred 888 sequence variants from 8383 input unique sequences

save.image("Desktop/stripsR/sequence-data/KMou_STRIPS_Run_2/dada.RData")

#Merge paired reads
#Merge the forward and reverse reads together to obtain the full denoised sequences
#Merging is performed by aligning the denoised forward reads with the reverse-complement
#By default merged sequences ar only output if the forward and reverse reads overlap by at least 12 bases
#and are identical to each other in the overlap region
mergers <- mergePairs(dadaFs, filtFs2, dadaRs, filtRs2, verbose=TRUE)

#The mergers object is a list of data.frames from each sample
#Each data.fram contains the merged sequence, its abundance,
#and the indicies of the forward and the reverse sequenced variants that were merged
#Paired reads that do not exactly overlap are removed by mergePairs

head(mergers[[1]])

#Construct the sequence table
#ASV table is a higher-resolution version of the OTU table produced by mothur
seqtab <- makeSequenceTable(mergers)

#Returns the dimension (# of columns x # of rows)
dim(seqtab)
# 430 46270

table(nchar(getSequences(seqtab)))

#Inspect distribution of sequence lengths
dist_seq_length <- table(nchar(getSequences(seqtab)))

barplot(dist_seq_length)

#Sequences that are much longer or shorter than expected may be the result of non-specific priming
#You can remove non-target-length sequences from your sequence table
#This is analogous to “cutting a band” in-silico to get amplicons of the targeted length
#Target length of V4 region of 16S should be ~250 bp
seqtab2 <- seqtab[,nchar(colnames(seqtab)) %in% 251:260]

#Remove chimeras
#Chimeric sequences are identified if they can be exactly reconstructed by combining
#a left-segment and a right-segment from two more abundant “parent” sequences
seqtab2.nochim <- removeBimeraDenovo(seqtab, method="consensus", multithread=TRUE, verbose=TRUE)
#Identified 1380 bimeras out of 46270 input sequences

dim(seqtab2.nochim)
#430 44890
#Chimeras make up about 0.96% of the merged sequence variants

sum(seqtab2.nochim)/sum(seqtab)
#0.9935339
#We account for the abundances of the chimeric variants
#Chimeric variants account for 0.65% of the merged sequence reads

#Track reads through the pipeline
#As a final check of progress, look at the number of reads that mde it through each step in the pipeoine
getN <- function(x) sum(getUniques(x))
track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab2.nochim))

colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
rownames(track) <- sample.names
head(track)

save.image("Desktop/stripsR/sequence-data/KMou_STRIPS_Run_2/dada2.RData")

write.csv(track, file = "Desktop/stripsR/STRIPS_Run_2_track.csv")

#Assign taxonomy to the sequenced variants
