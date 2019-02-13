library(rjson)

# setwd to raw data directory
fls = dir('.',pattern='txt')

stims = read.table('../Stimuli/stimnotes',sep='\t',header=T)
asc <- function(x) { strtoi(charToRaw(x),16L) }
chr <- function(n) { rawToChar(as.raw(n)) }

allDat = c()
trialDist = c()
rtClean = c()
for (fl in fls) {
  print(fl)
  trials = read.table(fl,sep="\n",quote = "")  
  colnames(trials) = list('trials')  
  # revs contains a list of json items, and rev 
  trials$trials = as.character(trials$trials)
  if (length(trials$trials) %% 73 == 0) {
    rtsClean = c()
    for (i in 8:73) {
      trial = fromJSON(trials$trials[i]) # let's get this json value
      trial_info = unlist(strsplit(trial$stimNotes,";"))
      # let's track trial distribution to make sure we only use subjects who saw one list
      trialDist = rbind(trialDist,data.frame(sid=trial_info[3],wd=trial_info[4],list=trial_info[2]))
      if (trial_info[1]=='correct') { 
        wd = trial_info[4]
        if (trial_info[2]=='A') { # column name will depend on list, in stimnotes
          ix = which(stims$list.A==wd)
          stemLength = -1 
          code = stims$code.A[ix]
        }
        if (trial_info[2]=='B') { # column name will depend on list, in stimnotes
          ix = which(stims$list.B==wd)
          stemLength = -1 
          code = stims$code.B[ix]
        }
        if (trial_info[2]=='C') { # column name will depend on list, in stimnotes
          ix = which(stims$list.C==wd)
          stemLength = -1 
          code = stims$code.C[ix]
        }
        # key intervals come from rtsClean
        IKIs = diff(as.numeric(strsplit(substr(trial$rtsClean,2,nchar(trial$rtsClean)-1),",")[[1]]))
        # to check rtsClean
        rtsClean = c(rtsClean,as.numeric(strsplit(substr(trial$rtsClean,2,nchar(trial$rtsClean)-1),",")[[1]]))
        # rtsAll contains the first keystroke, let's retrieve it:
        IKIs_all = as.numeric(strsplit(substr(trial$rtsAll,2,nchar(trial$rtsAll)-1),",")[[1]])
        theKeys = chr(as.numeric(strsplit(substr(trial$keysClean,2,nchar(trial$keysClean)-1),",")[[1]]))
        keyDat = 1:12*0
        keyDat[2:(length(IKIs)+1)] = IKIs
        keyDat[1] = IKIs_all[1] # rtsAll contains the first keystroke!
        allDat = rbind(allDat,data.frame(sid=trial_info[3],list=trial_info[2],wd,code,wdLen=nchar(wd),stemLength,theKeys,k=1:(length(keyDat)),keyDat))
      }
    }
  }
}

# center the keystroke N
allDat = allDat[allDat$keyDat>0,]

save(file='../all_keystrokes.Rd',allDat)
#trial_dist = table(trialDist$sid,trialDist$wd) # intended as a check that lists worked
#write.table(file='trial_distribution_lexical.txt',trial_dist)



