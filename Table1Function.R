
###############################################################################
### Table1 function 
###############################################################################

#This function computes summary of variables as need for Table 1 with test and P 
#for continuous and categorical values:updated Dec 24, 2016
#Takes a longer time to work for large data set 

#TO DO
#Make mean default with a warning if none specified
#Make aov.t default with a warning if none specified
#Check if all vars given are actually present. Throw error if not.
#Check why catvar names with "/" are accepted but not contvar
#Check why var names with "-" are not accepted

myTable1=function(dat,
                  contvar=NULL,          # Continuous Variables 
                  catvar=NULL,           # Categorical Variables
                  splitvar,              # The variable over which compare results. Must be Categorical e.g "Sex"
                  mydec=1,               # The number of decimal places for calculations
                  pdec=3,                # Decimal place for p-values
                  docaption=F,           # Should function do caption for you?
                  my.docaption=NULL,     # If false, then write caption eg. "Summaries by sex"
                  tsdec=2,               # Decimal place for T-test
                  rowPCT=F,              # Calculate catVar percentages in rows across splitvar instead of across catVar in the same splitvar
                  # E.g. catvar is Sex and splitvar is severity
                  # If rowPCT = F: in each severity calculate the percentage of each sex
                  # If rowPCT = T: in each sex calculate percentage of each severity
                  prmsd=NULL,            # Specify statistics for summaries e.g. "mean","median"
                  myinsert.bottom=NULL, 
                  Trace=F,               # For debugging
                  splitlabel=NULL,       # Label for the splitvar
                  mysim.p=F, 
                  myB=200, 
                  mylongtable=F, 
                  mylandscape=F,
                  myeps=0.001,
                  bracket=F,
                  mylevels=NULL,
                  contTest=NULL,        # Test to be applied respectively to the contvars e.g. "t.test","wilcox.t","aov.t"
                  catTest=NULL,         # Test to be applied respectively to the catvar e.g. "fisher.t","chisq.t"
                  Test=T,               # Test statistic column to be included in table
                  chi.correct=F,
                  exceloutput=F,        # Should an excel file be produced?
                  exceloutputName=NULL, # The name of the excel file to be produced e.g. "name"
                  latexoutput=F,        # Should a Latex file be produced?
                  my.loc="./tab1.tex",  # location for tex file e.g. "./name.tex"
                  mysize="scriptsize",
                  showtable=F,          # Whether to show Table on current screen. Only works on Macs
                  hasExcluded=F,        # If true then there must be a column called "excluded" that indicates which rows to remove with a 1
                  fisherWorkspace=200000,# The size of the workspace for fisher.t
                  disparateN=T,          # If n in the row is different from the n declared in the splitvar column then include the disparate n in the cell. e
                                        # eg n=10 in splitvar1 and 15 in splitvar 2 for a total of 25, but there is only data on 20 for contVar1, so we can't see where the data loss is.
                  ... ){   
 
  
  #Populating default tests for supplied variables so each variable has a corresponding test
  if(is.null(prmsd)){prmsd=c(rep("mean",length(contvar)))}
  if(is.null(contTest)&!is.null(contvar))contTest=rep("aov.t",length(contvar))
  if(is.null(catTest)&!is.null(catvar))catTest=rep("fisher.t",length(catvar))
  #Todo check for brackets and special characters and escape them
  
  if ((hasExcluded)){dat=dat[!(dat$excluded==1),]}
  
  defaultCaption="Summary of patients' variables across "  
  
  
  testsAvailable=data.frame(aov.t="ANOVA test",fisher.t="Fisher exact test",
                    chisq.t="Chi-squared test",t.test="T-test",
                    kruskal.t="Kruskal-Wallis test"
                    ,wilcox.t="Wilcoxon Rank Sum test")
  
  testsRequestedVector=unique(c(contTest, catTest))
  testsRequestedDataFrame=testsAvailable%>%dplyr::select(one_of(testsRequestedVector))
  testsRequestedMatrix=as.matrix(testsRequestedDataFrame)
  bottom=NULL
  n.testsRequestedMatrix=ncol(testsRequestedMatrix)
  bb.prmsd=NULL 
  
  prmsd.med="{\\scriptsize $a$\\ }{\\bf $b$\\ }{\\scriptsize $c$\\ } represent the lower quartile $a$, the median $b$, and 
the upper quartile $c$\\ for continuous variables."
  prmsd.mean="~~$x\\pm s$ represents $\\bar{X}\\pm 1$ SD."
  if(bracket==T)prmsd.mean="~~$x(s)$ represents $\\bar{X}(1SD)$."
  prmsd.cat=" Numbers after percents are counts." 
  test.used="\\indent Tests used:"
  if("median"%in%prmsd){
    bb.prmsd=paste(bb.prmsd,prmsd.med,sep="")
  }
  if("mean"%in%prmsd){
    bb.prmsd=paste(bb.prmsd,prmsd.mean,sep="")
   }
 
  if(!is.null(catvar)){
    bb.prmsd=paste(bb.prmsd,prmsd.cat,sep="")
  }

  for(i in 1:n.testsRequestedMatrix){
    if(i==n.testsRequestedMatrix){point="."}
    else{point=","}
    bottom=c(paste(bottom," \\textsuperscript{\\normalfont ", i,"} ",
                   testsRequestedMatrix[i],point , " ",sep="") )
  }

  if(Test==T){
    bottom=paste("\\noindent ",bb.prmsd," ", test.used ,bottom,sep="") }else{
    bottom=paste("\\noindent ",bb.prmsd,sep="")
  }
  
  ######################################################################################
  ######                     Pre-processing data                                   #####
  ######################################################################################
  
  #Make all catvars factors
  for(k in catvar){
    tmp.lbl=label(dat[[k]]) # Place holder for label
    dat[[k]]<-factor(dat[[k]]) # Change into a factor. Check how NA's under factors behave
    label(dat[[k]])<-tmp.lbl # Place label back
  }
  if(Trace==T)cat("Categorical Variables changed to factors","\n")
  
  #Make splitvar a factor
  if(is.null(mylevels)){
    dat[[splitvar]]=factor(dat[[splitvar]]) # changing split var to factors
  }else{
    dat[[splitvar]]=factor(dat[[splitvar]],levels=mylevels)                                                       
  }
  if(Trace==T)cat("Splitvar Variables changed to factors","\n")
  n.splitvarLevels=nlevels(dat[[splitvar]]) #Number of levels in the splitvar
  n.splitvar = table(dat[[splitvar]])       #Number of observations in each splitvar level
  n.splitvarCombined = c(n.splitvar, sum(n.splitvar)) #Number of observations in each level and combined
  
  ######################################################################################
  ######Calculating summaries (mean and median) stratified on splitvar and Combined#####
  ######################################################################################
  if(Trace==T)cat("Now Calculating summaries","\n")
  
  index<<-0  #A global variable that we will use this to keep track of which splitvar factor we are in in our custom functions.
  
  #We need to make our own functions that will print the summaries in one cell so that we can pass it to the tapply function coming up
  #Function for mean
  myMean=function(x){

    m=round(mean(x,na.rm=T),digits=mydec)
    s=round(sd(x,na.rm=T),digits=mydec)
    n=sum(!is.na(x))
    rr=paste(m," (",s,")",sep="")
    pm=paste(m," $\\pm$",s,sep="")
    if(bracket==F)rr=pm
    
    #check if disparateN is true and the n in the current cell is different from the n for the splitvar level.
    if(disparateN && n != n.splitvarCombined[index %% (n.splitvarLevels + 1) + 1])
    {
      rr=paste(rr, " n=", n, sep="")
    }
    index <<- index+1; #increment the index with each iteration
    return(rr)
  }
  
  #Function for median   
  myMedian=function(x){
    
    m=round(quantile(x,na.rm=T),digits=mydec)
    lq=m[[2]]
    mq=m[[3]]
    uq=m[[4]]
    n=sum(!is.na(x))
    rr=paste(mq," (",lq,"-",uq,")",sep="")
    
    #check if disparateN is true and the n in the current cell is different from the n for the splitvar level.
    if(disparateN && n != n.splitvarCombined[index %% (n.splitvarLevels + 1) + 1])
    {
      rr=paste(rr, " n=", n, sep="")
    }
    index <<- index+1;
    return(rr)
  }
  
  #Function to calculate the number of non-NA entries in a vector
  myN=function(x)(sum(!is.na(x)))
  
  cont.row.names=NULL
  stratifiedSummariesMatrix=NULL
  combinedSummariesVector=c()
  cumulativeContVarTables<-NULL
  #Test to exclude contvars if there are only categorical variables 
  if(!is.null(contvar))
  {
    #Calculate the summary of the contvars based on the factors/categories
    for(cv in contvar){
      tempRow = c()
      #Find the index of the current variable. contvar is a vector so ==cv will be applied to each element. 
      #Which() will provide the index
      pos.n=which(contvar==cv)
      if(prmsd[pos.n]=="mean")
      {
        tempRow = tapply(dat[[cv]], dat[[splitvar]],function(x){myMean(x)})
        combinedSummariesVector[pos.n]=unname(unlist( colwise(myMean)(dat[contvar[pos.n]])   ))
      } else if (prmsd[pos.n]=="median")
      {
        tempRow=tapply(dat[[cv]], dat[[splitvar]],function(x){myMedian(x)})
        combinedSummariesVector[pos.n]=unname(unlist( colwise(myMedian)(dat[contvar[pos.n]])   ))
      } else
      {
        stop("'",paste0(prmsd[pos.n], "'", " is not a valid entry. Only 'mean' and 'median' are accepted"))
      }
      
      stratifiedSummariesMatrix = rbind(stratifiedSummariesMatrix,tempRow)
      tmp.lbl<-cv
      
      #Ensure the variable has an appropriate label.
      if( (label(dat[[cv]])!="")) {
        if(Trace==T) cat(label(dat[[cv]]),"\n")
        tmp.lbl=label(dat[[cv]])
        if(Trace==T)cat(tmp.lbl," should be same as above","\n")
      }
      tmp.lbl=paste("\\bf{",tmp.lbl,"}",sep="")
      #Add the label to the end of the continuous variable row names
      cont.row.names=c(cont.row.names,tmp.lbl)
    }
    
    if(Trace==T)cat("Calculated Cont Var summaries","\n")
    
    #The number of non-NA entries of each contvar
    N=unname(unlist(colwise(myN)(dat[,contvar])))
    #Make a matrix of the following vectors
    cumulativeContVarTables= cbind(N,stratifiedSummariesMatrix,combinedSummariesVector)
    #Remove row names
    rownames(cumulativeContVarTables)<-NULL
    #Add row names as the first column
    cumulativeContVarTables=cbind(cont.row.names,cumulativeContVarTables)   
    
    if(Trace==T)cat("Table for contvar summaries done","\n")
  }#end of test for if(is.null(contvar))
  
  ######################################################################################
  ####Producing Table for Categorical Variables stratified on split var and Combined####
  ######################################################################################
  
  cumulativeCatVarTables=NULL
  
  #Check if any categorical variables are provided
  if(!is.null(catvar))
  {
    #For each catvar make a table of catvar against splitvar percentages and absolute n
    #with a combined column at the end
    for(k in catvar){
      
      n.col=n.splitvarLevels         #Number of levels in the splitvar
      n.row=nlevels(dat[[k]])        #Number of levels in the catvar
      r.names=levels(dat[[k]])       #The names of the levels of catvar
      
      #----Make a table of catvar against splitvar with combined absolute values (n)
      
      #All cat vars in dat are levels now and same size.
      #Make a table with catvar as rows and splitvar as columns
      catVarTableNs=table(dat[[k]], dat[[splitvar]])
      colNTotals=apply(catVarTableNs, 2, sum)  #Total n in a col (splitvar level)
      rowNTotals=apply(catVarTableNs, 1, sum)   #Total n in a row (catvar level)
      allN=sum(rowNTotals)                  #Total n of the cat var
      
      #Add row totals as a column on the far right
      catVarTableNsWCombined=cbind(catVarTableNs,rowNTotals)
      
      #----Make a table of catvar against splitvar with combined percentages

      #Default is rowPCT=F, so calculate percentages per column.
      combinedPercentages=round((rowNTotals/allN)*100,0) 
      #When calculating percentage per row the combined will always be 100
      if(rowPCT==T)combinedPercentages=c(rep(100,n.row))    
      
      colPercentages=round(sweep(catVarTableNs,2,colNTotals,"/")*100,0) #column percentages with sweep matric and vector division BCareful
      rowPercentages=round(sweep(catVarTableNs,1,rowNTotals,"/")*100,0) #row pct with sweep

      if(rowPCT==T) colPercentages=rowPercentages
      catVarTablePercentages=cbind(colPercentages,combinedPercentages)
      #how do we get row percentages
      
      #----Combine the Ns and percentages into one table
      
      #Create an empty matrix with the correct size.
      #n.col+1 because we need an extra column to put the combined
      #n.row+1 because we need an extra row to put the catvar name
      catVarTableNsPercentages=matrix(rep("",(n.row+1)*(n.col+1)),nrow=n.row+1)
      
      for(i in 2:(n.row+1)){
        for(j in 1:(n.col+1)){ 
          catVarTableNsPercentages[i,j]<-paste(catVarTablePercentages[i-1,j],"\\%~","(",catVarTableNsWCombined[i-1,j],")",sep="")   
        }
      }
      
      #----Make a table of the labels and N of the catvar
      rowNamesFormattted=paste("~~~~",r.names,sep="")
      catVarLabel<-k
      # :: specifies to use the function in package Hmisc
      if(Hmisc::label(dat[[k]])!=""){ #This how labels names are swap with actual rownames
        catVarLabel=label(dat[[k]])
      }
      catVarLabel=paste("\\bf{",catVarLabel,"}",sep="")
      labelAndRowNames=c(catVarLabel,rowNamesFormattted)# You may bold this cat names in future
      
      N2=rep("",n.row)         #A vector of empty spaces for each category
      N3=sum(!is.na(dat[[k]])) #The number of recorded values in catvar
      N4=c(N3,N2)              #A vector that will be the column showing the N in the category
      catVarTableLabelsNsPercentages=cbind(labelAndRowNames,N4,catVarTableNsPercentages)
      cumulativeCatVarTables=rbind(cumulativeCatVarTables,catVarTableLabelsNsPercentages)
    }
    colnames(cumulativeCatVarTables)<-NULL
  } #end of test for excluding catvar summaries 
  
  contCatVarTable=rbind(cumulativeContVarTables,cumulativeCatVarTables)
  
  ######################################################################################
  ####      Performing contvar statistic tests and producing their tables        ####
  ######################################################################################
  
  TShold.anova   <-NULL     # aov.t
  TShold.kruskal <-NULL     # kruskal.t
  TShold.ranksum <-NULL     # wilcox.t
  TShold.ttest   <-NULL     # t.test
  TShold.chisq   <-NULL     # chisq.t
  TShold.fisher  <-NULL     # fisher.t
  
  TScont         <-NULL     # Table to hold all contvar tests
  TSNcont        <-NULL     # Table to hold all contvar test names
  if(!is.null(contvar)){    
    if(n.splitvarLevels < 3) {
      
      #Do the t.test
      if( any(contTest%in%"t.test")){#Test to stop running t test if not needed                                       
        for(t.nn in contvar){
          #TODO if any group has less than 2 then do not do the test
          vartest=with(dat,var.test(as.formula(paste(t.nn,splitvar,sep="~")))) # Variance test to check which variance to use pooled or sattarwaite 
          var.p=vartest[["p.value"]]
          myvar.equal=T
          if(var.p < .05)myvar.equal<-F
          
          tmd=with(dat,t.test(as.formula(paste(t.nn,splitvar,sep="~")),var.equal=myvar.equal))
          
          ts.t=round(tmd[["statistic"]],digits=tsdec)[[1]]
          p.t= format.pval(round(tmd[['p.value']],digits=pdec) ,eps=myeps)  
          df.t=round(tmd[["parameter"]])[[1]]
          if(round(tmd[["p.value"]],digits=pdec) < myeps){P="~P"}else{P="~P="}
          t.ts=paste("$t(",df.t,")=",ts.t,",",P,p.t, "$",sep="")
          TShold.ttest=c(TShold.ttest,t.ts)
        }
      } #End of t.test
      
      #Do Wilcox.test    
      if( any(contTest%in%"wilcox.t")){#Test to stop running wilcox test if not needed                                       
        for(w.nn in contvar){
          wmd=with(dat,wilcox.test(as.formula(paste(w.nn,splitvar,sep="~"))))
          
          ts.w=round(wmd[["statistic"]], digits=tsdec)[[1]]
          p.w= format.pval(round(wmd[['p.value']],digits=pdec),eps=myeps)  
          # df.w=round(wmd[["parameter"]])[[1]]
          if(round(wmd[["p.value"]],digits=pdec) < myeps){P="~P"}else{P="~P="}
          w.ts=paste("W=",ts.w,", ",P, p.w, sep="")
          TShold.ranksum=c(TShold.ranksum, w.ts)
        }
      } #End of Wilcox.test 
      
    } else #End of two level split
    if (any(contTest%in%"wilcox.t") || any(contTest%in%"t.test"))
    {
      stop("T-test and Wilcox can only be used when splitvar is 2 levels only")
    }
    
    #Do Kruskal Wallis
    if( any(contTest%in%"kruskal.t")){    #Test to stop running kruskal test if not needed                                                                         
      for(k.nn in contvar){
        kmd=with(dat,kruskal.test(as.formula(paste(k.nn,splitvar,sep="~"))))
        ts.k=round(kmd[["statistic"]],digits=tsdec)
        p.k= format.pval(round(kmd[['p.value']], digits=pdec),eps=myeps)  
        df.k=round(kmd[["parameter"]])[[1]]
        
        if(round(kmd[["p.value"]],digits=pdec) < myeps){P=",~P"}else{P=",~P="}
        k.ts=paste("chi^2","=", ts.k, ", df=", df.k, P, p.k, sep="")
        TShold.kruskal=c(TShold.kruskal,k.ts)
      }
    } #End of Kruskal Wallis 
    
    #Do ANOVA
    if( any(contTest%in%"aov.t")){    #Test to stop running anova test if not needed                                                                           
      for(nn in contvar){
        #md=summary(with(dat,aov(age.at.start~Fitness.Group)))
        md=summary(with(dat,aov(as.formula(paste(nn,splitvar,sep="~")))))    
        md=unlist(md)
        df1=round(md["Df1"],0)
        df2=round(md["Df2"],0)
        P.a=format.pval(round(md["Pr(>F)1"],digits=pdec),eps=myeps)
        if(round(md["Pr(>F)1"],pdec)< myeps){P=",~P"}else{P=",~P="}
        ts=round(md["F value1"],digits=tsdec)[[1]]
        TS=paste("$F({",df1,", ",df2,"})=",ts,P,P.a,"$", sep="" )
        TShold.anova=c(TShold.anova,TS)
      }
    } #End of ANOVA 
    
    
    TScont<-NULL
    temp.TSNcont<-NULL
    #contTest=c("aov.t","aov.t","aov.t","kruskal.t")
    for(ii in 1:length(contvar)){
      {
        if(contTest[ii]=="t.test"){
          tscont=TShold.ttest[ii]
          temp.TSNcont="t-test"
        }else if(contTest[ii]=="aov.t"){
          tscont=TShold.anova[ii]
          temp.TSNcont="ANOVA"
        }else if(contTest[ii]=="wilcox.t"){
          tscont=TShold.ranksum[ii]
          temp.TSNcont="Wilcoxon Rank Sum"
        }else if(contTest[ii]=="kruskal.t"){
          tscont=TShold.kruskal[ii]
          temp.TSNcont="Kruskal-Wallis"
        }
      }
      if(Trace==T)cat(paste("Done for ",contvar[ii], "\n",sep=" "))
      TScont=c(TScont, tscont)
      TSNcont=c(TSNcont, temp.TSNcont)
    }
    
  } #end of test for TScont equal NULL if there are no cont var 
  
  ######################## Need to dom for categorical variables########################
  #catvar=c("sex","group.percent.days")
  #fisher test
  TScat<-NULL
  TSNcat<-NULL
  if(!is.null(catvar)){
    
    if( any(catTest%in%"fisher.t")){    #Test to stop running fihser test if not needed
      for(f.nn in catvar){
        fmd=fisher.test(table(dat[[f.nn]],dat[[splitvar]]),simulate.p.value=mysim.p,B=myB, workspace = fisherWorkspace)
        options(digits=7)
        p.f= format.pval(round(fmd[['p.value']],digits=pdec),eps=myeps)  
        
        if(round(fmd[["p.value"]],digits=pdec) < myeps){P="~~~~~~P"}else{P="~~~~~~P="}
        f.ts=paste("$",P,p.f,"$",sep="")
        TShold.fisher=c(TShold.fisher,f.ts)
        
      }
    }
    
    
    ###########chi-square
    if( any(catTest%in%"chisq.t")){    #Test to stop running chisquare test if not needed                                       
      for(chi.nn in catvar){
        options(digits=7)
        chimd=chisq.test(table(dat[[chi.nn]], dat[[splitvar]]  ),correct=chi.correct)
        
        tryCatch( chisq.test(table(dat[[chi.nn]], dat[[splitvar]]  ), correct=chi.correct),error=function(e)e,warning=function(w)w)->error_warn
        if(is(error_warn,"warning")){
          if(Trace==T)cat("Warning: Use fihser exact test for \"", chi.nn, "\" variable","\n")
        }
        ts.chi=round(chimd[["statistic"]],digits=tsdec)[[1]]
        p.chi=format.pval(round(chimd[['p.value']],digits=pdec),eps=myeps)  
        df.chi=round(chimd[["parameter"]])[[1]]
        
        if(round(chimd[["p.value"]],digits=pdec) < myeps){P=",~P"}else{P=",~P="}
        chi.ts=paste("$\\chi^2_{",df.chi,"}=",ts.chi,P,p.chi,"$",sep="")
        TShold.chisq=c(TShold.chisq,chi.ts)
      }
    }
    
    ### looping throu to get TS for catvars
    
    TScat<-NULL
    TSNcat<-NULL
    #catTest=c("chisq.t","fisher.t")
    for(iii in 1:length(catvar)){
      { 
        temp.TSNcat<-NULL
        if(catTest[iii]=="chisq.t"){
          tscat=TShold.chisq[iii]
          temp.TSNcat="Chi-square"
        }else if(catTest[iii]=="fisher.t"){
          tscat=TShold.fisher[iii]                        
          temp.TSNcat="Fisher"
        }
        if(Trace==T)cat(paste("Done for ",catvar[iii], "\n",sep=" "))
        TScat=c(TScat,tscat)
        TSNcat=c(TSNcat, temp.TSNcat)
        catn<-nlevels(dat[[catvar[iii]]])
        TScat=c(TScat,rep("",catn))
        TSNcat=c(TSNcat, rep("",catn))
      }      
    }
  }
  #end of setting TScat to NULL if there are no catvars
  
  #combine TStats for cont and cat vars
  Test.Statistics=c(TScont,TScat)
  Test.Statistics.Names=c(TSNcont,TSNcat)
  alltabbb=data.frame(contCatVarTable,Test.Statistics,Test.Statistics.Names)
  
  ch=colheads=table(dat[[splitvar]])
  hnam=names(colheads)
  colheads=c("Variables","n", hnam,"Combined","P value", "Test")
  extracolheads=c("","",paste("n=",as.vector(ch),sep=""),paste("n=",sum(as.vector(ch)),sep=""))
  insert.bottoml="\\scriptsize{   Data is presented as : 
Mean$\\pm$SD for continuous variables, row percentages (frequency) for categorical variables~~\\indent test Test used: \\textsuperscript{\\normalfont 1} T-test ~~~~~, \\textsuperscript{\\normalfont 2} Pearson chi-square test } "
  
  myres=list(contCatVarTable,alltabbb,colheads,extracolheads,insert.bottoml)
  names(myres)<-list("mytab","mytab1","colheads","extracolheads","insert.bottom")
  
  
  #writing to an excel file
  #cleann=function(x)str_remove_all( x,"[\\bf\\{~\\\\}$]")
  #cleann=function(x)str_replace_all( x,"[\\bf\\{~\\\\}$]"," ")
  # Function to remove Latex text to prepare for excel file. Also removes dots and replaces with spaces.
  cleann=function(x){
    y=gsub("([^0-9])\\.", "\\1 ", gsub("\\}","",gsub("\\{\\\\bf","",gsub("\\\\bf\\{","",x))))
    str_replace_all( y,"[\\{~\\\\}$]"," ")
  }
  
  library(stringr)
  
  xcelldat=apply(myres[["mytab1"]],2,FUN=cleann)
  nn.col=ncol(xcelldat)
  # browser()
  colnames(xcelldat)[1]<-"Variables"
  
  extraH=t(as.matrix( c(extracolheads,"","") ))
  
  hhhh=t(as.matrix(colnames(xcelldat)))
 
  xlabel=splitvar
  if(!is.null(splitlabel))xlabel=splitlabel
  ssplit=t(as.matrix(c(" ","",xlabel,rep("",(nn.col-3)))))
  xcelldat=rbind(ssplit,colheads,extraH,xcelldat)
  xcelldat=apply(xcelldat,2,FUN=function(x)gsub(mysize," ",x))
  
  if(Test==F){xcelldat=xcelldat[,-nn.col]}
  if(exceloutput==T){ 
    if(is.null(exceloutputName))exceloutputName=splitvar
    colnames(xcelldat)<-NULL
    write.csv(xcelldat,file=paste("./",exceloutputName,".csv",sep=""),row.names=F)
    }
  
  
  #Making LaTeX Tables  
  mytab=if(Test==T)myres[["mytab1"]] else myres[["mytab"]] # i change here today
  Variable=as.vector(mytab[,1])
  tab=mytab[,-1]
  l.splitvar=splitvar
  if(  !(is.null(splitlabel)))l.splitvar<-splitlabel
  n.sp=nlevels(dat[[splitvar]])
  if( !is.null(splitlabel)) splitvar<-splitlabel
  if(latexoutput==T){
  tab1=Hmisc::latex(tab,rowname=Variable,rowlabel="Variables",
             caption=(if(docaption==T)paste(defaultCaption,splitvar,sep="")else my.docaption),
             file=my.loc,where="!htbp",landscape=mylandscape,longtable=mylongtable,
             
             cgroup=(if(Test==T)c("",l.splitvar,"","","") else c("",l.splitvar,"","")),
             n.cgroup=(if(Test==T)c(1,n.sp,1,1,1) else c(1,n.sp,1,1)),
             colheads=(if(Test==T)c(myres[["colheads"]][-1]) else c(myres[["colheads"]][c(-1,-nn.col)]) ),
             
             size=mysize,
             insert.bottom=bottom,... )
    }else{
    tab1=NULL
    showtable=F
    }
  
  if(showtable==T)return(tab1) else return(myres)    
  #   return(myres)
  
  
}


