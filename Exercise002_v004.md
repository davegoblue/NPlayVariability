---
title: "N-Play Variability"
author: "davegoblue"
date: "March 19, 2016"
output: html_document
---



## Overview and Synopsis  
This routine analyzes variability of N-play video poker results based on simulation data made available by the Wizard of Odds <http://wizardofodds.com/games/video-poker/appendix/2/>.  The game simulated was 9/6 Jacks or Better, and this routine looks at the impact of playing 5,000 hands with 1/3/5/10/50/100 lines per hand.  The code broadly copies routines from Exercise002_v003.R, but with the intent of storing figures and summaries.
  
## Data Processing  
###_Coding Routine_  
The Wizard of Odds file was pre-processed in Excel and saved as a CSV once per each of the desired N-play outcomes.  The file is read in, parsed to eliminate any unwanted values, and then assessed based on random draws from uniform(0,1).  Results are stored for the final outcome and the minimum cumulative outcomes.  

Below is the routine for creating the data.  It has been converted to a function so it can be re-run multiple times.  

First, a function is created to read and process files for mapping probabilities to outcomes.  

```r
getBaseOutcomes <- function(myFileName="BaseOutcomes.csv", myDelete=NULL, forceEQ=FALSE) {
    
    if (file.exists(myFileName)) {
        baseOutcomes <- read.csv(myFileName,stringsAsFactors = FALSE)
        if (ncol(baseOutcomes) != 2) { stop("Error in CSV file, should have exactly 2 columns") }
        colnames(baseOutcomes) <- c("probs","outcomes")
    } else {
        baseOutcomes <- data.frame(probs=c(0.01,0.02,0.05,0.18,0.24,0.50),outcomes=c(10,5,2,1,0,-1))
    }
    
    baseOutcomes <- baseOutcomes[baseOutcomes$probs != 0,] ## Can have zeroes as inputs -- ignore those
    
    if ( forceEQ ) {
        pDelta <- sum(baseOutcomes$probs) - 1
        if ( abs(pDelta) < 0.0000001 & 
             abs(pDelta) / baseOutcomes[nrow(baseOutcomes),]$probs < 0.1
            ) 
        {
            print(paste0("Modifying probablities ",paste0(baseOutcomes[nrow(baseOutcomes),],collapse=" ")))
            baseOutcomes[nrow(baseOutcomes),]$probs <- baseOutcomes[nrow(baseOutcomes),]$probs - pDelta
            print(paste0("New probablities ",paste0(baseOutcomes[nrow(baseOutcomes),],collapse=" ")))
        }
    }
    
    if (sum(baseOutcomes$probs)!=1 | min(baseOutcomes$probs) < 0 | 
        sum(is.na(baseOutcomes$probs)) > 0 | sum(is.na(baseOutcomes$outcomes)) > 0) { 
        stop("Please resolve the issue with inputs for probs and outcomes, aborting") 
    }
    
    ## Store the original value read in as outcomes
    baseOutcomes$oldOutcomes <- baseOutcomes$outcomes
    
    ## Null the baseOutcomes$outcomes where outcomes >= X
    if (!is.null(myDelete)) {
        myCond <- parse(text=paste0("baseOutcomes$outcomes",myDelete))
        baseOutcomes$outcomes[eval(myCond)] <- 0
        print(paste0("Converted all cases where ",myCond," to baseOutcomes$outcomes = 0"))
    }
    
    baseMean <- sum(baseOutcomes$probs*baseOutcomes$outcomes)
    baseVar <- sum(baseOutcomes$probs*(baseOutcomes$outcomes-baseMean)^2)
    
    print(paste0("Probabilities sum to 1.  Outcomes has mean ",format(baseMean,digits=3),
                 " and variance ",format(baseVar,digits=3)))

    return(baseOutcomes)
}
```
  
Second, a function is created to draw the random variables and calculate the outcomes database.  

```r
calcOutcomes <- function(baseOutcomes=baseOutcomes,nPlay=1) {
    
    ## Allow nTrials, nPerTrial, and myHurdle to come from global environment
    print(paste0("Running ",nPlay,"-play with nTrials=",nTrials,
                 " nPerTrial=",nPerTrial," and hurdle ",myHurdle
                 )
          )
    
    myCDF <- numeric(nrow(baseOutcomes)+1)
    myCDF[1] <- 0

    for ( intCtr in 1:nrow(baseOutcomes) ) {
        myCDF[intCtr+1] <- myCDF[intCtr] + baseOutcomes$probs[intCtr]
    }

    mtxCumOutcomes <- matrix(baseOutcomes$outcomes[findInterval(matrix(data=runif(nTrials*nPerTrial,0,1),
                                                                       nrow=nPerTrial,
                                                                       ncol=nTrials
                                                                       ),
                                                                myCDF,rightmost.closed=TRUE
                                                                )
                                               ],
                         nrow=nPerTrial,
                         ncol=nTrials
                         )

    print(paste0("Ouctomes across ",nTrials*nPerTrial," draws of ",nPlay,"-play have mean: ",
                 format(mean(mtxCumOutcomes),digits=3)," and variance: ",
                 format(sd(mtxCumOutcomes)^2,digits=3)
                 )
         )

    mtxCumOutcomes <- apply(mtxCumOutcomes,2,FUN=cumsum)  ## About 2.5 seconds for 12,000 x 5,000

    ## Pop this back outside the function for further analysis
    mtxSaver <<- mtxCumOutcomes
    
    maxPerTrial <- apply(mtxCumOutcomes,2,FUN=max)  ## About 1.0 seconds for 12,000 x 5,000
    minPerTrial <- apply(mtxCumOutcomes,2,FUN=min)  ## About 1.0 seconds for 12,000 x 5,000
    lastPerTrial <- as.numeric(mtxCumOutcomes[nrow(mtxCumOutcomes),])
    dfSummary <- data.frame(myTrial = 1:nTrials, myMax = maxPerTrial, myMin = minPerTrial, 
                            myLast = lastPerTrial, myCond = FALSE, myN_Cond = NA, myVal_Cond = NA
                            )
    dfSummary$myCond <- eval(parse(text=paste0("dfSummary$myMin",myHurdle)))
    
    foo <- function(x) { 
        which(eval(parse(text=paste0("x",myHurdle))))[1]
    }
    
    dfSummary$myN_Cond <- apply(mtxCumOutcomes,2,FUN=foo)  ## About 2.5 seconds for 12,000 x 5,000
    
    for ( intCtr in 1:nTrials ) {
        dfSummary$myVal_Cond[intCtr] <- mtxCumOutcomes[dfSummary$myN_Cond[intCtr],dfSummary$myTrial[intCtr]]
    }
    
    return(dfSummary)
}
```

Additionally, a function is created to graph the data and store the outputs.  

```r
graphSummary <- function(graphData, nPlay=1) {
    
    graphData <- graphData[order(-graphData$myCond, graphData$myN_Cond, -graphData$myLast),]
    print(summary(graphData))

    ## Have the x and y units auto-calculated
    minX <- min(graphData$myMin)                  ## Find most negative element
    maxX <- max(0, graphData$myLast)              ## Find most positive element (use 0 if all are negative)
    powX <- log10(max(1, abs(minX), abs(maxX)))   ## Find rough "power" of data

    unitX <- 10^(round(powX-0.5,0)-1)             ## If thousands, use hundreds; if hundreds, use tens; etc.
    minX <- unitX*(floor(minX/unitX)-1)           ## Round to similar units as unitX
    maxX <- unitX*(ceiling(maxX/unitX)+1)         ## Round to similar units as unitX

    hist(graphData$myMin,
         col=rgb(1,0,0,.25),
         main=paste0("Results: ",nTrials," Trials (",nPerTrial," ",
                     nPlay,"-play draws per trial)"
                     ), 
         xlab="Units", ylab="N Trials",
         breaks=seq(minX,maxX,by=unitX),
         xlim=c(minX, maxX)
         )

    hist(graphData$myLast,col=rgb(0,0,1,.25),
         breaks=seq(minX,maxX,by=unitX),
         xlim=c(minX,maxX),
         add=TRUE
         )

    legend("topright",col=c(rgb(1,0,0,.25),rgb(0,0,1,.25),rgb(0.5,0,0.5,.5)),
           legend=c("Minimum","Final","Overlap"),pch=20,pt.cex=2
           )

}
```

###_Prepare the global parameters_  
Finally, the key global parameters are set.  


```r
nTrials <- 2000
nPerTrial <- 80000
myHurdle <- "<=-500"
storePer <- 10
```
  
  
## Results  
The simulation is repeated for each of the desired N-play outcomes.  In this case, we have run the routine for 1/3/5/10/50/100 play.  Further, we save the cumulative results at the end of every 10th hand.  
  

###_Results for 1-play_  

```r
## Run for 1-play
baseOutcomes <- getBaseOutcomes(myFileName="Play001Outcomes.csv",forceEQ=TRUE)
```

```
## [1] "Modifying probablities 0.54543467 -1"
## [1] "New probablities 0.54543466 -1"
## [1] "Probabilities sum to 1.  Outcomes has mean -0.00456 and variance 19.5"
```

```r
dfSummary <- calcOutcomes(baseOutcomes=baseOutcomes, nPlay=1)
```

```
## [1] "Running 1-play with nTrials=2000 nPerTrial=80000 and hurdle <=-500"
## [1] "Ouctomes across 1.6e+08 draws of 1-play have mean: -0.00407 and variance: 19.9"
```

```r
graphSummary(dfSummary, nPlay=1)
```

```
##     myTrial           myMax            myMin             myLast       
##  Min.   :   1.0   Min.   :  -1.0   Min.   :-3511.0   Min.   :-3312.0  
##  1st Qu.: 500.8   1st Qu.:  83.0   1st Qu.:-1615.0   1st Qu.:-1214.8  
##  Median :1000.5   Median : 509.5   Median :-1042.0   Median : -454.0  
##  Mean   :1000.5   Mean   : 747.2   Mean   :-1106.3   Mean   : -325.7  
##  3rd Qu.:1500.2   3rd Qu.:1173.0   3rd Qu.: -555.8   3rd Qu.:  482.0  
##  Max.   :2000.0   Max.   :5433.0   Max.   :   -1.0   Max.   : 5203.0  
##                                                                       
##    myCond           myN_Cond       myVal_Cond  
##  Mode :logical   Min.   : 4281   Min.   :-500  
##  FALSE:445       1st Qu.:12416   1st Qu.:-500  
##  TRUE :1555      Median :18904   Median :-500  
##  NA's :0         Mean   :24255   Mean   :-500  
##                  3rd Qu.:30571   3rd Qu.:-500  
##                  Max.   :79359   Max.   :-500  
##                  NA's   :445     NA's   :445
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
mtxCum001 <- mtxSaver[storePer * (1:(nPerTrial/storePer)),]
```
  
    
###_Results for 3-play_  

```r
## Run for 3-play
baseOutcomes <- getBaseOutcomes(myFileName="Play003Outcomes.csv",forceEQ=TRUE)
```

```
## [1] "Modifying probablities 0.262602735 -3"
## [1] "New probablities 0.26260273 -3"
## [1] "Probabilities sum to 1.  Outcomes has mean -0.0152 and variance 67.7"
```

```r
dfSummary <- calcOutcomes(baseOutcomes=baseOutcomes, nPlay=3)
```

```
## [1] "Running 3-play with nTrials=2000 nPerTrial=80000 and hurdle <=-500"
## [1] "Ouctomes across 1.6e+08 draws of 3-play have mean: -0.0155 and variance: 67.3"
```

```r
graphSummary(dfSummary, nPlay=3)
```

```
##     myTrial           myMax             myMin           myLast       
##  Min.   :   1.0   Min.   :   -3.0   Min.   :-8141   Min.   :-8127.0  
##  1st Qu.: 500.8   1st Qu.:  181.0   1st Qu.:-3521   1st Qu.:-2798.2  
##  Median :1000.5   Median :  834.5   Median :-2328   Median :-1375.5  
##  Mean   :1000.5   Mean   : 1209.0   Mean   :-2497   Mean   :-1240.1  
##  3rd Qu.:1500.2   3rd Qu.: 1792.8   3rd Qu.:-1341   3rd Qu.:  242.5  
##  Max.   :2000.0   Max.   :10536.0   Max.   :    1   Max.   : 9607.0  
##                                                                      
##    myCond           myN_Cond       myVal_Cond    
##  Mode :logical   Min.   :  870   Min.   :-502.0  
##  FALSE:158       1st Qu.: 3840   1st Qu.:-501.0  
##  TRUE :1842      Median : 6709   Median :-501.0  
##  NA's :0         Mean   :12706   Mean   :-500.7  
##                  3rd Qu.:14487   3rd Qu.:-500.0  
##                  Max.   :78383   Max.   :-500.0  
##                  NA's   :158     NA's   :158
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
mtxCum003 <- mtxSaver[storePer * (1:(nPerTrial/storePer)),]
```
  
  
###_Results for 5-play_  

```r
## Run for 5-play
baseOutcomes <- getBaseOutcomes(myFileName="Play005Outcomes.csv",forceEQ=TRUE)
```

```
## [1] "Modifying probablities 0.130120405 -5"
## [1] "New probablities 0.130120414 -5"
## [1] "Probabilities sum to 1.  Outcomes has mean -0.0253 and variance 130"
```

```r
dfSummary <- calcOutcomes(baseOutcomes=baseOutcomes, nPlay=5)
```

```
## [1] "Running 5-play with nTrials=2000 nPerTrial=80000 and hurdle <=-500"
## [1] "Ouctomes across 1.6e+08 draws of 5-play have mean: -0.0257 and variance: 132"
```

```r
graphSummary(dfSummary, nPlay=5)
```

```
##     myTrial           myMax            myMin            myLast        
##  Min.   :   1.0   Min.   :  -5.0   Min.   :-12815   Min.   :-12432.0  
##  1st Qu.: 500.8   1st Qu.: 307.8   1st Qu.: -5220   1st Qu.: -4255.2  
##  Median :1000.5   Median :1115.0   Median : -3448   Median : -2159.0  
##  Mean   :1000.5   Mean   :1667.3   Mean   : -3688   Mean   : -2055.7  
##  3rd Qu.:1500.2   3rd Qu.:2475.5   3rd Qu.: -1912   3rd Qu.:   154.2  
##  Max.   :2000.0   Max.   :9661.0   Max.   :    -7   Max.   :  9059.0  
##                                                                       
##    myCond           myN_Cond       myVal_Cond    
##  Mode :logical   Min.   :  481   Min.   :-504.0  
##  FALSE:100       1st Qu.: 2098   1st Qu.:-502.0  
##  TRUE :1900      Median : 3862   Median :-501.0  
##  NA's :0         Mean   : 8992   Mean   :-501.2  
##                  3rd Qu.: 9727   3rd Qu.:-500.0  
##                  Max.   :79259   Max.   :-500.0  
##                  NA's   :100     NA's   :100
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
mtxCum005 <- mtxSaver[storePer * (1:(nPerTrial/storePer)),]
```
  
  
###_Results for 10-play_  

```r
## Run for 10-play
baseOutcomes <- getBaseOutcomes(myFileName="Play010Outcomes.csv",forceEQ=TRUE)
```

```
## [1] "Modifying probablities 0.025913774 -10"
## [1] "New probablities 0.025913775 -10"
## [1] "Probabilities sum to 1.  Outcomes has mean -0.0506 and variance 345"
```

```r
dfSummary <- calcOutcomes(baseOutcomes=baseOutcomes, nPlay=10)
```

```
## [1] "Running 10-play with nTrials=2000 nPerTrial=80000 and hurdle <=-500"
## [1] "Ouctomes across 1.6e+08 draws of 10-play have mean: -0.0517 and variance: 342"
```

```r
graphSummary(dfSummary, nPlay=10)
```

```
##     myTrial           myMax           myMin            myLast      
##  Min.   :   1.0   Min.   :   -8   Min.   :-24401   Min.   :-24061  
##  1st Qu.: 500.8   1st Qu.:  500   1st Qu.: -8922   1st Qu.: -7799  
##  Median :1000.5   Median : 1464   Median : -6307   Median : -4492  
##  Mean   :1000.5   Mean   : 2345   Mean   : -6515   Mean   : -4133  
##  3rd Qu.:1500.2   3rd Qu.: 3276   3rd Qu.: -3815   3rd Qu.:  -956  
##  Max.   :2000.0   Max.   :18909   Max.   :    10   Max.   : 18403  
##                                                                    
##    myCond           myN_Cond       myVal_Cond    
##  Mode :logical   Min.   :  232   Min.   :-509.0  
##  FALSE:41        1st Qu.:  886   1st Qu.:-504.0  
##  TRUE :1959      Median : 1807   Median :-502.0  
##  NA's :0         Mean   : 5220   Mean   :-502.4  
##                  3rd Qu.: 4604   3rd Qu.:-501.0  
##                  Max.   :78882   Max.   :-500.0  
##                  NA's   :41      NA's   :41
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
mtxCum010 <- mtxSaver[storePer * (1:(nPerTrial/storePer)),]
```
  
  
###_Results for 50-play_  

```r
## Run for 50-play
baseOutcomes <- getBaseOutcomes(myFileName="Play050Outcomes.csv",forceEQ=TRUE)
```

```
## [1] "Modifying probablities 2.1776e-05 -50"
## [1] "New probablities 2.1764999999978e-05 -50"
## [1] "Probabilities sum to 1.  Outcomes has mean -0.253 and variance 5139"
```

```r
dfSummary <- calcOutcomes(baseOutcomes=baseOutcomes, nPlay=50)
```

```
## [1] "Running 50-play with nTrials=2000 nPerTrial=80000 and hurdle <=-500"
## [1] "Ouctomes across 1.6e+08 draws of 50-play have mean: -0.248 and variance: 5178"
```

```r
graphSummary(dfSummary, nPlay=50)
```

```
##     myTrial           myMax           myMin            myLast      
##  Min.   :   1.0   Min.   :  -38   Min.   :-76388   Min.   :-74937  
##  1st Qu.: 500.8   1st Qu.: 1564   1st Qu.:-37561   1st Qu.:-33812  
##  Median :1000.5   Median : 4142   Median :-26929   Median :-22147  
##  Mean   :1000.5   Mean   : 8007   Mean   :-27779   Mean   :-19857  
##  3rd Qu.:1500.2   3rd Qu.: 9108   3rd Qu.:-17002   3rd Qu.: -8428  
##  Max.   :2000.0   Max.   :95856   Max.   :   -63   Max.   : 87960  
##                                                                    
##    myCond           myN_Cond         myVal_Cond    
##  Mode :logical   Min.   :   21.0   Min.   :-538.0  
##  FALSE:8         1st Qu.:   84.0   1st Qu.:-518.0  
##  TRUE :1992      Median :  189.0   Median :-510.0  
##  NA's :0         Mean   : 1491.7   Mean   :-511.5  
##                  3rd Qu.:  620.2   3rd Qu.:-505.0  
##                  Max.   :75007.0   Max.   :-500.0  
##                  NA's   :8         NA's   :8
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
mtxCum050 <- mtxSaver[storePer * (1:(nPerTrial/storePer)),]
```
  
  
###_Results for 100-play_  

```r
## Run for 100-play
baseOutcomes <- getBaseOutcomes(myFileName="Play100Outcomes.csv",forceEQ=TRUE)
```

```
## [1] "Modifying probablities 6.3e-08 -99"
## [1] "New probablities 6.48000000379104e-08 -99"
## [1] "Probabilities sum to 1.  Outcomes has mean -0.506 and variance 18791"
```

```r
dfSummary <- calcOutcomes(baseOutcomes=baseOutcomes, nPlay=100)
```

```
## [1] "Running 100-play with nTrials=2000 nPerTrial=80000 and hurdle <=-500"
## [1] "Ouctomes across 1.6e+08 draws of 100-play have mean: -0.492 and variance: 19445"
```

```r
graphSummary(dfSummary, nPlay=100)
```

```
##     myTrial           myMax            myMin             myLast       
##  Min.   :   1.0   Min.   :   -73   Min.   :-155071   Min.   :-154189  
##  1st Qu.: 500.8   1st Qu.:  2895   1st Qu.: -73191   1st Qu.: -66509  
##  Median :1000.5   Median :  7803   Median : -52238   Median : -42846  
##  Mean   :1000.5   Mean   : 15309   Mean   : -54423   Mean   : -39390  
##  3rd Qu.:1500.2   3rd Qu.: 16206   3rd Qu.: -33632   3rd Qu.: -18920  
##  Max.   :2000.0   Max.   :216545   Max.   :     23   Max.   : 188937  
##                                                                       
##    myCond           myN_Cond         myVal_Cond    
##  Mode :logical   Min.   :   10.0   Min.   :-573.0  
##  FALSE:3         1st Qu.:   29.0   1st Qu.:-534.0  
##  TRUE :1997      Median :   63.0   Median :-520.0  
##  NA's :0         Mean   :  876.1   Mean   :-522.3  
##                  3rd Qu.:  224.0   3rd Qu.:-509.0  
##                  Max.   :66153.0   Max.   :-500.0  
##                  NA's   :3         NA's   :3
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
mtxCum100 <- mtxSaver[storePer * (1:(nPerTrial/storePer)),]
```
  
  
###_Plot out cumulative quantiles_  
The cumulative data has been stored and can be analyzed.  We prepare a quantiles function and apply it to the respective cumulative outcomes files.  
  

```r
fooFake <- function(x) { quantile(x,c(0.001,0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99,1)) }
myQuants001 <- t(apply(mtxCum001, 1, FUN=fooFake))
myQuants003 <- t(apply(mtxCum003, 1, FUN=fooFake))
myQuants005 <- t(apply(mtxCum005, 1, FUN=fooFake))
myQuants010 <- t(apply(mtxCum010, 1, FUN=fooFake))
myQuants050 <- t(apply(mtxCum050, 1, FUN=fooFake))
myQuants100 <- t(apply(mtxCum100, 1, FUN=fooFake))
```
  
  
Next, we generate a function for graphing the cumulative returns of various data:  
  

```r
graphCumulative <- function(quantFile, storePer=1, nPlay, 
                            yLimit=c(-20,5), abLimit=c(0,-4,-8)
                            ) {

    keyX <- seq(storePer, nPerTrial, by=storePer)
    
    ## Plot #1 is quantiles for cumulative returns (in units) by hand
    plot(x=keyX, y=5 * quantFile[keyX/storePer , 2]/1000, col="purple", 
         type="l", lwd=2, main=paste0(nPlay, "-play results"), 
         xlab="Hand number", ylim=yLimit
         )
    
    lines(x=keyX, y=5*quantFile[keyX/storePer , 3]/1000,col="blue")
    lines(x=keyX, y=5*quantFile[keyX/storePer , 5]/1000,col="orange")
    lines(x=keyX, y=5*quantFile[keyX/storePer , 6]/1000,col="dark green")

    abline(h=abLimit, lty=2)

    abline(v=c(storePer * which(5 * quantFile[,2]/1000 <= abLimit[2])[1], 
               storePer * which(5 * quantFile[,2]/1000 <= abLimit[3])[1]
               ), col="purple", lty=2
           )

    abline(v=c(storePer * which(5 * quantFile[,5]/1000 <= abLimit[2])[1], 
               storePer * which(5 * quantFile[,5]/1000 <= abLimit[3])[1]
               ), col="orange", lty=2
           )

    legend("topright", legend=c("Median","25%","5%","1%"), lwd=3, 
           cex=0.75, col=c("dark green","orange","blue","purple")
           )

    text(x=.4*nPerTrial, y=yLimit[2], 
         paste0("Results hit 1% at: ", 
                storePer * which(5 * quantFile[,2]/1000 <= abLimit[2])[1], 
                " and ", 
                storePer * which(5 * quantFile[,2]/1000 <= abLimit[3])[1]
                ) ,cex=0.75
         )

    text(x=.4*nPerTrial, y=(yLimit[2] + .1 * (yLimit[1] - yLimit[2]) ), 
         paste0("Results hit 25% at: ", 
                storePer * which(5 * quantFile[,5]/1000 <= abLimit[2])[1], 
                " and ", 
                storePer * which(5 * quantFile[,5]/1000 <= abLimit[3])[1]
                ) ,cex=0.75
         )
    
    
    ## Plot #2 is quantiles for cumulative returns (in percentage) by # base hands
    keyXPct <- keyX * nPlay
    
    plot(x=keyX, y=quantFile[keyX/storePer , 2] / keyXPct, col="purple", 
         type="l", lwd=2, main=paste0(nPlay, "-play results"), 
         xlab="Hand number", ylim=c(-0.1,0.025)
         )
    
    lines(x=keyX, y=quantFile[keyX/storePer , 3]/keyXPct,col="blue")
    lines(x=keyX, y=quantFile[keyX/storePer , 5]/keyXPct,col="orange")
    lines(x=keyX, y=quantFile[keyX/storePer , 6]/keyXPct,col="dark green")
    
    legend("topright", legend=c("Median","25%","5%","1%"), lwd=3, 
           cex=0.75, col=c("dark green","orange","blue","purple")
           )
    
    abline(h=c(0,-0.02,-0.04), lty=2)
}
```
  
  
And, then we run the function for 1/3/5/10/50/100 play.  
  

```r
graphCumulative(quantFile=myQuants001, storePer=storePer, nPlay=1, 
                yLimit=c(-10, 2.5), abLimit=c(0,-2,-4)
                )
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-2.png)

```r
graphCumulative(quantFile=myQuants003, storePer=storePer, nPlay=3, 
                yLimit=c(-20, 5), abLimit=c(0,-5,-10)
                )
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-3.png)![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-4.png)

```r
graphCumulative(quantFile=myQuants005, storePer=storePer, nPlay=5, 
                yLimit=c(-50, 12.5), abLimit=c(0,-10,-20)
                )
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-5.png)![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-6.png)

```r
graphCumulative(quantFile=myQuants010, storePer=storePer, nPlay=10, 
                yLimit=c(-100, 25), abLimit=c(0,-20,-40)
                )
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-7.png)![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-8.png)

```r
graphCumulative(quantFile=myQuants050, storePer=storePer, nPlay=50, 
                yLimit=c(-500, 125), abLimit=c(0,-100,-200)
                )
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-9.png)![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-10.png)

```r
graphCumulative(quantFile=myQuants100, storePer=storePer, nPlay=100, 
                yLimit=c(-1000, 250), abLimit=c(0,-200,-400)
                )
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-11.png)![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-12.png)
  
  
## Additional Comparisons  
Additionally, the likelihood of having various percentage results at different play lengths is calculated.


```r
for (nHands in c(100, 250, 500, 1000, 2000, 4000)) {
    keyIndex <- round(nHands / storePer, 0)
    keyHands <- keyIndex * storePer
    
    res001 <- mtxCum001[keyIndex,] / ( 1 * keyHands )
    res003 <- mtxCum003[keyIndex,] / ( 3 * keyHands )
    res005 <- mtxCum005[keyIndex,] / ( 5 * keyHands )
    res010 <- mtxCum010[keyIndex,] / ( 10 * keyHands )
    res050 <- mtxCum050[keyIndex,] / ( 50 * keyHands )
    res100 <- mtxCum100[keyIndex,] / ( 100 * keyHands )
    
    plot(x=(1:nTrials)/nTrials, y=res001[order(res001)], type="l", col="red",
         main=paste0("Results for ",keyHands," hands of base play"),
         xlab="Percentile", ylab="Percent return",ylim=c(-0.3,0.1)
         )
    
    abline(h=c(0,-0.1,-0.2),lty=2)
    
    lines(x=(1:nTrials)/nTrials, y=res003[order(res003)], col="purple")
    lines(x=(1:nTrials)/nTrials, y=res005[order(res005)], col="blue")
    ## lines(x=(1:nTrials)/nTrials, y=res010[order(res010)], col="orange")
    ## lines(x=(1:nTrials)/nTrials, y=res050[order(res050)], col="pink")
    lines(x=(1:nTrials)/nTrials, y=res100[order(res100)], col="dark green")
    
    legend("topleft",legend=c("1-play","3-play","5-play","100-play"),
           lwd=2,col=c("red","purple","blue","dark green"),cex=0.75)
}
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-2.png)![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-3.png)![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-4.png)![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-5.png)![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-6.png)
  
  
As well, the probability of being behind by more than X% plus Y$ is calculated for a few of the distributions of N-play.  
  

```r
riskRuinPctUnit <- function(frameResults, nPlay, pctStop) {
    
    frameMod <- rep(0, ncol(frameResults))
    frameTemp <- frameResults
    
    for (intCtr in 1:nrow(frameResults)) {
        frameTemp[intCtr , ] <- 5 * (frameResults[intCtr , ] + pctStop * (storePer * intCtr) * nPlay)
    }
    
    for (intCtr in 1:ncol(frameResults)) {
        frameMod[intCtr] <- min(frameTemp[ , intCtr])
    }
    
    frameMod <- frameMod[order(frameMod)]
    
    plot(x=(1:length(frameMod))/length(frameMod), y=frameMod/1000, type="l", col="purple", 
         main=paste0("Worst unit loss below ",round(100*pctStop,2),"% for ",nPlay,
                     "-play, through ",round(nPerTrial/1000,1),"k base hands"
                    ),
         xlab="Percentile", ylab=paste0("Worst unit loss (000s) below ",round(100*pctStop,2),"%")
         )
    
    abline(v=c(0.50,0.10,0.01), lty=2, col=c("dark green","blue","red"))
    keyUnits <- frameMod[round(c(0.50, 0.10, 0.01) * length(frameMod),0)]/1000
    abline(h=keyUnits, lty=2, col=c("dark green","blue","red"))

    legend("bottomright", col=c("dark green","blue","red"), lty=2, lwd=2, 
           legend=paste0(c("50% (", "10% (", "1%   ("),round(keyUnits,2),")")
           )
}
```
  
  
And then call the function a few times for a few scenarios of interest:  
  

```r
riskRuinPctUnit(frameResults=mtxCum001, nPlay=1, pctStop=0.01)  ## 1-play, 1%
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

```r
riskRuinPctUnit(frameResults=mtxCum003, nPlay=3, pctStop=0.01)  ## 3-play, 1%
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-2.png)

```r
riskRuinPctUnit(frameResults=mtxCum005, nPlay=5, pctStop=0.01)  ## 5-play, 1%
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-3.png)

```r
riskRuinPctUnit(frameResults=mtxCum010, nPlay=10, pctStop=0.01)  ## 10-play, 1%
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-4.png)

```r
riskRuinPctUnit(frameResults=mtxCum001, nPlay=1, pctStop=0.02)  ## 1-play, 2%
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-5.png)

```r
riskRuinPctUnit(frameResults=mtxCum003, nPlay=3, pctStop=0.02)  ## 3-play, 2%
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-6.png)

```r
riskRuinPctUnit(frameResults=mtxCum005, nPlay=5, pctStop=0.02)  ## 5-play, 2%
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-7.png)

```r
riskRuinPctUnit(frameResults=mtxCum010, nPlay=10, pctStop=0.02)  ## 10-play, 2%
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-8.png)
  
