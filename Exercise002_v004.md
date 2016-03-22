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
nPerTrial <- 50000
myHurdle <- "<=-500"
storePer <- 10
```
  
  
## Results  
The simulation is repeated for each of the desired N-play outcomes.  In this case, we have run the routine for 1/3/5/10/50/100 play.  

Further, we save every 10th hand for 1-play, 3-play, 5-play, and 10-play.
  

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
## [1] "Running 1-play with nTrials=2000 nPerTrial=50000 and hurdle <=-500"
## [1] "Ouctomes across 1e+08 draws of 1-play have mean: -0.00459 and variance: 19.5"
```

```r
graphSummary(dfSummary, nPlay=1)
```

```
##     myTrial           myMax            myMin             myLast       
##  Min.   :   1.0   Min.   :  -1.0   Min.   :-2737.0   Min.   :-2730.0  
##  1st Qu.: 500.8   1st Qu.:  59.0   1st Qu.:-1192.2   1st Qu.: -964.0  
##  Median :1000.5   Median : 330.0   Median : -777.0   Median : -335.5  
##  Mean   :1000.5   Mean   : 595.9   Mean   : -830.9   Mean   : -229.7  
##  3rd Qu.:1500.2   3rd Qu.: 934.2   3rd Qu.: -409.8   3rd Qu.:  394.0  
##  Max.   :2000.0   Max.   :3948.0   Max.   :    1.0   Max.   : 3719.0  
##                                                                       
##    myCond           myN_Cond       myVal_Cond  
##  Mode :logical   Min.   : 4397   Min.   :-500  
##  FALSE:620       1st Qu.:11769   1st Qu.:-500  
##  TRUE :1380      Median :16673   Median :-500  
##  NA's :0         Mean   :19550   Mean   :-500  
##                  3rd Qu.:25209   3rd Qu.:-500  
##                  Max.   :49957   Max.   :-500  
##                  NA's   :620     NA's   :620
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
## [1] "Running 3-play with nTrials=2000 nPerTrial=50000 and hurdle <=-500"
## [1] "Ouctomes across 1e+08 draws of 3-play have mean: -0.0158 and variance: 67.2"
```

```r
graphSummary(dfSummary, nPlay=3)
```

```
##     myTrial           myMax            myMin           myLast       
##  Min.   :   1.0   Min.   :  -3.0   Min.   :-5951   Min.   :-5847.0  
##  1st Qu.: 500.8   1st Qu.: 151.8   1st Qu.:-2612   1st Qu.:-2120.2  
##  Median :1000.5   Median : 685.0   Median :-1696   Median : -870.0  
##  Mean   :1000.5   Mean   :1003.5   Mean   :-1823   Mean   : -792.4  
##  3rd Qu.:1500.2   3rd Qu.:1491.2   3rd Qu.: -887   3rd Qu.:  423.5  
##  Max.   :2000.0   Max.   :6106.0   Max.   :   -1   Max.   : 5789.0  
##                                                                     
##    myCond           myN_Cond       myVal_Cond    
##  Mode :logical   Min.   : 1200   Min.   :-502.0  
##  FALSE:251       1st Qu.: 3847   1st Qu.:-501.0  
##  TRUE :1749      Median : 6292   Median :-501.0  
##  NA's :0         Mean   :10279   Mean   :-500.7  
##                  3rd Qu.:12515   3rd Qu.:-500.0  
##                  Max.   :49529   Max.   :-500.0  
##                  NA's   :251     NA's   :251
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
## [1] "Running 5-play with nTrials=2000 nPerTrial=50000 and hurdle <=-500"
## [1] "Ouctomes across 1e+08 draws of 5-play have mean: -0.0278 and variance: 128"
```

```r
graphSummary(dfSummary, nPlay=5)
```

```
##     myTrial           myMax            myMin           myLast       
##  Min.   :   1.0   Min.   :  -5.0   Min.   :-8444   Min.   :-8440.0  
##  1st Qu.: 500.8   1st Qu.: 239.8   1st Qu.:-3794   1st Qu.:-3151.2  
##  Median :1000.5   Median : 897.5   Median :-2588   Median :-1595.5  
##  Mean   :1000.5   Mean   :1310.5   Mean   :-2715   Mean   :-1391.4  
##  3rd Qu.:1500.2   3rd Qu.:1844.0   3rd Qu.:-1434   3rd Qu.:  200.5  
##  Max.   :2000.0   Max.   :9513.0   Max.   :   -3   Max.   : 9006.0  
##                                                                     
##    myCond           myN_Cond       myVal_Cond    
##  Mode :logical   Min.   :  577   Min.   :-504.0  
##  FALSE:170       1st Qu.: 2137   1st Qu.:-502.0  
##  TRUE :1830      Median : 3834   Median :-501.0  
##  NA's :0         Mean   : 7425   Mean   :-501.2  
##                  3rd Qu.: 8552   3rd Qu.:-500.0  
##                  Max.   :47281   Max.   :-500.0  
##                  NA's   :170     NA's   :170
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
## [1] "Running 10-play with nTrials=2000 nPerTrial=50000 and hurdle <=-500"
## [1] "Ouctomes across 1e+08 draws of 10-play have mean: -0.0517 and variance: 336"
```

```r
graphSummary(dfSummary, nPlay=10)
```

```
##     myTrial           myMax           myMin            myLast        
##  Min.   :   1.0   Min.   :  -10   Min.   :-14629   Min.   :-14523.0  
##  1st Qu.: 500.8   1st Qu.:  423   1st Qu.: -6548   1st Qu.: -5549.8  
##  Median :1000.5   Median : 1336   Median : -4378   Median : -2796.0  
##  Mean   :1000.5   Mean   : 2025   Mean   : -4643   Mean   : -2585.1  
##  3rd Qu.:1500.2   3rd Qu.: 2794   3rd Qu.: -2380   3rd Qu.:   100.2  
##  Max.   :2000.0   Max.   :19189   Max.   :     6   Max.   : 17174.0  
##                                                                      
##    myCond           myN_Cond       myVal_Cond    
##  Mode :logical   Min.   :  189   Min.   :-509.0  
##  FALSE:81        1st Qu.:  910   1st Qu.:-504.0  
##  TRUE :1919      Median : 1795   Median :-502.0  
##  NA's :0         Mean   : 4699   Mean   :-502.3  
##                  3rd Qu.: 4688   3rd Qu.:-501.0  
##                  Max.   :48995   Max.   :-500.0  
##                  NA's   :81      NA's   :81
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
## [1] "Running 50-play with nTrials=2000 nPerTrial=50000 and hurdle <=-500"
## [1] "Ouctomes across 1e+08 draws of 50-play have mean: -0.245 and variance: 4978"
```

```r
graphSummary(dfSummary, nPlay=50)
```

```
##     myTrial           myMax           myMin            myLast      
##  Min.   :   1.0   Min.   :  -38   Min.   :-53298   Min.   :-52223  
##  1st Qu.: 500.8   1st Qu.: 1550   1st Qu.:-26045   1st Qu.:-22761  
##  Median :1000.5   Median : 4000   Median :-18115   Median :-13520  
##  Mean   :1000.5   Mean   : 6909   Mean   :-19036   Mean   :-12262  
##  3rd Qu.:1500.2   3rd Qu.: 8243   3rd Qu.:-10837   3rd Qu.: -4042  
##  Max.   :2000.0   Max.   :72961   Max.   :  -112   Max.   : 67878  
##                                                                    
##    myCond           myN_Cond       myVal_Cond    
##  Mode :logical   Min.   :   23   Min.   :-540.0  
##  FALSE:20        1st Qu.:   82   1st Qu.:-518.0  
##  TRUE :1980      Median :  193   Median :-511.0  
##  NA's :0         Mean   : 1239   Mean   :-511.8  
##                  3rd Qu.:  618   3rd Qu.:-504.0  
##                  Max.   :48083   Max.   :-500.0  
##                  NA's   :20      NA's   :20
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
  
  
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
## [1] "Running 100-play with nTrials=2000 nPerTrial=50000 and hurdle <=-500"
## [1] "Ouctomes across 1e+08 draws of 100-play have mean: -0.521 and variance: 17670"
```

```r
graphSummary(dfSummary, nPlay=100)
```

```
##     myTrial           myMax            myMin             myLast       
##  Min.   :   1.0   Min.   :   -52   Min.   :-115779   Min.   :-115213  
##  1st Qu.: 500.8   1st Qu.:  2409   1st Qu.: -50995   1st Qu.: -45404  
##  Median :1000.5   Median :  6537   Median : -36634   Median : -29048  
##  Mean   :1000.5   Mean   : 11850   Mean   : -37862   Mean   : -26065  
##  3rd Qu.:1500.2   3rd Qu.: 13616   3rd Qu.: -22490   3rd Qu.: -11343  
##  Max.   :2000.0   Max.   :227971   Max.   :    -87   Max.   : 227743  
##                                                                       
##    myCond           myN_Cond         myVal_Cond    
##  Mode :logical   Min.   :    9.0   Min.   :-576.0  
##  FALSE:7         1st Qu.:   27.0   1st Qu.:-534.0  
##  TRUE :1993      Median :   58.0   Median :-520.0  
##  NA's :0         Mean   :  690.6   Mean   :-522.8  
##                  3rd Qu.:  208.0   3rd Qu.:-510.0  
##                  Max.   :46172.0   Max.   :-500.0  
##                  NA's   :7         NA's   :7
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
  
  
###_Plot out cumulative quantiles_  
The cumulative data for 1/3/5/10-play has been stored and can be analyzed.  We prepare a quantiles function and apply it to the respective cumulative outcomes files.  
  

```r
fooFake <- function(x) { quantile(x,c(0.001,0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99,1)) }
myQuants001 <- t(apply(mtxCum001, 1, FUN=fooFake))
myQuants003 <- t(apply(mtxCum003, 1, FUN=fooFake))
myQuants005 <- t(apply(mtxCum005, 1, FUN=fooFake))
myQuants010 <- t(apply(mtxCum010, 1, FUN=fooFake))
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
  
  
And, then we run the function for 1/3/5/10 play.  
  

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

