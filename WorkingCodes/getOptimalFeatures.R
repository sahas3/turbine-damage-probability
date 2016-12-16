# get optimat set of features from the whole set of generated features (Attributes) for the turbines

getOptimalFeatures <- function(trainData, trainClass, testData, testClass,
                               fsAlgo = 'rfe', # method for feature selection : can be 'rfe', 'SA' or 'GA'
                               rfeFunc = rfFuncs, # feature selection method for 'rfe'
                               parallelFlag = F, # parallel processing flag
                               caretMethod = NULL, # method for feature selection using CARET classifier models
                               tuneGrid = NULL, # tuneGrid : passed to CARET train
                               preProcessStr = NULL, # pre-processing function
                               numCores = detectCores()/2 # number of cores to be used for parallel processing
)
{
    seedValue <- 666
    set.seed(seedValue)

    if(is.null(tuneGrid))
    {
        tuneLength <- length(names(trainData))
    } else {
        tuneLength <- NULL
    }

    # generate folds for running feature selection
    CVFoldsFS <- createMultiFolds(trainClass, times = 5)

    # generate folds for resampling
    set.seed(seedValue + 1)
    CVFoldsTR <- createMultiFolds(trainClass, times = 5)

    set.seed(seedValue)

    # start parallel processing
    if (parallelFlag)
    {
        clusters <- makeCluster(numCores) #, type = 'FORK') # number of cores to use
        registerDoParallel(clusters)
    }

    if (fsAlgo == 'rfe')
    {
        # Recursive Feature Selection in CARET

        if (!is.null(caretMethod)) rfeFunc <- caretFuncs
        rfeFunc$summary <- twoClassSummary # use twoClassSummary to have 'ROC' as performance measure

        # define the control parameters for RFE
        rfeCtrlParams <- rfeControl(
            functions = rfeFunc, # cat(rfeFunc) # uses caret methods, # method = "svmRadial" to use SVM
            method = "repeatedcv",
            rerank = T,
            # number = 10, repeats = 5,
            index = CVFoldsFS,
            allowParallel = parallelFlag)


        # define control parameters for train
        trCtrlParams = trainControl(method = "repeatedcv"
                                    # , number = 10, repeats = 5
                                    , index = CVFoldsTR
                                    # , search = 'random'#
                                    # , selectionFunction = "oneSE"
                                    , classProbs = T, summaryFunction = twoClassSummary
                                    , allowParallel = parallelFlag, verboseIter = F
                                    , savePredictions = T
                                    # , search = 'random'
                                    ## Adaptive resampling information:
                                    # adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE)
        )

        set.seed(seedValue)
        # run the RFE algorithm
        if (is.null(caretMethod))
        {
            fsModel <- rfe(trainData, trainClass, sizes = 2:ncol(trainData)
                           , rfeControl = rfeCtrlParams
                           , trControl = trCtrlParams
                           , preProcess = preProcessStr
                           , metric = 'ROC'
            )
        } else {
            # if caretMethod is used instead of rfeFunc more options can be passed
            fsModel <- rfe(trainData, trainClass, sizes = 2:ncol(trainData)
                           , rfeControl = rfeCtrlParams
                           , method = caretMethod
                           # , tuneGrid = expand.grid(C = 10.^(-2:2)),
                           # , preProc = c("center", "scale"),
                           ## Inner resampling process
                           , trControl = trCtrlParams
                           , metric = 'ROC'
                           , preProcess = preProcessStr
                           , tuneGrid = tuneGrid # extra params
                           , tuneLength = tuneLength
            )
        }

        # get the prediction values
        predValsTrain <- predict(fsModel, trainData)
        predValsTest <- predict(fsModel, testData)

        predClassTrain <- predValsTrain[,1]
        predClassTest <- predValsTest[,1]

        predProbsTrain <- predValsTrain[,2]
        predProbsTest <- predValsTest[,2]

        # get the chosen feature set
        predAttrs <- predictors(fsModel)


    } else if (fsAlgo == 'GA')
    {

        # Genetic Algorithm

        caretGA$fitness_extern <- twoClassSummary

        gaCtrlParams <- gafsControl(
            # functions = rfGA, # Assess fitness with RF
            # functions = treebagGA, # Assess fitness with tree bag
            functions = caretGA, # Assess fitness with caretGA
            method = "repeatedcv",  # 10 fold cross validation
            # number = 10, repeats = 3,
            index = CVFoldsFS,
            genParallel = TRUE, # Use parallel programming
            allowParallel = TRUE
            , metric = c(internal = "ROC", external = "ROC") # set up metrics to be used for performance evaluation
            , maximize = c(internal = T, external = T)
        )

        ctrlParams = trainControl(method = "repeatedcv"
                                  # , number = 10, repeats = 3
                                  , index = CVFoldsTR
                                  # , search = 'random'# selectionFunction = "oneSE",
                                  , classProbs = T, summaryFunction = twoClassSummary
                                  , allowParallel = T, verboseIter = F
                                  , savePredictions = T
                                  # , search = 'random'
                                  ## Adaptive resampling information:
                                  # adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE)
        )

        set.seed(seedValue)
        lev <- levels(trainClass) # Set the levels

        fsModel <- gafs(trainData, trainClass
                        , iters = 500 # 100 generations of algorithm
                        , popSize = 20 # population size for each generation
                        , elite = 1 # elite population candidate
                        , levels = lev
                        , gafsControl = gaCtrlParams
                        ### now pass arguments to `train`
                        , method = "svmRadialCost"
                        , metric = "ROC"
                        , trControl = ctrlParams
                        , preProcess = preProcessStr
        )

        predProbsTest <- predict(fsModel, testData, type = 'prob')[,1]
        predAttrs <- predictors(fsModel$fit)

    } else if (fsAlgo == 'SA')
    {

        # Simulated Annealing : most of the function call is similar to GA

        caretSA$fitness_extern <- twoClassSummary

        saCtrlParams <- safsControl(
            # functions = rfGA, # Assess fitness with RF
            # functions = treebagGA, # Assess fitness with tree bag
            functions = caretSA, # Assess fitness with caret
            method = "repeatedcv",    # 10 fold cross validation
            # number = 10,
            repeats = 5,
            index = CVFoldsFS, ## Here are the exact folds to used:
            allowParallel = TRUE, # Use parallel programming
            improve = 25 # SA iterations can occur without improvement
            , metric = c(internal = "ROC", external = "ROC") ## optimization metric
            , maximize = c(internal = T, external = T)
        )

        ctrlParams = trainControl(method = "repeatedcv",
                                  # , number = 10,
                                  repeats = 5
                                  , index = CVFoldsTR
                                  # , search = 'random'# selectionFunction = "oneSE",
                                  , classProbs = T, summaryFunction = twoClassSummary
                                  , allowParallel = F, verboseIter = F
                                  , savePredictions = T
                                  # , search = 'random'
                                  ## Adaptive resampling information:
                                  # adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE)
        )

        set.seed(seedValue)
        lev <- levels(trainClass) # Set the levels

        fsModel <- safs(trainData, trainClass
                        , iters = 500 # 100 generations of algorithm
                        , safsControl = saCtrlParams
                        ### now pass arguments to `train`
                        , method = "svmRadialCost"
                        , metric = "ROC"
                        , tuneLength = tuneLength
                        , preProcess = preProcessStr
                        , trControl = ctrlParams
        )

        predProbsTest <- predict(fsModel, testData, type = 'prob')[,1]
        predAttrs <- predictors(fsModel$fit)
    }

    # print('Train Data :')
    # print(confusionMatrix(predClassTrain, trainClass))

    # print('Test Data :')
    # print(confusionMatrix(predClassTest, testClass))

    # plot the ROC curve after feature selection
    rocVal <- pROC::roc(testClass, predProbsTest)
    plotROCs(list(selectedFeatures = rocVal), legendStr = 'after feature selection')
    print('Selected Features are: ')
    print(predAttrs)

    if (parallelFlag) stopCluster(clusters)
    return(fsModel)
}