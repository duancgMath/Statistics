
# ==========================================================
# This is a classifier based on distance(Mahalanobis Distance)
# Input - TrainX -- training data set
#       - Trainy -- training labels
#       - TestX  -- test data set
# Output - label -- predict labels of training data set 
# =========================================================

classify.distance <- function(TrainX, Trainy, TestX=NULL, var.equal=FALSE){
    # Formulate Input datas
    if (is.null(TestX)) {
        TestX = TrainX
    }
    if (is.vector(TestX)) {
        TestX = t(as.matrix(TestX))
    } else if (!is.matrix(TestX)) {
        TestX = as.matrix(TestX)
    }
    
    # Size of datas
    test.n = nrow(TestX)
    class.n = length(levels(Trainy))
    
    # Mahalanobis Distance
    mu = matrix(0, nrow=class.n, ncol=ncol(TrainX))
    for (i in 1:class.n) {
        mu[i,] = colMeans(TrainX[Trainy==i,])
    }
    dist.Maha = matrix(0, nrow=class.n, ncol=test.n)
    if (var.equal) {
        for (i in 1:class.n) {
            dist.Maha[i,] = mahalanobis(TestX, mu[i,], var(TrainX))
        }
    }else {
        for (i in 1:class.n) {
            dist.Maha[i,] = mahalanobis(TestX, mu[i,], var(TrainX[Trainy==i,]))
        }
    }
    
    # Find min distence
    label = matrix(rep(0,test.n),ncol=1,
                   dimnames=list(1:test.n,"labels"))
    find.min = apply(dist.Maha,2,which.min)
    for (i in 1:test.n) {
        label[i] = find.min[i]
    }
    
    return(label)
}

