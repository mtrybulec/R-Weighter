weight.data.by.target.distribution <- function(data.frame, target.distribution) 
{
    # Split the data.frame using target.distribution variables.
    # Assumes the 'weight' variable is the last variable in target.distribution. 
    target.vars <- names(target.distribution)[1:length(target.distribution) - 1]
    data.groups <- split(data.frame, data.frame[target.vars])

    # Sum weights from data.frame using split groups:
    data.sums <- sapply(data.groups, function(data.group) colSums(data.group["weight"]))

    # Re-weight:
    reweight.factors <- target.distribution["weight"] / data.sums
    reweighted.groups <- mapply(
        function(data.group, reweight.factor) 
        {
            data.group["weight"] <- data.group["weight"] * reweight.factor
            data.group
        }, 
        data.groups, unlist(reweight.factors), SIMPLIFY = FALSE, USE.NAMES = FALSE)

    # Result:
    do.call("rbind", reweighted.groups)
}

calculate.weight.fit.for.target.distribution <- function(data.frame, target.distribution)
{
    # Split the data.frame using target.distribution variables.
    # Assumes the 'weight' variable is the last variable in target.distribution. 
    target.vars <- names(target.distribution)[1:length(target.distribution) - 1]
    data.groups <- split(data.frame, data.frame[target.vars])
    
    # Sum weights from data.frame using split groups:
    data.sums <- sapply(data.groups, function(data.group) colSums(data.group["weight"]))
    
    sum(abs(data.sums - target.distribution["weight"]))
}

weight.data.by.target.distributions <- function(data.frame, target.distributions) 
{
    for(index in 1:length(target.distributions))
    {
        data.frame <- weight.data.by.target.distribution(data.frame, target.distributions[[index]])
    }
    
    data.frame
}

calculate.weight.fit.for.target.distributions <- function(data.frame, target.distributions) 
{
    fit <- 0
    
    for(index in 1:length(target.distributions))
    {
        fit <- fit + calculate.weight.fit.for.target.distribution(data.frame, target.distributions[[index]])
    }
    
    fit
}

weight.data <- function(data.frame, target.distributions, epsilon = 0.01, max.steps = 10)
{
    fit <- calculate.weight.fit.for.target.distributions(data.frame, target.distributions)
    step <- 0
    
    while(fit > epsilon)
    {
        step <- step + 1
        cat("  - step ", step, ", fit: ", fit, "\n", sep = "")
        
        data.frame <- weight.data.by.target.distributions(data.frame, target.distributions)
        fit <- calculate.weight.fit.for.target.distributions(data.frame, target.distributions)
        
        if(step >= max.steps)
        {
            break
        }
    }
    
    cat("Final fit: ", fit, "\n\n", sep = "")
    data.frame
}    
    
example <- function()
{
    df <- data.frame(sex = c(1, 1, 2, 2), age = c(1, 2, 3, 1), weight = c(1, 1, 1, 1))

    cat("Original data frame:\n")
    print(df)
    
    td1 <- data.frame(sex = c(1, 2), weight = c(3.0, 4.0))
    td2 <- data.frame(age = c(3, 2, 1), weight = c(2.5, 1.5, 3.0))
    
    cat("\nTarget distribution 1:\n")
    print(td1)
    cat("\nTarget distribution 2:\n")
    print(td2)
    
    cat("\nWeighting:\n")
    df <- weight.data(df, list(td1, td2), epsilon = 0.0001, max.steps = 100)
    
    cat("Final data frame:\n")
    df
}