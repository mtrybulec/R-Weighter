weight.data.by.target.distribution <- function(data.frame, 
                                               target.distribution, 
                                               data.frame.weight.name = "weight",
                                               target.distribution.weight.name = "weight") 
{
    # Split the data.frame using target.distribution variables.
    # Assumes the 'weight' variable is the last variable in target.distribution. 
    target.vars <- names(target.distribution)[1:length(target.distribution) - 1]
    data.groups <- split(data.frame, data.frame[target.vars])

    # Sum weights from data.frame using split groups:
    data.sums <- sapply(data.groups, function(data.group) colSums(data.group[data.frame.weight.name]))

    # Re-weight:
    reweight.factors <- target.distribution[target.distribution.weight.name] / data.sums
    reweighted.groups <- mapply(
        function(data.group, reweight.factor) 
        {
            data.group[data.frame.weight.name] <- data.group[data.frame.weight.name] * reweight.factor
            data.group
        }, 
        data.groups, unlist(reweight.factors), SIMPLIFY = FALSE, USE.NAMES = FALSE)

    # Result:
    do.call("rbind", reweighted.groups)
}

calculate.weight.fit.for.target.distribution <- function(data.frame, 
                                                         target.distribution, 
                                                         data.frame.weight.name = "weight",
                                                         target.distribution.weight.name = "weight")
{
    # Split the data.frame using target.distribution variables.
    # Assumes the 'weight' variable is the last variable in target.distribution. 
    target.vars <- names(target.distribution)[1:length(target.distribution) - 1]
    data.groups <- split(data.frame, data.frame[target.vars])
    
    # Sum weights from data.frame using split groups:
    data.sums <- sapply(data.groups, function(data.group) colSums(data.group[data.frame.weight.name]))
    
    sum(abs(data.sums - target.distribution[target.distribution.weight.name]))
}

weight.data.by.target.distributions <- function(data.frame, 
                                                target.distributions, 
                                                data.frame.weight.name = "weight",
                                                target.distribution.weight.names = "weight") 
{
    for(index in 1:length(target.distributions))
    {
        data.frame <- weight.data.by.target.distribution(
            data.frame, 
            target.distributions[[index]], 
            data.frame.weight.name,
            target.distribution.weight.names[index])
    }
    
    data.frame
}

calculate.weight.fit.for.target.distributions <- function(data.frame, 
                                                          target.distributions, 
                                                          data.frame.weight.name = "weight",
                                                          target.distribution.weight.names = "weight") 
{
    fit <- 0
    
    for(index in 1:length(target.distributions))
    {
        fit <- fit + calculate.weight.fit.for.target.distribution(
            data.frame, 
            target.distributions[[index]], 
            data.frame.weight.name,
            target.distribution.weight.names[index])
    }
    
    fit
}

weight.data <- function(data.frame, 
                        target.distributions, 
                        data.frame.weight.name = "weight",
                        target.distribution.weight.names = "weight",
                        epsilon = 0.01, 
                        max.steps = 10)
{
    target.distribution.weight.names <- rep(target.distribution.weight.names, length.out = length(target.distributions))
    
    fit <- calculate.weight.fit.for.target.distributions(
        data.frame, 
        target.distributions, 
        data.frame.weight.name,
        target.distribution.weight.names)
    step <- 0
    
    while(fit > epsilon)
    {
        step <- step + 1
        cat("  - step ", step, ", fit: ", fit, "\n", sep = "")
        
        data.frame <- weight.data.by.target.distributions(
            data.frame, 
            target.distributions, 
            data.frame.weight.name,
            target.distribution.weight.names)
        fit <- calculate.weight.fit.for.target.distributions(
            data.frame, 
            target.distributions, 
            data.frame.weight.name,
            target.distribution.weight.names)
        
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
    df <- data.frame(sex = c(1, 1, 2, 2), age = c(1, 2, 3, 1), df.weight = c(1, 1, 1, 1))

    cat("Original data frame:\n")
    print(df)
    
    td1 <- data.frame(sex = c(1, 2), weight1 = c(3.0, 4.0))
    td2 <- data.frame(age = c(3, 2, 1), weight2 = c(2.5, 1.5, 3.0))

    cat("\nTarget distribution 1:\n")
    print(td1)
    cat("\nTarget distribution 2:\n")
    print(td2)
    
    cat("\nWeighting:\n")
    df <- weight.data(
        df, 
        list(td1, td2), 
        data.frame.weight.name = "df.weight", 
        target.distribution.weight.names = c("weight1", "weight2"),
        epsilon = 0.0001, 
        max.steps = 100)
    
    cat("Final data frame:\n")
    df
}