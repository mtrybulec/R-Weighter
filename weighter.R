weight.data.by.target.distribution <- function(data.frame, 
                                               target.distribution, 
                                               data.frame.weight.name = "weight",
                                               target.distribution.weight.name = "weight") 
{
    # Handle the case when weight variable names are the same in the data frame and the target distribution
    # (after mering of the two, if the variable names are the same, they'll be renamed with standard suffixes of '.x' and '.y').
    merged.data.frame.weight.name <- data.frame.weight.name
    merged.target.distribution.weight.name <- target.distribution.weight.name
    
    if(data.frame.weight.name == target.distribution.weight.name) 
    {
        merged.data.frame.weight.name <- paste(merged.data.frame.weight.name, ".x", sep = "")    
        merged.target.distribution.weight.name <- paste(merged.target.distribution.weight.name, ".y", sep = "")    
    }

    # Split the data.frame using target.distribution variables.
    # Assumes the 'weight' variable is the last variable in target.distribution. 
    target.vars <- names(target.distribution)[names(target.distribution) != target.distribution.weight.name]
    merged.data <- merge(data.frame, target.distribution, by = target.vars)
    data.groups <- split(merged.data, merged.data[target.vars], drop = TRUE)

    # Sum weights from data.frame using split groups; calculate the reweight factor and apply it:
    reweighted.groups <- lapply(
        data.groups, 
        function(data.group) 
        {
            weight.sum <- colSums(data.group[merged.data.frame.weight.name])
            reweight.factor <- data.group[merged.target.distribution.weight.name] / weight.sum
            data.group[merged.data.frame.weight.name] <- data.group[merged.data.frame.weight.name] * reweight.factor
            data.group
        })

    reweighted.data.frame <- do.call("rbind", reweighted.groups)

    # Drop the target.distribution weight from the result; 
    # rename the data.frame weight variable name to its original name:
    reweighted.data.frame[[merged.target.distribution.weight.name]] <- NULL
    colnames(reweighted.data.frame)[colnames(reweighted.data.frame) == merged.data.frame.weight.name] <- data.frame.weight.name

    reweighted.data.frame
}

calculate.weight.fit.for.target.distribution <- function(data.frame, 
                                                         target.distribution, 
                                                         data.frame.weight.name = "weight",
                                                         target.distribution.weight.name = "weight")
{
    # Handle the case when weight variable names are the same in the data frame and the target distribution
    # (after mering of the two, if the variable names are the same, they'll be renamed with standard suffixes of '.x' and '.y').
    merged.data.frame.weight.name <- data.frame.weight.name
    merged.target.distribution.weight.name <- target.distribution.weight.name
    
    if(data.frame.weight.name == target.distribution.weight.name) 
    {
        merged.data.frame.weight.name <- paste(merged.data.frame.weight.name, ".x", sep = "")    
        merged.target.distribution.weight.name <- paste(merged.target.distribution.weight.name, ".y", sep = "")    
    }
    
    # Split the data.frame using target.distribution variables.
    # Assumes the 'weight' variable is the last variable in target.distribution. 
    target.vars <- names(target.distribution)[names(target.distribution) != target.distribution.weight.name]
    merged.data <- merge(data.frame, target.distribution, by = target.vars)
    data.groups <- split(merged.data, merged.data[target.vars], drop = TRUE)

    # Sum weights from data.frame using split groups; 
    # subtract the target weight from the sum:
    data.sums <- sapply(
        data.groups, 
        function(data.group) 
        {
            abs(colSums(data.group[merged.data.frame.weight.name]) - data.group[[merged.target.distribution.weight.name]][1])
        })
    
    sum(unlist(data.sums))
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
    original.data.frame.names = names(data.frame)
    
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
    data.frame[original.data.frame.names]
}    
    
example1 <- function()
{
    df <- data.frame(sex = c(1, 1, 2, 2), age = c(1, 2, 3, 1), weight = c(1, 1, 1, 1))

    cat("Original data frame:\n")
    print(df)
    
    td1 <- data.frame(sex = c(1, 2), weight1 = c(3.0, 4.0))
    td2 <- data.frame(weight2 = c(2.5, 1.5, 3.0), age = c(3, 2, 1))
    
    cat("\nTarget distribution 1:\n")
    print(td1)
    cat("\nTarget distribution 2:\n")
    print(td2)
    
    cat("\nWeighting:\n")
    df <- weight.data(
        df, 
        list(td1, td2), 
        data.frame.weight.name = "weight", 
        target.distribution.weight.names = c("weight1", "weight2"),
        epsilon = 0.000001, 
        max.steps = 100)
    
    cat("Final data frame:\n")
    df
}

example2 <- function()
{
    df <- data.frame(sex = c(1, 1, 2, 2), age = c(1, 2, 3, 1), weight = c(1, 1, 1, 1))
    
    cat("Original data frame:\n")
    print(df)
    
    td <- data.frame(sex = c(1, 2, 1, 2), age = c(1, 1, 2, 3), weight = c(1.5, 1.5, 1.5, 2.5))

    cat("\nTarget distribution:\n")
    print(td)

    cat("\nWeighting:\n")
    df <- weight.data(
        df, 
        list(td), 
        epsilon = 0.000001, 
        max.steps = 100)
    
    cat("Final data frame:\n")
    df
}