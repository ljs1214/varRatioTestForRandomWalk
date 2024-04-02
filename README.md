# Function

## `variance_ratio_test`

### Usage
```r
variance_ratio_test(S, q, types)
```

### Arguments
- `S`: Numeric vector, the prices series.
- `q`: Integer, the lag number.
- `types`: String, choose from "homo", "hetero".

### Value
A list containing the z-score and p-value of the variance ratio test.

### Description
Performs the variance ratio test on a given series of log prices to test the random walk hypothesis.

### Examples
```r
prices <- c(100, 101, 102, 103, 102, 101)
variance_ratio_test(prices, 2, 'homo')
```

## `runs_test`

### Examples
```r
prices <- c(100, 101, 102, 103, 102, 101)
runs_test(prices)
```

## `adf_test`

### Examples
```r
prices <- c(100, 101, 102, 103, 102, 101)
adf_test(prices)
```

# output
When p-value > 0.05, show 'Fail to reject the null hypothesis of a unit root. No evidence of stationarity'. Otherwise, show 'Reject the null hypothesis of a unit root. Evidence of stationarity'.
