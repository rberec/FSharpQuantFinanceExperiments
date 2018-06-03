module Utils

open MathNet.Numerics.Distributions

let phi x =
    let normal = new Normal()
    normal.CumulativeDistribution(x)


let priceEuropeanCallAnalyticBS strike sigma r dvd s T =
    let df = exp(-r * T)
    let forward = s * exp(- dvd * T) / df
    let totalVolatility = sigma * sqrt(T)
    let d_ = log(forward / strike) / totalVolatility
    let d_plus = d_ + (0.5 * totalVolatility)
    let d_minus = d_ - (0.5 * totalVolatility)
    df * (forward * phi(d_plus) - strike * phi(d_minus))

let deltaAnalyticBS strike sigma r dvd s T =
    let forward = s * exp((r - dvd) * T)
    let totalVolatility = sigma * sqrt(T)
    let d_ = log(forward / strike) / totalVolatility
    let d_plus = d_ + (0.5 * totalVolatility)
    exp(-dvd * T) * phi(d_plus)

let testFormulas =
    let s0 = 100.0
    let strike = 100.0
    let r = 0.02
    let dvd = 0.0
    let T = 1.0
    let sigma = 0.2

    // http://www.math.drexel.edu/~pg/fin/VanillaCalculator.html
    // 6.3300691088
    let piceTest = priceEuropeanCallAnalyticBS strike sigma r dvd s0 T

    // 0.4566482686
    let deltaTest = deltaAnalyticBS strike sigma r 0.05 s0 T
    1

    

