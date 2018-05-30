module Utils

open MathNet.Numerics.Distributions

let phi x =
    let normal = new Normal()
    normal.CumulativeDistribution(x)


let priceEuropeanCallAnalytic s0 strike r dvd T sigma =
    let df = exp(-r * T)
    let forward = s0 * exp(- dvd * T) / df
    let totalVolatility = sigma * sqrt(T)
    let d_ = log(forward / strike) / totalVolatility
    let d_plus = d_ + (0.5 * totalVolatility)
    let d_minus = d_ - (0.5 * totalVolatility)
    df * (forward * phi(d_plus)) - (strike * phi(d_minus))

let delta strike sigma r dvd s T =
    let forward = s * exp((r - dvd) * T)
    let totalVolatility = sigma * sqrt(T)
    let d_ = log(forward / strike) / totalVolatility
    let d_plus = d_ + (0.5 * totalVolatility)
    exp(-dvd * T) * phi(d_plus)

