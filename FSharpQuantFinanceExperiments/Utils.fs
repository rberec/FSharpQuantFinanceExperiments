module Utils

open MathNet.Numerics.Distributions

let phi x =
    let normal = new Normal()
    normal.CumulativeDistribution(x)


let priceEuropeanCallAnalytic s0 strike r T sigma =
    let discountedK = strike * exp(-r * T)
    let totalVolatility = sigma * sqrt(T)
    let d_ = log(s0 / discountedK) / totalVolatility
    let d_plus = d_ + (0.5 * totalVolatility)
    let d_minus = d_ - (0.5 * totalVolatility)
    (s0 * phi(d_plus)) - (discountedK * phi(d_minus))

