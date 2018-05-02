// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


open System
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

let callPayoff strike price = 
    max(price - strike) 0.0

let europeanPayoff payoff assetPath =
    assetPath |> Seq.last |> payoff

let europeanCallPayoff strike assetPath =
    assetPath |> europeanPayoff (callPayoff strike)
    
let getAssetPath s0 r dt sigma (normal:Normal) nSteps =
    Seq.unfold (fun s -> let sNew = (s * exp(((r - (0.5 * sigma * sigma)) * dt) + (sigma * sqrt(dt) * normal.Sample())))
                         Some(s, sNew)) s0
    |> Seq.take (nSteps + 1)

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

// Monte Carlo pricing

let priceEuropeanCallMC s0 strike r T sigma nPaths nSteps =
    let normal = new Normal(0.0, 1.0)
    let dt = T / float nSteps
    let payoffs = seq { for n in 1 .. nPaths do 
                        let assetPath = getAssetPath s0 r dt sigma normal nSteps |> Seq.toList
                        yield assetPath |> europeanCallPayoff strike
                      }

    let df = exp(-r * T)
    let priceMC = df * payoffs.Mean()
    let stddevMC = df * payoffs.StandardDeviation() / sqrt(float nPaths)
    (priceMC, stddevMC)

let europeanCallMC s0 strike r T sigma nPaths nSteps =    
    let result = priceEuropeanCallMC s0 strike r T sigma nPaths nSteps
    printfn "European Call (using MC) mean:%f stddev:%f" (fst result) (snd result)

let europeanCallAnalytic s0 strike r T sigma =
    let result = priceEuropeanCallAnalytic s0 strike r T sigma
    printfn "European Call (analytic) %f" result

[<EntryPoint>]
let main argv = 
    let s0 = 100.0
    let strike = 100.0
    let r = 0.02
    let T = 1.0
    let sigma = 0.2
    let nPaths = 10_000_000
    let nSteps = 12

    
    europeanCallMC s0 strike r T sigma nPaths nSteps
    europeanCallAnalytic s0 strike r T sigma
    0 // return an integer exit code
