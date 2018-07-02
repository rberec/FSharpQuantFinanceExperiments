open System
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open MathNet.Numerics.Random
open Utils
open RandomProcess
open HedgingSimulation

let callPayoff strike price = 
    max(price - strike) 0.0


let europeanPayoff payoff assetPath =
    assetPath |> Seq.last |> payoff


let europeanCallPayoff strike assetPath =
    assetPath |> europeanPayoff (callPayoff strike)


let upOutPayoff payoff barier assetPath = 
    if assetPath |> Seq.exists (fun s -> s >= barier) then 0.0 else payoff assetPath


let upOutEuropeanCallPayoff strike barier =
    upOutPayoff (europeanCallPayoff strike) barier 


// Monte Carlo pricing

let priceOptionMC option randomProcess r T (seed:int) nSteps nPaths =
    if (T < 0.0) then invalidArg "T" (sprintf "Negative expiry %f." T)
    if (nSteps < 0) then invalidArg "nSteps" (sprintf "Negative nSteps %d." nSteps)
    if (nPaths < 0) then invalidArg "nPaths" (sprintf "Negative nPaths %d." nPaths)

    let randomSource = new MersenneTwister(seed)
    let normal = new Normal(0.0, 1.0, randomSource)
    let dt = T / float nSteps
    let payoffs = seq<double> { for n in 1 .. nPaths do 
                                let assetPath = randomProcess dt normal nSteps
                                yield assetPath |> option
                              }

    let df = exp(-r * T)
    let priceMC = df * payoffs.Mean()
    let stddevMC = df * payoffs.StandardDeviation() / sqrt(float nPaths)
    (priceMC, stddevMC)


let priceEuropeanCallMC randomProcess strike r T (seed:int) nSteps nPaths =
    let option = europeanCallPayoff strike
    priceOptionMC option randomProcess r T seed nSteps nPaths


let priceUpOutEuropeanCallMC randomProcess strike barier r T (seed:int) nSteps nPaths = 
    let option = upOutEuropeanCallPayoff strike barier
    priceOptionMC option randomProcess r T seed nSteps nPaths
  

let runAndPrintResultMC pricer nPaths =
    let result = pricer nPaths
    printfn "Option price  mean:%f stddev:%f (using MC, n = %i)" (fst result) (snd result) (nPaths)


let europeanCallAnalytic s0 strike r dvd T sigma =
    let result = priceEuropeanCallAnalyticBS strike sigma r dvd s0 T
    printfn "European Call (analytic) %f" result


[<EntryPoint>]
let main argv = 
    let s0 = 100.0
    let strike = 100.0
    let r = 0.02
    let dvd = 0.0
    let mpr = 0.0
    let T = 1.0
    let sigma = 0.2
    let nPaths = [| 10; 100; 1_000; 10_000; 100_000; 1_000_000 |]
    let nSteps = 12
    let seed = 9318669

    testBasicDeltaHedging()
    

    let randomProcess = blackScholesProcess s0 r dvd mpr sigma

    printfn "Test of European Call Option:"
    let europeanCallTest = priceEuropeanCallMC randomProcess strike r T seed nSteps
    nPaths |> Array.map (runAndPrintResultMC europeanCallTest) |> ignore
    europeanCallAnalytic s0 strike r dvd T sigma

    printfn "Test of European Up and Out Call Option:"
    let barier = 120.0
    let europeanUpOutCallTest = priceUpOutEuropeanCallMC randomProcess strike barier r T seed nSteps
    nPaths |> Array.map (runAndPrintResultMC europeanUpOutCallTest) |> ignore

    0 // return an integer exit code
