module HedgingSimulation

open Utils
open RandomProcess
open MathNet.Numerics.Distributions
open MathNet.Numerics.Random
open FSharp.Charting

type Portfolio =
    {
        shares      : double
        option      : double
        cash        : double
        value       : double
    }

let portfolioValue prtf s =
    prtf.cash + prtf.shares * s

let moneyDvdReturn prtf s r dvd dt =
    prtf.cash * (exp(r*dt) - 1.) + prtf.shares * s * (exp(dvd*dt) - 1.)

let initialPortfolio strike sigmaImplied sigmaHedging r dvd s T =
    let newDelta = deltaAnalyticBS strike sigmaHedging r dvd s T
    let optionValue = priceEuropeanCallAnalyticBS strike sigmaImplied r dvd s T
    {shares = newDelta;
     option = optionValue;
     cash = - newDelta * s + optionValue;
     value = 0.}

let rebalance strike sigmaImplied sigmaHedging r dvd s T prtf dt=
    let newDelta = deltaAnalyticBS strike sigmaHedging r dvd s T
    let optionValue = priceEuropeanCallAnalyticBS strike sigmaImplied r dvd s T
    let prtfVal = (portfolioValue prtf s) + (moneyDvdReturn prtf s r dvd dt) - optionValue
    {shares = newDelta;
     option = optionValue;
     cash =  prtfVal - newDelta * s + optionValue
     value = prtfVal}

let getPrtfValues prtfArray = 
    [for prtf in prtfArray -> prtf.value]

let blackScholesDeltaHedging s0 strike sigmaRealized sigmaImplied sigmaHedging r dvd mpr T (normal:Normal) nSteps =
    let dt = T / float nSteps
    let path = blackScholesProcess s0 r dvd mpr sigmaRealized dt normal nSteps
    let rebalFunc = rebalance strike sigmaImplied sigmaHedging r dvd

    let rec evolveHedge pth prtf tau =
        seq {
            match tau with
                | negative when negative <= 0.0 -> ()
                | _ ->  
                    let newPrtf = rebalFunc (Array.head pth) tau prtf dt
                    yield newPrtf
                    yield! evolveHedge (Array.tail pth) newPrtf (tau - dt) 
            }

    let startPrtf = initialPortfolio strike sigmaImplied sigmaHedging r dvd s0 T//{shares = 0.; cash = 0.; value = 0.; option = 0.}
    let xx = (evolveHedge (Array.tail path) startPrtf (T-dt) )
    xx |> Seq.toArray

(*
let blackScholesDeltaHedgingV2 s0 strike sigma r dvd mpr T (normal:Normal) nSteps =
    let dt = T / float nSteps
    let path = blackScholesProcess s0 r dvd mpr sigma dt normal nSteps
    let pathPair = Seq.pairwise (path)
    let rebalFunc = rebalance strike sigma r dvd

    let rec evolveHedge pth prtf tau =
        seq {
            match tau with
                | negative when negative <= 0.0 -> ()
                | _ ->  
                    let s0 = snd (Seq.head pth)
                    let newPrtf = rebalFunc s0 tau prtf dt
                    yield newPrtf
                    yield! evolveHedge (Seq.tail pth) newPrtf (tau - dt) 
            }

    let startPrtf = {shares = 0.; cash = 0.; value = 0.}
    let xx = (evolveHedge pathPair startPrtf T)
    xx |> Seq.toArray
*)

let testBasicDeltaHedging() = 
    let s0 = 100.0
    let strike = 100.0
    let r = 0.02
    let dvd = 0.0
    let mpr = 0.0
    let T = 1.0
    let sigmaRealized = 0.4
    let sigmaImplied = 0.2
    let sigmaHedging = 0.4
    let nPaths = 100
    let nSteps = 10000
    let seed = 931866

    (*
    let path = blackScholesProcess s0 r dvd mpr sigma (T / 10.0) (new Normal(0., 1.)) 10

    let printSeq seq1 = Seq.iter (printf "%A ") seq1; printfn ""
    let seqPairwise = Seq.pairwise (path)
    printSeq seqPairwise
    *)

    let paths = [for i in [1..nPaths] -> getPrtfValues (blackScholesDeltaHedging s0 strike sigmaRealized sigmaImplied sigmaHedging r dvd mpr T (new Normal(0.0, 1.0, new MersenneTwister(seed + i))) nSteps) ]

    paths |> List.map Chart.FastLine |> Chart.Combine |> Chart.Show

    let finalValue = exp(-r*T) * (List.fold (fun (acc:double) (path:double list) -> acc + path.[nSteps-1]) 0.0 paths) / (double nPaths)

    printfn "%f" finalValue
    printfn "%f" (priceEuropeanCallAnalyticBS strike sigmaRealized r dvd s0 T)
    printfn "%f" (priceEuropeanCallAnalyticBS strike sigmaImplied r dvd s0 T)
    


    

