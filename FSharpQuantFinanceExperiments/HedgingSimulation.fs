module HedgingSimulation

open Utils
open RandomProcess
open MathNet.Numerics.Distributions

type Portfolio =
    {
        shares : double
        cash   : double
        value  : double
    }

let portfolioValue prtf s =
    prtf.cash + prtf.shares * s

let moneyDvdReturn prtf s r dvd dt =
    prtf.cash * (exp(r*dt) - 1.) + prtf.shares * s * (exp(dvd*dt) - 1.)

let rebalance strike sigma r dvd s T prtf dt=
    let newDelta = delta strike sigma r dvd s T
    let prtfVal = (portfolioValue prtf s) + (moneyDvdReturn prtf s r dvd dt)
    {shares = newDelta;
     cash =  prtfVal - newDelta * s;
     value = prtfVal}

let getPrtfValues prtfArray = 
    [for prtf in prtfArray -> prtf.value]

let blackScholesDeltaHedging s0 strike sigma r dvd mpr T (normal:Normal) nSteps =
    let dt = T / float nSteps
    let path = blackScholesProcess s0 r dvd mpr sigma dt (normal:Normal) nSteps
    let rebalFunc = rebalance strike sigma r dvd

    let rec evolveHedge pth prtf tau =
        seq {
            match tau with
                | negative when negative <= 0.0 -> ()
                | _ ->  
                    let newPrtf = rebalFunc (List.head pth) tau prtf dt
                    yield newPrtf
                    yield! evolveHedge (List.tail pth) newPrtf (tau - dt) 
            }

    let startPrtf = {shares = 0.; cash = 0.; value = 0.}
    let xx = (evolveHedge path startPrtf T)
    xx |> Seq.toArray

    


    

