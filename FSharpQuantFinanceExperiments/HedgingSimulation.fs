module HedgingSimulation

open Utils
open RandomProcess
open MathNet.Numerics.Distributions

type Portfolio =
    {
        shares : double
        cash   : double
    }

let portfolioValue prtf s =
    prtf.cash + prtf.shares * s

let delta strike sigma r dvd s T =
    let discountedK = strike * exp(-(r - dvd) * T)
    let totalVolatility = sigma * sqrt(T)
    let d_ = log(s / discountedK) / totalVolatility
    let d_plus = d_ + (0.5 * totalVolatility)
    phi(d_plus)

let rebalance strike sigma r dvd s T prtf =
    let newDelta = delta strike sigma r dvd s T
    {shares=newDelta; cash = (portfolioValue prtf s) - newDelta * s}


let blackScholesDeltaHedging s0 strike sigma r dvd T (normal:Normal) nSteps =
    let path = blackScholesProcess s0 r dvd mpr sigma dt (normal:Normal) nSteps


    

