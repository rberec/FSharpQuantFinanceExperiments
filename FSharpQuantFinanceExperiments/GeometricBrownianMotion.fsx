#I __SOURCE_DIRECTORY__
#r @"../packages/MathNet.Numerics.4.4.0/lib/net461/MathNet.Numerics.dll"
#r @"../packages/MathNet.Numerics.FSharp.4.4.0/lib/net45/MathNet.Numerics.FSharp.dll"

open System
open MathNet.Numerics.Distributions

module GeometricBrownianMotion =
    // Generate price using geometric Brownian motion
    let randomPrice drift volatility dt initial (dist:Normal) = 
        // Calculate parameters of the exponential
        let driftExp = (drift - 0.5 * pown volatility 2) * dt
        let randExp = volatility * (sqrt dt)

        // Recursive loop that actually generates the price
        let rec loop price = seq {
            yield price
            let price = price * exp (driftExp + randExp * dist.Sample()) 
            yield! loop price }

        // Return path starting at 'initial'
        loop initial

    let dist = Normal(0.0, 1.0, RandomSource = Random(100))
    let path = randomPrice 0.05 0.05 0.005 5.0 dist, 500