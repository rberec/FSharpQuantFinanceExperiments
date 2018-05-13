module RandomProcess

open MathNet.Numerics.Distributions

let blackScholesProcess s0 r sigma dt (normal:Normal) nSteps =
    Seq.unfold (fun s -> let sNew = (s * exp(((r - (0.5 * sigma * sigma)) * dt) + (sigma * sqrt(dt) * normal.Sample())))
                         Some(s, sNew)) s0
    |> Seq.take (nSteps + 1)
    |> Seq.toList
