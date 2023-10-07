open System.Diagnostics

open FSharpObj
[<EntryPoint>]
let main _ =
    let stopwatch = Stopwatch()
    stopwatch.Start()
    let mesh =
        "cornell_box.obj"
        // "stanford-bunny.obj"
        // "xyzrgb_dragon.obj"
        |> Parser.ParseOBJ
        // |> Processor.RecalculateNormal
    printfn $"Mesh with {mesh.vertices.Length} vertices and {mesh.triangles.Length / 3} triangles"
    printfn $"Parsing mesh cost {stopwatch.Elapsed.TotalSeconds} seconds"
    0
