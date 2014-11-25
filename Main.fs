open System
open System.IO
open System.Diagnostics
open FSharp.Collections.ParallelSeq

module Core = 
    type Digit = {Digit: int; Definition : float[] }

    let load file = 
        let map (f : string[]) = 
            let mapRow (row : string) = 
                let splited = row.Split(',')
                let def = splited.[ 1..] |> Array.map ( fun r -> float r)
                {Digit = (int splited.[0]); Definition = def }
            f |> PSeq.map ( fun r -> mapRow r) |> PSeq.toArray
        (File.ReadAllLines file).[1..] |> map

    let kNN (trained : Digit seq) unknown distance k =
        let predicition = trained 
                          |> Seq.map (fun n -> n, distance n.Definition unknown  )
                          |> Seq.sortBy ( fun n -> snd n)
                          |> Seq.take k
                          |> Seq.map (fun n -> fst n)
                          |> Seq.groupBy (fun n -> n.Digit)
                          |> Seq.sortBy (fun n -> let _,s = n
                                                  Seq.length s)
                          |> Seq.head
        fst predicition

    let validate trainedSet unknownSet distance k = 
        unknownSet 
        |> PSeq.mapi ( fun i n -> (kNN trainedSet n.Definition distance k) = n.Digit)
        |> PSeq.sumBy ( fun n -> (if n = true then 1.0 else 0.0))
        |> fun n ->  (100. * n) / float (Seq.length unknownSet)

module Helpers = 
    let len = 28
    
    let contains x = Seq.exists ((=) x)

    let inline cordinates x = 
        x % len, x/len  

    let neighborhood = 
        Array.init (len*len) (fun n -> let x,y = cordinates n
                                       let xs = [x-2 .. x+2] |> Seq.where (fun z -> z > 0 && z < len) 
                                       let ys = [y-2 .. y+2] |> Seq.where (fun z -> z > 0 && z < len)
                                       let xs' = Seq.fold ( fun acc z -> let ys' = Seq.fold (fun bcc u -> u::bcc) List.empty ys 
                                                                         ys'@acc) List.empty xs
                                       xs' |> Array.ofList)

module Distance = 
    let ed aSet bSet = 
        Array.fold2 (fun acc a b -> acc + pown (a - b) 2) 0. aSet bSet
        |> float

    let ncc aSet bSet =         
        let t = Array.fold2(fun acc a b -> acc + a*b) 0. aSet bSet 
        let e = Array.fold(fun acc a -> acc + a*a) 0. aSet 
        let w = Array.fold(fun acc b -> acc + b*b) 0. bSet 
        -t/(sqrt (e*w))

    let zncc aSet bSet = 
        let a' = aSet |> Array.average
        let b' = bSet |> Array.average  
        let t = Array.fold2(fun acc a b -> acc + (a - a')*(b-b')) 0. aSet bSet 
        let e = Array.fold(fun acc a -> acc + (a-a')*(a-a')) 0. aSet
        let w = Array.fold(fun acc b -> acc + (b-b')*(b-b')) 0. bSet
        -t/(sqrt (e*w))

    //Don't try using it
    let imed aSet (bSet : float[]) = 
        let spatialDistance (a, b) = 
            let distance (a,b) = 
                let pd (a,b) (a', b') = (pown (a - a') 2) + (pown (b - b') 2) |> float |> Math.Sqrt
                pd (Helpers.cordinates a)(Helpers.cordinates b)
            exp (-0.5 * pown (distance (a, b)) 2)

        let levelDifference (x,y) = 
            pown (x - y) 2 |> float

        let _,d = Array.fold (fun acc a -> let i, acc' = acc      
                                           let _, res = Helpers.neighborhood.[i] 
                                                       |> Array.map (fun l -> bSet.[l])
                                                       |> Array.fold (fun (j, bcc') b -> j+1, bcc' + spatialDistance (i,j) * levelDifference(a,b) ) (0,0.)                                           
                                           i+1, acc' + res) (0,0.) aSet
        
        (d |> float) / ( 2.0 * System.Math.PI)

let performTest test = 
    let watch = Stopwatch();
    watch.Start()
    let t = test()
    watch.Stop()
    ((watch.ElapsedMilliseconds |> float)/1000.) |> printfn "Time: %f s" 
    t |> printfn "Accuracy: %f"

[<EntryPoint>]
let main args = 
    let trained = Core.load """D:\Programowanie\FSharpProjekty\Dojo-Digit-Recognizer-Solution\trainingsample.csv"""
    let unknown = Core.load """D:\Programowanie\FSharpProjekty\Dojo-Digit-Recognizer-Solution\validationsample.csv"""
    let test = (fun () -> Core.validate trained unknown Distance.ed 1)
    let test2 = (fun () -> Core.validate trained unknown Distance.ncc 1)
    let test3 = (fun () -> Core.validate trained unknown Distance.zncc 1)

    printfn "Computation Started. ED"
    performTest test

    printfn "Computation Started. NCC"
    performTest test2

    printfn "Computation Started. ZNCC"
    performTest test3
    
    Console.Read() |> ignore
    0