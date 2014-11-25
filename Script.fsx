#r "./packages/FSharp.Collections.ParallelSeq.1.0.2/lib/net40/FSharp.Collections.ParallelSeq.dll"
open System.IO
open FSharp.Collections.ParallelSeq

type Digit = {Digit: int; Definition : int[] }

let load file = 
    let map (f : string[]) = 
        let mapRow (row : string) = 
            let splited = row.Split(',')
            {Digit = (int splited.[0]); Definition = (splited.[ 1..] |> Array.map ( fun r -> int r)) }
        f |> Array.map ( fun r -> mapRow r)
    (File.ReadAllLines file).[1..] |> map

let kNN trained unknown distance k =
    let predicition = trained 
                      |> Seq.sortBy ( fun n -> distance n.Definition unknown)
                      |> Seq.take k
                      |> Seq.groupBy (fun n -> n.Digit)
                      |> Seq.sortBy (fun n -> let _,s = n
                                              Seq.length s)
                      |> Seq.head
    fst predicition

let validate trainedSet unknownSet distance k = 
    unknownSet 
    |> PSeq.map ( fun n -> (kNN trainedSet n.Definition distance k) = n.Digit)
    |> PSeq.sumBy ( fun n -> (if n = true then 1.0 else 0.0))
    |> fun n ->  (100. * n) / float (Seq.length unknownSet)

let distance = Array.fold2 (fun acc a b -> acc + pown (a - b) 2) 0
let trained = load """D:\Programowanie\FSharpProjekty\Dojo-Digit-Recognizer-Solution\trainingsample.csv"""
let unknown = load """D:\Programowanie\FSharpProjekty\Dojo-Digit-Recognizer-Solution\validationsample.csv"""

#time 
validate trained unknown distance 1
#time 