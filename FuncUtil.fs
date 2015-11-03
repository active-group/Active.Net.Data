namespace Active.Net

/// helper fns for functional programming
module FuncUtils =
    open FSharpx.Functional.Prelude
    /// return a function that ignores its argument and returns a
    /// e.g. let ignore = constantly ()
    let constantly a = (fun _ -> a)
    let curry = curry
    let curry3 f = (fun a -> fun b -> fun c -> f (a,b,c))
    let uncurry = uncurry
    let uncurry3 f = (fun (a,b,c) -> f a b c)
    let tuple2 = tuple2
    let tuple3 = tuple3
    let swap = swap
    let flip = flip

    /// protect an operation against System.ObjectDisposed
    let ifNotDisposed (f: unit -> 'A): option<'A> =
        try
            Some (f ())
        with
        | :? System.ObjectDisposedException -> None

module Validation =
    let success x = Choice1Of2 x
    let failure x = Choice2Of2 x
    let isSuccess = function
    | Choice1Of2 _ -> true
    | Choice2Of2 _ -> false
    let isFailure = function
    | Choice1Of2 _ -> false
    | Choice2Of2 _ -> true

module List =
    /// lookup first item in associative list by key
    let lookup key = List.tryFind (fst >> ((=) key))
    /// lookup all items in associative list by key
    let lookupAll key = List.filter (fst >> ((=) key))
    /// lookup item value in associative list by key
    let lookupValue key alist = lookup key alist |> Option.map snd
    /// lookup all item values in associative list by key
    let lookupAllValues key alist = lookupAll key alist |> List.map snd
    /// partition into 3 slots
    let partition3 (label:'T -> Choice<unit,unit,unit>) (l:'T list) : ('T list * 'T list * 'T list) =
        l
        |> List.fold (fun (a,b,c) i ->
            match label i with
            | Choice1Of3 _ -> (i::a,b,c)
            | Choice2Of3 _ -> (a,i::b,c)
            | Choice3Of3 _ -> (a,b,i::c))
            ([],[],[])
        |> (fun (a,b,c) -> (List.rev a, List.rev b, List.rev c))


module Seq =
    /// determine which places are (a) only in old, (b) both in old and njuh, (c) only in njuh ('new' is a keyword)
    let diff3 (old:'a seq) (njuh:'a seq) : ('a seq * 'a seq * 'a seq) =
        let old = old |> Set.ofSeq
        let njuh = njuh |> Set.ofSeq

        let removed = Set.difference old njuh |> Set.toSeq
        let common = Set.intersect old njuh   |> Set.toSeq
        let added = Set.difference njuh old   |> Set.toSeq
        (removed, common, added)

module Tuple =
    let fstOf3 (a,b,c) = a
    let sndOf3 (a,b,c) = b
    let thirdOf3 (a,b,c) = c

module Array =
    /// return new array that is missing element at index idx
    /// throws IndexOutOfRangeException if not (0 <= idx < length)
    let delete idx (a:'a []) =
        let origSize = a.Length
        if idx < 0 || idx >= origSize then raise (System.IndexOutOfRangeException())
        let newSize = origSize - 1
        Array.init newSize (fun i -> if i < idx then a.[i] else a.[i+1])
    /// replace element at index idx with a, returning newly allocated array
    /// throws IndexOutOfRangeException if not (0 <= idx < length)
    let replaceAt idx (a:'a) (arr:'a[]) =
        let origSize = arr.Length
        if idx < 0 || idx >= origSize then raise (System.IndexOutOfRangeException())
        Array.init origSize (function
            | i when i = idx -> a
            | i -> arr.[i])
    let swap idx0 idx1 (arr: 'a[]) =
        Array.init arr.Length
            (function
            | idx when idx = idx0 -> arr.[idx1]
            | idx when idx = idx1-> arr.[idx0]
            | idx -> arr.[idx])

module Map =
    let keys m = Map.toSeq m |> Seq.map (fun (k,v) -> k)
    let values m = Map.toSeq m |> Seq.map (fun (k,v) -> v)
