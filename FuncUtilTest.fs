namespace Active.Net.Test.Data

module FuncUtilTest =
    open Active.Net.TestUtil
    open Active.Net.Seq  // diff3
    open Active.Net.List // lookup*
    open Active.Net.Array // delete
    open NUnit.Framework
    open FsUnit // should, etc.
    open FsCheck

    [<TestFixture>]
    type ListTest() =
    (*
        /// lookup first item in associative list by key
    let lookup key = List.tryFind (fst >> ((=) key))
    /// lookup all items in associative list by key
    let lookupAll key = List.filter (fst >> ((=) key))
    /// lookup item value in associative list by key
    let lookupValue key alist = lookup key alist |> Option.map snd
    /// lookup all item values in associative list by key
    let lookupAllValues key alist = lookupAll key alist |> List.map snd

    *)
        [<Test>]
        member x.``lookup`` () =
            Assert.AreEqual(Some (1,2), lookup 1 (List.zip [1..4] [2..5]))
            Assert.AreEqual(None, lookup 0 (List.zip [1..4] [2..5]))
            Assert.AreEqual(Some (1,5), lookup 1 ((1,5)::(List.zip [1..4] [2..5])))

        [<Test>]
        member x.``lookupAll`` () =
            Assert.AreEqual([(1,2)], lookupAll 1 (List.zip [1..4] [2..5]))
            Assert.AreEqual([], lookupAll 0 (List.zip [1..4] [2..5]))
            Assert.AreEqual([(1,5);(1,2)] |> Set.ofList, (lookupAll 1 ((1,5)::(List.zip [1..4] [2..5]))) |> Set.ofList)

        [<Test>]
        member x.``lookupValue`` () =
            Assert.AreEqual(Some 2, lookupValue 1 (List.zip [1..4] [2..5]))
            Assert.AreEqual(None, lookupValue 0 (List.zip [1..4] [2..5]))
            Assert.AreEqual(Some 5, lookupValue 1 ((1,5)::(List.zip [1..4] [2..5])))

        [<Test>]
        member x.``lookupAllValues`` () =
            Assert.AreEqual([2], lookupAllValues 1 (List.zip [1..4] [2..5]))
            Assert.AreEqual([], lookupAllValues 0 (List.zip [1..4] [2..5]))
            Assert.AreEqual([5;2] |> Set.ofList, (lookupAllValues 1 ((1,5)::(List.zip [1..4] [2..5]))) |> Set.ofList)

        [<Test>]
        member x.``partition3`` () =
            Assert.AreEqual(([],[],[]), partition3 (fun _ -> Choice1Of3(())) [])
            let l1 = [0..10]
            Assert.AreEqual((l1,[],[]), partition3 (fun _ -> Choice1Of3(())) l1)
            Assert.AreEqual(([],l1,[]), partition3 (fun _ -> Choice2Of3(())) l1)
            Assert.AreEqual(([],[],l1), partition3 (fun _ -> Choice3Of3(())) l1)
            Assert.AreEqual(([0;3;6;9],[1;4;7;10],[2;5;8]), partition3 (fun i -> match i % 3 with | 0 -> Choice1Of3(()) | 1 -> Choice2Of3(()) | _ -> Choice3Of3(())) l1)

    [<TestFixture>]
    type SeqTest () =
        [<QuietProperty>]
        member x.``diff3`` (xs:int[]) (ys:int[]) =
            let (onlyX, xAndY, onlyY) = diff3 xs ys
            let xSet     = xs    |> Set.ofSeq
            let ySet     = ys    |> Set.ofSeq
            let onlyXSet = onlyX |> Set.ofSeq
            let xAndYSet = xAndY |> Set.ofSeq
            let onlyYSet = onlyY |> Set.ofSeq
            // all xs,ys are contained
            Assert.AreEqual(xSet, Set.union onlyXSet xAndYSet)
            Assert.AreEqual(ySet, Set.union xAndYSet onlyYSet)
            /// all xs+ys are contained
            Assert.AreEqual(Set.union xSet ySet, Set.unionMany [onlyXSet; xAndYSet; onlyYSet])
            Assert.AreEqual(xSet - xAndYSet, onlyXSet)
            Assert.AreEqual(ySet - xAndYSet, onlyYSet)
            // mutually different
            Assert.AreEqual(Set.empty, Set.intersect onlyXSet xAndYSet)
            Assert.AreEqual(Set.empty, Set.intersect onlyYSet xAndYSet)
            Assert.AreEqual(Set.empty, Set.intersect onlyXSet onlyYSet)
            // every x is in either onlyXSet or xAndYSet, but not in onlyYSet
            for x in xs do
                Assert.True(Set.contains x onlyXSet || Set.contains x xAndYSet)
                Assert.False(Set.contains x onlyYSet)
            // every y is in either onlyYSet or xAndYSet, but not in onlyXSet
            for y in ys do
                Assert.True(Set.contains y onlyYSet || Set.contains y xAndYSet)
                Assert.False(Set.contains y onlyXSet)

        [<Test>]
        member x.``diff3 edge case empty`` () =
            let (onlyX, xAndY, onlyY) = diff3 [] []
            onlyX |> should equal []
            xAndY |> should equal []
            onlyY |> should equal []

        [<Test>]
        member x.``diff3 edge case empty left`` () =
            let (onlyX, xAndY, onlyY) = diff3 [] [1]
            onlyX |> should equal []
            xAndY |> should equal []
            onlyY |> should equal [1]

        [<Test>]
        member x.``diff3 edge case only common`` () =
            let (onlyX, xAndY, onlyY) = diff3 [1] [1]
            onlyX |> should equal []
            xAndY |> should equal [1]
            onlyY |> should equal []

        [<Test>]
        member x.``diff3 edge case empty right`` () =
            let (onlyX, xAndY, onlyY) = diff3 [1] []
            onlyX |> should equal [1]
            xAndY |> should equal []
            onlyY |> should equal []

        [<Test>]
        member x.``diff3 edge case empty onlyY`` () =
            let (onlyX, xAndY, onlyY) = diff3 [1;2] [2]
            onlyX |> should equal [1]
            xAndY |> should equal [2]
            onlyY |> should equal []

        [<Test>]
        member x.``diff3 edge case empty onlyX`` () =
            let (onlyX, xAndY, onlyY) = diff3 [1] [1;2]
            onlyX |> should equal []
            xAndY |> should equal [1]
            onlyY |> should equal [2]

        [<Test>]
        member x.``diff3 edge case empty xAndY`` () =
            let (onlyX, xAndY, onlyY) = diff3 [1] [2]
            onlyX |> should equal [1]
            xAndY |> should equal []
            onlyY |> should equal [2]

    [<TestFixture>]
    type ArrayTest() =
        [<Test>]
        member x.``delete deletes`` () =
            Assert.AreEqual([|0;1;2|], delete 3 [|0;1;2;3|])
            Assert.AreEqual([|0;1;3|], delete 2 [|0;1;2;3|])
            Assert.AreEqual([|0;2;3|], delete 1 [|0;1;2;3|])
            Assert.AreEqual([|1;2;3|], delete 0 [|0;1;2;3|])
            Assert.Throws<System.IndexOutOfRangeException>(fun _ -> delete 0 [||] |> ignore) |> ignore
            Assert.Throws<System.IndexOutOfRangeException>(fun _ -> delete -1 [||] |> ignore) |> ignore
            Assert.Throws<System.IndexOutOfRangeException>(fun _ -> delete 1 [||] |> ignore) |> ignore
            Assert.Throws<System.IndexOutOfRangeException>(fun _ -> delete 1 [|0|] |> ignore) |> ignore
        [<Test>]
        member x.``replaceAt replaces`` () =
            Assert.AreEqual([|99;1;2|], replaceAt 0 99 [|0..2|])
            Assert.AreEqual([|0;99;2|], replaceAt 1 99 [|0..2|])
            Assert.AreEqual([|0;1;99|], replaceAt 2 99 [|0..2|])
            Assert.Throws<System.IndexOutOfRangeException>(fun _ -> replaceAt 0 0 [||] |> ignore) |> ignore
            Assert.Throws<System.IndexOutOfRangeException>(fun _ -> replaceAt -1 0 [||] |> ignore) |> ignore
            Assert.Throws<System.IndexOutOfRangeException>(fun _ -> replaceAt 1 0 [||] |> ignore) |> ignore
            Assert.Throws<System.IndexOutOfRangeException>(fun _ -> replaceAt 1 0 [|0|] |> ignore) |> ignore
        [<Test>]
        member x.``swap is commutative`` () =
            Prop.forAll(Arb.fromGen Arb.generate<NonEmptyArray<int>>) (fun (NonEmptyArray a) ->
            Prop.forAll (Arb.fromGen (Gen.choose(0, a.Length-1))) (fun idx0 ->
            Prop.forAll (Arb.fromGen (Gen.choose(0, a.Length-1))) (fun idx1 ->
                swap idx0 idx1 a = swap idx1 idx0 a)))
            |> AssertProperty

        [<Test>]
        member x.``swap is idempotent if indices are equal`` () =
            Prop.forAll(Arb.fromGen Arb.generate<NonEmptyArray<int>>) (fun (NonEmptyArray a) ->
                Prop.forAll (Arb.fromGen (Gen.choose(0, a.Length-1)))
                    (fun idx -> swap idx idx a = a))
            |> AssertProperty

        [<Test>]
        member x.``swap`` () =
            Assert.AreEqual([|1;0|], swap 0 1 [|0;1|])
            Assert.AreEqual([|1;0|], swap 1 0 [|0;1|])
            Assert.AreEqual([|0;2;1|], swap 1 2 [|0;1;2|])
