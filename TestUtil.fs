namespace Active.Net

/// utilities for testing
module TestUtil =
    open FsCheck
    open NUnit.Framework

    /// FsCheck Property attribute that is quiet on success by default
    [<System.AttributeUsage(System.AttributeTargets.Method, AllowMultiple = false)>]
    type QuietPropertyAttribute() =
        inherit FsCheck.NUnit.PropertyAttribute(QuietOnSuccess=true)

    /// FsCheck generator for non-null strings
    // alternative:
    (* """
        [<QuietProperty>]
        member x.``notnull`` (NonNull (s:string)) =
            Assert.NotNull s
    """*)
    let nonNullStringGenerator =  Arb.generate<NonNull<string>> |> Gen.map (fun (NonNull s) -> s)

    /// provide one random value of desired type using the registered generator;
    /// size is passed to generator; for ints it's the maximum provided abs(i)
    let genOne (size:int) : 'a = Arb.generate<'a> |> Gen.sample size 1 |> List.head
    /// provide n random value of desired type using the registered generator;
    /// size is passed to generator; for ints it's the maximum provided abs(i)
    let genMany (size:int) (n:int) : 'a list = Arb.generate<'a> |> Gen.sample size n
    

    // hooking FsCheck into NUnit
    let NUnitRunner =
        { new IRunner with
            member x.OnStartFixture t = ()
            member x.OnArguments (ntest,args, every) = ()
            member x.OnShrink(args, everyShrink) = ()
            member x.OnFinished(name, result) = 
                match result with 
                | TestResult.True _ -> Assert.Pass ()
                | _ -> Assert.Fail (Runner.onFinishedToString name result) 
        }
  
      /// NUnit config with NUnitRunner as runner
    let nunitConfig = { Config.Default with Runner = NUnitRunner }

    /// Assert FsCheck property
    let AssertProperty p = Check.One ({ nunitConfig with MaxFail = 0 }, p)

    /// Assert FsCheck property with given config
    let AssertPropertyWithConfig c p = Check.One(c, p)

    module Arb =
        /// arbitrary non-null string
        let string =
            let shrink s = Arb.shrink s
            in Arb.fromGenShrink (nonNullStringGenerator, shrink)

        /// arbitrary bytes array
        let bytes = Arb.from<byte[]>

        /// make an arbitrary for pairs from two arbitraries
        let pair (a1: Arbitrary<'a>) (a2: Arbitrary<'b>): Arbitrary<'a * 'b> =
            let generator = gen {
                                let! x1 = a1.Generator
                                let! x2 = a2.Generator
                                return (x1, x2)
                            }
            let shrink (x1, x2) = (seq { for x1' in a1.Shrinker x1 -> (x1', x2) }) 
                                  |> Seq.append (seq { for x2' in a2.Shrinker x2 -> (x1, x2') })
            Arb.fromGenShrink (generator, shrink)

        // helper function for list
        let shrinkList (shr: 'a -> seq<'a>) (xs: list<'a>): seq<list<'a>> =
            let n = List.length xs
            let rec shrinkOne l = match l with
                                  | [] -> Seq.empty
                                  | (x::xs) -> Seq.append (seq { for x' in shr x -> x'::xs })
                                                          (seq { for xs' in shrinkOne xs -> x::xs'})
            let rec removes k n xs =
                        let xs1 = Seq.take k xs |> Seq.toList
                        let xs2 = Seq.skip k xs |> Seq.toList
                        if k > n
                        then Seq.empty
                        elif Seq.isEmpty xs2
                        then Seq.singleton []
                        else Seq.append (Seq.singleton xs2) (Seq.map (List.append xs1) (removes k (n-k) xs2))
            Seq.append (Seq.concat (seq { for k in Seq.unfold (fun n -> if n > 0 then Some (n, n / 2) else None) n -> removes k n xs }))
                       (shrinkOne xs)

        /// make an arbitrary for lists from an arbitrary for the list elements
        let list (a: Arbitrary<'a>): Arbitrary<list<'a>> =
            let generator = Gen.sized (fun n -> gen {
                                                    let! k = Gen.choose (0, n)
                                                    return! Gen.sequence (seq { for _ in 1 .. k -> a.Generator })
                                                })
            let shrink xs = shrinkList a.Shrinker xs
            Arb.fromGenShrink (generator, shrink)

        /// make an arbitrary for sets from an arbitrary for the set elements
        let set (a: Arbitrary<'a>): Arbitrary<Set<'a>> =
            let larb = list a
            let generator = larb.Generator |> Gen.map Set.ofList
            // questionable ...
            let shrink s = Set.toList s |> larb.Shrinker |> Seq.map Set.ofList
            Arb.fromGenShrink (generator, shrink)

        let choiceOf2 (a1: Arbitrary<'a>) (a2: Arbitrary<'b>): Arbitrary<Choice<'a, 'b>> =
            let generator = Gen.oneof [a1.Generator |> Gen.map Choice1Of2; a2.Generator |> Gen.map Choice2Of2]
            let shrink c =
                match c with
                | Choice1Of2 x -> a1.Shrinker x |> Seq.map Choice1Of2
                | Choice2Of2 y -> a2.Shrinker y |> Seq.map Choice2Of2
            Arb.fromGenShrink (generator, shrink)
        
        /// expand arbitrary to include options
        let optional (a: Arbitrary<'a>): Arbitrary<option<'a>> =
            let generator = Gen.frequency [(9, a.Generator |> Gen.map Some);(1, gen {return None})]
            let shrink = function
                | Some x -> None |> Seq.singleton |> Seq.append (a.Shrinker x |> Seq.map Some)
                | None -> Seq.empty              
            Arb.fromGenShrink (generator, shrink)

        let suchThat (p: 'a -> bool) (a: Arbitrary<'a>): Arbitrary<'a> =
            let generator = Gen.suchThat p a.Generator
            let shrink x = a.Shrinker x |> Seq.filter p
            Arb.fromGenShrink (generator, shrink)

        /// turn a generator for <'a> into a generator for <option<'a>>
        let optGen (g: Gen<'a>): Gen<option<'a>> =
            Gen.oneof[Gen.map (Some) g; gen {return None}]

        /// arbitrary that picks one element randomly
        let pickOneOf (els:'a []) =
            let generator = gen {
                    let! idx = Gen.choose(0, els.Length-1)
                    return els.[idx]
                }
            Arb.fromGen generator

        /// resize an arbitrary
        let resize (newSize: int) (a: Arbitrary<'A>) =
            let generator = Gen.resize newSize a.Generator
            let shrinker = a.Shrinker
            Arb.fromGenShrink (generator, shrinker)
