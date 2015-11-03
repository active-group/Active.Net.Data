namespace Active.Net.Data

module Lazy = 
    let (|Created|_|) (v : Lazy<'T>) =
        if v.IsValueCreated then Some v.Value else None

type Queue<'T> private (front : 'T list, back : 'T list) =
    let list = lazy (front @ List.rev back)

    static let Q(xs, ys) = Queue<'T>(xs,ys)

    static member OfList xs = Q(xs,[])
    static member Empty = Q([],[])

    member __.IsEmpty = front.IsEmpty && back.IsEmpty
    member __.Length = front.Length + back.Length

    member __.Enqueue x =
        match list with
        | Lazy.Created value -> Q(value, [x])
        | _ -> Q(front, x :: back)

    member __.Dequeue() =
        match list with
        | Lazy.Created [] -> failwith "Queue underflow."
        | Lazy.Created (x :: xs) -> x, Q(xs,[])
        | _ ->
            match front, back with
            | [], [] -> failwith "Queue underflow."
            | [], _ -> Q(list.Value, []).Dequeue()
            | x::xs, ys -> x, Q(xs,ys)

    member __.ToList() = list.Value
    override __.ToString () = list.Value.ToString()

/// functional queue ala Okasaki
// copied from http://www.fssnip.net/raw/f7
module Queue =
    let inline (|Q|) (q : Queue<_>) = q
    
    let empty<'T> = Queue<'T>.Empty
    let ofList ts = Queue<_>.OfList ts
    let toList (Q q) = q.ToList()
    let enqueue (Q q) x = q.Enqueue x
    let dequeue (Q q) = q.Dequeue()

