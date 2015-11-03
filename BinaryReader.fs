namespace Active.Net

/// monad to read bytes with System.IO.BinaryReader
// F# builder 'fails' for rec loop in readBytesArrayOfFixedSize
module BinaryReaderMonad =
    open System.IO // BinaryReader
    type 'a M = BinaryReader -> 'a
    let bind (ma:'a M) (f: 'a -> 'b M) : 'b M =
        (fun reader -> let a = ma reader in f a reader)
    let (>>=) = bind
    let result res (reader:BinaryReader) = res // return is a keyword
    let readInt32 (reader:BinaryReader) = reader.ReadInt32()
    let readString (reader:BinaryReader) =  reader.ReadString()
    let readBytes count (reader:BinaryReader) = reader.ReadBytes(count)
    let readBytesArray : byte[] M = readInt32 >>= (fun count -> (readBytes count) >>= result)
    let readBytesArrayOfFixedSize : byte[][] M =
        readInt32 >>= (fun length ->
        if length = 0
        then result Array.empty
        else readInt32 >>= (fun elementSize ->
                let rec loop n res =
                    if n = 0
                    then result (List.rev res |> Array.ofList)
                    else readBytes elementSize >>= (fun bytes -> loop (n-1) (bytes::res))
                loop length []))
