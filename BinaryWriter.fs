namespace Active.Net


/// fns to write binary, almost a monad
module BinaryWriterUtil =
    open System.IO // BinaryWriter
    open FSharpx.Functional.Prelude // flip

    /// write int
    let writeInt32 (i:int32) (writer:BinaryWriter) =
        writer.Write(i)
        writer
    /// write plain bytes
    let writeBytes (bytes:byte[]) (writer:BinaryWriter) =
        writer.Write(bytes)
        writer
    /// write bytes array, prefixed by length
    let writeBytesArray (bytes:byte[]) (writer:BinaryWriter) =
        writer
        |> writeInt32 bytes.Length
        |> writeBytes bytes
    /// write (utf8-)encoded string, prefixed by length
    let writeString (s:string) (writer:BinaryWriter) =
        writer.Write(s)
        writer
    /// write array of byte[], where each element is of the same size
    let writeBytesArrayOfFixedSize (bytesArray:byte[][]) (writer:BinaryWriter) =
        let length = bytesArray.Length
        let elementSize = if bytesArray.Length = 0 then 0 else bytesArray.[0].Length
        writer
        |> writeInt32 length
        |> if length = 0
            then id
            else writeInt32 elementSize
                >> (fun writer -> bytesArray |> Array.fold (flip writeBytes) writer)

