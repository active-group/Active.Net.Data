namespace Active.Net.Test.Data

module BinaryReaderTest =
    open Active.Net.BinaryReaderMonad
    open System.IO // BinaryReader
    open FsCheck
    open FsCheck.NUnit
    open FSharp.Data
    open Active.Net.TestUtil
    open FsUnit
    open System

    let encoding = new System.Text.UTF8Encoding()
    let read (buf:byte[]) reader =
        let mem = new MemoryStream(buf)
        new BinaryReader(mem, encoding)
        |> reader

    [<QuietProperty>]
    let ``readInt32`` (i:int32) =
        let buf = BitConverter.GetBytes(i)
        (read buf) readInt32 |> should equal i

    [<QuietProperty>]
    let ``readString`` (s:string) =
        s <> null ==> lazy
                        let mem = new MemoryStream()
                        use writer = new BinaryWriter(mem, encoding)
                        writer.Write(s)
                        let buf = mem.ToArray()
                        (read buf) readString |> should equal s

    [<QuietProperty>]
    let ``readBytes`` (buf:byte[]) =
        (read buf) (readBytes buf.Length) |> should equal buf

    [<QuietProperty>]
    let ``readByteArray`` (bs:byte[]) =
        let lengthBytes = BitConverter.GetBytes(bs.Length)
        let buf = Array.append lengthBytes bs
        (read buf) readBytesArray |> should equal bs

    [<QuietProperty>]
    let ``readBytesArrayOfFixedSize`` (count:int) (bs:byte[]) =
        count >= 0 ==> lazy
                       (let bbs = Array.create count bs
                        let buf = 
                            Array.concat (seq {
                                yield BitConverter.GetBytes(count)
                                if count <> 0
                                then yield BitConverter.GetBytes(bs.Length)
                                     for i in 1..count do
                                        yield bs
                                else yield Array.empty
                            })
                        // should equal cannot handle empty arrays (falsely fails if bs = [||])
                        Assert.AreEqual((read buf) readBytesArrayOfFixedSize, bbs))
