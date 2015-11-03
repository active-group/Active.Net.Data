namespace Active.Net

module Hash =
    open System.Security.Cryptography
    open Nessos.FsPickler
    open Nessos.FsPickler.Combinators

    type Algorithm = HashAlgorithm
    /// fn that returns _a new_ HashAlgorithm instance upon call (for thread-safety)
    // can't just have let crypto = Hash.sha256 (), as HashAlgorithm has state and is not thread-safe
    type NewAlgorithm = unit -> Algorithm

    /// type alias for hashes
    type Hash = Hash of byte[]
    type T = Hash

    /// make hash from byte array, no computation
    let make (x: byte[]): Hash = Hash x
    /// make hash from byte array, no computation (alias to make)
    let fromByteArray = make

    /// compute hash of byte array
    let compute (crypto:NewAlgorithm) (x:byte[]) = crypto().ComputeHash x |> make

    /// convert a hash to a byte array
    let toByteArray (Hash x: Hash): byte[] = x

    /// return the bytes of a hash as a list
    let bytes (h: Hash): list<byte> =
        Array.toList (toByteArray h)

    /// how many bits are in a hash
    let sizeInBits (Hash x: Hash): int = Bytes.sizeInBits x

    /// how many bytes are in a hash
    let sizeInBytes (Hash x: Hash): int = Array.length x

    /// extract some bits as an integer
    /// the bit index is from the first byte, the bits within a byte are ordered MSB ... LSB
    /// the bits in the result will be in the same order
    let bits (Hash x: Hash) (bitIndex: int) (bitCount: int): uint64 = Bytes.bits x bitIndex bitCount

    /// compare two hashes lexicographically (ascending)
    let compare (Hash x1: Hash) (Hash x2: Hash): int =
        Seq.compareWith (fun (i1: byte) (i2: byte) -> i1.CompareTo(i2)) x1 x2

    /// hash sequence of byte[] as if it were a single byte[]
    let computeSeqHash (newAlgorithm:NewAlgorithm) (bytes:byte[] seq) =
        let hash = newAlgorithm()
        assert hash.CanTransformMultipleBlocks
        bytes
        |> Seq.iter (fun bytes -> hash.TransformBlock(bytes, 0, bytes.Length, bytes, 0) |> ignore)
        hash.TransformFinalBlock([||], 0,0)|>ignore
        hash.Hash |> make

    /// SHA256
    let sha256() = new System.Security.Cryptography.SHA256Managed() :> Algorithm // don't know why cast is needed

    /// FsPickler pickler for hashes
    let pickler: Pickler<Hash> =
        Pickler.wrap fromByteArray toByteArray Pickler.bytes

    module Testing =
        open FsCheck

        let generator sizeInBytes = gen {
            let! bytes = Arb.generate<byte> |> Gen.arrayOfLength sizeInBytes
            return fromByteArray bytes
        }