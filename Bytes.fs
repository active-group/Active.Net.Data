namespace Active.Net

module Byte =
    type T = byte
    /// hex string representation of single byte, eg. 'f2'
    let toHexString(b:byte) = sprintf "%02x" b
    /// byte to hex string, eg. 'f2'
    let fromHexString(s:string) = System.Convert.ToByte(s,16)


/// helper functions to manipulate bytes (+ bits)
module Bytes =
    type T = byte []

    /// how many bits are in a byte array
    let sizeInBits (x:T): int = (Array.length x) * 8

    /// extract some bits as an integer
    /// the bit index is from the first byte, the bits within a byte are ordered MSB ... LSB
    /// the bits in the result will be in the same order
    let bits (x: T) (bitIndex: int) (bitCount: int): uint64 =
        assert (bitCount <= 64)
        assert ((bitIndex + bitCount) <= sizeInBits x)
        let rec loop (bitIndex: int) (bitCount: int) (result: uint64): uint64 =
            if bitCount = 0
            then result
            else
            let byteIndex = bitIndex / 8
            let bitWithinByteIndex = 7 - (bitIndex % 8)
            if (x.[byteIndex] &&& (1uy <<< bitWithinByteIndex)) <> 0uy then
                loop (bitIndex + 1) (bitCount - 1) ((result <<< 1) ||| 1UL)
                else
                loop (bitIndex + 1) (bitCount - 1) (result <<< 1)
        loop bitIndex bitCount 0UL

    /// turn byte array to hex digit sequence, eg. "e03fab"
    let toHexString(bytes:T ) =
        let hex = System.BitConverter.ToString(bytes)
        hex.Replace("-","");

    /// turn hex digit sequence to byte array, eg. "e03fab"
    let fromHexString(hex:string) : T =
        [|0..2..hex.Length-1|]
        |> Array.map (fun i -> hex.Substring(i,2) |> Byte.fromHexString)
