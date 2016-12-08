namespace Diwen.Identifiers.Test
open System
open NUnit.Framework
open Diwen.Identifiers
open FsUnit
open Check

[<TestFixture>]
module CheckTest = 
   
    let shouldFail check = 
        match check with
        | Failure s -> Assert.Pass s
        | Success r -> 
            match r with 
            | Some s -> Assert.Fail (sprintf "Success returned for incorrect value %s" s)
            | None -> Assert.Fail ("Success returned for null value?")

    let shouldPass check = 
        match check with
        | Failure s -> Assert.Fail (sprintf "Failure: %s" s) 
        | Success r -> 
            match r with
            | Some s -> Assert.Pass s 
            | None -> Assert.Pass "passed with null value"
    
    [<Test>]
    let ``Correct WorksiteKey returns Success``() = 
        isWorksiteKey (Some "12345") |> shouldPass

    [<Test>]
    let ``Incorrect WorksiteKey returns Failure``() = 
        isWorksiteKey (Some "1234a") |> shouldFail

    [<Test>]
    let ``Correct LEI Code returns Success``() = 
        isLEI (Some "00000000000000000098") |> shouldPass
    
    [<Test>]
    let ``Incorrect LEI Code returns Failure``() = 
        isLEI (Some "00000000000000000097") |> shouldFail

    [<Test>]
    let ``Correct ISIN Code returns Success``() = 
        isISIN (Some "FI0000000003") |> shouldPass

    [<Test>]
    let ``Incorrect ISIN Code returns Failure``() = 
        isISIN (Some "012345678901") |> shouldFail

    [<Test>]
    let ``Another Incorrect ISIN Code returns Failure``() = 
        isISIN None |> shouldFail

    [<Test>]
    let ``Passing a too short value does not cause errors``() = 
        isLEIReservedCharacters (Some "12") |> shouldFail // but not throw
