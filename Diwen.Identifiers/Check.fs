namespace Diwen.Identifiers

    module Check = 

        open Utils    


       
        type ValidationResult = 
            | Success of string option
            | Failure of string

        let bind switchfunction twoTrackInput = 
            match twoTrackInput with
            | Success s -> switchfunction s
            | Failure f -> Failure f 

        let (>>=) twoTrackInput switchFunction = 
            bind switchFunction twoTrackInput

        let validate1 (value:string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                match s.Length with
                | 5 -> Success value
                | _ -> Failure "length not five"

        let validate2 (value:string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                match s.[0] with
                | 'a' -> Success value
                | _ -> Failure "does not start with 'a'"

        let isFoo value = 
            value
            |> validate1
            >>= validate2 

        let isNSINCode (value: string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                if s.Length = 9 && String.forall isUpperCaseLetterOrDigit s then
                    Success value
                else 
                    Failure "incorrect format for NSIN"


        let validateWorksiteKeyLength (value:string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                match s.Length with
                | i when i >= 5 && i<= 35 -> Success value
                | _ -> Failure "length should be between 5 and 35"

        let isAlphanumericUpperCase (value:string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                match String.forall isUpperCaseLetterOrDigit s with
                | true -> Success value
                | _ -> Failure "not all uppercase letters or digits"

        let isWorksiteKey (value: string option) = 
            value 
            |> validateWorksiteKeyLength
            >>= isAlphanumericUpperCase

        let validateLEILength (value: string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                match s.Length with
                | 20 -> Success value
                | _ -> Failure "Length not 20"

        let isLEIReservedCharacters (value: string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                match s.[4..5] with
                | "00" -> Success value
                | _ -> Failure "does not contain zeros in reserved positions"

        let isLEICheckCharacter (value: string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                match (s.[s.Length-2..] = calculateLEICheckCharacter s.[..s.Length-3]) with
                | true -> Success value
                | _ -> Failure "Check character does not match"

        let isLEI (value:string option) = 
            value 
            |> validateLEILength 
            >>= isAlphanumericUpperCase
            >>= isLEIReservedCharacters 
            >>= isLEICheckCharacter

        let isISINLength (value: string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                match s.Length with 
                | 12 -> Success value
                | _ -> Failure "Length is not 12"

        let startsWithCountryCode (value: string option) = 
            match value with
            | None -> Failure "null value"
            | Some s ->
                match isCountryCode s.[0..1] with
                | true -> Success value
                | _ -> Failure "does not start with country code"

        let containsNSINCode (value: string option) = 
            match value with 
            | None -> Failure "null value"
            | Some s -> 
                match isNSINCode (Some s.[2..10]) with
                | Success s -> Success s
                | _ -> Failure "does not contain NSIN code"

        let isISINCheckCharacter (value:string option) = 
           match value with
           | None -> Failure "null value"
           | Some s ->
               match (charToInt s.[s.Length-1] = calculateISINCheckCharacter s.[0..s.Length-2]) with
               | true -> Success value
               | _ -> Failure "check character does not match"

        let isISIN (value: string option) =
            value 
            |> isISINLength
            >>= startsWithCountryCode
            >>= containsNSINCode
            >>= isISINCheckCharacter