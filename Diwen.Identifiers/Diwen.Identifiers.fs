namespace Diwen.Identifiers

module Check = 

    let upperCaseLetters = ['A'..'Z']

    let digits = ['0'..'9']

    let isUpperCaseLetter (c: char) =
        List.contains c upperCaseLetters

    let isDigit (c: char) =
        List.contains c digits 

    let isCountryCode (s: string) = 
        s.Length = 2 
        && String.forall isUpperCaseLetter s 

    let isUpperCaseLetterOrDigit (c: char) =  
        isDigit c 
        || isUpperCaseLetter c

    let isNSINCode (value: string) = 
        value.Length = 9 
        && String.forall isUpperCaseLetterOrDigit value

    let characterToDigit (c : char) = 
        if isDigit c then
            string c
        else 
            string ((int c) - 55)

    let convertLettersToDigits (value: string) = 
        String.collect characterToDigit value

    let reverseString (s:string) = 
        new string(Array.rev (s.ToCharArray()))

    let calculateWeightedValue (i:int, c: char) = 
        let d = (int c) - int('0')
        if i % 2 = 1 then
            d
        else 
            let value  =  2 * d
            if value > 9 then 
                value - 9
            else
                value         

    let calculateWeightedSum (value: string) = 
        Array.rev (value.ToCharArray())
        |> Array.mapi (fun i  c -> calculateWeightedValue(i,c))
        |> Array.sum

    let calculateISINCheckCharacter (value: string) =
        convertLettersToDigits value
        |> calculateWeightedSum
        |> (fun sum -> (10 - (sum % 10)) % 10)

    let isISINCheckCharacter (value: string) = 
        int value.[value.Length-1] - (int '0') = calculateISINCheckCharacter value.[0..value.Length-2]

    let isISINCode (value: string) =
        value.Length = 12
        && isCountryCode value.[0..1]
        && isNSINCode value.[2..10]
        && isDigit value.[11]
        && isISINCheckCharacter value  
    
