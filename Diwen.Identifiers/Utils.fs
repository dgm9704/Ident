namespace Diwen.Identifiers

    module private Utils =

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

        let characterToDigit (c : char) = 
            if isDigit c then
                string c
            else 
                string ((int c) - 55)

        let convertLettersToDigits (value: string) = 
            String.collect characterToDigit value

        let reverseString (s:string) = 
            new string(Array.rev (s.ToCharArray()))

        let charToInt (c: char) = 
            ((int c) - int('0'))

        let sumDigits (i: int) = 
            i / 10 + i % 10

        let calculateISINWeightedValue (i:int, c: char) = 
            ((i + 1) % 2 + 1) * charToInt c 
            |> sumDigits 
        
        let calculateISINWeightedSum (value:string) = 
            Array.rev (value.ToCharArray())
            |> Array.mapi (fun i  c -> calculateISINWeightedValue(i,c))
            |> Array.sum

        let calculateISINCheckCharacter (value:string) =
            convertLettersToDigits value
            |> calculateISINWeightedSum
            |> (fun sum -> (10 - (sum % 10)) % 10)

        let calculateLEICheckCharacter (value: string) = 
            convertLettersToDigits value + "00"
            |> int
            |> (fun i -> 98 - i % 54)
            |> string 

        //let isISINCheckCharacter (value:string) = 
        //    charToInt value.[value.Length-1] = calculateISINCheckCharacter value.[0..value.Length-2]


        let isWorksiteCheckDigit value = true
        // ISO 7604 Mod 37,36