module RippleCarryAdder exposing (..)

import Array
import Bitwise


notGate a =
    case a of
        0 ->
            1

        1 ->
            0

        _ ->
            -1


andGate a b =
    Bitwise.and a b


orGate a b =
    Bitwise.or a b


xorGate a b =
    Bitwise.xor a b


halfAdder a b =
    let
        sumDigit =
            andGate (orGate a b) (xorGate a b)

        carryOut =
            andGate a b
    in
    { carry = carryOut
    , sum = sumDigit
    }


fullAdder a b carryIn =
    let
        firstResult =
            halfAdder b carryIn

        secondResult =
            halfAdder a firstResult.sum

        finalCarry =
            orGate firstResult.carry secondResult.carry
    in
    { carry = finalCarry
    , sum = secondResult.sum
    }


type alias Binary =
    { d0 : Int
    , d1 : Int
    , d2 : Int
    , d3 : Int
    }


rippleCarryAdder a b carryIn =
    let
        -- Extract digits
        firstSignal =
            extractDigits a

        secondSignal =
            extractDigits b

        -- Compute sum and carry-out
        firstResult =
            fullAdder firstSignal.d3 secondSignal.d3 carryIn

        secondResult =
            fullAdder firstSignal.d2 secondSignal.d2 firstResult.carry

        thirdResult =
            fullAdder firstSignal.d1 secondSignal.d1 secondResult.carry

        finalResult =
            fullAdder firstSignal.d0 secondSignal.d0 thirdResult.carry
    in
    [ finalResult, thirdResult, secondResult, firstResult ]
        |> List.map .sum
        |> (::) finalResult.carry
        |> numberFromDigits


extractDigits number =
    digits number
        |> padZeros 4
        |> Array.fromList
        |> arrayToRecord


numberFromDigits digitsList =
    List.foldl (\digit number -> digit + 10 * number) 0 digitsList


arrayToRecord array =
    let
        firstElement =
            Array.get 0 array
                |> Maybe.withDefault -1

        secondElement =
            Array.get 1 array
                |> Maybe.withDefault -1

        thirdElement =
            Array.get 2 array
                |> Maybe.withDefault -1

        fourthElement =
            Array.get 3 array
                |> Maybe.withDefault -1
    in
    { d0 = firstElement
    , d1 = secondElement
    , d2 = thirdElement
    , d3 = fourthElement
    }


stringToInt string =
    String.toInt string
        |> Maybe.withDefault -1


digits number =
    let
        getDigits n =
            if n == 0 then
                []

            else
                remainderBy 10 n :: getDigits (n // 10)
    in
    getDigits number
        |> List.reverse


padZeros total list =
    let
        numberOfZeros =
            total - List.length list
    in
    List.repeat numberOfZeros 0 ++ list
