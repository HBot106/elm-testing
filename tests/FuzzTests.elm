module FuzzTests exposing (..)

import Array exposing (..)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (..)
import Random exposing (maxInt, minInt)
import Test exposing (..)


addOneTests : Test
addOneTests =
    describe "addOne"
        [ fuzzWith { runs = 200 } (intRange minInt maxInt) "adds 1 to the given integer" <|
            \num ->
                addOne num |> Expect.equal (num + 1)
        ]



-- addTests : Test
-- addTests =
--     describe "add"
--         [ fuzz2 int int "adds two given integers" <|
--             \num1 num2 ->
--                 add num1 num2
--                     |> Expect.equal (num1 + num2)
--         ]


addOne : Int -> Int
addOne x =
    1 + x


add : Int -> Int -> Int
add x y =
    x + y


flip : Bool -> Bool
flip x =
    not x


flipTests : Test
flipTests =
    describe "flip"
        [ fuzz bool "negates the given boolean value" <|
            \value ->
                flip value |> Expect.equal (not value)
        ]


multiplyFloat : Float -> Int -> Float
multiplyFloat x y =
    x * toFloat y


multiplyFloatTests : Test
multiplyFloatTests =
    describe "multiplyFloat"
        [ fuzz2 float int "multiplies given numbers" <|
            \x y ->
                multiplyFloat x y
                    |> Expect.within (Absolute 0.000000001) (x * toFloat y)
        ]


pizzaLeft : Float -> Float -> Float
pizzaLeft eatenPercent totalSlices =
    totalSlices - (eatenPercent * totalSlices)


pizzaLeftTests : Test
pizzaLeftTests =
    describe "pizzaLeft"
        [ fuzz2 percentage float "returns remaining pizza slices" <|
            \eaten total ->
                pizzaLeft eaten total
                    |> Expect.within (Absolute 0.000000001) (total - (eaten * total))
        ]


stringTests : Test
stringTests =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                -- Unit Test - 1
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    palindrome
                        |> String.reverse
                        |> Expect.equal palindrome
            , test "reverses a known string" <|
                -- Unit Test - 2
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            , fuzz string "restores the original string if you run it again" <|
                -- Fuzz Test
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]


listLengthTests : Test
listLengthTests =
    describe "List.length"
        [ fuzz (list int) "never returns a negative value" <|
            \intList ->
                intList
                    |> List.length
                    |> Expect.atLeast 0
        ]


arrayGetTests : Test
arrayGetTests =
    describe "Array.get"
        [ fuzz (array (intRange -20 20)) "returns Nothing for out of range index" <|
            \intArray ->
                let
                    length =
                        Array.length intArray
                in
                intArray
                    |> Array.get length
                    |> Expect.equal Nothing
        ]


addTests : Test
addTests =
    describe "add"
        [ fuzz (tuple ( int, int )) "adds two given integers" <|
            \( num1, num2 ) ->
                add num1 num2
                    |> Expect.equal (num1 + num2)
        ]


listReverseTests : Test
listReverseTests =
    describe "List.reverse"
        [ fuzz (tuple ( list int, int )) "doesn't remove a member of the list" <|
            \( intList, num ) ->
                intList
                    |> List.reverse
                    |> List.member num
                    |> Expect.equal (List.member num intList)
        ]
