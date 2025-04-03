﻿open System

// task 3
let circleArea radius : float =
    let pi = 3.14159
    pi * radius * radius

let cylinderVolume radius height : float =
    let area = circleArea radius
    height * area

// task 4
let rec sumDigitsUp number : int = 
    if number = 0 then 0
    else 
        (number % 10) + sumDigitsUp(number / 10)

// task 5
let sumDigitsDown n = 
    let rec sum n curSum = 
        if n = 0 then curSum
        else
            let n1 = n / 10
            let digit = n % 10
            let result = curSum + digit
            sum n1 result 
    sum n 0

// Количество делителей Вверх
let rec countDevisiorsUp x index : int =
    match index with
    | index when (x % index = 0 && index < x) -> 1 + countDevisiorsUp x (index + 1)
    | index when (index >= x) -> 0
    | _ -> 0 + countDevisiorsUp x (index + 1)

// Количество делителей Вниз
let countDevisiorsDown x = 
    let rec countDevisiorsDown x index sum =
        let isNeed = (x % index = 0) && (index < x)
        let new_sum = sum + 1
        match isNeed with
            | true -> countDevisiorsDown x (index + 1) (new_sum)
            | false when index < x -> countDevisiorsDown x (index + 1) sum
            | _ -> sum
    countDevisiorsDown x 1 0

// task 6
let factorial n =
    let rec mul n acc =
        match n with
            | 0 | 1 -> acc
            | _ -> mul(n-1) (acc*n)
    mul n 1

let isTrue (b:bool) =
    match b with
        | true -> factorial 5
        | false -> sumDigitsDown 123

// task 7-8
let rec reduce (n:int) (func : int -> int -> int) (acc:int) =
    match n with
        | 0 -> acc
        | _ ->
            let digit = (n%10)
            let newAcc = func acc digit
            let curDigit = (n/10)
            reduce curDigit func newAcc

// task 9-10
let rec filterReduce (n:int) (func : int -> int -> int) (acc:int) (condition : int -> bool) =
    match n with
        | 0 -> acc
        | _ ->
            let digit = n%10
            let newacc =    
                match condition digit with
                    | true -> func acc digit
                    | false -> acc
            let curDigit = n / 10
            filterReduce curDigit func newacc condition



[<EntryPoint>]
let main argv =
(*    //Объем цилиндра
    Console.WriteLine("Введите радиус цилиндра:")
    let radius = Console.ReadLine() |> float
    Console.WriteLine("Введите высоту цилиндра:")
    let height = Console.ReadLine() |> float

        //Суперпозиция
    let countVolume (r, h) = cylinderVolume r h
    let printVolume v = Console.WriteLine($"Объём цилиндра: {v}")
    let volume = countVolume >> printVolume
    volume (radius, height)

        //Каррирование
    let volumeFunction = cylinderVolume radius
    let result = volumeFunction height
    Console.WriteLine($"Объём цилиндра: {result}")

    //Сумма цифр
    Console.WriteLine("Введите число, чтобы посчитать сумму его цифр:")
    let input_for_sum = Console.ReadLine()
    let number_for_sum = int input_for_sum  

    let sum_1 = sumDigitsUp number_for_sum
    Console.WriteLine($"Сумма цифр {number_for_sum} равна {sum_1}")

    let sum_2 = sumDigitsDown number_for_sum
    Console.WriteLine($"Сумма цифр {number_for_sum} равна {sum_2}")


    Console.WriteLine("Введите число, чтобы посчитать количество делителей:")
    let input_for_count = Console.ReadLine()
    let number_for_count = int input_for_count

    let count_1 = countDevisiorsUp number_for_count 1
    Console.WriteLine($"Количество делителей {number_for_count}: {count_1}")

    let count_2 = countDevisiorsDown number_for_count
    Console.WriteLine($"Количество делителей {number_for_count}: {count_2}")
*)  
(*    // 6
    Console.WriteLine(isTrue false)
    
    // 7-8
    let testReduce () =
        Console.WriteLine(reduce 1234 (fun acc digit -> acc + digit) 0)
        Console.WriteLine(reduce 1234 (fun acc digit -> acc * digit) 1)
        Console.WriteLine(reduce 1234 (fun acc digit -> if digit < acc then digit else acc) 10)
        Console.WriteLine(reduce 1234 (fun acc digit -> if digit > acc then digit else acc) 0)

    testReduce()*)

    // 9-10
    let filterReduceTest () = 
        Console.WriteLine(filterReduce 12345 (fun acc digit -> acc + digit) 0 (fun digit -> digit % 2 = 0))
        Console.WriteLine(filterReduce 12345 (fun acc digit -> acc * digit) 1 (fun digit -> digit <> 1))
        Console.WriteLine(filterReduce 12345 (fun acc digit -> acc + 1) 0 (fun digit -> digit > 3))
        Console.WriteLine(filterReduce 12345 (fun acc digit -> if digit < acc then digit else acc) 10 (fun digit -> true))
    filterReduceTest()

    0