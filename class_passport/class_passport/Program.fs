open System
open System.Text.RegularExpressions

type Passport(fullName: string, series: string, number: string, dateOfIssue: string, departmentCode: string) =

    member this.FullName = fullName
    member this.Series = series
    member this.Number = number
    member this.DateOfIssue = dateOfIssue
    member this.DepartmentCode = departmentCode

    member this.IsValidSeries() =
        Regex.IsMatch(series, "^\d{4}$")

    member this.IsValidNumber() =
        Regex.IsMatch(number, "^\d{6}$")

    member this.IsValidDate() =
        Regex.IsMatch(dateOfIssue, @"^\d{2}\.\d{2}\.\d{4}$") 

    member this.IsValidDepartmentCode() =
        Regex.IsMatch(departmentCode, @"^\d{3}-\d{3}$")

    member this.IsValid() =
        let valid = this.IsValidSeries() && this.IsValidNumber() && this.IsValidDate() && this.IsValidDepartmentCode()

        match valid with
            | true -> 
                printfn "Статус: Валиден \n" 
                true
            | false -> 
                printfn "Статус: Не валиден \n" 
                false

    override this.ToString() =
        sprintf "ФИО: %s\nСерия: %s\nНомер: %s\nДата выдачи: %s\nКод подразделения: %s"
            this.FullName this.Series this.Number this.DateOfIssue this.DepartmentCode
     
    override this.Equals(obj) =
        match obj with
        | :? Passport as other -> this.Series = other.Series && this.Number = other.Number
        | _ -> false

    member this.Print() =
            System.Console.WriteLine(this.ToString())

[<EntryPoint>]
let main argv =
    let pass1 = Passport("Иванов Иван Иванович", "1234", "567890", "12.04.2015", "770-001")
    let pass2 = Passport("Петров Пётр Петрович", "1234", "567890", "01.05.2017", "770-002")
    let pass3 = Passport("Некорректный Чел", "12A4", "56789", "31.13.2023", "123456")

    pass1.Print()
    pass1.IsValid()

    pass2.Print()
    pass2.IsValid()

    pass3.Print()
    pass3.IsValid()

    printfn "pass1 == pass2? %b" (pass1.Equals(pass2)) 
    printfn "pass1 == pass3? %b" (pass1.Equals(pass3)) 

    0
