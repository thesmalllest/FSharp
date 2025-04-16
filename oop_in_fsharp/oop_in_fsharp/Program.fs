(*[<AbstractClass>]
type GeometricFigure() =
    abstract member Area: unit -> float
    default this.Area() = 0.0

type IPrint = 
    abstract member Print: unit -> unit

type Rectangle(width: float, height: float) =
    inherit GeometricFigure()
    interface IPrint with
        member this.Print() =
            System.Console.WriteLine(this.ToString())

    member this.Width = width
    member this.Height = height

    override this.Area() = this.Width * this.Height

    override this.ToString() =
        sprintf "Прямоугольник: ширина = %.2f, высота = %.2f, площадь = %.2f" this.Width this.Height (this.Area())

type Square(side: float) =
    inherit Rectangle(side, side)
    interface IPrint with
        member this.Print() =
            System.Console.WriteLine(this.ToString())

    member this.Side = side
    
    override this.ToString() = 
        sprintf "Квадрат: сторона = %.2f, площадь = %.2f" side (this.Area())

type Circle(radius: float) = 
    inherit GeometricFigure()
    interface IPrint with
        member this.Print() =
            System.Console.WriteLine(this.ToString())

    member this.Radius = radius

    override this.Area() = System.Math.PI * this.Radius * this.Radius

    override this.ToString() = 
        sprintf "Круг: радиус = %.2f, площадь = %.2f" radius (this.Area())

[<EntryPoint>]
let main argv = 
    let rectangle = Rectangle(3.0, 6.0)
    let square = Square(4.0)
    let circle = Circle(2.0)

    (rectangle :> IPrint).Print()
    (square :> IPrint).Print()
    (circle :> IPrint).Print()

    0*)

type Figure =
    | Rectangle of width: float * height: float
    | Square of side: float
    | Circle of radius: float

let area figure =
    match figure with
    | Rectangle (width, height) -> width * height
    | Square side -> side * side
    | Circle radius -> System.Math.PI * radius * radius

let rectangle = Rectangle(3.0, 6.0)
let square = Square(4.0)
let circle = Circle(2.0)

printfn "Площадь прямоугольника: %.2f" (area rectangle)
printfn "Площадь квадрата: %.2f" (area square)
printfn "Площадь круга: %.2f" (area circle)

