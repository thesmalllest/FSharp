type Maybe<'T> =
    | Just of 'T
    | Nothing

// Функтор (fmap)
let fmap f m =
    match m with
    | Just x -> Just (f x)
    | Nothing -> Nothing

// Аппликатив (ap)
let ap mf mx =
    match mf, mx with
    | Just f, Just x -> Just (f x)
    | _ -> Nothing

// Монада (bind)
let bind m f =
    match m with
    | Just x -> f x
    | Nothing -> Nothing

let id x = x
let f = (+) 2
let g = (*) 3

let m = Just 5
let nothing = Nothing

// 🔹 Функтор
let lawFunctor1 = fmap id m = m
let lawFunctor2 = fmap (g >> f) m = (fmap f >> fmap g) m

// 🔹 Аппликатив
let lawAp1 = ap (Just id) m = m
let lawAp2 = ap (Just f) (Just 10) = Just (f 10)
let lawAp3 = ap (Just f) (Just 7) = ap (Just (fun f -> f 7)) (Just f)

// 🔹 Монада
let mf x = Just (x + 1)
let mg x = Just (x * 2)

let lawBind1 = bind (Just 5) mf = mf 5
let lawBind2 = bind m Just = m
let lawBind3 = bind (bind m mf) mg = bind m (fun x -> bind (mf x) mg)

[<EntryPoint>]
let main argv =
    printfn "Функтор — идентичность: %b" lawFunctor1
    printfn "Функтор — композиция:   %b" lawFunctor2
    printfn "Аппликатив — идент.:    %b" lawAp1
    printfn "Аппликатив — гомом.:    %b" lawAp2
    printfn "Аппликатив — интерч.:   %b" lawAp3
    printfn "Монада — лев. нейтр.:   %b" lawBind1
    printfn "Монада — прав. нейтр.:  %b" lawBind2
    printfn "Монада — ассоц.:        %b" lawBind3
    0
