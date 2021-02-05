module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

type Flour = Flour of int

type Model =
  { Flour: int
    Hydration: int }

type Msg =
| ChangeFlour of int
| ChangeHydration of int

let init () =
  { Flour = 1000
    Hydration = 72 }

let validateHydration hydration =
  match hydration with
  | h when h < 0  -> 0
  | h when h > 100 -> 100
  | h -> h

let update (msg: Msg) (model: Model) =
  match msg with
  | ChangeFlour  f -> { model with Flour = f }
  | ChangeHydration h -> { model with Hydration = validateHydration h }

let ingredient (percent: float) flour =
  ((flour |> float) * percent) |> int

let water hydration =
  ingredient ((hydration |> float) / 100.0)

let salt = ingredient 0.021
let yeast = ingredient 0.004

let toGramString = sprintf "%ig"

let toInt str =
  match str with
  | "" -> 0
  | _ -> int str

let view (model: Model) dispatch =
  div []
    [ h1 [] [ str "Recipe"]
      ul []
        [ li [] [ str "Flour: "; str (toGramString model.Flour) ]
          li [] [ str "Water: "; str (toGramString (water model.Hydration model.Flour)) ]
          li [] [ str "Salt: "; str (toGramString (salt model.Flour)) ]
          li [] [ str "Yeast: "; str (toGramString (yeast model.Flour)) ] ]
      div []
        [ input [ Type "number"; OnChange (fun e -> dispatch (ChangeFlour (toInt e.Value))); Value model.Flour ]; str "g"
          br []
          input [ Type "number"; OnChange (fun e -> dispatch (ChangeHydration (toInt e.Value))); Value model.Hydration ]; str "%" ] ]

Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run