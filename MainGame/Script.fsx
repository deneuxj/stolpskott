// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

type ScalarExpr =
    | Constant of float
    | Variable of string
    | Add of ScalarExpr * ScalarExpr
    | Mult of ScalarExpr * ScalarExpr
    | Div of ScalarExpr * ScalarExpr
    | DotProduct of VectorExpr * VectorExpr
    | ExtractX of VectorExpr
    | ExtractY of VectorExpr
    | ExtractZ of VectorExpr

and VectorExpr =
    | Build of ScalarExpr * ScalarExpr * ScalarExpr
    | Variable of string
    | Add of VectorExpr * VectorExpr
    | Scale of ScalarExpr * VectorExpr
        
type Function =
    | ScalarFun of string[] * ScalarExpr
    | VectorFun of string[] * VectorExpr

module Operators =
    let (~-.) e  = Mult(Constant -1.0, e)
    let (.+.) e1 e2 = ScalarExpr.Add(e1, e2)
    let (.-.) e1 e2 = e1 .+. (-.e2)
    let (.*.) e1 e2 = Mult(e1, e2)
    let (./.) e1 e2 = Div(e1, e2)

    let (~-) e = Scale(-.(Constant 1.0), e)
    let (+) e1 e2 = VectorExpr.Add(e1, e2)
    let (-) e1 e2 = e1 + -e2
    let (*) e1 e2 = DotProduct(e1, e2)

open Operators

let rec derivS v e =
    match e with
    | Constant x -> Constant 0.0
    | ScalarExpr.Variable w ->
        if w = v then
            Constant 1.0
        else
            e
    | ScalarExpr.Add (e1, e2) ->
        (derivS v e1) .+. (derivS v e2)
    | Mult (e1, e2) ->
        ((derivS v e1) .*. e2) .+. (e1 .*. (derivS v e2))
    | Div (e1, e2) ->
        ((derivS v e1) .*. e2) .+. (e1 .*. (derivS v e2)) ./. (e2 .*. e2)
    | DotProduct (e1, e2) ->
        ((derivV v e1) * e2) .+. (e1 * (derivV v e2))
    | ExtractX e ->
        ExtractX (derivV v e)
    | ExtractY e ->
        ExtractY (derivV v e)
    | ExtractZ e ->
        ExtractZ (derivV v e)

and derivV v e =
    match e with
    | Build (e1, e2, e3) ->
        Build (derivS v e1, derivS v e2, derivS v e3)
    | VectorExpr.Variable v2 ->
        let exploded = Build (ExtractX e, ExtractY e, ExtractZ e)
        derivV v exploded
    | VectorExpr.Add(e1, e2) ->
        (derivV v e1) + (derivV v e2)
    | Scale(k, e) ->
        let k' = derivS v k
        let e' = derivV v e
        Scale (k', e) + Scale (k, e')

let expandDotProduct (e1, e2) =
    ((ExtractX e1) .*. (ExtractX e2))
    .+.
    ((ExtractY e1) .*. (ExtractY e2))
    .+.
    ((ExtractZ e1) .*. (ExtractZ e2))

let rec pushS e =
    match e with
    | Constant _ -> e
    | ScalarExpr.Variable _ -> e
    | ScalarExpr.Add (e1, e2) ->
        pushS e1 .+. pushS e2
    | Mult (e1, e2) ->
        pushS e1 .*. pushS e2
    | Div (e1, e2) ->
        pushS e1 ./. pushS e2
    | DotProduct (e1, e2) ->
        (e1, e2) |> expandDotProduct |> pushS
    | ExtractX(Build(e, _, _))
    | ExtractY(Build(_, e, _))
    | ExtractZ(Build(_, _, e)) -> e
    | ExtractX(VectorExpr.Add(e1, e2)) ->
        pushS (ExtractX e1) .+. pushS (ExtractX e2)
    | ExtractY(VectorExpr.Add(e1, e2)) ->
        pushS (ExtractY e1) .+. pushS (ExtractY e2)
    | ExtractZ(VectorExpr.Add(e1, e2)) ->
        pushS (ExtractZ e1) .+. pushS (ExtractZ e2)
    | ExtractX(Scale(k, e)) ->
        pushS k .*. pushS (ExtractX e)
    | ExtractY(Scale(k, e)) ->
        pushS k .*. pushS (ExtractY e)
    | ExtractZ(Scale(k, e)) ->
        pushS k .*. pushS (ExtractZ e)
    | ExtractX (VectorExpr.Variable _)
    | ExtractY (VectorExpr.Variable _)
    | ExtractZ (VectorExpr.Variable _) ->
        e

let dbg label f =
    printfn "Enter %s" label
    let r = f()
    printfn "Returned %A" r
    r

let rec (|CleanS|) e =
    match e with
    | ScalarExpr.Add(CleanS (Constant 0.0), CleanS e)
    | ScalarExpr.Add(CleanS e, CleanS (Constant 0.0)) ->
        e

    | ScalarExpr.Add(CleanS e1, CleanS e2) ->
        e1 .+. e2

    | Mult(CleanS(Constant 0.0), _)
    | Mult(_, CleanS(Constant 0.0)) ->
        Constant 0.0
    
    | Mult(CleanS(Constant 1.0), CleanS e)
    | Mult(CleanS e, CleanS(Constant 1.0)) ->
        e

    | Mult(CleanS e1, CleanS e2) ->
        e1 .*. e2

    | Div(CleanS e, CleanS(Constant 1.0)) ->
        e

    | Div(CleanS (Constant 0.0), _) ->
        Constant 0.0

    | Div(CleanS e1, CleanS e2) ->
        e1 ./. e2

    | ScalarExpr.Variable _
    | Constant _ -> e

    | DotProduct(CleanV e1, CleanV e2) ->
        DotProduct(e1, e2)

    | ExtractX(CleanV e) -> ExtractX e
    | ExtractY(CleanV e) -> ExtractY e
    | ExtractZ(CleanV e) -> ExtractZ e

and (|CleanV|) e =
    match e with
    | Build(CleanS e1, CleanS e2, CleanS e3) -> Build(e1, e2, e3)
    | VectorExpr.Variable _ -> e
    | VectorExpr.Add(CleanV e1, CleanV e2) -> e1 + e2
    | Scale(CleanS k, CleanV e) -> Scale(k, e)

let rec (|SumS|_|) e =
    fun () ->
        printfn "S: %A" e
        match e with
        | Constant 0.0 -> Some []
        | Constant _ 
        | ScalarExpr.Variable _ ->
            Some [e]
        | ScalarExpr.Add(e1, e2) ->
            Some [e1 ; e2]
        | Mult (e1, SumS es) ->
            es
            |> List.map (fun e2  -> e1 .*. e2)
            |> Some
        | Mult (SumS es, e3) ->
            es
            |> List.map (fun e1 -> e1 .*. e3)
            |> Some
        | Mult _ ->
            None
        | Div(SumS es, e3) ->
            es
            |> List.map (fun e1 -> e1 ./. e3)
            |> Some
        | Div _ ->
            None
        | DotProduct _ ->
            None
        | ExtractX _
        | ExtractY _
        | ExtractZ _ ->
            None
    |> dbg "SumS"

let sum es =
    let (CleanS e) = List.fold (.+.) (Constant 0.0) es
    e
    
let rec (|FactS|_|) v e =
    fun () ->
        printfn "F: %A" e
        match e with
        | Constant 0.0 -> Some (Constant 0.0)
        | Constant _ -> None
        | ScalarExpr.Variable w when v = w ->
            Some (Constant 1.0)
        | ScalarExpr.Variable _ ->
            None
        | Mult(FactS v e1, e2) ->
            Some (Mult(e1, e2))
        | Mult(e1, FactS v e2) ->
            Some (Mult(e1, e2))
        | Mult _ ->
            None
        | SumS(es) ->
            let factEs =
                es
                |> List.choose (fun e -> match e with FactS v e -> Some e | _ -> None)

            if List.length factEs = List.length es then
                Some (sum factEs)
            else
                None
        | _ -> None
    |> dbg "FactS"

let rec containsS v e =
    match e with
    | ScalarExpr.Variable w when w = v ->
        true
    | ScalarExpr.Variable _ ->
        false
    | Constant _ ->
        false
    | ScalarExpr.Add(e1, e2)
    | Div(e1, e2)
    | Mult(e1, e2) ->
        containsS v e1 || containsS v e2
    | DotProduct(e1, e2) ->
        containsV v e1 || containsV v e2
    | ExtractX e
    | ExtractY e
    | ExtractZ e ->
        containsV v e

and containsV v e =
    match e with
    | Build(e1, e2, e3) ->
        containsS v e1 || containsS v e2 || containsS v e3
    | VectorExpr.Variable _ ->
        false
    | VectorExpr.Add(e1, e2) ->
        containsV v e1 || containsV v e2
    | Scale(k, e) ->
        containsS v k || containsV v e
            
let rec solveS v (lhs, rhs) =
    match lhs with
    | FactS v e -> Some (rhs ./. e)
    | SumS(es) ->
        let withV, withoutV =
            es
            |> List.partition (containsS v)
        if List.isEmpty withoutV then
            match lhs with
            | SumS([SumS es1 ; SumS es2]) ->
                let es = List.append es1 es2
                solveS v (sum es, rhs)
            | _ -> None
        else
            let lhs = sum withV
            let rhs' = sum withoutV
            solveS v (lhs, rhs .-. rhs')
    | _ -> None     

(*
let solveV v e =
    let r1 = ExtractX e |> solveS v
    let r2 = ExtractY e |> solveS v
    let r3 = ExtractZ e |> solveS v
    [
        for r1 in r1 do
            for r2 in r2 do
                for r3 in r3 do
                    yield (r1, r2, r3)
    ]
*)

(*
let k = ScalarExpr.Variable "k"
let t = ScalarExpr.Variable "t"
let P0 = VectorExpr.Variable "P0"
let P1 = VectorExpr.Variable "P1"
let D0 = VectorExpr.Variable "D0"
let D1 = VectorExpr.Variable "D1"

let I0 = P0 + Scale(k, D0)
let I1 = P1 + Scale(t, D1)
let d = I1 - I0
let dist = d * d
let ra0 = DotProduct(d, I0)
let ra1 = DotProduct(d, I1)

let s0 = solveS "k" ra0
let l0 = liftS "k" ra0
*)

let v s = ScalarExpr.Variable s
let n x = ScalarExpr.Constant x

let e =
    v "x" .+. (v "y" .*. (v "x" .+. n 1.0))

match solveS "x" (e, Constant 0.0) with
| Some(CleanS rhs) -> printfn "x = %A" rhs
| None -> printfn "???"
