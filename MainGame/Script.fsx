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
                let sum =
                    factEs
                    |> List.fold (.+.) (Constant 0.0)
                Some (sum)
            else
                None
        | _ -> None
    |> dbg "FactS"

let rec (|Clean|) e =
    fun () ->
        printfn "<<< %A" e
        match e with
        | ScalarExpr.Add(Clean (Constant 0.0), Clean e)
        | ScalarExpr.Add(Clean e, Clean (Constant 0.0)) ->
            e

        | ScalarExpr.Add(Clean e1, Clean e2) ->
            e1 .+. e2

        | Mult(Clean(Constant 0.0), _)
        | Mult(_, Clean(Constant 0.0)) ->
            Constant 0.0
    
        | Mult(Clean(Constant 1.0), Clean e)
        | Mult(Clean e, Clean(Constant 1.0)) ->
            e

        | Mult(Clean e1, Clean e2) ->
            e1 .*. e2

        | Div(Clean e, Clean(Constant 1.0)) ->
            e

        | Div(Clean (Constant 0.0), _) ->
            Constant 0.0

        | Div(Clean e1, Clean e2) ->
            e1 ./. e2

        | ScalarExpr.Variable _
        | Constant _ -> e

        | DotProduct _ -> e

        | ExtractX _
        | ExtractY _
        | ExtractZ _ -> e

    |> dbg "Clean"

(*        
let rec solveS v (lhs, rhs) =
    match lhs with
    | Constant k -> [Constant 0.0, rhs .-. lhs]
    | ScalarExpr.Variable w when w = v ->
        [lhs, rhs]
    | ScalarExpr.Variable _ ->
        [Constant 0.0, rhs .-. lhs]
    | ScalarExpr.Add (e1, e2) ->
        failwith "TODO"
    | Mult (e1, e2) ->
        failwith "TODO"

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
    v "x" .+. (v "y" .*. (v "x" .+. n 0.0))

let (Clean c) = (n 0.0 .+. n 1.0) .+. n 1.0

printfn "%A" c

match e with
| FactS "x" (Clean e') -> printfn "%A = x * %A" e e'
//| SumS es -> printfn "%A" es
| _ -> printfn "???"