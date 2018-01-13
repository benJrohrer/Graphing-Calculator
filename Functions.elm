module Functions where

import String
import Parsing as P

type OP = OP_VAR | OP_ADD | OP_SUB | OP_MUL | OP_DIV | OP_EXP | OP_LPAR | OP_RPAR | OP_NPAR

type Function_Elem = 
    ELEM_EVAL Evaluable
    |ELEM_OP OP

type Function = 
    NUM Float
    |NEG Function
    |VAR 
    |ADD Function Function
    |SUB Function Function
    |MUL Function Function
    |DIV Function Function
    |EXP Function Function

type Evaluable = 
    STACK EvalStack
    | NEVAL Evaluable
    | EVAL Float
    | EVAR 
    
type alias EvalStack =
    (List Evaluable, List OP)


eval : Function -> Float -> Float
eval f x =
    case f of
        NUM x1      -> x1
        NEG f1      -> -1 * (eval f1 x)
        VAR         -> x
        ADD f1 f2   -> (eval f1 x) + (eval f2 x)
        SUB f1 f2   -> (eval f1 x) - (eval f2 x)
        MUL f1 f2   -> (eval f1 x) * (eval f2 x)
        DIV f1 f2   -> let div = (eval f2 x) in
            case div of
                0   -> 0
                _   -> (eval f1 x) / (eval f2 x)
                
        EXP f1 f2   -> (eval f1 x) ^ (eval f2 x)


stringToElem : String -> Result String Function_Elem
stringToElem s =
    case s of
        "+"     -> Ok (ELEM_OP OP_ADD)
        "-"     -> Ok (ELEM_OP OP_SUB)
        "*"     -> Ok (ELEM_OP OP_MUL)
        "/"     -> Ok (ELEM_OP OP_DIV)
        "^"     -> Ok (ELEM_OP OP_EXP)
        "("     -> Ok (ELEM_OP OP_LPAR)
        "~("    -> Ok (ELEM_OP OP_NPAR)
        ")"     -> Ok (ELEM_OP OP_RPAR)
        "x"     -> Ok (ELEM_EVAL EVAR)
        "~x"    -> Ok (ELEM_EVAL (NEVAL EVAR))
        _       -> 
            let r = String.toFloat s in 
            case r of
                Ok n    -> Ok (ELEM_EVAL (EVAL n))
                _       -> 
                    let r2 = String.toFloat (String.dropLeft 1 s) in
                        case r2 of
                            Ok n    -> Ok (ELEM_EVAL (NEVAL (EVAL n)))
                            _       -> Err s

appendEvalStack : EvalStack -> EvalStack -> EvalStack
appendEvalStack (evals1, ops1) (evals2, ops2) =
    (List.append evals1 evals2, List.append ops1 ops2)

openPar : Int -> Bool -> EvalStack -> EvalStack
openPar d neg (evals, ops) =
    if d > 0 then
        case evals of
            eval :: r   ->
                case eval of
                    STACK es    ->
                        let nes = openPar (d-1) neg es in
                            ((STACK nes) :: r, ops)
                    (NEVAL (STACK es))  ->
                        let nes = openPar( d-1) neg es in
                            ((NEVAL (STACK nes)) :: r, ops)
                    _           -> Debug.crash "Invalid depth in openPar"
            []            -> Debug.crash "Empty list in openPar"
    else
        if neg then
            ((NEVAL (STACK ([], []))) :: evals, ops)
        else
            (((STACK ([], [])) :: evals), ops)

addEvalStack : Int -> Function_Elem -> EvalStack -> EvalStack
addEvalStack d felem (evals, ops) =
    if d > 0 then
        case evals of
            eval :: r   ->
                case eval of
                    STACK es    ->
                        let nes = addEvalStack (d-1) felem es in
                            ((STACK nes) :: r, ops)
                    (NEVAL (STACK es))  ->
                        let nes = addEvalStack (d-1) felem es in
                            ((NEVAL (STACK nes)) :: r, ops)
                    _           -> Debug.crash "Invalid depth in addEvalStack"
            []            -> Debug.crash "Empty list in addEvalStack"
    else
        case felem of
            ELEM_OP op      -> (evals, op::ops)
            ELEM_EVAL eval  -> (eval::evals, ops)



buildEvalStack : Int -> EvalStack -> List String -> Result String EvalStack
buildEvalStack d ces ls =
    case ls of
        []      -> Ok ces
        s :: rest  -> 
            let r = stringToElem s in
            case r of
                Ok elem     ->
                    case elem of
                        ELEM_OP OP_NPAR ->
                            let nes = (openPar d True ces) in buildEvalStack (d+1) nes rest
                        ELEM_OP OP_LPAR ->
                            let nes = (openPar d False ces) in buildEvalStack (d+1) nes rest
                        ELEM_OP OP_RPAR -> buildEvalStack (d-1) ces rest
                        _               ->
                            let nes = (addEvalStack d elem ces) in buildEvalStack d nes rest
                Err errs            -> Err errs

composeFunction : OP -> Function -> Function -> Result String Function
composeFunction op f1 f2 =
    case op of
        OP_ADD  -> Ok (ADD f1 f2)
        OP_SUB  -> Ok (SUB f1 f2)
        OP_MUL  -> Ok (MUL f1 f2)
        OP_DIV  -> Ok (DIV f1 f2)
        OP_EXP  -> Ok (EXP f1 f2)
        _       -> Err "Unimplemented function or operator"

evaluableToFunction : Evaluable -> Result String Function
evaluableToFunction eval =
    case eval of
        STACK es    -> stackToFunction es
        EVAL n      -> Ok (NUM n)
        EVAR        -> Ok VAR
        NEVAL ev    -> let r = evaluableToFunction ev in
            case r of
                Ok f    -> Ok (NEG f)
                Err s   -> Err s

regroupEval : Evaluable -> Evaluable
regroupEval e =
    case e of
        STACK es    -> STACK (regroup es)
        _           -> e

regroup : EvalStack -> EvalStack
regroup (evals, ops) =
    case evals of
        (eval1 :: eval2 :: revals)  ->
            let regeval1 = regroupEval eval1 in
            let regeval2 = regroupEval eval2 in
                case ops of
                    (op1 :: rops)   -> 
                        case op1 of
                            OP_MUL  ->
                                regroup (STACK ([regeval1, regeval2], [op1]) :: revals, rops)
                            OP_DIV -> 
                                regroup (STACK ([regeval1, regeval2], [op1]) :: revals, rops)
                            _       ->
                                let (regevals, regops) = regroup (regeval2 :: revals, rops) in
                                    (regeval1 :: regevals, op1 :: regops)
                    []              -> (evals, ops)
        _                       -> (evals, ops)


stackToFunction : EvalStack -> Result String Function
stackToFunction (evals, ops) = 
    case evals of
        (eval1 :: reval)    ->
            let r2 = evaluableToFunction eval1 in
                case r2 of
                    Err s   -> Err s
                    Ok f2   ->
                        case ops of
                            (op :: rops)    -> 
                                let r1 = (stackToFunction (reval, rops)) in
                                case r1 of
                                    Ok f1    -> composeFunction op f1 f2
                                    Err s    -> Err s
                            []              -> Ok f2
        []                  -> Err "Unbalanced operands/operators"



buildFunction : List String -> Result String Function
buildFunction ls =
    let r = buildEvalStack 0 ([], []) ls in
        case r of
            Ok es   -> 
                let res = regroup es in
                    let rf = stackToFunction res in
                        case rf of
                            Ok f    -> Ok f
                            Err s   -> Err s
            Err s   -> Err s

stringToFunction : String -> Result String Function
stringToFunction s =
    buildFunction (P.parseStringToCompute s)


-- added by Zakir for testing
stringToResult : String -> String
stringToResult s =
  let func = stringToFunction s in
    case func of
        Ok f    -> toString (eval f 0)
        Err s   -> s







