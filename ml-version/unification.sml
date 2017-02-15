(* term is either variable `x`, or application `(e e ...)` *)
datatype term = Tyvar of string
              | Tyapp of string * (term list)

(* 
  *Types*

  subs: (term, string) list -> term -> term

  *Notes*

  subs: `substitutions`, `termToBeSubstituted`
    1. if `term` is a variable, apply the `substitutions` to the term.
    2. if `term` is an empty application, `term` is the result.
    3. if `term` is an application and not empty, map the `substitutions` to
    every expressions of the `term`.
*)
fun subs [] term = term
  | subs ((t1, v1)::ss) (term as Tyvar name) = 
  if name = v1 then t1 else subs ss term
  | subs _ (term as Tyapp(name, [])) = term
  | subs l (Tyapp (name, args)) = 
  let fun arglist r [] = rev r
    | arglist r (h::t) =
    arglist ((subs l h)::r) t
  in
    Tyapp (name, arglist [] args)
  end

(* 
  *Types*

  compose: (term, string) list -> (term, string) list -> (term, string) list
    iter: (term, string) list -> (term, string) -> (term, string) list -> (term, string) list 

  *Notes*

  compose: `substitutions1`, `substitution2`
    use `iter` to apply every substitution of `substitutions1` to `substitution2`

  iter: `resultList`, `aSubstitution`, `listToBeComposed`
    apply `aSubstitution` to `listToBeComposed` and get `resultList`
*)
fun compose [] s1 = s1
  | compose (s::ss) s1 =
    let fun iter r s [] = rev r
      | iter r s ((t1, v1)::ss) = 
        iter (((subs [s] t1), v1)::r) s ss
    in
      compose ss (s::(iter [] s s1))
    end

exception Unify of string


(*
  *Types*

  unify: term -> term -> (term, string) list
    iter: (term, string) list -> term -> term -> (term, string) list
      occurs: string -> term -> bool
      unify_args: (term, string) list -> term list -> term list -> (term, string) list

  *Notes*

  unify: `t1`, `t2`
    Call the helper function `iter` to unify `t1` and `t2`.

  iter: `revertedListOfResult`, `t1`, `t2`
    The helper function to unify `t1` `t2` in an iterate way.
    1. `t1` is a variable named `v1`, and `t2` is a variable named `v2`. If the two names `v1` and `v2` are equal, add
    the substitution `{t1/v2}`, or empty list `[]`??? (Why not the unchanged `r`?)
    2. `t1` is a variable named `v` and `t2` is an empty application, add the substitution `{t2/v}`.
    3. `t1` is an empty application and `t2` is a variable named `v`, add the substitution `{t1/v}`.
    4. `t1` is a variable named `v` and `t2` is an nonempty applicaton, if v
    doesn't occur in t2, add the substitution `{t2/v}`, or error.
    5. `t1` is an nonempty applicaton and `t2` is a variable named `v`, if v
    doesn't occur in t1, add the substitution `{t1/v}`, or error.
    6. `t1` and `t2` are both applications, if the names are equal and the
    number of args are equal, unify each term of `args1` and `args2` one to one
    correspondence.

  occurs: `nameOfTerm`, `term`
    decide whether `nameOfTerm` occurs in `term`
        
  unify_args: `revertedListOfSubstitution`, `termList1`, `termList2`
    unify `termList1` and `termList2`, which separately are two applications' args list.
    1. The length of `termList1` must be the same as `termList2`'s.
    2. unify each term of `termList1` and `termList2` one to one correspondence,
    using pre-collected information `revertedListOfSubstitution`.
    at the end of each iteration, compose the result substitutions to
    `revertedListOfSubstitution`.
*)
fun unify t1 t2 =
  let fun iter r t1 t2 =
    let fun occurs v (Tyapp (name, [])) = false
      | occurs v (Tyapp (name, ((Tyvar vn)::t))) = 
        if vn = v then true else occurs v (Tyapp (name, t))
      | occurs v (Tyapp (name, (s::t))) =
        occurs v s orelse occurs v (Tyapp (name, t))
      | occurs v (Tyvar vn) = vn = v
    fun unify_args r [] [] = rev r
      | unify_args r [] _ = raise Unify "Arity"
      | unify_args r _ [] = raise Unify "Arity"
      | unify_args r (t1::t1s) (t2::t2s) = 
        unify_args (compose (iter [] (subs r t1) (subs r t2)) r) t1s t2s
    in
      case (t1, t2) of
           (Tyvar v1, Tyvar v2) => if (v1 = v2) then [] else ((t1, v2)::r)
         | (Tyvar v, Tyapp (_, [])) => ((t2, v)::r)
         | (Tyapp (_, []), Tyvar v) => ((t1, v)::r)
         | (Tyvar v, Tyapp _) => 
             if occurs v t2 then raise Unify "Occurs" else ((t2, v)::r)
         | (Tyapp _, Tyvar v) =>
             if occurs v t1 then raise Unify "Occurs" else ((t1, v)::r)
         | (Tyapp (name1, args1), Tyapp (name2, args2)) =>
             if (name1 = name2)
               then unify_args r args1 args2
               else raise Unify "Const"
    end
  in
    iter [] t1 t2
  end



(*
fun pptsterm tau =
let fun iter prec (Tyvar name) = ""^name
  | iter prec (Tyapp(name,[])) = name
  | iter prec (Tyapp("%f",[a1,a2])) =
  let fun maybebracket s = if prec <= 10 then s else "("^s^")"
  in
    maybebracket ((iter 11 a1)^" -> "^(iter 10 a2)) 
  end
    | iter prec (Tyapp(name,args)) = 
    let fun arglist r [] = r
        | arglist r (h::t) =
          arglist (r^(iter 30 h)^(if t=[] then "" else ", ")) t
    in
      if (length args) > 1 then (arglist "(" args)^") "^name else (arglist ""
      args)^" "^name
    end 
  in
    iter 10 tau 
  end
*)

fun ppterm (Tyvar name) = name
  | ppterm (Tyapp(name,[])) = name | ppterm (Tyapp(name,args)) =
    let fun arglist r [] = r
      | arglist r (h::t) =
        arglist (r^(ppterm h)^(if t=[] then "" else ",")) t 
    in
      name^(arglist "(" args)^")"
    end

fun ppsubs s =
  let fun iter r [] = r^"]"
    | iter r ((term,var)::t) =
      iter (r^(ppterm term)^"/"^var^(if t=[] then "" else ",")) t
  in iter "[" s end

(*
fun ppexp e =
  let fun ppe r e =
    case e of
     (Var v) => r^v
   | (Comb(e1,e2)) => r^"("^(ppe "" e1)^" "^(ppe "" e2)^")"
   | (Abs(v,e)) => r^"(\\"^v^"."^(ppe "" e)^")"
   | (Let((v,e1),e2)) => r^"let "^v^"="^(ppe "" e1)^" in "^(ppe "" e2)
  in
    ppe "" e 
  end

fun ppts sigma =
  let fun iter r (Forall(sv,sts)) = iter (r^"!"^sv^".") sts
    | iter r (Type term) = r^(pptsterm term)
  in
    iter "" sigma 
  end

fun ppassums Gamma =
  let fun iter r [] = r
    | iter r ((v,ts)::assums) =
      iter (r^v^":"^(ppts ts)^(if assums=[] then "" else ",")) assums
  in
    iter "" Gamma 
  end
*)
