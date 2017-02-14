use "define.sml";

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
