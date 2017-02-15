use "unification.sml";

val x = Tyvar "x"
val y = Tyvar "y"
val z = Tyvar "z"

fun apply s l = Tyapp (s, l)

val a = apply "a" []
fun j (x, y, z) = apply "j" [x, y, z]
fun f (x, y) = apply "f" [x, y]

val t1 = j (x, y, z)
val t2 = j (f (y, y), f (z, z), f (a, a));

val U = unify t1 t2;
ppsubs U;
