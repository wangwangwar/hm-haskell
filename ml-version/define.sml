datatype term = Tyvar of string
              | Tyapp of string * (term list)
