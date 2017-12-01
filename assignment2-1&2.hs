replic lt = map(\(a,b) -> replicate a b) (zip lt lt)
data Formula
        = Atom Bool			-- atomic formula
        | And Formula Formula		-- f /\ f
        | Or Formula Formula		-- f \/ f
	| Implies Formula Formula       -- f -> f
        | Not Formula			-- not(f)

instance Show Formula where 
   show (Atom a) = "Atom " ++ show a
   show (And a b) = "And (" ++ show a ++ ") (" ++ show b ++ ")"
   show (Or a b) = "Or (" ++ show a ++ ") (" ++ show b ++ ")"
   show (Implies a b) = "Implies (" ++ show a ++ ") (" ++ show b ++ ")"
   show (Not a) = "Not (" ++ show a ++ ")"

collect_atoms (Atom x) = [x]
collect_atoms (And a b) = collect_atoms a ++ collect_atoms b
collect_atoms (Or a b) = collect_atoms a ++ collect_atoms b
collect_atoms (Implies a b) = collect_atoms a ++ collect_atoms b
collect_atoms (Not a) = collect_atoms a

eval (Atom a) = a
eval (And a b) = eval a && eval b
eval (Or a b) = eval a || eval b
eval (Implies a b) = if (eval a) && (not (eval b)) then False else True
eval (Not a) = not (eval a)