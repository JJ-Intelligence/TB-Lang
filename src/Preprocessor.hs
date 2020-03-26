data TypeConstraint 
                = TCGeneric String -- 'a'
                | TCEq String -- 'Eq a => a'
                | TCInt 
                | TCBool 
                | TCList TypeConstraint 
                | TCEmpty

data VarScope = Passed | Local -- Needed to maintain the type of a parameter if it is overwritten by a local var.

TypeEnv: Map (String, VarScope) Address
 --> For global x var or parameter x var, insert (x, Passed). For local x var (in function scope), insert (x, Local).
 --> When looking for a var, first look for it with Local scope, then with Global scope if not in Local.
TypeStore: Map Address TypeConstraint


=> TODO:
 * Setup a compiler-time type-checking CEK
 * States should include an Env (Map (String, Scope) Address) and Store (Address TypeConstraint) (due to global vars changing type from within functions)
 * States should include a Kontinuation?
 * Code type system for variable declaration and operations.
 * Code type system for blocks, loops, and statements.
 * Code type system for function definitions.
 * Code type system for function calls.