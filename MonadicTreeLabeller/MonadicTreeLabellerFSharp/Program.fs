
open Monads

[<EntryPoint>]
let main argv = 
          
    let labTree2 = State.labelWithMonad State.tree
    State.showTree labTree2

    0 

