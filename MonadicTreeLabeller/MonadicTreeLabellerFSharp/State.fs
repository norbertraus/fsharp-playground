namespace Monads

//State type that takes generic state and returns state and content tuple
type State<'state, 'a> = State of ('state -> 'state * 'a)

module State =

    type StateBuilder() =
        
        member this.Return a = State(fun s -> (s, a))

        member this.Bind(m, f) = State(fun s -> let (s', a) = match m with
                                                              | State f -> f s
                                                match f a with
                                                | State f -> f s')  
    let state = StateBuilder()

    let GetState = State (fun s -> (s, s))
    let SetState s = State (fun _ -> (s, ()))

    let Execute m s = match m with
                      | State f -> let (s', a) = f s in a


    type Node<'a> =
        | Leaf of 'a 
        | Left of 'a * Node<'a>
        | Right of 'a * Node<'a>
        | Both of 'a * Node<'a> * Node<'a>


    let tree = Both("a", Leaf("b"), Both("c", Both("d", Leaf("e"), Leaf("f")), Leaf("g")))  

    let showLeaf l indent = System.Console.WriteLine("{0}{1}:{2}", (new System.String(' ', indent)), "Node", l.ToString())

    let showTree t =
        let rec showTree' root indent =
             match root with 
             | Leaf a -> showLeaf a indent   
             | Both (a, l, r) -> 
                showLeaf a indent 
                showTree' l (indent + 2) 
                showTree' r (indent + 2) 
             | _ -> failwith "not expected"    
    
        showTree' t 0
    
    //showTree tree |> ignore

    //functional labeller
    let Labeller node =
        let rec Labeller' root label =
            match root with
            | Leaf a -> (Leaf((a, label)), label + 1)
            | Both (a, l, r) ->
                let left = Labeller' l (label + 1)
                let right = Labeller' r (snd left)
                (Both((a, label), fst left, fst right), snd right)
            | _ -> failwith "not expected"   
        
        fst (Labeller' node 0)

    let labelledTree = Labeller tree

    //showTree labelledTree |> ignore

    //monadic labeller
    let MonadicLabeller node =
        let rec MonadicLabeller' r =
            match r with
            | Leaf(c) -> state { let! x = GetState
                                 do! SetState (x + 1)
                                 return Leaf(c,x) }
                         
            | Both(c,l,r) -> state { let! x = GetState
                                     do! SetState (x + 1)
                                     let! l = MonadicLabeller' l
                                     let! r = MonadicLabeller' r
                                     return Both((c, x),l,r) }
            | _ -> failwith "not expected"   

        MonadicLabeller' node 
    
    let labelWithMonad x = 
       match MonadicLabeller(x) with
       | State f -> let (a, b) = (f 0) in b

    let labTree2 = labelWithMonad tree

    //showTree labTree2 |> ignore