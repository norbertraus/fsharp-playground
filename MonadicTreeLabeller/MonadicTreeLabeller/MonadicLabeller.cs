using System;

namespace MonadicTreeLabeller
{
    public class MonadicLabeller
    {
        public Node<Tuple<int, string>> Label(Node<string> root)
        {
            //create state monad that will carry through the label
            //invoke the state monad passing inital label for the tree
            return MakeMonad(root).Func(0).Item2;
        }

        //creates state monad - returns function that accepts the state and closes over the content for the state monad
        //this allows to pass inital value of the state and construct the state monad that will carry trought the side effects
        private StateMonad<int, Node<Tuple<int, string>>> MakeMonad(Node<string> root)
        {
            //compose state monad that will carry through the updated label with the results
            if(root.Left == null && root.Right == null)
            {
                return StateMonad.Bind(UpdateState(),
                    state => StateMonad.Return<int, Node<Tuple<int, string>>>(
                        new Node<Tuple<int, string>>(Tuple.Create(state, root.Content))));
            }

            if(root.Right == null)
            {
                return StateMonad.Bind(UpdateState(),
                    state => StateMonad.Bind(MakeMonad(root.Left),
                        left => StateMonad.Return<int, Node<Tuple<int, string>>>(
                            new Node<Tuple<int, string>>(Tuple.Create(state, root.Content), left: left))));
            }

            if(root.Left == null)
            {
                return StateMonad.Bind(UpdateState(),
                    state => StateMonad.Bind(MakeMonad(root.Right),
                        right => StateMonad.Return<int, Node<Tuple<int, string>>>(
                            new Node<Tuple<int, string>>(Tuple.Create(state, root.Content), right: right))));
            }

            //process left and right
            return StateMonad.Bind(UpdateState(),
                state => StateMonad.Bind(MakeMonad(root.Left),
                    left => StateMonad.Bind(MakeMonad(root.Right),
                        right => StateMonad.Return<int, Node<Tuple<int, string>>>(
                            new Node<Tuple<int, string>>(Tuple.Create(state, root.Content), left, right)))));

        }

        private static StateMonad<int, int> UpdateState()
        {
            return new StateMonad<int, int>(x => Tuple.Create(x + 1, x));
        }
    }
}