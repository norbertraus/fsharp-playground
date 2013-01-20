using System;

namespace MonadicTreeLabeller
{
    public class StateMonad<State, Content>
    {
        public static StateMonad<State, Content> Create(Func<State, Tuple<State, Content>> func)
        {
            return new StateMonad<State, Content>(func);
        }

        public StateMonad(Func<State, Tuple<State, Content>> func)
        {
            Func = func;
        }

        public Func<State, Tuple<State, Content>> Func { get; private set; }
    }

    public class StateMonad
    {
        public static StateMonad<State, T> Return<State, T>(T content)
        {
            return new StateMonad<State, T>(state => Tuple.Create(state, content));
        }

        public static StateMonad<State, TDst> Bind<State, TSrc, TDst>(StateMonad<State, TSrc> stateMonad, Func<TSrc, StateMonad<State, TDst>> monadMaker)
        {
            return new StateMonad<State, TDst>(state =>
            {
                var tuple = stateMonad.Func(state);
                return monadMaker(tuple.Item2).Func(tuple.Item1);
            });
        }
    }



    //public delegate StateContentPair<T> StateMonad<T>(int state);

    //public static class LinqStateMonad
    //{
    //    public static StateMonad<U> Select<T, U>(this StateMonad<T> p, Func<T, U> selector)
    //    {
    //        return state => new StateContentPair<U>(selector(p(state).Content), state);
    //    }
    //    public static StateMonad<V> SelectMany<T, U, V>(this StateMonad<T> p, Func<T, StateMonad<U>> selector, Func<T, U, V> projector)
    //    {
    //        return state =>
    //        {
    //            var first = p(state);
    //            var second = selector(first.Content)(first.State);
    //            var content = projector(first.Content, second.Content);
    //            return new StateContentPair<V>(content, second.State);
    //        };
    //    }

    //    public static StateMonad<int> GetState()
    //    {
    //        return s => new StateContentPair<int>(s, s);
    //    }

    //    public static StateMonad<T> SetState<T>(int s1)
    //    {
    //        return s => new StateContentPair<T>(default(T), s1);
    //    }
    //}

    
////LINQ Monadic Labelling of a Binary Tree

//public static StateMonad<BTree> MLabel(BTree tree)
//{
//    var branch = tree as BBranch;
//    var leaf = tree as BLeaf;

//    StateMonad<BTree> ret = null;

//    if (leaf != null)
//    {
//        var q = from x in LinqStateMonad.GetState()
//                from f in LinqStateMonad.SetState<int>(x + 1)
//                select new BLeaf { Content = leaf.Content, State = x };
//        ret = s => { var r = q(s); return new StateContentPair<BTree>(r.Content, r.State); };
//    }

//    if (branch != null)
//    {
//        var q = from l in MLabel(branch.Left)
//                from r in MLabel(branch.Right)
//                select new BBranch() { Left = l, Right = r };
//        ret = s => { var r = q(s); return new StateContentPair<BTree>(r.Content, r.State); };
//    }

//    return ret;
//}
}