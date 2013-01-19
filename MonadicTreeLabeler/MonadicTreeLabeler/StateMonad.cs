using System;

namespace MonadicTreeLabeler
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
}