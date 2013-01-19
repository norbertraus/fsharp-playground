using System;

namespace MonadicTreeLabeller
{
    public class Labeller
    {
        public Node<Tuple<int, string>> Label(Node<string> root)
        {
            Func<Node<string>, int, Tuple<Node<Tuple<int, string>>, int>> labeller = null;
            labeller = (node, label) =>
            {
                int nextLabel = label + 1;
                Node<Tuple<int, string>> left = null;
                if(node.Left != null)
                {
                    var leftLabeled = labeller(node.Left, nextLabel);
                    left = leftLabeled.Item1;
                    nextLabel = leftLabeled.Item2;
                }

                Node<Tuple<int, string>> right = null;
                if(node.Right != null)
                {
                    var rightLabeled = labeller(node.Right, nextLabel);
                    right = rightLabeled.Item1;
                    nextLabel = rightLabeled.Item2;
                }

                return new Tuple<Node<Tuple<int, string>>, int>(
                    new Node<Tuple<int, string>>(Tuple.Create(label, node.Content), left, right),
                    nextLabel);
            };

            return labeller(root, 0).Item1;
        }
    }
}