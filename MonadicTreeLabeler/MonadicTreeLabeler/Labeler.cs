using System;

namespace MonadicTreeLabeler
{
    public class Labeler
    {
        public Node<Tuple<int, string>> Label(Node<string> root)
        {
            Func<Node<string>, int, Tuple<Node<Tuple<int, string>>, int>> labeler = null;
            labeler = (node, label) =>
            {
                int nextLabel = label + 1;
                Node<Tuple<int, string>> left = null;
                if(node.Left != null)
                {
                    var leftLabeled = labeler(node.Left, nextLabel);
                    left = leftLabeled.Item1;
                    nextLabel = leftLabeled.Item2;
                }

                Node<Tuple<int, string>> right = null;
                if(node.Right != null)
                {
                    var rightLabeled = labeler(node.Right, nextLabel);
                    right = rightLabeled.Item1;
                    nextLabel = rightLabeled.Item2;
                }

                return new Tuple<Node<Tuple<int, string>>, int>(
                    new Node<Tuple<int, string>>(Tuple.Create(label, node.Content), left, right),
                    nextLabel);
            };

            return labeler(root, 0).Item1;
        }
    }
}