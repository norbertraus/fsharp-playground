using System;

namespace MonadicTreeLabeller
{
    public class Node<T>
    {
        public static Node<T> Create<T>(T content, Node<T> left = null, Node<T> right = null)
        {
            return new Node<T>(content, left, right);
        }

        public Node(T content, Node<T> left = null, Node<T> right = null)
        {
            Content = content;
            Left = left;
            Right = right;
        }

        public T Content { get; private set; }

        public Node<T> Left { get; private set; }

        public Node<T> Right { get; private set; }

        public void Show(int indent = 0)
        {
            //show current content
            Console.Write(new string(' ', indent));
            Console.Write("Node: \"{0}\"", Content);
            Console.WriteLine();
            if(Left != null)
            {
                Left.Show(indent + 2);
            }

            if(Right != null)
            {
                Right.Show(indent + 2);
            }
        }
    }
}