using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MonadicTreeLabeler
{
    class Program
    {
        static void Main(string[] args)
        {
            var tree = Build();
            tree.Show();

            var labeledTree = new Labeler().Label(tree);
            labeledTree.Show();

            var labeledWithMonads = new MonadicLabeler().Label(tree);
            labeledWithMonads.Show();
        }

        public static Node<string> Build()
        {
            return new Node<string>("a",
                new Node<string>("b"),
                new Node<string>("c",
                    new Node<string>("d",
                        new Node<string>("e"),
                        new Node<string>("f")),
                    new Node<string>("g")));
        }
    }

}
