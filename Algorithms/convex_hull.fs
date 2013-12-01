module convexhull

//#r "System";;
//#r "System.Xaml.dll";;
//#r "PresentationCore.dll";;
//#r "PresentationFramework.dll";;
//#r "WindowsBase.dll";;

open System
open System.Windows

type Turn =
  | Left
  | Right

type public Point2D = { X:double; Y:double }

//translates from radians to degrees between 0-360
let degree r = 
  let angle = r * (180.0 / Math.PI)
  if angle > 0.0 then angle else 360.0 + angle

let turn (x:Point2D) (y:Point2D) (z:Point2D) =
  let xy_angle = Math.Atan2((y.Y - x.Y), (y.X - x.X))
  let yz_angle = Math.Atan2((z.Y - y.Y), (z.X - y.X))
       
  if degree yz_angle >= degree xy_angle then Left else Right

let polar_sort (points:Point2D list) =
  let minp = List.minBy(fun p -> p.X) points
  List.sortBy(fun p -> Math.Atan2(p.Y - minp.Y, p.X - minp.X)) points
 
let hull (points: Point2D list) =
  let rec hull' p s =
    match p with
    | [] -> s
    | x :: xs ->
      match s with
      | a::b::ss -> 
        match turn b a x with
        | Left -> hull' xs (x::s)
        | Right -> hull' p (b::ss)
      | _ -> hull' xs (x::s)

  let sorted = polar_sort points
  hull' sorted []

module UI =
  
  open System.Windows
  
  //let points = [| for x,y in [1.0,1.0; 3.0,3.0; 4.0,6.0; 10.0,8.0; 9.0,14.0; 7.0,17.0; 5.0,15.0; 4.0,12.0; 2.0,13.0; 3.0,9.0] -> {X=x;Y=y}|]

  let pts =
        let rand = System.Random()
        [|for _ in 1..100 do
            let x, y = rand.NextDouble(), rand.NextDouble()
            if (x - 0.5) ** 2.0 + (y - 0.5) ** 2.0 < 0.25 then
              yield 300. * Vector(x, y) + Vector(100., 100.)|]

  let points = [| for v in pts -> {X=v.X;Y=v.Y}|]
  let drag : int option ref = ref None
  let size = 5.0

  //draw ellipse
  let createEllipse i _ =
    let ellipse = Shapes.Ellipse(Fill=Media.Brushes.Red, Width=size, Height=size)
    ellipse.MouseDown.Add(fun e -> drag := Some i)
    ellipse.MouseUp.Add(fun e -> drag := None)
    ellipse
  
  let move canvas update (e:Input.MouseEventArgs) =
    match !drag with
    | None -> ()
    | Some i ->
      let pos = e.GetPosition canvas
      points.[i] <- {X=pos.X; Y=pos.Y}
      update i

  let point (r:Point2D) = Windows.Point(r.X, r.Y)

  let update (poly: Shapes.Polygon) (ellipses: _ []) i =
    let p, ellipse = points.[i], ellipses.[i]
    Controls.Canvas.SetLeft(ellipse, p.X - size /2.0)
    Controls.Canvas.SetBottom(ellipse, 500.0 - p.Y - size / 2.0)
    poly.Points.Clear()

    points |> Array.toList |> hull |> List.iter (fun p -> poly.Points.Add(point(p)))

  [<System.STAThread>]
  let run =
     do
        let canvas = Controls.Canvas(Width=500.0, Height=500.0, Background=Media.Brushes.White)
        let polygon = Shapes.Polygon(Stroke=Media.Brushes.Black, StrokeThickness=1.0)
        polygon |> canvas.Children.Add |> ignore
        let ellipses = points |> Array.mapi createEllipse
        Seq.iter (canvas.Children.Add >> ignore) ellipses
        let update = update polygon ellipses
        canvas.MouseMove.Add(move canvas update)
        Seq.iter update [0..points.Length-1]
        let canvas = Controls.Viewbox(Child=canvas, Stretch=Media.Stretch.Uniform)
        Window(Content=canvas, Title="Convex hull") |> Application().Run |> ignore;;

module testing =

  open NUnit.Framework

  [<Test>]
  let should_return_convex_hull() =
    let points = [for x,y in [1.0,1.0; 3.0,3.0; 4.0,6.0; 10.0,8.0; 9.0,14.0; 7.0,17.0; 5.0,15.0; 4.0,12.0; 2.0,13.0; 3.0,9.0] -> {X=x;Y=y}]
    Assert.That(List.rev(hull points), Is.EqualTo([for x,y in [1.0,1.0; 10.0,8.0; 9.0,14.0; 7.0,17.0; 2.0,13.0] -> {X=x;Y=y}]))

  [<Test>]
  let should_sort_by_polar_angle() =
    Assert.That(polar_sort ([for x,y in [(4.0, 6.0);(6.0, 11.0);(3.0, 3.0)] -> {X=x;Y=y}]), Is.EqualTo([for x,y in [(3.0, 3.0);(6.0, 11.0);(4.0, 6.0)]-> {X=x;Y=y}]))

  [<Test>]
  let should_turn_left_1() =
    Assert.That(turn {X=1.0;Y=1.0} {X=3.0;Y=3.0} {X=4.0;Y=6.0}, Is.EqualTo(Left))
    
  [<Test>]
  let should_turn_right_2() =
    Assert.That(turn {X=3.0;Y=3.0} {X=4.0;Y=6.0} {X=10.0;Y=8.0}, Is.EqualTo(Right))  

  [<Test>]
  let should_turn_left_3() =
    Assert.That(turn {X=4.0;Y=6.0} {X=10.0;Y=8.0} {X=9.0;Y=14.0}, Is.EqualTo(Left))  

  [<Test>]
  let should_turn_left_4() =
    Assert.That(turn {X=10.0;Y=8.0} {X=9.0;Y=14.0} {X=7.0;Y=17.0}, Is.EqualTo(Left))  
  
  [<Test>]
  let should_turn_left_5() =
    Assert.That(turn {X=9.0;Y=14.0} {X=7.0;Y=17.0} {X=5.0;Y=15.0}, Is.EqualTo(Left))  
  
  [<Test>]
  let should_turn_left_6() =
    Assert.That(turn {X=7.0;Y=17.0} {X=5.0;Y=15.0} {X=4.0;Y=12.0}, Is.EqualTo(Left))

  [<Test>]
  let should_turn_right_7() =
    Assert.That(turn {X=5.0;Y=15.0} {X=4.0;Y=12.0} {X=2.0;Y=13.0}, Is.EqualTo(Right))

  [<Test>]
  let should_trun_left_8() =
    Assert.That(turn {X=4.0;Y=12.0} {X=2.0;Y=13.0} {X=3.0;Y=9.0}, Is.EqualTo(Left))
   
  [<Test>]
  let should_trun_left_9() =
    Assert.That(turn {X=2.0;Y=13.0} {X=3.0;Y=9.0} {X=1.0;Y=1.0}, Is.EqualTo(Right))
