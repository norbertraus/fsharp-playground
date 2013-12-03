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

[<System.Diagnostics.DebuggerDisplay("x={X}, y={Y}")>]
type Point2D = { X:double; Y:double }

let ccw (a:Point2D) (b:Point2D) (c:Point2D) =
  let det = (b.X-a.X)*(c.Y-a.Y) - (b.Y-a.Y)*(c.X-a.X)
  if det > 0. then 1
  elif det < 0. then -1
  else 0

//computes determinant of the two vectors xy and zx.
//if the det is zero then the vectors are colinear, 
//if det is greater than zero the turn is left otherwise the turn is right
let turn (x:Point2D) (y:Point2D) (z:Point2D) =
  match ccw x y z with
  | c when c = 0 -> Left 
  | c when c = 1 -> Left
  | _ -> Right

let polar_sort (points:Point2D list) =
  //choose extreme point with the loweset y and x
  let sorted = points |> List.sortWith (fun p1 p2 -> 
    if p1.Y < p2.Y then -1
    elif p1.Y > p2.Y then 1
    elif p1.X < p2.X then -1
    elif p1.X > p2.X then 1
    else 0) 

  let fst = List.head sorted
  
  let cmp (p1:Point2D) (p2:Point2D) =
    let dx1 = p1.X - fst.X
    let dy1 = p1.Y - fst.Y
    let dx2 = p2.X - fst.X
    let dy2 = p2.Y - fst.Y

    if dy1 >=0. && dy2 < 0. then -1 //p1-up and p2-down
    elif dy2 >=0. && dy1 < 0. then 1 //p1-down and p2-up
    elif dy1=0. && dy2=0. then
      if dx1 >= 0. && dx2 < 0. then -1
      elif dx2 >= 0. && dx1 < 0. then +1
      else 0; 
    else -(ccw fst p1 p2) 

  let s = List.tail sorted |> List.sortWith(fun p1 p2 -> cmp p1 p2) 
  fst :: s
   
let polar_sort2 (points:Point2D list) =
  //choose extreme point with the loweset y and x
  let sorted = points |> List.sortWith (fun p1 p2 -> 
    match p1.Y.CompareTo(p2.Y) with
    | 0 -> p1.X.CompareTo(p2.X)
    | _ as c -> c) 

  let refPoint, tail = sorted.Head, sorted.Tail
  
  refPoint :: (tail |> List.sortBy (fun p -> 
    let dx = p.X - refPoint.X
    let dy = p.Y - refPoint.Y
    Math.Atan2(dy, dx)))

let hull (points: Point2D list) =
  let rec hull' p s =
    match p with
    | [] -> s
    | x :: xs ->
      match s with
      | a::b::ss -> 
        //match turn b a x with
        match turn b a x with
        | Left -> hull' xs (x::s)
        | Right -> hull' p (b::ss)
      | _ -> hull' xs (x::s)

  let sorted = polar_sort2 points
  hull' sorted []

module GrahamScan =

  type Point = { X : float; Y : float }

  /// finds the points lying on the convex hull of the given set of points and 
  /// returns those points in clockwise direction, starting at the point
  /// with minimum y-value
  /// Remarks: it's a more or less direct implementation of the algorithm named
  /// after Ronald Graham that is explained on http://en.wikipedia.org/wiki/Graham_scan
  let FindConvexHull (pts : Point seq) : Point seq =

      // it's nicer to work with lists so let's convert
      let ptl = List.ofSeq pts

      // to make something worthwhile we need at last two points
      if ptl.Length <= 2 then Seq.empty
      else

      // this is a helperfunction (explained in the wikipedia article) in which direction
      // 3 points "turn"
      let ccw (a : Point) (b : Point) (c : Point) =
          (b.X - a.X)*(c.Y - a.Y) - (b.Y - a.Y)*(c.X - a.X)

      // 1. Let's find the point with the minimum y-coordinate
      /// this is the comparision function for this
      let cmpPts (a : Point) (b : Point) =
          match a.Y.CompareTo(b.Y) with
          | 0 -> a.X.CompareTo(b.X)
          | _ as r -> r
      // and with it we can look for the mentioned point
      let sortedY = ptl |> List.sortWith cmpPts
      let org, rest = sortedY.Head, sortedY.Tail

      // 2. we have to sort the list in increasing order of the angle
      // that a point p makes with org and the x-axis
      let winkelCos (p : Point) =
          let dx = float (p.X - org.X)
          let dy = float (p.Y - org.Y)
          let l = System.Math.Sqrt(dx*dx + dy*dy)
          dx / l
      // and here we sort the list (we only sort the remainder without
      // org and prepend it afterwards to ward of 
      // any issue with "division by zero"
      let sortedW = org::(rest |> List.sortBy winkelCos)

      // here is the actual algorithm
      // it uses two lists
      // lastPts: every visited point is put but might
      //          be removed if the "turn direction"
      //          'turns' out to be wrong
      // nextPts: the points left to be checked
      //          as the algorithm progresses those
      //          points are moved to lastPts
      // so lastPts will contain the found points
      // on the convex hull at every step, but in
      // clockwise orientation (as we push in front
      // of the list)
      let rec scan (lastPts : Point list) (nextPts : Point list) =
          // we are done if there are no points left to check
          if nextPts.IsEmpty then lastPts
          else

          // if there are points left take the first one
          let c = nextPts.Head

          match lastPts with
          // if there are at least 2 points b,a in the visited points
          // and a,b,c is NOT a counterclockwise turn
          // we have to remove b from lastPoints and continue checking
          // backwards ...
          | b::a::_ when ccw a b c >= 0. -> scan (lastPts.Tail) nextPts
          // in every other case we can push c onto the visited
          // stack and continue
          | _ -> scan (c::lastPts) nextPts.Tail

      // run it
      sortedW |> scan [] |> Seq.ofList

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

    points 
    |> Array.toList 
    |> hull 
    |> List.iter (fun p -> poly.Points.Add(point(p)))

//    let pp:GrahamScan.Point[]  = points |> Array.map (fun p -> {X=p.X;Y=p.Y})
//    pp 
//    |> GrahamScan.FindConvexHull 
//    |> Seq.toList 
//    |> List.iter (fun p -> poly.Points.Add(Windows.Point(p.X, p.Y)))

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
    let pp:GrahamScan.Point list  = points |> List.map (fun p -> {X=p.X;Y=p.Y})
    let convex = GrahamScan.FindConvexHull pp |> Seq.map (fun p -> {X=p.X;Y=p.Y}) |> Seq.toList |> List.rev
    let convex2 = hull points |> List.rev 
    Assert.That(convex2, Is.EqualTo([for x,y in [1.0,1.0; 10.0,8.0; 9.0,14.0; 7.0,17.0; 2.0,13.0] -> {X=x;Y=y}]))

  [<Test>]
  let should_sort_by_polar_angle() =
    Assert.That(polar_sort ([for x,y in [(4.0, 6.0);(6.0, 11.0);(3.0, 3.0)] -> {X=x;Y=y}]), Is.EqualTo([for x,y in [(3.0, 3.0);(6.0, 11.0);(4.0, 6.0)]-> {X=x;Y=y}]))

  [<Test>]
  let should_turn_left_1() =
    Assert.That(turn {X=1.0;Y=1.0} {X=3.0;Y=3.0} {X=4.0;Y=6.0}, Is.EqualTo(Left))
    Assert.That(turn {X=1.0;Y=1.0} {X=3.0;Y=3.0} {X=4.0;Y=6.0}, Is.EqualTo(Left))
    
  [<Test>]
  let should_turn_right_2() =
    Assert.That(turn {X=3.0;Y=3.0} {X=4.0;Y=6.0} {X=10.0;Y=8.0}, Is.EqualTo(Right))  
    Assert.That(turn {X=3.0;Y=3.0} {X=4.0;Y=6.0} {X=10.0;Y=8.0}, Is.EqualTo(Right))  

  [<Test>]
  let should_turn_left_3() =
    Assert.That(turn {X=4.0;Y=6.0} {X=10.0;Y=8.0} {X=9.0;Y=14.0}, Is.EqualTo(Left))  
    Assert.That(turn {X=4.0;Y=6.0} {X=10.0;Y=8.0} {X=9.0;Y=14.0}, Is.EqualTo(Left))  

  [<Test>]
  let should_turn_left_4() =
    Assert.That(turn {X=10.0;Y=8.0} {X=9.0;Y=14.0} {X=7.0;Y=17.0}, Is.EqualTo(Left))  
    Assert.That(turn {X=10.0;Y=8.0} {X=9.0;Y=14.0} {X=7.0;Y=17.0}, Is.EqualTo(Left))  
  
  [<Test>]
  let should_turn_left_5() =
    Assert.That(turn {X=9.0;Y=14.0} {X=7.0;Y=17.0} {X=5.0;Y=15.0}, Is.EqualTo(Left))  
    Assert.That(turn {X=9.0;Y=14.0} {X=7.0;Y=17.0} {X=5.0;Y=15.0}, Is.EqualTo(Left))  
  
  [<Test>]
  let should_turn_left_6() =
    Assert.That(turn {X=7.0;Y=17.0} {X=5.0;Y=15.0} {X=4.0;Y=12.0}, Is.EqualTo(Left))
    Assert.That(turn {X=7.0;Y=17.0} {X=5.0;Y=15.0} {X=4.0;Y=12.0}, Is.EqualTo(Left))

  [<Test>]
  let should_turn_right_7() =
    Assert.That(turn {X=5.0;Y=15.0} {X=4.0;Y=12.0} {X=2.0;Y=13.0}, Is.EqualTo(Right))
    Assert.That(turn {X=5.0;Y=15.0} {X=4.0;Y=12.0} {X=2.0;Y=13.0}, Is.EqualTo(Right))

  [<Test>]
  let should_trun_left_8() =
    Assert.That(turn {X=4.0;Y=12.0} {X=2.0;Y=13.0} {X=3.0;Y=9.0}, Is.EqualTo(Left))
    Assert.That(turn {X=4.0;Y=12.0} {X=2.0;Y=13.0} {X=3.0;Y=9.0}, Is.EqualTo(Left))
   
  [<Test>]
  let should_trun_left_9() =
    Assert.That(turn {X=2.0;Y=13.0} {X=3.0;Y=9.0} {X=1.0;Y=1.0}, Is.EqualTo(Right))
    Assert.That(turn {X=2.0;Y=13.0} {X=3.0;Y=9.0} {X=1.0;Y=1.0}, Is.EqualTo(Right))
