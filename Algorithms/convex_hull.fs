module convexhull
open System

type Turn =
  | Left
  | Right

//translates from radians to degrees between 0-360
let degree r = 
  let angle = r * (180.0 / Math.PI)
  if angle > 0.0 then angle else 360.0 + angle

let turn (x:double*double) (y:double*double) (z:double*double) =
  let xy_angle = Math.Atan2((snd y - snd x), (fst y - fst x))
  let yz_angle = Math.Atan2((snd z - snd y), (fst z - fst y))
       
  if degree yz_angle >= degree xy_angle then Left else Right

let polar_sort (points:(double*double) list) =
  let minp = List.minBy(fun p -> fst p) points
  List.sortBy(fun p -> Math.Atan2(snd p - snd minp, fst p - snd minp)) points
 
let hull (points: (double*double) list) =
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

module testing =

  open NUnit.Framework

  [<Test>]
  let should_return_convex_hull() =
    let points = [(1.0,1.0);(3.0,3.0);(4.0,6.0);(10.0,8.0);(9.0,14.0);(7.0,17.0);(5.0,15.0);(4.0,12.0);(2.0,13.0);(3.0,9.0)]
    Assert.That(List.rev(hull points), Is.EqualTo([(1.0,1.0);(10.0,8.0);(9.0,14.0);(7.0,17.0);(2.0,13.0)]))

  [<Test>]
  let should_sort_by_polar_angle() =
    Assert.That(polar_sort ([(4.0, 6.0);(6.0, 11.0);(3.0, 3.0)]), Is.EqualTo([(3.0, 3.0);(6.0, 11.0);(4.0, 6.0)]))

  [<Test>]
  let should_turn_left_1() =
    Assert.That(turn (1.0, 1.0) (3.0, 3.0) (4.0, 6.0), Is.EqualTo(Left))
    
  [<Test>]
  let should_turn_right_2() =
    Assert.That(turn (3.0, 3.0) (4.0, 6.0) (10.0, 8.0), Is.EqualTo(Right))  

  [<Test>]
  let should_turn_left_3() =
    Assert.That(turn (4.0, 6.0) (10.0, 8.0) (9.0, 14.0), Is.EqualTo(Left))  

  [<Test>]
  let should_turn_left_4() =
    Assert.That(turn (10.0, 8.0) (9.0, 14.0) (7.0, 17.0), Is.EqualTo(Left))  
  
  [<Test>]
  let should_turn_left_5() =
    Assert.That(turn (9.0, 14.0) (7.0, 17.0) (5.0, 15.0), Is.EqualTo(Left))  
  
  [<Test>]
  let should_turn_left_6() =
    Assert.That(turn (7.0, 17.0) (5.0, 15.0) (4.0, 12.0), Is.EqualTo(Left))

  [<Test>]
  let should_turn_right_7() =
    Assert.That(turn (5.0, 15.0) (4.0, 12.0) (2.0, 13.0), Is.EqualTo(Right))

  [<Test>]
  let should_trun_left_8() =
    Assert.That(turn (4.0, 12.0) (2.0, 13.0) (3.0, 9.0), Is.EqualTo(Left))
   
  [<Test>]
  let should_trun_left_9() =
    Assert.That(turn (2.0, 13.0) (3.0, 9.0) (1.0, 1.0), Is.EqualTo(Right))
   