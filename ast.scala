// (Tuple3.apply[Int, NamedTuple[Tuple2["wow", "wow2"], Tuple2[Int, Int]], Int](
//   Tuples
//     .valuesOf[Tuple3["name", "nested", "newField"], Tuple3[Int, NamedTuple[Tuple2["wow", "wow2"], Tuple2[Int, Int]], Int]](easy)
//     ._1: Int,
//   Tuple2.apply[Int, Int](
//     Tuples
//       .valuesOf[Tuple2["wow", "wow2"], Tuple2[Int, Int]](
//         Tuples
//           .valuesOf[Tuple3["name", "nested", "newField"], Tuple3[Int, NamedTuple[Tuple2["wow", "wow2"], Tuple2[Int, Int]], Int]](
//             easy
//           )
//           ._2: NamedTuple[Tuple2["wow", "wow2"], Tuple2[Int, Int]]
//       )
//       ._1: Int,
//     Tuples
//       .valuesOf[Tuple2["wow", "wow2"], Tuple2[Int, Int]](
//         Tuples
//           .valuesOf[Tuple3["name", "nested", "newField"], Tuple3[Int, NamedTuple[Tuple2["wow", "wow2"], Tuple2[Int, Int]], Int]](
//             easy
//           )
//           ._2: NamedTuple[Tuple2["wow", "wow2"], Tuple2[Int, Int]]
//       )
//       ._2: Int
//   ): NamedTuple[Tuple2["wow", "wow2"], Tuple2[Int, Int]],
//   Tuples
//     .valuesOf[Tuple1["newField"], Tuple1[Int]](NamedTuple.build[Tuple1["newField"]]()[Tuple1[Int]](Tuple1.apply[Int](123)))
//     ._1: Int
// ): NamedTuple[Tuple3["name", "nested", "newField"], Tuple3[Int, NamedTuple[Tuple2["wow", "wow2"], Tuple2[Int, Int]], Int]])
