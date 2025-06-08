// (Tuple3.apply[
//   _1.type,
//   _2.type,
//   Option[NamedTuple[
//     Tuple2["something", "tup"],
//     Tuple2[
//       NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//       Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//     ]
//   ]]
// ](
//   Tuples
//     .valuesOf[
//       Tuple3["name", "age", "other"],
//       Tuple3[
//         Int,
//         Int,
//         Option[NamedTuple[
//           Tuple2["something", "tup"],
//           Tuple2[
//             NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//             Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//           ]
//         ]]
//       ]
//     ](a)
//     ._1,
//   Tuples
//     .valuesOf[
//       Tuple3["name", "age", "other"],
//       Tuple3[
//         Int,
//         Int,
//         Option[NamedTuple[
//           Tuple2["something", "tup"],
//           Tuple2[
//             NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//             Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//           ]
//         ]]
//       ]
//     ](a)
//     ._2,
//   Tuples
//     .valuesOf[
//       Tuple3["name", "age", "other"],
//       Tuple3[
//         Int,
//         Int,
//         Option[NamedTuple[
//           Tuple2["something", "tup"],
//           Tuple2[
//             NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//             Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//           ]
//         ]]
//       ]
//     ](a)
//     ._3
//     .map[NamedTuple[
//       Tuple2["something", "tup"],
//       Tuple2[
//         NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//         Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//       ]
//     ]](
//       (a: NamedTuple[
//         Tuple2["something", "tup"],
//         Tuple2[
//           NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//           Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//         ]
//       ]) =>
//         Tuple2.apply[NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]], Tuple2[
//           _1,
//           NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]
//         ]](
//           Tuple2.apply[_1.type, _2.type](
//             Tuples
//               .valuesOf[Tuple2["name", "age"], Tuple2[Int, Int]](
//                 Tuples
//                   .valuesOf[
//                     Tuple2["something", "tup"],
//                     Tuple2[
//                       NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//                       Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//                     ]
//                   ](a)
//                   ._1
//               )
//               ._1,
//             Tuples
//               .valuesOf[Tuple2["name", "age"], Tuple2[Int, Int]](
//                 Tuples
//                   .valuesOf[
//                     Tuple2["something", "tup"],
//                     Tuple2[
//                       NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//                       Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//                     ]
//                   ](a)
//                   ._1
//               )
//               ._2
//           ): NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//           Tuple2.apply[_1.type, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]](
//             Tuples
//               .valuesOf[
//                 Tuple2["something", "tup"],
//                 Tuple2[
//                   NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//                   Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//                 ]
//               ](a)
//               ._2
//               ._1,
//             Tuple2.apply[_1.type, _2.type](
//               Tuples
//                 .valuesOf[Tuple2["name123", "name1"], Tuple2[Int, Int]](
//                   Tuples
//                     .valuesOf[
//                       Tuple2["something", "tup"],
//                       Tuple2[
//                         NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//                         Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//                       ]
//                     ](a)
//                     ._2
//                     ._2
//                 )
//                 ._1,
//               Tuples
//                 .valuesOf[Tuple2["name123", "name1"], Tuple2[Int, Int]](
//                   Tuples
//                     .valuesOf[
//                       Tuple2["something", "tup"],
//                       Tuple2[
//                         NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//                         Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//                       ]
//                     ](a)
//                     ._2
//                     ._2
//                 )
//                 ._2
//             ): NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]
//           )
//         ): NamedTuple[
//           Tuple2["something", "tup"],
//           Tuple2[
//             NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//             Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//           ]
//         ]
//     )
// ): NamedTuple[
//   Tuple3["name", "age", "other"],
//   Tuple3[
//     Int,
//     Int,
//     Option[NamedTuple[
//       Tuple2["something", "tup"],
//       Tuple2[
//         NamedTuple[Tuple2["name", "age"], Tuple2[Int, Int]],
//         Tuple2[String, NamedTuple[Tuple2["name123", "name1"], Tuple2[Int, Int]]]
//       ]
//     ]]
//   ]
// ])
