package usage

import scaladays._

// @mappable trait A

// @mappable class B {
//   @mappable val bVal = "hi there"
// }

// @mappable case class Customer(i: Int, s: String)

// @mappable object D {
//   @mappable type Inner = Int
//   @mappable val inner: Inner = 5
// }

// class E(@mappable i: Int)

@codegen object MyDomain

object Usage extends App {
  import MyDomain._
  import spray.json._
  import DefaultJsonProtocol._

  val myPrice = Price(10, 100)
  val myItem = Item(diameter = 10, weight = 100, price = myPrice)

  val c = Customer(
    name = "Michael",
    yearOfBirth = 1983,
    item = myItem,
    itemo = Some(myItem),
    itemList = Seq(myItem, myItem),
    imap = Map("k1" -> myItem, "k2" -> myItem)
  )

  val c2 = c.toJson.toString.parseJson.convertTo[Customer]

  println(c2)
}
