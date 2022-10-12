package book.red_fp.ch1

private case class CreditCard(id: String)
private case class Coffee(price: Double)
private case class Charge(cc: CreditCard, amount: Double):
  def combine(other: Charge): Charge =
    if cc == other.cc then other.copy(amount = amount + other.amount)
    else throw new Exception("Can't combine charges to different cards")

private class Cafe:
  def buyCoffee(cc: CreditCard): (Coffee, Charge) =
    val coffee = Coffee(1200)
    (coffee, Charge(cc, coffee.price))

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) =
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce(_ combine _))

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
