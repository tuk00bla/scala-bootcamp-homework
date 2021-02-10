object FourthHomework {

  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val vegetablePrices = Map(
    "tomatoes" -> 4,
    "peppers" -> 5,
    "olives" -> 17,
  )

  val vegetableAmounts = Map(
    "tomatoes" -> 17,
    "peppers" -> 234,
    "olives" -> 32,
    "cucumbers" -> 323,
  )

  val totalVegetableWeights: Map[String, Int] = {
    for {
      (vegetable, weight) <- vegetableWeights
      count <- vegetableAmounts.get(vegetable)
    } yield (vegetable, count * weight)
  }

  val totalVegetableCost: Int = {
    (for {
      (vegetable, amount) <- vegetableAmounts
      price = vegetablePrices.getOrElse(vegetable, 10) * amount
    } yield price).sum
  }

  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = {
    if (n == 1)
      set.map(Set(_))
    else
      for {
        elem   <- set
        subset <- allSubsetsOfSizeN(set - elem, n - 1)
      } yield  subset + elem
  }

  def sortConsideringEqualValues[T](map: Map[T, Int]): Seq[(Set[T], Int)] = {
    (for {
      (id, innerMap) <- map.groupBy { case (_, id) => id }
      string = innerMap.keySet
    } yield string -> id).toSeq.sortBy { case (_, id) => id }
  }

  def runningSumOfArray(nums: Array[Int]): Array[Int] = {
    nums.foldLeft(Array.empty[Int])((acc, x) => acc :+ x + acc.lastOption.getOrElse(0))
  }

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.foldLeft(Array.empty[Boolean])((res, candy) =>
      if (candies.forall(_ <= candy + extraCandies)) res :+ true else res :+ false)
  }
}
