import org.scalatest.funsuite.AnyFunSuite

class PriceBasketTest extends AnyFunSuite{


  val items = Array("Apples", "Bread", "Soup", "Soup", "Milk")
  // test 1
  test("PriceList.getPrice") {
    val basket1 = new Basket(items)
    val result1 = basket1.getPrice("Apples")
    assert(result1 == 1.00)
    val result2 = basket1.getPrice("Milk")
    assert(result2 == 1.30)
  }

  // test 2
  test("Discounts.getDiscount") {
    val basket1 = new Basket(items)
    val result1 = basket1.getDiscount("Apples")
    assert(result1 == 0.10)
    val result2 = basket1.getDiscount("SoupAndBread")
    assert(result2 == 0.50)
  }

  // test 3
  test("Discounts.getTotalDiscount") {
    // When discount is there for bread and apples
    val basket1 = new Basket(items)
    val result1 = basket1.getTotalDiscount()
    assert(result1 == 0.50)

    // When there are multiple pairs of soups and only 1 bread (only one bread discount should be applied)
    val altItems1 = Array("Bread", "Soup", "Soup", "Soup", "Soup" ,"Milk")
    val basket2 = new Basket(altItems1)
    val result2 = basket2.getTotalDiscount()
    assert(result2 == 0.40)

    // When there are many breads but only one pair of soups (only one bread discount should be applied)
    val altItems2 = Array("Bread", "Bread", "Bread", "Soup", "Soup", "Milk")
    val basket3 = new Basket(altItems2)
    val result3 = basket3.getTotalDiscount()
    assert(result3 == 0.40)

    // When there are many breads and many pair of soups (here two times discount should be applied for bread)
    val altItems3 = Array("Bread", "Bread", "Bread", "Soup", "Soup", "Soup", "Soup", "Milk")
    val basket4 = new Basket(altItems3)
    val result4 = basket4.getTotalDiscount()
    assert(result4 == 0.80)

    // When there are no discounts
    val altItems4 = Array("Bread", "Bread", "Soup", "Milk")
    val basket5 = new Basket(altItems4)
    val result5 = basket5.getTotalDiscount()
    assert(result5 == 0.00)
  }

  // test 4
  test("Basket.getSubtotal"){
    val basket1 = new Basket(items)
    val result = basket1.getSubtotal()
    assert(result == 4.40)
  }

  // test 5
  test("Basket.getFinalPrice"){
    val basket1 = new Basket(items)
    val result = basket1.getFinalPrice()
    assert(result == 3.9000000000000004)
  }

}