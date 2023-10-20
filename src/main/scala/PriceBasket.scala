object PriceBasket{
  // Main function
  def main(args: Array[String]) = {
    val basket1 = new Basket(args) // Initialising a new basket with items from command line
    basket1.displayPriceAndDiscount()
  }
}

trait PriceList{
  val prices = Map("Soup" -> 0.65, "Bread" -> 0.80, "Milk" -> 1.30, "Apples" -> 1.00) // In real life scenario, this value can be accessed from a persistant source
  
  // Function to get price for an item
  def getPrice(item:String): Double = {
    val price: Double = prices.getOrElse(item, -1.00)
    return price
  }
}

trait Discounts extends PriceList{
  // trait variables
  val discounts = Map("Apples" -> 0.10, "SoupAndBread" -> 0.50) // In real life scenario, this value can be accessed from a persistant source
  var totalDiscount: Double = 0.00
  var soupAndBreadDiscount: Double = 0.00
  var appleDiscount:Double = 0.00

  // Function to get discount for a given product
  def getDiscount(item:String): Double = {
    val discount: Double = discounts.getOrElse(item, -1)
    return discount
  }

  // Function to get totalDiscount
  def getTotalDiscount(): Double = totalDiscount

  // Function to apply discounts to a specific instance
  def applyDiscount(items:Array[String]) = {
    var noOfApples = items.count(_ == "Apples")
    var noOfSoups = items.count(_ == "Soup")
    var noOfBreads = items.count(_ == "Bread")
    // Logic for discount for apples
    appleDiscount = noOfApples * getPrice("Apples") * getDiscount("Apples")
    soupAndBreadDiscount = 0.0
    // Logic for discount for Soups and Bread
    if((noOfSoups / 2) <= noOfBreads){
      soupAndBreadDiscount = (noOfSoups / 2) * getPrice("Bread") * getDiscount("SoupAndBread")
    }
    else{
      soupAndBreadDiscount = noOfBreads * getPrice("Bread") * getDiscount("SoupAndBread")
    }
  
    totalDiscount = appleDiscount + soupAndBreadDiscount
  }

  // Function to display discounts to console
  def displayDiscounts() = {
    if(appleDiscount != 0.0){
      println(f"Apple 10%% off: £$appleDiscount%.2f")
    }
    if(soupAndBreadDiscount != 0.0){
      println(f"Bread 50%% off: £$soupAndBreadDiscount%.2f")
    }
    if(totalDiscount == 0.0){
      println("(No offers available)")
    }
  }
}

class Basket(var items: Array[String]) extends Discounts{
  // class variables
  private var invalidEntries = 0
  private var subTotal: Double = 0.00
  private var finalPrice: Double = 0.00

  // functions to be executed on object creation
  calculateSubtotal()
  applyDiscount(items)
  calculateTotal()

  //Funtion to get subTotal
  def getSubtotal() = subTotal

  //Function to calculate subtotal
  def calculateSubtotal() = {
    for(item <- items){
      var itemPrice: Double = getPrice(item)
      if(itemPrice == -1.00){
        invalidEntries += 1
        itemPrice = 0
      }
      subTotal = subTotal + itemPrice
    }
  }

  //Function to get finalPrice
  def getFinalPrice() = finalPrice

  //Function to calculate total price
  def calculateTotal() = {
    finalPrice = subTotal - getTotalDiscount()
  }

  // // Funtion to display invalid entries(if any)
  // def displayInvalidEntries() = {
  //   if(invalidEntries != 0)
  //     println(s"Invalid entries:$invalidEntries")
  // }

  // Function to display prices and discounts to console
  def displayPriceAndDiscount() = {
    println(f"Subtotal: £$subTotal%.2f")
    displayDiscounts()
    println(f"Total price: £$finalPrice%.2f")
    // displayInvalidEntries()
  }
}
