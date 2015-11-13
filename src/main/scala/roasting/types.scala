package pe.ambivalenta.roast.model.types

/**
 First define some types - set of basic weights.
*/

object WeightUnit extends Enumeration{
	type WeightUnit = Value
	val Grams = Value("Grams")
	val Kilograms = Value("Kilograms")
	val Ounces = Value("Ounces")
	val Pounds = Value("Pounds")
}
import WeightUnit._
case class Weight(quantity:Double, units:WeightUnit)



trait Animal{
	val weight : Weight
	val lowCookingTemp : Int = 160
	val highCookingTemp : Int = 230
}

trait Poultry extends Animal
{
	override val lowCookingTemp : Int = 180

}

trait WhiteMeat extends Animal {
	override val highCookingTemp : Int = 210	
}


case class Beef(weight: Weight) extends Animal
case class Lamb(weight: Weight) extends Animal
case class Venison(weight: Weight) extends Animal
case class Pork(weight: Weight) extends Animal

case class Chicken(weight: Weight) extends Animal with Poultry with WhiteMeat
case class Turkey(weight: Weight) extends Animal with Poultry with WhiteMeat


object Doneness extends Enumeration { 
	type Doneness = Value
	val Rare= Value("Rare") 
	val Medium=Value("Medium") 
	val Well=Value("Well")
}
