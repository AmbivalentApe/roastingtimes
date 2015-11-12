package pe.ambivalenta.roast.model

import scala.math.round

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



abstract class Animal{
	val weight : Weight
}
abstract class Poultry extends Animal; // Duck typing? I'll get me coat.

case class Beef(weight: Weight) extends Animal
case class Lamb(weight: Weight) extends Animal
case class Venison(weight: Weight) extends Animal
case class Pork(weight: Weight) extends Animal


object Doneness extends Enumeration { 
	type Doneness = Value
	val Rare= Value("Rare") 
	val Medium=Value("Medium") 
	val Well=Value("Well") 
	val VeryWell=Value("Very Well")
}

object Heat extends Enumeration {
	/*
	* This is a bit lazy, and could of course by converted on the fly, but it's easier to
	* just have fixed values and round them as we like
	*/
	type Heat = Value
	val HighC = Value("220")
	val LowC = Value("180")

	val HighCFan = Value("200")
	val LowCFan = Value("160")

	val HighF = Value("400")
	val LowF = Value("360")


}

object WeightCalculator {
	import WeightUnit._
	def normaliseWeight(weight: Weight) : Weight = weight.units match{ 
		// force all weights into Grams to make life easier elsewhere.
    	case WeightUnit.Grams => weight
    	case WeightUnit.Kilograms => Weight(weight.quantity*1000.0,WeightUnit.Grams)
    	case WeightUnit.Ounces => Weight(weight.quantity*28.35,WeightUnit.Grams)
    	case WeightUnit.Pounds => Weight(weight.quantity*16*28.35,WeightUnit.Grams)
    	
	}
}



object RoastCalculator {
	import Doneness._
	def animals() : Vector[String] = Vector("Pork","Beef","Lamb","Venison")
	def calculateSizzle(weight : Weight) : Long = {
		

		//'Sizzle' time on high heat (~220c). This will eventually need to support Animal as an additional
		//parameter as we want deal with poultry. This time isn't linear with size of the joint being roasted,
		//so we cap it at the extremes

		
		val scaled = WeightCalculator.normaliseWeight(weight).quantity/100.0
		scaled match {
			case v  if(scaled < 20.0) => 20
			case w  if(scaled > 40.0) => 40
			case _ => round(scaled)
		}
	}

	def calculateNormalCookingTime(animal : Animal, doneness : Doneness) : Long = {
		//Calculate the 'main' longer slower roast - e.g. at 180 c

		val weightInGrams = WeightCalculator.normaliseWeight(animal.weight).quantity
		val isBig = if(weightInGrams >= 5000) true else false // we want smaller cooking times for larger joints to prevent them from being toast.
		round((weightInGrams/500.0) * (animal match {
			case p:Pork => doneness match{
							case Doneness.Well => 25.0
							case Doneness.VeryWell => 30.0
							case _ => 0.0 // Undercooking pork is probably a bad idea.
						}
			case _ => doneness match {
							case Doneness.Rare => if(isBig){9.0} else 10.0
							case Doneness.Medium => if(isBig){12.0} else 15.0
							case Doneness.Well => if(isBig){18.0} else 20.0
							case _ => 0.0 // WHY WHY WOULD YOU DO THIS
						}
		}))}

	def calculateTotalCookingTimes(animal : Animal, doneness : Doneness) : (Long,Long) = {
		(calculateSizzle(animal.weight),calculateNormalCookingTime(animal,doneness))
	}


	
}
