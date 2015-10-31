package roast

import scala.math.round

/**
 First define some types - set of basic weights.
*/
abstract class Weight
case class Grams(quantity: Double) extends Weight
case class KiloGrams(quantity: Double) extends Weight
case class Ounces(quantity: Double) extends Weight
case class PoundsOunces(pounds: Int, ounces: Double) extends Weight


abstract class Meat{
	val weight : Weight
}
abstract class Poultry extends Meat; // Duck typing? I'll get me coat.

case class Beef(weight: Weight) extends Meat;
case class Lamb(weight: Weight) extends Meat;
case class Venison(weight: Weight) extends Meat;
case class Pork(weight: Weight) extends Meat

object Doneness extends Enumeration { 
	type Doneness = Value
	val Rare= Value("Rare") 
	val Medium=Value("Medium") 
	val Well=Value("Well") 
	val VeryWell=Value("VeryWell")
}

object WeightCalculator {
	def normaliseWeight(weight: Weight) : Grams = weight match{ 
		// force all weights into Grams to make life easier elsewhere.
    	case g:Grams => g
    	case k:KiloGrams => Grams(k.quantity*1000.0)
    	case o:Ounces => Grams(o.quantity*28.35)
    	case p:PoundsOunces => normaliseWeight(Grams(((p.pounds*16)+p.ounces)*28.35))
	}
}


object RoastCalculator {
	import Doneness._

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

	def calculateNormalCookingTime(animal : Meat, doneness : Doneness) : Long = {
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

	def calculateTotalCookingTimes(animal : Meat, doneness : Doneness) : (Long,Long) = {
		(calculateSizzle(animal.weight),calculateNormalCookingTime(animal,doneness))
	}


	
}
