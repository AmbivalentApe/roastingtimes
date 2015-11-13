package pe.ambivalenta.roast.model

import scala.math.round
import pe.ambivalenta.roast.model.types._

object Calculator {
	import Doneness._
	import WeightUnit._

	def normaliseWeight(weight: Weight) : Weight = weight.units match{ 
		// force all weights into Grams to make life easier elsewhere.
    		case WeightUnit.Grams => weight
    		case WeightUnit.Kilograms => Weight(weight.quantity*1000.0,WeightUnit.Grams)
    		case WeightUnit.Ounces => Weight(weight.quantity*28.35,WeightUnit.Grams)
    		case WeightUnit.Pounds => Weight(weight.quantity*16*28.35,WeightUnit.Grams)
    	
	}



	def animals() : Vector[String] = Vector("Beef","Chicken","Lamb","Pork","Venison","Turkey")
	def calculateSizzle(weight : Weight) : Long = {
		

		//'Sizzle' time on high heat (~220c). This will eventually need to support Animal as an additional
		//parameter as we want deal with poultry. This time isn't linear with size of the joint being roasted,
		//so we cap it at the extremes

		
		val scaled = normaliseWeight(weight).quantity/100.0
		scaled match {
			case v  if(scaled < 20.0) => 20
			case w  if(scaled > 40.0) => 40
			case _ => round(scaled)
		}
	}

	def calculateNormalCookingTime(animal : Animal, doneness : Doneness) : Long = {
		//Calculate the 'main' longer slower roast - e.g. at 180 c

		val weightInGrams = normaliseWeight(animal.weight).quantity
		val isBig = if(weightInGrams >= 5000) true else false // we want smaller cooking times for larger joints to prevent them from being toast.
		round((weightInGrams/500.0) * (animal match {
			case p:Pork => doneness match{
							case Doneness.Well => 25.0
							case _ => 0.0 // Undercooking pork is probably a bad idea.
						     }				
			case c:Poultry => c match {
							case k:Chicken => 15.0
							case t:Turkey => 20.0

						 }
			case _ => doneness match {
							case Doneness.Rare => if(isBig){9.0} else 10.0
							case Doneness.Medium => if(isBig){12.0} else 15.0
							case Doneness.Well => if(isBig){18.0} else 20.0
						}
		}))}

	def calculateTotalCookingTimes(animal : Animal, doneness : Doneness) : (Long,Long) = {
		val sizzle = animal match {
			case p:Poultry => 20
			case _ => calculateSizzle(animal.weight)
		}
		(sizzle,calculateNormalCookingTime(animal,doneness))
	}

	def convertToFahrenheit(temp : Double) : Double = round(temp * (9.0/5.0) + 32.0)

	
}
