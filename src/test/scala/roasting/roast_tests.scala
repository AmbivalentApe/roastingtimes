
package pe.ambivalenta.roast.model.tests
import utest._

import pe.ambivalenta.roast.model.WeightUnit._
import pe.ambivalenta.roast.model.Weight
import pe.ambivalenta.roast.model.WeightCalculator
import pe.ambivalenta.roast.model.RoastCalculator
import pe.ambivalenta.roast.model.Doneness._
import pe.ambivalenta.roast.model.Beef
import pe.ambivalenta.roast.model.Lamb
import pe.ambivalenta.roast.model.Venison
import pe.ambivalenta.roast.model.Pork
import scala.math.round

object WeightTests extends TestSuite{
	val tests = TestSuite{
		'normalise_grams{
			val in = Weight(2.3,pe.ambivalenta.roast.model.WeightUnit.Grams)
			assert(WeightCalculator.normaliseWeight(in)==Weight(2.3,pe.ambivalenta.roast.model.WeightUnit.Grams))

		}
		'normalise_kilos{
			val in = Weight(1.9,pe.ambivalenta.roast.model.WeightUnit.Kilograms)
			assert(WeightCalculator.normaliseWeight(in)==Weight(1900.0,pe.ambivalenta.roast.model.WeightUnit.Grams))

		}
		'normalise_ounces{
			val in = Weight(1.0,pe.ambivalenta.roast.model.WeightUnit.Ounces)
			assert(WeightCalculator.normaliseWeight(in)==Weight(28.35,pe.ambivalenta.roast.model.WeightUnit.Grams))

		}
		'normalise_pounds_ounces{
			val in = Weight(1.8125,pe.ambivalenta.roast.model.WeightUnit.Pounds)
			val k = WeightCalculator.normaliseWeight(in)
			assert(round(k.quantity)==822)
			

		}

	}	
}


object SizzleTests extends TestSuite{
	val tests = TestSuite{
		'sizzle_floor{
			val in = Weight(1999,pe.ambivalenta.roast.model.WeightUnit.Grams)
			assert(RoastCalculator.calculateSizzle(in)==20)

		}
		'sizzle_cap{
			val in = Weight(4001,pe.ambivalenta.roast.model.WeightUnit.Grams)
			assert(RoastCalculator.calculateSizzle(in)==40)

		}
		'sizzle_default{
			val in = Weight(2300,pe.ambivalenta.roast.model.WeightUnit.Grams)
			assert(RoastCalculator.calculateSizzle(in)==23)
		}
		'sizzle_convert{
			val in = Weight(88,pe.ambivalenta.roast.model.WeightUnit.Ounces)
			assert(RoastCalculator.calculateSizzle(in)==25)

		}
	}
}


object BeefTests extends TestSuite{
	val tests = TestSuite{
		'main_rare_regular{
			val in = Beef(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Rare)==40)
		}
		'main_rare_large{
			val in = Beef(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Rare)==90)
		}
		'main_medium_regular{
			val in = Beef(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Medium)==60)
		}
		'main_medium_large{
			val in = Beef(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Medium)==120)
		}
		'main_well_regular{
			val in = Beef(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Well)==80)
		}
		'main_well_large{
			val in = Beef(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Well)==180)
		}
		'main_very_well_regular{
			val in = Beef(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.VeryWell)==0)
		}
		'main_very_well_large{
			val in = Beef(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.VeryWell)==0)
		}

	}

}

object LambTests extends TestSuite{
	val tests = TestSuite{
		'main_rare_regular{
			val in = Lamb(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Rare)==40)
		}
		'main_rare_large{
			val in = Lamb(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Rare)==90)
		}
		'main_medium_regular{
			val in = Lamb(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Medium)==60)
		}
		'main_medium_large{
			val in = Lamb(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Medium)==120)
		}
		'main_well_regular{
			val in = Lamb(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Well)==80)
		}
		'main_well_large{
			val in = Lamb(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Well)==180)
		}
		'total_well_large{
			val in = Lamb(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			println(RoastCalculator.calculateTotalCookingTimes(in, pe.ambivalenta.roast.model.Doneness.Well))
			
			assert(RoastCalculator.calculateTotalCookingTimes(in, pe.ambivalenta.roast.model.Doneness.Well)==(40,180))
		}
		'main_very_well_regular{
			val in = Lamb(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.VeryWell)==0)
		}
		'main_very_well_large{
			val in = Lamb(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.VeryWell)==0)
		}


	}

}

object VensionTests extends TestSuite{
	val tests = TestSuite{
		'main_rare_regular{
			val in = Venison(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Rare)==40)
		}
		'main_rare_large{
			val in = Venison(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Rare)==90)
		}
		'main_medium_regular{
			val in = Venison(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Medium)==60)
		}
		'main_medium_large{
			val in = Venison(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Medium)==120)
		}
		'main_well_regular{
			val in = Venison(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Well)==80)
		}
		'main_well_large{
			val in = Venison(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Well)==180)
		}
		'main_very_well_regular{
			val in = Venison(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.VeryWell)==0)
		}
		'main_very_well_large{
			val in = Venison(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.VeryWell)==0)
		}

	}

}

object PorkTests extends TestSuite{
	val tests = TestSuite{
		'main_rare_regular{
			val in = Pork(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Rare)==0)//unwise
		}
		'main_rare_large{
			val in = Pork(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Rare)==0)//unwise
		}
		'main_medium_regular{
			val in = Pork(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Medium)==0)//unwise
		}
		'main_medium_large{
			val in = Pork(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Medium)==0)//unwise
		}
		'main_well_regular{
			val in = Pork(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Well)==100)
		}
		'main_well_large{
			val in = Pork(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.Well)==250)//cooking time constant irrespective of joint size
		}
		'main_very_well_regular{
			val in = Pork(Weight(2000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.VeryWell)==120)
		}
		'main_very_well_large{
			val in = Pork(Weight(5000.0,pe.ambivalenta.roast.model.WeightUnit.Grams))
			assert(RoastCalculator.calculateNormalCookingTime(in, pe.ambivalenta.roast.model.Doneness.VeryWell)==300) //cooking time constant irrespective of joint size
		}

	}

}

