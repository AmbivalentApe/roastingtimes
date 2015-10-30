
package roast.tests
import utest._

import roast.KiloGrams
import roast.Grams
import roast.Ounces
import roast.PoundsOunces
import roast.WeightCalculator
import roast.RoastCalculator
import roast.Doneness._
import roast.Beef
import roast.Lamb
import roast.Venison
import roast.Pork
import scala.math.round

object WeightTests extends TestSuite{
	val tests = TestSuite{
		'normalise_grams{
			val in = Grams(2.3)
			assert(WeightCalculator.normaliseWeight(in)==Grams(2.3))

		}
		'normalise_kilos{
			val in = KiloGrams(1.9)
			assert(WeightCalculator.normaliseWeight(in)==Grams(1900.0))

		}
		'normalise_ounces{
			val in = Ounces(1)
			assert(WeightCalculator.normaliseWeight(in)==Grams(28.35))

		}
		'normalise_pounds_ounces{
			val in = PoundsOunces(1,13)
			val k = WeightCalculator.normaliseWeight(in)
			assert(round(k.quantity)==822)
			

		}

	}	
}


object SizzleTests extends TestSuite{
	val tests = TestSuite{
		'sizzle_floor{
			val in = Grams(1999)
			assert(RoastCalculator.calculateSizzle(in)==20)

		}
		'sizzle_cap{
			val in = Grams(4001)
			assert(RoastCalculator.calculateSizzle(in)==40)

		}
		'sizzle_default{
			val in = Grams(2300)
			assert(RoastCalculator.calculateSizzle(in)==23)
		}
		'sizzle_convert{
			val in = Ounces(88)
			assert(RoastCalculator.calculateSizzle(in)==25)

		}
	}
}


object BeefTests extends TestSuite{
	val tests = TestSuite{
		'main_rare_regular{
			val in = Beef(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Rare)==40)
		}
		'main_rare_large{
			val in = Beef(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Rare)==90)
		}
		'main_medium_regular{
			val in = Beef(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Medium)==60)
		}
		'main_medium_large{
			val in = Beef(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Medium)==120)
		}
		'main_well_regular{
			val in = Beef(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Well)==80)
		}
		'main_well_large{
			val in = Beef(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Well)==180)
		}
		'main_very_well_regular{
			val in = Beef(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.VeryWell)==0)
		}
		'main_very_well_large{
			val in = Beef(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.VeryWell)==0)
		}

	}

}

object LambTests extends TestSuite{
	val tests = TestSuite{
		'main_rare_regular{
			val in = Lamb(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Rare)==40)
		}
		'main_rare_large{
			val in = Lamb(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Rare)==90)
		}
		'main_medium_regular{
			val in = Lamb(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Medium)==60)
		}
		'main_medium_large{
			val in = Lamb(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Medium)==120)
		}
		'main_well_regular{
			val in = Lamb(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Well)==80)
		}
		'main_well_large{
			val in = Lamb(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Well)==180)
		}
		'total_well_large{
			val in = Lamb(Grams(5000.0))
			println(RoastCalculator.calculateTotalCookingTimes(in,roast.Doneness.Well))
			
			assert(RoastCalculator.calculateTotalCookingTimes(in,roast.Doneness.Well)==(40,180))
		}
		'main_very_well_regular{
			val in = Lamb(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.VeryWell)==0)
		}
		'main_very_well_large{
			val in = Lamb(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.VeryWell)==0)
		}


	}

}

object VensionTests extends TestSuite{
	val tests = TestSuite{
		'main_rare_regular{
			val in = Venison(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Rare)==40)
		}
		'main_rare_large{
			val in = Venison(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Rare)==90)
		}
		'main_medium_regular{
			val in = Venison(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Medium)==60)
		}
		'main_medium_large{
			val in = Venison(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Medium)==120)
		}
		'main_well_regular{
			val in = Venison(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Well)==80)
		}
		'main_well_large{
			val in = Venison(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Well)==180)
		}
		'main_very_well_regular{
			val in = Venison(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.VeryWell)==0)
		}
		'main_very_well_large{
			val in = Venison(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.VeryWell)==0)
		}

	}

}

object PorkTests extends TestSuite{
	val tests = TestSuite{
		'main_rare_regular{
			val in = Pork(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Rare)==0)//unwise
		}
		'main_rare_large{
			val in = Pork(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Rare)==0)//unwise
		}
		'main_medium_regular{
			val in = Pork(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Medium)==0)//unwise
		}
		'main_medium_large{
			val in = Pork(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Medium)==0)//unwise
		}
		'main_well_regular{
			val in = Pork(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Well)==100)
		}
		'main_well_large{
			val in = Pork(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.Well)==250)//cooking time constant irrespective of joint size
		}
		'main_very_well_regular{
			val in = Pork(Grams(2000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.VeryWell)==120)
		}
		'main_very_well_large{
			val in = Pork(Grams(5000.0))
			assert(RoastCalculator.calculateNormalCookingTime(in,roast.Doneness.VeryWell)==300) //cooking time constant irrespective of joint size
		}

	}

}

