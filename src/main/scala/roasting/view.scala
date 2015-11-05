package pe.ambivalenta.roast.view.reactjs

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import org.scalajs.dom

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSApp

import pe.ambivalenta.roast.model._
import pe.ambivalenta.roast.model.Doneness._

import org.scalajs.dom.document


object Screen extends JSApp{

    case class State(animal: Animal,doneness:Doneness, times:(Long,Long)){

    	def onNewAnimal(newAnimal : Animal): State = 
    		State(newAnimal,doneness,RoastCalculator.calculateTotalCookingTimes(newAnimal,doneness))

    	def onNewDoneness(newDoneness : Doneness) : State = State(animal,newDoneness,
            RoastCalculator.calculateTotalCookingTimes(animal,newDoneness))
   
    }

    class Backend($: BackendScope[Unit, State]){

        def onChangeWeight(e: ReactEventI) = 
            $.modState(state => state.onNewAnimal(newAnimal(state.animal.getClass.getSimpleName,
                                          WeightCalculator.normaliseWeight(Grams(e.target.value.toDouble)))))

        def onChangeDoneness(e: ReactEventI) =
            $.modState(state => state.onNewDoneness(Doneness.withName(e.target.value)))

        def onChangeAnimal(e:ReactEventI) = 
            $.modState(state => state.onNewAnimal(newAnimal(e.target.value,state.animal.weight)))
                

        def newAnimal(name:String,weight:Weight) =
            name match {
                case p if p=="Pork" => Pork(weight)
                case l if l=="Lamb" => Lamb(weight)
                case v if v=="Venison" => Venison(weight)
                case b if b=="Beef" => Beef(weight)
            }
        

        def render(state: State) = {
            val s= state
            //<.div(state.animal.weight);
            <.div(
                <.p(<.label("Animal:"),
                    <.select(RoastCalculator.animals().map(v=> <.option(^.key:=v,^.value:=v)(v)),
                            ^.value := s.animal.getClass.getSimpleName,
                            ^.onChange ==> onChangeAnimal)
                    ),
                <.p(<.label("Weight:"),
                        <.input(
                            ^.placeholder := "S",
                            ^.value       := WeightCalculator.normaliseWeight(s.animal.weight).quantity,
                            ^.onChange    ==> onChangeWeight
                            )),
                <.p(<.label("Doneness:"),
                        <.select( Doneness.values.map(v => <.option(^.key:=v.toString,^.value:=v.toString)(v.toString)),
                            ^.onChange ==> onChangeDoneness,
                            ^.value := s.doneness.toString
                          )
            
                ),

                s.times._2 match {
                    case z if z ==0 => <.p("That seems unwise")
                    case _ => <.p(s"Sizzle for ${s.times._1} minutes at 220 degrees c, then turn down the heat to 160 and roast for ${s.times._2} minutes")
                }
            )
                
        }
            
        
    }

    val Animals = ReactComponentB[Vector[scala.Any]]("Animals")
        .render_P {
            s => <.label(Beef.getClass.getSimpleName)
        }
        .build


    val ScreenApp = ReactComponentB[Unit]("Screen")
        .initialState(State(Beef(Grams(2500)),Doneness.Medium,
            RoastCalculator.calculateTotalCookingTimes(Beef(Grams(2500)),Doneness.Medium)))
        .renderBackend[Backend]
        .buildU
    

	@JSExport
  	override def main(): Unit = {
        ReactDOM.render(ScreenApp(), document.getElementById("container"))
  	}


}