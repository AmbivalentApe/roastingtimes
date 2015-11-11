package pe.ambivalenta.roast.view.reactjs

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import org.scalajs.dom

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSApp

import pe.ambivalenta.roast.model._
import pe.ambivalenta.roast.model.Doneness._
import pe.ambivalenta.roast.model.Heat._
import pe.ambivalenta.roast.model.WeightUnit._


import org.scalajs.dom.document


object Screen extends JSApp{

    case class State(animal: Animal,doneness:Doneness, times:(Long,Long)){

    	def onNewAnimal(newAnimal : Animal): State = 
    		State(newAnimal,doneness,RoastCalculator.calculateTotalCookingTimes(newAnimal,doneness))

    	def onNewDoneness(newDoneness : Doneness) : State = State(animal,newDoneness,
            RoastCalculator.calculateTotalCookingTimes(animal,newDoneness))
   
    }

    class Backend($: BackendScope[Unit, State]){

        def onChangeWeight(e: ReactEventI) = {
            val parsed_weight = e.target.value match {
                case blank if blank=="" => 0
                case e if e.endsWith(".") => (e+'0').toDouble
                case _ => e.target.value.toDouble
            }
            $.modState(state => state.onNewAnimal(newAnimal(state.animal.getClass.getSimpleName,
                                                                Weight(parsed_weight,state.animal.weight.units))))
            
        }

        def onChangeWeightUnit(e:ReactEventI) =
            $.modState(state => state.onNewAnimal(newAnimal(state.animal.getClass.getSimpleName,
                                                                Weight(state.animal.weight.quantity,
                                                                        WeightUnit.withName(e.target.value)))))

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
            val step = s.animal.weight.units match {
                                        case WeightUnit.Grams => 100
                                        case WeightUnit.Kilograms => 0.1
                                        case WeightUnit.Ounces => 1
                                        case WeightUnit.Pounds => 0.1
                                    }

            //<.div(state.animal.weight);
            <.div(^.cls:="container",

                
                    <.div(^.cls:="row",

                        <.div(^.cls:="col-md-5",
                                <.h3(<.span(^.cls:="label label-primary", "meat")),
                        
                    
                        <.select(RoastCalculator.animals().map(v=> <.option(^.key:=v,^.value:=v)(v)),
                            ^.value := s.animal.getClass.getSimpleName,
                            ^.onChange ==> onChangeAnimal,
                            ^.cls:="form-control select select-primary"
                            )
                        )
                    ),

                <.div(^.cls:="row",
                    <.div(^.cls:="col-md-5",
                            <.h3(<.span(^.cls:="label label-primary", "weight")),
                    
                        <.input(
                            ^.placeholder := "S",
                            ^.value       := s.animal.weight.quantity,
                            ^.onChange    ==> onChangeWeight,
                            ^.`type` := "number",
                            ^.cls := "form-control",
                            ^.step := step
                            ),<.p(),
                                                <.select( WeightUnit.values.map(v => <.option(^.key:=v.toString,^.value:=v.toString)(v.toString)),
                            ^.onChange ==> onChangeWeightUnit,
                            ^.value := s.animal.weight.units.toString,
                            ^.cls := "form-control select select-primary"

                          )
)
                    ),
                
                
                    <.div(^.cls:="row",

                        <.div(^.cls:="col-md-5",
                            <.h3(<.span(^.cls:="label label-primary", "done-ness")),
                        
                    
<.select( Doneness.values.map(v => <.option(^.key:=v.toString,^.value:=v.toString)(v.toString)),
                            ^.onChange ==> onChangeDoneness,
                            ^.value := s.doneness.toString,
                            ^.cls := "form-control select select-primary"

                          ))                        )
                    ,

                <.p(),                

                <.div(^.cls:="row",
                    <.div(^.cls:="col-md-5",
                    
                        

                        s.times._2 match {
                            case z if z ==0 => <.div(^.cls:="panel panel-danger",
                                                    <.div(^.cls:="panel-heading",
                                                        <.div("instructions", ^.cls:="panel-title")),
                                                    <.div(^.cls:="panel-body", 
                                                        <.div(<.p("that seems unwise"))))
                            case _ =>
                                <.div(^.cls:="panel panel-default",
                                    <.div(^.cls:="panel-heading",
                                        <.div("instructions", ^.cls:="panel-title")),
                                    <.div(^.cls:="panel-body", 
                                        <.div(  <.p(<.strong("sizzle"),s" for ${s.times._1} minutes at ${Heat.HighC}\u2103/${Heat.HighCFan}\u2103 (fan)/${Heat.HighF}\u2109,"),
                                            <.p(<.strong("then")),
                                            <.p(<.strong("turn down the heat"), s" to ${Heat.LowC}\u2103/${Heat.LowCFan}\u2103/${Heat.LowF}\u2109 and roast for ${s.times._2} minutes")
                                            )))
                        })
                    ),

                <.div(^.cls:="row",
                    <.div(^.cls:="col-md-5",
                        <.h6(<.p("disclaimer: this site is for guidance only - you should invest in a decent meat thermometer and not eat anything you're not sure about."),
                        <.p("a 2015 - production by ",<.a(^.href:="https://ambivalenta.pe","ambivalentape")))


                        ))

           
            )
        }
    }

    

    val ScreenApp = ReactComponentB[Unit]("Screen")
        .initialState(State(Beef(Weight(2500,WeightUnit.Grams)),Doneness.Medium,
            RoastCalculator.calculateTotalCookingTimes(Beef(Weight(2500,WeightUnit.Grams)),Doneness.Medium)))
        .renderBackend[Backend]
        .buildU
    

	@JSExport
  	override def main(): Unit = {
        ReactDOM.render(ScreenApp(), document.getElementById("container"))
  	}


}