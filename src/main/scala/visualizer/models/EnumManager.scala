/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package visualizer.models

import chisel3.experimental.EnumAnnotations.{EnumComponentAnnotation, EnumDefAnnotation}
import firrtl.AnnotationSeq
import firrtl.annotations.{CircuitName, ComponentName, ModuleName, Named}
import firrtl.options.InputAnnotationFileAnnotation
import firrtl.options.phases.GetIncludes
import treadle.TreadleTester

import scala.collection.mutable

/** Manage the relationships between the annotation file containing information on enums
  * and the wave form signals based on that type. This is used by the WavePanel to show
  * state names associated with enums
  *
  */
object EnumManager {
  val typeNames:              mutable.HashSet[String] = new mutable.HashSet()
  val targetToTypeName:       mutable.HashMap[Named, String] = new mutable.HashMap()
  val definitions:            mutable.HashMap[String, mutable.HashMap[BigInt, String]] = new mutable.HashMap()
  val signalNameToDefinition: mutable.HashMap[String, mutable.HashMap[BigInt, String]] = new mutable.HashMap()

  def init(annotationSeq: AnnotationSeq,dataModel: DataModel, tester: TreadleTester): Unit = {
    val myAnnos = (new GetIncludes).transform(annotationSeq.filter(_.isInstanceOf[InputAnnotationFileAnnotation]))
    myAnnos.foreach {

      case EnumDefAnnotation(typeName, definition) =>
        val map = definitions.getOrElseUpdate(typeName, new mutable.HashMap())
        map ++= definition.map { case (name, value) => value -> name }

      case EnumComponentAnnotation(target, typeName) =>
        typeNames += typeName
        targetToTypeName(target) = typeName

      case _ =>
        // irrelevant annotation
    }

    val engine = tester.engine
    val symbolTable = engine.symbolTable

    targetToTypeName.keys.foreach {
      case tt @ ComponentName(componentName, ModuleName(annoModuleName, _)) =>
        symbolTable.instanceNameToModuleName.foreach {
          case (instanceName, moduleName) =>
            if (annoModuleName == moduleName) {

              // this little bit of trickery is because for treadle top level signals don't carry a module name
              val enumWireName = if (instanceName.isEmpty) {
                componentName
              } else {
                instanceName + "." + componentName
              }

              dataModel.nameToSignal.get(enumWireName) match {
                case Some(_) =>
                  signalNameToDefinition(enumWireName) = definitions(targetToTypeName(tt))
                case _ =>
              }
            }
        }
    }
  }

  def hasEnumDefinition(signalName: String): Boolean = {
    signalNameToDefinition.contains(signalName)
  }

  def getDefinition(signalName: String): Option[mutable.HashMap[BigInt, String]] = {
    signalNameToDefinition.get(signalName)
  }
}
