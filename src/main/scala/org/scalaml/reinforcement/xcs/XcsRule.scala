/**
  * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
  *
  * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file
  * except in compliance with the License. You may obtain a copy of the License at
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software is distributed on an
  * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  *
  * The source code in this file is provided by the author for the sole purpose of illustrating the
  * concepts and algorithms presented in "Scala for Machine Learning".
  * ISBN: 978-1-783355-874-2 Packt Publishing.
  *
  * Version 0.99
  */
package org.scalaml.reinforcement.xcs

import scala.util.Random

import org.scalaml.trading.Signal
import org.scalaml.trading.operator.EQUAL
import org.scalaml.ga.{Operator, Gene, Quantization}
import org.scalaml.reinforcement.qlearning.QLState
import org.scalaml.core.Types.ScalaMl._
import XcsRule._
import org.scalaml.ga.Chromosome
import Gene._


/**
  * Class that defined a action associated to a sensor and a target value. A typical
  * action is a sensor exceeding a target value (or threshold). XCS action are defined
  * as gene so they can be chained as chromosomes and define a strategy to optimize the
  * operation of a system.
  * @constructor Create an XCS action.
  * @param sensorId identifier of the sensor for which an action may be triggered or fired
  * @param target threshold value associated to a sensor to trigger the action
  *
  * @author Patrick Nicolas
  * @since March 24, 2014
  * @note Scala for Machine Learning Chapter 11 Reinforcement learning / Extended learning
  *       classifier systems
  */
class XcsAction(
                 sensorId: String,
                 target: Double)
               (implicit quant: Quantization, encoding: Encoding) extends Gene(sensorId, target, EQUAL)


object XcsAction {
  val XCSACTION_SIZE = 32

  def apply(action: XcsAction, r: Random): XcsAction =
    (action ^ r.nextInt(XCSACTION_SIZE)).asInstanceOf[XcsAction]
}

/**
  * Class that define a rule or policy in XCS algorithm. The rule is encoded as a gene so
  * it can be manipulated by the Genetic Algorithm. A rule is defined by the format:<br>
  * <i>IF signal THEN action</i>.<br> The constructor increase a global rules count used to
  * automatically assigned a label to each signal/predicate.
  * @constructor Create a XCS rule as a pair of signal and action.
  * @author Patrick Nicolas
  * @since March 24, 2014
  * @note Scala for Machine Learning Chapter 11 Reinforcement learning / Extended learning
  *       classifier systems
  */
case class XcsRule(val signal: Signal, val action: XcsAction)


/**
  * Companion singleton for the XCS rule, The object defines the XcsSensor type as
  * a trading signal.
  *
  * @author Patrick Nicolas
  * @since March 24, 2014
  * @note Scala for Machine Learning Chapter 11 Reinforcement learning / Extended learning
  *       classifier systems
  */
object XcsRule {
  type XcsSensor = Signal
}


// -----------------------------------------  EOF ----------------------------------------