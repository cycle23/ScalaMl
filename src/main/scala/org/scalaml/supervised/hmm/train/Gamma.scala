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
package org.scalaml.supervised.hmm.train


import scala.util.{Try, Success, Failure}

import org.apache.log4j.Logger

import org.scalaml.util.MathUtils._
import org.scalaml.core.Types.ScalaMl.{DblMatrix, DblArray}
import org.scalaml.util.{DisplayUtils, LoggingUtils}
import org.scalaml.supervised.hmm.HMMConfig
import HMMConfig._, LoggingUtils._


/**
  * Class that encapsulates the execution parameters for the three
  * canonical forms of the HMM. The variables are:
  * '''alpha(t, i)''': Forward probability or relevant probability for state S(i) given
  * a sequence of observation {0, 1,   t} [Formula M3]
  * '''beta(t, i)''': Backward probability or relevant probability for state S(i) given
  * the observations {t+1, t+2, ... T-1}
  * '''delta(t, i)''': Highest probability of a single path of t observations
  * which ends with a given state S(i)
  * '''gamma(t, i)''': Probability of being in a given state S(i) at observation t
  * [Formula M9]
  * '''Di-gamma(t, i, j)''': The joint probability to be within a state S(i), transition
  * to state S(j) at observation t + 1 [Formula M8]
  * '''Psi(t, i)''': Auxiliary variable that computes the index of the state that
  * maximum the probability of a single path of a sequence of t observations.[Formula M14]
  *
  * @constructor Create a new execution state for the HMM for a predefined Lambda model
  * @see "A Revealing Introduction to Hidden Markov Models"
  *      http://www.cs.sjsu.edu/~stamp/RUA/HMM.pdf for notation.
  * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state
  *               probabilities, the state-transition probabilities matrix and the emission probabilities
  *               matrix.
  * @param maxIters   Maximum number of iterations used in training (Baum-Welch)
  *
  * @author Patrick Nicolas
  * @since March 24, 2014
  * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model
  */

/**
  * Matrix of the probability of being in a given state S(i) at observation of
  * index t [Formula M9]
  * @see Chapter 7 Sequential data models / Hidden Markov Model / Training / Baum-Welch
  */
class Gamma(numObs: Int, numStates: Int) {
  private val gamma = DMatrix(numObs, numStates)

  /**
    * Update the probabilities of being in a given state for all the observations
    * @see Chapter 7 Sequential data models / Hidden Markov Model / Training / Baum-Welch
    * @param alpha Alpha matrix of relevant probabilities of being in the  state S(i)
    *              given a sequence of observation {0, 1, ... t}
    * @param beta Beta matrix of probabilities of being in a state S(i) given the
    *             observations {t+1, t+2, ... T-1}
    */
  def update(alpha: DMatrix, beta: DMatrix): Unit = {
    foreach(numObs, t => {
      // Compute the denominator of formula M8
      val sum = /:(numStates, (s, i) => {
        gamma +=(t, i, alpha(t, i) * beta(t, i))
        s + gamma(t, i)
      })
      gamma /=(t, sum)
    })
  }

  /**
    * Fold operator for the summation of the probability of being in
    * a state i give the observations {0, 1, ... t}
    * @param t index of the observation in the sequence
    * @param i index of the state S(i)
    * @return the probability being in a given state for all the observations
    */
  def fold(t: Int, i: Int): Double = /:(t, (s, n) => s + gamma(n, i))

  /**
    * Fold operator for the summation of the probability of being in
    * a state i give the observations {0, 1, ... t}
    * @param t index of the observation in the sequence
    * @param i index of the state S(i)
    * @return the probability being in a given state for all the observations
    */
  def fold(t: Int, i: Int, k: Int, obs: Array[Int]): Double =
    /:(t, (s, n) => s + {
      if (obs(n) == k) gamma(n, i) else 0.0
    })

  /**
    * Retrieve the gamma value (probability) for an observation at
    * index t, (t+1 nth observation) for a state of index j S(j)
    * @param t Index of the observation in the sequence
    * @param j index of the state
    */
  def apply(t: Int, j: Int): Double = gamma(t, j)
}


// ----------------------------------------  EOF ------------------------------------------------------------