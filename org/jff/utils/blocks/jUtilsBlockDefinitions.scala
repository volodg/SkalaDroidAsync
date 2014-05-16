package org.jff.utils.blocks

object jUtilsBlockDefinitions {

  type OptionResult[T] = Either[Error, T]

  type ActionBlock = () => Unit
  
  type Analyzer[T1, T2] = (T1) => OptionResult[T2]
}
