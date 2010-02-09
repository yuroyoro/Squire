package com.yuroyoro.squire

import scala.collection.mutable.ArrayBuffer

sealed trait Condition{
  val params = new ArrayBuffer[Any]

  def condition:String
  def and( left:Condition ) = And( this, left )
  def or( left:Condition ) = Or( this, left )
}

case class Not ( val cond:Condition ) extends Condition{
  params ++= cond.params
  override def condition = "NOT " + cond.condition
}

case class And( val right:Condition,val left:Condition ) extends Condition{
  params ++= right.params
  params ++= left.params
  override def condition = right.condition + " AND " + left.condition
}

case class Or( val right:Condition, val left:Condition ) extends Condition{
  params ++= right.params
  params ++= left.params
  override def condition = right.condition + " OR " + left.condition
}

sealed trait Criteria extends Condition{
  val col:String
  val op:String
  val paramPrefix = ""
  val paramSuffix = ""
  val delimitor = ","

  def addParam( v:Any ) = {
    params += v
    this
  }

  override def condition =
    "%s %s %s %s %s".format( col, op,
      Array.make( params.size, "?").mkString( paramPrefix,  delimitor,  paramSuffix) )

  override def toString = "%s %s %s ".format( col, op, params.mkString( paramPrefix, delimitor, paramSuffix) )
}

case class Eq(col:String) extends Criteria{
  val op = "="
  def =/( v:Any ) = addParam( v )
}

case class NotEq(col:String) extends Criteria{
  val op = "<>"
  def <>( v:Any ) = addParam( v )
}

case class Lt(col:String) extends Criteria{
  val op = "<"
  def <( v:Any ) = addParam( v )
}

case class Le(col:String) extends Criteria{
  val op = "<="
  def <=( v:Any ) = addParam( v )
}

case class Gt(col:String) extends Criteria{
  val op = ">"
  def >( v:Any ) = addParam( v )
}

case class Ge(col:String) extends Criteria{
  val op = ">="
  def >=( v:Any ) = addParam( v )
}

case class Is( col:String ) extends Criteria{
  val op = "IS"
  def is( v:Null ) = addParam( "null" )
}

case class Like( col:String ) extends Criteria{
  val op = "LIKE"
  def like( v:Any ) = addParam( v )
}

case class In( col:String) extends Criteria{
  val op = "IN"
  override val paramPrefix = "("
  override val paramSuffix = ")"
  def in( v:Any* ) = {
    params ++= v
    this
  }
}

object Implicits {
  val * = "*"
  def select = new Select("*")
  def select( columns:String* ) = new Select( columns.mkString(",") )
  def not( op:Criteria ) = Not( op )
  def by( columns:String*) = columns.mkString(",")
  implicit def string2by( columns:String) = By( columns )
  implicit def string2Eq( col:String ) = Eq( col )
  implicit def string2NotEq( col:String ) = NotEq( col )
  implicit def string2Lt( col:String ) = Lt( col )
  implicit def string2Le( col:String ) = Le( col )
  implicit def string2Gt( col:String ) = Gt( col )
  implicit def string2Ge( col:String ) = Ge( col )
  implicit def string2Is( col:String ) = Is( col )
  implicit def string2Like( col:String ) = Like( col )
  implicit def string2In( col:String ) = In( col )
  implicit def string2SetVal( col:String ) = SetVal( col )
}

