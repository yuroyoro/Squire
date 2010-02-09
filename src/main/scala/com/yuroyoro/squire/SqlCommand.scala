package com.yuroyoro.squire

import java.sql.ResultSet
import scala.collection.mutable.ArrayBuffer
import com.twitter.querulous.evaluator.QueryEvaluator

trait SqlCommand {
  val head:Option[SqlCommand]
  val command:String
  def arg:String
  val pms = new ArrayBuffer[Any]

  def constract = command format arg
  def query:String = head match {
    case None => constract
    case Some(cmd) => cmd.query + " " + constract
  }

  def params:ArrayBuffer[Any] = head match{
    case None => pms
    case Some(cmd) => cmd.params ++ pms
  }

  override def toString = query
}

trait Selectable extends SqlCommand{
  def apply[A](f: ResultSet => A)( implicit qe:QueryEvaluator ):Seq[A] = select( f )( qe )
  def select[A](f: ResultSet => A)( implicit qe:QueryEvaluator ): Seq[A] =
    qe.select( query , params: _* )(f)
  def selectOne[A](f: ResultSet => A)( implicit qe:QueryEvaluator ): Option[A] =
    qe.selectOne( query , params: _* )(f)
  def count( implicit qe:QueryEvaluator ): Int =
    qe.count( query , params: _* )
}

trait Executable extends SqlCommand{
  def apply( implicit qe:QueryEvaluator ): Int = execute( qe )
  def execute( implicit qe:QueryEvaluator ): Int = qe.execute( query ,  params: _* )
}

trait SelectConditionale extends Selectable {
  def where( cond:Condition ) = new Where( this , cond ) with Selectable with Groupable with Sortable
}

trait ExecuteConditionale extends Executable {
  def where( cond:Condition ) = new Where( this , cond ) with Executable
}

trait Groupable extends Selectable{
  def group( cond:String ) = new Group( this, cond ) with Sortable
}

trait Sortable extends Selectable {
  def order( cond:String ) = new Order( this, cond ) with Selectable
}

case class By( columns:String ) {
  def asc = columns + " ASC"
  def desc = columns + " DESC"
}

class Select ( val arg:String ) extends SqlCommand {
  val head = None
  val command = "SELECT %s"
  def from( table:String ) = new From( this , table ) with SelectConditionale
}

class From( val h:SqlCommand, val arg:String ) extends SqlCommand {
  val head = Some(h)
  val command = "FROM %s"
}

class Where( val h:SqlCommand, cond:Condition ) extends SqlCommand{
  val head = Some(h)
  def arg = cond.condition
  pms ++= cond.params
  val command = "WHERE %s"
}

class Group( val h:SqlCommand, val arg:String ) extends SqlCommand {
  val head = Some(h)
  val command = "GROUP BY %s"
}

class Order( val h:SqlCommand, val arg:String ) extends SqlCommand {
  val head = Some(h)
  val command = "ORDER BY %s"
}

class Insert extends SqlCommand{
  val head = None
  def arg:String = ""
  val command = "INSERT"
  def into( table:String ) = new Into( this, table ) with SelectConditionale
  def values( values:Any* ) = new Values( this , values: _* ) with Executable
}

class Into( val h:SqlCommand, table:String ) extends SqlCommand{
  val head = Some(h)
  val command = "INTO %s"
  var cols:String = ""
  override def arg = table + " " + cols

  def apply( columns:String* ) = {
    cols = columns.mkString( "(", ",", ")" )
    this
  }

  def values( values:Any* ) = new Values( this , values: _* ) with Executable
}

class Values( val h:SqlCommand, values:Any* ) extends SqlCommand {
  val head = Some(h)
  val command = "VALUES %s"
  pms ++= values
  override def arg = Array.make( values.size, "?").mkString( "(", ",", ")" )
}

class Update( val arg:String ) extends SqlCommand{
  val head = None
  val command = "UPDATE %s"

  def set( sets:SetVal* ) = new SetValues( this, sets: _* ) with ExecuteConditionale
}

class SetValues( val h:SqlCommand, sets:SetVal* ) extends SqlCommand {
  val head = Some(h)
  val command = "SET %s"
  var cols:String = ""
  pms ++= sets.map{ _.param }
  override def arg = sets.mkString(",")
}

case class SetVal( column:String ){
  var param:Any = null
  def =/( v:Any ) = param = v

  override def toString = "%s = ?".format( column )
}


