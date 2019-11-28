package model

import util.StreamUtil

sealed trait CustomData {
  def writeTo(stream: java.io.OutputStream)
}

object CustomData {
  def readFrom(stream: java.io.InputStream): CustomData = {
    StreamUtil.readInt(stream) match {
      case Log.tag => Log.readFrom(stream)
      case Rect.tag => Rect.readFrom(stream)
      case Line.tag => Line.readFrom(stream)
      case Polygon.tag => Polygon.readFrom(stream)
      case _ => throw new java.io.IOException("Unexpected discriminant value")
    }
  }

  case class Log(text: String = "") extends CustomData {
    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, Log.tag)
      StreamUtil.writeString(stream, text)
    }
  }

  case object Log extends Tagged {
    override val tag: Int = 0

    def readFrom(stream: java.io.InputStream): Log = Log(StreamUtil.readString(stream))
  }

  case class Rect(pos: model.Vec2Float = Vec2Float(),
                  size: model.Vec2Float = Vec2Float(),
                  color: model.ColorFloat = ColorFloat())
    extends CustomData {

    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, Rect.tag)
      pos.writeTo(stream)
      size.writeTo(stream)
      color.writeTo(stream)
    }
  }
  case object Rect extends Tagged {
    override val tag: Int = 1

    def readFrom(stream: java.io.InputStream): Rect = {
      Rect(
        model.Vec2Float.readFrom(stream),
        model.Vec2Float.readFrom(stream),
        model.ColorFloat.readFrom(stream))
    }
  }

  case class Line(p1: model.Vec2Float = Vec2Float(),
                  p2: model.Vec2Float = Vec2Float(),
                  width: Float = 0.0f,
                  color: model.ColorFloat = ColorFloat())
    extends CustomData {

    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, Line.tag)
      p1.writeTo(stream)
      p2.writeTo(stream)
      StreamUtil.writeFloat(stream, width)
      color.writeTo(stream)
    }
  }

  case object Line extends Tagged {
    override val tag: Int = 2

    def readFrom(stream: java.io.InputStream): Line = Line(
      model.Vec2Float.readFrom(stream),
      model.Vec2Float.readFrom(stream),
      StreamUtil.readFloat(stream),
      model.ColorFloat.readFrom(stream)
    )
  }


  case class Polygon(vertices: Seq[model.ColoredVertex] = Seq.empty) extends CustomData {
    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, Polygon.tag)
      StreamUtil.writeInt(stream, vertices.size)
      vertices.foreach(_.writeTo(stream))
    }
  }

  case object Polygon extends Tagged {
    override val tag: Int = 3

    def readFrom(stream: java.io.InputStream): Polygon = Polygon((0 to StreamUtil.readInt(stream)).map { _ => model.ColoredVertex.readFrom(stream) })
  }

}