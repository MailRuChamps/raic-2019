package model

case class ColoredVertex(
                          position: model.Vec2Float = Vec2Float(),
                          color: model.ColorFloat = ColorFloat(0)) {

  def writeTo(stream: java.io.OutputStream) {
    position.writeTo(stream)
    color.writeTo(stream)
  }
}

object ColoredVertex {
  def readFrom(stream: java.io.InputStream): ColoredVertex = {
    ColoredVertex(
      model.Vec2Float.readFrom(stream),
      model.ColorFloat.readFrom(stream))
  }
}
