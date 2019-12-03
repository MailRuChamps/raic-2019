package model

import util.StreamUtil

sealed abstract class TextAlignment (val discriminant: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, discriminant)
    }
}

object TextAlignment {
    case object LEFT extends TextAlignment(0)
    case object CENTER extends TextAlignment(1)
    case object RIGHT extends TextAlignment(2)
    def readFrom(stream: java.io.InputStream): TextAlignment = StreamUtil.readInt(stream) match {
        case 0 => LEFT
        case 1 => CENTER
        case 2 => RIGHT
        case _ => throw new java.io.IOException("Unexpected discriminant value")
    }
}
