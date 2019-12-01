package model

import util.StreamUtil

sealed abstract class MineState (val discriminant: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, discriminant)
    }
}

object MineState {
    case object PREPARING extends MineState(0)
    case object IDLE extends MineState(1)
    case object TRIGGERED extends MineState(2)
    case object EXPLODED extends MineState(3)
    def readFrom(stream: java.io.InputStream): MineState = StreamUtil.readInt(stream) match {
        case 0 => PREPARING
        case 1 => IDLE
        case 2 => TRIGGERED
        case 3 => EXPLODED
        case _ => throw new java.io.IOException("Unexpected discriminant value")
    }
}
