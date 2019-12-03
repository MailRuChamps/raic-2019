package model

import util.StreamUtil

case class JumpState(canJump: Boolean, speed: Double, maxTime: Double, canCancel: Boolean) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeBoolean(stream, canJump)
        StreamUtil.writeDouble(stream, speed)
        StreamUtil.writeDouble(stream, maxTime)
        StreamUtil.writeBoolean(stream, canCancel)
    }
}
object JumpState {
    def readFrom(stream: java.io.InputStream): JumpState = JumpState(
        StreamUtil.readBoolean(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readDouble(stream)
        ,
        StreamUtil.readBoolean(stream)
        )
}
