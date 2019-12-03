package model

import util.StreamUtil

sealed trait PlayerMessageGame {
    def writeTo(stream: java.io.OutputStream)
}
object PlayerMessageGame {
    case class CustomDataMessage(data: model.CustomData) extends PlayerMessageGame {
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, CustomDataMessage.TAG)
            data.writeTo(stream)
        }
    }
    object CustomDataMessage {
        val TAG: Int = 0
        def readFrom(stream: java.io.InputStream): CustomDataMessage = CustomDataMessage(
            model.CustomData.readFrom(stream)
            )
    }

    case class ActionMessage(action: model.Versioned) extends PlayerMessageGame {
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, ActionMessage.TAG)
            action.writeTo(stream)
        }
    }
    object ActionMessage {
        val TAG: Int = 1
        def readFrom(stream: java.io.InputStream): ActionMessage = ActionMessage(
            model.Versioned.readFrom(stream)
            )
    }

    def readFrom(stream: java.io.InputStream): PlayerMessageGame = {
        StreamUtil.readInt(stream) match {
            case CustomDataMessage.TAG => CustomDataMessage.readFrom(stream)
            case ActionMessage.TAG => ActionMessage.readFrom(stream)
            case _ => throw new java.io.IOException("Unexpected discriminant value")
        }
    }
}
