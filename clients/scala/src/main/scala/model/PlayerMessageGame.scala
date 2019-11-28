package model

import util.StreamUtil

sealed trait PlayerMessageGame {
  def writeTo(stream: java.io.OutputStream)
}

object PlayerMessageGame {
  def readFrom(stream: java.io.InputStream): PlayerMessageGame = {
    StreamUtil.readInt(stream) match {
      case CustomDataMessage.tag => CustomDataMessage.readFrom(stream)
      case ActionMessage.tag => ActionMessage.readFrom(stream)
      case _ => throw new java.io.IOException("Unexpected discriminant value")
    }
  }

  case class CustomDataMessage(data: model.CustomData) extends PlayerMessageGame {
    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, CustomDataMessage.tag)
      data.writeTo(stream)
    }
  }

  case object CustomDataMessage extends Tagged {
    override val tag: Int = 0

    def readFrom(stream: java.io.InputStream): CustomDataMessage =
      CustomDataMessage(model.CustomData.readFrom(stream))
  }


  case class ActionMessage(action: Map[Int, model.UnitAction]) extends PlayerMessageGame {

    override def writeTo(stream: java.io.OutputStream) {
      StreamUtil.writeInt(stream, ActionMessage.tag)
      StreamUtil.writeInt(stream, action.size)
      action.foreach { case (key, action) =>
        StreamUtil.writeInt(stream, key)
        action.writeTo(stream)
      }
    }
  }

  case object ActionMessage extends Tagged {
    override val tag: Int = 0

    def readFrom(stream: java.io.InputStream): ActionMessage = {
      ActionMessage(
        (0 to StreamUtil.readInt(stream)).map { _ =>
          (StreamUtil.readInt(stream), model.UnitAction.readFrom(stream))
        }.toMap
      )
    }

  }
}