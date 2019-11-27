package model

import util.StreamUtil

abstract class PlayerMessageGame {
    @Throws(java.io.IOException::class)
    abstract fun writeTo(stream: java.io.OutputStream)
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): PlayerMessageGame {
            when (StreamUtil.readInt(stream)) {
                CustomDataMessage.TAG -> return CustomDataMessage.readFrom(stream)
                ActionMessage.TAG -> return ActionMessage.readFrom(stream)
                else -> throw java.io.IOException("Unexpected discriminant value")
            }
        }
    }

    class CustomDataMessage : PlayerMessageGame {
        lateinit var data: model.CustomData
        constructor() {}
        constructor(data: model.CustomData) {
            this.data = data
        }
        companion object {
            val TAG = 0
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): CustomDataMessage {
                val result = CustomDataMessage()
                result.data = model.CustomData.readFrom(stream)
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            data.writeTo(stream)
        }
    }

    class ActionMessage : PlayerMessageGame {
        lateinit var action: MutableMap<Int, model.UnitAction>
        constructor() {}
        constructor(action: MutableMap<Int, model.UnitAction>) {
            this.action = action
        }
        companion object {
            val TAG = 1
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): ActionMessage {
                val result = ActionMessage()
                val actionSize = StreamUtil.readInt(stream)
                result.action = mutableMapOf()
                for (i in 0 until actionSize) {
                    var actionKey: Int
                    actionKey = StreamUtil.readInt(stream)
                    var actionValue: model.UnitAction
                    actionValue = model.UnitAction.readFrom(stream)
                    result.action.put(actionKey, actionValue)
                }
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, action.size)
            for (actionEntry in action) {
                StreamUtil.writeInt(stream, actionEntry.key)
                actionEntry.value.writeTo(stream)
            }
        }
    }
}
