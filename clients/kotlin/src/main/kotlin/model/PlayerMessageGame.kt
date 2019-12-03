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
        lateinit var action: model.Versioned
        constructor() {}
        constructor(action: model.Versioned) {
            this.action = action
        }
        companion object {
            val TAG = 1
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): ActionMessage {
                val result = ActionMessage()
                result.action = model.Versioned.readFrom(stream)
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            action.writeTo(stream)
        }
    }
}
