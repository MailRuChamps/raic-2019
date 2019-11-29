package model

import util.StreamUtil

abstract class CustomData {
    @Throws(java.io.IOException::class)
    abstract fun writeTo(stream: java.io.OutputStream)
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): CustomData {
            when (StreamUtil.readInt(stream)) {
                Log.TAG -> return Log.readFrom(stream)
                Rect.TAG -> return Rect.readFrom(stream)
                Line.TAG -> return Line.readFrom(stream)
                Polygon.TAG -> return Polygon.readFrom(stream)
                PlacedText.TAG -> return PlacedText.readFrom(stream)
                else -> throw java.io.IOException("Unexpected discriminant value")
            }
        }
    }

    class Log : CustomData {
        lateinit var text: String
        constructor() {}
        constructor(text: String) {
            this.text = text
        }
        companion object {
            val TAG = 0
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): Log {
                val result = Log()
                result.text = StreamUtil.readString(stream)
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeString(stream, text)
        }
    }

    class Rect : CustomData {
        lateinit var pos: model.Vec2Float
        lateinit var size: model.Vec2Float
        lateinit var color: model.ColorFloat
        constructor() {}
        constructor(pos: model.Vec2Float, size: model.Vec2Float, color: model.ColorFloat) {
            this.pos = pos
            this.size = size
            this.color = color
        }
        companion object {
            val TAG = 1
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): Rect {
                val result = Rect()
                result.pos = model.Vec2Float.readFrom(stream)
                result.size = model.Vec2Float.readFrom(stream)
                result.color = model.ColorFloat.readFrom(stream)
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            pos.writeTo(stream)
            size.writeTo(stream)
            color.writeTo(stream)
        }
    }

    class Line : CustomData {
        lateinit var p1: model.Vec2Float
        lateinit var p2: model.Vec2Float
        var width: Float = 0.0f
        lateinit var color: model.ColorFloat
        constructor() {}
        constructor(p1: model.Vec2Float, p2: model.Vec2Float, width: Float, color: model.ColorFloat) {
            this.p1 = p1
            this.p2 = p2
            this.width = width
            this.color = color
        }
        companion object {
            val TAG = 2
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): Line {
                val result = Line()
                result.p1 = model.Vec2Float.readFrom(stream)
                result.p2 = model.Vec2Float.readFrom(stream)
                result.width = StreamUtil.readFloat(stream)
                result.color = model.ColorFloat.readFrom(stream)
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            p1.writeTo(stream)
            p2.writeTo(stream)
            StreamUtil.writeFloat(stream, width)
            color.writeTo(stream)
        }
    }

    class Polygon : CustomData {
        lateinit var vertices: Array<model.ColoredVertex>
        constructor() {}
        constructor(vertices: Array<model.ColoredVertex>) {
            this.vertices = vertices
        }
        companion object {
            val TAG = 3
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): Polygon {
                val result = Polygon()
                result.vertices = Array(StreamUtil.readInt(stream), {
                    var verticesValue: model.ColoredVertex
                    verticesValue = model.ColoredVertex.readFrom(stream)
                    verticesValue
                })
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, vertices.size)
            for (verticesElement in vertices) {
                verticesElement.writeTo(stream)
            }
        }
    }

    class PlacedText : CustomData {
        lateinit var text: String
        lateinit var pos: model.Vec2Float
        lateinit var alignment: model.TextAlignment
        var size: Float = 0.0f
        lateinit var color: model.ColorFloat
        constructor() {}
        constructor(text: String, pos: model.Vec2Float, alignment: model.TextAlignment, size: Float, color: model.ColorFloat) {
            this.text = text
            this.pos = pos
            this.alignment = alignment
            this.size = size
            this.color = color
        }
        companion object {
            val TAG = 4
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): PlacedText {
                val result = PlacedText()
                result.text = StreamUtil.readString(stream)
                result.pos = model.Vec2Float.readFrom(stream)
                when (StreamUtil.readInt(stream)) {
                0 ->result.alignment = model.TextAlignment.LEFT
                1 ->result.alignment = model.TextAlignment.CENTER
                2 ->result.alignment = model.TextAlignment.RIGHT
                else -> throw java.io.IOException("Unexpected discriminant value")
                }
                result.size = StreamUtil.readFloat(stream)
                result.color = model.ColorFloat.readFrom(stream)
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeString(stream, text)
            pos.writeTo(stream)
            StreamUtil.writeInt(stream, alignment.discriminant)
            StreamUtil.writeFloat(stream, size)
            color.writeTo(stream)
        }
    }
}
