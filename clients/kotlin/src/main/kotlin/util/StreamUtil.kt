package util

import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets

object StreamUtil {
    @Throws(IOException::class)
    fun readBytes(stream: InputStream, byteCount: Int): ByteArray {
        val bytes = ByteArray(byteCount)
        var offset = 0
        while (offset < bytes.size) {
            val read = stream.read(bytes, offset, bytes.size - offset)
            if (read == -1) {
                break
            }
            offset += read
        }
        if (offset != bytes.size) {
            throw IOException("Unexpected EOF")
        }
        return bytes
    }

    @Throws(IOException::class)
    fun readBoolean(stream: InputStream): Boolean {
        return ByteBuffer.wrap(readBytes(stream, 1)).get().toInt() != 0
    }

    @Throws(IOException::class)
    fun readInt(stream: InputStream): Int {
        return ByteBuffer.wrap(readBytes(stream, Integer.BYTES)).order(ByteOrder.LITTLE_ENDIAN).int
    }

    @Throws(IOException::class)
    fun readLong(stream: InputStream): Long {
        return ByteBuffer.wrap(readBytes(stream, java.lang.Long.BYTES)).order(ByteOrder.LITTLE_ENDIAN).long
    }

    @Throws(IOException::class)
    fun readFloat(stream: InputStream): Float {
        return ByteBuffer.wrap(readBytes(stream, java.lang.Float.BYTES)).order(ByteOrder.LITTLE_ENDIAN).float
    }

    @Throws(IOException::class)
    fun readDouble(stream: InputStream): Double {
        return ByteBuffer.wrap(readBytes(stream, java.lang.Double.BYTES)).order(ByteOrder.LITTLE_ENDIAN).double
    }

    @Throws(IOException::class)
    fun readString(stream: InputStream): String {
        val length = readInt(stream)
        return String(readBytes(stream, length), StandardCharsets.UTF_8)
    }

    @Throws(IOException::class)
    fun writeBytes(stream: OutputStream, bytes: ByteArray) {
        stream.write(bytes)
    }

    @Throws(IOException::class)
    fun writeBoolean(stream: OutputStream, value: Boolean) {
        writeBytes(stream, byteArrayOf((if (value) 1 else 0).toByte()))
    }

    @Throws(IOException::class)
    fun writeInt(stream: OutputStream, value: Int) {
        writeBytes(stream, ByteBuffer.allocate(Integer.BYTES).order(ByteOrder.LITTLE_ENDIAN).putInt(value).array())
    }

    @Throws(IOException::class)
    fun writeLong(stream: OutputStream, value: Long) {
        writeBytes(stream, ByteBuffer.allocate(java.lang.Long.BYTES).order(ByteOrder.LITTLE_ENDIAN).putLong(value).array())
    }

    @Throws(IOException::class)
    fun writeFloat(stream: OutputStream, value: Float) {
        writeBytes(stream, ByteBuffer.allocate(java.lang.Float.BYTES).order(ByteOrder.LITTLE_ENDIAN).putFloat(value).array())
    }

    @Throws(IOException::class)
    fun writeDouble(stream: OutputStream, value: Double) {
        writeBytes(stream, ByteBuffer.allocate(java.lang.Double.BYTES).order(ByteOrder.LITTLE_ENDIAN).putDouble(value).array())
    }

    @Throws(IOException::class)
    fun writeString(stream: OutputStream, value: String) {
        val bytes = value.toByteArray(StandardCharsets.UTF_8)
        writeInt(stream, bytes.size)
        writeBytes(stream, bytes)
    }
}

