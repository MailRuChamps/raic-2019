package util

import java.io.{IOException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}


object StreamUtil {

  def readBytes(stream: InputStream, byteCount: Int): Array[Byte] = {
    val bytes = new Array[Byte](byteCount)
    var offset = 0
    while (offset < bytes.size) {
      val read = stream.read(bytes, offset, bytes.size - offset)
      if (read == -1) {
        return bytes
      }
      offset += read
    }
    if (offset != bytes.size) {
      throw new IOException("Unexpected EOF")
    }
    bytes
  }

  def readBoolean(stream: InputStream): Boolean = {
    ByteBuffer.wrap(readBytes(stream, 1)).get() != 0
  }

  def readInt(stream: InputStream): Int = {
    ByteBuffer.wrap(readBytes(stream, Integer.BYTES)).order(ByteOrder.LITTLE_ENDIAN).getInt
  }

  def readLong(stream: InputStream): Long = {
    ByteBuffer.wrap(readBytes(stream, java.lang.Long.BYTES)).order(ByteOrder.LITTLE_ENDIAN).getLong
  }

  def readFloat(stream: InputStream): Float = {
    ByteBuffer.wrap(readBytes(stream, java.lang.Float.BYTES)).order(ByteOrder.LITTLE_ENDIAN).getFloat
  }

  def readDouble(stream: InputStream): Double = {
    ByteBuffer.wrap(readBytes(stream, java.lang.Double.BYTES)).order(ByteOrder.LITTLE_ENDIAN).getDouble
  }

  def readString(stream: InputStream): String = {
    new String(readBytes(stream, readInt(stream)), StandardCharsets.UTF_8)
  }

  def writeBytes(stream: OutputStream, bytes: Array[Byte]): Unit = {
    stream.write(bytes)
  }

  def writeBoolean(stream: OutputStream, value: Boolean): Unit = {
    writeBytes(stream, Array.fill[Byte](1)((if (value) 1 else 0).toByte))
  }

  def writeInt(stream: OutputStream, value: Int): Unit = {
    writeBytes(stream, ByteBuffer.allocate(Integer.BYTES).order(ByteOrder.LITTLE_ENDIAN).putInt(value).array())
  }

  def writeLong(stream: OutputStream, value: Long): Unit = {
    writeBytes(stream, ByteBuffer.allocate(java.lang.Long.BYTES).order(ByteOrder.LITTLE_ENDIAN).putLong(value).array())
  }

  def writeFloat(stream: OutputStream, value: Float): Unit = {
    writeBytes(stream, ByteBuffer.allocate(java.lang.Float.BYTES).order(ByteOrder.LITTLE_ENDIAN).putFloat(value).array())
  }

  def writeDouble(stream: OutputStream, value: Double): Unit = {
    writeBytes(stream, ByteBuffer.allocate(java.lang.Double.BYTES).order(ByteOrder.LITTLE_ENDIAN).putDouble(value).array())
  }

  def writeString(stream: OutputStream, value: String): Unit = {
    val bytes = value.getBytes(StandardCharsets.UTF_8)
    writeInt(stream, bytes.size)
    writeBytes(stream, bytes)
  }
}