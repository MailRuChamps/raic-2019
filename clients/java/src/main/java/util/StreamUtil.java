package util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public class StreamUtil {
    public static byte[] readBytes(InputStream stream, int byteCount) throws IOException {
        byte[] bytes = new byte[byteCount];
        int offset = 0;
        while (offset < bytes.length) {
            int read = stream.read(bytes, offset, bytes.length - offset);
            if (read == -1) {
                break;
            }
            offset += read;
        }
        if (offset != bytes.length) {
            throw new IOException("Unexpected EOF");
        }
        return bytes;
    }

    public static boolean readBoolean(InputStream stream) throws IOException {
        return ByteBuffer.wrap(readBytes(stream, 1)).get() != 0;
    }

    public static int readInt(InputStream stream) throws IOException {
        return ByteBuffer.wrap(readBytes(stream, Integer.BYTES)).order(ByteOrder.LITTLE_ENDIAN).getInt();
    }

    public static long readLong(InputStream stream) throws IOException {
        return ByteBuffer.wrap(readBytes(stream, Long.BYTES)).order(ByteOrder.LITTLE_ENDIAN).getLong();
    }

    public static float readFloat(InputStream stream) throws IOException {
        return ByteBuffer.wrap(readBytes(stream, Float.BYTES)).order(ByteOrder.LITTLE_ENDIAN).getFloat();
    }

    public static double readDouble(InputStream stream) throws IOException {
        return ByteBuffer.wrap(readBytes(stream, Double.BYTES)).order(ByteOrder.LITTLE_ENDIAN).getDouble();
    }

    public static String readString(InputStream stream) throws IOException {
        int length = readInt(stream);
        return new String(readBytes(stream, length), StandardCharsets.UTF_8);
    }

    public static void writeBytes(OutputStream stream, byte[] bytes) throws IOException {
        stream.write(bytes);
    }

    public static void writeBoolean(OutputStream stream, boolean value) throws IOException {
        writeBytes(stream, new byte[] { (byte) (value ? 1 : 0) });
    }

    public static void writeInt(OutputStream stream, int value) throws IOException {
        writeBytes(stream, ByteBuffer.allocate(Integer.BYTES).order(ByteOrder.LITTLE_ENDIAN).putInt(value).array());
    }

    public static void writeLong(OutputStream stream, long value) throws IOException {
        writeBytes(stream, ByteBuffer.allocate(Long.BYTES).order(ByteOrder.LITTLE_ENDIAN).putLong(value).array());
    }

    public static void writeFloat(OutputStream stream, float value) throws IOException {
        writeBytes(stream, ByteBuffer.allocate(Float.BYTES).order(ByteOrder.LITTLE_ENDIAN).putFloat(value).array());
    }

    public static void writeDouble(OutputStream stream, double value) throws IOException {
        writeBytes(stream, ByteBuffer.allocate(Double.BYTES).order(ByteOrder.LITTLE_ENDIAN).putDouble(value).array());
    }

    public static void writeString(OutputStream stream, String value) throws IOException {
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8);
        writeInt(stream, bytes.length);
        writeBytes(stream, bytes);
    }
}