import std.bitmanip;

abstract class Stream {
    abstract ubyte[] readBytes(size_t byteCount);
    abstract void writeBytes(const ubyte[] data);
    abstract void flush();

    bool readBool() {
        return readBytes(1)[0] != 0;
    }

    int readInt() {
        return littleEndianToNative!(int)(readBytes(int.sizeof)[0..int.sizeof]);
    }

    long readLong() {
        return littleEndianToNative!(long)(readBytes(long.sizeof)[0..long.sizeof]);
    }

    float readFloat() {
        return littleEndianToNative!(float)(readBytes(float.sizeof)[0..float.sizeof]);
    }

    double readDouble() {
        return littleEndianToNative!(double)(readBytes(double.sizeof)[0..double.sizeof]);
    }

    string readString() {
        int length = readInt();
        ubyte[] data = readBytes(cast(size_t)(length));
        return cast(string)(data);
    }

    void write(bool value) {
        writeBytes(nativeToLittleEndian(value));
    }

    void write(int value) {
        writeBytes(nativeToLittleEndian(value));
    }

    void write(long value) {
        writeBytes(nativeToLittleEndian(value));
    }

    void write(float value) {
        writeBytes(nativeToLittleEndian(value));
    }

    void write(double value) {
        writeBytes(nativeToLittleEndian(value));
    }

    void write(string value) {
        ubyte[] data = cast(ubyte[])(value);
        write(cast(int)data.length);
        writeBytes(data);
    }
}