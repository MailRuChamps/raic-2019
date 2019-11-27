import struct


class StreamWrapper:
    BOOL_FORMAT_STRUCT = struct.Struct("?")
    INT_FORMAT_STRUCT = struct.Struct("<i")
    LONG_FORMAT_STRUCT = struct.Struct("<q")
    FLOAT_FORMAT_STRUCT = struct.Struct("<f")
    DOUBLE_FORMAT_STRUCT = struct.Struct("<d")

    def __init__(self, stream):
        self.stream = stream

    def flush(self):
        self.stream.flush()

    def close(self):
        self.stream.close()

    # Reading primitives

    def read_bool(self):
        return self.BOOL_FORMAT_STRUCT.unpack(self.stream.read(1))[0]

    def read_int(self):
        return self.INT_FORMAT_STRUCT.unpack(self.stream.read(4))[0]

    def read_long(self):
        return self.LONG_FORMAT_STRUCT.unpack(self.stream.read(8))[0]

    def read_float(self):
        return self.FLOAT_FORMAT_STRUCT.unpack(self.stream.read(4))[0]

    def read_double(self):
        return self.DOUBLE_FORMAT_STRUCT.unpack(self.stream.read(8))[0]

    def read_string(self):
        length = self.read_int()
        data = self.stream.read(length)
        if len(data) != length:
            raise IOError("Unexpected EOF")
        return data.decode("utf-8")

    # Writing primitives

    def write_bool(self, value):
        self.stream.write(self.BOOL_FORMAT_STRUCT.pack(value))

    def write_int(self, value):
        self.stream.write(self.INT_FORMAT_STRUCT.pack(value))

    def write_long(self, value):
        self.stream.write(self.LONG_FORMAT_STRUCT.pack(value))

    def write_float(self, value):
        self.stream.write(self.FLOAT_FORMAT_STRUCT.pack(value))

    def write_double(self, value):
        self.stream.write(self.DOUBLE_FORMAT_STRUCT.pack(value))

    def write_string(self, value):
        data = value.encode("utf-8")
        self.write_int(len(data))
        self.stream.write(data)
