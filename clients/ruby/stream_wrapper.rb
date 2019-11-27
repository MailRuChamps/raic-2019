class StreamWrapper
    @@BYTE_FORMAT_STRING = "c"
    @@INT_FORMAT_STRING = "l<"
    @@LONG_FORMAT_STRING = "q<"
    @@FLOAT_FORMAT_STRING = "e"
    @@DOUBLE_FORMAT_STRING = "E"

    def initialize(stream)
        @stream = stream
    end

    def flush()
        @stream.flush()
    end

    def close()
        @stream.close()
    end

    # Reading primitives

    def read_bool()
        @stream.read_bytes(1).unpack(@@BYTE_FORMAT_STRING)[0] != 0
    end

    def read_int()
        @stream.read_bytes(4).unpack(@@INT_FORMAT_STRING)[0]
    end

    def read_long()
        @stream.read_bytes(8).unpack(@@LONG_FORMAT_STRING)[0]
    end

    def read_float()
        @stream.read_bytes(4).unpack(@@FLOAT_FORMAT_STRING)[0]
    end

    def read_double()
        @stream.read_bytes(8).unpack(@@DOUBLE_FORMAT_STRING)[0]
    end

    def read_string()
        length = read_int()
        @stream.read_bytes(length).pack("U*")
    end

    # Writing primitives

    def write_bool(value)
        @stream.write_bytes([value ? 1 : 0].pack(@@BYTE_FORMAT_STRING))
    end

    def write_int(value)
        @stream.write_bytes([value].pack(@@INT_FORMAT_STRING))
    end

    def write_long(value)
        @stream.write_bytes([value].pack(@@LONG_FORMAT_STRING))
    end

    def write_float(value)
        @stream.write_bytes([value].pack(@@FLOAT_FORMAT_STRING))
    end

    def write_double(value)
        @stream.write_bytes([value].pack(@@DOUBLE_FORMAT_STRING))
    end

    def write_string(value)
        data = value.unpack("U*")
        write_int(data.length())
        @stream.write_bytes(data.pack("C*"))
    end
end